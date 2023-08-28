!------------------------------------------------------------------------------
!
! read_gridded_hourly.f90 -- Read Netcdf gridded one-per-hour forecast files.
!
! This is a support routine for the NCEP/PSL bias correction system
! for CMAQ forecast outputs.
!
! 2022-apr-03	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from read_gridded_aqm.f90 version 2022-apr-03.
!		Read all hourly files for one file type and several
!		  variables, in a single call.
!		Automatically handle varying dimensionality in file variables.
! 2022-apr-19	Fix up diagnostics and error handling.
! 2022-apr-22	Include forecast hour zero, because it is actually the
!		  first legitimate forecast hour in RRFS-CMAQ.
! 2022-may-21	Read and return the units attributes.
! 2022-dec-03	New file name convention and one-hour time shift.
!		First forecast hour file is named *.f01 or *.f001.
!		Ignore f00 and f000 files from now on.
!
! Notes:
!
! This reader is for RRFS-CMAQ one-per-hour gridded forecast
! files starting in 2022.  This reader handles multiple variables
! in each Netcdf file set.  Each call returns a full gridded
! forecast time series for each requested Netcdf variable, and a
! single forecast cycle.
!
! The returned data array must be pre-allocated by the caller,
! including an extra dimension for multiple variables.  This is
! a 4-D array with dimensions X, Y, vars, and forecast hours.
!
! The forecast date and cycle start time are implicit, because
! these are embedded in the path and file names for the Netcdf
! file set.  It is the caller's responsibility to properly
! label and align input file sets into correct time sequences.
!
! The units attribute string for each variable is also returned.
! A blank string is returned if there is any problem reading each
! units attribute, such as missing attribute.
!
! For efficiency, this routine keeps each input file open between
! consecutive variables from the same file.  File open/close is
! actually handled by the lower-level reader, read_netcdf_var.
! Caller should group the requested variables for each input file
! set, before calling this reader.
!
! Caller should pre-fill the master output array with caller's
! missing values, before calling this reader.  Grids with
! caller's missing values are left in place, when missing files
! or file read errors are encountered.
!
! Missing values and missing value attributes are not expected
! in input files.  They are not explicitly supported in this
! version.  If present, they will be treated as ordinary data
! values, and will not receive the correct special handling.
!
! To support RRFS-CMAQ, etc, this version automatically adapts
! to varying 3-D and 4-D variables within Netcdf input files.
! However, it is assumed that the dimension layout is the same
! across all file variables and all forecast hours in the current
! input file group, i.e. within the current subroutine call.
!
!------------------------------------------------------------------------------

module read__gridded_hourly
contains

subroutine read_gridded_hourly (in_template, varnames, var_select, diag, &
      vdata, nhours_actual, units, status)

   use netcdf
   use netcdf_sup
   use read__netcdf_var
   use stdlit, only : normal, fail
   use string_utils
   implicit none

   character(*), parameter :: &
      module_id = 'read_gridded_hourly.f90 version 2022-dec-03'

   character(*), intent(in ) :: in_template	 ! input filename template
   character(*), intent(in ) :: varnames(:)	 ! requested var names (vars)
   logical,      intent(in ) :: var_select(:)	 ! mask for selected variables
   integer,      intent(in ) :: diag		 ! verbosity level, 0-N

   real,         intent(out) :: vdata(:,:,:,:)   ! gridded output array
		                                 !   (X, Y, vars, hours)
   integer,      intent(out) :: nhours_actual(:) ! actual hours read (vars)
   character(*), intent(out) :: units(:)	 ! units attributes (vars)
   integer,      intent(out) :: status           ! result status, normal or fail

! Local variables.

   character(len(in_template)) infile
   character(len(varnames)) varname
   character(len(units)) units_in
   character fmt1*80, fstr2*2, fstr3*3

   integer vi, nvars, nx, ny, nlay, ntimes, rank
   integer fhour, nhours, diag2
   integer nfiles_missing, nmiss, len_varnames
   integer ncid, varid, nc_status

   logical file_missing, var_missing, ex
   logical first_open, first_fmiss, show_group

   logical, save :: first_call = .true.

   logical need_units(size(units))

   integer, allocatable :: dims_in(:), dims_expected(:)
   logical, allocatable :: grid_missing(:,:)	! missing grid flags (var,hours)

! Varying input arrays for RRFS-CMAQ gridded file formats.
! Use single precision for efficiency, to match file data type.

   real, allocatable :: indata3(:,:,:)		! (lon, lat, time)
   real, allocatable :: indata4(:,:,:,:)	! (lon, lat, layer, time)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print *, 'read_gridded_hourly: Start.'

   if ( (diag >= 2) .and. (first_call) ) then
      print *, '  Module ID = ' // module_id
      first_call = .false.
   end if

   nx     = size (vdata, 1)		! get output array dimensions
   ny     = size (vdata, 2)
   nvars  = size (vdata, 3)
   nhours = size (vdata, 4)

   rank   = -99				! signal need to inquire dimensionality

   where (var_select(:)) nhours_actual(:) = 0	! clear highest hour for vars

   nfiles_missing = 0			! init diagnostics

   first_open  = .true.			! verbosity controls, print only
   first_fmiss = .true.			! first ocurrences at diag=3
   show_group  = .true.

   allocate (grid_missing(nvars, nhours))
   grid_missing(:,:) = .true.

   where (var_select(:)) units(:) = ' '    ! default units strings to unknown
   need_units(:) = .true.

!-------------------------------------------------
! Main loop over each hourly forecast file.
!-------------------------------------------------

! Note, starting 2022 Dec 1:
! Both index "fhour" and forcast hour labels in RRFS-CMAQ file names are
! now one-based.  Forecast file names run from f000 to fNNN, e.g. f072.
! Forecast hour zero files (f000, etc.) are now ignored.

hour_loop: &
   do fhour = 1, nhours

! Resolve the final input file name for the current forecast hour.
! Support both two-digit and three-digit forecast hour strings.
! Do three digits first, prevent invalid "FF" substitution.
! Use repeat count 99 for multiple replacements.

! On overflow, this method leaves "FF" strings unresolved,
! resulting in sensible error messages.

      infile = in_template

      if (fhour < 1000) then
         write (fstr3, '(i3.3)') fhour
         call replace_substring (infile, 'FFF', fstr3, rep_count=99)
      end if

      if (fhour < 100) then
         write (fstr2, '(i2.2)') fhour
         call replace_substring (infile, 'FF', fstr2, rep_count=99)
      end if

!-------------------------------------------------
! Inner loop over each selected variable.
!-------------------------------------------------

var_loop: &
    do vi = 1, nvars
      if (.not. var_select(vi)) cycle	! skip vars not in current request

      varname = varnames(vi)

      if (diag >= 4) print '(a,i4,2a)', '  Hour', fhour, ', Read var ', &
         trim (varname)

!-------------------------------------------------------------------------
! One time only, determine the number of dimensions for file variables.
!-------------------------------------------------------------------------

! Assume the dimension layout is the same for all file variables
! in the current input file group, across all forecast hours.

get_rank: &
      if (rank < 0) then
         call getnc_dim_sizes (infile, varname, diag, rank, status, &
            file_missing, var_missing)

! Diagnostics.

         if (file_missing) then
            if (diag >= 3 .or. first_fmiss) print *, '***   File missing: ' &
               // trim (infile)
            first_fmiss = .false.
            show_group  = .false.
            nfiles_missing = nfiles_missing + 1
            cycle hour_loop
         end if

         if (var_missing) then
            print *, '***   Var missing: ' // trim (varname)
            print *, '***     File = ' // trim (infile)
            cycle var_loop
         end if

         if (status /= normal) then
            print *, '***   Netcdf error reading ' // trim (varname)
            print *, '***     File = ' // trim (infile)
            cycle var_loop
         end if

         if ((rank /= 3) .and. (rank /= 4)) then
            print '(a,i0)', ' ***   Unexpected number of dimensions for ' &
               // trim (varname) // ' = ', rank
            print *,         '***     File = ' // trim (infile)
            cycle var_loop
         end if

! Setup to read 3-D or 4-D file variable.  There are currently two
! dimensionality cases for RRFS-CMAQ.  The time and level dimensions
! are vestigial, both with extent = 1.  Fortran subscript ordering:
!
!    (lon, lat, time)        -- phyf*.nc
!    (lon, lat, layer, time) -- o3_sfc*.nc, rcmaq.t12z.pm25tot_sfc_f*.nc

         ntimes = 1
         nlay   = 1

         if (rank == 3) dims_expected = (/ nx, ny, ntimes /)
         if (rank == 4) dims_expected = (/ nx, ny, nlay, ntimes /)

      end if get_rank

!-------------------------------------------------------------------------
! Read 3-D or 4-D file variable.  Use generic Netcdf reader.
!-------------------------------------------------------------------------

! At diag level 3 only, squelch many duplicate file names.  Print input
! file name only for the first occurrence of the current file type.

      diag2 = diag
      if ( (diag == 3) .and. (.not. first_open) ) diag2 = 2
      first_open = .false.

! Input array is auto-allocated.
! This routine actually keeps the current file open between
! consecutive calls for the same file, for efficiency.

      if (rank == 3) call read_netcdf_var (infile, varname, diag2, indata3, &
         status, nc_id=ncid, var_id=varid)

      if (rank == 4) call read_netcdf_var (infile, varname, diag2, indata4, &
         status, nc_id=ncid, var_id=varid)

! Read errors are soft errors.  On read error, skip current variable
! or current file, and move on to next variable or next file.

! However, need to properly account for missing files, for cleaner
! diagnostics.  Lower level does not return enough details, so do this.

      if (status /= normal) then
         inquire (file=infile, exist=ex)	! check for file missing

         if (.not. ex) then			! yes, file is missing
            first_fmiss = .false.		! SKIP HOUR, and assume lower
            show_group  = .false.               !   level printed diag info
            nfiles_missing = nfiles_missing + 1
            cycle hour_loop

         else				! no, must be missing var or read error
            cycle var_loop		! SKIP VAR, and assume lower level
         end if				!   printed diag info
      end if

! Check for expected dimension sizes.

      if (rank == 3) dims_in = shape (indata3)
      if (rank == 4) dims_in = shape (indata4)

! Handle unexpected dim sizes as soft error.  Skip current variable,
! and move on to next variable or next file.

      if (any (dims_in /= dims_expected)) then
         print *, '*** read_gridded_hourly: Unexpected var dimensions in file.'
         print *, '***   File = ' // trim (infile)
         print *, '***   Var name = ' // trim (varname)
         fmt1 = '(a,3(2x,i0),a)'
         print fmt1, ' ***   Expected dimensions    =', dims_expected(:)
         print fmt1, ' ***   Var dimensions in file =', dims_in(:)
         cycle var_loop
      end if

! Transfer data for this variable to the correct slice of the
! 4-D output array.  Omit vestigial time and layer dimensions.
! In both cases, only a single X/Y grid is copied.

      if (rank == 3) vdata(:,:,vi,fhour) = indata3(:,:,1)
				! (X, Y, VAR, FHOUR) <-- (X, Y, TIME)

      if (rank == 4) vdata(:,:,vi,fhour) = indata4(:,:,1,1)
				! (X, Y, VAR, FHOUR) <-- (X, Y, LAYER, TIME)

      nhours_actual(vi) = fhour      ! track the last hour read for this var
      grid_missing(vi,fhour) = .false.

! Also read the units attribute, if available.
! Any attribute error is silent and returns the defaulted blank string.
!
! Read the units only one time for each var.  Assume all files in the
! same forecast have the same units attributes.

      if (need_units(vi)) then
         nc_status = nf90_get_att_trim (ncid, varid, 'units', units_in)

         if (nc_status == nf90_noerr) then
            units(vi) = units_in
            need_units(vi) = .false.
         end if
      end if

! Grid diagnostics.

      if (diag >= 3) print '(a,i3.3,2(a,g16.5),3x,a)', '   F', fhour, &
          ' min, max data = ', minval (vdata), ', ', maxval (vdata), &
          trim (varname)

    end do var_loop
   end do hour_loop

!-------------------------------------------------
! Summary diagnostics.
!-------------------------------------------------

! This is usually the most common error, so keep it brief.
! Return status to caller is full failure.

   if (nfiles_missing == nhours) then
      if (diag >= 2) then
         print *, '*** All files missing for group: ' // trim (in_template)
      end if

      status = fail
      return				! error return
   end if

! Less common errors.  Provide more summary details.
! Also treat as soft errors.  Return normal status and partially missing data.

   if (diag >= 2) then
      if (nfiles_missing > 0) print '(a,i5,a,i0,2a)', ' ***', nfiles_missing, &
         ' out of ', nhours, ' files missing for current file group.'

      len_varnames = maxval ((/ (len_trim (varnames(vi)), vi = 1, nvars) /))
      				! longest non-blank length of ALL var names
                                ! for column alignment across all file types

      write (fmt1, '(a,i0,a)') '(a,i5,a,a', len_varnames, ',a,9999(1x,i0))'

      do vi = 1, nvars
         if (.not. var_select(vi)) cycle     ! skip vars not in current request

         nmiss = count (grid_missing(vi,:))

         if (nmiss /= 0) then
            if (show_group) then
               print *, '*** File group = ' // trim (in_template)
               show_group = .false.
            end if

            print fmt1,' ***', nmiss,' missing hours for ', varnames(vi),' =', &
               pack ( (/ (fhour, fhour = 1, nhours) /), (grid_missing(vi,:)) )
         end if
      end do

   end if

   if (diag >= 3) print *, 'read_gridded_hourly:  All files read.  Return.'

   status = normal			! normal return, partial or full data

end subroutine read_gridded_hourly
end module read__gridded_hourly
