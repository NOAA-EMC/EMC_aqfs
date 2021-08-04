!------------------------------------------------------------------------------
!
! read_gridded_aqm.f90 -- Read Netcdf gridded CMAQ and MET forecast data.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
!
! 2019-may-16	Handle varying number of forecast hours.
!		Change output data array to dynamically allocated.
!		Modify tests for required and varying dimension sizes.
!		Handle unexpected required dim sizes as soft error only.
!
! Notes:
!
! This reader is for the Netcdf gridded archive format for CMAQ
! and MET outputs, provided by Pius Lee, January 10, 2014.
!
! This reader handles both of the following file formats, and
! similar NCEP formats.  These formats use the same NCEP standard
! layout of dimensions and general structure.
!
!   CMAQ intermediate output files   aqm.t06z.O3_pm25.ncf
!   MET  intermediate output files   sfc_met_n_PBL.t06z.ncf
!
! Each call returns gridded forecast data for a single Netcdf
! variable, and a single forecast cycle.
!
! The returned data array is auto-allocated or reallocated.
! This is a 3-D array with dimensions X, Y, and forecast hours.
! These correspond to the original file dimensions COL, ROW, and
! TSTEP.  The vestigial LAY dimension is removed.
!
! The forecast date and cycle start time are implicit, because
! these are embedded in each Netcdf file name.  It is the
! caller's responsibility to align the input day files into a
! correct time sequence.
!
! This routine converts the double precision input array which
! was originally single, back to a single precision output array.
!
!------------------------------------------------------------------------------

module read__gridded_aqm
contains

subroutine read_gridded_aqm (infile, varname, nx, ny, nhours_expect, diag, &
      vdata, status)

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal, fail
   implicit none

   character(*),     intent(in ) :: infile	  ! input file name
   character(*),     intent(in ) :: varname	  ! requested var name
   integer,          intent(in ) :: nx		  ! required X dimension size
   integer,          intent(in ) :: ny		  ! requited Y dimension size
   integer,          intent(in ) :: nhours_expect ! expected hours, warning only
   integer,          intent(in ) :: diag	  ! verbosity level, 0-N

   real,allocatable, intent(out) :: vdata(:,:,:)  ! output array (X, Y, hours)
   integer,          intent(out) :: status	  ! result status, normal or
   						  !   fail (stdlit)
! Local variables.

   character fmt1*50
   integer nlay, nhours_file
   integer dim4_in(4), dim3_expected(3)

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'read_gridded_aqm: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

   call read_netcdf_var (infile, varname, diag, indata, status)

! Read errors are soft errors.
! On read error, return with fail status from lower level.

   if (status /= normal) return

! Check for dimensions required to conform, to ensure robustness.

   dim4_in       = shape (indata)	! input dims: COL, ROW, LAY, TSTEP
   nhours_file   = dim4_in(4)		! TSTEP only

   nlay          = 1			! expected vestigial LAY dimension
   dim3_expected = (/ nx, ny, nlay /)	! expected first 3 dims, omit TSTEP

! Handle unexpected dim sizes as soft error.  Abort current forecast cycle only..

   if (any (dim4_in(1:3) /= dim3_expected(1:3))) then
      print *, '*** read_gridded_aqm: Incorrect var dimensions in file.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = ' // trim (varname)
      fmt1 = '(a,3(1x,i0),a)'
      print fmt1, &
         ' *** Expected 4-D dimensions     = (', dim3_expected(1:3), ' any)'
      print fmt1, &
         ' *** Current file var dimensions = (', dim4_in(:), ')'

      status = fail			! signal soft error, abort cycle
      return
   end if

! Check for unexpected number of forecast hours.
! Warning only.  Print and continue with variable size array.

   if (nhours_file /= nhours_expect) then
      print *,         '*** read_gridded_aqm: Warning:' &
                             // ' Unexpected number of forecast hours.'
      print *,         '*** File = ' // trim (infile)
      print *,         '*** Var name = ' // trim (varname)
      print '(a,i0)', ' *** Expected number of hours = ', nhours_expect
      print '(a,i0)', ' *** Number of hours in file  = ', nhours_file
   end if

! Auto-allocate and transfer data to the 3-D output array.
! Omit the vestigial LAY dimension.
! Return all forecast time steps from file.  Change from 2014 versions.

! Round from double to single precision, using default rounding mode.
! Restore single precision as it was in the original input file.

   vdata = real (indata(:,:,1,:))    ! (X, Y, HOUR) <-- (COL, ROW, LAY, TSTEP)

! Diagnostics, if requested.

   if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (vdata), ', ', &
         maxval (vdata)
   end if

   if (diag >= 3) print *, 'read_gridded_aqm: Return.'

end subroutine read_gridded_aqm
end module read__gridded_aqm
