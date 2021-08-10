!------------------------------------------------------------------------------
!
! read_gridded_vars.f90 -- Read gridded time series for raw CMAQ and MET
!			   forecasts, multiple selected variables.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
! 2014-may-13	Adjust verbosity for 2 = sparse progress display.
!
! 2019-may-17	Handle varying number of forecast hours.
!		Get forecast hours from input file, rather than predetermined.
!		Skip derived variables in the list of variables to read.
! 2019-may-20	Add nhours_actual_max output for caller's convenience.
!
! Input:   Input file specs for Netcdf input files with multiple variables.
!          List of of requested Netcdf variables, with related parameters.
!	   Grid coordinate file.
!          diag = verbosity control, 0 = errors only.  See below for more.
!
! Output:  grid_data = 4-D array for time series data.
!		(forecast cycle, forecast hour, variable, station)
!	   Note, there are two time dimensions, AKA "forecast cycle" or
!		"date", and "forecast hour".
!
! Notes:
!
! In a single call, this version reads a given list of variables
! from the indicated set of input files.  Gridded data for all
! variables is returned in a single multidimensional array.
! Two-dimensional coordinate grids are also read and returned.
!
! Variables marked "derived" in the reader_codes list are skipped,
! to be computed later in a different module.  However, slots for
! derived variables are incuded in the master output array.
!
! Output arrays are allocated by this routine, and need not be
! pre-allocated.  Any previously allocated arrays are deallocated
! on entry, then re-allocated.
!
! This version supports input variables with differing numbers of
! forecast hours.  In particular, MET variables currently include
! one more forecast hour than AQM variables.  This is because
! MET variables include the initialization hour, whereas AQM
! variables do not.
!
! The returned gridded data array will probably be over-
! dimensioned for the number of forecast hours.  Supplemental
! output array "nhours_actual" contains the actual hours for each
! returned variable.
!
! This version assumes that CMAQ and MET gridded input files do
! not contain any missing values.  No recognition or processing
! of input missing values or missing value attributes is
! currently done.
!
! However, missing values are used as padding at the end of
! varying-length forecast hour time series.  Missing values are
! also used to fill the allocated array slots for derived
! variables.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = sparse,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
!------------------------------------------------------------------------------

module read__gridded_vars
contains

!-----------------------------------------------------------------------------
! Public reader interface, returns multi-variable 4-D array.
!-----------------------------------------------------------------------------

subroutine read_gridded_vars (varnames, reader_codes, grid_file_templates, &
      grid_coord_file, year, month, day, cycle_time, nhours_spec, vmiss, &
      diag, grid_data, nhours_actual, nhours_actual_max, grid_lats, &
      grid_lons, status)

   use config, only : dp
   use expand__filename
   use read__grid_coords
   use read__gridded_aqm
   use stdlit, only : normal
   use string_utils
   implicit none

! Input arguments.

   character(*), intent(in) :: varnames(:)		! requested var names
   character(*), intent(in) :: reader_codes(:)		! reader type codes
   character(*), intent(in) :: grid_file_templates(:)	! filename templaces
   character(*), intent(in) :: grid_coord_file		! aux. coordinate file

   integer,      intent(in) :: year, month, day		! forecast start date
   integer,      intent(in) :: cycle_time		! forecast cycle time
   integer,      intent(in) :: nhours_spec		! no. of forecast hours
   real,         intent(in) :: vmiss			! missing value for fill
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real, intent(out), allocatable :: grid_data(:,:,:,:)	 ! gridded forecast data
  							 !  (X, Y, vars, hours)

   integer, intent(out), allocatable :: nhours_actual(:) ! actual hours each var
   integer, intent(out)             :: nhours_actual_max ! maximum hours used

   real(dp), intent(inout), allocatable :: grid_lats(:,:)  ! grid coordinates
   real(dp), intent(inout), allocatable :: grid_lons(:,:)  !  (X, Y)

   integer,  intent(out)                :: status	 ! normal or fail

! Local variables.

   character(len(varnames)) varname
   character(len(reader_codes)) reader_code, prefix
   character(len(grid_file_templates)) data_file
   character fmt1*50

   integer vi, nx, ny, nvars
   integer nhours_expected, extra_hour
   integer nhours_dim, nhours_insert

   real, allocatable :: indata(:,:,:)	! gridded data input buffer
   					! (X, Y, hours) from (COL, ROW, TSTEP)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print '(a,i0,99(a,i0.2))', '============================ ', &
      year, '-', month, '-', day, ' ', cycle_time, 'Z'

   if (diag >= 3) print *, 'read_gridded_vars: Start.'

   nvars = size (varnames)

   if (diag >= 3) print *, 'Number of requested variables = ', nvars
   if (diag >= 3) print '(999(1x,a))', 'Requested variables =', &
      (trim (varnames(vi)), vi = 1, nvars)

! Read CMAQ 2-D grid coordinates.

! In the general case, this would be embedded in the data readers.
! However, CMAQ and MET forecast data sets do not have embedded coordinates.
! Therefore, read the coordinates from this supplemental file.

! Read the coordinate file one time only.
! Can use allocated for the test, with intent (inout).

   if (allocated (grid_lats)) then
      if (diag >= 3) print *, '  Skipping read_coord_file, already done.'
   else
      call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)
   end if

! Get grid dimensions from coordinate grid.

   nx = size (grid_lats, 1)
   ny = size (grid_lats, 2)

! Set up to track actual hours read for each variable.

   allocate (nhours_actual(nvars))
   nhours_actual(:) = 0

!-------------------------------------------------
! Main loop over each requested variable.
! Read forecast data, add to master data array.
!-------------------------------------------------

var_loop: &
   do vi = 1, nvars
      varname     = varnames(vi)		! control info for current var
      reader_code = reader_codes(vi)

! Skip derived variables, to be computed later in a different module.

      prefix = reader_code(1:7)			! check for keyword prefix only
      call lowercase (prefix)			! case insensitive here

      if (prefix == 'derived') cycle

! Make input file name for the current variable and date.

      call expand_filename (grid_file_templates(vi), year, month, day, &
         cycle_time, data_file)

! Set up for the specified reader for the current variable.

! Currently there is only a single common reader, shared between
! similar NCEP file types.  The file types differ only in the
! expected number of forecast hours.

      if (reader_code == 'reader.aqm') then
         extra_hour = 0

      else if (reader_code == 'reader.met') then
         extra_hour = 1

      else
         print *, '*** read_gridded_vars: Unknown reader code "' &
            // trim (reader_code) // '" in config file.'
         print *, '*** Var name = ' // trim (varname) // '.'
         print *, '*** Fundamental configuration problem.  Abort.'
         call exit (1)
      end if

! Call the common reader, read in the whole data array.

! Input buffer is dynamically allocated to support variable hours dimension.
! Typically, AQM and MET files have differing number of forecast hours.
! In this version, all actual hours from each file are always read in
! at this point.

! This reader quietly removes the vestigial "LAY" dimension in NCEP files.

      nhours_expected = nhours_spec + extra_hour   ! for making warnings only

      call read_gridded_aqm (data_file, varname, nx, ny, nhours_expected, &
         diag, indata, status)		! (X, Y, hours) from (COL, ROW, TSTEP)

! Abort this entire forecast cycle, on failure to read any single variable.

      if (status /= normal) then
         fmt1 = '(3a,i5.4,3i3.2,"Z")'		! nice message for sparse log
         print fmt1, ' *** read_gridded_vars: Error reading "', &
            trim (varname), '" for cycle', year, month, day, cycle_time
         print *, '*** Abort output file for this cycle.'
         print *
         return			! return abort "status" from reader subroutine
      end if

! First time only, initialize master data array to hold all variables.
! Internal array is overdimensioned to handle varying hours dimension.
! Shorter variables will be padded with standard missing value.

      nhours_actual(vi) = size (indata, 3)	! actual hours for current var

      if (vi == 1) then

! Over-estimate number of hours to contain all variables,
! including some degenerate situations.
! Include at least one extra, for difference between AQM and MET files.

         nhours_dim = 2 + maxval ((/ 48, nhours_spec, nhours_actual(vi) /))

! Allocate the master gridded data array for the current forecast cycle,
! all variables.  Includes empty slots for derived vars.

         if (diag >= 3) print *, 'read_gridded_vars: Allocate main array for' &
            // ' one forecast cycle, all variables.'
         if (diag >= 3) print '(a,4(1x,i0))', '   nx, ny, nvars, nhours =', &
            nx, ny, nvars, nhours_dim

         allocate (grid_data(nx, ny, nvars, nhours_dim))

         grid_data(:,:,:,:) = vmiss	! pre-fill all with missing values
      end if

! Insert gridded data for current variable into the master data array.
! Missing value padding past actual hours was already pre-filled.

      nhours_insert = min (nhours_actual(vi), nhours_dim)
      					! guard against overrun for weird cases

      grid_data(:,:,vi,1:nhours_insert) = indata(:,:,1:nhours_insert)
   end do var_loop

! Return maximum hours across all vars, for extent of complete data array.

   nhours_actual_max = maxval (nhours_actual(:))

! All variables read successfully.  Return success status from last reader.

   if (diag >= 3) print *, 'read_gridded_vars: Return success.'

end subroutine read_gridded_vars
end module read__gridded_vars
