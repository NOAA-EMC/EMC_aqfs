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
! Input:   infile = path to Netcdf input file with multiple variables.
!          var_codes = list of short codes for requested variables.
!          diag = verbosity control, 0 = errors only.  See below for more.
!
! Output:  grid_data = 4-D array for time series data.
!		(forecast cycle, forecast hour, variable, station)
!	   Note, there are two time dimensions, AKA "forecast cycle" or
!		"date", and "forecast hour".
!
! Notes:
!
! In a single call, this version reads the given input file, and
! returns the data for a specified set of variables in a single
! multidimensional array.
!
! For the Kalman/Analog filter program, call this program once to
! read all the obs data, and another time to read all the model
! data.
!
! The variables to read are specified by a list of short code
! names.  Translation to the actual Netcdf var names is handled
! within this module.
!
! This version assumes a fixed dimension order in the input data.
! This is (stations, forecast_time, fcst_cycle) in Fortran order.
! The actual dimension names are not significant, only the
! ordering.
!
! Output arrays are allocated by this routine, and need not be
! pre-allocated.  Any that were previously allocated are
! deallocated on entry, then re-allocated.
!
! CMAQ and MET gridded archives do not normally contain missing
! values, or else there is an assumed missing value code.
! Therefore, this routine does not currently include any handling
! for missing values or missing value attributes.
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
      grid_coord_file, year, month, day, cycle_time, nhours, diag, &
      grid_data, grid_lats, grid_lons, status)

   use config, only : dp
   use expand__filename
   use read__grid_coords
   use read__gridded_aqm
   use stdlit, only : normal
   implicit none

! Input arguments.

   character(*), intent(in) :: varnames(:)		! requested var names
   character(*), intent(in) :: reader_codes(:)		! reader type codes
   character(*), intent(in) :: grid_file_templates(:)	! filename templaces
   character(*), intent(in) :: grid_coord_file		! aux. coordinate file

   integer,      intent(in) :: year, month, day		! forecast start date
   integer,      intent(in) :: cycle_time		! forecast cycle time
   integer,      intent(in) :: nhours			! # hrs in forecast cyc.
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real, intent(out), allocatable :: grid_data(:,:,:,:)	 ! gridded forecast data
  							 !  (X, Y, vars, hours)
   real(dp), intent(inout), allocatable :: grid_lats(:,:)   ! grid coordinates
   real(dp), intent(inout), allocatable :: grid_lons(:,:)   !  (X, Y)
   integer,  intent(out)                :: status	 ! normal or fail

! Local variables.

   character(len(varnames)) varname
   character(len(reader_codes)) reader_code
   character(len(grid_file_templates)) data_file

   character fmt1*50
   integer vi, nx, ny, nvars

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

! Allocate the master gridded data array for the current forecast cycle,
! all variables.

   nx = size (grid_lats, 1)			! get dims from coordinate file
   ny = size (grid_lats, 2)

   if (diag >= 3) print *, 'read_gridded_vars: Allocate main array for one' &
      // ' forecast cycle, all variables.'
   if (diag >= 3) print '(a,4(1x,i0))', '   nx, ny, nvars, nhours =', &
      nx, ny, nvars, nhours

   allocate (grid_data(nx, ny, nvars, nhours))

!-------------------------------------------------
! Main loop over each requested variable.
! Read forecast data, add to master data array.
!-------------------------------------------------

   do vi = 1, nvars
      varname     = varnames(vi)		! control info for current var
      reader_code = reader_codes(vi)

! Make the file name for the current variable and date.

      call expand_filename (grid_file_templates(vi), year, month, day, &
         cycle_time, data_file)

! Call the specified reader for the current variable.

      if (any (reader_code == (/ 'reader.aqm', 'reader.met' /))) then

         call read_gridded_aqm (data_file, varname, diag, grid_data(:,:,vi,:), &
            status)

      else
         print *, '*** read_gridded_vars: Unknown reader code "' &
            // trim (reader_code) // '" in config file.'
         print *, '*** Var name = ' // trim (varname) // '.  Abort.'
         call exit (1)
      end if

! Abort this entire forecast cycle, on failure to read any single variable.

      if (status /= normal) then
         fmt1 = '(3a,i5.4,3i3.2,"Z")'		! nice message for sparse log
         print fmt1, ' *** read_gridded_vars: Error reading "', &
            trim (varname), '" for cycle', year, month, day, cycle_time
         print *, '*** Abort output file for this cycle.'
         print *
         return			! return abort status from reader subroutine
      end if
   end do

! All variables read successfully.  Return success status from last reader.

   if (diag >= 3) print *, 'read_gridded_vars: Return success.'

end subroutine read_gridded_vars

end module read__gridded_vars
