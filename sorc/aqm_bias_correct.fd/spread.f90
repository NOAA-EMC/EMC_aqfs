!-----------------------------------------------------------------------------
!
! spread.f90 -- Expand bias corrections to grids, and write output file.
!
! This is the top level routine of the spreading module,
! one of the four main components of the NOAA NCO/ARL/PSD
! bias correction system for CMAQ forecast outputs.
!
! 2014-jul-10	Original top-level spreading control module.
!		By Dave Allured, NOAA/PSD/CIRES.
!
! This spreading module incorporates three major functions:
!
! * Read raw gridded forecasts for a given forecast cycle.
! * Expand site bias corrections to entire forecast grids.
! * Write bias corrected output file.
!
! Primary inputs:
!
! * Gridded raw forecast file for current forecast cycle.
! * Uncorrected forecasts interpolated to site locations.
! * Bias corrected forecasts at site locations.
! * Auxiliary grid coordinate file, as needed.
! * Site coordinates.
!
! Primary output:
!
! * Single bias corrected gridded file containing:
!   1. Bias corrected grids for all forecast hours.
!   2. Bias grids for all forecast hours.
!
! Notes:
!
! Input and output file paths are specified with path templates.
! These contain YYYY MM DD HH substitution strings that will
! resolve to desired file names for the given forecast cycle.
! Templates may include full or relative paths, and they may
! begin with an $ENV environnment variable.
!
! A separate grid coordinate file is needed only when the gridded
! raw forecast files do not include their own grid coordinates.
!
! Forecast date index:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
!-----------------------------------------------------------------------------

module spread_mod
contains

subroutine spread (in_gridded_template, grid_coord_file, &
      output_file_template, target_var, forecast_date, cycle_time, base_year, &
      calendar, uncorr_sites, corr_sites, site_lats, site_lons, vmiss, diag)

   use config, only : dp
   use expand__filename
   use index_to_date_mod
   use read__gridded_vars
   use spread__bias
   use stdlit, only : normal
   use write__corrected_netcdf
   implicit none

! Input arguments.

   character(*), intent(in) :: in_gridded_template  ! gridded input template
   character(*), intent(in) :: grid_coord_file	    ! aux. grid coordinate file
   character(*), intent(in) :: output_file_template ! output filename template
   character(*), intent(in) :: target_var	    ! target variable name
   integer,      intent(in) :: forecast_date	    ! target date index
   integer,      intent(in) :: cycle_time	    ! forecast cycle time
   integer,      intent(in) :: base_year	    ! base year for date indexes
   character(*), intent(in) :: calendar	  	    ! calendar system in use
   real(dp),     intent(in) :: uncorr_sites(:,:)    ! uncorrected forecasts
						    !   at sites (hours, sites)
   real(dp),     intent(in) :: corr_sites(:,:)	    ! bias corrected forecasts
						    !   at sites (hours, sites)
   real(dp),     intent(in) :: site_lats(:)	    ! site coordinates
   real(dp),     intent(in) :: site_lons(:)
   real(dp),     intent(in) :: vmiss		    ! common missing value code
   integer,      intent(in) :: diag		    ! diag verbosity level, 0-N

! Local program parameter.

! Local assumption that all raw forecasts to be processed will be
! in aqm Netcdf format.
! This is a temporary fix to read the raw gridded forecast file.
! See the embedded reader selection in read_gridded_forecasts.f90.

   character(*), parameter :: reader_code = 'reader.aqm'

! Local variables.

   character fdate_str*24, fmt1*60, outfile*200

   integer year, month, day, nhours
   integer ivar, status

! Dynamic arrays.

   real(dp), allocatable :: grid_lats(:,:)	    ! grid coordinates (X, Y)
   real(dp), allocatable :: grid_lons(:,:)

   real,     allocatable :: uncorr_grids(:,:,:,:)   ! input raw forecast grids
						    !   (X, Y, [var], hours)
						    ! note single precision here
   real(dp), allocatable :: bias_grids (:,:,:)	    ! output bias grids
						    !   (X, Y, hours)
   real(dp), allocatable :: corr_grids (:,:,:)	    ! bias corrected grids
						    !   (X, Y, hours)

!-------------------------------------------------------
! Read raw gridded forecast data for target variable.
!-------------------------------------------------------

   if (diag >= 3) print *, 'spread: Begin spreading module.'

! Decode the date index for the current forecast cycle.

   call index_to_date (forecast_date, year, month, day, base_year, calendar)
      						! get current Y M D integers

! Extract number of forecast hours from input array.
! Reader needs this dim size due to variability across source files.

   nhours = size (uncorr_sites, 1)

! Utilize the same reader used by the interpolation program.
! To include all possible file configurations, this routine also reads
! the companion grid coordinates.

! This routine reads forecast grids in the original single precision.
! Later they will be converted on the fly to doubles.

   if (diag >= 4) then
      print '(3a)', '   in_gridded_template = [', trim (in_gridded_template),']'
   end if

   call read_gridded_vars ( (/ target_var /), (/ reader_code /), &
      (/ in_gridded_template /), grid_coord_file, year, month, day, &
      cycle_time, nhours, diag, uncorr_grids, grid_lats, grid_lons, status)

! Abort if raw forecast grids for target variable are not available.

   if (status /= normal) then
      fmt1 = '(a,i0.4,3(1x,i0.2),"Z")'
      print '(3a)', '*** spread: Raw ', trim (target_var), &
         ' forecast grids for target forecast cycle are not available.'
      print fmt1,   '*** Target forecast cycle = ', year, month, day, cycle_time
      print '(3a)', '*** Fatal, can not complete bias correction for this', &
         ' forecast cycle.'
      call exit (1)
   end if

!-------------------------------------------------------
! Spread bias corrections to forecast grids.
!-------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  spread: Spread bias corrections to grids.'

! Convert gridded input to double precision on the fly, for calculations.

   ivar = 1				! index for target variable input grids

   call spread_bias (dble (uncorr_grids(:,:,ivar,:)), uncorr_sites, &
      corr_sites, grid_lats, grid_lons, site_lats, site_lons, vmiss, diag, &
      bias_grids, corr_grids)

!-------------------------------------------------------
! Write bias corrected output file.
!-------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  spread: Write output file.'

! Create actual output file name for current forecast cycle.

   call expand_filename (output_file_template, year, month, day, cycle_time, &
      outfile)

! Write output file.  Data and coordinate vars must be converted
! to single precision, before calling output routine.

   call write_corrected_netcdf (outfile, target_var, grid_lats, grid_lons, &
      corr_grids, bias_grids, vmiss, diag)

   if (diag >= 3) print *, 'spread: Return.'

end subroutine spread
end module spread_mod
