!------------------------------------------------------------------------------
!
! bias_correct.f90
!
! Main program to perform bias correction on a gridded forecast
! time series for one variable, and one 48-hour forecast cycle.
!
! This is the combined QC, analog filter, and spreading components
! of the NOAA NCO/ARL/PSD bias correction system for CMAQ forecast
! outputs.
!
! 2014-jul-16	Original version of main program and process control.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2014-jul-23	Fix program name in error message.
!
! 2015-jun-13	Stop reading obs data at the day before current forecast date.
!		For retrospective testing, this prevents results too perfect,
!		  due to current day obs that are unavailable in normal
!		  real-time operation.
! 2015-oct-27	Pass site ID's and coordinates into main_analog, for
!		  diagnostics and site exception handling.
!
! 2016-jan-20	Parallel support.  Move site exception list reader out of
!		  site loop, up to main program.
! 2016-feb-09	Add config parameters: filter method, debug controls, etc.
! 2016-feb-10	Move test file writers out of main_analog, up to main program.
!
! Credits:
!
! The filter component is the analog/Kalman filter for forecast
! time series interpolated to observation site locations.  The
! original Matlab code and filter algorithms are by Luca Delle
! Monache, Thomas Nipen, and Badrinath Nagarajan of NCAR.
!
! Core interpolation and spreading routines were developed by
! Irina Djalalova and James Wilczak of NOAA/ESRL/PSD3.
!
! Primary inputs:
!
! * Configuration file with data specs and other run parameters.
! * AIRNow obs data for target correction variable, for the extent
!   of the training period.
! * Model forecast data interpolated to station coordinates,
!   training period through target forecast cycle.  This is a data
!   archive produced by the companion program "interpolate_update"..
!
! Primary outputs:
!
! * Bias corrected 48-hour gridded forecast for the target variable.
! * Coordinate list for all sites currently validated by the
!   quality control module.
!
!------------------------------------------------------------------------------
!
! Usage, run command with four or five arguments:
!
! ./bias_correct config-file forecast-cycle start-date forecast-date diag=N
!
! config-file	  Configuration file for current platform and data set
!		  set configuration.  Includes file name templates for
!		  input and output files, and variable lists for
!		  analog variables and bias correction target.
!
! forecast-cycle  HH, initial hour for the target forecast cycle.
!		  Currently 06 or 12.  Set to $Cyc for WCOSS real-time
!		  operation.
!
! start-date	  YYYYMMDD: Start date of desired training period.
!
! forecast-date	  YYYYMMDD: Forecast initial date for bias correction.
!		  The training period ends on the day before this date.
!
! diag=N	  Set verbosity level N.  Optional argument, default
!		  is set below.  0 = errors only, 1 = milestones,
!		  2 = brief progress, 3 and up = increasing details.
!
! Examples:
!
! ./bias_correct config.pm2.5.0612 20140515 20140529 12z
! ./bias_correct config.pm2.5.0612 20140515 20140612 06z diag=4
!
!------------------------------------------------------------------------------

program bias_correct

   use align__obs_to_forecasts
   use config, only : dp
   use get__command_args
   use expand__filename
   use index_to_date_mod
   use main__analog
   use read__config_file_main
   use read__exception_list
   use read__interp_forecasts
   use read__obs_qc
   use read__grid_coords
   use spreading_mod
   use stdlit
   use write__site_list
   use write__test_day_files
   use write__test_site_files
   implicit none

! Local variables.

   integer, parameter :: id_len = 9		! station ID string length
   						! for 9-digit AIRNow site ID's
   character(200) config_file, site_list_title
   character(60)  title_varname
   character(24)  fdate_str

   integer ndays, nhours, diag, cycle_time
   integer start_date, training_end_date, forecast_date, base_year
   integer di, hi, vi, ndays_show

   real(dp) vmiss, standard_vmiss

! Config file parameters.

   character(200) obs_file_template, interp_file_template
   character(200) in_gridded_template, output_file_template
   character(200) new_site_list_template, grid_coord_file
   character(200) site_exception_file
   character(60)  target_obs_var, target_model_var
   character(200) filter_method, output_limit_method
   character(200) site_file_template, day_file_template

   integer num_analogs
   logical stop_after_filter

! Analog var table (config file).

   character(60),  allocatable :: analog_vars(:)	! var config data (V)
   real(dp),       allocatable :: lower_limits(:)	! (V)
   real(dp),       allocatable :: upper_limits(:)	! (V)
   logical,        allocatable :: is_circular(:)	! (V)

! Obs input data.

   character(id_len), allocatable :: obs_ids(:)		! obs site ID's (S)
   real(dp), allocatable :: obs_lats(:), obs_lons(:)	! obs site coordinates
   real(dp), allocatable :: obs_in(:,:)		 	! obs input time series
  							!  (sites, hours)
   character*80 obs_units				! obs var units

   real(dp), allocatable :: obs_reshaped(:,:,:,:)  ! obs reshaped to model data
   						   ! (days, hours, 1 var, sites)
! Interpolated model input data.

   character(id_len), allocatable :: interp_ids(:) ! model site IDs (S)
   real(dp), allocatable :: interp_lats(:)	   ! model site coordinates (S)
   real(dp), allocatable :: interp_lons(:)
   real(dp), allocatable :: model_in(:,:,:,:)	   ! interpolated. model data
   						   ! (days, hours, vars, sites)
! Grid coordinates.

   real(dp), allocatable :: grid_lats(:,:)	   ! (X, Y)
   real(dp), allocatable :: grid_lons(:,:)

! Target var, uncorrected and corrected data at site locations, forecast date.

   real(dp), allocatable :: uncorr_sites(:,:)	   ! (hours, sites)
   real(dp), allocatable :: corr_sites(:,:)	   ! (hours, sites)

! Target var, corrected data at site locations, all days in period.
! For writing test files only.

   real(dp), allocatable :: filter_result_3d(:,:,:)  ! (days, hours, sites)

! Site exception bias thresholds.

   real(dp), allocatable :: excep_thresh_low(:)    ! (sites)
   real(dp), allocatable :: excep_thresh_high(:)

! Program parameters.

   character(*), parameter :: prog_name = 'bias_correct'
   character(*), parameter :: calendar  = 'gregorian'

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   call fdate (fdate_str)
   print *
   print *, &
      '======================================================================='
   print '(2a)', fdate_str, '  bias_correct.f90: Start.'

! Set dynamic run parameters.

   diag = 2	! Set default verbosity: 0 = errors only, 1 = milestones,
   		! 2 = brief progress, 3 and up = increasing detail level.
                ! May be changed on command line.

! Assumed missing value.

! Currently, 2014-jun-12, model forecast data is read with an
! assumed missing value.  However, the AIRNow obs reader performs
! actual missing value conversion on the original obs missing value.

   standard_vmiss  = -999		! assumed missing value in data
   vmiss = standard_vmiss		! also set vmiss for general usage

! Get command line parameters.

   call get_command_args (prog_name, calendar, config_file, cycle_time, &
      start_date, forecast_date, base_year, diag)

! Date check, tighter than the command line input routine.

   if (forecast_date <= start_date) then
      print *
      print *, '*** bias_correct: Abort, date error on command line.'
      print *, '*** Forecast date must be at least one day later than' &
         // ' start date.'
      print *, '*** The training period must be at least one day long.'
      print *, '*** The forecast date is not included in the training period.'
      call exit (1)
   end if

! Read and process the configuration file.

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_config_file_main.'

   call read_config_file_main (config_file, obs_file_template, &
      interp_file_template, in_gridded_template, output_file_template, &
      new_site_list_template, grid_coord_file, site_exception_file, &
      target_obs_var, target_model_var, analog_vars, &
      lower_limits, upper_limits, is_circular, &
      filter_method, num_analogs, output_limit_method, &
      site_file_template, day_file_template, stop_after_filter)

! Read grid coordinate file.  Grid coords needed for both QC and final output.

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_grid_coords.'

   call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)

!---------------------------------------------------------------------
! Read quality-controlled observational data.
!---------------------------------------------------------------------
!
! * Read original obs time series for training period.
! * Convert data units to units needed by this program.
! * Convert file missing values to program missing values.
! * Run quality control procedures on straight obs time series.
!
! Secondary output is an updated list of valid site coordinates.
! Normally this file is safely ignored.  But when differences in
! valid sites are sufficient, this site list may then used to
! generate a new local archive of interpolated previous forecasts.
!
!---------------------------------------------------------------------

   print '(a)', 'Read obs data for target variable, and run quality control.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_obs_qc.'

   training_end_date = forecast_date - 1	! read only training period,
   						! NOT current forecast date

   call read_obs_qc (obs_file_template, target_obs_var, start_date, &
      training_end_date, base_year, standard_vmiss, grid_lats, grid_lons, &
      diag, obs_ids, obs_lats, obs_lons, obs_in, obs_units)

!---------------------------------------------------------------------
! Write updated coordinate list for QC validated sites.
!---------------------------------------------------------------------

   title_varname = target_obs_var		! make comprehensible var name

   if (target_obs_var == 'COPOPM') title_varname = 'PM2.5'
   if (target_obs_var == 'COPO'  ) title_varname = 'ozone'

   site_list_title = 'List of AIRNow ' // trim (title_varname) &
      // ' sites for bias correction.'

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call write_site_list.'

   call write_site_list (new_site_list_template, forecast_date, cycle_time, &
      base_year, calendar, site_list_title, obs_ids, obs_lats, obs_lons, diag)

!---------------------------------------------------------------------
! Read interpolated forecast data for same training period.
!---------------------------------------------------------------------
!
! Read interpolated forecast data (model data) from the start of
! the training period, through the target forecast date.  Note that
! the forecast date is included here, but was NOT read for obs data.
!
! Data for the target forecast date must be present for all given
! analog variables, to enable the bias correction process.
!
!---------------------------------------------------------------------

   print *, 'Read interpolated forecast data, all analog variables.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_interp_forecasts.'

   call read_interp_forecasts (interp_file_template, analog_vars, start_date, &
      forecast_date, base_year, cycle_time, standard_vmiss, diag, interp_ids, &
      interp_lats, interp_lons, model_in)

   ndays  = size (model_in, 1)			! (days, hours, vars, sites)
   nhours = size (model_in, 2)

!---------------------------------------------------------------------
! Align obs data with interpolated forecast arrays.
!---------------------------------------------------------------------
!
! * Align obs data with interpolated forecast arrays, by site ID's.
! * Reshape and duplicate into conforming 48-hour overlapping subsets.
! * Phase shift obs hours for correct alignment with forecast hours.
!
!---------------------------------------------------------------------

   print *, 'Align obs data with interpolated forecast data.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call align_obs_to_forecasts.'

   call align_obs_to_forecasts (obs_in, cycle_time, ndays, nhours, vmiss, &
      obs_ids, obs_lats, obs_lons, interp_ids, interp_lats, interp_lons, &
      diag, obs_reshaped)			! (days, hours, 1 var, sites)

   if (diag >= 2) then
      vi = 1				! dummy var subscript for obs
      ndays_show = min (ndays, 3)
      print *, trim (target_obs_var) // ':    (sample of obs reshaped array)'
      print '(3i6, 5f10.2)', ((di, hi, vi, obs_reshaped(di, hi, vi, 1:5), &
         hi = 1, 3), di = 1, ndays_show)
      print *
   end if

!---------------------------------------------------------------------
! Read supplemental files.
!---------------------------------------------------------------------

! Read the site bias threshold exception list, which is a sparse list.
! Output arrays are aligned with current site indexing.

   call read_exception_list (site_exception_file, interp_ids, vmiss, diag, &
      excep_thresh_low, excep_thresh_high)

!---------------------------------------------------------------------
! Apply Kalman/analog filter to interpolated forecasts.
!---------------------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call main_analog.'

   call main_analog (filter_method, model_in, obs_reshaped, vmiss, &
      target_model_var, analog_vars, lower_limits, upper_limits, is_circular, &
      excep_thresh_low, &
      excep_thresh_high, num_analogs, diag, interp_ids, interp_lats, &
      interp_lons, uncorr_sites, corr_sites, &
      filter_result_3d)

   if (diag >= 3) print *, 'main_analog returned.'

! Recover memory from large arrays, no longer needed.
! Next step uses large arrays.

   deallocate (model_in, obs_reshaped)

!-----------------------------------------------------------------
! Write intermediate test files, if selected.
!-----------------------------------------------------------------

   if (day_file_template /= 'none' ) &
      call write_test_day_files  (day_file_template,  filter_result_3d, &
         vmiss, diag, interp_ids, interp_lats, interp_lons)

   if (site_file_template /= 'none' ) &
      call write_test_site_files (site_file_template, filter_result_3d, &
         vmiss, diag, interp_ids, interp_lats, interp_lons)

! Check for early stop request.

   if (stop_after_filter) then
      print *, '*** "Stop after filter" is selected (config file).'
      call fdate (fdate_str)
      print '(2a)', fdate_str, '  bias_correct.f90: Stop.'
      call exit
   end if

!---------------------------------------------------------------------
! Spread bias corrections to forecast grids, and write output file.
!---------------------------------------------------------------------

   call fdate (fdate_str)
   print *
   print '(2a)', fdate_str, '  Call spreading module.'

   call spreading (in_gridded_template, grid_coord_file, &
      output_file_template, target_model_var, output_limit_method, &
      forecast_date, cycle_time, base_year, calendar, uncorr_sites, &
      corr_sites, interp_lats, interp_lons, vmiss, diag)

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  bias_correct.f90: Done.'

end program bias_correct
