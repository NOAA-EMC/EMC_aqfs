!-----------------------------------------------------------------------------
!
! main_analog.f90 -- Apply Kalman/analog filter to forecast time series.
!
! This is the top level routine of the analog filter module,
! one of the four main components of the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Original Matlab version written by:
! Luca Delle Monache - lucadm@ucar.edu  (April 2010)
! Updated by:
! Luca Delle Monache - lucadm@ucar.edu  (June 2011)
! Badrinath Nagarajan - badri@ucar.edu  (June 2011)
!
! 2012-apr-20	main_analog_code.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
! 2013-jul-09	Final modified version for CMAQ PM2.5 demonstration.
!		By Irina Djalalova, NOAA/ESRL/PSD.
!
! 2014-mar-25	main_analog.f90:
!		Stand-alone demo version.
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		This version for PSD local demo context, match previous output.
!		Output 365 forecasts, 1 per day, 24 forecast hours per cycle.
!
! 2014-jun-24	main_analog.f90:
!		Convert stand-alone version to subroutine of bias_correct.f90.
!		By Dave Allured.
!		Remove demo file readers.  Data now comes from calling program.
! 2014-jul-02	Add return arg for uncorrected model data on forecast date.
! 2014-jul-15	Minor fix to diagnostic prints.
!
! 2015-jun-13	Previous versions output AN result, not KF/AN.
!		Add missing KF/AN code, ref. main_analog_code.m
!		  version 2013-jul-09, "method{8} = KF-AN".
!		Add Irina's bug fix, kpar%update = 24 for KF/AN, part 2 only.
!		For real KF/AN, make analogs for whole training period,
!		  not just one forecast.
!		Add method select and multiple filter methods.
!		Filter name change.  Old KFAN (Matlab method 5) is renamed to
!		  KFAS, Kalman Filter in Analog Space.  Ref. Irina, June 10.
!		KFAS is a dummy output that was never used.
! 2015-jul-02	Two bug fixes for KF/AN, per Irina.
!		Fix matrix transpose for kf_luca 1-D input and output.
!		Change kpar%update = nhours = 48 (forecast length) for kf_luca.
! 2015-oct-27	Pass site ID's into kf_analog, for diagnostics and site
!		  exception handling.
!
! 2016-jan-20	Add OpenMP parallel directives on site loop.
!		Read site exception list in main program, not site loop.
! 2016-feb-04	Finish KFAN parallel debug.
!		Switch to method routines, anenmean_method and kfan_method.
!		Hide details inside methods.
!		Filter name change, following recent NCAR code and discussions.
!		Old AN "Analog" method is now AnEnMean = Analog Ensemble Mean,
!		  which is what this method actually was all along.
! 2016-feb-05	Cleanup.  Remove dead method code which was moved down
!		  into method routines.
! 2016-feb-08	Do not assign scalar site_id in loop.  Presumably deferred
!		  length character is parallel violation, breaks ifort.
!		Cleaner and safer parallel loop.  Omit helper arrays.
! 2016-feb-10	Move test file writers up to main program.
!		Simplify calling interface and site loop.
! 2016-feb-15	Simplied anenmean_method interface.  Remove KFAS-related args.
!
! Notes:
!
! Many diagnostics could be added.
!
! Advanced Fortran features in play:
!
! * Allocatable arrays in derived types.
! * Allocate on assignment.
! * Reallocate on assignment.
!
! Verbosity settings (old version, June 19):
!
! 0 = errors only,   1 = milestones,    2 = brief progress,
! 3 = major details, 4 = short details, 5 = more details, etc.
!
!-----------------------------------------------------------------------------

module main__analog
contains

subroutine main_analog (filter_method, pred, obs, vmiss, target_var, &
    analog_vars, lower_limits, upper_limits, is_circular, bias_thresh_low, &
    bias_thresh_high, num_analogs, diag, site_ids, site_lats, site_lons, &
    uncorrected, corrected, filter_result)

  use config,           only : dp
  use anenmean__method
  use find__analog,     only : fpar_type
  use kfan__method
  use x__analog,        only : apar_type
  use kf__luca,         only : kpar_type
  implicit none

! Input arguments.

  character(*), intent(in) :: filter_method	! selected method name
  real(dp),     intent(in) :: pred(:,:,:,:)	! multi var model forecast data
  						!   (days, hours, vars, sites)
  real(dp),     intent(in) :: obs(:,:,:,:)	! observational data for 1 var
  						!   (days, hours, 1 var, sites)
  real(dp),     intent(in) :: vmiss		! common missing value code

  character(*), intent(in) :: target_var	! bias correction target var
  character(*), intent(in) :: analog_vars(:)	! analog vars, including target
  real(dp),     intent(in) :: lower_limits(:)	! analog var parameters (V)
  real(dp),     intent(in) :: upper_limits(:)	!   (V)
  logical,      intent(in) :: is_circular(:)	!   (V)

  real(dp),     intent(in) :: bias_thresh_low(:)   ! bias thresholds (sites)
  real(dp),     intent(in) :: bias_thresh_high(:)  !   for exception sites

  integer,      intent(in) :: num_analogs	! no. of best analogs to keep
  integer,      intent(in) :: diag		! diag verbosity level, 0-N

  character(*), intent(in) :: site_ids(:)	! site ID's
  real(dp),     intent(in) :: site_lats(:)	! site coordinates
  real(dp),     intent(in) :: site_lons(:)

! Output arguments.

  real(dp), intent(out), allocatable :: uncorrected(:,:)  ! uncorrected and
  real(dp), intent(out), allocatable :: corrected(:,:)	  ! corrected forecasts
  							  ! for final day only
  							  !   (hours, sites)
  real(dp), intent(out), allocatable :: filter_result(:,:,:)
  					   ! filter final result array
					   ! (days, hours, sites)
					   ! output only for writing test files

!-------------------------------------------------
! Local variables.
!-------------------------------------------------

  type (apar_type) :: apar
  type (fpar_type) :: fpar
  type (kpar_type) :: kpar

  character fdate_str*24

  integer j, di, hi, isite, vi
  integer ndays, nhours, nvars, nsites
  integer iobs_var, ndays_show

  integer short_period				! 1/0 switches
  integer forecast_model, forecast_obs_every	! 1/0 switches

  real(dp) ratio

!-------------------------------------------------
! Filter control parameters.
!-------------------------------------------------

  print *, 'main_analog:  Start.'

  j = site_lats(1) + site_lons(1)	! temporary, suppress compiler warnings

  if (diag >= 3) print *, 'shape (pred) = ', shape (pred)
  if (diag >= 3) print *, 'shape (obs)  = ', shape (obs)

  nvars  = size (analog_vars)	! get dimensions
  nhours = size (pred, 2)	! number of hours in each forecast cycle
  nsites = size (pred, 4)

  ndays  = size (pred, 1)	! number of forecast cycles in data
  				! training period is 1 to ndays-1
				! last day is current forecast cycle

  short_period = 0		! only for GEM
  forecast_model = 0		! only one model is used          (mandatory)
  forecast_obs_every = 1	! model forecast comes every hour (mandatory)

! Data time range.  Now determined automatically.

  apar%start_stat = -999	! setting is dynamic, now managed within
  				!   each method

  apar%num_an  = num_analogs	! Number of best analogs to use for AN
  apar%weights = 1		! 0: Do not weight analogs
				! 1: Weight them by the inverse metric
				! 2: Weight them linearly
  apar%skipMissingAnalogs = 1	! Applies to both ANKF and AN
				! 1: Always use num_an analogs, even if
				! some of the best ones are missing
  fpar%useRealTrends = 0	! 1: Use trend, 0: use neighbours
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2

! Correction for speed or ozone or PM2.5.

  kpar%enforce_positive = 1	! enforce correction for values > 0

  fpar%lowerMetric = 0.00001	! Lower bound for allowed metric value

! Kalman filter parameters.

  kpar%varo = 0.005		! variance of observation variance
  kpar%varp = 1			! variance prediction variance

  ratio = 0.1			! KF method parameter (sigma_ratio)

  kpar%update = -999		! number of time steps per forecast cycle
  				! (array stride for kf_luca)
				! this setting is now managed within each method

  kpar%start = (/ 0,  0 /)	! starting point to compute statistics
  kpar%timeZone = 0

! Analog parameters

  fpar%window = 0		! check analog at the exact hour per day

  fpar%trend = (/ 1, 1, 1 /)	! check hour-1, hour & hour+1 for the var trend
  				! with equal weighting coefficients
				! MUST have odd number of elements.
				! Numbers are the weighted coefficients.
				! 2014-feb-19, MUST have exactly 3 elements.

! Copy caller's var parameters to local structures.

  allocate (kpar%lower_limits(nvars), kpar%upper_limits(nvars))
  allocate (fpar%is_circular(nvars))

  kpar%lower_limits(:) = lower_limits(:)
  kpar%upper_limits(:) = upper_limits(:)
  fpar%is_circular(:)  = is_circular(:)

! Get array subscript for the target forecast variable to be corrected.

  apar%fvar = 0
  do vi = 1, nvars
    if (analog_vars(vi) == target_var) then
      apar%fvar = vi
      exit
    end if
  end do

  if (apar%fvar == 0) then
    print '(2a)', '*** main_analog: Specified target variable is not in', &
      ' table of analog vars.'
    print '(2a)',        '*** Target variable  = ', trim (target_var)
    print '(999(1x,a))', '*** Analog vars      =', &
      (trim (analog_vars(j)), j = 1, nvars)
    print '(2a)', '*** Abort.'
    call exit (1)
  end if

!-----------------------------------------------------------------
! Forecast model input data:
!-----------------------------------------------------------------
!
! The model data are forecast time series interpolated to the
! qualified AIRNow site locations, not gridded time series.
!
! obs must be in the format:
!    <day> x <hour> x   1   x <site>
!
! ens (model data) must be in the format:
!    <day> x <hour> x <var> x <site>
!
! Note that obs originates as a simple 1-D hourly site time
! series.  Obs has been reshaped into double time dimensions
! days x hours, to conform to the array configuration of the
! model forecast data.
!
! This means replicating obs data for additional hours, when
! this program's forecast period is configured to be anything
! greater than 24 hours.  This reshaping is now performed by
! align_obs_to_forecasts.f90, called by the main program.
!
! The ensemble dimension from the original Matlab version was
! removed.  This was a vestigial dimension, not in functional
! use, and not fully implemented.
!
! This version assumes that the same missing value code is used
! across all obs and forecast input data.  This could be checked,
! if needed.
!
!-----------------------------------------------------------------

  if (diag >= 2) then
    print *
    print *, 'Samples of model forecast input data, multiple variables:'

    ndays_show = min (ndays, 3)

    do vi = 1, nvars			! test only
      print *, trim (analog_vars(vi)) // ':'
      print '(3i6, 5f10.2)', ((di, hi, vi, pred(di, hi, vi, 1:5), &
        hi = 1, 3), di = 1, ndays_show)
    end do

    print *
    print *, 'Samples of obs input data:'

    iobs_var = 1

    do vi = iobs_var, iobs_var		! test only
      print *, trim (target_var) // ':'
      print '(3i6, 5f10.2)', ((di, hi, vi, obs(di, hi, vi, 1:5), &
        hi = 1, 3), di = 1, ndays_show)
    end do
  end if

!-------------------------------------------------
! Initialize for main loop.
!-------------------------------------------------

  if (diag >= 2) print *
  if (diag >= 2) print *, 'Allocate main_analog result array.'

  allocate (filter_result(ndays, nhours, nsites))	! DHS

  if (diag >= 3) print *, 'Main allocate complete.'

  call fdate (fdate_str)
  print *
  print '(2a)', fdate_str, '  main_analog: Run ' // trim (filter_method) &
    // ' filter for each site.'

!-------------------------------------------------
! Main site loop for selected filter method.
!-------------------------------------------------

!$omp parallel do				! START PARALLEL ZONE
site_loop: &
  do isite = 1, nsites

    if (  (diag >= 2 .and. (mod (isite, 100) == 0 .or. isite == nsites) ) &
     .or. (diag >= 3) ) then
      print *, '*** site index = ', isite
    end if

! Call the filter method routine for the current site.
! These routines must be parallel compatibile.
! Arguments isite and site_id are for diagnostics only.

! Note, 2014-feb-17, Dave A:  Removed all methods that were in the
! original Matlab code, except for the target KF/AN code.  To recon-
! struct other methods, refer to main_analog_code.m version 2013-jul-9.

    if (filter_method == 'AnEnMean') then

      call anenmean_method (obs(:,:,iobs_var,isite), pred(:,:,:,isite), &
        vmiss, apar, fpar, bias_thresh_low(isite), bias_thresh_high(isite), &
        isite, site_ids(isite), diag, filter_result(:,:,isite))

    else if (filter_method == 'KFAN') then

      call kfan_method (obs(:,:,iobs_var,isite), pred(:,:,:,isite), &
        vmiss, apar, fpar, kpar, ratio, bias_thresh_low(isite), &
        bias_thresh_high(isite), isite, site_ids(isite), diag, &
        filter_result(:,:,isite))

! Trap invalid method.

    else
      print *
      print *, '*** Unknown filter method name.  Abort.'
      print *, '*** Filter method name = "' // trim (filter_method) // '"'
      call exit (1)
    end if

  end do site_loop

!$omp end parallel do				! END PARALLEL ZONE

  call fdate (fdate_str)
  print '(2a)', fdate_str, '  main_analog: All sites complete.'

!-----------------------------------------------------------------
! Return original and corrected data for final cycle to caller.
!-----------------------------------------------------------------

  if (diag >= 2) print *
  if (diag >= 2) print *, 'main_analog: Allocate output data arrays.'

  allocate (uncorrected (nhours, nsites), corrected (nhours, nsites))

  if (diag >= 2) print '(9a)', ' main_analog: Copy uncorrected ', &
     trim (target_var), ' data to output array.'

  uncorrected(:,:) = pred(ndays, :, apar%fvar, :)	! HS <-- DHVS

  if (diag >= 2) print '(9a)', ' main_analog: Copy bias corrected ', &
     trim (target_var), ' data to output array.'

  corrected(:,:) = filter_result(ndays, :, :)		! HS <-- DHS

! Display sample output data.

  if (diag >= 2) then
    print *
    print *, 'Samples of bias corrected output data:'
    print *, trim (target_var) // ':'
    print '(i6, 5f10.2)', (hi, corrected(hi, 1:5), hi = 1, 8)
    print *
  end if

  if (diag >= 3) print *, 'main_analog:  Done.'

end subroutine main_analog
end module main__analog
