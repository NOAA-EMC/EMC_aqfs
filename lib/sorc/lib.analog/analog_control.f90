!-----------------------------------------------------------------------------
!
! analog_control.f90 -- Top level site loop for Kalman/analog filter.
!
! This is the top level routine of the analog filter module,
! one of the four main components of the NOAA/NCAR bias
! correction system for CMAQ forecast outputs.
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
! 2017-apr-28	analog_control.f90:
!		Isolate the main filter control and site loop.
!		Move configuration remnants to read_config_file_main.f90.
! 2017-apr-29	Split off method dispatch into separate routine.
! 2017-may-17	Add support for site-specific predictor weighting.
! 2017-jun-02	Minor improvements to diagnostic prints.
! 2017-jun-15	Minor.  Fix array parameter for Intel fortran version 14.
!
! Notes:
!
! Advanced Fortran features in use:
!
! * OpenMP parallelization.
! * Allocatable arrays in derived types.
!
! Verbosity settings:
!
! 0 = errors only,   1 = milestones,    2 = brief progress,
! 3 = major details, 4 = short details, 5 = more details, etc.
!
!-----------------------------------------------------------------------------

module analog__control
contains

subroutine analog_control (filter_method, pred, obs, vmiss, target_var, &
    analog_vars, apar, fpar, kpar, wpar, pred_weights, bias_thresh_low, &
    bias_thresh_high, diag, site_ids, site_lats, site_lons, &
    uncorrected, corrected, filter_result, new_weights, new_rmse)

  use config,            only : dp
  use find__analog,      only : fpar_type
  use analog__ensemble,  only : apar_type
  use kf__luca,          only : kpar_type
  use weight__control
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

  type (apar_type), intent(in) :: apar		   ! subsystem parameter
  type (fpar_type), intent(in) :: fpar		   !   structures
  type (kpar_type), intent(in) :: kpar
  type (wpar_type), intent(in) :: wpar

  real(dp),     intent(in) :: pred_weights(:,:)    ! predictor weights
  						   !   (vars, sites)

  real(dp),     intent(in) :: bias_thresh_low(:)   ! bias thresholds (sites)
  real(dp),     intent(in) :: bias_thresh_high(:)  !   for exception sites

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

  real(dp), intent(out), allocatable :: new_weights(:,:)  ! generated weights
  							  !   (vars, sites)
  real(dp), intent(out), allocatable :: new_rmse(:)	  ! best RMSE with each
  							  ! weight set (sites)

!-------------------------------------------------
! Local variables.
!-------------------------------------------------

  type (apar_type) apar2			! modifiable copy of structure

  character fdate_str*24

  integer j, di, hi, isite, vi
  integer ndays, nhours, nvars, nsites
  integer iobs_var, ndays_show

  integer, parameter :: samp(4) = (/ 1, 2, 3, 12 /)   ! hours for data samples

!-------------------------------------------------
! Initial diagnostics.
!-------------------------------------------------

  print *, 'analog_control:  Start.'

  j = site_lats(1) + site_lons(1)	! temporary, suppress compiler warnings

  if (diag >= 3) print *, 'shape (pred) = ', shape (pred)
  if (diag >= 3) print *, 'shape (obs)  = ', shape (obs)

  nvars  = size (analog_vars)	! get dimensions
  nhours = size (pred, 2)	! number of hours in each forecast cycle
  nsites = size (pred, 4)

  ndays  = size (pred, 1)	! number of forecast cycles in data
  				! training period is 1 to ndays-1
				! last day is current forecast cycle

! Get array subscript for the target forecast variable to be corrected.

  apar2 = apar				! writeable copy for local mods

  apar2%fvar = 0
  do vi = 1, nvars
    if (analog_vars(vi) == target_var) then
      apar2%fvar = vi
      exit
    end if
  end do

  if (apar2%fvar == 0) then
    print '(2a)', '*** analog_control: Specified target variable is not in', &
      ' table of analog vars.'
    print '(2a)',        '*** Target variable  = ', trim (target_var)
    print '(999(1x,a))', '*** Analog vars      =', &
      (trim (analog_vars(j)), j = 1, nvars)
    print '(2a)', '*** Abort.'
    call exit (1)
  end if

!-----------------------------------------------------------------
! Obs and forecast model input data:
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
      print '(3i6, 5f12.4)', ((di, samp(hi), vi, pred(di, samp(hi), vi, 1:5), &
        hi = 1, size(samp)), di = 1, ndays_show)
    end do

    print *
    print *, 'Samples of obs input data:'

    iobs_var = 1

    do vi = iobs_var, iobs_var		! test only
      print *, trim (target_var) // ':'
      print '(3i6, 5f12.4)', ((di, samp(hi), vi, obs(di, samp(hi), vi, 1:5), &
        hi = 1, size(samp)), di = 1, ndays_show)
    end do
  end if

!-------------------------------------------------
! Initialize for main loop.
!-------------------------------------------------

  if (diag >= 2) print *
  if (diag >= 2) print *, 'Allocate analog_control result arrays.'

  allocate (filter_result(ndays, nhours, nsites))	! DHS
  allocate (new_weights(nvars, nsites))			! VS
  allocate (new_rmse(nsites))				! S

  if (diag >= 3) print *, 'Main allocates complete.'

  call fdate (fdate_str)
  print *
  print '(2a)', fdate_str, '  analog_control: Run ' // trim (filter_method) &
    // ' filter for each site.'

!-------------------------------------------------
! Main site loop.
!-------------------------------------------------

!$omp parallel do				! START PARALLEL ZONE

site_loop: &
  do isite = 1, nsites

    if (  (diag >= 2 .and. (mod (isite, 100) == 0 .or. isite == nsites) ) &
     .or. (diag >= 3) ) then
      print *, '*** site index = ', isite
    end if

! Call the next lower filter layer for the current site.
! This routine must be parallel compatibile.
! Arguments isite and site_id are for diagnostics only.

    call weight_control (filter_method, obs(:,:,iobs_var,isite), &
      pred(:,:,:,isite), vmiss, apar2, fpar, kpar, wpar, &
      pred_weights(:,isite), bias_thresh_low(isite), bias_thresh_high(isite), &
      isite, site_ids(isite), diag, filter_result(:,:,isite), &
      new_weights(:,isite), new_rmse(isite))

  end do site_loop

!$omp end parallel do				! END PARALLEL ZONE

  call fdate (fdate_str)
  print '(2a)', fdate_str, '  analog_control: All sites complete.'

!-----------------------------------------------------------------
! Return original and corrected data for final cycle to caller.
!-----------------------------------------------------------------

  if (diag >= 2) print *
  if (diag >= 2) print *, 'analog_control: Allocate output data arrays.'

  allocate (uncorrected (nhours, nsites), corrected (nhours, nsites))

  if (diag >= 2) print '(9a)', ' analog_control: Copy uncorrected ', &
     trim (target_var), ' data to output array.'

  uncorrected(:,:) = pred(ndays, :, apar2%fvar, :)	! HS <-- DHVS

  if (diag >= 2) print '(9a)', ' analog_control: Copy bias corrected ', &
     trim (target_var), ' data to output array.'

  corrected(:,:) = filter_result(ndays, :, :)		! HS <-- DHS

! Display sample output data.

  if (diag >= 2) then
    print *
    print *, 'Samples of bias corrected output data:'
    print *, trim (target_var) // ':'
    print '(i6, 5f12.4)', (hi, corrected(hi, 1:5), hi = 1, 8)
    print *
  end if

  if (diag >= 3) print *, 'analog_control:  Done.'

end subroutine analog_control
end module analog__control
