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

subroutine main_analog (ens_data, obs_data, target_var, analog_vars, &
    lower_limits, upper_limits, is_circular, vmiss, diag, uncorrected, &
    corrected)

  use config, only : dp
  use find__analog, only : fpar_type
  use kf__analog
  use kf__luca, only : kpar_type
  implicit none

! Input arguments.

  real(dp),     intent(in) :: ens_data(:,:,:,:)	! multi var model forecast data
  						!   (days, hours, vars, sites)
  real(dp),     intent(in) :: obs_data(:,:,:,:)	! observational data for 1 var
  						!   (days, hours, 1 var, sites)

  character(*), intent(in) :: target_var	! bias correction target var
  character(*), intent(in) :: analog_vars(:)	! analog vars, including target
  real(dp),     intent(in) :: lower_limits(:)	! analog var parameters (V)
  real(dp),     intent(in) :: upper_limits(:)	!   (V)
  logical,      intent(in) :: is_circular(:)	!   (V)

  real(dp),     intent(in) :: vmiss		! common missing value code
  integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

  real(dp), intent(out), allocatable :: uncorrected(:,:) ! uncorrected and
  real(dp), intent(out), allocatable :: corrected(:,:)	 ! corrected forecasts
  							 ! for final day only
  							 !   (hours, sites)
! External function definition.

  integer get_free_unit

!-------------------------------------------------
! Local variables.
!-------------------------------------------------

  type (apar_type) :: apar
  type (fpar_type) :: fpar
  type (kpar_type) :: kpar

  character(200) text_dir, out_template, outname
  character fdate_str*24

  integer j, di, hi, si, vi
  integer ndays, nhours, nvars, nsites
  integer iobs_var, ndays_show
  integer outfile, iday, outday1, outday2

  integer short_period				! 1/0 switches
  integer forecast_model, forecast_obs_every	! 1/0 switches

  real(dp) ratio

  logical write_site_files, write_text_files

! Local data arrays.

  real(dp), allocatable :: ens(:,:,:)		! subsets for current site
  real(dp), allocatable :: obs(:,:)		! obs subset is single var only

! Result arrays from analog filter.  See subroutine docs.

  real(dp), allocatable :: kfan_result(:,:,:)	! Kalman-Analog result
  real(dp), allocatable :: ensan_result(:,:,:)	! bias corrected KFAN result
  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs
  real(dp), allocatable :: analog_in_an(:,:,:)	! nearest analogs found

! End of declarations.

!-------------------------------------------------
! Filter control parameters.
!-------------------------------------------------

  print *, 'Start main analog code.'

  if (diag >= 3) print *, 'shape (ens_data) = ', shape (ens_data)
  if (diag >= 3) print *, 'shape (obs_data) = ', shape (obs_data)

  nvars  = size (analog_vars)		! get dimensions
  nhours = size (ens_data, 2)		! number of hours in each forecast cycle
  nsites = size (ens_data, 4)

  ndays  = size (ens_data, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle

  write_site_files = .false.	! true = write check files for sites
  write_text_files = .false.	! true = write daily text files, test mode only

!  text_dir = 'PM_Probability'	! test file output directory
!  text_dir = 'PM_KFANout'
  text_dir = 'text'

  out_template = trim (text_dir) // '/' // 'pmtsrwind_cmaqobs_an_DDD.txt'
  				! template for test mode output files

  short_period = 0		! only for GEM
  forecast_model = 0		! only one model is used          (mandatory)
  forecast_obs_every = 1	! model forecast comes every hour (mandatory)

! Data time range.  Now determined automatically.

!!  apar%start_stat = 335  	! starting point to compute statistics
!!  				! (for November or for September only)

  apar%start_stat = ndays	! production mode: bias correct only
  				!   the final date in input arrays

!jp  apar%num_an = 10		! Number of best analogs to use for AN
  apar%num_an = 3		! Number of best analogs to use for AN
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

  kpar%update = 24		! number of time stamps per forecast
  				! CMAQ has 24 forecasts per day

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
      print '(3i6, 5f10.2)', ((di, hi, vi, ens_data(di, hi, vi, 1:5), &
      hi = 1, 3), di = 1, ndays_show)
    end do

    print *
    print *, 'Samples of obs input data:'

    iobs_var = 1

    do vi = iobs_var, iobs_var		! test only
      print *, trim (target_var) // ':'
      print '(3i6, 5f10.2)', ((di, hi, vi, obs_data(di, hi, vi, 1:5), &
        hi = 1, 3), di = 1, ndays_show)
    end do
  end if

!-------------------------------------
! Initialize for main loop.
!-------------------------------------

  if (diag >= 2) print *
  if (diag >= 1) print *, 'Allocate main_analog result arrays.'

  allocate (kfan_result(ndays, nhours, nsites))		! KFAN output arrays
  allocate (ensan_result(ndays, nhours, nsites))	! both DHES

! Two dummy output arrays for KFAN.
! Not used in this version.  Therefore, skip the site dimension.

  allocate (ianalog(ndays, nhours, apar%num_an))	! DHA
  allocate (analog_in_an(ndays, nhours, apar%num_an))	! DHA

  if (diag >= 3) print *, 'Main allocate complete.'

!---------------------------------------------------
! Main site loop for the KF/AN filter method.
!---------------------------------------------------

! Note, 2014-feb-17, Dave A:  Removed all methods that were in the
! original Matlab code, except for the target KF/AN code.  To recon-
! struct other methods, refer to main_analog_code.m version 2013-jul-9.

  call fdate (fdate_str)
  print *
  print '(2a)', fdate_str, '  main_analog: Run analog filter for each site.'

site_loop: &
  do si = 1, nsites
    if (diag >= 3) print *, '*** site index = ', si

! Subset arrays for current site.

    obs = obs_data(:, :, iobs_var, si)	! (days, hours, vars, sites)
    					! --> (days, hours)

    ens = ens_data(:, :, :,        si)	! (days, hours, vars, sites)
    					! --> (days, hours, vars)

! Compute Analog result.

    kpar%update = 1

    call kf_analog (obs, ens, vmiss, apar, fpar, kpar, ratio, diag, &
      kfan_result(:,:,si), ensan_result(:,:,si), Ianalog, analog_in_an)
      					! last four are outputs

!-----------------------------------------------------------------
! Diagnostic text file output for each site.  Test mode only.
!-----------------------------------------------------------------

    if (write_site_files) then
      write (outname, '(a,i3.3,a)') 'stns/stn', si, '.txt'

      outfile = get_free_unit ()	! allocate output unit number,
					! will be released on close file

      outday1 = apar%start_stat + 1
      outday2 = ndays

      open  (outfile, file=outname, action='write')
      write (outfile, '(f20.15)') ensan_result(outday1:outday2, :, si)
      close (outfile)
    end if

  end do site_loop

  call fdate (fdate_str)
  print '(2a)', fdate_str, '  main_analog: All sites complete.'

!-----------------------------------------------------------------
! Write AN data to daily text files.  Test mode only.
!-----------------------------------------------------------------

  if (write_text_files) then

    print *
    print *, 'main_analog: Write test output day files.'
    print *, 'Writing to ' // trim (out_template)

    outfile = get_free_unit ()		! allocate output unit number, will
					! be released on close last file
    outday1 = apar%start_stat
    outday2 = ndays

    do iday = outday1, outday2

! Create unique file name for current day.

      outname = out_template
      j = index (outname, 'DDD')		! insert the decimal day number
      write (outname(j:j+2), '(i3.3)') iday	! into the output file name

! Write text file for current day.

      print *, '  Output: ' // trim (outname)

      open  (outfile, file=outname, action='write')
      write (outfile, '(f20.15)') ensan_result(iday:iday, :, :)
      close (outfile)
    end do

  end if

!-----------------------------------------------------------------
! Return original and corrected data for final cycle to caller.
!-----------------------------------------------------------------

  if (diag >= 2) print *
  if (diag >= 2) print *, 'main_analog: Allocate output data arrays.'

  allocate (uncorrected (nhours, nsites), corrected (nhours, nsites))

  if (diag >= 2) print '(9a)', ' main_analog: Copy uncorrected ', &
     trim (target_var), ' data to output array.'

  uncorrected(:,:) = ens_data(ndays, :, apar%fvar, :)	! HS <-- DHVS

  if (diag >= 2) print '(9a)', ' main_analog: Copy bias corrected ', &
     trim (target_var), ' data to output array.'

  corrected(:,:) = ensan_result(ndays, :, :)		! HS <-- DHS

! Display sample output data.

  if (diag >= 2) then
    print *
    print *, 'Samples of bias corrected output data:'
    print *, trim (target_var) // ':'
    print '(i6, 5f10.2)', (hi, corrected(hi, 1:5), hi = 1, 5)
    print *
  end if

  if (diag >= 3) print *, 'main_analog:  Done.'

end subroutine main_analog
end module main__analog
