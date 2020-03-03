!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! --- NOTE
!     Original code: from Prof. Roland Stull UBC Group (rstull@eos.ubc.ca)
!     Written in Matlab by: Thomas Nipen, March 2005
!			    (tnnipen@interchange.ubc.ca)
!     Modified By: Luca Delle Monache, June 2005 (lucadm@ucar.edu)
!
! 2012-apr-20	kf_luca.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used unchanged by Djalalova and Wilczak, NOAA/ESRL/PSD3,
!		  for development of CMAQ bias correction method.
!
! 2014-mar-25	kf_luca.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2014-jul-15	Minor bug fix in diagnostic print.
!
! 2017-apr-28	Move ratio input argument into kpar structure.
!		Minor comment fixes.
!
! --- KF input parameters
! --- Inputs
!     obs       = array of observations
!     pred      = array of predictions
!     vmiss     = value corresponding to missing value
!     kpar      = parameters
!                 kpar.varo = variance of observation variance
!                 kpar.varp = variance prediction variance
!                 kpar.ratio = sigmas ratio
!                 kpar.iperiod = initialization period (training period)
!                 kpar.lower_limits = variable lower bounds
!                 kpar.upper_limits = variable upper bounds
!                 kpar.update = time between update (24 hours)
!                 kpar.start = start time of time series [obs pred]
!                 kpar.timeZone = timeZone of measurements (for output graphics)
!     algorithm = specifies which approach to filter
!                 1:  current operational mode, where coefficients are
!                     calculated only from data at the same time of day
!                 2:  calculates coefficients from the previous
!		      'par.update' hours
!
! --- Output
!     rmsePred  = RMSE prediction
!     rmseKF    = RMSE KF-corrected prediction
!     corrPred  = correlation prediction
!     corrKF    = correlation KF-corrected prediction
!     newKF     = Kalman filtered predictions. Time series aligned with the
!                 prediction
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module kf__luca			! standard visibility

  use config, only : dp
  implicit none

! Parameter structure for Kalman filter.

  type kpar_type

    real(dp) varo		! variance of observation variance
    real(dp) varp		! variance prediction variance
    real(dp) ratio		! KF method parameter (sigma_ratio)
    integer  iperiod		! initialization period (training period)
    integer  update		! time between update (24 hours)
    				! *** or: number of time steps per forecast
    integer  start(2)		! start time of time series [obs pred]
    integer  timezone		! timeZone of measurements, for output graphics
    integer  enforce_positive	! 1 = enforce correction for values > 0

    real(dp), allocatable :: lower_limits(:)	! variable lower bound
    real(dp), allocatable :: upper_limits(:)	! variable upper bound

  end type kpar_type

contains

!-----------------------------------------------------------
! Kalman filter routine.
!-----------------------------------------------------------

subroutine kf_luca (obs, pred, vmiss, kpar, diag, output)
  implicit none

  real(dp),         intent(in ) :: obs(:)	! input observations
  real(dp),         intent(in ) :: pred(:)	! input predictions
  real(dp),         intent(in ) :: vmiss	! common missing value
  type (kpar_type), intent(in ) :: kpar		! Kalman filter parameters
  integer,          intent(in ) :: diag		! verbosity, 0 = errors only

  real(dp), allocatable, intent(out) :: output(:)  ! Kalman filtered predictions
  						   ! same size as input pred.
! Local variables.

  integer t, t1, t2, t3, tprev, tstep, ntimes
  integer originalT, minT, maxT, hour, hour2

  logical doanalog, obs_valid, pred_valid

  real(dp) p_x, p_sigv, sigv, sigw
  real(dp) Last_Error, kalman_gain

! Automatic arrays.

  real(dp) kf(size(obs)), x(size(obs)), y(size(obs))
  real(dp) originalPred(size(obs)), newKF(size(obs))

!---------------------------------------
! Initialize.
!---------------------------------------

  if (diag >= 5) print *, '*** kf_luca: Start.'

!  if(~isfield(par, 'doanalog'))	! discrepancy in parameter name
!    par%doanalog = 0			! in Matlab version
!  end

  doanalog = .false.			! true = analog mode, only do first hour
  					! Mar 25, fixed setting for KF/AN filter
  ntimes    = size (obs)
  originalT = ntimes

  originalPred = pred			! save a copy of original pred values

! Adjust starting time of different time series.

  minT = minval (kpar%start)
  maxT = maxval (kpar%start)

  kf(:) = 0				! stores KF predictions

  x(:) = 0				! bias
  x(1) = 0				! initial bias

! Calculate the initial forecast errors.

  y(:) = pred(:) - obs(:)

! Reject analogs with error values out of range.  ldm, 14-6-2005.

  where (obs  < kpar%lower_limits(1) .or. obs  > kpar%upper_limits(1)) y = vmiss
  where (pred < kpar%lower_limits(1) .or. pred > kpar%upper_limits(1)) y = vmiss

!---------------------------------------
! Calculate coefficients.
!---------------------------------------

! sigv = "sigmasubepsilon" in the paper
! sigw = "sigmasubeta" in the paper
! Kalman_gain = "beta" in the paper

  if (doanalog) then			! analog mode, only process the first
     hour2 = 1				! hour, since all the others are bogus
  else
     hour2 = kpar%update		! non-analog, process all hours
  end if

hour_loop: &
  do hour = 1, hour2

    if (diag >= 7) print '(a,i0)', '    hour = ', hour

! Initial values.

    p_x = 1				! expected mean-square-error when KF
					! is applied to estimate bias, i.e., x

    p_sigv = 1000			! expected mean-square-error when KF is applied
					! to estimate sigma-v
    sigv = 1
    Last_Error = 0			! previous error
    kalman_gain = 1

    t1    = kpar%update+hour
    t2    = ntimes
    tstep = kpar%update

time_loop: &
    do t = t1, t2, tstep

      obs_valid  = (      obs(t)  >= kpar%lower_limits(1) &
                    .and. obs(t)  <= kpar%upper_limits(1)  )

      pred_valid = (      pred(t) >= kpar%lower_limits(1) &
                    .and. pred(t) <= kpar%upper_limits(1)  )

! Both obs and pred are valid.  Apply Kalman formula.

check_valid: &
      if (obs_valid .and. pred_valid) then

        tprev = t - kpar%update
        y(tprev) = Last_Error

        if (y(tprev) /= vmiss) then		 ! ldm, 14-6-2005
          kalman_gain = (p_sigv + kpar%varo) / (p_sigv + kpar%varo + kpar%varp)
          p_sigv = (p_sigv + kpar%varo) * (1 - kalman_gain)
          sigv = sigv + (kalman_gain * ( ((y(t) - y(tprev)) ** 2) &
                        / (2 + kpar%ratio) - sigv))
        end if

        sigw = kpar%ratio * sigv
        kalman_gain = (p_x + sigw)/(p_x + sigw + sigv)
        p_x = (p_x + sigw) * (1 - kalman_gain)
        x(t) = x(t - kpar%update) + kalman_gain *(y(t) - x(t - kpar%update))
        Last_Error = y(t)

! Obs and/or pred are missing, so use previous estimated bias,
! but increase the error p_x.

      else
         Last_Error = vmiss
         p_x = p_x + (kpar%ratio * sigv)
         p_x = min (p_x, 10000d0)
         x(t) = x(t-kpar%update)
      end if check_valid

    end do time_loop

  end do hour_loop

! Shift the bias such that the bias from the previous day is used
! when combined with the model forecasts.

  t2 = ntimes - kpar%update
  t3 = kpar%update + 1

  x(t3:ntimes) = x(1:t2)

!---------------------------------------
! Remove bias from forecast.
!---------------------------------------

  kf(:) = pred - x
  where (pred == vmiss) kf = vmiss

! Enforce positive values.

  if (kpar%enforce_positive == 1) then
    where (kf /= vmiss) kf = max (kf, 0d0)
  end if

! Readjust kalman filtered output.

!!  *** Original Matlab:
!!  newKF = originalPred;
!!  newKF(1-par%start(2)+maxT:originalT-par%start(2)+minT) = kf

  t1 = 1 - kpar%start(2) + maxT
  t2 = originalT - kpar%start(2) + minT

  newKF = originalPred(:)
  newKF(t1:t2) = kf(:)		! these better be conforming arrays
  output = newKF(:)

  if (diag >= 6) print '(a,i6,2f12.3)', 't1, pred, corrected = ', t1, &
    originalPred(t1), output(t1)

  if (diag >= 6) print '(a,i6,2f12.3)', 't2, pred, corrected = ', t2, &
    originalPred(t2), output(t2)

  if (diag >= 6) print *, '*** kf_luca: Return.'

end subroutine kf_luca
end module kf__luca
