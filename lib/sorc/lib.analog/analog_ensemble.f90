!------------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Produces forecast based on Kalman filtering prediction in analog space.
!
! 2012-apr-20	kf_analog.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used unchanged by Djalalova and Wilczak, NOAA/OAR/ESRL/PSD3,
!		  for development of CMAQ bias correction method.
!
! 2014-mar-25	kf_analog.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Remove unused ensemble dimension, omit bug and simplify.
!		Remove var dimension from only the obs(...) input array.
!		This removes an unnatural co-indexing dependency between
!		  obs and ens arrays, via apar%fvar.
!
! 2015-oct-29	Add site-specific bias threshold filter for individual analogs.
!		Fix mis-labeled error messages.
!		Minor optimization of infrequent diagnostic.
!
! 2016-jan-26	Parallel support.
!		Replace thread unsafe threshold filter with inline version.
!		Move site exception list reader up to main program.
! 2016-feb-04	Restore some return arrays, for better code compatibility.
!		Adjust comments to indicate KFAS and ANEnMean results.
!
! 2016-feb-15	x_analog.f90:
!		Simplified version of kf_analog.f90.
!		Remove unused KFAS calculations and output (so-called KFAN).
!		But keep AnEnMean calculations identical.
!
! 2017-mar-27	analog_ensemble.f90:
!		Interface changes.  Add method name protocol.
!		Add predictor weights.
!		Rename some variables for clarity.
! 2017-may-25	Add parameter apar%forecast_last_hour to limit range of
!		  computed forecasts.
!
! 2018-jan-19	Add Irina's new bias formula for present forecast only.
!		Add parameter apar%bias_formula.
!
! *** To do:
! *** After initial proving, convert reallocates to static arrays,
!     for efficiency.
!
!------------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module analog__ensemble		! standard visibility

  use config, only : dp
  implicit none

! Parameter structure for analog_ensemble.

  type apar_type

    integer   fvar		  ! array index for target var to be corrected
    integer   start_stat	  ! start day of forecasts needed to compute
    integer   forecast_last_hour  ! last forecast hour needed to compute
    integer   num_an		  ! Number of best analogs to use for AN
    integer   weight_type	  ! Type of analog weighting:
				  ! 0: Do not weight analogs
				  ! 1: Weight them by the inverse metric
				  ! 2: Weight them linearly
    integer   skipMissingAnalogs  ! Applies to both ANKF and AN
				  ! 1: Always use num_an analogs,
				  ! even if some of the best ones are missing
     real(dp) bias_thresh_low	  ! bias exclusion thresholds for analogs
     real(dp) bias_thresh_high
     character(2) bias_formula	  ! Selected bias formula:
     				  ! mb = mean (forecast + predictions) plus bias
				  ! fb = forecast plus bias
  end type apar_type

contains

!-----------------------------------------------------------
! Analog filter function.
!-----------------------------------------------------------

subroutine analog_ensemble (obs, pred, pred_weights, vmiss, apar, fpar, &
    isite, site_id, diag, ensan, Ianalog, analog_in_an)

  use config, only : dp
  use find__analog
  use ieee_arithmetic
  use kf__luca
  implicit none

  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: pred_weights(:)	! V   - var weights
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! analog_ensemble params
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: ensan(:,:)		! DH  - bias corr result
  							!   actually is AnEnMean
  integer,         intent(out) :: Ianalog(:,:,:)	! DHA - found indicies
  real(dp),        intent(out) :: analog_in_an(:,:,:)	! DHA - found analogs

! Local variables.

!!  character filename*40		! debug only

  integer a1, a2, d, h, i, i2, ilast, fvar
  integer nday, nhour, nvar, dim_analog
  integer winLower, winUpper, nanalogs
  integer num_available, num_selected, ct
  integer nan_count, count_missing

  real(dp) mean_ens, mean_obs, bias

  type(fpar_type) fpar2

! Dynamic arrays.

  integer,  allocatable :: inds(:)	! sorted indices from find_analog
  integer,  allocatable :: isequence(:), Ian(:)

  real(dp), allocatable :: obs_flat(:), pred_flat(:)
  real(dp), allocatable :: obstemp(:), predtemp(:)
  real(dp), allocatable :: an_weights(:), test_bias(:)

  real(dp), allocatable :: metric(:)	! returned metrics for selected analogs

  logical,  allocatable :: mask2(:), analog_mask(:)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 5) print '(a,i5,a)', ' *** analog_ensemble, site', isite, &
                       ': Start.'

  nday  = size (pred, 1)		! get array dimensions
  nhour = size (pred, 2)		! first two must match obs
  nvar  = size (pred, 3)

  dim_analog = size (Ianalog, 3)

  if (diag >= 6) print '(a,99(1x,i0))', &
    '  nday, nhour, nvar, dim_analog =', nday, nhour, nvar, dim_analog

  if (diag >= 6) then
    print *, 'apar ='
    print *, apar
!!    print *, 'fpar ='
!!    print *, fpar			! print not supported in gfortran 4.8.2
!!    print *, 'kpar ='
!!    print *, kpar			! print not supported in gfortran 4.8.2
  end if

  fvar  = apar%fvar			! short name for readability

  fpar2 = fpar				! local copy of find_analog parameters
  					! for loop control of time values

  ensan(:,:)          = vmiss		! clear result arrays
  analog_in_an(:,:,:) = vmiss
  Ianalog(:,:,:)      = 0

!---------------------------------------------------------------
! Main nested loop over each forcast day, and forecast hour.
!---------------------------------------------------------------

day_loop: &
  do d = apar%start_stat, nday		! only loop over target forecast days
  					! to be corrected; start of array is
					! the training period
hour_loop: &
    do h = 1, apar%forecast_last_hour	! only loop over forecast hours needed

      if (diag >= 6) print '(2(a,i0))', 'analog_ensemble: D = ', d, ', H = ', h

      fpar2%fday  = d			! override target time parameters
      fpar2%fhour = h			! only one target hour at a time,
      					! for find_analog

! Only continue if predictions for ALL variables are available.  If
! any variables are missing values, then default to the original target
! variable prediction for the current hour.  I.e., no bias correction.

      if (any (pred(d, h, :) == vmiss)) then
        if (diag >= 6) print '(3(a,i0))', &
          '*** Missing forecast values, day ', d, ', hour', h, &
          ', skipping bias correction.'

        ensan(d, h) = pred(d, h, fvar)
        cycle hour_loop
      end if

! Fix for problem with trend for the first and last hours.

! (3 trends currently required.  Needs upgrade to handle other sizes,
! but that probably wants an algorithmic approach.  How about simple
! proportional re-weighting after truncation?)

      if (size (fpar%trend) /= 3) then
        print *, &
            '*** analog_ensemble: Abort, selected trend size is not supported.'
        print *, '*** Currently there must be exactly three trend weights.'
        print '(a,i0)', ' *** Number of weights in fpar%trend = ', &
          size (fpar%trend)
        call exit (1)
      end if

      if (h == 1 .or. h == nhour) then	! first and last hours:
        fpar2%trend = (/ 1 /)		! window constrained, use single weight
      else
        fpar2%trend = fpar%trend	! middle hours: use original trends
      end if

!-----------------------------------------------------
! Find all analogs for the current forecast hour.
!-----------------------------------------------------

      if (diag >= 7) print '(a,99(1x,i0))', '  shape (pred) = ', shape (pred)

! Find flattened indicies of best analogs.
! Returned indices are sorted, with best matches at the END.

! Note, analogs are based only on model data in "pred", NOT obs.

      call find_analog (pred, pred_weights, vmiss, fpar2, fvar, diag, inds, &
            metric, winLower, winUpper)

!!      open (10, file='ianalog2')
!!      write (10, '(i10)') inds

!!      open (10, file='metric2')
!!      write (10, '(f10.2)') metric

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/ianalog.', d, '.', h
!!      open (10, file=filename)
!!      write (10, '(i10)') inds

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/metric.',  d, '.', h
!!      open (10, file=filename)
!!      write (10, '(f10.2)') metric

!! print *, '*** DEBUG STOP, ANALOG_ENSEMBLE'
!! stop

!! if (h == 14) then
!!   print '(a,i0,a,i0)', '*** DEBUG STOP, analog_ensemble, day ',d, ', hour ',h
!!   stop
!! end if

! Flatten the obs and pred arrays in the current window.
! Reallocate on assignment, as needed.

!!      len_flat = (d - 1) * (winUpper - winLower + 1)
!!      allocate (obs_flat (len_flat))		! instead, auto reallocate...
!!      allocate (pred_flat(len_flat))

      obs_flat  = (/ obs (1:d-1, winLower:winUpper)       /)	! 2-D to 1-D
      pred_flat = (/ pred(1:d-1, winLower:winUpper, fvar) /)

! Get the selected analog values.
! Append the current (d,h) prediction value as the final element.

      obstemp  = (/ obs_flat (inds), vmiss            /)
      predtemp = (/ pred_flat(inds), pred(d, h, fvar) /)

      nanalogs = size (obstemp)

!-----------------------------------------------------------
! Remove individual analogs that exceed a site-specific
! threshold for bias value.
!-----------------------------------------------------------

! Enable all analogs for current site without both specified thresholds.

bias_threshold_filter: &
      if (  apar%bias_thresh_low  == vmiss &
       .or. apar%bias_thresh_high == vmiss) then

        analog_mask = spread (.true., 1, nanalogs)

! Compute individual test bias values for site with specified thresholds.

      else
        test_bias = obstemp(:) - predtemp(:)

! Apply thresholds and create mask.
! TRUE = analog enabled, FALSE = disabled (bias outside the given thresholds).

        analog_mask =    (test_bias(:) > apar%bias_thresh_low)  &
                   .and. (test_bias(:) < apar%bias_thresh_high)

        if (diag >= 4) then			! threshold diagnostic
          print '(2a,3i6)', &
          '  site, nday, nhour, no. of threshold disabled analogs = ', &
          site_id, nday, nhour, count (analog_mask(:) .eqv. .false.)
        end if

      end if bias_threshold_filter

!-------------------------------------------------
! AN ensemble + bias computation.
!-------------------------------------------------

      isequence = (/ (i, i = 1, nanalogs) /)	! make consecutive indices

! If selected, ignore missing analogs such that we have apar%num_an
! analogs, when possible.

      if (apar%skipMissingAnalogs == 1) then

!        I = intersect(find(obstemp ~= vmiss), find(predtemp ~= vmiss))
!        I = I(max(1,length(I)-apar%num_an+1):end)

        mask2 = (obstemp /= vmiss .and. predtemp /= vmiss)

        mask2 = mask2 .and. analog_mask		! also exclude threshold rejects

        Ian = pack (isequence, mask2)		! indices of non-missing analogs

        num_available = size (Ian)			! limit to requested
        num_selected = min (num_available, apar%num_an)	! number of analogs

        i2 = num_available - num_selected + 1		! reduce index array
        if (i2 > 1) Ian = Ian(i2:)			! to size limit

        if (diag >= 7) print *, 'size (Ian) = ', size (Ian)
!!        print '(10i5)', Ian

! Otherwise, allow fewer than apar%num_an analogs, i.e. when some are missing.
! DEFERRED 2014-mar-11.  This option was not in current use.

      else
        print *,           '*** analog_ensemble: Abort.'
        print '(a,i0,a)', ' *** Option apar%skipMissingAnalogs = ', &
          apar%skipMissingAnalogs, ' is not currently supported.'
        call exit (1)

!!        I = length(predtemp)-apar%num_an:length(predtemp)-1
!!        I = intersect(I, find(obstemp ~= vmiss))
!!        I = intersect(I, find(predtemp ~= vmiss))
      end if

      ct = size (Ian)				! number of selected analogs

! Determine type of weighting.

      if (apar%weight_type == 0) then		! No weighting
        an_weights = (/ (1, i = 1, ct) /)	! trick for array of ones

      else if (apar%weight_type == 1) then	! Weighted by metric
        an_weights = 1 / (metric(Ian) ** 2)

! The weights should not be nans, because the metric for a value
! where the obs and pred are not missing should be a valid number

!!        assert(isempty(find(isnan(metric(end-ct+1:end)), 1)))

        nan_count = count (ieee_is_nan (an_weights(:)))

        if (nan_count /= 0) then
          print *, '*** analog_ensemble: NaNs detected when computing' &
            // ' weights for metrics.'
          print *, '*** Number of weights = ', ct
          print *, '*** NaN count         = ', nan_count
          call exit (1)
        end if

      else if (apar%weight_type == 2) then	! Linear weighting
        an_weights = (/ (i, i = 1, ct) /)	! 1 through N

      else
        print *, '*** analog_ensemble: Abort, weighting scheme not supported.'
        print '(a,i0)', ' *** Option apar%weight_type = ', apar%weight_type
        call exit (1)
      end if

      an_weights = an_weights / sum (an_weights)	! Normalize weights

      if (diag >= 7) print *, 'an_weights = '
      if (diag >= 7) print '(10f8.5)', an_weights

! Compute weighted mean arrays.

      if (diag >= 7) print *, 'Compute weighted mean arrays:'
      if (diag >= 7) print *, 'Ian ='
      if (diag >= 7) print '(10i6)', Ian
      if (diag >= 7) print *, 'size (Ian)        = ', size (Ian)
      if (diag >= 7) print *, 'size (an_weights) = ', size (an_weights)
      if (diag >= 7) print *, 'size (predtemp)   = ', size (predtemp)

      mean_ens = sum (predtemp(Ian) * an_weights)   ! Compute weighted ens mean

      if (diag >= 7) print *, 'size (obstemp)    = ', size (obstemp)

      mean_obs = sum (obstemp(Ian)  * an_weights)   ! Compute weighted obs mean

! Complain about missing values, but do not stop.

      if (diag >= 6) then

         if (diag >= 7) print *, 'count missing:'

         count_missing = count (obstemp(Ian) == vmiss)

         if (count_missing /= 0) then
           print *
           print '(2a,i0,2a,i0,a)', '*** analog_ensemble: obstemp(Ian)', &
                 ' contains ', count_missing, ' missing values,', &
                 ' out of a total of ', size (obstemp(Ian)), ' values.'
           print '(10f8.2)', obstemp(Ian)
         end if

      end if

!------------------------------------------------------------------
! Bias correction.  Use analogs if we have 3 or more available.
!------------------------------------------------------------------

! Thomas's method removed, 2014-mar-12.  See original Matlab, to recover.

!!    %if (ct >= 3 && metric(end) <= par%metric_thres)  % hybrid AN-ANKF, v1
!!    %if (ct >= 3 && pred(d, h, par%fvar, e) <= 7) %  hybrid AN-ANKF, v2

enough_analogs: &
      if (ct >= 3) then

        bias = mean_obs - mean_ens
        ilast = size (predtemp)

        if (apar%bias_formula == 'mb') then
          ensan(d, h) = ( (mean_ens * ct + predtemp(ilast)) / (ct + 1) ) + bias
          					! original bias formula
        else
          ensan(d, h) = predtemp(ilast) + bias	! use present forecast only
        end if

!!        %ensan(d, h) = predtemp(end) + bias;	    ! Thomas Palined...and me...
!!        %ensan(d, h) = mean_obs;		    ! The more elegant...
!!        %analog_in_an(d, h, 1:ct) = obstemp(Ian);  ! Save the analogs used
!!						     ! for AN (until 9 Jan 12)

        a1 = apar%num_an - ct + 1		! also return the analogs used
        a2 = apar%num_an
        analog_in_an(d, h, a1:a2) = obstemp(Ian)

! Otherwise default to prediction.

      else
        ilast = size (predtemp)
        ensan(d, h) = predtemp(ilast)
!!        %ensan(d, h) = kfan(d, h);		! for the hybrid method
      end if enough_analogs

    end do hour_loop
  end do day_loop

  if (diag >= 6) print *, '*** analog_ensemble: Return.'

end subroutine analog_ensemble
end module analog__ensemble
