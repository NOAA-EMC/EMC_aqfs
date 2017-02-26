!------------------------------------------------------------------------------
!
! kfan_method.f90 -- Kalman/analog filter for single site forecast time series.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2016
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
! 2016-feb-04	kfan_method.f90:
!		Top level control for KFAN method for single site time series.
!		Break out from main_analog.f90, PSD version 2016-feb-04.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2016-feb-15	Use simplified x_analog subroutine, instead of kf_analog.
!
!------------------------------------------------------------------------------

module kfan__method
contains

subroutine kfan_method (obs, pred, vmiss, apar, fpar, kpar, ratio, &
    bias_thresh_low, bias_thresh_high, isite, site_id, diag, kfan_result)

  use config, only : dp
  use find__analog, only : fpar_type
  use x__analog
  use kf__luca
  implicit none

  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! kf_analog parameters
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  type(kpar_type), intent(in ) :: kpar			! Kalman filter params
  real(dp),        intent(in ) :: ratio
  real(dp),        intent(in ) :: bias_thresh_low	! bias thresholds
  real(dp),        intent(in ) :: bias_thresh_high	!   for analogs
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: kfan_result(:,:)	! DH - bias corr. result

! Local variables.

  type (apar_type) :: apar2
  type (fpar_type) :: fpar2
  type (kpar_type) :: kpar2

  character fdate_str*24
  integer ndays, nhours

  real(dp), allocatable :: anenmean_result(:,:)     ! DH

  real(dp), allocatable :: obs_flat(:)		    ! reshaping temp arrays
  real(dp), allocatable :: ensan_flat(:)
  real(dp), allocatable :: result_1d(:)

! Unused result arrays from kf_analog.  See subroutine docs.

  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs
  real(dp), allocatable :: analog_in_an(:,:,:)	! nearest analogs found

! Start AnEnMean method.

  if (diag >= 3) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  kfan_method, site', isite, ': Start.'
  end if

! Conservative.  Make writeable copies of parameter structures,
! to avoid parallel conflicts.
!
! * 2016-feb-4:  Possible compiler bugs in parallel support for structures.
! * Assume lower levels might modify any of these structures.

  apar2 = apar
  fpar2 = fpar
  kpar2 = kpar

! Allocate result arrays for kf_analog.

  ndays  = size (pred, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (pred, 2)		! number of hours in each forecast cycle

  allocate (anenmean_result(ndays, nhours))		! DH
  allocate (ianalog(ndays, nhours, apar2%num_an))	! DHA
  allocate (analog_in_an(ndays, nhours, apar2%num_an))	! DHA

! KF/AN part 1.  Compute multiple analog forecasts.

  apar2%start_stat = 1			! make analogs for all available dates

  call x_analog (obs, pred, vmiss, apar2, fpar2, bias_thresh_low, &
    bias_thresh_high, isite, site_id, diag, anenmean_result, Ianalog, &
    analog_in_an)			! last three are outputs

! KF/AN part 2.  Apply Kalman filter to AnEnMean result (analog ensemble mean).

! Convert 2-D inputs to 1-D.
! Dimensions must be transposed for kf_luca, to match Matlab.

  obs_flat   = (/ transpose (obs) /)		! (1-D) <-- (HD) <-- (DH)
  ensan_flat = (/ transpose (anenmean_result) /)

  kpar2%update = nhours				! array stride for KF/AN

  call kf_luca (obs_flat, ensan_flat, vmiss, kpar2, ratio, diag, result_1d)

  kfan_result = transpose (reshape (result_1d, (/ nhours, ndays /) ))
						! (DH) <-- (HD) <-- (1-D)

  if (diag >= 4) then
    print '(a,99(1x,i0))', '  kfan_result dims = ', shape (kfan_result)
    print '(a,99(1x,i0))', '  kf_luca 1-D size = ', size (result_1d)
    print '(a,99(1x,i0))', '  ndays, nhours    = ', ndays, nhours
  end if

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  kfan_method, site', isite, ': Return.'
  end if

end subroutine kfan_method
end module kfan__method
