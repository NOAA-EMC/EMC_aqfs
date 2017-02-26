!------------------------------------------------------------------------------
!
! anenmean_method.f90 -- Analog ensemble mean filter for one site time series.
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
! 2016-feb-04	anenmean_method.f90:
!		Top level control for AnEnMean method, single site time series.
!		Break out from main_analog.f90, PSD version 2016-feb-01.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2016-feb-15	Use simplified x_analog subroutine, instead of kf_analog.
!
!------------------------------------------------------------------------------

module anenmean__method
contains

subroutine anenmean_method (obs, pred, vmiss, apar, fpar, bias_thresh_low, &
    bias_thresh_high, isite, site_id, diag, anenmean_result)

  use config, only : dp
  use find__analog, only : fpar_type
  use x__analog
  implicit none

  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! kf_analog parameters
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  real(dp),        intent(in ) :: bias_thresh_low	! bias thresholds
  real(dp),        intent(in ) :: bias_thresh_high	!   for analogs
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: anenmean_result(:,:)	! DH - bias corr. result

! Local variables.

  type (apar_type) :: apar2
  type (fpar_type) :: fpar2

  character fdate_str*24
  integer ndays, nhours

! Unused result arrays from x_analog.  See subroutine docs.

  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs
  real(dp), allocatable :: analog_in_an(:,:,:)	! nearest analogs found

! Start AnEnMean method.

  if (diag >= 3) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  anenmean_method, site', isite, ': Start.'
  end if

! Conservative.  Make writeable copies of parameter structures,
! to avoid parallel conflicts.
!
! * 2016-feb-4:  Possible compiler bugs in parallel support for structures.
! * Assume lower levels might modify any of these structures.

  apar2 = apar
  fpar2 = fpar

! Allocate dummy result arrays for single site.

  ndays  = size (pred, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (pred, 2)		! number of hours in each forecast cycle

  allocate (ianalog(ndays, nhours, apar2%num_an))	! DHA
  allocate (analog_in_an(ndays, nhours, apar2%num_an))	! DHA

! Set up and apply Analog Ensemble filter.

  apar2%start_stat = ndays		! production mode: bias correct only
  					!   the final date in input arrays

  call x_analog (obs, pred, vmiss, apar2, fpar2, bias_thresh_low, &
    bias_thresh_high, isite, site_id, diag, anenmean_result, Ianalog, &
    analog_in_an)			! last three are outputs

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  anenmean_method, site', isite, ': Return.'
  end if

end subroutine anenmean_method
end module anenmean__method
