MODULE metinfo

!-------------------------------------------------------------------------------
! Name:     Meteorology Information
! Purpose:  Contains information about the input meteorology fields.
! Revised:  10 Apr 2003  Original version.  (T. Otte)
!           04 Dec 2003  Removed unused variable MET_SEASON.  (T. Otte)
!-------------------------------------------------------------------------------

  ! Input data format.

  ! Grid information.

  INTEGER, PARAMETER :: maxmetlays = 100  ! maximum input vertical layers

  INTEGER            :: nh         ! number of vertical layers for height field
  INTEGER            :: nx         ! east-west dimension
  INTEGER            :: ny         ! north-south dimension
  INTEGER            :: nz         ! number of vertical layers

  ! Non-hydrostatic base state variables.

  INTEGER            :: met_inhyd  ! 1=non-hyd, 0=hyd
  REAL               :: met_p00    ! non-hyd ref pressure [Pa]
  REAL               :: met_tiso   ! non-hyd ref isothermal temp [K]
  REAL               :: met_tlp    ! non-hyd ref lapse [K/500 hPa]
  REAL               :: met_ts0    ! non-hyd ref sfc temp [K]

  ! Date and time information.

  CHARACTER*24       :: met_startdate   ! YYYY-MM-DD-HH:MM:SS.SSSS

END MODULE metinfo
