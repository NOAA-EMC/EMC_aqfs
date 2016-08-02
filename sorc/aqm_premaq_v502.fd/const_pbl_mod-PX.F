MODULE const_pbl

!-------------------------------------------------------------------------------
! Name:     Constants for Planetary Boundary Layer
! Purpose:  Parametric constants for describing planetary boundary layer.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  20 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           26 Nov 2003  Added minimum value for KTH from Eta.  (T. Otte)
!-------------------------------------------------------------------------------

  ! Limit at which similarity theory is allowed to ensure ( HPBL/Z0 >= 125 )

  REAL,          PARAMETER     :: similim = 0.008


  ! Surface layer similarity

  ! (1) simple - no distinction for momentum and heat
! REAL,          PARAMETER     :: vkar  =  0.4
! REAL,          PARAMETER     :: betam =  5.0
! REAL,          PARAMETER     :: betah =  5.0
! REAL,          PARAMETER     :: gamam = 16.0
! REAL,          PARAMETER     :: gamah = 16.0
! REAL,          PARAMETER     :: pro   =  1.0

  ! (2) Businger, et al. (1971)
! REAL,          PARAMETER     :: vkar  =  0.35
! REAL,          PARAMETER     :: betam =  4.7
! REAL,          PARAMETER     :: betah =  6.35
! REAL,          PARAMETER     :: gamam = 15.0
! REAL,          PARAMETER     :: gamah =  9.0
! REAL,          PARAMETER     :: pro   =  0.74

  ! (3) Hogstrom (1988)
  REAL,          PARAMETER     :: vkar  =  0.40
  REAL,          PARAMETER     :: betam =  6.00
  REAL,          PARAMETER     :: betah =  8.21
  REAL,          PARAMETER     :: gamam = 19.30
  REAL,          PARAMETER     :: gamah = 11.60
  REAL,          PARAMETER     :: pro   =  0.95


  ! Aerodynamic resistance parameter
  ! Calculate Ra according to M-O sfc layer theory like in PX LSM

  REAL,          PARAMETER     :: betam_PX =  5.00
  REAL,          PARAMETER     :: betah_PX =  5.00


  ! Limits on bulk Richardson number for surface layer similarity

  REAL,          PARAMETER     :: ricr    =  0.25  ! critical Richardson num
  REAL,          PARAMETER     :: rimax   =  0.70  ! max. bulk Richardson num
  REAL,          PARAMETER     :: rimin   = -4.75  ! min. bulk Richardson num
  REAL,          PARAMETER     :: amolmin =  1.25  ! min allowed 1.0/ABS(MOL)


  ! Boundary layer theory

  REAL,          PARAMETER     :: alamdao = 0.07
  REAL,          PARAMETER     :: ch      = 0.80
  REAL,          PARAMETER     :: aneut   = 1.70
  REAL,          PARAMETER     :: bneut   = 4.50


  ! Reference height at which log wind profile is definitely satisfied

  REAL,          PARAMETER     :: zref = 10.0  ! meters


  ! The way we estimate emissions requires PBL ht. at least 50 m to
  ! have reasonable distributions of concentrations.  DWB (NOV 07, 97)

  REAL,          PARAMETER     :: pblmax     = 5000.0   ! [m]
  REAL,          PARAMETER     :: pblmin     =   50.0   ! [m]


  ! Minimum value for eddy diffusivity.
  ! Based on minimum value used in Eta processing, per conversation
  ! with Pius Lee (SAIC/NCEP).  TLO 26 Nov 2003

  REAL,          PARAMETER     :: kthmin     = 0.01  ! [m^2/s]

END MODULE const_pbl
