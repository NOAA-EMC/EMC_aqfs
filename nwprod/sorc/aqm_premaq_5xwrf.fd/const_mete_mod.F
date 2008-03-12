MODULE const_mete

!-------------------------------------------------------------------------------
! Name:     Constants for Meteorology
! Purpose:  Contains derived constants for meteorology.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const

! Derived Geodetic and Geometric Constants:

  ! angular speed of earth's rotation [ 7.2921159E-5 rad/sec ]
  REAL,          PARAMETER     :: eomega = 2.0 * pi / siday

  ! conversion factor from degrees to radians [ 0.1745329252 ]
  REAL,          PARAMETER     :: deg2rad = pi / 180.0

  ! conversion factor from radians to degrees [ 57.295779513 ]
  REAL,          PARAMETER     :: rad2deg = 180.0 / pi


! Atmospheric Constants:      

  ! ratio of mol wt of water vapor to mol wt of air [ 0.62201513666 ]
  REAL,          PARAMETER     :: mvoma = mwwat /mwair

  ! gas constant over gravity [ 29.274836781 m/K ]
  REAL,          PARAMETER     :: rog = rdgas / grav

  ! gravity over gas constant [ 0.03415902905 K/m ]
  REAL,          PARAMETER     :: gor = 1.0 / rog

  ! gas constant over Cp [ 0.28571428571 ]
  REAL,          PARAMETER     :: rovcp = rdgas / cpd

  ! gas constant times Cp [ 288443.17924 ]
  REAL,          PARAMETER     :: rdcp = rdgas * cpd

  ! dry adiabatic lapse rate [ 9.7597226E-3 K/m ]
  REAL,          PARAMETER     :: dalr = grav / cpd

  ! ratio of latent heat of vaporization to specific heat [ 2489.1411967 ]
  !   - FSB The following is for cases when no correction for 
  !     temperature or moisture is made.
  REAL,          PARAMETER     :: lvocp = lv0 / cpd

END MODULE const_mete
