MODULE const

!-------------------------------------------------------------------------------
! Name:     Constants
! Purpose:  Contains fundamental constants for air quality modeling.
!           Sets universal constants for all CMAQ programs.
! Notes:    - Initial code is CONST.EXT in EPA's CMAQ system.
!           - Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! References:
!
!      CRC76,        "CRC Handbook of Chemistry and Physics (76th Ed)",
!                     CRC Press, 1995 
!      Hobbs, P.V.   "Basic Physical Chemistry for the Atmospheric Sciences",
!                     Cambridge Univ. Press, 206 pp, 1995.  
!      Snyder, J.P., "Map Projections-A Working Manual, U.S. Geological Survey
!                     Paper 1395 U.S.GPO, Washington, DC, 1987.
!      Stull, R. B., "An Introduction to Bounday Layer Meteorology", Kluwer, 
!                     Dordrecht, 1988
!-------------------------------------------------------------------------------


! Geometric Constants:

  ! pi  (single precision: 3.141593)
  REAL*8,        PARAMETER     :: pi = 3.14159265358979324

  ! pi/180 [ rad/deg ]
  REAL*8,          PARAMETER     :: pi180 = pi / 180.0


! Geodetic Constants:
 
  ! radius of earth [ m ]
  ! -- radius of sphere having same surface area as Clarke ellipsoid of 1866
  !    (Source: Snyder, 1987)
  REAL,          PARAMETER     :: rearth = 6370997.0

  ! length of a sidereal day [ sec ]  (Source:  CRC76, pp. 14-6) 
  REAL,          PARAMETER     :: siday = 86164.09

  ! mean gravitational acceleration [ m/sec**2 ]
  ! --  mean of polar and equatorial values  (Source:  CRC76, pp. 14-6)
  REAL,          PARAMETER     :: grav = 9.80622

  ! latitude degrees to meters
  REAL,          PARAMETER     :: dg2m = rearth * pi180

  ! Solar Constant  [ W/m**2 ]  (Source:  CRC76, pp. 14-2)
  REAL,          PARAMETER     :: solcnst = 1373.0


! Fundamental Constants: (Source: CRC76, pp. 1-1 to 1-6)

  ! Avogadro's Constant [ number/mol ]
  REAL,          PARAMETER     :: avo = 6.0221367e23

  ! universal gas constant [ J/mol-K ]
  REAL,          PARAMETER     :: rgasuniv = 8.314510

  ! standard atmosphere [ Pa ]
  REAL,          PARAMETER     :: stdatmpa = 101325.0

  ! standard temperature [ K ]
  REAL,          PARAMETER     :: stdtemp = 273.15

  ! Stefan-Boltzmann [ W/(m**2 K**4) ]
  REAL,          PARAMETER     :: stfblz = 5.67051e-8


! Non-MKS:

  ! Molar volume at STP [ L/mol ] Non MKS units
  REAL,          PARAMETER     :: molvol = 22.41410


! Atmospheric Constants: 

  ! mean molecular weight for dry air [ g/mol ]
  ! -- 78.06% N2, 21% O2, and 0.943% A on a mole fraction basis
  !    (Source: Hobbs, 1995, pp. 69-70)
  REAL,          PARAMETER     :: mwair = 28.9628

  ! dry-air gas constant [ 287.07548994 J/kg-K ]
  REAL,          PARAMETER     :: rdgas = 1.0e3 * rgasuniv / mwair

  ! mean molecular weight for water vapor [ g/mol ]
  REAL,          PARAMETER     :: mwwat = 18.0153

  ! gas constant for water vapor [ 461.52492604 J/kg-K ]
  REAL,          PARAMETER     :: rwvap = 1.0e3 * rgasuniv / mwwat

  ! FSB NOTE: CPD, CVD, CPWVAP and CVWVAP are calculated assuming dry air and
  ! water vapor are classical ideal gases, i.e. vibration does not contribute
  ! to internal energy.

  ! specific heat of dry air at constant pressure [ 1004.7642148 J/kg-K ]
  REAL,          PARAMETER     :: cpd = 7.0 * rdgas / 2.0

  ! specific heat of dry air at constant volume [ 717.68872485 J/kg-K ]
  REAL,          PARAMETER     :: cvd = 5.0 * rdgas / 2.0

  ! specific heat for water vapor at constant pressure [ 1846.0997042 J/kg-K ]
  REAL,          PARAMETER     :: cpwvap = 4.0 * rwvap

  ! specific heat for water vapor at constant volume [ 1384.5747781 J/kg-K ]
  REAL,          PARAMETER     :: cvwvap = 3.0 * rwvap

  ! vapor press of water at 0 C [ Pa ]  (Source: CRC76 pp. 6-15)
  REAL,          PARAMETER     :: vp0 = 611.29

  ! The following values are taken from p. 641 of Stull (1988):

  ! latent heat of vaporization of water at 0 C [ J/kg ]
  REAL,          PARAMETER     :: lv0 = 2.501e6

  ! Rate of change of latent heat of vaporization w.r.t. temperature [ J/kg-K ]
  REAL,          PARAMETER     :: dlvdt = 2370.0

  ! latent heat of fusion of water at 0 C [ J/kg ]
  REAL,          PARAMETER     :: lf0 = 3.34e5

END MODULE const
