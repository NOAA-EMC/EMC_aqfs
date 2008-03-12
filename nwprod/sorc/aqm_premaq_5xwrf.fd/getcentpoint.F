SUBROUTINE getcentpoint (nx, ny, latin1, latin2, dx, dy, la1, lo1,  &
                         centlat, centlon, xn)

!-------------------------------------------------------------------------------
! Name:     Get Center Point
! Purpose:  Calcluates latitude and longitude coordinates of center point of
!           domain from variables available in the GRIB header.
! Revised:  13 Jan 2003  Original version.  (G. Pouliot and T. Otte)
!           30 Mar 2004  Modified calculations of IXCENT, IYCENT, XCENT3D,
!                        YCENT3D, CENTLON, and CENTLAT to reduce imprecision
!                        in machine binary solution.  (T. Otte)
!           30 Apr 2004  Capture cone factor for output.  (T. Otte)
!-------------------------------------------------------------------------------

  USE fdesc3
  USE gridinfo

  IMPLICIT NONE

  REAL(8),       INTENT(OUT)   :: centlat
  REAL(8),       INTENT(OUT)   :: centlon
  REAL,          INTENT(IN)    :: dx
  REAL,          INTENT(IN)    :: dy
  REAL(8)                      :: easting
  REAL(8)                      :: ec
  REAL(8)                      :: esqr
  REAL(8)                      :: f
  INTEGER                      :: ixcent
  INTEGER                      :: iycent
  REAL(8)                      :: lambda
  REAL(8)                      :: lambda0
  REAL,          INTENT(IN)    :: la1
  REAL,          INTENT(IN)    :: latin1
  REAL,          INTENT(IN)    :: latin2
  REAL,          INTENT(IN)    :: lo1
  REAL(8)                      :: m1
  REAL(8)                      :: m2
  REAL(8)                      :: n
  REAL(8)                      :: northing
  INTEGER,       INTENT(IN)    :: nx
  INTEGER,       INTENT(IN)    :: ny
  REAL(8)                      :: phi
  REAL(8)                      :: phi1
  REAL(8)                      :: phi2
  REAL(8),       PARAMETER     :: pi         = 3.14159265358979323846d0
  REAL(8)                      :: r
  REAL(8)                      :: rf
  REAL(8)                      :: t
  REAL(8)                      :: t1
  REAL(8)                      :: t2
  REAL(8)                      :: tf
  REAL(8)                      :: theta
  REAL(8)                      :: theta0
  REAL,          INTENT(OUT)   :: xn

  REAL(8),       PARAMETER     :: deg2rad    = pi / 1.8d2

!-------------------------------------------------------------------------------
! Fill header variables that can be generally "mapped" from incoming variables.
!-------------------------------------------------------------------------------

  ncols3d = nx
  nrows3d = ny
  p_alp3d = DBLE(latin1)
  p_bet3d = DBLE(latin2)
  xcell3d = DBLE(dx) * 1.0d3
  ycell3d = DBLE(dy) * 1.0d3

!-------------------------------------------------------------------------------
! Set eccentricity of earth.  Since we are using a spherical earth, the
! eccentricity is 0.0.  Define these variables anyway to leave code general.
!-------------------------------------------------------------------------------

  ec   = 0.0d0
  esqr = ec * ec

!-------------------------------------------------------------------------------
! Fill local variables for lower-left latitude and longitude, and two true
! latitudes from input values.  Convert to radians.
!-------------------------------------------------------------------------------
        
  phi    = DBLE(la1)    * deg2rad
  lambda = DBLE(lo1)    * deg2rad
  phi1   = DBLE(latin1) * deg2rad
  phi2   = DBLE(latin2) * deg2rad

!-------------------------------------------------------------------------------
! Calculate center latitude and longitude from input information.
!-------------------------------------------------------------------------------
        
  m1 = DCOS(phi1) / DSQRT( 1.0d0 - esqr * DSIN(phi1) * DSIN(phi1) )
  m2 = DCOS(phi2) / DSQRT( 1.0d0 - esqr * DSIN(phi2) * DSIN(phi2) )

  t  = DTAN( pi / 4.0d0 - phi  / 2.0d0 ) /  &
       ( (1.0d0 - ec * DSIN(phi )) / (1.0d0 + ec * DSIN(phi )) )**(ec/2.0d0)
  t1 = DTAN( pi / 4.0d0 - phi1 / 2.0d0 ) /  &
       ( (1.0d0 - ec * DSIN(phi1)) / (1.0d0 + ec * DSIN(phi1)) )**(ec/2.0d0)
  t2 = DTAN( pi / 4.0d0 - phi2 / 2.0d0 ) /  &
       ( (1.0d0 - ec * DSIN(phi2)) / (1.0d0 + ec * DSIN(phi2)) )**(ec/2.0d0)

  n = (DLOG(m1)-DLOG(m2)) / (DLOG(t1)-DLOG(t2))
  f = m1 / (n * t1**n)
  r = a * f * t**n

  easting  = -xcell3d / 2.0d0 * DBLE(ncols3d-1)
  northing = -ycell3d / 2.0d0 * DBLE(nrows3d-1)
  theta    = DASIN( easting / r)
  lambda0  = lambda - ( theta / n )
  rf       = northing + r*COS(theta)
  tf       = (rf/(a*f))**(1.0d0/n)
  theta0   = -2.0d0 * DATAN(tf) + ( pi/2.0d0 )

  ixcent = NINT( (lambda0/deg2rad) * 100 )
  iycent = NINT( ( theta0/deg2rad) * 100 )

!-------------------------------------------------------------------------------
! Fill last of the header variables with calculated values.
!-------------------------------------------------------------------------------

  xcent3d = DBLE(ixcent) / 1.0d2
  ycent3d = DBLE(iycent) / 1.0d2
  xorig3d = easting
  yorig3d = northing
  p_gam3d = xcent3d

  centlon = DBLE(ixcent) / 1.0d2
  centlat = DBLE(iycent) / 1.0d2

  xn      = REAL(n)

END SUBROUTINE getcentpoint
