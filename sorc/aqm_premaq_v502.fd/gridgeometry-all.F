SUBROUTINE gridgeometry (inner, f, smap, xlon, xlat)

!-------------------------------------------------------------------------------
! Name:     Grid Geometry
! Purpose:  Calculates latitude, longitude, map-scale factors, and Coriolis
!           parameter based on map projection information.
! Notes:    Liberally adapted from NCAR's MM5/TERRAIN/src/terrestial_info.F.
!           Assumes calculations only occur for coarsest domain in projection.
!           05 Dec 2001  Original version in TERRAIN.  (NCAR)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           26 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           21 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           28 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE gridinfo
  USE metinfo

  USE const, only  : pi, degran =>  pi180

  IMPLICIT NONE

  REAL                         :: a_km
  REAL                         :: c2
  REAL                         :: cell
  REAL                         :: cell1
  REAL                         :: cell2
  REAL                         :: cntri
  REAL                         :: cntrim
  REAL                         :: cntrj
  REAL                         :: cntrjm
  REAL(8)                      :: conv
  REAL                         :: deglat
  REAL,          INTENT(OUT)   :: f         ( : , : )
  REAL                         :: fac
  REAL                         :: flp
  REAL                         :: flpp
  INTEGER                      :: i
  INTEGER                      :: ii1
  INTEGER,       INTENT(IN)    :: inner                ! 1 = inner, 0 = outer
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: jj1
  INTEGER                      :: jx
  REAL,          PARAMETER     :: omega2    = 2.0 * 7.2921e-5  ! sidereal day
  REAL                         :: phi1
  REAL                         :: phictr
  CHARACTER*16,  PARAMETER     :: pname     = 'GRIDGEOMETRY'
  REAL                         :: pole
  REAL                         :: psi1
  REAL                         :: psix
  REAL                         :: psx
  REAL                         :: r
  REAL,          INTENT(OUT)   :: smap      ( : , : )
  REAL                         :: x
  REAL                         :: xcntr
  REAL,          INTENT(OUT)   :: xlat      ( : , : )
  REAL                         :: xlat1
  REAL,          INTENT(OUT)   :: xlon      ( : , : )
  REAL                         :: xn
  REAL                         :: y
  REAL                         :: ycntr

  EXTERNAL       ENVYN
  LOGICAL        ENVYN, AKHI_RUN
  INTEGER        ISTAT


!-------------------------------------------------------------------------------
! This process is used to accomdate the PRODGEN for HI
!-------------------------------------------------------------------------------

  AKHI_RUN = ENVYN ('AKHI_RUN', 'USE DIFF EARTH R ?', .FALSE. , ISTAT)

!-------------------------------------------------------------------------------
! Calculate geometrical conversion factors.
!-------------------------------------------------------------------------------

  conv   = 1.0d0 / degran

  if ( .not. AKHI_RUN ) then
     a_km   = REAL ( a  / 1.0d3 )
  else
     a_km   = REAL ( ahi/ 1.0d3 )
  endif

!-------------------------------------------------------------------------------
! Set array dimensions.
!-------------------------------------------------------------------------------

  IF ( ( SIZE(f) /= SIZE(smap) ) .OR. ( SIZE(f) /= SIZE(xlat) ) .OR.  &
       ( SIZE(f) /= SIZE(xlon) ) ) GOTO 8000

  ix = SIZE(f,1)  ! <--- Does not matter which array is used here.
  jx = SIZE(f,2)  !      They are all the same size.

!-------------------------------------------------------------------------------
! Calculate cone factor based on projection.
!-------------------------------------------------------------------------------

  IF ( map%cenlat < 0.0 ) THEN
    fac = -1.0  ! Southern Hemisphere
  ELSE
    fac =  1.0  ! Northern Hemisphere
  ENDIF

  IF ( map%igrid == 1 ) THEN  ! Mercator

    xn = 0.0

  ELSE IF ( map%igrid == 3 ) THEN  ! Lambert conformal

    xn = ALOG10( COS(map%truelat1 / REAL(conv)) ) -  &
         ALOG10( COS(map%truelat2 / REAL(conv)) )
    xn = xn / (ALOG10( TAN((45.0 - fac*map%truelat1/2.0) / REAL(conv))) -  &
               ALOG10( TAN((45.0 - fac*map%truelat2/2.0) / REAL(conv))))

  ELSE IF ( map%igrid == 5 ) THEN  ! polar stereographic

    xn = 1.0

  ELSE

    GOTO 8100

  ENDIF

!-------------------------------------------------------------------------------
! Get geometrical parameters based on map projection.
!-------------------------------------------------------------------------------

  pole   = 90.0 * fac

  cntrim = nx / 2.0 + 0.5
  cntrjm = ny / 2.0 + 0.5

  cntri  = cntrim - 0.5*FLOAT(inner)
  cntrj  = cntrjm - 0.5*FLOAT(inner)

  IF ( map%igrid /= 1 ) THEN  ! NOT Mercator

    psi1 = ( 90.0 - fac*map%truelat1 ) * degran
    psx  = ( 90.0 - fac*map%cenlat   ) * degran

    IF ( map%igrid == 5 ) THEN  ! polar stereographic
      cell  = a_km * SIN(psx) / xn
      cell2 = (1.0 + COS(psi1)) / (1.0 + COS(psx))
    ELSE IF ( map%igrid == 3 ) THEN  ! Lambert conformal
      cell  = a_km * SIN(psi1) / xn
      cell2 = ( TAN(psx/2.0) ) / ( TAN(psi1/2.0) )
    ENDIF

    r     = cell * (cell2)**xn
    xcntr = 0.0
    ycntr = -r

  ELSE  ! Mercator true at PHI1

    phi1   = 0.0
    phi1   = phi1 * degran
    c2     = a_km * COS(phi1)
    xcntr  = 0.0
    phictr = map%cenlat * degran
    cell   = COS(phictr) / (1.0 + SIN(phictr))
    ycntr  = -c2 * ALOG(cell)

  ENDIF

!-------------------------------------------------------------------------------
! Calculate latitude, longitude, map-scale factors, and Coriolis parameter.
!-------------------------------------------------------------------------------

  ii1 = ix - inner
  jj1 = jx - inner

!$omp parallel do

  DO j = 1, jj1

    y = ycntr + (j-cntrj)*map%dy

    DO i = 1, ii1

      x = xcntr + (i-cntri)*map%dx

      IF ( map%igrid /= 1 ) THEN  ! NOT Mercator

        r = SQRT(x*x + y*y)
        IF ( map%cenlat < 0.0 ) r = -r

        IF ( y == 0.0 ) THEN
          IF ( x >= 0.0 ) THEN
            flp =  90.0 * degran
          ELSE
            flp = -90.0 * degran
          ENDIF
        ELSE
          IF ( map%cenlat < 0.0 ) THEN
            flp = ATAN2(x,y)
          ELSE
            flp = ATAN2(x,-y)
          ENDIF
        ENDIF

        flpp = flp / xn / degran + map%cenlon
        IF ( flpp >  180.0 ) flpp = flpp - 360.0
        IF ( flpp < -180.0 ) flpp = flpp + 360.0
        xlon(i,j) = flpp

        IF ( map%igrid == 3 ) THEN  ! Lambert conformal
          cell  = r * xn / (a_km * SIN(psi1))
          cell1 = TAN(psi1/2.0) * cell**(1.0/xn)
        ELSE IF ( map%igrid == 5 ) THEN  ! polar stereographic
          cell  = r / a_km
          cell1 = cell / (1.0 + COS(psi1))
        ENDIF

        cell2     = ATAN(cell1)
        psx       = 2.0 * cell2 / degran
        xlat(i,j) = pole - psx
        xlat1     = xlat(i,j) * degran
        psix      = psx * degran

        IF ( map%igrid == 3 ) THEN  ! Lambert conformal
          f(i,j)    = omega2 * SIN(xlat1)
          smap(i,j) = (SIN(psi1) / SIN(psix)) *  &
                      ( (TAN(psix/2.0) / TAN(psi1/2.0))**xn )
        ELSE IF ( map%igrid == 5 ) THEN  ! polar stereographic
          f(i,j)    = omega2 * SIN(xlat1)
          smap(i,j) = ( (1.0 + COS(psi1)) / (1.0 + COS(psix)) )**xn
        ENDIF

      ELSE IF ( map%igrid == 1 ) THEN  ! Mercator

        xlon(i,j) = map%cenlon + ((x-xcntr) / c2) / degran
        IF ( xlon(i,j) >  180.0 ) xlon(i,j) = xlon(i,j) - 360.0
        IF ( xlon(i,j) < -180.0 ) xlon(i,j) = xlon(i,j) + 360.0

        cell      = EXP(y/c2)
        xlat(i,j) = 2.0 * (ATAN(cell) / degran) - 90.0
        deglat    = xlat(i,j) * degran
        smap(i,j) = COS(phi1) / COS(deglat)
        f(i,j)    = omega2 * SIN(deglat)

      ENDIF

    ENDDO
  ENDDO

!$omp end parallel do

!-------------------------------------------------------------------------------
! If this calculation is for the inner staggered domain, fill last row and
! column with data buffered from previous row and column.  These data are
! non-physical, so they will not be used.  However, for visualization purposes,
! "reasonable" values are used so the array bounds (max and min) are not thrown
! off by an unreasonably large (positive or negative) value.
!-------------------------------------------------------------------------------

  IF ( inner == 1 ) THEN

       f(ix,:) =    f(ix-1,:)   ;      f(:,jx) =    f(:,jx-1)
    smap(ix,:) = smap(ix-1,:)   ;   smap(:,jx) = smap(:,jx-1)
    xlat(ix,:) = xlat(ix-1,:)   ;   xlat(:,jx) = xlat(:,jx-1)
    xlon(ix,:) = xlon(ix-1,:)   ;   xlon(:,jx) = xlon(:,jx-1)

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000)
      GOTO 1001

 8100 WRITE (6,9100) map%igrid
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GRIDGEOMETRY',               &
              /, 1x, '***   INPUT ARRAYS ARE NOT SAME SIZE',       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GRIDGEOMETRY',               &
              /, 1x, '***   UNKNOWN MAP PROJECTION ', i3,          &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE gridgeometry
