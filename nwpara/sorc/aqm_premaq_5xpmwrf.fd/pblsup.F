SUBROUTINE pblsup

!-------------------------------------------------------------------------------
! Name:     PBL Supplement
! Purpose:  Computes 2-D parameters needed for CMAQ but not provided by
!           the meteorology model (i.e., XWSTAR, XTEMP10, XTEMP1P5, XWIND10,
!           and XALBEDO).
! Notes:    Liberally adapated from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Enabled last row and column of XUU and XVV to be
!                        used in calculation of UNS and VNS.  (T. Otte)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           26 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           13 Apr 2004  Added DEALLOCATE for UNS and VNS.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  Removed calculations for near-surface
!                        temperature and wind.  (T. Otte)
!           24 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct,
!                        opimized the code by removing unnecessary array 
!                        structure and array access.
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const
  USE const_pbl
  USE metinfo
  USE parms3, ONLY: badval3

  IMPLICIT NONE

  REAL,          PARAMETER     :: amolmini   = 1.0 / amolmin
  INTEGER                      :: c
  INTEGER                      :: cm1
  REAL                         :: cpair
  REAL,          PARAMETER     :: ep1        = 0.608
  REAL                         :: hfx
  INTEGER                      :: ilu
  INTEGER                      :: k
  REAL                         :: lv
  REAL                         :: p10
  REAL                         :: p1p5
! REAL,    SAVE, ALLOCATABLE   :: pl         ( : )
  REAL                         :: pl
  CHARACTER*16,  PARAMETER     :: pname      = 'PBLSUP'
! REAL,    SAVE, ALLOCATABLE   :: ql         ( : )
  REAL                         :: ql
  INTEGER                      :: r
  INTEGER                      :: rm1
  REAL                         :: theta1
  REAL                         :: theta2
  REAL                         :: thetav1
! REAL,    SAVE, ALLOCATABLE   :: tl         ( : )
  REAL                         :: tstv
  REAL                         :: u2
  REAL,    SAVE, ALLOCATABLE   :: ul         ( : )
  REAL,    SAVE, ALLOCATABLE   :: uns        ( : , : , : )
! REAL                         :: ust
  REAL,    SAVE, ALLOCATABLE   :: vns        ( : , : , : )
  REAL                         :: wvflx
! REAL,    SAVE, ALLOCATABLE   :: zl         ( : )
! REAL                         :: zruf
  REAL                         :: ztemp

  INTERFACE

    SUBROUTINE getpblht (c, r, wspd)
      IMPLICIT NONE
      INTEGER,       INTENT(IN)    :: c
      INTEGER,       INTENT(IN)    :: r
      REAL,          INTENT(IN)    :: wspd       ( : )
    END SUBROUTINE getpblht

  END INTERFACE

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  One sign is that the XUSTAR array
! will contain all 0.0 values.  In that case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  IF ( MAXVAL(xustar) == 0.0 ) THEN  ! assume initialization period

    xwstar(:,:)   = 0.0
    xmol(:,:)     = badval3   ! inverse taken in metcro.F

  ELSE

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

!   IF ( .NOT. ALLOCATED ( pl ) ) ALLOCATE ( pl ( metlay ) )
!   IF ( .NOT. ALLOCATED ( ql ) ) ALLOCATE ( ql ( metlay ) )
!   IF ( .NOT. ALLOCATED ( tl ) ) ALLOCATE ( tl ( metlay ) )
    IF ( .NOT. ALLOCATED ( ul ) ) ALLOCATE ( ul ( metlay ) )
    IF ( .NOT. ALLOCATED ( uns ) )  &
      ALLOCATE ( uns ( ncols_x, nrows_x,   metlay ) )
    IF ( .NOT. ALLOCATED ( vns ) )  &
      ALLOCATE ( vns ( ncols_x, nrows_x,   metlay ) )
!   IF ( .NOT. ALLOCATED ( zl ) ) ALLOCATE ( zl ( metlay ) )

!-------------------------------------------------------------------------------
! Compute wind components on cross points.
!-------------------------------------------------------------------------------

!$omp parallel do
    DO k = 1, metlay

      DO r = 2, nrows_x-1
        rm1 = r - 1
        DO c = 2, ncols_x-1
          cm1 = c - 1

          uns(c,r,k) = 0.25 * ( xuu(c,r,  k) + xuu(cm1,r,  k) +  &
                                xuu(c,rm1,k) + xuu(cm1,rm1,k) )

          vns(c,r,k) = 0.25 * ( xvv(c,r,  k) + xvv(cm1,r,  k) +  &
                                xvv(c,rm1,k) + xvv(cm1,rm1,k) )
        ENDDO
      ENDDO

      DO c = 2, ncols_x-1

        uns(c,1,      k) = 0.5 * ( xuu(c,1,        k) + xuu(cm1,1,        k) )
        uns(c,nrows_x,k) = 0.5 * ( xuu(c,nrows_x-1,k) + xuu(cm1,nrows_x-1,k) )

        vns(c,1,      k) = 0.5 * ( xvv(c,1,k)         + xvv(cm1,1,k) )
        vns(c,nrows_x,k) = 0.5 * ( xvv(c,nrows_x-1,k) + xvv(cm1,nrows_x-1,k) )

      ENDDO

      DO r = 2, nrows_x-1

        uns(1,      r,k) = 0.5 * ( xuu(1,        r,k) + xuu(1,        rm1,k) )
        uns(ncols_x,r,k) = 0.5 * ( xuu(ncols_x-1,r,k) + xuu(ncols_x-1,rm1,k) )

        vns(1,      r,k) = 0.5 * ( xvv(1,        r,k) + xvv(1,        rm1,k) )
        vns(ncols_x,r,k) = 0.5 * ( xvv(ncols_x-1,r,k) + xvv(ncols_x-1,rm1,k) )

      ENDDO

      uns(1,      1,      k) = xuu(1,        1,        k)
      uns(1,      nrows_x,k) = xuu(1,        nrows_x-1,k)
      uns(ncols_x,nrows_x,k) = xuu(ncols_x-1,nrows_x-1,k)
      uns(ncols_x,1,      k) = xuu(ncols_x-1,1,        k)

      vns(1,      1,      k) = xvv(1,        1,        k)
      vns(1,      nrows_x,k) = xvv(1,        nrows_x-1,k)
      vns(ncols_x,nrows_x,k) = xvv(ncols_x-1,nrows_x-1,k)
      vns(ncols_x,1,      k) = xvv(ncols_x-1,1,        k)

    ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Compute additional variables.
!-------------------------------------------------------------------------------

!$omp parallel do
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        DO k = 1, metlay

          ul(k) = MAX( 0.5, SQRT( (uns(c,r,k) * uns(c,r,k)) +   &
                                  (vns(c,r,k) * vns(c,r,k)) ) )

!         zl(k) = x3htm  (c,r,k)
!         tl(k) = xtempm (c,r,k)
!         ql(k) = xdenswm(c,r,k) / ( xdensam(c,r,k) - xdenswm(c,r,k) )
!         pl(k) = xpresm (c,r,k) / 100.0   ! [hPa]

        ENDDO

        pl = xpresm (c,r,1) / 100.0   ! [hPa]
        ql = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )

        ! Estimate temperatures at 1.5 m and 10 m.

!       zruf   = xzruf(c,r)
!       cpair  = 1004.67 * (1.0 + 0.84 * ql(1))   ! [J / kg K]

        cpair  = 1004.67 * (1.0 + 0.84 * ql)      ! [J / kg K]
        hfx    = -xhfx(c,r) / (xdensam(c,r,1) * cpair)

!       ust    = xustar(c,r)
!       theta1 = tl(1) * (1000.0/pl(1))**0.286

        theta1 = xtempm(c,r,1) * (1000.0/pl)**0.286

        ! Calculate Monin-Obukhov length.

!       thetav1   = theta1 * (1.0 + ep1 * ql(1))            
        thetav1   = theta1 * (1.0 + ep1 * ql)

        lv        = lv0 - dlvdt * (xtempg(c,r) -  stdtemp) 
        wvflx     = xqfx(c,r) / lv

!       tstv      = (hfx * (1.0 + ep1 * ql(1)) + ep1 * theta1 * wvflx) /  & 
        tstv      = (hfx * (1.0 + ep1 * ql) + ep1 * theta1 * wvflx) /  & 
                     xustar(c,r)
        xmol(c,r) = thetav1 * xustar(c,r) * xustar(c,r) /  &
                    (vkar * grav * tstv)
        xmol(c,r) = SIGN( MAX(ABS(xmol(c,r)), amolmini), xmol(c,r) )

        ! Need to supply PBL height if PBL scheme suggests PBL height of 0.0.

        IF ( xpbl(c,r) == 0.0 ) THEN
          CALL getpblht (c, r, ul)
        ENDIF

        ! Compute W-star.

        if ( xmol(c,r) >= 0.0 ) then
           xwstar(c,r) = 0.0
        else
           xwstar(c,r) = xustar(c,r) * ( xpbl(c,r) /   &
                         ( vkar * ABS( xmol(c,r) ) ) )**0.3333333
        endif

      ENDDO
    ENDDO

!$omp end parallel do

!-------------------------------------------------------------------------------
! Dellocate variables.
!-------------------------------------------------------------------------------

!   DEALLOCATE ( pl  )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( ql  )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( tl  )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( ul  )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( uns )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( vns )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( zl  )  ! commented out to avoid memory fragmentation

  ENDIF

END SUBROUTINE pblsup
