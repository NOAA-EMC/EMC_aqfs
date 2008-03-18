SUBROUTINE pblpkg

!-------------------------------------------------------------------------------
! Name:     PBL Package
! Purpose:  Computes PBL height.
! Notes:    -- For "MAXK" layer meteorology new method for the estimation of
!              surface fluxes following Byun (Nov 1990, Jan 1997).
!           -- Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           26 Mar 2003  Enabled last row and column of XUU and XVV to be
!                        used in calculation of UNS and VNS.  Constrained
!                        calculation of non-PBL parameters with user
!                        option LRAD.  (T. Otte and J. Pleim)
!           11 Apr 2003  Modified for PREMAQ.  Removed radiation calculations.
!                        (T. Otte)
!           26 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           13 Apr 2004  Added DEALLOCATE for QL.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentaion.  (T. Otte)
!           24 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const
  USE const_pbl

  IMPLICIT NONE

  INTEGER                      :: c
  INTEGER                      :: cm1
  REAL                         :: cpair
  REAL                         :: dtemp
  REAL                         :: f_ptemp
  REAL                         :: f_vtemp
  REAL                         :: fint
  REAL                         :: hfx
  INTEGER                      :: k
  INTEGER                      :: kpbl
  INTEGER                      :: kpblht
  REAL                         :: press
  REAL                         :: qmix
  REAL,    SAVE, ALLOCATABLE   :: ql         ( : )  
  INTEGER                      :: r
  REAL,    SAVE, ALLOCATABLE   :: ribk       ( : )
  REAL,          PARAMETER     :: ricrit = 0.25
  INTEGER                      :: rm1
  REAL                         :: tconv
  REAL                         :: temp
  REAL                         :: thv1
  REAL                         :: tog
  REAL,    SAVE, ALLOCATABLE   :: uns        ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: vns        ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: vptempk    ( : )
  REAL                         :: wssq
  REAL                         :: wss

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Virtual temperature [K]

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )


  ! Potential temperature [K]

  f_ptemp(temp,press) = temp * ( 100000.0 / press )**(2.0/7.0)

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------
 
  IF ( .NOT. ALLOCATED ( ql      ) )  &
    ALLOCATE ( ql                           ( metlay ) )
  IF ( .NOT. ALLOCATED ( ribk    ) )  &
    ALLOCATE ( ribk                         ( metlay ) )
  IF ( .NOT. ALLOCATED ( vptempk ) )  &
    ALLOCATE ( vptempk                      ( metlay ) )
  IF ( .NOT. ALLOCATED ( uns     ) )  &
    ALLOCATE ( uns     ( ncols_x, nrows_x,    metlay ) )
  IF ( .NOT. ALLOCATED ( vns     ) )  &
    ALLOCATE ( vns     ( ncols_x, nrows_x,    metlay ) )

!-------------------------------------------------------------------------------
! Compute wind speed profile from true horizontal wind data.
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
! Calculate PBL height.
!-------------------------------------------------------------------------------
 
!$omp parallel do
  DO r = 1, nrows_x
    DO c = 1, ncols_x

      ql(1) = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )
      vptempk(1) = f_vtemp( xtempm(c,r,1), ql(1) )
      vptempk(1) = f_ptemp( vptempk(1), xpresm(c,r,1) )

      IF ( xmol(c,r) < 0.0 ) THEN
        cpair = 1004.67 * (1.0 + 0.84 * ql(1))   ! [J / kg K]
        hfx   = -xhfx(c,r) / (xdensam(c,r,1) * cpair)
        wss   = (xustar(c,r)**3 + xwstar(c,r)**3)**0.333333
        tconv = -8.5 * hfx / wss
        thv1  = vptempk(1) + tconv
      ELSE
        thv1  = vptempk(1)
      ENDIF

      ! Compute bulk Richardson number with respect to surface.

      ribk(1) = 0.0
      tog     = vptempk(1) / grav

      pblloop: DO k = 2, metlay

        ql(k) = xdenswm(c,r,k) / ( xdensam(c,r,k) - xdenswm(c,r,k) )
        vptempk(k) = f_vtemp( xtempm(c,r,k), ql(k) )
        vptempk(k) = f_ptemp( vptempk(k), xpresm(c,r,k) )

        dtemp   = vptempk(k) - thv1
        wssq    = (uns(c,r,k) - uns(c,r,1))**2 +  &
                  (vns(c,r,k) - vns(c,r,1))**2 + 1.0e-10
        ribk(k) = (x3htm(c,r,k) - x3htm(c,r,1)) * dtemp / (tog * wssq)
        IF ( ribk(k) >= ricrit ) EXIT pblloop

      ENDDO pblloop
            
      kpbl = k

      fint = (ricrit - ribk(kpbl-1)) / (ribk(kpbl) - ribk(kpbl-1))

      IF ( fint > 0.5 ) THEN
        kpblht = kpbl
        fint   = fint - 0.5
      ELSE
        kpblht = kpbl - 1
        fint   = fint + 0.5
      ENDIF

      xpbl(c,r) = fint * ( x3htf(c,r,kpblht) - x3htf(c,r,kpblht-1) ) +  &
                  x3htf(c,r,kpblht-1)

    ENDDO        
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------
 
! DEALLOCATE ( ql      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( ribk    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( uns     )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vns     )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vptempk )  ! commented out to avoid memory fragmentation

END SUBROUTINE pblpkg
