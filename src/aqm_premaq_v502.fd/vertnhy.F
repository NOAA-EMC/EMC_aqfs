SUBROUTINE vertnhy

!-------------------------------------------------------------------------------
! Name:     Vertical Velocities -- Non-Hydrostatic
! Purpose:  Generates a complete set of meteorological data for transport
!           processors of Models-3 CTM.
!           But, for X3 coordinate
!
!             WHAT = 0 at X3 = 0 (i.e., at the surface)
!             WHAT = 0 at X3 = 1 (i.e., at the top of the atmosphere)
!
!           With X3 coordinate, WHAT (although we are using XWHAT0 array here)
!           is positive upward.
!
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Corrected calculation of weights used for vertical
!                        interpolation to full levels.  Corrected algorithm
!                        to calculate XWHAT from XUHAT and XVHAT.  (T. Otte)
!           27 Mar 2003  Modified for PREMAQ.  (T. Otte)
!           25 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const
  USE coord

  IMPLICIT NONE

  INTEGER                      :: ccc
  INTEGER                      :: cm1
  INTEGER                      :: col
  INTEGER                      :: cp1
  REAL,          SAVE          :: ddx2
  REAL,          SAVE          :: ddy2
  REAL,          SAVE          :: dx
  REAL,          SAVE          :: dy
  LOGICAL,       SAVE          :: firstime   = .TRUE.
  INTEGER                      :: iter
  INTEGER                      :: lbnd
  INTEGER                      :: lp1
  INTEGER                      :: lvl
  INTEGER                      :: map1
  INTEGER                      :: map2
  REAL,  SAVE,   ALLOCATABLE   :: pstar      ( : , : )
  REAL,  SAVE,   ALLOCATABLE   :: pstari     ( : , : )
  INTEGER                      :: rm1
  INTEGER                      :: row
  INTEGER                      :: rp1
  INTEGER                      :: rrr
  REAL,  SAVE,   ALLOCATABLE   :: wght_bot   ( : )
  REAL,  SAVE,   ALLOCATABLE   :: wght_top   ( : )
  REAL,  SAVE,   ALLOCATABLE   :: xfac       ( : , : )
  REAL,  SAVE,   ALLOCATABLE   :: yfac       ( : , : )
  REAL,  SAVE,   ALLOCATABLE   :: zfac       ( : , : , : )

!-------------------------------------------------------------------------------
! On first call to subroutine, calculate reference factors and arrays.
! Note that P-star is constant in time in the non-hydrostatic application.
!-------------------------------------------------------------------------------

  IF ( firstime ) THEN

    ALLOCATE ( pstar    (ncols_x, nrows_x)         )
    ALLOCATE ( pstari   (ncols_x, nrows_x)         )
    ALLOCATE ( wght_bot                   (metlay) )
    ALLOCATE ( wght_top                   (metlay) )
    ALLOCATE ( xfac     (ncols_x, nrows_x)         )
    ALLOCATE ( yfac     (ncols_x, nrows_x)         )
    ALLOCATE ( zfac     (ncols_x, nrows_x, metlay) )

    dx   = xcell_gd
    dy   = ycell_gd

    ddx2 = 0.5 / dx
    ddy2 = 0.5 / dy
                           
!$omp parallel do
    DO lvl = 1, metlay-1
      wght_top(lvl) = ( xx3face(lvl)   - xx3midl(lvl) ) /  &
                      ( xx3midl(lvl+1) - xx3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO
!$omp end parallel do

    wght_bot(metlay) = 1.0
    wght_top(metlay) = 0.0

!$omp parallel do
    DO row = 1, nrows_x 
      DO col = 1, ncols_x
        pstar (col,row) = xprsfc(col,row) - x3top
        pstari(col,row) = 1.0 / pstar(col,row)
      ENDDO
    ENDDO
!$omp end parallel do

!$omp parallel do
    DO row = 1, nrows_x
      rp1 = MIN(row+1,nrows_x)
      rm1 = MAX(row-1,1)        
      DO col = 1, ncols_x
        cp1 = MIN(col+1,ncols_x)
        cm1 = MAX(col-1,1)

        ! First time XPRSFC must include reference surface pressure at given
        ! topography for MM5 nonhydrostatic application, i.e., sigma-po coord.            
        xfac(col,row) = ddx2 * ( pstar(cp1,row) - pstar(cm1,row) ) *  &
                        pstari(col,row) 

        yfac(col,row) = ddy2 * ( pstar(col,rp1) - pstar(col,rm1) ) *  &
                        pstari(col,row) 

        DO lvl = 1, metlay
          zfac(col,row,lvl) = grav * pstari(col,row) * xdensaf_ref(col,row,lvl)
        ENDDO

      ENDDO
    ENDDO
!$omp end parallel do

    firstime = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Compute vertical velocity on full levels and scalar points.  Here, the
! horizontal wind components are on dot points and mid-layers, so interpolation
! is performed to obtain approximations of horizontal wind components on full
! levels and at scalar points.  The calculation of XWHAT follows from equation
! 12-124 in "Science Algorithms of the EPA Models-3/CMAQ Modeling System" 
! (EPA/600/R-99/030).
!-------------------------------------------------------------------------------

  lbnd = LBOUND(xwhat,3)
  xwhat(:,:,lbnd+metlay) = 0.0   ! velocity zero, at top
  xwhat(:,:,lbnd)        = 0.0   ! velocity zero, at bottom

!$omp parallel do
  DO lvl = 1, metlay-1
    lp1 = lvl + 1
    DO ROW = 1, nrows_x
      rrr = MIN(row, nrows_x-1)
      rm1 = MAX(row-1, 1)
      DO col = 1, ncols_x
        ccc = MIN(col, ncols_x-1)
        cm1 = MAX(col-1, 1)

        xwhat(col,row,lvl) = 0.25 * ( 1.0 - xx3face(lvl) ) *                   &
                             ( xfac(col,row) *                                 &
                             ( wght_bot(lvl)                                   &
                             * ( xuhat(cm1,rm1,lvl) + xuhat(ccc,rm1,lvl) +     &
                                 xuhat(cm1,rrr,lvl) + xuhat(ccc,rrr,lvl) )     &
                             + wght_top(lvl)                                   &
                             * ( xuhat(cm1,rm1,lp1) + xuhat(ccc,rm1,lp1) +     &
                                 xuhat(cm1,rrr,lp1) + xuhat(ccc,rrr,lp1) ) )   &
                             + yfac(col,row) *                                 &
                             ( wght_bot(lvl)                                   &
                             * ( xvhat(cm1,rm1,lvl) + xvhat(ccc,rm1,lvl) +     &
                                 xvhat(cm1,rrr,lvl) + xvhat(ccc,rrr,lvl) )     &
                             + wght_top(lvl)                                   &
                             * ( xvhat(cm1,rm1,lp1) + xvhat(ccc,rm1,lp1) +     &
                                 xvhat(cm1,rrr,lp1) + xvhat(ccc,rrr,lp1) ) ) ) &
                             + zfac(col,row,lvl) * xwwind(col,row,lvl)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

END SUBROUTINE vertnhy
