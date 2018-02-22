SUBROUTINE verthyd

!-------------------------------------------------------------------------------
! Name:     Vertical Velocities -- Hydrostatic
! Purpose:  Computes vertical velocities, XWHAT, from horizontal velocities
!           using the continuity equation.
!
!               d(PSTAR)     -->            -->    d (PSTAR * WHAT)
!     PSRATE = --------  = - DEL . (PSTAR * VEL) - ----------------- 
!               dt                                  d (X3)
!
!     where,
!                           -->
!     PSTAR = PSURF - PTOP, VEL is the horizontal velocity, t is time,
!     -->
!     DEL . denotes the horizontal divergence operator, SIGMA is the normalized
!                                P - PTOP
!     pressure coordinate, i.e., --------, and OMEGA is the vertical
!                                 PSTAR
!                     d(X3FACE)
!     velocity, i.e., -------- = WHAT
!                       d(t)
!
!     For hydrostatic SIGMA-P coordinate, X3 = 1-SIGMA
!     The boundary conditions for the vertical velocity are:
!
!     OMEGA = 0 at SIGMA = 0 (i.e., at the top of the atmosphere)
!     OMEGA = 0 at SIGMA = 1 (i.e., at the surface)
!
!     But, for X3 coordinate
!
!     WHAT = 0 at X3 = 0 (i.e., at the surface)
!     WHAT = 0 at X3 = 1 (i.e., at the top of the atmosphere)
!
!     With X3 coordinate, WHAT is positive upward.
!     One can integrate this equation vertically from X3 = 0 to an
!     arbitrary X3 level to obtain WHAT at that level,
!
!                    -- (X3)
!        d(PSTAR)    |   -->            -->
!   (X3) -------- +  |   DEL . (PSTAR * VEL) d(X3) = - PSTAR * WHAT     (2)
!         d(t)       |
!                   -- 0
!
!     If this is done over the entire atmosphere (i.e., from X3 = 0
!     to X3 = 1) then the following expression is obtained:
!
!                             -- 1
!               d(PSTAR)      |   -->            -->
!      PSRATE = -------- = -  |   DEL . (PSTAR * VEL) d(X3)             (3)
!                 d(t)        |
!                            -- 0
!
!     The expression in Equation (3) can be substituted into Equation (2)
!     to yield the following expression for XWHAT at X3
!
!               (X3)
!      WHAT = - ----- PSRATE
!               PSTAR 
!
!                       -- (X3)
!                 1     |    -->            -->
!            -  -----   |    DEL . (PSTAR * VEL) d(X3)               (4)
!               PSTAR   |
!                      -- 0
!
!     This routine evaluates the two integrals in Equation (4) and computes
!     vertical velocity as a function of height.
!
!     For more information see for instance:
!
!     Haltiner, G. J. and Williams, R. T., " Numerical Prediction and Dynamic
!     Meteorology," 1980, Wiley, pp. 1-28.
!
! Notes:    Liberally adapted from EPA's MCIP2.
!
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed JDRATE.  (T. Otte)
!           27 Mar 2003  Modified for PREMAQ.  (T. Otte)
!           28 Mar 2003  Corrected algorithm to calculate W in the PSRATE
!                        calculation.  (T. Otte)
!           25 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           13 Apr 2004  Added DEALLOCATE for MFAC, PSTAR, and PSTARI. (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------


  USE premaqparm
  USE xvars
  USE coord

  IMPLICIT NONE

  INTEGER                      :: ccc
  INTEGER                      :: cm1
  INTEGER                      :: col
  INTEGER                      :: cp1
  INTEGER                      :: cp1d
  REAL                         :: dx
  REAL                         :: flux_east
  REAL                         :: flux_north
  REAL                         :: flux_south
  REAL                         :: flux_west
  INTEGER                      :: iter
  INTEGER                      :: lbnd
  INTEGER                      :: lvl
  REAL                         :: map_s
  REAL                         :: map_t
  REAL,    SAVE, ALLOCATABLE   :: mapsqi_s   ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: mapsqi_t   ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: mfac       ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: pstar      ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: pstar_s    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: pstar_t    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: pstari     ( : , : )
  INTEGER                      :: rm1
  INTEGER                      :: row
  INTEGER                      :: rp1
  INTEGER                      :: rp1d
  INTEGER                      :: rrr
  REAL,    SAVE, ALLOCATABLE   :: w          ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: werr       ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: xuhat_s    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: xvhat_t    ( : , : , : )

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( mapsqi_s ) )  &
    ALLOCATE ( mapsqi_s ( ncols_x-1, nrows_x-1 )          )
  IF ( .NOT. ALLOCATED ( mapsqi_t ) )  &
    ALLOCATE ( mapsqi_t ( ncols_x-1, nrows_x-1 )          )
  IF ( .NOT. ALLOCATED ( mfac     ) )  &
    ALLOCATE ( mfac     ( ncols_x,   nrows_x   )          )
  IF ( .NOT. ALLOCATED ( pstar    ) )  &
    ALLOCATE ( pstar    ( ncols_x,   nrows_x   )          )
  IF ( .NOT. ALLOCATED ( pstar_s  ) )  &
    ALLOCATE ( pstar_s  ( ncols_x-1, nrows_x-1 )          )
  IF ( .NOT. ALLOCATED ( pstar_t  ) )  &
    ALLOCATE ( pstar_t  ( ncols_x-1, nrows_x-1 )          )
  IF ( .NOT. ALLOCATED ( pstari   ) )  &
    ALLOCATE ( pstari   ( ncols_x,   nrows_x   )          )
  IF ( .NOT. ALLOCATED ( w        ) )  &
    ALLOCATE ( w        ( ncols_x,   nrows_x,    metlay ) )
  IF ( .NOT. ALLOCATED ( xuhat_s  ) )  &
    ALLOCATE ( xuhat_s  ( ncols_x-1, nrows_x-1,  metlay ) )
  IF ( .NOT. ALLOCATED ( xvhat_t  ) )  &
    ALLOCATE ( xvhat_t  ( ncols_x-1, nrows_x-1,  metlay ) )

!-------------------------------------------------------------------------------
! Calculate delta-X3.
!-------------------------------------------------------------------------------

  lbnd = LBOUND(xx3face,1)
  DO lvl = 1, metlay
    xdx3(lvl) = xx3face(lbnd+lvl) - xx3face(lbnd+lvl-1)
  ENDDO

!-------------------------------------------------------------------------------
! Calculate P-star and inverse P-star.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO row = 1, nrows_x 
    DO col = 1, ncols_x
      pstar (col,row) = xpdbot(col,row) + xpdtop(col,row)
      pstari(col,row) = 1.0 / pstar(col,row)
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Calculate map-scale factors (1/M^2) on square and triangle flux points.
! Calculate factor (M^2/delta-X) at scalar points to be used in scalar
! calculation.
!-------------------------------------------------------------------------------

  dx = REAL(xcell_gd)

!$omp parallel do

  DO row = 1, nrows_x
    rp1  = row + 1
!   rp1d = MIN(row+1, nrows_x-1)
    rp1d = MIN(rp1, nrows_x-1)
    DO col = 1, ncols_x
      cp1  = col + 1
!     cp1d = MIN(col+1, ncols_x-1)
      cp1d = MIN(cp1, ncols_x-1)

      mfac(col,row) = xmapc(col,row)**2 / dx

      IF ( ( col == ncols_x ) .OR. ( row == nrows_x ) ) CYCLE

      map_s = 0.25 * ( xmapc(col,rp1) + xmapc(cp1,rp1 ) +  &
                       xmapd(col,row) + xmapd(col,rp1d) )

      map_t = 0.25 * ( xmapc(cp1,row) + xmapc(cp1, rp1) +  &
                       xmapd(col,row) + xmapd(cp1d,row) )
 
      xmap_s  (col,row) = map_s
      xmap_t  (col,row) = map_t

      mapsqi_s(col,row) = 1.0 / (map_s * map_s)
      mapsqi_t(col,row) = 1.0 / (map_t * map_t)

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Calculate U- and V-component winds at square and triangle flux points.
! Calculate P-star at square and triangle flux points.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO row = 1, nrows_x-1
    rp1  = row + 1
!   rp1d = MIN(row+1, nrows_x-1)
    rp1d = MIN(rp1, nrows_x-1)
    DO col = 1, ncols_x-1
      cp1  = col + 1
!     cp1d = MIN(col+1, ncols_x-1)
      cp1d = MIN(cp1, ncols_x-1)

      pstar_s(col,row) = 0.5 *  &
                         ( pstar(col,rp1) + pstar(cp1,rp1) )
      pstar_t(col,row) = 0.5 *  &
                         ( pstar(cp1,row) + pstar(cp1,rp1) )

      IF ( ( col == ncols_x ) .OR. ( row == nrows_x ) ) CYCLE

      DO lvl = 1, metlay
        xuhat_s(col,row,lvl) = 0.5 *  &
                               ( xuhat(col,row,lvl) + xuhat(col,rp1d,lvl) )
        xvhat_t(col,row,lvl) = 0.5 *  &
                               ( xvhat(col,row,lvl) + xvhat(cp1d,row,lvl) )

      !--- Assign new variables in order not to conflict with "metdot.F" ----

        xu_s (col,row,lvl) = xuhat_s(col,row,lvl)
        xv_t (col,row,lvl) = xvhat_t(col,row,lvl)

      ENDDO

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Calculate rate of change of P-star.  Calculation follows equation 12-121
! in "Science Algorithms of the EPA Models-3/CMAQ Modeling System" 
! (EPA/600/R-99/030).  This assumes Q-sub-rho (in the equation) is zero.
!-------------------------------------------------------------------------------

  xpsrate(:,:) = 0.0

  lbnd = LBOUND(xwhat,3)
  xwhat(:,:,lbnd+metlay) = 0.0
  xwhat(:,:,lbnd)        = 0.0

!$omp parallel do
  DO row = 1, nrows_x
    rrr = MIN(row, nrows_x-1)
    rm1 = MAX(row-1,1)
    DO col = 1, ncols_x
      ccc = MIN(col,ncols_x-1)
      cm1 = MAX(col-1,1)
      DO lvl = 1, metlay

        flux_west  = pstar_s(cm1,rm1) * xuhat_s(cm1,rm1,lvl) *  &
                     mapsqi_s(cm1,rm1)

        flux_east  = pstar_s(ccc,rm1) * xuhat_s(ccc,rm1,lvl) *  &
                     mapsqi_s(ccc,rm1)

        flux_south = pstar_t(cm1,rm1) * xvhat_t(cm1,rm1,lvl) *  &
                     mapsqi_t(cm1,rm1)

        flux_north = pstar_t(cm1,rrr) * xvhat_t(cm1,rrr,lvl) *  &
                     mapsqi_t(cm1,rrr)

        w(col,row,lvl) = xdx3(lvl) * mfac(col,row) *  &
                         (flux_east - flux_west + flux_north - flux_south)

        xpsrate(col,row) = xpsrate(col,row) - w(col,row,lvl)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Compute WHAT for each layer using the top layer's WHAT according to
! the vertically integrated continuity equation.  Compute top-down or
! bottom-up as required.
!-------------------------------------------------------------------------------

  IF ( iwind <= 0 ) THEN  ! top-down as often used in met. modeling

    DO lvl = metlay, 2, -1    !  induction on lvl; vectorize on col,row

!$omp parallel do
      DO row = 1, nrows_x
        DO col = 1, ncols_x

          xwhat(col,row,lvl-1) = xwhat(col,row,lvl) + pstari(col,row) *   &
                                 (xdx3(lvl) * xpsrate(col,row) + w(col,row,lvl))
        ENDDO
      ENDDO
!$omp end parallel do
    ENDDO

  ELSE  ! bottom up integration

    DO lvl = 1, metlay

!$omp parallel do
      DO row = 1, nrows_x
        DO col = 1, ncols_x

          xwhat(col,row,lvl) = xwhat(col,row,lvl-1) - pstari(col,row) *   &
                               (xdx3(lvl) * xpsrate(col,row) + w(col,row,lvl))
        ENDDO
      ENDDO
!$omp end parallel do
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Compute Flux Divergence Term and Store in DFLUX.  Then, correct errors
! using ABS(IWIND) number of iterations.
!-------------------------------------------------------------------------------

  IF ( iwind /= 0 ) THEN

    IF ( .NOT. ALLOCATED ( werr ) )  &
      ALLOCATE ( werr (ncols_x, nrows_x, 0:metlay) )

    iterate: DO iter = 1, ABS(iwind)

      lbnd = LBOUND(werr,3)
      werr(:,:,lbnd+metlay) = 0.0
      werr(:,:,lbnd)        = 0.0
              
!$omp parallel do
      DO lvl = 1, metlay
        DO row = 1, nrows_x
          DO col = 1, ncols_x

            xdflux(col,row,lvl) = pstari(col,row) * w(col,row,lvl) +   &
                                  ( xwhat(col,row,lvl) - xwhat(col,row,lvl-1) )
          ENDDO
        ENDDO
      ENDDO
!$omp end parallel do

      ! Update vertical wind to remove XDFLUX for each layer... (either, downward)

      IF ( iwind <= 0 ) THEN  !  top-down integration

        DO lvl = metlay, 2, -1
!$omp parallel do
          DO row = 1, nrows_x
            DO col = 1, ncols_x
              werr(col,row,lvl-1) = werr(col,row,lvl) + xdflux(col,row,lvl-1)  &
                                    + pstari(col,row)*xpsrate(col,row)*xdx3(lvl-1)
              xwhat(col,row,lvl-1) = xwhat(col,row,lvl-1) - werr(col,row,lvl-1) 
            ENDDO
          ENDDO
!$omp end parallel do
        ENDDO

      ELSE  !  bottom-up integration

        DO lvl = 1, metlay
!$omp parallel do
          DO row = 1, nrows_x
            DO col = 1, ncols_x
              werr(col,row,lvl) = werr(col,row,lvl-1) + xdflux(col,row,lvl)  &
                                  + pstari(col,row)*xpsrate(col,row)*xdx3(lvl)
              xwhat(col,row,lvl) = xwhat(col,row,lvl) - werr(col,row,lvl) 
            ENDDO
          ENDDO
!$omp end parallel do
        ENDDO

      ENDIF

    ENDDO iterate

!   DEALLOCATE ( werr )  ! commented out to avoid memory fragmentation

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( mapsqi_s )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( mapsqi_t )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( mfac     )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( pstar    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( pstar_s  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( pstar_t  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( pstari   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( w        )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xuhat_s  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xvhat_t  )  ! commented out to avoid memory fragmentation

END SUBROUTINE verthyd
