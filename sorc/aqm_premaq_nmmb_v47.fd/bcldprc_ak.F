SUBROUTINE bcldprc_ak

!-------------------------------------------------------------------------------
! Name:     Cloud Processor for Anthes-Kuo Scheme
! Purpose:  Determine the base and top of cloudy layers, as well as the mean
!           water content and fractional area of cloud coverage using JEP H54 
!           cloud and photolysis scheme.  (ANTHES-KUO scheme)
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!    ***    08 Jul 2004  Change definition of total cloud fraction from average
!                        to maximum.  (J. Pleim)
!           20 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const_mete

  IMPLICIT NONE

  INTEGER                      :: c
  REAL                         :: cbase
  REAL                         :: ccmax
  REAL,    SAVE, ALLOCATABLE   :: ccov       ( : , : , : )
  REAL                         :: ctop
  REAL                         :: dp
  REAL                         :: dtdp
  REAL                         :: e_aerk
  REAL                         :: esat1
  REAL                         :: frac
  INTEGER                      :: iflag
  INTEGER                      :: itr
  INTEGER                      :: k
  INTEGER                      :: kbase
  INTEGER                      :: kct
  INTEGER                      :: kmx
  INTEGER                      :: ktop
  REAL                         :: pbar
  REAL                         :: pbase
  REAL                         :: plcl
  REAL                         :: pres
  REAL                         :: qlcl
  REAL                         :: qsat
  REAL                         :: qwat
  REAL                         :: qwsa
  INTEGER                      :: r
  REAL                         :: rh
  REAL                         :: rhc
  REAL                         :: sg1
  REAL                         :: sumz
  REAL                         :: tad
  REAL                         :: tbar
  REAL                         :: tbase
  REAL                         :: tempc
  REAL                         :: tlcl
  REAL                         :: twc
  REAL                         :: wl
  REAL                         :: wtbar
  REAL                         :: x1

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Saturation vapor pressure [Pa]

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )

  qsat(esat1,pres) = esat1 * mvoma / ( pres - esat1 )

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( ccov ) ) ALLOCATE ( ccov (ncols_x, nrows_x, metlay) )

!-------------------------------------------------------------------------------
! Initialize all cloud variables.
!-------------------------------------------------------------------------------

  xwbar   (:,:)   = 0.0
  xcldbot (:,:)   = 0.0
  xcldtop (:,:)   = 0.0
  xcfract (:,:)   = 0.0
  ccov    (:,:,:) = 0.0

!-------------------------------------------------------------------------------
! Loop over grid points and define cloud coverage (CCOV).
!-------------------------------------------------------------------------------

!$omp parallel do
  DO r = 1, nrows_x
    DO c = 1, ncols_x

      kmx = 1

      DO k = 1, metlay

        ! Define RH and critical RH of all layers.

        rh = xwvapor(c,r,k) /  &
             qsat( e_aerk( xtempm(c,r,k)-stdtemp ), xpresm(c,r,k) )
        rh  = MIN(rh,1.0)

        ! Set RHC to at least 98% in PBL - JEP 5/91

        IF ( x3htf(c,r,k-1) < xpbl(c,r) ) THEN

!         rhc = 0.98
          rhc = 0.80
          kmx = k

          IF ( rh > rhc ) THEN

            ! CBL mixing induced clouds should not exceed the frac area of 
            ! the updrafts at top of cbl, les estimates are ~34%
            ! (Schumann 89, and Wyngaard and Brost 84)

            ccov(c,r,k) = 0.34 * ( rh - rhc ) / ( 1.0 - rhc )
          ELSE
            ccov(c,r,k) = 0.0
          ENDIF

        ELSE

!         sg1 = xpresm(c,r,k) / xpresm(c,r,kmx)
          sg1 = 1. - xx3midl(k)
          rhc = 1.0 - ( 2.0 * sg1 * (1.0-sg1) * (1.0 + 1.732*(sg1-0.5)) )

          IF ( rh > rhc ) THEN
            ccov(c,r,k) = ( (rh - rhc)/(1.0 - rhc) )**2   ! Geleyn et al., 1982
          ELSE
            ccov(c,r,k) = 0.0
          ENDIF

        ENDIF

        ccov(c,r,k) = MAX( MIN( ccov(c,r,k), 1.0 ), 0.0 )

      ENDDO

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Loop over grid points and get cloud top and bottom and XWBAR.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO r = 1, nrows_x
    DO c = 1, ncols_x

      kct   = 0
      kbase = 0
      ktop  = 0
      ccmax = 0.0

      ! Determine level of highest rh relative to RHC.

      DO k = 2, metlay-1
        IF ( ccov(c,r,k) > ccmax ) THEN
          ccmax = ccov(c,r,k)
          kct   = k
        ENDIF
      ENDDO

      frac  = 0.0
      cbase = 0.0
      ctop  = 0.0
      wtbar = 0.0
      sumz  = 0.0

      ! Exit with no cloud values if RH < RHC at all levels.

      IF ( ccmax < 0.01 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        CYCLE
      ENDIF

      ! Look for cloud top and base layer up and down from level of max RH.

      top: DO k = kct, metlay
        ktop = k - 1
        IF ( ccov(c,r,k) < 0.5*ccmax ) EXIT top
      ENDDO top

      bottom: DO k = kct, 1, -1
        kbase = k + 1
        IF ( ccov(c,r,k) < 0.5*ccmax ) EXIT bottom
      ENDDO bottom

      DO k = 1, ktop
        IF ( k < kbase ) cbase = cbase + xdx3htf(c,r,k)
        ctop = ctop + xdx3htf(c,r,k)
      ENDDO

      xcldtop(c,r) = ctop
      xcldbot(c,r) = cbase

      ! We need pres. at the bottom of the lowest layer containing clouds.
      ! Temp. and qv at cloud base are for mid layer values.

      plcl = xpresf(c,r,kbase-1)

      tlcl = ( plcl - xpresm(c,r,kbase) ) /                  &
             ( xpresm(c,r,kbase-1) - xpresm(c,r,kbase) ) *   &
             ( xtempm(c,r,kbase-1) - xtempm(c,r,kbase) ) +   &
             xtempm(c,r,kbase)

      qlcl = qsat( e_aerk( tlcl - stdtemp ), plcl )

      iflag = 0
      pbase = plcl
      tbase = tlcl

      ! Follow moist adiabat up.

      DO k = kbase, ktop

        dp   = pbase - xpresm(c,r,k)
        pbar = pbase - dp / 2.0
        tbar = tbase

        DO itr = 1, 5
          x1   = lv0 * qsat( e_aerk( tbar-stdtemp ), pbar ) / ( rdgas * tbar )
          dtdp = rdgas * tbar / pbar / cpd * ( ( 1.0 + x1 ) /  &
                 ( 1.0 + mvoma * lv0 / cpd / tbar * x1 ) )
          tad  = tbase - dp * dtdp
          tbar = ( tad + tbase ) * 0.5
        ENDDO

        ! Determine water content by fraction of adiabatic.

        tad   = MAX(tad, 150.0)
        IF ( tad > xtempm(c,r,k) ) iflag = 1

        ! Pressure in Pascal = cb*1000

        wl    = 0.7 * EXP( ( xpresm(c,r,k) - plcl ) / 8000.0 ) + 0.2
        qwsa  = qsat( e_aerk(tad - stdtemp), xpresm(c,r,k)  )

        qwat  = wl * ( qlcl - qwsa )
        qwat  = MAX(qwat, 0.0)

        twc   = qwat * xpresm(c,r,k) * 1.0e3 / rdgas / xtempm(c,r,k)

        wtbar = wtbar + twc * xdx3htf(c,r,k)

        frac  = frac + ccov(c,r,k) * xdx3htf(c,r,k)
        sumz  = sumz + xdx3htf(c,r,k)
        tbase = tad
        pbase = xpresm(c,r,k)
            
      ENDDO

!!!   xcfract(c,r) = frac  / sumz
      xcfract(c,r) = ccmax
      xwbar  (c,r) = wtbar / sumz

      IF ( xcfract(c,r) < 0.001 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
      ENDIF

      IF ( xwbar(c,r) == 0.0 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        xcfract(c,r) = 0.0
        CYCLE
      ENDIF

      ! If stable environment, use alternate water content expression.

      IF ( iflag == 0 ) THEN
        wtbar = 0.0
        DO k = kbase, ktop
          twc   = 0.05e3 * xwvapor(c,r,k) * xpresm(c,r,k) /   &
                  rdgas / xtempm(c,r,k)
          wtbar = wtbar + twc * xdx3htf(c,r,k)
        ENDDO
        xwbar(c,r) = wtbar / sumz
      ENDIF

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( ccov )  ! commented out to avoid memory fragmentation

END SUBROUTINE bcldprc_ak
