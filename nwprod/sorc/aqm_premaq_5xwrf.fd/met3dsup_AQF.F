SUBROUTINE met3dsup

!-------------------------------------------------------------------------------
! Name:     Meteorology 3D Supplemental Parameters
! Purpose:  Compute and output supplemental 3D parameters.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  09 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Added calculation of mid-layer density using virtual
!                        temperature.  Expanded dot-point arrays to be filled
!                        (NCOLS_X+1, NROWS_X+1).  (T. Otte and J. Pleim)
!           01 Apr 2003  Modified for PREMAQ, including removal of XPRESM
!                        re-calculation for hydrostatic runs.  (T. Otte)
!           23 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine.  (T. Otte)
!           24 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE metvars, ONLY: sigmaf, sigmah
  USE const
  USE metinfo, inhyd => met_inhyd

  IMPLICIT NONE

  INTEGER                      :: c
  REAL                         :: densdry    ! density of dry air only
  REAL                         :: dlnaerk
  REAL                         :: e_aerk
  REAL                         :: f_vtemp
  REAL,          PARAMETER     :: gravi      = 1.0 / grav
  INTEGER                      :: k
  INTEGER                      :: k1, kint
  REAL                         :: lamdaoo    ! ref base lambda
  INTEGER                      :: lbndf1
  INTEGER                      :: lbndf2
  INTEGER                      :: lbndf3
  INTEGER                      :: lbndm1
  INTEGER                      :: lbndm2
  REAL,          PARAMETER     :: poo        = 100000.0  ! ref prs [Pa]
  REAL,    SAVE, ALLOCATABLE   :: pstar      ( : , : )
  REAL                         :: qmix
  INTEGER                      :: r
  REAL                         :: rhoo       ! ref density of dry air
  REAL                         :: rhovo      ! ref density of vapor
  REAL                         :: temp
  REAL                         :: tempc
  REAL                         :: vtmpml     ! vertual T at mid-lvl k
  REAL                         :: vtmpmu     ! vertual T at mid-lvl k+1
  REAL                         :: p1, p2, p3
  REAL                         :: eta1, eta2, pcal
  REAL                         :: dx3topf , dx3botf , x3jtopf , x3jbotf
  REAL                         :: dx3topm , dx3botm , x3jtopm , x3jbotm
  REAL                         :: dx3topm2, dx3botm2, x3jtopm2, x3jbotm2

  INTERFACE

    SUBROUTINE layht (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: x3htf      ( : , : , : )
      REAL,          INTENT(OUT)   :: x3htm      ( : , : , : )
      REAL,          INTENT(IN)    :: x3jacobf   ( : , : , : )
      REAL,          INTENT(IN)    :: x3jacobm   ( : , : , : )
      REAL,          INTENT(IN)    :: xx3face    ( : )
      REAL,          INTENT(IN)    :: xx3midl    ( : )
    END SUBROUTINE layht

  END INTERFACE

!------------------------------------------------------------------------------
! Functions.
!------------------------------------------------------------------------------

  !---------------------------------------------------------------
  !--- Saturation vapor pressure [Pa]  ! vp0 = 611.29, tempc [C]
  !---------------------------------------------------------------

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )

  !-------------------------------------------------------------
  !--- Function for d LOG(AERK) / dTemp 
  !-------------------------------------------------------------

  dlnaerk(tempc) = 17.625 * 243.04 / ( 243.04 + tempc )**2   
 
  !-------------------------------------------------------------------
  !--- Virtual Temperature [K] ... temp [K], qmix = specific humidity
  !-------------------------------------------------------------------

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )

!------------------------------------------------------------------------------
! Allocate necessary variables.
!------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( pstar ) ) ALLOCATE ( pstar ( ncols_x, nrows_x ) )

!------------------------------------------------------------------------------
! Calculate integration constants for the entropy computation.
!------------------------------------------------------------------------------

  rhoo    = poo / ( rdgas * stdtemp )
  lamdaoo = rwvap * stdtemp * dlnaerk(0.0)
  rhovo   = e_aerk(0.0) / ( rwvap * stdtemp )

!------------------------------------------------------------------------------
! Compute 3D parameters for sigma-p type coordinate using Pstar & Temp.
! Pressure, density at the face  & the middle
!------------------------------------------------------------------------------

  lbndf1 = LBOUND(xpresf,3)           ! 0
  lbndf2 = LBOUND(xx3face,1)          ! 0
  lbndf3 = LBOUND(xjdensaf,3)         ! 0
  lbndm1 = LBOUND(xpresm,3)           ! 1
  lbndm2 = LBOUND(xx3midl,1)          ! 1

!-----------------------------
! -- nonhydrostatic
!-----------------------------

 IF ( inhyd == 1 ) THEN

!$omp parallel do
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        pstar  (c,r)        = xprsfc(c,r) - x3top

        xpresf (c,r,lbndf1) = pstar(c,r) * (xx3midl(lbndm2) - xx3face(lbndf2)) &
                              + xpresm(c,r,lbndm1)

        xdensaf(c,r,lbndf3) = xpresf(c,r,lbndf1) / ( rdgas * xtempg(c,r) )
      ENDDO
    ENDDO  
!$omp end parallel do

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xpresf(c,r,lbndf1+k) = - pstar(c,r) * ( xx3face(lbndf2+k)   &
                                 - xx3midl(k) ) + xpresm(c,r,k)
        ENDDO

      ENDDO
    ENDDO  
!$omp end parallel do

!-----------------------------
! -- hydrostatic
!-----------------------------

 ELSE

  !---------------------------------------------------------
  ! bottom full level (1st interface, "xx3face(0)=0")
  !---------------------------------------------------------

!$omp parallel do
    DO r = 1, nrows_x
      DO c = 1, ncols_x
        pstar   (c,r)        = xprsfc(c,r) - x3top
        xpresf  (c,r,lbndf1) = pstar(c,r) * ( 1.0 - xx3face(lbndf2) ) + x3top
        xjdensaf(c,r,lbndf3) = xpresf(c,r,lbndf1) / ( rdgas * xtempg(c,r) )

        xpres3df(c,r,lbndf1) = xprsfc(c,r)
        xdensaf (c,r,lbndf3) = xpres3df(c,r,lbndf1) / ( rdgas * xtempg(c,r) )
      ENDDO
    ENDDO 
!$omp end parallel do

  xdenss(:,:) = xdensaf(:,:,lbndf3)

  !-------------------------------------------------------------------------
  ! rest of the full (interface) hydrostatic levels
  !   **  p = pdtop*eta1 + pd*eta2 + pt
  !
  ! the non-hydrostatic full level will be based on the mid level P inputs
  !       pmid(i) = (pint(i)+pint(i-1)) / 2    ** full level index from "0"
  !   ==> pint(i) = 2*pmid(i)-pint(i-1)
  !-------------------------------------------------------------------------

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
           p1 = xpdtop(c,r)
           p2 = xpdbot(c,r)

           if ( k < neta2 ) then
              eta1 = 1.
              eta2 = sigmaf(k+1)           ! xpresf k-dim start from "0"
           ELSE
              eta1 = sigmaf(k+1)
              eta2 = 0.
           endif

           xpresf  (c,r,lbndf1+k) = p1*eta1 + p2*eta2 + x3top

           xpres3df(c,r,lbndf1+k) = 2.0 * xpres3d (c,r,lbndf1+k)        &
                                  - xpres3df (c,r,lbndf1+k-1)
        ENDDO
      ENDDO
    ENDDO
!$omp end parallel do


!
! print out one location for verification
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! only for print out, can be eliminated

  ! DO k = 1, metlay
  !    r = 150
  !    c = 150

  !    p1 = xpdtop(c,r)
  !    p2 = xpdbot(c,r)

  !    if ( k  <= neta2 ) then
  !       eta1 = 1.
  !       eta2 = sigmah(k)
  !    ELSE
  !       eta1 = sigmah(k)
  !       eta2 = 0.
  !    endif

  !    pcal=p1*eta1 + p2*eta2 + x3top
  !    write (*,*) "k=", k, " p_cal-mid=", pcal, "pwrf=", xpres3d(c,r,k), " sigmah=", sigmah(k)
  ! ENDDO 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! onlt for print out, can be eliminated

 ENDIF

!------------------------------------------------------------------------------
! Compute Air Density at mid and full levels
!------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, metlay

     k1 = MIN(k+1,metlay)

     DO r = 1, nrows_x
        DO c = 1, ncols_x

           vtmpml = f_vtemp ( xtempm(c,r,k ), xwvapor(c,r,k ) )
           vtmpmu = f_vtemp ( xtempm(c,r,k1), xwvapor(c,r,k1) )

           xjdensam(c,r,k) = xpresm(c,r,k) / ( rdgas * vtmpml )
           xjdensaf(c,r,k) = xpresf(c,r,k) / ( rdgas * (vtmpml+vtmpmu)/2.0 )

           xdensam (c,r,k) = xpres3d (c,r,k) / ( rdgas * vtmpml )
           xdensaf (c,r,k) = xpres3df(c,r,k) / ( rdgas * (vtmpml+vtmpmu)/2.0 )

        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

  xdensam_ref = xdensam
  xdensaf_ref(:,:,lbndf3:lbndf3+metlay) = xdensaf(:,:,lbndf3:lbndf3+metlay)

!-------------------------------------------------------------------------------
! If output will be hydrostatic, compute Jacobian and layer heights.
!-------------------------------------------------------------------------------

  IF ( inhyd /= 1 ) THEN
    lbndf1 = LBOUND(x3jacobf,3)

!$omp parallel do
    DO r = 1, nrows_x
      DO c = 1, ncols_x
        x3jacobf(c,r,lbndf1) = gravi * pstar(c,r) / xjdensaf(c,r,lbndf1)
      ENDDO
    ENDDO
!$omp end parallel do

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
           if ( k <= neta2 ) then
              p3 = xpdbot(c,r)
           else
              p3 = xpdtop(c,r)
           endif

          !-----------------------------------------------------------------
          ! -- Based on John Pleim's formula for 1 region x3face (0---1)
          !-----------------------------------------------------------------

           x3jacobm(c,r,k)        = 2. * gravi * p3 / xjdensam(c,r,k)
           x3jacobf(c,r,lbndf1+k) = 2. * gravi * p3 / xjdensaf(c,r,lbndf1+k)

        ENDDO
      ENDDO
    ENDDO
!$omp end parallel do

!---------------------------------------------------------------------------
! Treatment of the Jacobian at the layer "neta2+1" where Eta1 & Eta2 transit
!   *** x3jacobf k-dim start from "0"
!---------------------------------------------------------------------------

    kint = neta2

!-- Full level

!$omp parallel do
    DO r = 1, nrows_x
       DO c = 1, ncols_x
          x3jtopm = x3jacobm(c,r,kint+1)
          x3jbotm = x3jacobm(c,r,kint  )
          dx3topf = xx3midl(kint+1) - xx3face(kint)
          dx3botf = xx3face(kint  ) - xx3midl(kint)
          x3jacobf (c,r,kint) = ( dx3topf*x3jtopm + dx3botf*x3jbotm )     &
                              / ( dx3topf + dx3botf )
       ENDDO
    ENDDO
!$omp end parallel do

  !
  ! print out the information for verification
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! onlt for print out, can be eliminated

   ! write (*,*) "===================== JACOBIAN CHECK ======================="

   ! write (*,*) "k (or lbndf1)=", lbndf1, " x3jacobf=", x3jacobf(c,r,lbndf1)
   ! DO k = 1, metlay
   !    c = 150
   !    r = 150
   !    write (*,*) "k=", k, " x3jacobf=", x3jacobf(c,r,lbndf1+k), " x3jacobm=", x3jacobm(c,r,k)
   ! ENDDO
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! onlt for print out, can be eliminated


    CALL layht  (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)

  ENDIF

!-------------------------------------------------------------------------------
! Calculate height differences.
!-------------------------------------------------------------------------------

  lbndf2 = LBOUND(x3htf,3)

!$omp parallel do
  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x
        xdx3htf(c,r,k) = x3htf(c,r,lbndf2+k) - x3htf(c,r,lbndf2+k-1)
      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!------------------------------------------------------------------------------
! Compute entropy at mid levels
!------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, metlay
     k1 = MIN(k+1,metlay)

     DO r = 1, nrows_x
        DO c = 1, ncols_x

           xdenswm(c,r,k) = xdensam(c,r,k) * xwvapor(c,r,k)                   &
                          / ( 1.0 + xwvapor(c,r,k) )

           densdry = xdensam(c,r,k) - xdenswm(c,r,k)

           IF ( xdenswm(c,r,k) < 1.0E-5 ) THEN  ! bone dry
              xentrp(c,r,k) = densdry * cvd * LOG( xtempm(c,r,k) / stdtemp )  &
                             -densdry * rdgas * LOG( densdry / rhoo )
           ELSE
              xentrp(c,r,k) = ( densdry * cvd + xdenswm(c,r,k) * cvwvap )  &
                             * LOG( xtempm(c,r,k) / stdtemp )              &
                             + xdenswm(c,r,k) * lamdaoo                    &
                             - rdgas * densdry *  LOG( densdry / rhoo )    &
                             - rwvap * xdenswm(c,r,k)                      &
                                     * LOG( xdenswm(c,r,k) / rhovo )
           ENDIF

        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Set up contravariant velocity (u- and v-components).
! (Note: XUU and XVV correspond to wind components at DOT points.)
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, metlay
    DO r = 1, nrows_x - 1
      DO c = 1, ncols_x - 1

        xuhat(c,r,k) = xmapd(c,r) * xuu(c,r,k)
        xvhat(c,r,k) = xmapd(c,r) * xvv(c,r,k)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Calculate contravariant velocity (w-component).
!-------------------------------------------------------------------------------

  IF ( inhyd == 1 ) THEN  ! non-hydrostatic
    CALL vertnhy
  ELSE
    CALL verthyd
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( pstar )  ! commented out to avoid memory fragmentation

END SUBROUTINE met3dsup
