SUBROUTINE refstate

!-------------------------------------------------------------------------------
! Name:     Reference State
! Purpose:  Calculate non-hydrostatic reference state from meteorology input.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           24 Jan 2004  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const
  USE metinfo, aa => met_tlp, p00 => met_p00, ts0 => met_ts0

  IMPLICIT NONE

  REAL                         :: aplusa
  REAL                         :: bb
  REAL                         :: bb2
  INTEGER                      :: c
  REAL                         :: cc
  REAL                         :: const1
  REAL,          PARAMETER     :: gor        = grav / rdgas
  INTEGER                      :: k
  INTEGER                      :: lbndf
  REAL                         :: lnp
  INTEGER                      :: lvl
  INTEGER                      :: lvlf
  REAL                         :: presf
  REAL                         :: presm
  REAL                         :: press
  REAL                         :: pstar0
  INTEGER                      :: r
  REAL                         :: rh00
  REAL                         :: tempf
  REAL                         :: tempm
  REAL                         :: tsfc

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

!-------------------------------------------------------------------------------
! Extract and define constants for reference calculations.
!-------------------------------------------------------------------------------

  IF ( met_inhyd == 1 ) THEN  ! non-hydrostatic

    lnp    = LOG(p00)
    rh00   = p00 / ( rdgas * stdtemp )
    bb     = ts0 - ( aa * lnp )
    bb2    = bb * bb

    const1 = (0.5 * aa * lnp + bb) * lnp
    aplusa = 2.0 * aa

  ELSE

    rh00   = 100000.0 / ( rdgas * stdtemp )

  ENDIF

!-------------------------------------------------------------------------------
! Since array sizes are assumed-shape, and full-level arrays begin at zero,
! need to set lower bound variable and use it to anchor filling full-level
! fields.  Assume all full-level arrays have same starting point.
!-------------------------------------------------------------------------------

  lbndf = LBOUND(xx3face,1)

!-------------------------------------------------------------------------------
! Calculate reference density and reference Jacobian.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO r = 1, nrows_x
    DO c = 1, ncols_x

      IF ( met_inhyd == 1 ) THEN  ! non-hydrostatic

        cc     = ( xtopo(c,r) * gor ) - const1
        press  = EXP( ( -bb + SQRT( bb2 - aplusa * cc ) ) / aa )
        tsfc   = ts0 + aa * LOG( press / p00 )

      ELSE

        press  = xprsfc(c,r)
        tsfc   = xtempm(c,r,1)

      ENDIF

      pstar0 = press - x3top

      xdenss  (c,r)       = press / ( rdgas * tsfc )
      xdensaf (c,r,lbndf) = xdenss(c,r)
      x3jacobf(c,r,lbndf) = pstar0 / ( grav * xdenss(c,r) )  ! for JACOBS0

      DO lvl = 1, metlay

        lvlf = lbndf + lvl

        presm = x3top + pstar0 * ( 1.0 - xx3midl(lvl)  )
        presf = x3top + pstar0 * ( 1.0 - xx3face(lvlf) )

        IF ( met_inhyd == 1 ) THEN  ! non-hydrostatic

          tempm = ts0 + aa * LOG( presm / p00 )
          tempf = ts0 + aa * LOG( presf / p00 )

        ELSE

          tempm = xtempm(c,r,lvl)

          IF ( lvl < metlay ) THEN 
            tempf = ( xtempm(c,r,lvl) + xtempm(c,r,lvl+1) ) / 2.0
          ELSE
            tempf = xtempm(c,r,lvl)
          ENDIF

        ENDIF

        xpresm(c,r,lvl)    = presm  ! Pa
        xtempm(c,r,lvl)    = tempm

        xdensam(c,r,lvl)   = presm / ( rdgas * tempm )
        xdensaf(c,r,lvlf)  = presf / ( rdgas * tempf )

        x3jacobm(c,r,lvl)  = pstar0 / ( grav * xdensam(c,r,lvl)  )
        x3jacobf(c,r,lvlf) = pstar0 / ( grav * xdensaf(c,r,lvlf) )

        xentrp(c,r,lvl) = xdensam(c,r,lvl) *                             &
                          ( cvd * LOG( xtempm(c,r,lvl) / stdtemp )       &  
                            - rdgas * LOG( xdensam(c,r,lvl) / rh00 ) )

      ENDDO

    ENDDO
  ENDDO
!$omp end parallel do

  xdensam_ref = xdensam
  xdensaf_ref(:,:,lbndf:lbndf+metlay) = xdensaf(:,:,lbndf:lbndf+metlay)

!-------------------------------------------------------------------------------
! Compute layer heights.
!-------------------------------------------------------------------------------

  CALL layht (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)

  DO k = 1, metlay
    xdx3htf(:,:,k) = x3htf(:,:,lbndf+k) - x3htf(:,:,lbndf+k-1)
  ENDDO

END SUBROUTINE refstate
