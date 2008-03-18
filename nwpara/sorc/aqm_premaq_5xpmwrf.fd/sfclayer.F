SUBROUTINE sfclayer (theta1, theta2, u1, u2, z1, z2, hfx, ustar, amol, ra, z0)

!-------------------------------------------------------------------------------
! Name:     Surface Layer
! Purpose:  Interpolate temperature to a given height using surface layer
!           similarity based on Hogstrom (1988).
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  25 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           08 Apr 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const_pbl

  IMPLICIT NONE

  REAL                         :: alogz1z0
  REAL                         :: alogz1z2
  REAL,          INTENT(IN)    :: amol       ! Monin-Obukhov length [m]
  REAL,          INTENT(IN)    :: hfx        ! sensible heat flux [W/m^2]
  REAL                         :: psih
  REAL                         :: psih0
  REAL                         :: psim
  REAL                         :: psim0
  REAL,          INTENT(IN)    :: ra         ! aerodynamic resistance [s/m]
  REAL,          INTENT(IN)    :: theta1     ! pot'l temp at z1 [K]
  REAL,          INTENT(OUT)   :: theta2     ! pot'l temp at z2 [K]
  REAL,          INTENT(IN)    :: u1         ! wind speed at z1 [m/s]
  REAL,          INTENT(OUT)   :: u2         ! wind speed at z2 [m/s]
  REAL,          INTENT(IN)    :: ustar      ! friction velocity [m/s]
  REAL,          INTENT(IN)    :: z0         ! roughness length [m]
  REAL                         :: x1
  REAL                         :: x2
  REAL,          INTENT(IN)    :: z1         ! height [m]
  REAL                         :: z1ol
  REAL,          INTENT(IN)    :: z2         ! height [m]
  REAL                         :: z2ol

  ! Compute psi functions from aerodynamic resistance.

  z1ol = z1 / amol
  z2ol = z2 / amol

  alogz1z2 = ALOG(z1/z2)
  alogz1z0 = ALOG(z1/z0)

  IF ( z1ol >= 0.0 ) THEN

    IF ( z1ol > 1.0 ) THEN
       psih0 = 1.0 - betah - z1ol
       psim0 = 1.0 - betam - z1ol
    ELSE
       psih0 = - betah * z1ol
       psim0 = - betam * z1ol
    ENDIF

    IF ( z2ol > 1.0 ) THEN
       psih = psih0 - (1.0 - betah - z2ol)
       psim = psim0 - (1.0 - betam - z2ol)
    ELSE
       psih = psih0 + betah * z2ol
       psim = psim0 + betam * z2ol
    ENDIF

  ELSE

    psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - gamah*z1ol)) /  &
                       (1.0 + SQRT(1.0 - gamah*z2ol)) )
    x1   = (1.0 - gamam * z1ol)**0.25
    x2   = (1.0 - gamam * z2ol)**0.25
    psim = 2.0 * ALOG( (1.0+x1) / (1.0+x2) ) +        &
                 ALOG( (1.0+x1*x1) / (1.0+x2*x2)) -   &
                 2.0 * ATAN(x1) + 2.0 * ATAN(x2)

  ENDIF
      
  theta2 = theta1 - pro*hfx / (vkar*ustar) * (alogz1z2-psih)

  u2     = u1     - ustar/vkar * (alogz1z2-psim)
  u2     = MAX (u2, 0.1)
      
END SUBROUTINE sfclayer
