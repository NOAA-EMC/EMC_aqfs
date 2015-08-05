SUBROUTINE init_met

!-------------------------------------------------------------------------------
! Name:     Initialize Meteorology Arrays
! Purpose:  Initializes meteorology arrays.
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           03 Dec 2003  Added O3MR.  (T. Otte)
!           29 Apr 2004  Added TROP, U10M, and V10M.  (T. Otte)
!           02 Feb 2005  Added ALBDO, CSUSF, CSDSF    (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE metvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Initialize meteorology arrays.
!-------------------------------------------------------------------------------

  sigmah   (:)     = -1.0     ;      sigmaf   (:)     = -1.0

  acpcp    (:,:)   = 0.0      ;      fricv    (:,:)   = 0.0
  hgt      (:,:)   = 0.0      ;      lhtfl    (:,:)   = 0.0
  shtfl    (:,:)   = 0.0      ;      temp_2m  (:,:)   = 0.0
  ulwrf    (:,:)   = 0.0      ;      uswrf    (:,:)   = 0.0
  veg      (:,:)   = 0.0      ;      psfc     (:,:)   = 0.0
  vgtyp    (:,:)   = 0        ;      cnwat    (:,:)   = 0.0
  skint    (:,:)   = 0.0      ;      hpbl     (:,:)   = 0.0
  hpblr    (:,:)   = 0.0      ;      hmixht   (:,:)   = 0.0

  sotyp    (:,:)   = 0.0      ;      soilw    (:,:)   = 0.0
  lai      (:,:)   = 0.0      ;      snowc    (:,:)   = 0.0

  ccond    (:,:)   = 0.0      ;      sfexc    (:,:)   = 0.0
  dswrf    (:,:)   = 0.0      ;      trop     (:,:)   = 0.0

  csusf    (:,:)   = 0.0      ;      csdsf    (:,:)   = 0.0
  albdo    (:,:)   = 0.0

  psccb    (:,:)   = 0.0      ;      pscct    (:,:)   = 0.0
  pdccb    (:,:)   = 0.0      ;      pdcct    (:,:)   = 0.0
  ptccb    (:,:)   = 0.0      ;      ptcct    (:,:)   = 0.0

  u10m     (:,:)   = 0.0      ;      v10m     (:,:)   = 0.0

  pdtop    (:,:)   = 0.0      ;      pdbot    (:,:)   = 0.0

  smapc    (:,:)   = 0.0      ;      smapd    (:,:)   = 0.0
  latc     (:,:)   = 0.0      ;      latd     (:,:)   = 0.0
  lonc     (:,:)   = 0.0      ;      lond     (:,:)   = 0.0
  f        (:,:)   = 0.0      ;      fd       (:,:)   = 0.0

  c_ice    (:,:,:) = 0.0      ;      clwmr    (:,:,:) = 0.0
  geohgt   (:,:,:) = 0.0      ;      pres     (:,:,:) = 0.0
  rwmr     (:,:,:) = 0.0      ;      snmr     (:,:,:) = 0.0
  spc_h    (:,:,:) = 0.0      ;      t_cdc    (:,:,:) = 0.0
  tke      (:,:,:) = 0.0      ;      tmp      (:,:,:) = 0.0
  u_grd    (:,:,:) = 0.0      ;      v_grd    (:,:,:) = 0.0
  u_grda   (:,:,:) = 0.0      ;      v_grda   (:,:,:) = 0.0
  v_vel    (:,:,:) = 0.0

  kth      (:,:,:) = 0.0
  o3mr     (:,:,:) = 0.0

  pres3d   (:,:,:) = 0.0

  rcold    (:,:)   = 0.0      ;      rnold    (:,:)   = 0.0
  scr2d    (:,:)   = 0.0

END SUBROUTINE init_met
