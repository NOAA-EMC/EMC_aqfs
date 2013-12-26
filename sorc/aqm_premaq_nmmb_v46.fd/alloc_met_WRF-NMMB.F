SUBROUTINE alloc_met

!-------------------------------------------------------------------------------
! Name:     Allocate Meteorology Variables
! Purpose:  Allocate arrays for input meteorology variables.
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Modified for Kh.  (T. Otte)
!           25 Nov 2003  Modified for GFS Ozone.  (T. Otte)
!           04 Dec 2003  Modified for tropopause height.  (T. Otte)
!           29 Apr 2004  Modified for 10-m component winds.  (T. Otte)
!           28 JAN 2005  Modified for Albedo, CSDSF, CSUSF.  (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars

  IMPLICIT NONE

  ALLOCATE ( sigmaf           (nz+1) )  ! interface layer #
  ALLOCATE ( sigmah           (nz)   )  ! hybrid layer #

  ALLOCATE ( pres     (nx, ny, nz)   )  ! pressure, calculated
  ALLOCATE ( pres3d   (nx, ny, nz)   )  ! pressure, read in 
  ALLOCATE ( geohgt   (nx, ny, nh)   )  ! geopotential height
  ALLOCATE ( tmp      (nx, ny, nz)   )  ! temperature
  ALLOCATE ( spc_h    (nx, ny, nz)   )  ! specific humidity
  ALLOCATE ( u_grda   (nx, ny, nz)   )  ! u-component wind, A-grid
  ALLOCATE ( v_grda   (nx, ny, nz)   )  ! v-component wind, A-grid
  ALLOCATE ( u_grd    (nx, ny, nz)   )  ! u-component wind, B-grid (dot point)
  ALLOCATE ( v_grd    (nx, ny, nz)   )  ! v-component wind, B-grid (dot point)
  ALLOCATE ( v_vel    (nx, ny, nz)   )  ! vertical velocity
  ALLOCATE ( tke      (nx, ny, nz)   )  ! turb. kin. energy
  ALLOCATE ( clwmr    (nx, ny, nz)   )  ! cloud water
  ALLOCATE ( c_ice    (nx, ny, nz)   )  ! cloud ice
  ALLOCATE ( rwmr     (nx, ny, nz)   )  ! rain water mix rat
  ALLOCATE ( snmr     (nx, ny, nz)   )  ! snow mixing ratio
  ALLOCATE ( t_cdc    (nx, ny, nz)   )  ! total cloud cover

  ALLOCATE ( kth      (nx, ny, nz+1) )  ! eddy diffusivity

  ALLOCATE ( o3mr     (nx, ny, nz)   )  ! GFS ozone mix rat

  ALLOCATE ( hgt      (nx, ny)       )  ! surface height, surface geopotential
  ALLOCATE ( temp_2m  (nx, ny)       )  ! 2-m temperature
  ALLOCATE ( acpcp    (nx, ny)       )  ! convective precip
  ALLOCATE ( ncpcp    (nx, ny)       )  ! non-conv precip
  ALLOCATE ( fricv    (nx, ny)       )  ! friction velocity
  ALLOCATE ( veg      (nx, ny)       )  ! vegetation
  ALLOCATE ( lhtfl    (nx, ny)       )  ! net latent ht flux
  ALLOCATE ( shtfl    (nx, ny)       )  ! net sens heat flux
  ALLOCATE ( uswrf    (nx, ny)       )  ! upward SW rad flux
  ALLOCATE ( ulwrf    (nx, ny)       )  ! upward LW rad flux
  ALLOCATE ( psfc     (nx, ny)       )  ! surface pressure
  ALLOCATE ( vgtyp    (nx, ny)       )  ! vegetation type
  ALLOCATE ( cnwat    (nx, ny)       )  ! plant canopy water
  ALLOCATE ( skint    (nx, ny)       )  ! skin temperature
  ALLOCATE ( hpbl     (nx, ny)       )  ! PBL height
  ALLOCATE ( hpblr    (nx, ny)       )  ! PBL height, NCEP based Richardson number
  ALLOCATE ( hmixht   (nx, ny)       )  ! Mixed layer depth [m]
  ALLOCATE ( ccond    (nx, ny)       )  ! canopy conductance
  ALLOCATE ( sfexc    (nx, ny)       )  ! sfc exchange coeff

  ALLOCATE ( sotyp    (nx, ny)       )  ! soil type
  ALLOCATE ( soilw    (nx, ny)       )  ! soil moisture fraction (0-10 cm)
  ALLOCATE ( lai      (nx, ny)       )  ! Leaf Area Idex (non-dim)
  ALLOCATE ( snowc    (nx, ny)       )  ! Snow Cover (%)


  ALLOCATE ( dswrf    (nx, ny)       )  ! downward SW rad flux
  ALLOCATE ( csusf    (nx, ny)       )  ! upward clear sky SW flux
  ALLOCATE ( csdsf    (nx, ny)       )  ! downward clear sky SW flux
  ALLOCATE ( albdo    (nx, ny)       )  ! albedo

  ALLOCATE ( psccb    (nx, ny)       )  ! shallow convective cloud bottom
  ALLOCATE ( pscct    (nx, ny)       )  ! shallow convective cloud top
  ALLOCATE ( pdccb    (nx, ny)       )  ! deep convective cloud bottom
  ALLOCATE ( pdcct    (nx, ny)       )  ! deep convective cloud top
  ALLOCATE ( ptccb    (nx, ny)       )  ! convective cloud bottom
  ALLOCATE ( ptcct    (nx, ny)       )  ! convective cloud top

  ALLOCATE ( trop     (nx, ny)       )  ! tropopause height
  ALLOCATE ( u10m     (nx, ny)       )  ! 10-m u-comp wind
  ALLOCATE ( v10m     (nx, ny)       )  ! 10-m v-comp wind

  ALLOCATE ( pdtop    (nx, ny)       )  ! PDTOP
  ALLOCATE ( pdbot    (nx, ny)       )  ! PD

  ALLOCATE ( smapc    (nx, ny)       )  ! map-scale fac (cr)
  ALLOCATE ( smapd    (nx, ny)       )  ! map-scale fac (dt)
  ALLOCATE ( latc     (nx, ny)       )  ! latitude (cr)
  ALLOCATE ( latd     (nx, ny)       )  ! latitude (dt)
  ALLOCATE ( lonc     (nx, ny)       )  ! longitude (cr)
  ALLOCATE ( lond     (nx, ny)       )  ! longitude (dt)
  ALLOCATE ( f        (nx, ny)       )  ! Coriolis  (cr)
  ALLOCATE ( fd       (nx, ny)       )  ! Coriolis  (dt)

  ALLOCATE ( rcold    (nx, ny)       )  ! last hr acpcp
  ALLOCATE ( rnold    (nx, ny)       )  ! last hr ncpcp
  ALLOCATE ( scr2d    (nx, ny)       )  ! scratch 2d array

  ALLOCATE ( got_pres3d       (nh)   )  ! pressure flag
  ALLOCATE ( got_geohgt       (nh)   )  ! geohgt flag
  ALLOCATE ( got_tmp          (nz)   )  ! tmp flag
  ALLOCATE ( got_spc_h        (nz)   )  ! spc_h flag
  ALLOCATE ( got_u_grd        (nz)   )  ! u_grd flag
  ALLOCATE ( got_v_grd        (nz)   )  ! v_grd flag
  ALLOCATE ( got_v_vel        (nz)   )  ! v_vel flag
  ALLOCATE ( got_tke          (nz)   )  ! tke flag
  ALLOCATE ( got_clwmr        (nz)   )  ! clwmr flag
  ALLOCATE ( got_c_ice        (nz)   )  ! c_ice flag
  ALLOCATE ( got_rwmr         (nz)   )  ! rwmr flag
  ALLOCATE ( got_snmr         (nz)   )  ! snmr flag
  ALLOCATE ( got_t_cdc        (nz)   )  ! t_cdc flag

  ALLOCATE ( got_kth          (nz+1) )  ! kth flag

  ALLOCATE ( got_o3mr         (nz)   )  ! o3mr flag

END SUBROUTINE alloc_met
