SUBROUTINE dealloc_met

!-------------------------------------------------------------------------------
! Name:     Deallocate Meteorology Variables
! Purpose:  Deallocate arrays for input meteorology variables.
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Modified for Kh.  (T. Otte)
!           25 Nov 2003  Modified for GFS Ozone.  (T. Otte)
!           04 Dec 2003  Modified for tropopause height.  (T. Otte)
!           29 Apr 2004  Modified for 10-m component winds.  (T. Otte)
!           30 JAN 2005  Modified for albdo, csusf, csdsf    (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE metvars

  IMPLICIT NONE

  DEALLOCATE ( sigmaf     )  ! interface layer #
  DEALLOCATE ( sigmah     )  ! hybrid layer #

  DEALLOCATE ( pres       )  ! pressure
  DEALLOCATE ( geohgt     )  ! geopotential height
  DEALLOCATE ( tmp        )  ! temperature
  DEALLOCATE ( spc_h      )  ! specific humidity
  DEALLOCATE ( u_grd      )  ! u-component wind
  DEALLOCATE ( v_grd      )  ! v-component wind
  DEALLOCATE ( v_vel      )  ! vertical velocity
  DEALLOCATE ( tke        )  ! turb. kin. energy
  DEALLOCATE ( clwmr      )  ! cloud water
  DEALLOCATE ( c_ice      )  ! cloud ice
  DEALLOCATE ( rwmr       )  ! rain water mix ratio
  DEALLOCATE ( snmr       )  ! snow mixing ratio
  DEALLOCATE ( t_cdc      )  ! total cloud cover

  DEALLOCATE ( kth        )  ! eddy diffusivity

  DEALLOCATE ( o3mr       )  ! GFS ozone mix ratio

  DEALLOCATE ( hgt        )  ! surface height
  DEALLOCATE ( temp_2m    )  ! 2-m temperature
  DEALLOCATE ( acpcp      )  ! convective precip
  DEALLOCATE ( fricv      )  ! friction velocity
  DEALLOCATE ( veg        )  ! vegetation
  DEALLOCATE ( lhtfl      )  ! net latent ht flux
  DEALLOCATE ( shtfl      )  ! net sens heat flux
  DEALLOCATE ( uswrf      )  ! upward SW rad flux
  DEALLOCATE ( ulwrf      )  ! upward LW rad flux
  DEALLOCATE ( ncpcp      )  ! non-conv precip
  DEALLOCATE ( psfc       )  ! surface pressure
  DEALLOCATE ( vgtyp      )  ! vegetation type
  DEALLOCATE ( cnwat      )  ! plant canopy water
  DEALLOCATE ( skint      )  ! skin temperature
  DEALLOCATE ( hpbl       )  ! PBL height
  DEALLOCATE ( ccond      )  ! canopy conductance
  DEALLOCATE ( sfexc      )  ! sfc exchange coeff

  DEALLOCATE ( dswrf      )  ! downward SW rad flux
  DEALLOCATE ( csusf      )  ! upward clear sky SW flux
  DEALLOCATE ( csdsf      )  ! downward clear sky SW flux
  DEALLOCATE ( albdo      )  ! albedo

  DEALLOCATE ( psccb      )  ! shallow convective cloud bottom
  DEALLOCATE ( pscct      )  ! shallow convective cloud top
  DEALLOCATE ( pdccb      )  ! deep convective cloud bottom
  DEALLOCATE ( pdcct      )  ! deep convective cloud top
  DEALLOCATE ( ptccb      )  ! convective cloud bottom
  DEALLOCATE ( ptcct      )  ! convective cloud top

  DEALLOCATE ( trop       )  ! tropopause height
  DEALLOCATE ( u10m       )  ! 10-m u-comp wind
  DEALLOCATE ( v10m       )  ! 10-m v-comp wind

  DEALLOCATE ( smapc      )  ! map-scale fac (cr)
  DEALLOCATE ( smapd      )  ! map-scale fac (dt)
  DEALLOCATE ( latc       )  ! latitude (cr)
  DEALLOCATE ( latd       )  ! latitude (dt)
  DEALLOCATE ( lonc       )  ! longitude (cr)
  DEALLOCATE ( lond       )  ! longitude (dt)
  DEALLOCATE ( f          )  ! Coriolis

  DEALLOCATE ( rcold      )  ! last hr acpcp
  DEALLOCATE ( rnold      )  ! last hr ncpcp
  DEALLOCATE ( scr2d      )  ! scratch 2d array

  DEALLOCATE ( got_geohgt )  ! geohgt flag
  DEALLOCATE ( got_tmp    )  ! tmp flag
  DEALLOCATE ( got_spc_h  )  ! spc_h flag
  DEALLOCATE ( got_u_grd  )  ! u_grd flag
  DEALLOCATE ( got_v_grd  )  ! v_grd flag
  DEALLOCATE ( got_v_vel  )  ! v_vel flag
  DEALLOCATE ( got_tke    )  ! tke flag
  DEALLOCATE ( got_clwmr  )  ! clwmr flag
  DEALLOCATE ( got_c_ice  )  ! c_ice flag
  DEALLOCATE ( got_rwmr   )  ! rwmr flag
  DEALLOCATE ( got_snmr   )  ! snmr flag
  DEALLOCATE ( got_t_cdc  )  ! t_cdc flag

  DEALLOCATE ( got_kth    )  ! kth flag

  DEALLOCATE ( got_o3mr   )  ! o3mr flag

END SUBROUTINE dealloc_met
