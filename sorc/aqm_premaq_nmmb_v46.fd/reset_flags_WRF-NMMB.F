SUBROUTINE reset_flags

!-------------------------------------------------------------------------------
! Name:     Reset Flags
! Purpose:  Resets input data-capture flags to "false".
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Modified for Kh.  (T. Otte)
!           25 Nov 2003  Modified for GFS Ozone.  (T. Otte)
!           04 Dec 2003  Modified for tropopause height.  (T. Otte)
!           29 Apr 2004  Modified for 10-m component winds.  (T. Otte)
!           02 Feb 2005  Modified for Radiation Fields: albedo, csusf, csdsf
!                                                            (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE metvars

  IMPLICIT NONE

  got_pres3d ( : ) = .FALSE.
  got_geohgt ( : ) = .FALSE.
  got_tmp    ( : ) = .FALSE.
  got_spc_h  ( : ) = .FALSE.
  got_u_grd  ( : ) = .FALSE.
  got_v_grd  ( : ) = .FALSE.
  got_v_vel  ( : ) = .FALSE.
  got_tke    ( : ) = .FALSE.
  got_clwmr  ( : ) = .FALSE.
  got_c_ice  ( : ) = .FALSE.
  got_rwmr   ( : ) = .FALSE.
  got_snmr   ( : ) = .FALSE.
  got_t_cdc  ( : ) = .FALSE.

  got_kth    ( : ) = .FALSE.

  got_o3mr   ( : ) = .FALSE.

  got_hgt          = .FALSE.
  got_temp_2m      = .FALSE.
  got_acpcp        = .FALSE.
  got_fricv        = .FALSE.
  got_veg          = .FALSE.
  got_lhtfl        = .FALSE.
  got_shtfl        = .FALSE.
  got_uswrf        = .FALSE.
  got_ulwrf        = .FALSE.
  got_ncpcp        = .FALSE.
  got_psfc         = .FALSE.
  got_vgtyp        = .FALSE.
  got_cnwat        = .FALSE.
  got_skint        = .FALSE.
  got_hpbl         = .FALSE.
  got_hpblr        = .FALSE.
  got_hmixht       = .FALSE.
  got_ccond        = .FALSE.
  got_sfexc        = .FALSE.

  got_sotyp        = .FALSE.
  got_soilw        = .FALSE.

  got_lai          = .FALSE.
  got_snowc        = .FALSE.

  got_dswrf        = .FALSE.
  got_csusf        = .FALSE.
  got_csdsf        = .FALSE.
  got_albdo        = .FALSE.

  got_psccb        = .FALSE.
  got_pscct        = .FALSE.
  got_pdccb        = .FALSE.
  got_pdcct        = .FALSE.
  got_ptccb        = .FALSE.
  got_ptcct        = .FALSE.

  got_trop         = .FALSE.
  got_u10m         = .FALSE.
  got_v10m         = .FALSE.

  got_pdtop        = .FALSE.
  got_pd           = .FALSE.

END SUBROUTINE reset_flags
