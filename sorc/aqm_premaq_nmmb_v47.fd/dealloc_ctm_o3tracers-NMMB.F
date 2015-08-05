SUBROUTINE dealloc_ctm

!-------------------------------------------------------------------------------
! Name:     Deallocate Arrays for CTM Dimensions
! Purpose:  Deallocate arrays based on CTM grid.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed JDRATE.  (T. Otte)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           21 Oct 2003  Removed fields associated with GRIDBDY2D, GRIDBDY3D,
!                        and METBDY2D.  (T. Otte)
!           25 Nov 2003  Added JACOBF and JACOBS.  (T. Otte)
!           03 Dec 2003  Added O3.  (T. Otte)
!           04 Dec 2003  Added TROP.  (T. Otte)
!           09 Apr 2004  Added PURB.  (T. Otte)
!           29 Apr 2004  Added conditions on nullification of pointers and
!                        deallocation of arrays associated with creating
!                        time-varying chemical boundary conditions.  Added
!                        deallocation for variables in chemical BC header
!                        arrays.  Combined TEM1P5 and TEMP10 as TEMP2.
!                        Added WSPD10 and WDIR10 to output.  (T. Otte)
!           14 JAN 2005  Added ATTEN, CSRAD, CSUSF, CSDSF, ATTENX AND CSWTOA 
!                                                            (Hsin-mu Lin)
!           14 Jun 2005  Added boundary tracers for GFS O3.  (T. Otte and
!                        R. Mathur)
!           24 Jun 2005  Added array to capture reference O3 profile.  (T. Otte)
!           25 Apr 2006  Changed tracers in TB3 from one per lateral boundary
!                        for ozone to tracers that nominally represent ozone in
!                        the boundary layer, free troposphere, and top layer.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE groutcom
  USE mcoutcom
  USE mdoutcom
  USE choutcom

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate grid arrays for COORD.
!-------------------------------------------------------------------------------

  DEALLOCATE ( vglvs_gd   )
  DEALLOCATE ( x3face_gd  )

!-------------------------------------------------------------------------------
! Deallocate grid arrays for VGRD.
!-------------------------------------------------------------------------------

  DEALLOCATE ( x3face     )
  DEALLOCATE ( x3midl     )

!-------------------------------------------------------------------------------
! Release memory for GROUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( glat_d     )
  NULLIFY    ( glon_d     ) 
  NULLIFY    ( gmsfsq_d   )
  DEALLOCATE ( gd2        )

  NULLIFY    ( glat_c     )
  NULLIFY    ( glon_c     )
  NULLIFY    ( gmsfsq_c   )
  NULLIFY    ( gtopo_c    )
  NULLIFY    ( gdluse_c   )
  NULLIFY    ( gpurb_c    )
  DEALLOCATE ( gc2        )

  NULLIFY    ( gx3htf_c   )
  NULLIFY    ( gx3htm_c   )
  DEALLOCATE ( gc3        )

!-------------------------------------------------------------------------------
! Release memory for MCOUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( prsfc_c    )
  NULLIFY    ( jacobs_c   )
  NULLIFY    ( ustar_c    )
  NULLIFY    ( wstar_c    )

  NULLIFY    ( pbl_c      )
  NULLIFY    ( pbl2_c     )
  NULLIFY    ( pblr_c     )
  NULLIFY    ( mixht_c    )

  NULLIFY    ( zzero_c    )
  NULLIFY    ( moli_c     )
  NULLIFY    ( hfx_c      )
  NULLIFY    ( qfx_c      )
  NULLIFY    ( radyni_c   )
  NULLIFY    ( rbndyi_c   )
  NULLIFY    ( rstomi_c   )
  NULLIFY    ( tempg_c    )
  NULLIFY    ( temp2_c    )
  NULLIFY    ( wspd10_c   )
  NULLIFY    ( wdir10_c   )
  NULLIFY    ( glw_c      )
  NULLIFY    ( gsw_c      )
  NULLIFY    ( rgrnd_c    )
  NULLIFY    ( rainn_c    )
  NULLIFY    ( rainc_c    )
  NULLIFY    ( cfract_c   )
  NULLIFY    ( cldtop_c   )
  NULLIFY    ( cldbot_c   )
  NULLIFY    ( wbar_c     )
  NULLIFY    ( trop_c     )

  NULLIFY    ( attenX_c   )
  NULLIFY    ( atten_c    )
  NULLIFY    ( csrad_c    )
  NULLIFY    ( cswtoa_c   )
  NULLIFY    ( csusf_c    )
  NULLIFY    ( csdsf_c    )

  NULLIFY    ( psccb_c    )
  NULLIFY    ( pscct_c    )
  NULLIFY    ( pdccb_c    )
  NULLIFY    ( pdcct_c    )
  NULLIFY    ( ptccb_c    )
  NULLIFY    ( ptcct_c    )

  NULLIFY    ( sotyp_c    )
  NULLIFY    ( soilw_c    )
  NULLIFY    ( lai_c      )
  NULLIFY    ( snowc_c    )

  NULLIFY    ( vd_c       )
  DEALLOCATE ( mc2        )

  NULLIFY    ( jacobf_c   )
  NULLIFY    ( jacobm_c   )
  NULLIFY    ( densa_j_c  )
  NULLIFY    ( what_jd_c  )
  NULLIFY    ( cldwtr_c   )
  NULLIFY    ( ranwtr_c   )
  NULLIFY    ( qice_c     )
  NULLIFY    ( qsnow_c    )
  NULLIFY    ( tempa_c    )
  NULLIFY    ( wvapor_c   )
  NULLIFY    ( press_c    )
  NULLIFY    ( densa_c    )
  NULLIFY    ( wwind_c    )
  NULLIFY    ( x3htm_c    )
  NULLIFY    ( x3htf_c    )
  NULLIFY    ( eddyv_c    )
  NULLIFY    ( pvc_c      )
  NULLIFY    ( presf_c    )
  DEALLOCATE ( mc3        )

  NULLIFY    ( jacobf_b   )
  NULLIFY    ( jacobm_b   )
  NULLIFY    ( densa_j_b  )
  NULLIFY    ( what_jd_b  )
  NULLIFY    ( cldwtr_b   )
  NULLIFY    ( ranwtr_b   )
  NULLIFY    ( qice_b     )
  NULLIFY    ( qsnow_b    )
  NULLIFY    ( tempa_b    )
  NULLIFY    ( wvapor_b   )
  NULLIFY    ( press_b    )
  NULLIFY    ( densa_b    )
  NULLIFY    ( wwind_b    )
  NULLIFY    ( x3htm_b    )
  NULLIFY    ( x3htf_b    )
  NULLIFY    ( eddyv_b    )
  NULLIFY    ( pvc_b      )
  NULLIFY    ( presf_b    )
  DEALLOCATE ( mb3        )

!-------------------------------------------------------------------------------
! Release memory for MDOUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( uu_d       )
  NULLIFY    ( vv_d       )
  NULLIFY    ( uhat_s     )
  NULLIFY    ( vhat_t     )
  DEALLOCATE ( md3        )

!-------------------------------------------------------------------------------
! Release memory for CHOUTCOM arrays.
!-------------------------------------------------------------------------------

  IF ( ASSOCIATED ( o3_c     ) ) NULLIFY    ( o3_c     )
  IF ( ALLOCATED  ( CC3      ) ) DEALLOCATE ( cc3      )

  IF ( ASSOCIATED ( o3_b     ) ) NULLIFY    ( o3_b     )
  IF ( ALLOCATED  ( cb3      ) ) DEALLOCATE ( cb3      )

  IF ( ALLOCATED  ( cb3vname ) ) DEALLOCATE ( cb3vname )
  IF ( ALLOCATED  ( cb3units ) ) DEALLOCATE ( cb3units )
  IF ( ALLOCATED  ( cb3vdesc ) ) DEALLOCATE ( cb3vdesc )

  IF ( ALLOCATED  ( o3_ref_b ) ) DEALLOCATE ( o3_ref_b )

  NULLIFY    ( o3bl_b     )
  NULLIFY    ( o3ft_b     )
  NULLIFY    ( o3st_b     )
  NULLIFY    ( o3stadv_b  )
  NULLIFY    ( o3sttrn_b  )

  IF (ALLOCATED(tb3)) THEN
     DEALLOCATE ( tb3        )
  END IF

END SUBROUTINE dealloc_ctm
