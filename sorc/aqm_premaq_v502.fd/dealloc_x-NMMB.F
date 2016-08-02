SUBROUTINE dealloc_x

!-------------------------------------------------------------------------------
! Name:     Deallocate X Arrays
! Purpose:  Deallocate X arrays with transfer array dimensions.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           02 Apr 2003  Added XF2DEF.  (T. Otte and J. Pleim)
!           03 Apr 2003  Added XSNOCOV.  (D. Schwede)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           03 Dec 2003  Added XO3.  (T. Otte)
!           04 Dec 2003  Added XTROP.  (T. Otte)
!           09 Apr 2004  Added XPURB.  (T. Otte)
!           29 Apr 2004  Combined XTEMP1P5 and XTEMP10 as XTEMP2.  Added
!                        XWSPD10 and XWDIR10.  (T. Otte)
!           14 Jan 2005  Added XATTEN, XATTENX, XCSRAD, XALBEDO, XCSUSF, XCSDSF
!                        AND XCSTOA.           (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE xvars

!-------------------------------------------------------------------------------
! Nullify pointer variables that reference X-arrays.
!-------------------------------------------------------------------------------

  CALL null_x

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  DEALLOCATE ( xx3face  )
  DEALLOCATE ( xx3midl  )
  DEALLOCATE ( xdx3     )

  DEALLOCATE ( xdepspc  )
  DEALLOCATE ( xdepidx  )
!  DEALLOCATE ( xlusname )

!-------------------------------------------------------------------------------
! Two-Dimensional Arrays   
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatd )
  DEALLOCATE ( xlond )
  DEALLOCATE ( xmapd )

  DEALLOCATE ( xmap_s )
  DEALLOCATE ( xmap_t )

  DEALLOCATE ( xwstar2 )
  DEALLOCATE ( xustar2 )
  DEALLOCATE ( xmol2   )

!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain excluding boundary.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xpurb )
  DEALLOCATE ( xlufrac_01 )  !add by lipan
  DEALLOCATE ( xlufrac_02 )  !add by lipan  
  DEALLOCATE ( xlufrac_03 )  !add by lipan  
  DEALLOCATE ( xlufrac_04 )  !add by lipan  
  DEALLOCATE ( xlufrac_05 )  !add by lipan
  DEALLOCATE ( xlufrac_06 )  !add by lipan  
  DEALLOCATE ( xlufrac_07 )  !add by lipan  
  DEALLOCATE ( xlufrac_08 )  !add by lipan    
  DEALLOCATE ( xlufrac_09 )  !add by lipan
  DEALLOCATE ( xlufrac_10 )  !add by lipan  
  DEALLOCATE ( xlufrac_11 )  !add by lipan  
  DEALLOCATE ( xlufrac_12 )  !add by lipan  
  DEALLOCATE ( xlufrac_13 )  !add by lipan
  DEALLOCATE ( xlufrac_14 )  !add by lipan  
  DEALLOCATE ( xlufrac_15 )  !add by lipan  
  DEALLOCATE ( xlufrac_16 )  !add by lipan   
  DEALLOCATE ( xlufrac_17 )  !add by lipan
  DEALLOCATE ( xlufrac_18 )  !add by lipan  
  DEALLOCATE ( xlufrac_19 )  !add by lipan  
  DEALLOCATE ( xlufrac_20 )  !add by lipan  
  DEALLOCATE ( xlufrac_21 )  !add by lipan
  DEALLOCATE ( xlufrac_22 )  !add by lipan  
  DEALLOCATE ( xlufrac_23 )  !add by lipan  
  DEALLOCATE ( xlufrac_24 )  !add by lipan 
  
!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain including boundary.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatc  )
  DEALLOCATE ( xlonc  )
  DEALLOCATE ( xmapc  )
  DEALLOCATE ( xtopo  )
  DEALLOCATE ( xfc    )

  DEALLOCATE ( xprsfc )
  DEALLOCATE ( xdenss )
  DEALLOCATE ( xtempg )
  DEALLOCATE ( xrainn )
  DEALLOCATE ( xrainc )
  DEALLOCATE ( xdluse )

  DEALLOCATE ( xglw    )
  DEALLOCATE ( xgsw    )
  DEALLOCATE ( xhfx    )
  DEALLOCATE ( xqfx    )
  DEALLOCATE ( xustar  )

  DEALLOCATE ( xpbl    )
  DEALLOCATE ( xpbl2   )
  DEALLOCATE ( xpblr   )

  DEALLOCATE ( xmixht  )
  DEALLOCATE ( xzruf   )
  DEALLOCATE ( xmol    )
  DEALLOCATE ( xrgrnd  )
  DEALLOCATE ( xwr     )
  DEALLOCATE ( xlai    )
  DEALLOCATE ( xsnowc  )
  DEALLOCATE ( xveg    )
  DEALLOCATE ( xch     )

!-------------------------------------------------------------------------------
! All other CROSS2D arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xwstar   )
  DEALLOCATE ( xradyn   )
  DEALLOCATE ( xrbndy   )
  DEALLOCATE ( xrstom   )
  DEALLOCATE ( xtemp2   )
  DEALLOCATE ( xwspd10  )
  DEALLOCATE ( xwdir10  )
  DEALLOCATE ( xcfract  )
  DEALLOCATE ( xcldtop  )
  DEALLOCATE ( xcldbot  )
  DEALLOCATE ( xwbar    )
  DEALLOCATE ( xsnocov  )
  DEALLOCATE ( xtrop    )

  DEALLOCATE ( xattenX  )
  DEALLOCATE ( xatten   )
  DEALLOCATE ( xcsrad   )
  DEALLOCATE ( xcstoa   )
  DEALLOCATE ( xcsusf   )
  DEALLOCATE ( xcsdsf   )
  DEALLOCATE ( xalbedo  )

  DEALLOCATE ( xpsccb   )
  DEALLOCATE ( xpscct   )
  DEALLOCATE ( xpdccb   )
  DEALLOCATE ( xpdcct   )
  DEALLOCATE ( xptccb   )
  DEALLOCATE ( xptcct   )

  DEALLOCATE ( xpdtop   )
  DEALLOCATE ( xpdbot   )

  DEALLOCATE ( xsotyp   )
  DEALLOCATE ( xsoilw   )
  DEALLOCATE ( xq2m     )  !add by lipan
  DEALLOCATE ( xsoim1   )  !add by lipan  
  DEALLOCATE ( xsoim2   )  !add by lipan  
  DEALLOCATE ( xsoit1   )  !add by lipan  
  DEALLOCATE ( xsoit2   )  !add by lipan 
  DEALLOCATE ( xsltyp   )  !add by lipan 
  DEALLOCATE ( xseaice  )  !add by lipan  
  DEALLOCATE ( xvgtyp   )  !add by lipan   
  
  DEALLOCATE ( xvd      )

!-------------------------------------------------------------------------------
! Following could be used for both time independent & dependent processing
! CROSS3D arrays for full ctm domain (include boundary).
!-------------------------------------------------------------------------------

  DEALLOCATE ( xtempm   )
  DEALLOCATE ( xpresm   )
  DEALLOCATE ( xdensam  )
  DEALLOCATE ( xjdensam )
  DEALLOCATE ( xdenswm  )
  DEALLOCATE ( xentrp   )
  DEALLOCATE ( x3jacobf )
  DEALLOCATE ( x3jacobm )
  DEALLOCATE ( x3htf    )
  DEALLOCATE ( x3htm    )
  DEALLOCATE ( xwhat    )
  DEALLOCATE ( xwvapor  )
  DEALLOCATE ( xwwind   )
  DEALLOCATE ( xcldwtr  )
  DEALLOCATE ( xranwtr  )
  DEALLOCATE ( xqice    )
  DEALLOCATE ( xqsnow   )
  DEALLOCATE ( xcfrac_3d)  !add by lipan
  DEALLOCATE ( xeddyv   )

  DEALLOCATE ( xpres3d  )
  DEALLOCATE ( xpres3df )
  DEALLOCATE ( xgeohgt  )

  DEALLOCATE ( xuua     )
  DEALLOCATE ( xvva     )
  DEALLOCATE ( xuhata   )
  DEALLOCATE ( xvhata   )

  DEALLOCATE ( xpvc     )

!-------------------------------------------------------------------------------
! Time-dependent chemical species arrays for full CTM domain.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xo3 )

!-------------------------------------------------------------------------------
! Time-dependent DOT3D arrays for full CTM domain (include boundary).
!-------------------------------------------------------------------------------

  DEALLOCATE ( xuu   )
  DEALLOCATE ( xvv   )
  DEALLOCATE ( xuhat )
  DEALLOCATE ( xvhat )

  DEALLOCATE ( xu_s  )
  DEALLOCATE ( xv_t  )

!-------------------------------------------------------------------------------
! Other arrays used in the main (MCIP) and other subroutines.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xrtold  )
  DEALLOCATE ( xdx3htf )
  DEALLOCATE ( xdensaf )
  DEALLOCATE ( xjdensaf)
  DEALLOCATE ( xpresf  )
  DEALLOCATE ( xdflux  )
  DEALLOCATE ( xprsold )
  DEALLOCATE ( xpsrate )

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic model.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xdensam_ref )
  DEALLOCATE ( xdensaf_ref )

END SUBROUTINE dealloc_x
