SUBROUTINE init_x

!-------------------------------------------------------------------------------
! Name:     Initialize X arrays.
! Purpose:  Initializes X arrays.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           02 Apr 2003  Added XF2DEF.  (T. Otte and J. Pleim)
!           03 Apr 2003  Added XSNOCOV.  (D. Schwede)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           03 Dec 2003  Added XO3.  (T. Otte)
!           09 Apr 2004  Added XPURB.  (T. Otte)
!           29 Apr 2004  Combined XTEMP1P5 and XTEMP10 as XTEMP2.  Added
!                        XWSPD10 and XWDIR10.  (T. Otte)
!           14 JAN 2005  Added XATTEN, XATTENX, XCSRAD, XALBEDO, XCSUSF, XCSDSF
!                        AND XCSTOA (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE parms3, ONLY: badval3

  IMPLICIT NONE
 
  INTEGER                    :: l
  CHARACTER*16,  PARAMETER   :: pname      = 'INIT_X'

!-------------------------------------------------------------------------------
! Initialize X-arrays.
!-------------------------------------------------------------------------------

  xx3face (:)     = badval3  ;    xx3midl (:)     = badval3

  xcfract (:,:)   = badval3  ;    xch     (:,:)   = badval3
  xcldbot (:,:)   = badval3  ;    xcldtop (:,:)   = badval3
  xdenss  (:,:)   = badval3  ;    xdluse  (:,:)   = badval3
  xfc     (:,:)   = badval3
  xglw    (:,:)   = badval3  ;    xgsw    (:,:)   = badval3
  xhfx    (:,:)   = badval3  ;    xlai    (:,:)   = badval3
  xlatc   (:,:)   = badval3  ;    xlatd   (:,:)   = badval3
  xlonc   (:,:)   = badval3  ;    xlond   (:,:)   = badval3
  xmapc   (:,:)   = badval3  ;    xmapd   (:,:)   = badval3
  xmap_s  (:,:)   = badval3  ;    xmap_t  (:,:)   = badval3
  xmol    (:,:)   = badval3  ;    xpbl    (:,:)   = badval3
  xpbl2   (:,:)   = badval3  ;    xmixht  (:,:)   = badval3
  xpblr   (:,:)   = badval3  ;    xmol2   (:,:)   = badval3

  xprsfc  (:,:)   = badval3  ;    xpurb   (:,:)   = badval3
  xqfx    (:,:)   = badval3  ;    xradyn  (:,:)   = badval3
  xrainc  (:,:)   = badval3  ;    xrainn  (:,:)   = badval3
  xrbndy  (:,:)   = badval3  ;    xrgrnd  (:,:)   = badval3
  xrstom  (:,:)   = badval3  ;    xrtold  (:,:)   = badval3 
  xsnocov (:,:)   = 0.0      ;    xtemp2  (:,:)   = badval3
  xtempg  (:,:)   = badval3  ;    xtopo   (:,:)   = badval3
  xustar  (:,:)   = badval3  ;    xveg    (:,:)   = badval3
  xwbar   (:,:)   = badval3  ;    xwdir10 (:,:)   = badval3
  xwr     (:,:)   = badval3  ;    xwspd10 (:,:)   = badval3
  xwstar  (:,:)   = badval3  ;    xzruf   (:,:)   = badval3
  xustar2 (:,:)   = badval3  ;    xwstar2 (:,:)   = badval3

  xsotyp  (:,:)   = badval3  ;    xsoilw  (:,:)   = badval3
  xsnowc  (:,:)   = badval3

  xattenX (:,:)   = badval3
  xatten  (:,:)   = badval3  ;    xcsrad  (:,:)   = badval3
  xalbedo (:,:)   = badval3  ;    xcstoa  (:,:)   = badval3
  xcsusf  (:,:)   = badval3  ;    xcsdsf  (:,:)   = badval3

  xpsccb  (:,:)   = badval3  ;    xpscct  (:,:)   = badval3
  xpdccb  (:,:)   = badval3  ;    xpdcct  (:,:)   = badval3
  xptccb  (:,:)   = badval3  ;    xptcct  (:,:)   = badval3
  xpdtop  (:,:)   = badval3  ;    xpdbot  (:,:)   = badval3

  x3htf   (:,:,:) = badval3  ;    x3htm   (:,:,:) = badval3
  x3jacobf(:,:,:) = badval3  ;    x3jacobm(:,:,:) = badval3
  xcldwtr (:,:,:) = badval3  ;    xdensam (:,:,:) = badval3
  xjdensam(:,:,:) = badval3
  xdenswm (:,:,:) = badval3  ;    xdx3htf (:,:,:) = badval3
  xeddyv  (:,:,:) = badval3  ;    xentrp  (:,:,:) = badval3
  xo3     (:,:,:) = badval3  ;    xpresm  (:,:,:) = badval3
  xqice   (:,:,:) = badval3  ;    xqsnow  (:,:,:) = badval3
  xranwtr (:,:,:) = badval3  ;    xtempm  (:,:,:) = badval3
  xuhat   (:,:,:) = badval3  ;    xuu     (:,:,:) = badval3
  xvd     (:,:,:) = badval3  ;    xvhat   (:,:,:) = badval3
  xvv     (:,:,:) = badval3  ;    xwhat   (:,:,:) = badval3
  xu_s    (:,:,:) = badval3  ;    xv_t    (:,:,:) = badval3
  xwvapor (:,:,:) = badval3  ;    xwwind  (:,:,:) = badval3
  xpres3d (:,:,:) = badval3  ;    xgeohgt (:,:,:) = badval3
  xpres3df(:,:,:) = badval3  ;    xpvc    (:,:,:) = 0.0
  xuua    (:,:,:) = badval3  ;    xvva    (:,:,:) = badval3
  xuhata  (:,:,:) = badval3  ;    xvhata  (:,:,:) = badval3

!-------------------------------------------------------------------------------
! Set up dry deposition pointer indices.
!-------------------------------------------------------------------------------

  DO l = 1, ltotg
    xdepidx(l) = l
  ENDDO

  xdepspc( 1) = 'SO2'      ; xdepspc( 2) = 'SULF'     ; xdepspc( 3) = 'NO2'
  xdepspc( 4) = 'NO'       ; xdepspc( 5) = 'O3'       ; xdepspc( 6) = 'HNO3'
  xdepspc( 7) = 'H2O2'     ; xdepspc( 8) = 'ALD'      ; xdepspc( 9) = 'HCHO'
  xdepspc(10) = 'OP'       ; xdepspc(11) = 'PAA'      ; xdepspc(12) = 'ORA'
  xdepspc(13) = 'NH3'      ; xdepspc(14) = 'PAN'      ; xdepspc(15) = 'HONO'
  xdepspc(16) = 'CO'       ; xdepspc(17) = 'METHANOL' ; xdepspc(18) = 'N2O5'
  xdepspc(19) = 'NO3'      ; xdepspc(20) = 'GEN_ALD'

END SUBROUTINE init_x
