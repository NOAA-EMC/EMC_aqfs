SUBROUTINE alloc_x

!-------------------------------------------------------------------------------
! Name:     Allocate X Arrays
! Purpose:  Allocate X arrays with transfer array dimensions.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed XJDRATE.  Expanded dot-point array dimensions
!                        to be (NCOLS_X+1, NROWS_X+1).  (T. Otte)
!           02 Apr 2003  Added XF2DEF.  (T. Otte and J. Pleim)
!           03 Apr 2003  Added XSNOCOV.  (D. Schwede)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           22 Oct 2003  Modified for XEDDYV on full levels.  (T. Otte)
!           03 Dec 2003  Added XO3.  (T. Otte)
!           04 Dec 2003  Added XTROP.  (T. Otte)
!           22 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           09 Apr 2004  Added PURB.  (T. Otte)
!           29 Apr 2004  Combined XTEMP1P5 and XTEMP10 as XTEMP2.  Added
!                        XWSPD10 and XWDIR10.  (T. Otte)
!           15 Jun 2004  Added XPURB.  (T. Otte)
!           14 Jan 2005  Added XATTEN, XATTENX, XCSRAD, XALBEDO, XCSUSF, XCSDSF
!                        AND XCSTOA                         (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  ! Layer face and middle point coordinate (increasing monotonically)

  ALLOCATE ( xx3face (0:metlay) )
  ALLOCATE ( xx3midl (  metlay) )
  ALLOCATE ( xdx3    (  metlay) )

  ALLOCATE ( xlusname (maxluc)    )

!-------------------------------------------------------------------------------
! Two-Dimensional Arrays   
!-------------------------------------------------------------------------------

  ALLOCATE ( xlatd   (ncols_x-1, nrows_x-1) )
  ALLOCATE ( xlond   (ncols_x-1, nrows_x-1) )
  ALLOCATE ( xmapd   (ncols_x-1, nrows_x-1) )

  ALLOCATE ( xmap_s  (ncols_x-1, nrows_x-1) )
  ALLOCATE ( xmap_t  (ncols_x-1, nrows_x-1) )

  ALLOCATE ( xustar2 (ncols_x  , nrows_x  ) )
  ALLOCATE ( xwstar2 (ncols_x  , nrows_x  ) )
  ALLOCATE ( xmol2   (ncols_x  , nrows_x  ) )

!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain excluding boundary
!-------------------------------------------------------------------------------

  ALLOCATE ( xpurb   (ncols, nrows) )

!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain including boundary
!-------------------------------------------------------------------------------

  ALLOCATE ( xlatc   (ncols_x, nrows_x) )
  ALLOCATE ( xlonc   (ncols_x, nrows_x) )
  ALLOCATE ( xmapc   (ncols_x, nrows_x) )
  ALLOCATE ( xtopo   (ncols_x, nrows_x) )
  ALLOCATE ( xfc     (ncols_x, nrows_x) )

  ALLOCATE ( xprsfc  (ncols_x, nrows_x) )
  ALLOCATE ( xdenss  (ncols_x, nrows_x) )
  ALLOCATE ( xtempg  (ncols_x, nrows_x) )
  ALLOCATE ( xrainn  (ncols_x, nrows_x) )
  ALLOCATE ( xrainc  (ncols_x, nrows_x) )
  ALLOCATE ( xdluse  (ncols_x, nrows_x) )

  ALLOCATE ( xglw    (ncols_x, nrows_x) )
  ALLOCATE ( xgsw    (ncols_x, nrows_x) )
  ALLOCATE ( xhfx    (ncols_x, nrows_x) )
  ALLOCATE ( xqfx    (ncols_x, nrows_x) )
  ALLOCATE ( xustar  (ncols_x, nrows_x) )

  ALLOCATE ( xpbl    (ncols_x, nrows_x) )
  ALLOCATE ( xpbl2   (ncols_x, nrows_x) )
  ALLOCATE ( xpblr   (ncols_x, nrows_x) )

  ALLOCATE ( xmixht  (ncols_x, nrows_x) )
  ALLOCATE ( xzruf   (ncols_x, nrows_x) )
  ALLOCATE ( xmol    (ncols_x, nrows_x) )
  ALLOCATE ( xrgrnd  (ncols_x, nrows_x) )
  ALLOCATE ( xwr     (ncols_x, nrows_x) )
  ALLOCATE ( xlai    (ncols_x, nrows_x) )
  ALLOCATE ( xsnowc  (ncols_x, nrows_x) )
  ALLOCATE ( xveg    (ncols_x, nrows_x) )
  ALLOCATE ( xch     (ncols_x, nrows_x) )

!-------------------------------------------------------------------------------
! All other CROSS2D arrays.
!-------------------------------------------------------------------------------

  ! CROSS2D arrays filled either by PBLPKG or PBLSUP.

  ALLOCATE ( xwstar  (ncols_x, nrows_x) )
  ALLOCATE ( xradyn  (ncols_x, nrows_x) )
  ALLOCATE ( xrbndy  (ncols_x, nrows_x) )
  ALLOCATE ( xrstom  (ncols_x, nrows_x) )
  ALLOCATE ( xtemp2  (ncols_x, nrows_x) )
  ALLOCATE ( xwspd10 (ncols_x, nrows_x) )
  ALLOCATE ( xwdir10 (ncols_x, nrows_x) )
  ALLOCATE ( xcfract (ncols_x, nrows_x) )
  ALLOCATE ( xcldtop (ncols_x, nrows_x) )
  ALLOCATE ( xcldbot (ncols_x, nrows_x) )
  ALLOCATE ( xwbar   (ncols_x, nrows_x) )
  ALLOCATE ( xsnocov (ncols_x, nrows_x) )
  ALLOCATE ( xtrop   (ncols_x, nrows_x) )

  ALLOCATE ( xattenX (ncols_x, nrows_x) )
  ALLOCATE ( xatten  (ncols_x, nrows_x) )
  ALLOCATE ( xcsrad  (ncols_x, nrows_x) )
  ALLOCATE ( xcstoa  (ncols_x, nrows_x) )
  ALLOCATE ( xcsusf  (ncols_x, nrows_x) )
  ALLOCATE ( xcsdsf  (ncols_x, nrows_x) )
  ALLOCATE ( xalbedo (ncols_x, nrows_x) )

  ALLOCATE ( xpsccb  (ncols_x, nrows_x) )
  ALLOCATE ( xpscct  (ncols_x, nrows_x) )
  ALLOCATE ( xpdccb  (ncols_x, nrows_x) )
  ALLOCATE ( xpdcct  (ncols_x, nrows_x) )
  ALLOCATE ( xptccb  (ncols_x, nrows_x) )
  ALLOCATE ( xptcct  (ncols_x, nrows_x) )

  ALLOCATE ( xpdtop  (ncols_x, nrows_x) )
  ALLOCATE ( xpdbot  (ncols_x, nrows_x) )

  ALLOCATE ( xsotyp  (ncols_x, nrows_x) )
  ALLOCATE ( xsoilw  (ncols_x, nrows_x) )

  ALLOCATE ( xvd     (ncols_x, nrows_x, ltotg)  )

!-------------------------------------------------------------------------------
! Following could be used for both time independent and dependent processing
! CROSS3D arrays for full ctm domain (include boundary).
!-------------------------------------------------------------------------------

  ALLOCATE ( xtempm   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xpresm   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdensam  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xjdensam (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdenswm  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xentrp   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( x3jacobf (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( x3jacobm (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( x3htf    (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( x3htm    (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xwhat    (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xwvapor  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xwwind   (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xcldwtr  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xranwtr  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xqice    (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xqsnow   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xeddyv   (ncols_x, nrows_x, 0:metlay) )

  ALLOCATE ( xpres3d  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xpres3df (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xgeohgt  (ncols_x, nrows_x,   metlay) )

  ALLOCATE ( xuua     (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xvva     (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xuhata   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xvhata   (ncols_x, nrows_x,   metlay) )

  ALLOCATE ( xpvc     (ncols_x, nrows_x,   metlay) )

!-------------------------------------------------------------------------------
! Time-dependent chemical species arrays for full CTM domain.
!-------------------------------------------------------------------------------

  ALLOCATE ( xo3      (ncols_x, nrows_x,   metlay) )

!-------------------------------------------------------------------------------
! Time-dependent DOT3D arrays for full CTM domain (include boundary).
!-------------------------------------------------------------------------------

  ALLOCATE ( xuu      (ncols_x-1, nrows_x-1, metlay) )
  ALLOCATE ( xvv      (ncols_x-1, nrows_x-1, metlay) )
  ALLOCATE ( xuhat    (ncols_x-1, nrows_x-1, metlay) )
  ALLOCATE ( xvhat    (ncols_x-1, nrows_x-1, metlay) )

  ALLOCATE ( xu_s     (ncols_x-1, nrows_x-1, metlay) )
  ALLOCATE ( xv_t     (ncols_x-1, nrows_x-1, metlay) )

!-------------------------------------------------------------------------------
! Other arrays used in the main (MCIP) and other subroutines.
!-------------------------------------------------------------------------------

  ALLOCATE ( xrtold   (ncols_x, nrows_x)           )
  ALLOCATE ( xdx3htf  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdensaf  (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xjdensaf (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xpresf   (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xdflux   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xprsold  (ncols_x, nrows_x)           )
  ALLOCATE ( xpsrate  (ncols_x, nrows_x)           )

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic model.
!-------------------------------------------------------------------------------

  ALLOCATE ( xdensam_ref (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdensaf_ref (ncols_x, nrows_x, 0:metlay) )

!-------------------------------------------------------------------------------
! Set up pointer for dry deposition indices.
!-------------------------------------------------------------------------------

  ALLOCATE ( xdepspc (ltotg) )
  ALLOCATE ( xdepidx (ltotg) )
    lso2       => xdepidx( 1)    ;    lso4       => xdepidx( 2)
    lno2       => xdepidx( 3)    ;    lno        => xdepidx( 4)
    lo3        => xdepidx( 5)    ;    lhno3      => xdepidx( 6)
    lh2o2      => xdepidx( 7)    ;    lald       => xdepidx( 8)
    lhcho      => xdepidx( 9)    ;    lop        => xdepidx(10)
    lpaa       => xdepidx(11)    ;    lora       => xdepidx(12)
    lnh3       => xdepidx(13)    ;    lpan       => xdepidx(14)
    lhono      => xdepidx(15)    ;    lco        => xdepidx(16)
    lmethanol  => xdepidx(17)    ;    ln2o5      => xdepidx(18)
    lno3       => xdepidx(19)    ;    lgald      => xdepidx(20)

END SUBROUTINE alloc_x
