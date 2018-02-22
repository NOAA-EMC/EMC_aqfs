MODULE xvars

!-------------------------------------------------------------------------------
! Name:     X-Variables
! Purpose:  Contains X-variables (CTM arrays plus boundary in horizontal).
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
!           14 JAN 2005  Added XATTEN, XCSRAD, XALBEDO, AND XCSTOA (Hsin-mu Lin)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  REAL              :: x3top       ! top of X-array data
  REAL              :: x_resol     ! horizontal resol. of X-array data [m]

  ! Layer face and middle point coordinate (increasing monotonically)

  REAL, ALLOCATABLE :: xx3face ( : )   ! layer face of X-array data
  REAL, ALLOCATABLE :: xx3midl ( : )   ! layer middle of X-array data
  REAL, ALLOCATABLE :: xdx3    ( : )   ! layer thickness (positive always)

!-------------------------------------------------------------------------------
! Two-Dimensional Arrays   
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xlatd   ( : , : )  ! latitude  at DOT [degrees]
  REAL, ALLOCATABLE :: xlond   ( : , : )  ! longitude at DOT [degrees]
  REAL, ALLOCATABLE :: xmapd   ( : , : )  ! map scale at DOT [dim'less]
  REAL, ALLOCATABLE :: xfc     ( : , : )  ! Coriolis force at cross point

  REAL, ALLOCATABLE :: xmol2   ( : , : )  ! Monin-Obukhov length [m], Special for ACM2 PBL
  REAL, ALLOCATABLE :: xustar2 ( : , : )  ! special for ACM2 PBL
  REAL, ALLOCATABLE :: xwstar2 ( : , : )  ! special for ACM2 PBL

!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain excluding boundary
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xpurb   ( : , : )  ! percentage of urban category [%]
  REAL, ALLOCATABLE :: xlufrac_01   ( : , : )  ! Urban Land (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_02   ( : , : )  ! Dryland Cropland and Pasture (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_03   ( : , : )  ! Irrigated Cropland and Pasture (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_04   ( : , : )  ! Mixed Dryland and Irrigated Cropland and Pasture (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_05   ( : , : )  ! Cropland-Grassland Mosaic (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_06   ( : , : )  ! Cropland-Woodland Mosaic (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_07   ( : , : )  ! Grassland (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_08   ( : , : )  ! Shrubland (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_09   ( : , : )  ! Mixed Shrubland-Grassland (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_10   ( : , : )  ! Savannah (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_11   ( : , : )  ! Deciduous Broadleaf Forest (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_12   ( : , : )  ! Deciduous Needleleaf Forest (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_13   ( : , : )  ! Evergreen Broadleaf Forest (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_14   ( : , : )  ! Evergreen Needleleaf Forest (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_15   ( : , : )  ! Mixed Forest (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_16   ( : , : )  ! Water (FRACTION)
  REAL, ALLOCATABLE :: xlufrac_17   ( : , : )  ! Herbaceous Wetland (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_18   ( : , : )  ! Wooded Wetland (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_19   ( : , : )  ! Barren or Sparsely Vegetated (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_20   ( : , : )  ! Herbaceous Tundra (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_21   ( : , : )  ! Wooded Tundra (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_22   ( : , : )  ! Mixed Tundra (FRACTION) 
  REAL, ALLOCATABLE :: xlufrac_23   ( : , : )  ! Bare Ground Tundra (FRACTION)  
  REAL, ALLOCATABLE :: xlufrac_24   ( : , : )  ! Snow or Ice (FRACTION)
  
!-------------------------------------------------------------------------------
! CROSS2D arrays for full CTM domain including boundary
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xlatc   ( : , : )  ! latitude  at CROSS [degree]
  REAL, ALLOCATABLE :: xlonc   ( : , : )  ! longitude at CROSS [degree]
  REAL, ALLOCATABLE :: xmapc   ( : , : )  ! map scale at CROSS [dim'less]
  REAL, ALLOCATABLE :: xtopo   ( : , : )  ! topographic height (MSL) [m]

  REAL, ALLOCATABLE :: xmap_s  ( : , : )  ! map scale at C-grid U
  REAL, ALLOCATABLE :: xmap_t  ( : , : )  ! map scale at C-grid V

  REAL, ALLOCATABLE :: xprsfc  ( : , : )  ! sfc pressure at cross [Pa]
  REAL, ALLOCATABLE :: xdenss  ( : , : )  ! surface air density
  REAL, ALLOCATABLE :: xtempg  ( : , : )  ! ground surface temperature [K]
  REAL, ALLOCATABLE :: xrainn  ( : , : )  ! nonconvective rain (cumulative)
  REAL, ALLOCATABLE :: xrainc  ( : , : )  ! convective rain (cumulative)
  REAL, ALLOCATABLE :: xdluse  ( : , : )  ! dominant land use category

  REAL, ALLOCATABLE :: xglw    ( : , : )
  REAL, ALLOCATABLE :: xgsw    ( : , : )
  REAL, ALLOCATABLE :: xhfx    ( : , : )
  REAL, ALLOCATABLE :: xqfx    ( : , : )
  REAL, ALLOCATABLE :: xustar  ( : , : )

  REAL, ALLOCATABLE :: xpbl    ( : , : )
  REAL, ALLOCATABLE :: xpbl2   ( : , : )
  REAL, ALLOCATABLE :: xpblr   ( : , : )
  REAL, ALLOCATABLE :: xmixht  ( : , : )

  REAL, ALLOCATABLE :: xzruf   ( : , : )  ! surface roughness [m]
  REAL, ALLOCATABLE :: xmol    ( : , : )  ! Monin-Obukhov length [m] 
  REAL, ALLOCATABLE :: xrgrnd  ( : , : )

!-------------------------------------------------------------------------------
! All other CROSS2D arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xwstar  ( : , : )
  REAL, ALLOCATABLE :: xradyn  ( : , : )
  REAL, ALLOCATABLE :: xrbndy  ( : , : )
  REAL, ALLOCATABLE :: xrstom  ( : , : )
  REAL, ALLOCATABLE :: xtemp2  ( : , : )
  REAL, ALLOCATABLE :: xwspd10 ( : , : )
  REAL, ALLOCATABLE :: xwdir10 ( : , : )
  REAL, ALLOCATABLE :: xcfract ( : , : )
  REAL, ALLOCATABLE :: xcldtop ( : , : ) 
  REAL, ALLOCATABLE :: xcldbot ( : , : ) 
  REAL, ALLOCATABLE :: xwbar   ( : , : ) 
  REAL, ALLOCATABLE :: xsnocov ( : , : )
  REAL, ALLOCATABLE :: xtrop   ( : , : )

  REAL, ALLOCATABLE :: xattenX ( : , : )
  REAL, ALLOCATABLE :: xatten  ( : , : )
  REAL, ALLOCATABLE :: xcsrad  ( : , : )
  REAL, ALLOCATABLE :: xcstoa  ( : , : )
  REAL, ALLOCATABLE :: xcsusf  ( : , : )
  REAL, ALLOCATABLE :: xcsdsf  ( : , : )
  REAL, ALLOCATABLE :: xalbedo ( : , : )

  REAL, ALLOCATABLE :: xpsccb  ( : , : )
  REAL, ALLOCATABLE :: xpscct  ( : , : )
  REAL, ALLOCATABLE :: xpdccb  ( : , : )
  REAL, ALLOCATABLE :: xpdcct  ( : , : )
  REAL, ALLOCATABLE :: xptccb  ( : , : )
  REAL, ALLOCATABLE :: xptcct  ( : , : )

  REAL, ALLOCATABLE :: xsotyp  ( : , : )
  REAL, ALLOCATABLE :: xsoilw  ( : , : )

  REAL, ALLOCATABLE :: xsnowc  ( : , : )
  REAL, ALLOCATABLE :: xq2m    ( : , : )  !add by lipan
  REAL, ALLOCATABLE :: xsoim1  ( : , : )  !add by lipan
  REAL, ALLOCATABLE :: xsoim2  ( : , : )  !add by lipan 
  REAL, ALLOCATABLE :: xsoit1  ( : , : )  !add by lipan
  REAL, ALLOCATABLE :: xsoit2  ( : , : )  !add by lipan 
  REAL, ALLOCATABLE :: xsltyp  ( : , : )  !add by lipan 
  REAL, ALLOCATABLE :: xseaice ( : , : )  !add by lipan 
  INTEGER, ALLOCATABLE :: xvgtyp  ( : , : )  !add by lipan           

  REAL, ALLOCATABLE :: xpdtop  ( : , : )
  REAL, ALLOCATABLE :: xpdbot  ( : , : )

  REAL, ALLOCATABLE :: xvd     ( : , : , : ) ! species dep. velocity [m/s] 

!-------------------------------------------------------------------------------
! Additional CROSS2D arrays for dry deposition.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xwr     ( : , : )     ! precip intercepted by canopy [m]
  REAL, ALLOCATABLE :: xlai    ( : , : )     ! leaf area index [area/area]
  REAL, ALLOCATABLE :: xveg    ( : , : )     ! vegetation coverage [fraction]
  REAL, ALLOCATABLE :: xch     ( : , : )     ! sfc exchange coeff [kg/m3 m/s]

!-------------------------------------------------------------------------------
! Following could be used for both time independent & dependent processing
! CROSS3D arrays for full ctm domain (include boundary).
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xtempm  ( : , : , : )  ! temp. at layer middle [K]
  REAL, ALLOCATABLE :: xpresm  ( : , : , : )  ! pressure at layer middle [Pa]
  REAL, ALLOCATABLE :: xdensam ( : , : , : )  ! air density at middle [kg/m^3]
  REAL, ALLOCATABLE :: xjdensam( : , : , : )  ! air density at middle [kg/m^3], Jacobian only
  REAL, ALLOCATABLE :: xdenswm ( : , : , : )  ! vapor density at middle [kg/m^3]
  REAL, ALLOCATABLE :: xentrp  ( : , : , : )  ! entropy at middle [kg/m^3]
  REAL, ALLOCATABLE :: x3jacobf( : , : , : )  ! Jacobian at layer face [m]
  REAL, ALLOCATABLE :: x3jacobm( : , : , : )  ! Jacobian at layer middle [m]
  REAL, ALLOCATABLE :: x3htf   ( : , : , : )  ! AGL height at layer face [m]
  REAL, ALLOCATABLE :: x3htm   ( : , : , : )  ! AGL height at layer middle [m]
  REAL, ALLOCATABLE :: xwhat   ( : , : , : )  ! contra-w wind at face [m/s]
  REAL, ALLOCATABLE :: xwvapor ( : , : , : )  ! vapor specific humidity [qv]
  REAL, ALLOCATABLE :: xwwind  ( : , : , : )  ! vertical wind at face [m/s]
  REAL, ALLOCATABLE :: xcldwtr ( : , : , : )  ! cloud water mixing ratio
  REAL, ALLOCATABLE :: xranwtr ( : , : , : )  ! rain water mixing ratio
  REAL, ALLOCATABLE :: xqice   ( : , : , : )  ! ice mixing ratio
  REAL, ALLOCATABLE :: xqsnow  ( : , : , : )  ! snow mixing ratio
  REAL, ALLOCATABLE :: xcfrac_3d ( : , : , : )  ! NMM cloud fraction  !add by lipan
  REAL, ALLOCATABLE :: xeddyv  ( : , : , : )  ! vert. eddy diffusivity [m^2/s]

  REAL, ALLOCATABLE :: xpres3d ( : , : , : )  ! hybrid level pressure [Pa]
  REAL, ALLOCATABLE :: xpres3df( : , : , : )  ! interface level pressure [Pa]
  REAL, ALLOCATABLE :: xgeohgt ( : , : , : )  ! geopotential height

  REAL, ALLOCATABLE :: xpvc    ( : , : , : )  ! potential vorticity

!-------------------------------------------------------------------------------
! Time-dependent 3D arrays for full CTM domain (include boundary).
! original velocity input from PRODGEN A-grid
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xuua    ( : , : , : )  ! u component of hor. wind [m/s], A-grid
  REAL, ALLOCATABLE :: xvva    ( : , : , : )  ! v component of hor. wind [m/s], A-grid
  REAL, ALLOCATABLE :: xuhata  ( : , : , : )  ! contra-u wind for gen. CTM [m/s], A-grid
  REAL, ALLOCATABLE :: xvhata  ( : , : , : )  ! contra-v wind for gen. CTM [m/s], A-grid

!-------------------------------------------------------------------------------
! Time-dependent 3D arrays for full CTM domain (include boundary).
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xu_s    ( : , : , : )  ! C-grid U
  REAL, ALLOCATABLE :: xv_t    ( : , : , : )  ! C-grid V

!-------------------------------------------------------------------------------
! Time-dependent chemical species arrays for full CTM domain.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xo3     ( : , : , : )  ! ozone mixing ratio

!-------------------------------------------------------------------------------
! Time-dependent DOT3D arrays for full CTM domain (include boundary).
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xuu     ( : , : , : )  ! u component of hor. wind [m/s], B-grid
  REAL, ALLOCATABLE :: xvv     ( : , : , : )  ! v component of hor. wind [m/s], B-grid
  REAL, ALLOCATABLE :: xuhat   ( : , : , : )  ! contra-u wind for gen. CTM [m/s], B-grid
  REAL, ALLOCATABLE :: xvhat   ( : , : , : )  ! contra-v wind for gen. CTM [m/s], B-grid

!-------------------------------------------------------------------------------
! Other arrays used in the main (MCIP) and other subroutines.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xrtold  ( : , : )      ! total rain at previous hours
  REAL, ALLOCATABLE :: xdx3htf ( : , : , : )  ! layer thickness [m]
  REAL, ALLOCATABLE :: xdensaf ( : , : , : )  ! total air density at interface
  REAL, ALLOCATABLE :: xjdensaf( : , : , : )  ! total air density at interface, Jacobian only
  REAL, ALLOCATABLE :: xpresf  ( : , : , : )  ! total air pressure at face
  REAL, ALLOCATABLE :: xdflux  ( : , : , : )  ! flux divergence term
  REAL, ALLOCATABLE :: xprsold ( : , : )      ! sfc press at previous hours
  REAL, ALLOCATABLE :: xpsrate ( : , : )      ! D(PRSFC)/DT from OMEGA eq.

!-------------------------------------------------------------------------------
! Character arrays.
!-------------------------------------------------------------------------------

!  CHARACTER*16, ALLOCATABLE :: xlusname ( : )  ! land use category names

!-------------------------------------------------------------------------------
! Dry deposition arrays.
!-------------------------------------------------------------------------------

  CHARACTER*16, ALLOCATABLE         :: xdepspc  ( : )  ! deposition species
  INTEGER,      ALLOCATABLE, TARGET :: xdepidx  ( : )  ! species index

  INTEGER, POINTER :: &
             lso2,      lso4,      lno2,      lno,       lo3,        &
             lhno3,     lh2o2,     lald,      lhcho,     lop,        &
             lpaa,      lora,      lnh3,      lpan,      lhono,      &
             lco,       lmethanol, ln2o5,     lno3,      lgald

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic model:  derived
! partially based on the parameters given in the MM header record.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xdensam_ref ( : , : , : )  ! mid-lyr  ref dens  [kg/m^3]
  REAL, ALLOCATABLE :: xdensaf_ref ( : , : , : )  ! full-lvl ref dens  [Kg/m^3]

END MODULE xvars
