MODULE mcoutcom

!-------------------------------------------------------------------------------
! Name:     Meteorology Cross-Point Output Common Blocks
! Purpose:  Contains PREMAQ meteorology cross-point output common blocks.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed JDRATE and reduced MC3INDEX to reflect
!                        one less array.  (T. Otte)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           21 Oct 2003  Removed fields associated with METBDY2D.  (T. Otte)
!           25 Nov 2003  Added JACOBF and JACOBS.  (T. Otte)
!           05 Dec 2003  Added TROP.  (T. Otte)
!           29 Apr 2004  Combined TEM1P5 and TEMP10 as TEMP2.  Added WSPD10
!                        and WDIR10.  (T. Otte)
!           14 JAN 2005  Added ATTEN, CSRAD, AND CSWTOA            (Hsin-mu Lin)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Time dependent cross 2D arrays for CTM domain.  (MET_CRO_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mc2index   = 45

  REAL, ALLOCATABLE, TARGET :: mc2        ( : , : , : )

  ! Parameters related to vertical grid.

  REAL, POINTER :: prsfc_c    ( : , : )      ! surface pressure [Pa]
  REAL, POINTER :: jacobs_c   ( : , : )      ! surface total Jacobian [m]

  ! Parameters for PBL.

  REAL, POINTER :: ustar_c    ( : , : )      ! cell-avg friction velocity [m/s]
  REAL, POINTER :: wstar_c    ( : , : )      ! convective velocity scale [m/s]

  REAL, POINTER :: pbl_c      ( : , : )      ! PBL height [m], TKE based
  REAL, POINTER :: pbl2_c     ( : , : )      ! PBL height [m], ACM2 based Richardson number
  REAL, POINTER :: pblr_c     ( : , : )      ! PBL height [m], NCEP based Richardson number
  REAL, POINTER :: mixht_c    ( : , : )      ! Mixed layer depth [m]

  REAL, POINTER :: zzero_c    ( : , : )      ! roughness length
  REAL, POINTER :: moli_c     ( : , : )      ! inverse Monin-Obukhov length [1/m] 
  REAL, POINTER :: hfx_c      ( : , : )      ! sensible heat flux [W/m**2]
  REAL, POINTER :: qfx_c      ( : , : )      ! latent heat flux [W/m**2]

  ! Resistances.

  REAL, POINTER :: radyni_c   ( : , : )      ! aerodynamic resistance [s/m]
  REAL, POINTER :: rbndyi_c   ( : , : )      ! laminar BL resistance [s/m]
  REAL, POINTER :: rstomi_c   ( : , : )      ! bulk stomatal resist [s/m]

  ! Parameters for surface meteorology.

  REAL, POINTER :: tempg_c    ( : , : )      ! ground skin temperature [K]
  REAL, POINTER :: temp2_c    ( : , : )      ! temperature at 2 m [K]
  REAL, POINTER :: wspd10_c   ( : , : )      ! wind speed at 10 m [m/s]
  REAL, POINTER :: wdir10_c   ( : , : )      ! wind direction at 10 m [degrees]
  REAL, POINTER :: lai_c      ( : , : )      ! lai (non-dim)
  REAL, POINTER :: snowc_c    ( : , : )      ! Snow cover (%)

  ! Parameters for soil

  REAL, POINTER :: sotyp_c    ( : , : )      ! soil type (as in Zobler) 
  REAL, POINTER :: soilw_c    ( : , : )      ! volumetric soil moisture content

  ! Parameters for radiation at surface.

  REAL, POINTER :: glw_c      ( : , : )      ! longwave radiation at ground
  REAL, POINTER :: gsw_c      ( : , : )      ! sol rad absorbed at ground
  REAL, POINTER :: rgrnd_c    ( : , : )      ! sol rad reaching ground=GSW_C/(1-ALB)
  REAL, POINTER :: csrad_c    ( : , : )      ! clear-sky SW flux on the surface, off -line
  REAL, POINTER :: cswtoa_c   ( : , : )      ! clear-sky SW flux on top of domain, off-line
  REAL, POINTER :: csusf_c    ( : , : )      ! upward clear-sky SW flux on the surface
  REAL, POINTER :: csdsf_c    ( : , : )      ! dnward clear-sky SW flux on the surface

  ! Parameters for precipitation.
    !!! note -- we store pcpn rate (per met. time step) rather than the 
    !!!         accumulated value

  REAL, POINTER :: rainn_c    ( : , : )      ! nonconvective precip [cm]
  REAL, POINTER :: rainc_c    ( : , : )      ! convective precip [cm]

  ! Parameters for clouds.

  REAL, POINTER :: cfract_c   ( : , : )      ! total fractional cloud coverage
  REAL, POINTER :: cldtop_c   ( : , : )      ! cloud top (K index in real)  ???
  REAL, POINTER :: cldbot_c   ( : , : )      ! cloud bottom (K index in real) ???
  REAL, POINTER :: wbar_c     ( : , : )      ! avg liquid water content of clouds
  REAL, POINTER :: psccb_c    ( : , : )      ! shallow convective cloud bottom
  REAL, POINTER :: pscct_c    ( : , : )      ! shallow convective cloud top
  REAL, POINTER :: pdccb_c    ( : , : )      ! deep convective cloud bottom
  REAL, POINTER :: pdcct_c    ( : , : )      ! deep convective cloud top
  REAL, POINTER :: ptccb_c    ( : , : )      ! convective cloud bottom
  REAL, POINTER :: ptcct_c    ( : , : )      ! convective cloud top

  ! Parameters for chemistry.

  REAL, POINTER :: trop_c     ( : , : )      ! tropopause height
  REAL, POINTER :: atten_c    ( : , : )      ! radiation attenuation factor
  REAL, POINTER :: attenX_c   ( : , : )      ! radiation attenuation factor, off-line

  ! Deposition velocities.

  REAL, POINTER :: vd_c       ( : , : , : )  ! deposition velocity [m/s]

  ! For header information (except for VD).

  CHARACTER*16 :: mc2vname ( mc2index ) 
  CHARACTER*16 :: mc2units ( mc2index ) 
  CHARACTER*80 :: mc2vdesc ( mc2index ) 

  ! Header description.

  DATA mc2vname / 'PRSFC',      'JACOBS',     'USTAR',      'WSTAR',       &
                  'PBL',        'ZRUF',       'MOLI',       'HFX',         &
                  'QFX',        'RADYNI',     'RBNDYI',     'RSTOMI',      &
                  'TEMPG',      'TEMP2',      'WSPD10',     'WDIR10',      &
                  'GLW',        'GSW',        'RGRND',      'RN',          &
                  'RC',         'CFRAC',      'CLDT',       'CLDB',        &
                  'WBAR',       'TROP',       'ATTEN_X',    'ATTEN',       &
                  'CSRAD',      'CSWTOA',     'CSUSF',      'CSDSF',       &
                  'PSCCB',      'PSCCT',      'PDCCB',      'PDCCT',       &
                  'PTCCB',      'PTCCT',      'PBL2' ,      'PBLR',        &
                  'MIXHT',      'SOTYP',      'SOILW',      'LAI',         &
                  'SNOWC'   /

  DATA mc2units / 'Pascal',     'M',          'M/S',        'M/S',         &
                  'M',          'M',          '1/M',        'WATTS/M**2',  &
                  'WATTS/M**2', 'M/S',        'M/S',        'M/S',         &
                  'K',          'K',          'M/S',        'DEGREES',     &
                  'WATTS/M**2', 'WATTS/M**2', 'WATTS/M**2', 'CM',          &
                  'CM',         'FRACTION',   'M',          'M',           &
                  'G/M**3',     'Pascal',     'DIM-LESS',   'DIM-LESS',    &
                  'WATTS/M**2', 'WATTS/M**2', 'WATTS/M**2', 'WATTS/M**2',  &
                  'Pascal',     'Pascal',     'Pascal',     'Pascal',      &
                  'Pascal',     'Pascal',     'M'     ,     'M',           &
                  'M',          'DIM_LESS',   'fraction',   'DIM_LESS',    &
                  'fraction' /

  DATA mc2vdesc( 1) / 'surface pressure'                    /   
  DATA mc2vdesc( 2) / 'total Jacobian at surface'           /
  DATA mc2vdesc( 3) / 'cell averaged friction velocity'     /   
  DATA mc2vdesc( 4) / 'convective velocity scale'           /   
  DATA mc2vdesc( 5) / 'PBL height'                          /   
  DATA mc2vdesc( 6) / 'surface rouhness length'             /   
  DATA mc2vdesc( 7) / 'inverse of Monin-Obukhov length'     /   
  DATA mc2vdesc( 8) / 'sensible heat flux'                  /   
  DATA mc2vdesc( 9) / 'latent heat flux'                    /   
  DATA mc2vdesc(10) / 'inverse of aerodynaic resistance'    /   
  DATA mc2vdesc(11) / 'inverse laminar bnd layer resistance'/   
  DATA mc2vdesc(12) / 'inverse of bulk stomatal resistance' /   
  DATA mc2vdesc(13) / 'skin temperature at ground'          /   
  DATA mc2vdesc(14) / 'air temperature at 2 m'              /
  DATA mc2vdesc(15) / 'wind speed at 10 m'                  /
  DATA mc2vdesc(16) / 'wind direction at 10 m'              /
  DATA mc2vdesc(17) / 'longwave radiation at ground'        /   
  DATA mc2vdesc(18) / 'solar radiation absorbed at ground'  /   
  DATA mc2vdesc(19) / 'solar rad reaching sfc'              /   
  DATA mc2vdesc(20) / 'nonconvec. pcpn per met TSTEP'       /   
  DATA mc2vdesc(21) / 'convective pcpn per met TSTEP'       /   
  DATA mc2vdesc(22) / 'total cloud fraction'                /   
  DATA mc2vdesc(23) / 'cloud top layer height (m)  '        /   
  DATA mc2vdesc(24) / 'cloud bottom layer height (m)  '     /   
  DATA mc2vdesc(25) / 'avg. liquid water content of cloud'  /   
  DATA mc2vdesc(26) / 'tropopause height'                   /

  DATA mc2vdesc(27) / 'radiation attenuation fctr, off-line'/
  DATA mc2vdesc(28) / 'radiation attenuation factor'        /
  DATA mc2vdesc(29) / 'dnward srfc clear-sky SW, off-line'  /
  DATA mc2vdesc(30) / 'dnward TOA SW, off-line'             /
  DATA mc2vdesc(31) / 'upward surface clear-sky SW (Eta)'   /
  DATA mc2vdesc(32) / 'downward surface clear-sky SW (Eta)' /

  DATA mc2vdesc(33) / 'shallow convective cloud bottom'     /
  DATA mc2vdesc(34) / 'shallow convective cloud top'        /
  DATA mc2vdesc(35) / 'deep convective cloud bottom'        /
  DATA mc2vdesc(36) / 'deep convective cloud top'           /
  DATA mc2vdesc(37) / 'convective cloud bottom'             /
  DATA mc2vdesc(38) / 'convective cloud top'                /

  DATA mc2vdesc(39) / 'PBL height, ACM2 based Richardson #' /
  DATA mc2vdesc(40) / 'PBL height, NCEP based Richardson #' /
  DATA mc2vdesc(41) / 'Mixed layer depth [m]'               /
  DATA mc2vdesc(42) / 'soil type'                           /
  DATA mc2vdesc(43) / 'volumetric soil moisture content'    /
  DATA mc2vdesc(44) / 'Leaf Area Index(non-dim)'            /
  DATA mc2vdesc(45) / 'Snow Cover'                          /

!-------------------------------------------------------------------------------
! Time dependent cross 3D arrays for CTM domain.  (MET_CRO_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mc3index   = 18

  REAL, ALLOCATABLE, TARGET :: mc3        ( : , : , : , : )

  ! Essential for generalized CTM.

  REAL, POINTER :: jacobf_c   ( : , : , : )  ! tot Jacobian at layer face
  REAL, POINTER :: jacobm_c   ( : , : , : )  ! tot Jacobian at mid-layer
  REAL, POINTER :: densa_j_c  ( : , : , : )  ! J weighted tot air density
  REAL, POINTER :: what_jd_c  ( : , : , : )  ! J-dens wgtd contra-W wind

  ! Used for cloud and AQCHEM.

  REAL, POINTER :: cldwtr_c   ( : , : , : )  ! cloud water mixing ratio
  REAL, POINTER :: ranwtr_c   ( : , : , : )  ! rain  water mixing ratio
  REAL, POINTER :: qice_c     ( : , : , : )  ! ice  mixing ratio
  REAL, POINTER :: qsnow_c    ( : , : , : )  ! snow mixing ratio

  ! Meteorological variables.

  REAL, POINTER :: tempa_c    ( : , : , : )  ! air temperature [K]
  REAL, POINTER :: wvapor_c   ( : , : , : )  ! water vapor mixing ratio
  REAL, POINTER :: press_c    ( : , : , : )  ! pressure [Pa] - Mid level 
  REAL, POINTER :: densa_c    ( : , : , : )  ! density [kg/m^3]
  REAL, POINTER :: wwind_c    ( : , : , : )  ! Z-comp of true wind [m/s]
  REAL, POINTER :: x3htm_c    ( : , : , : )  ! mid-layer height
  REAL, POINTER :: x3htf_c    ( : , : , : )  ! layer face height
  REAL, POINTER :: presf_c    ( : , : , : )  ! pressure [Pa] - Full level

  ! Vertical eddy diffusivity field.

  REAL, POINTER :: eddyv_c    ( : , : , : )  ! vert. eddy diffusivity

  ! Vertical Potential Vorticity

  REAL, POINTER :: pvc_c      ( : , : , : )  ! vert. potential vorticity

  ! For header information.

  CHARACTER*16 :: mc3vname ( mc3index ) 
  CHARACTER*16 :: mc3units ( mc3index ) 
  CHARACTER*80 :: mc3vdesc ( mc3index ) 

  ! Header description.

  DATA mc3vname / 'JACOBF',     'JACOBM',     'DENSA_J',    'WHAT_JD',     &
                  'QC',         'QR',         'QI',         'QS',          &
                  'TA',         'QV',         'PRES',       'DENS',        &
                  'WWIND',      'ZH',         'ZF',         'EDDYV',       &
                  'PV',         'PRES-F_lvl'   /

  DATA mc3units / 'M',          'M',          'KG/M**2',    'KG/M**2',     &
                  'KG/KG',      'KG/KG',      'KG/KG',      'KG/KG',       &
                  'K',          'KG/KG',      'Pa',         'KG/M**3',     &
                  'M/S',        'M',          'M',          'M**2/S',      &
                  'e-6*M^2*K/KG/S', 'Pa'  /

  DATA mc3vdesc( 1)  / 'total Jacobian at layer face'          /
  DATA mc3vdesc( 2)  / 'total Jacobian at layer middle'        /
  DATA mc3vdesc( 3)  / 'J weighted total air density'          /
  DATA mc3vdesc( 4)  / 'J & Density weighted vertical contra-W'/
  DATA mc3vdesc( 5)  / 'cloud water mixing ratio'              /
  DATA mc3vdesc( 6)  / 'rain water mixing ratio'               /
  DATA mc3vdesc( 7)  / 'ice mixing ratio'                      /
  DATA mc3vdesc( 8)  / 'snow mixing ratio'                     /
  DATA mc3vdesc( 9)  / 'air temperature'                       /
  DATA mc3vdesc(10)  / 'water vapor mixing ratio'              /
  DATA mc3vdesc(11)  / 'pressure - Mid level'                  /
  DATA mc3vdesc(12)  / 'total density of air'                  /
  DATA mc3vdesc(13)  / 'true W component of wind'              /
  DATA mc3vdesc(14)  / 'mid-layer height above ground'         /
  DATA mc3vdesc(15)  / 'full-layer height above ground'        /
  DATA mc3vdesc(16)  / 'vertical eddy diffusivity'             /
  DATA mc3vdesc(17)  / 'vertical potential vorticity'          /
  DATA mc3vdesc(18)  / 'pressure - Full level'                 /

!-------------------------------------------------------------------------------
! Time dependent boundary 3D arrays for CTM domain.  (MET_BDY_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mb3index   = mc3index

  REAL, ALLOCATABLE, TARGET :: mb3        ( : , : , : )

  ! Essential for generalized CTM.

  REAL, POINTER :: jacobf_b   ( : , : )  ! Total Jacobian at layer face
  REAL, POINTER :: jacobm_b   ( : , : )  ! Total Jacobian at mid-layer
  REAL, POINTER :: densa_j_b  ( : , : )  ! J weighted total air density
  REAL, POINTER :: what_jd_b  ( : , : )  ! J-density wghtd contra-W wind

  ! Used for cloud and AQCHEM.

  REAL, POINTER :: cldwtr_b   ( : , : )  ! cloud water mixing ratio
  REAL, POINTER :: ranwtr_b   ( : , : )  ! rain  water mixing ratio
  REAL, POINTER :: qice_b     ( : , : )  ! ice  mixing ratio
  REAL, POINTER :: qsnow_b    ( : , : )  ! snow mixing ratio

  ! Meteorological variables.

  REAL, POINTER :: tempa_b    ( : , : )  ! air temperature [K]
  REAL, POINTER :: wvapor_b   ( : , : )  ! water vapor mixing ratio
  REAL, POINTER :: press_b    ( : , : )  ! pressure [Pa] - Mid level
  REAL, POINTER :: densa_b    ( : , : )  ! density [kg/m^3]
  REAL, POINTER :: wwind_b    ( : , : )  ! Z component of true wind [m/s]
  REAL, POINTER :: x3htm_b    ( : , : )  ! mid-layer height
  REAL, POINTER :: x3htf_b    ( : , : )  ! layer face height
  REAL, POINTER :: presf_b    ( : , : )  ! pressure [Pa] - Full level

  ! Vertical eddy diffusivity.

  REAL, POINTER :: eddyv_b    ( : , : )  ! vert. eddy diffusivity

  ! Vertical Potential Vorticity

  REAL, POINTER :: pvc_b      ( : , : )  ! vert. potential vorticity

END MODULE mcoutcom
