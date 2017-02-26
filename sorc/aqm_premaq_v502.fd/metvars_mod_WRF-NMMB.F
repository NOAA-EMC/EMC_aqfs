MODULE metvars

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables
! Purpose:  Contains input meteorology arrays.
! Notes:    Variable names reflect GRIB abbreviations from Table 2 of
!           NCEP Office Note 388:  "GRIB Edition 1".
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Modified for Kh.  (T. Otte)
!           25 Nov 2003  Modified for GFS Ozone.  (T. Otte)
!           04 Dec 2003  Modified for tropopause height.  (T. Otte)
!           29 Apr 2004  Modified for 10-m component winds.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1-d meteorology arrays.
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: sigmaf     ( : )          ! interface sigma
  REAL,          ALLOCATABLE   :: sigmah     ( : )          ! hybrid sigma

!-------------------------------------------------------------------------------
! 3-d meteorology arrays.
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: pres       ( : , : , : )  ! pressure, calculated
  REAL,          ALLOCATABLE   :: pres3d     ( : , : , : )  ! pressure, read in
  REAL,          ALLOCATABLE   :: geohgt     ( : , : , : )  ! geopot'l height
  REAL,          ALLOCATABLE   :: tmp        ( : , : , : )  ! temperature
  REAL,          ALLOCATABLE   :: spc_h      ( : , : , : )  ! specific humidity
  REAL,          ALLOCATABLE   :: u_grda     ( : , : , : )  ! u-component wind
  REAL,          ALLOCATABLE   :: v_grda     ( : , : , : )  ! v-component wind
  REAL,          ALLOCATABLE   :: u_grd      ( : , : , : )  ! u-component wind, B-grid (dot point)
  REAL,          ALLOCATABLE   :: v_grd      ( : , : , : )  ! v-component wind, B-grid (dot point)
  REAL,          ALLOCATABLE   :: v_vel      ( : , : , : )  ! vertical velocity
  REAL,          ALLOCATABLE   :: tke        ( : , : , : )  ! turb. kin. energy
  REAL,          ALLOCATABLE   :: clwmr      ( : , : , : )  ! cloud water
  REAL,          ALLOCATABLE   :: c_ice      ( : , : , : )  ! cloud ice
  REAL,          ALLOCATABLE   :: rwmr       ( : , : , : )  ! rain water mx rat
  REAL,          ALLOCATABLE   :: snmr       ( : , : , : )  ! snow mix ratio
  REAL,          ALLOCATABLE   :: t_cdc      ( : , : , : )  ! total cloud cover

  REAL,          ALLOCATABLE   :: kth        ( : , : , : )  ! eddy diffusivity

  REAL,          ALLOCATABLE   :: o3mr       ( : , : , : )  ! GFS ozone mix rat

!-------------------------------------------------------------------------------
! 2-d meteorology arrays.
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: hgt        ( : , : )      ! surface height
  REAL,          ALLOCATABLE   :: temp_2m    ( : , : )      ! 2-m temperature
  REAL,          ALLOCATABLE   :: acpcp      ( : , : )      ! convective precip
  REAL,          ALLOCATABLE   :: fricv      ( : , : )      ! friction velocity
  REAL,          ALLOCATABLE   :: veg        ( : , : )      ! vegetation
  REAL,          ALLOCATABLE   :: lhtfl      ( : , : )      ! net latent ht flux
  REAL,          ALLOCATABLE   :: shtfl      ( : , : )      ! net sens heat flux
  REAL,          ALLOCATABLE   :: uswrf      ( : , : )      ! upward SW rad flux
  REAL,          ALLOCATABLE   :: ulwrf      ( : , : )      ! upward LW rad flux
  REAL,          ALLOCATABLE   :: ncpcp      ( : , : )      ! non-conv precip
  REAL,          ALLOCATABLE   :: psfc       ( : , : )      ! surface pressure
  INTEGER,       ALLOCATABLE   :: vgtyp      ( : , : )      ! vegetation type
  REAL,          ALLOCATABLE   :: cnwat      ( : , : )      ! plant canopy water
  REAL,          ALLOCATABLE   :: skint      ( : , : )      ! skin temperature
  REAL,          ALLOCATABLE   :: hpbl       ( : , : )      ! PBL height
  REAL,          ALLOCATABLE   :: hpblr      ( : , : )      ! PBL height, NCEP based Richardson number
  REAL,          ALLOCATABLE   :: hmixht     ( : , : )      ! Mixed layer depth [m]
  REAL,          ALLOCATABLE   :: ccond      ( : , : )      ! canopy conductance
  REAL,          ALLOCATABLE   :: sfexc      ( : , : )      ! sfc exchange coeff

  REAL,          ALLOCATABLE   :: sotyp      ( : , : )      ! soil type
  REAL,          ALLOCATABLE   :: soilw      ( : , : )      ! soil moisture fraction (0-10 cm)
  REAL,          ALLOCATABLE   :: lai        ( : , : )      ! Leaf Area Idex [non-dim]
  REAL,          ALLOCATABLE   :: snowc      ( : , : )      ! Snow Cover [%]
  REAL,          ALLOCATABLE   :: snocov     ( : , : )      ! Snow Cover [%]  add by lipan
  REAL,          ALLOCATABLE   :: q2m        ( : , : )      ! mixing ratio at 2 m [kg/kg]  add by lipan
  REAL,          ALLOCATABLE   :: soim1      ( : , : )      ! soil moisture 0-10cm [fraction]  add by lipan 
  REAL,          ALLOCATABLE   :: soim2      ( : , : )      ! soil moisture 10-40cm [fraction]  add by lipan 
  REAL,          ALLOCATABLE   :: soit1      ( : , : )      ! soil temperature 0-10cm [K]  add by lipan 
  REAL,          ALLOCATABLE   :: soit2      ( : , : )      ! soil temperaute 10-40cm [K]  add by lipan 
  INTEGER,       ALLOCATABLE   :: sltyp      ( : , : )      ! soil type USDA  add by lipan      
  REAL,          ALLOCATABLE   :: cicep      ( : , : )      ! surface ice pellets  add by lipan 
  
  REAL,          ALLOCATABLE   :: dswrf      ( : , : )      ! dnward SW rad flux
  REAL,          ALLOCATABLE   :: csusf      ( : , : )      ! upward clear sky SW flux
  REAL,          ALLOCATABLE   :: csdsf      ( : , : )      ! dnward clear sky SW flux
  REAL,          ALLOCATABLE   :: albdo      ( : , : )      ! albedo

  REAL,          ALLOCATABLE   :: psccb      ( : , : )      ! shallow convective cloud bottom
  REAL,          ALLOCATABLE   :: pscct      ( : , : )      ! shallow convective cloud top
  REAL,          ALLOCATABLE   :: pdccb      ( : , : )      ! deep convective cloud bottom
  REAL,          ALLOCATABLE   :: pdcct      ( : , : )      ! deep convective cloud top
  REAL,          ALLOCATABLE   :: ptccb      ( : , : )      ! convective cloud bottom
  REAL,          ALLOCATABLE   :: ptcct      ( : , : )      ! convective cloud top

  REAL,          ALLOCATABLE   :: trop       ( : , : )      ! tropopause height
  REAL,          ALLOCATABLE   :: u10m       ( : , : )      ! 10-m u-comp wind
  REAL,          ALLOCATABLE   :: v10m       ( : , : )      ! 10-m v-comp wind

  REAL,          ALLOCATABLE   :: pdtop      ( : , : )      ! pdtop
  REAL,          ALLOCATABLE   :: pdbot      ( : , : )      ! pd

  REAL,          ALLOCATABLE   :: smapc      ( : , : )      ! map-scale fac (cr)
  REAL,          ALLOCATABLE   :: smapd      ( : , : )      ! map-scale fac (dt)
  REAL,          ALLOCATABLE   :: latc       ( : , : )      ! latitude (cr)
  REAL,          ALLOCATABLE   :: latd       ( : , : )      ! latitude (dt)
  REAL,          ALLOCATABLE   :: lonc       ( : , : )      ! longitude (cr)
  REAL,          ALLOCATABLE   :: lond       ( : , : )      ! longitude (dt)
  REAL,          ALLOCATABLE   :: f          ( : , : )      ! Coriolis  (cr)
  REAL,          ALLOCATABLE   :: fd         ( : , : )      ! Coriolis  (dt)

!-------------------------------------------------------------------------------
! Scratch arrays.
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: rcold      ( : , : )      ! last hr acpcp
  REAL,          ALLOCATABLE   :: rnold      ( : , : )      ! last hr ncpcp
  REAL,          ALLOCATABLE   :: scr2d      ( : , : )      ! scratch 2d array

!-------------------------------------------------------------------------------
! Data flags.
!-------------------------------------------------------------------------------

  LOGICAL,       ALLOCATABLE   :: got_pres3d ( : )
  LOGICAL,       ALLOCATABLE   :: got_geohgt ( : )
  LOGICAL,       ALLOCATABLE   :: got_tmp    ( : )
  LOGICAL,       ALLOCATABLE   :: got_spc_h  ( : )
  LOGICAL,       ALLOCATABLE   :: got_u_grd  ( : )
  LOGICAL,       ALLOCATABLE   :: got_v_grd  ( : )
  LOGICAL,       ALLOCATABLE   :: got_v_vel  ( : )
  LOGICAL,       ALLOCATABLE   :: got_tke    ( : )
  LOGICAL,       ALLOCATABLE   :: got_clwmr  ( : )
  LOGICAL,       ALLOCATABLE   :: got_c_ice  ( : )
  LOGICAL,       ALLOCATABLE   :: got_rwmr   ( : )
  LOGICAL,       ALLOCATABLE   :: got_snmr   ( : )
  LOGICAL,       ALLOCATABLE   :: got_t_cdc  ( : )

  LOGICAL,       ALLOCATABLE   :: got_kth    ( : )

  LOGICAL,       ALLOCATABLE   :: got_o3mr   ( : )

  LOGICAL                      :: got_hgt
  LOGICAL                      :: got_temp_2m
  LOGICAL                      :: got_acpcp
  LOGICAL                      :: got_fricv
  LOGICAL                      :: got_veg
  LOGICAL                      :: got_lhtfl
  LOGICAL                      :: got_shtfl
  LOGICAL                      :: got_uswrf
  LOGICAL                      :: got_ulwrf
  LOGICAL                      :: got_ncpcp
  LOGICAL                      :: got_psfc
  LOGICAL                      :: got_vgtyp
  LOGICAL                      :: got_cnwat
  LOGICAL                      :: got_skint
  LOGICAL                      :: got_hpbl
  LOGICAL                      :: got_hpblr
  LOGICAL                      :: got_hmixht
  LOGICAL                      :: got_ccond
  LOGICAL                      :: got_sfexc
  LOGICAL                      :: got_land

  LOGICAL                      :: got_dswrf
  LOGICAL                      :: got_csusf
  LOGICAL                      :: got_csdsf
  LOGICAL                      :: got_albdo

  LOGICAL                      :: got_psccb
  LOGICAL                      :: got_pscct
  LOGICAL                      :: got_pdccb
  LOGICAL                      :: got_pdcct
  LOGICAL                      :: got_ptccb
  LOGICAL                      :: got_ptcct

  LOGICAL                      :: got_trop
  LOGICAL                      :: got_u10m
  LOGICAL                      :: got_v10m

  LOGICAL                      :: got_pdtop
  LOGICAL                      :: got_pd

  LOGICAL                      :: got_sotyp
  LOGICAL                      :: got_soilw

  LOGICAL                      :: got_lai
  LOGICAL                      :: got_snowc
  LOGICAL                      :: got_snocov   !add by lipan
  LOGICAL                      :: got_q2m      !add by lipan
  LOGICAL                      :: got_soim1    !add by lipan  
  LOGICAL                      :: got_soim2    !add by lipan 
  LOGICAL                      :: got_soit1    !add by lipan  
  LOGICAL                      :: got_soit2    !add by lipan  
  LOGICAL                      :: got_sltyp    !add by lipan 
  LOGICAL                      :: got_cicep    !add by lipan      

END MODULE metvars
