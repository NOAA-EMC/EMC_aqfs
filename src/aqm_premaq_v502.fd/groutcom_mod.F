MODULE groutcom

!-------------------------------------------------------------------------------
! Name:     Grid Output Common Blocks
! Purpose:  Contains PREMAQ grid output common blocks.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           21 Oct 2003  Removed fields associated with GRIDBDY2D and
!                        GRIDBDY3D.  (T. Otte)
!           09 Apr 2004  Added PURB to GRIDCRO2D.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Time independent dot 2D arrays for CTM domain.  (GRID_DOT_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gd2index   = 3

  REAL, ALLOCATABLE, TARGET :: gd2        ( : , : , : )

  REAL, POINTER :: glat_d     ( : , : )  ! lat (S- ; N+) [deg]
  REAL, POINTER :: glon_d     ( : , : )  ! lon (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_d   ( : , : )  ! sq map scale factor 

  ! Header descriptions

  CHARACTER*16,      PARAMETER :: gd2vname(gd2index) = (/ 'LAT  ',  &
                                                          'LON  ',  &
                                                          'MSFD2' /)
  CHARACTER*16,      PARAMETER :: gd2units(gd2index) = (/ 'DEGREES ', &
                                                          'DEGREES ', &
                                                          '(M/M)**2' /)
  CHARACTER*80,      PARAMETER :: gd2vdesc(gd2index) =                    &
                                    (/ 'latitude (south negative)     ',  &
                                       'longitude (west negative)     ',  &
                                       'squared map-scale factor (DOT)' /)

!-------------------------------------------------------------------------------
! Time independent cross 2d arrays for CTM domain.  (GRID_CRO_2D)
!-------------------------------------------------------------------------------

!  INTEGER, PARAMETER :: gc2index = 6
!  INTEGER, PARAMETER :: gc2index = 7   !add by lipan lwmask 
  INTEGER, PARAMETER :: gc2index = 31   !add by lipan lufrac_01...24   

  REAL, ALLOCATABLE, TARGET :: gc2        ( : , : , : )

  REAL, POINTER :: glat_c     ( : , : )     ! lat (S- ; N+) [deg]
  REAL, POINTER :: glon_c     ( : , : )     ! lon (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_c   ( : , : )     ! sq map scale factor
  REAL, POINTER :: gtopo_c    ( : , : )     ! elevation AGL [m]
  REAL, POINTER :: gdluse_c   ( : , : )     ! dominant land use category
  REAL, POINTER :: gpurb_c    ( : , : )     ! percentage of urban
  REAL, POINTER :: glwmask_c  ( : , : )     ! land-water mask (1=land, 0=water) 
  REAL, POINTER :: glufrac01_c  ( : , : )     ! Urban Land (FRACTION)  
  REAL, POINTER :: glufrac02_c  ( : , : )     ! Dryland Cropland and Pasture (FRACTION)
  REAL, POINTER :: glufrac03_c  ( : , : )     ! Irrigated Cropland and Pasture (FRACTION)  
  REAL, POINTER :: glufrac04_c  ( : , : )     ! Mixed Dryland and Irrigated Cropland and Pasture (FRACTION) 
  REAL, POINTER :: glufrac05_c  ( : , : )     ! Cropland-Grassland Mosaic (FRACTION)  
  REAL, POINTER :: glufrac06_c  ( : , : )     ! Cropland-Woodland Mosaic (FRACTION)
  REAL, POINTER :: glufrac07_c  ( : , : )     ! Grassland (FRACTION)  
  REAL, POINTER :: glufrac08_c  ( : , : )     ! Shrubland (FRACTION)
  REAL, POINTER :: glufrac09_c  ( : , : )     ! Mixed Shrubland-Grassland (FRACTION)  
  REAL, POINTER :: glufrac10_c  ( : , : )     ! Savannah (FRACTION)
  REAL, POINTER :: glufrac11_c  ( : , : )     ! Deciduous Broadleaf Forest (FRACTION)  
  REAL, POINTER :: glufrac12_c  ( : , : )     ! Deciduous Needleleaf Forest (FRACTION) 
  REAL, POINTER :: glufrac13_c  ( : , : )     ! Evergreen Broadleaf Forest (FRACTION)  
  REAL, POINTER :: glufrac14_c  ( : , : )     ! Evergreen Needleleaf Forest (FRACTION)
  REAL, POINTER :: glufrac15_c  ( : , : )     ! Mixed Forest (FRACTION)  
  REAL, POINTER :: glufrac16_c  ( : , : )     ! Water (FRACTION)          
  REAL, POINTER :: glufrac17_c  ( : , : )     ! Herbaceous Wetland (FRACTION) 
  REAL, POINTER :: glufrac18_c  ( : , : )     ! Wooded Wetland (FRACTION)
  REAL, POINTER :: glufrac19_c  ( : , : )     ! Barren or Sparsely Vegetated (FRACTION) 
  REAL, POINTER :: glufrac20_c  ( : , : )     ! Herbaceous Tundra (FRACTION) 
  REAL, POINTER :: glufrac21_c  ( : , : )     ! Wooded Tundra (FRACTION)  
  REAL, POINTER :: glufrac22_c  ( : , : )     ! Mixed Tundra (FRACTION)
  REAL, POINTER :: glufrac23_c  ( : , : )     ! Bare Ground Tundra (FRACTION)  
  REAL, POINTER :: glufrac24_c  ( : , : )     ! Snow or Ice (FRACTION) 
  
  ! For header information

  CHARACTER*16 :: gc2vname ( gc2index ) 
  CHARACTER*16 :: gc2units ( gc2index ) 
  CHARACTER*80 :: gc2vdesc ( gc2index ) 

  ! Header description

  DATA gc2vname / 'LAT',     'LON',     'MSFX2',     'HT',            &
                  'DLUSE',   'PURB',    'LWMASK',    'LUFRAC_01',     &
		  'LUFRAC_02', 'LUFRAC_03', 'LUFRAC_04', 'LUFRAC_05', &
		  'LUFRAC_06', 'LUFRAC_07', 'LUFRAC_08', 'LUFRAC_09', &	
		  'LUFRAC_10', 'LUFRAC_11', 'LUFRAC_12', 'LUFRAC_13', &
		  'LUFRAC_14', 'LUFRAC_15', 'LUFRAC_16', 'LUFRAC_17', &	
		  'LUFRAC_18', 'LUFRAC_19', 'LUFRAC_20', 'LUFRAC_21', &
		  'LUFRAC_22', 'LUFRAC_23', 'LUFRAC_24' /

  DATA gc2units / 'DEGREES', 'DEGREES', '(M/M)**2',  'M',             &
                  'CATEGORY','PERCENT', 'CATEGORY',  'FRACTION',      &
		  'FRACTION',  'FRACTION', 'FRACTION', 'FRACTION',    &
		  'FRACTION',  'FRACTION', 'FRACTION', 'FRACTION',    &		  
		  'FRACTION',  'FRACTION', 'FRACTION', 'FRACTION',    &
		  'FRACTION',  'FRACTION', 'FRACTION', 'FRACTION',    &		  
		  'FRACTION',  'FRACTION', 'FRACTION', 'FRACTION',    &
		  'FRACTION',  'FRACTION', 'FRACTION' /

  DATA gc2vdesc(1)  / 'latitude (south negative)'                                      /   
  DATA gc2vdesc(2)  / 'longitude (west negative)'                                      /   
  DATA gc2vdesc(3)  / 'squared map-scale factor (CROSS)'                               /   
  DATA gc2vdesc(4)  / 'terrain elevation'                                              /   
  DATA gc2vdesc(5)  / 'dominant land use category from classification USGS24'                                     /
  DATA gc2vdesc(6)  / 'percent of urban category'                                      /
  DATA gc2vdesc(7)  / 'land-water mask'                                                /
  DATA gc2vdesc(8)  / 'Urban Land (FRACTION)'                                          /  
  DATA gc2vdesc(9)  / 'Dryland Cropland and Pasture (FRACTION)'                        / 
  DATA gc2vdesc(10) / 'Irrigated Cropland and Pasture (FRACTION)'                      /  
  DATA gc2vdesc(11) / 'Mixed Dryland and Irrigated Cropland and Pasture (FRACTION)'    /   
  DATA gc2vdesc(12) / 'Cropland-Grassland Mosaic (FRACTION)'                           /  
  DATA gc2vdesc(13) / 'Cropland-Woodland Mosaic (FRACTION)'                            / 
  DATA gc2vdesc(14) / 'Grassland (FRACTION)'                                           /  
  DATA gc2vdesc(15) / 'Shrubland (FRACTION)'                                           /  
  DATA gc2vdesc(16) / 'Mixed Shrubland-Grassland (FRACTION)'                           /  
  DATA gc2vdesc(17) / 'Savannah (FRACTION)'                                            / 
  DATA gc2vdesc(18) / 'Deciduous Broadleaf Forest (FRACTION)'                          /  
  DATA gc2vdesc(19) / 'Deciduous Needleleaf Forest (FRACTION)'                         /   
  DATA gc2vdesc(20) / 'Evergreen Broadleaf Forest (FRACTION)'                          /  
  DATA gc2vdesc(21) / 'Evergreen Needleleaf Forest (FRACTION)'                         / 
  DATA gc2vdesc(22) / 'Mixed Forest (FRACTION)'                                        /  
  DATA gc2vdesc(23) / 'Water (FRACTION)'                                               /   
  DATA gc2vdesc(24) / 'Herbaceous Wetland (FRACTION)'                                  /  
  DATA gc2vdesc(25) / 'Wooded Wetland (FRACTION)'                                      / 
  DATA gc2vdesc(26) / 'Barren or Sparsely Vegetated (FRACTION)'                        /  
  DATA gc2vdesc(27) / 'Herbaceous Tundra (FRACTION)'                                   /   
  DATA gc2vdesc(28) / 'Wooded Tundra (FRACTION)'                                       /  
  DATA gc2vdesc(29) / 'Mixed Tundra (FRACTION)'                                        / 
  DATA gc2vdesc(30) / 'Bare Ground Tundra (FRACTION)'                                  /  
  DATA gc2vdesc(31) / 'Snow or Ice (FRACTION)'                                         /   
  
     

!-------------------------------------------------------------------------------
! Time independent cross 3D arrays for CTM domain.  (GRID_CRO_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: gc3index = 2

  REAL, ALLOCATABLE, TARGET :: gc3        ( : , : , : , : )

  REAL, POINTER :: gx3htf_c   ( : , : , : )  ! lyr-top hgt abv grnd [m]
  REAL, POINTER :: gx3htm_c   ( : , : , : )  ! mid-lyr hgt abv grnd [m]

  ! For header information

  CHARACTER*16 :: gc3vname ( gc3index ) 
  CHARACTER*16 :: gc3units ( gc3index ) 
  CHARACTER*80 :: gc3vdesc ( gc3index ) 

  ! Header description

  DATA gc3vname / 'X3HT0F',  'X3HT0M' /

  DATA gc3units / 'M',       'M' /

  DATA gc3vdesc(1) / 'height of layer face (top) above ground'/   
  DATA gc3vdesc(2) / 'height of layer middle above ground'    /   

END MODULE groutcom
