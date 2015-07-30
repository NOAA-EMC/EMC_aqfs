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

  INTEGER, PARAMETER :: gc2index = 6

  REAL, ALLOCATABLE, TARGET :: gc2        ( : , : , : )

  REAL, POINTER :: glat_c     ( : , : )     ! lat (S- ; N+) [deg]
  REAL, POINTER :: glon_c     ( : , : )     ! lon (W- ; E+) [deg]
  REAL, POINTER :: gmsfsq_c   ( : , : )     ! sq map scale factor
  REAL, POINTER :: gtopo_c    ( : , : )     ! elevation AGL [m]
  REAL, POINTER :: gdluse_c   ( : , : )     ! dominant land use category
  REAL, POINTER :: gpurb_c    ( : , : )     ! percentage of urban

  ! For header information

  CHARACTER*16 :: gc2vname ( gc2index ) 
  CHARACTER*16 :: gc2units ( gc2index ) 
  CHARACTER*80 :: gc2vdesc ( gc2index ) 

  ! Header description

  DATA gc2vname / 'LAT',     'LON',     'MSFX2',     'HT',  &
                  'DLUSE',   'PURB' /

  DATA gc2units / 'DEGREES', 'DEGREES', '(M/M)**2',  'M',   &
                  'CATEGORY','PERCENT' /

  DATA gc2vdesc(1) / 'latitude (south negative)'       /   
  DATA gc2vdesc(2) / 'longitude (west negative)'       /   
  DATA gc2vdesc(3) / 'squared map-scale factor (CROSS)'/   
  DATA gc2vdesc(4) / 'terrain elevation'               /   
  DATA gc2vdesc(5) / 'dominant land use category'      /
  DATA gc2vdesc(6) / 'percent of urban category'       /

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
