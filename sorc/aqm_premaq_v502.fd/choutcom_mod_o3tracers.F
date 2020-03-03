MODULE choutcom

!-------------------------------------------------------------------------------
! Name:     Chemistry Output Common
! Purpose:  Contains PREMAQ chemistry output common arrays.
! Revised:  03 Dec 2003  Original version. (T. Otte)
!           29 Apr 2004  Allow CB3INDEX to be set dynamically, and moved
!                        allocation of arrays dependent on CB3INDEX to
!                        READCHEMBC.  (T. Otte)
!           14 Jun 2005  Added arrays for boundary tracers.  (T. Otte and
!                        R. Mathur)
!           24 Jun 2005  Added array to capture reference 03 profile.  (T. Otte)
!           25 Apr 2006  Changed tracers in TB3 from one per lateral boundary
!                        for ozone to tracers that nominally represent ozone in
!                        the boundary layer, free troposphere, and top layer.
!                        (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Time-dependent cross 3d arrays for CTM domain.  (CHEM_CRO_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: cc3index = 1

  REAL, ALLOCATABLE, TARGET :: cc3        ( : , : , : , : )

  REAL, POINTER :: o3_c       ( : , : , : )  ! ozone concentration

  INTEGER       :: pc_o3                     ! index for O3 cro pointer

  ! For header information

  CHARACTER*16 :: cc3vname ( cc3index ) 
  CHARACTER*16 :: cc3units ( cc3index ) 
  CHARACTER*80 :: cc3vdesc ( cc3index ) 

!-------------------------------------------------------------------------------
! Time-dependent boundary 3D arrays for CTM domain.  (CHEM_BDY_3D)
!-------------------------------------------------------------------------------

  INTEGER       :: cb3index              ! number of species in BC file

  REAL, ALLOCATABLE, TARGET :: cb3        ( : , : , : )
  REAL, ALLOCATABLE         :: o3_ref_b   ( : , : )

  REAL, POINTER :: o3_b       ( : , : )  ! ozone concentration

  INTEGER       :: pb_o3                 ! index for O3 bdy pointer

  ! For header information

  CHARACTER*16, ALLOCATABLE :: cb3vname ( : ) 
  CHARACTER*16, ALLOCATABLE :: cb3units ( : ) 
  CHARACTER*80, ALLOCATABLE :: cb3vdesc ( : ) 

  ! Boundary tracers.

  INTEGER, PARAMETER :: tb3index   = 5

  REAL, ALLOCATABLE, TARGET :: tb3        ( : , : , : )

  REAL, POINTER :: o3bl_b     ( : , : )  ! O3 in boundary layer
  REAL, POINTER :: o3ft_b     ( : , : )  ! O3 in free troposphere and above
  REAL, POINTER :: o3st_b     ( : , : )  ! O3 in stratosphere (e.g., top layer)
  REAL, POINTER :: o3stadv_b  ( : , : )  ! advected O3 in stratosphere
  REAL, POINTER :: o3sttrn_b  ( : , : )  ! transported O3 in stratosphere

  ! For header information.

  CHARACTER*16 :: tb3vname ( tb3index )
  CHARACTER*16 :: tb3units ( tb3index )
  CHARACTER*80 :: tb3vdesc ( tb3index )

  ! Header description.

  DATA tb3vname / 'O3BL',       'O3FT',       'O3ST',       'O3STADV', &
                  'O3STTRN' /

  DATA tb3units / 'ppmV',       'ppmV',       'ppmV',       'ppmV',    &
                  'ppmV' /

  DATA tb3vdesc( 1)  / 'O3 in boundary layer (e.g., below 3 km)'              /
  DATA tb3vdesc( 2)  / 'O3 in free troposphere and above (e.g., above 3 km)'  /
  DATA tb3vdesc( 3)  / 'O3 in stratosphere (e.g., top layer)'                 /
  DATA tb3vdesc( 4)  / 'advected O3 in stratosphere (e.g., top layer)'        /
  DATA tb3vdesc( 5)  / 'transported O3 in stratosphere (e.g., top layer)'     /

END MODULE choutcom
