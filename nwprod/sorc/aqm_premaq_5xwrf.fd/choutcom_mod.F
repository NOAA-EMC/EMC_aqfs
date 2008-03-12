MODULE choutcom

!-------------------------------------------------------------------------------
! Name:     Chemistry Output Common
! Purpose:  Contains PREMAQ chemistry output common arrays.
! Revised:  03 Dec 2003  Original version. (T. Otte)
!           29 Apr 2004  Allow CB3INDEX to be set dynamically, and moved
!                        allocation of arrays dependent on CB3INDEX to
!                        READCHEMBC.  (T. Otte)
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

  REAL, POINTER :: o3_b       ( : , : )  ! ozone concentration

  INTEGER       :: pb_o3                 ! index for O3 bdy pointer

  ! For header information

  CHARACTER*16, ALLOCATABLE :: cb3vname ( : ) 
  CHARACTER*16, ALLOCATABLE :: cb3units ( : ) 
  CHARACTER*80, ALLOCATABLE :: cb3vdesc ( : ) 

END MODULE choutcom
