MODULE mdoutcom

!-------------------------------------------------------------------------------
! Name:     Meteorology Dot-Point Output Common Blocks
! Purpose:  Contains PREMAQ meteorology dot-point output common blocks.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Time dependent dot 3D arrays for CTM domain.  (MET_DOT_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: md3index   = 4

  REAL, ALLOCATABLE, TARGET :: md3        ( : , : , : , : )

  REAL, POINTER :: uu_d       ( : , : , : )  ! U-comp of true wind [m/s]
                                             ! at dot point
  REAL, POINTER :: vv_d       ( : , : , : )  ! V-comp of true wind [m/s]
                                             ! at dot point
  REAL, POINTER :: uhat_s     ( : , : , : )  ! contravar-U multiplied by
                                             ! DENSITY*JACOBIAN at X-dir
                                             ! flux pt (square point)
  REAL, POINTER :: vhat_t     ( : , : , : )  ! contravar-V multiplied by
                                             ! DENSITY*JACOBIAN at Y-dir
                                             ! flux pt (triangle point)

  ! For header information.

  CHARACTER*16 :: md3vname ( md3index ) 
  CHARACTER*16 :: md3units ( md3index ) 
  CHARACTER*80 :: md3vdesc ( md3index ) 

  ! Header description.

  DATA md3vname / 'UWIND',   'VWIND',   'UHAT_JD',  'VHAT_JD'  /

  DATA md3units / 'M/S',     'M/S',     'KG/(M*S)', 'KG/(M*S)' /

  DATA md3vdesc(1) / 'U-comp. of true wind at dot point'          /   
  DATA md3vdesc(2) / 'V-comp. of true wind at dot point'          /   
  DATA md3vdesc(3) / '(contra_U*Jacobian*Density) at square pt'   /   
  DATA md3vdesc(4) / '(contra_V*Jacobian*Density) at triangle pt' /   

END MODULE mdoutcom
