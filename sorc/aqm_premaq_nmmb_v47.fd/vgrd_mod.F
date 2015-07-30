MODULE vgrd

!-------------------------------------------------------------------------------
! Name:     Vertical Grid Dimension Parameters
! Purpose:  Contains vertical grid dimension parameters.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: x3face     ( : )  ! vert coord lyr sfc
  REAL,          ALLOCATABLE   :: x3midl     ( : )  ! vert coord lyr ctr

END MODULE vgrd
