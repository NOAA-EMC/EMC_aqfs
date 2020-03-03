SUBROUTINE comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Common Header
! Purpose:  Builds a common header part for I/O API output.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE fdesc3
  USE premaqparm, ONLY: nlays

  IMPLICIT NONE

  INTEGER,       INTENT(IN)    :: sdate       ! YYYYDDD
  INTEGER,       INTENT(IN)    :: stime       ! HHMMSS

!-------------------------------------------------------------------------------
! Fill common headers from MODULE COORD.
!-------------------------------------------------------------------------------

  sdate3d = sdate
  stime3d = stime

  gdnam3d = gdname_gd
  gdtyp3d = gdtyp_gd
  p_alp3d = p_alp_gd
  p_bet3d = p_bet_gd
  p_gam3d = p_gam_gd

  xcent3d = xcent_gd
  ycent3d = ycent_gd
  xorig3d = xorig_gd
  yorig3d = yorig_gd
  xcell3d = xcell_gd
  ycell3d = ycell_gd

  vgtyp3d = vgtyp_gd
  vgtop3d = vgtop_gd

  ! Layer defined in standard met. coordinate.

  vglvs3d(1:nlays+1) = vglvs_gd(1:nlays+1)
 
  ! Initialize FDESC3D and UPDESC3D array.

  fdesc3d(1:mxdesc3) = ' '
  updsc3d(1:mxdesc3) = ' '

END SUBROUTINE comheader
