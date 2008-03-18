SUBROUTINE dynflds (sdate, gmt)

!-------------------------------------------------------------------------------
! Name:     Dynamic Fields
! Purpose:  Maps and calculates time-variant fields on MCIP X grids.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  09 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           16 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           29 Apr 2004  Removed call to EDYINTB.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: gmt        ! HH.MI (.MI = MI/60)
  INTEGER,       INTENT(IN)    :: sdate      ! YYYYDDD

!-------------------------------------------------------------------------------
! Put input meteorology variables on MCIP (X) grid.
!-------------------------------------------------------------------------------

  CALL metvars2ctm

!-------------------------------------------------------------------------------
! Calculate supplemental 3-D variables.
!-------------------------------------------------------------------------------

  CALL met3dsup
  
!-------------------------------------------------------------------------------
! Calculate planetary boundary layer height.
!-------------------------------------------------------------------------------

! CALL pblpkg  ! commented out in favor of Eta PBL height

!-------------------------------------------------------------------------------
! Calculate planetary boundary layer fields.
!-------------------------------------------------------------------------------

  CALL pblsup

!-------------------------------------------------------------------------------
! Calculate cloud fields.
!-------------------------------------------------------------------------------

  CALL bcldprc_ak

!-------------------------------------------------------------------------------
! Calculate radiation fields for attenuation.
!-------------------------------------------------------------------------------

  CALL solar (sdate, gmt)

!-------------------------------------------------------------------------------
! Calculate dry deposition.
!-------------------------------------------------------------------------------

  CALL etadry

END SUBROUTINE dynflds
