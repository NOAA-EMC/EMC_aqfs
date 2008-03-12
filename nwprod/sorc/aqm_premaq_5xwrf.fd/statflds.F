SUBROUTINE statflds

!-------------------------------------------------------------------------------
! Name:     Static Fields
! Purpose:  Maps and calculates time-invariant fields on PREMAQ X grids.
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           25 Nov 2003  Removed commented call to GETLUSE since land use will
!                        always come directly from Eta.  (T. Otte)
!           25 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           09 Apr 2004  Included processing to get input from LANDA file.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE metvars

  IMPLICIT NONE

  INTERFACE

    SUBROUTINE gridgeometry (inner, f, smap, xlon, xlat)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: f         ( : , : )
      INTEGER,       INTENT(IN)    :: inner
      REAL,          INTENT(OUT)   :: smap      ( : , : )
      REAL,          INTENT(OUT)   :: xlat      ( : , : )
      REAL,          INTENT(OUT)   :: xlon      ( : , : )
    END SUBROUTINE gridgeometry

  END INTERFACE

!-------------------------------------------------------------------------------
! Extract urban land use percentage from external emissions file.
!-------------------------------------------------------------------------------

  CALL readlanda

!-------------------------------------------------------------------------------
! Create latitude, longitude, map-scale factors, and Coriolis
! from grid geometry.
!-------------------------------------------------------------------------------

  CALL gridgeometry (0,  f, smapc, lonc, latc)
  CALL gridgeometry (1, fd, smapd, lond, latd)

!-------------------------------------------------------------------------------
! Put time-invariant fields on PREMAQ grid.
!-------------------------------------------------------------------------------

  CALL metgrid2ctm

!-------------------------------------------------------------------------------
! Calculate reference state.
!-------------------------------------------------------------------------------

 ! CALL refstate

END SUBROUTINE statflds
