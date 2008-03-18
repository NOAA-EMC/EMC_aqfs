SUBROUTINE fillsigma (inmetlays, inmetlaysht)

!-------------------------------------------------------------------------------
! Name:     Fill Sigma Arrays
! Purpose:  Fill full-sigma and half-sigma arrays.
! Revised:  25 Feb 2003  Original version.  (T. Otte)
!           24 Jan 2004  Replaced shell_sort O(n^2) by heap_sort O(n log n)
!           (David Wong)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE premaqparm

  IMPLICIT NONE

  INTEGER,       INTENT(INOUT) :: inmetlays   ( maxmetlays )
  INTEGER,       INTENT(INOUT) :: inmetlaysht ( maxmetlays )
  INTEGER                      :: n

  INTERFACE

    SUBROUTINE heap_sort_down (array)
      IMPLICIT NONE
      INTEGER,       INTENT(INOUT) :: array       ( : )
    END SUBROUTINE heap_sort_down

  END INTERFACE

!-------------------------------------------------------------------------------
! Sort input arrays into descending order.  Note that "extra" unused slots
! in the input arrays are filled with "-1" in setup.F.
!-------------------------------------------------------------------------------

  CALL heap_sort_down (inmetlays)
  CALL heap_sort_down (inmetlaysht)

!-------------------------------------------------------------------------------
! Fill half-sigma and full-sigma arrays.
!-------------------------------------------------------------------------------

  IF ( metvert == 1 ) THEN  !

    DO n = 1, nz
      sigmah(n) = inmetlays(n) / 10000.0
    ENDDO

    IF ( nh == nz + 1 ) THEN  ! assume heights are on full layers
      DO n = 1, nz + 1
        sigmaf(n) = inmetlaysht(n) / 10000.0
      ENDDO
    ELSE  ! need to calculate full layers
      sigmaf(1) = 1.0000
      DO n = 1, nz-1
        sigmaf(n+1) = 2.0 * sigmah(n) - sigmaf(n)
      ENDDO
      sigmaf(nz+1) = 0.0000
    ENDIF

  ELSE

    PRINT*, '...need to modify code to get NMM levels'
    STOP 645

  ENDIF

END SUBROUTINE fillsigma
