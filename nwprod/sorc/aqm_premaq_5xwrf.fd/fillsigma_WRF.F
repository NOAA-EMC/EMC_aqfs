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

  ! CALL heap_sort_down (inmetlays)
  ! CALL heap_sort_down (inmetlaysht)

!-------------------------------------------------------------------------------
! Fill half-sigma and full-sigma arrays.
!
! ETA1 & ETA2 are the sigma value of the top and bottom region of WRF-NMM
! Value of ETA1 & ETA2 are from 1 to 0, the transition from ETA2 to ETA1 is at 
! level "neta2+1" (where ETA1=1, ETA2=0)
!-------------------------------------------------------------------------------

  IF ( metvert == 1 ) THEN  !

    DO n = 1, nz
      sigmah(n) = inmetlays(nz+1-n) / 1000000.0
    ENDDO

    IF ( nh == nz + 1 ) THEN                ! assume heights are on full layers
      DO n = 1, nz + 1
        sigmaf(n) = inmetlaysht(n+1-n) / 1000000.0
      ENDDO

    ELSE                                    ! need to calculate full layers

      sigmaf(1) = 1.0000                ! lowest layer of Eta2
      DO n = 1, nz-1
         IF (n == neta2) THEN           ! transition of ETA1 & ETA2
            sigmaf(n+1) = 1.0           ! lowest layer of Eta1
         ELSE
            sigmaf(n+1) = 2.0 * sigmah(n) - sigmaf(n)
         ENDIF
      ENDDO
      sigmaf(nz+1) = 0.0000             ! Top layer of  Eta1

    ENDIF

  ELSE

    PRINT*, '...need to modify code to get NMM levels'
    STOP 645

  ENDIF

END SUBROUTINE fillsigma
