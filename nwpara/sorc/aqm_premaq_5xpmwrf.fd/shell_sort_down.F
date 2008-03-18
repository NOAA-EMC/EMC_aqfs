SUBROUTINE shell_sort_down (array)

!-------------------------------------------------------------------------------
! Name:     Shell Sort
! Purpose:  Sorts ARRAY into descending numerical order by Shell's method
!           of diminishing increment sort.  Recommended for sorting 50 or
!           fewer elements.
! Notes:    Taken from "Numerical Recipes in Fortran, Second Edition" by
!           Press et al. (1992).  See Section 8.1.
! Revised:  ?? ??? 1992  Original version in Numerical Recipes.  (Press et al.)
!           24 Feb 2003  Converted to Fortran 90.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       INTENT(INOUT) :: array     (:)
  INTEGER                      :: i
  INTEGER                      :: inc
  INTEGER                      :: j
  INTEGER                      :: n
  INTEGER                      :: value

  n   = SIZE(array)

  inc = 1
  DO
    inc = 3*inc + 1
    IF ( inc > n ) EXIT
  ENDDO

  DO
    inc = inc / 3
    DO i = inc+1, n
      value = array(i)
      j = i
      DO
        IF ( array(j-inc) < value ) THEN
          array(j) = array(j-inc)
          j = j - inc
          IF ( j > inc ) CYCLE
        ENDIF
        EXIT
      ENDDO
      array(j) = value
    ENDDO
    IF ( inc <= 1 ) EXIT
  ENDDO

END SUBROUTINE shell_sort_down
