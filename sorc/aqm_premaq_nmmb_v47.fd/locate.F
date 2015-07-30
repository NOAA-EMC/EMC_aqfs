SUBROUTINE locate (xx, x, j)

!-------------------------------------------------------------------------------
! Name:     Locate
! Purpose:  Locates index of value X for given monotonic array XX.
! Input:    XX:  array of monotonic values
!                For our application, it represents vertical coordinate values
!           X:   Value to bracket in XX
! Output:   J:   Index where X is located in XX
!                            XX(J) <= X < XX(J+1)  
!                 ***  I hope this is correct - test it DWB    
! Notes:    - This routine is adapted from the Numerical Recipe.
!           - Liberally adapted from EPA's MCIP2.
! Revised:  19 Mar 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       INTENT(OUT)   :: j
  INTEGER                      :: lbnd
  REAL,          PARAMETER     :: small      = 1.0e-6
  INTEGER                      :: ubnd
  REAL,          INTENT(IN)    :: x
  REAL,          INTENT(IN)    :: xx         ( : )

  ! Step thru array 1 element at a time.

  lbnd = LBOUND(xx,1)
  ubnd = UBOUND(xx,1)

  DO j = lbnd, ubnd-1
    IF ( ( x+small >= xx(j) ) .AND. ( x < xx(j+1) ) ) EXIT
  ENDDO

END SUBROUTINE locate
