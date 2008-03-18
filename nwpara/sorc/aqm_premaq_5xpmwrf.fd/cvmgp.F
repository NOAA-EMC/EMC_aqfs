FUNCTION cvmgp (x1, x2, x3)

!-------------------------------------------------------------------------------
! Name:     Cray Vector Merge -- Positive
! Purpose:  Emulates Cray vector merge function for positive values.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL                         :: cvmgp
  REAL,          INTENT(IN)    :: x1
  REAL,          INTENT(IN)    :: x2
  REAL,          INTENT(IN)    :: x3

  IF ( x3 >= 0.0 ) THEN
    cvmgp = x1
  ELSE
    cvmgp = x2
  ENDIF

END FUNCTION cvmgp
