SUBROUTINE graceful_stop (pname)

!-------------------------------------------------------------------------------
! Name:     Graceful Stop
! Purpose:  Gracefully stop program and close I/O API files.
! Revised:  09 Jan 2002  Original version.  (T. Otte)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time

  IMPLICIT NONE

  INTEGER,       PARAMETER     :: m3stat    = 6237  ! PREMAQ (NCEP) error
  CHARACTER*16,  INTENT(IN)    :: pname
  CHARACTER*80                 :: xmsg

  xmsg = 'ABNORMAL TERMINATION IN ' // TRIM(pname)
  CALL m3exit (pname, sdate, stime, xmsg, m3stat)

END SUBROUTINE graceful_stop
