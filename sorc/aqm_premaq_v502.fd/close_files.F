SUBROUTINE close_files

!-------------------------------------------------------------------------------
! Name:     Close Files
! Purpose:  Close I/O API files.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  09 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE iodecl3

  IMPLICIT NONE

  CHARACTER*16,  PARAMETER     :: pname      = 'CLOSE_FILES'

  IF ( .NOT. shut3() ) THEN
    WRITE (6,9000)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CLOSE_FILES',                        &
              /, 1x, '***   COULD NOT CLOSE I/O API OUTPUT FILES',         &
              /, 1x, 70('*'))

END SUBROUTINE close_files
