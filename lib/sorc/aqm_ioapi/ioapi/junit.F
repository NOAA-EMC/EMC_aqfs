
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/junit.F,v 1.3 2000/11/28 21:13:39 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION  JUNIT()

C***********************************************************************
C  function body starts at line 66
C
C  FUNCTION:
C
C    Routine returns next available FORTRAN unit number
C
C  ARGUMENT LIST DESCRIPTION:  empty argument list
C
C  RETURN VALUE:  JUNIT        Unit number selected
C
C  LOCAL VARIABLE DESCRIPTION:
C
C    IUNIT    state variable:  counts through available units
C    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
C    TOP      parameter:  last   ...
C
C  REVISION HISTORY:
C
C    3/88  Maximum number of I/O unit numbers was increased from 50
C          to 75 due to increased file I/O requirements.
C    5/88  Modified for ROMNET
C    7/90  Modified for ROM 2.2 -- uses EXWST for error abort.
C    8/90  Algorithm simplification:  replaced IF-GOTO loop by DO loop.
C    8/90  Algorithm simplification:  counting algorithm instead of table
C          of flags; uses POSIX standards-approved unit numbers 11-99
C    3/92  Models-3 Prototype version (eliminate EXWST)
C    8/96  Modified by CJC -- On counting-algorithm failure, performs
C          INQUIREs to find available unit.
C    2/97  conditional definition of EXIT under AIX
C    9/99  by CJC:  modifications for portability
C
C***********************************************************************

#ifdef   _AIX
#define  EXIT exit_
#endif

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

      INTEGER      BOT          !  1 less than initial unit number
      INTEGER      TOP          !  final unit number
      PARAMETER  ( BOT = 10, TOP = 99 )


C...........   LOCAL VARIABLES and their descriptions:

      INTEGER      J
      LOGICAL      FLAG

C............................................................................
C.......   begin body of JUNIT:

      DO J = TOP, BOT, -1
          INQUIRE( UNIT=J, OPENED=FLAG )
          IF ( .NOT. FLAG ) THEN
              JUNIT = J
              RETURN
          END IF 
      END DO
  
C.........   If you get to here: failure

      WRITE (*,91001) BOT, TOP
      CALL EXIT( 2 )
      RETURN


C*************************  FORMAT  STATEMENTS  **************************

C  Error and warning message formats     91xxx


91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',
     &          /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',
     &          /, 5X, 'First POSIX-approved unit:', I4 ,
     &          /, 5X, 'Last  POSIX-approved unit:', I4 ,
     &          //)

      END
