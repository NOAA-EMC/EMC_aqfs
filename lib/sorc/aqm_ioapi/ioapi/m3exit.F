
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/m3exit.F,v 1.3 2000/11/28 21:13:41 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE M3EXIT( CALLER, JDATE, JTIME, MSGTXT, EXITSTAT )

C***********************************************************************
C  subroutine body starts at line  67
C
C  FUNCTION:  Generate simple error messages for Models-3 core;
C             terminate program execution via CALL EXIT( EXITSTAT )
C
C  PRECONDITIONS REQUIRED:  JDATE:JTIME represented as YYYYDDD:HHMMSS
C
C  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3, SHUT3
C
C  REVISION  HISTORY:	
C	prototype 5/92 by CJC
C       Revised   8/96 to close currently-open POSIX-OK Fortran units.
C       Modified  1/97 by CJC to trim trailing blanks from MSGTXT
C       Modified  2/97 by CJC:  conditional definition of EXIT under AIX 
C       Modified 10/98 by CJC:  Factor all output through m3msg2(), to
C                      get around SGI OpenMP bug.
C                      Caution:  messages may not be sequenced correctly
C                      when called by multiple threads simultaneously
C       Modified  5/99 by ALT for coupling-mode operation
C***********************************************************************

#ifdef   _AIX
#define  EXIT exit_
#endif

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   CALLER          !  name of the caller
        INTEGER         JDATE, JTIME    !  model date&time for the error
        CHARACTER*(*)   MSGTXT          !  error message
        INTEGER         EXITSTAT        !  exit status for program

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER*24    DT2STR
        EXTERNAL        DT2STR

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG
        INTEGER         IDEV, ISTAT
        LOGICAL         OFLAG


C***********************************************************************
C   begin body of subroutine  M3EXIT

#ifdef IOAPICPL
        IF ( EXITSTAT .NE. 0 ) THEN
            CALL SETSYNCHRO3V(1)
        END IF
#endif
        CALL M3MSG2( ' ' )      !  guarantees initialization of logdev

        IF ( .NOT. SHUT3() ) THEN
            MESG = 'Could not shut down I/ O API files correctly'
            CALL M3MSG2( MESG )
        END IF

        IF ( EXITSTAT .NE. 0 ) THEN
            MESG = '*** ERROR ABORT in subroutine ' // CALLER
        ELSE    !  errstat = 0:  successful completion
            MESG = '--->>  Normal Completion of program ' // CALLER
        END IF


        CALL M3MSG2( MESG )
        CALL M3MSG2( MSGTXT )

        IF ( JDATE .GT. 0  .OR.  JTIME .GT. 0 ) THEN
            WRITE( MESG, '( A, 2X, A, 2X, A, I7, A, I6.6, A )' ) 
     &      'Date and time', DT2STR( JDATE, JTIME ),  
     &      '(', JDATE, ':', JTIME, ')'
            CALL M3MSG2( MESG )
        END IF

        DO  IDEV = 10, 99
            INQUIRE( UNIT = IDEV, OPENED = OFLAG, IOSTAT = ISTAT )
            !!  paranoia to deal with certain rare situations on SGI
            IF ( ISTAT .EQ. 0 ) THEN
                IF ( OFLAG ) CLOSE( UNIT = IDEV, IOSTAT = ISTAT )
                IF ( ISTAT .NE. 0 ) THEN
                    WRITE( MESG, '( A, 2X, I4, A, I10 )' )
     &                  'Error closing unit', IDEV,
     &                  ' status =', ISTAT
                    CALL M3MSG2( MESG )
                END IF
            END IF
        END DO

        CALL EXIT( EXITSTAT )

        END

