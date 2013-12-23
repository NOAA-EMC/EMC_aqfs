
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/hhmmss.f,v 1.3 2000/11/28 21:22:49 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        CHARACTER*10 FUNCTION  HHMMSS ( JTIME )

C***********************************************************************
C  function body starts at line  62
C
C  FUNCTION:  format and return the time as a character string 
C             "HH:MM:SS"
C
C
C  PRECONDITIONS REQUIRED:  valid time HHMMSS with hours component
C             at most 9999
C
C  RETURN VALUE:  time, as "HH:MM:SS"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:
C	prototype 10/90 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER       JTIME   !  Julian time, coded YYYYDDD


C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        CHARACTER*1	DIGITS( 0:9 )
        DATA       	DIGITS
     &  / '0','1','2','3','4','5','6','7','8','9' /

        SAVE DIGITS

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       HOUR
        INTEGER       MINS
        INTEGER       SECS
        INTEGER       J , K

        CHARACTER*10    CHRBUF


C***********************************************************************
C   begin body of function  HHMMSS

        CHRBUF = '          '

        HOUR = JTIME
        SECS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100
        MINS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100

        J = 1
        K = HOUR / 1000
        IF ( K .GT. 9 ) THEN
            HHMMSS = '<TIMERROR>'
            RETURN
        ELSE IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 100 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 10 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        CHRBUF( J:J ) = DIGITS( MOD( HOUR, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( MINS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( MINS, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( SECS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( SECS, 10 ) )

        HHMMSS = CHRBUF

        RETURN

        END

