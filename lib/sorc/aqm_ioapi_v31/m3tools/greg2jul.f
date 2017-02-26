
        PROGRAM GREG2JUL

C***********************************************************************
C Version "$Id: greg2jul.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 2014 UNC Institute for the Environment
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  67
C
C  DESCRIPTION:
C       Convert Gregorian-style dates to Julian-style and echo
C       the result (e.g., for use in scripting).
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       M3IO
C
C  REVISION  HISTORY:
C       Prototype  2/2014 by CJC
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........   EXTERNAL FUNCTION:

        INTEGER :: IARGC

C...........   PARAMETERs:  Lookup table for months:

        CHARACTER*3, PARAMETER :: MONTHS ( 12 ) = (/
     &  'JAN' , 'FEB' , 'MAR', 'APR' , 'MAY' , 'JUN',
     &  'JUL' , 'AUG' , 'SEP', 'OCT' , 'NOV' , 'DEC'  /)

        CHARACTER*64, PARAMETER :: SPLASH( 14 ) = (/
     &  '%  greg2jul <calendar date>                    ',
     &  'or                                             ',
     &  '% set gdate = `greg2jul 20140201`              ',
     &  '                                               ',
     &  'Options for <calendar date>:                   ',
     &  '     <MON> <DD> <YYYY>, e.g., Feb 12 2010      ',
     &  '     <MM>  <DD> <YYYY>, e.g., 02 12 2010       ',
     &  '     <YYYYMMDD>       , e.g., 20100212         ',
     &  '     TODAY                                     ',
     &  '     YESTERDAY                                 ',
     &  '     TOMORROW                                  ',
     &  'Case is NOT significant.                       ',
     &  'Use 3-letter month-names ("JAN", "FEB", etc.)  ',
     &  'Output format is 7-digit integer YYYYDDDD      '   /)

        CHARACTER*16, PARAMETER :: PNAME = 'GREG2JUL'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER     MON, DAY, YR, JDATE, JTIME, I
        INTEGER     ARGCNT  !  number of command-line args, from IARGC()

        CHARACTER*80    MONBUF
        CHARACTER*80    DAYBUF
        CHARACTER*80    YRBUF

C***********************************************************************
C   begin body of program GREG2JUL

        ARGCNT = IARGC()
        CALL GETDTTIME( JDATE, JTIME )

        IF ( ARGCNT .EQ. 1 ) THEN

            CALL GETARG( 1, MONBUF )
            CALL UPCASE( MONBUF )
            I = INDEX( MONBUF, '.' )
            IF ( I .GT. 0 ) MONBUF( I:I ) = ' '
            IF ( MONBUF(1:6) .EQ. '--HELP' ) THEN
                WRITE( *, '( 5X, A )' ) 
     &            'DESCRIPTION:  convert GRegorian dates to Julian',
     &            'USAGE:', ( SPLASH( I ), I = 1, 14 )
                CALL EXIT( 0 )
            ELSE IF ( MONBUF .EQ. 'TODAY' ) THEN
                CONTINUE
            ELSE IF ( MONBUF .EQ. 'YESTERDAY' ) THEN
                CALL NEXTIME( JDATE, JTIME, -240000 )
            ELSE IF ( MONBUF .EQ. 'TOMORROW' ) THEN
                CALL NEXTIME( JDATE, JTIME, 240000 )
            ELSE
                YR    = STR2INT( MONBUF )
                DAY   = MOD( YR , 100 )
                YR    =      YR / 100
                MON   = MOD( YR , 100 )
                YR    =      YR / 100
                JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )

                IF ( MON    .GT.   12  .OR.
     &               MON    .LT.    1  .OR.
     &               DAY    .GT.   31  .OR.
     &               DAY    .LT.    1  .OR.
     &               YR     .GT. 9999 )         CALL EXIT( 2 )
            END IF

        ELSE IF ( ARGCNT .EQ. 3 ) THEN

            CALL GETARG( 1, MONBUF )
            CALL UPCASE( MONBUF )
            DO  MON = 1, 12
                IF ( INDEX( MONBUF, MONTHS( MON ) ) .GT. 0 ) THEN
                    GO TO 12
                END IF
            END DO
            MON = STR2INT( MONBUF )
12          CONTINUE            !  month found by name

            CALL GETARG( 2, DAYBUF )
            DAY = STR2INT( DAYBUF )

            CALL GETARG( 3, YRBUF )
            YR = STR2INT( YRBUF )

            IF ( MON    .GT.   12  .OR.
     &           MON    .LT.    1  .OR.
     &           DAY    .GT.   31  .OR.
     &           DAY    .LT.    1  .OR.
     &           YR     .GT. 9999 )         CALL EXIT( 2 )
            JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )

        ELSE

            WRITE( *, '( 5X, A )' ) 
     &          'USAGE ERROR', ( SPLASH( I ), I=1, 14 )
            CALL EXIT( 2 )

        END IF

        WRITE( *, '( I7.7 )' ) JDATE

       CALL EXIT( 0 )

      END PROGRAM GREG2JUL
