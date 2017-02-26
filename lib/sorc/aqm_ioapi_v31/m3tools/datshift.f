
        PROGRAM DATSHIFT

C***********************************************************************
C Version "$Id: datshift.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  55
C
C  DESCRIPTION:
C	interactively month, day, year;
C	get julian date YYYYDD back.
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Prototype  8/95 by CJC
C       Version   11/2001 by CJC for I/O API Version 2.1
C       Version   07/2007 by CJ:  Fortran-90 only;
C       USE M3UTILIO.  Support for YESTERDAY, TODAY, TOMORROW.
C       Integer-overflow avoidance for large NDAYS values.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: IARGC

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         GDATE   !  scratch gregorian date
        INTEGER         EGDATE  !  output gregorian date
        INTEGER         JDATE   !  internal julian date
        INTEGER         JTIME   !  internal julian time
        INTEGER         SGDATE  !  input gregorian date
        INTEGER         TSTEP, NDAYS   !  time step for changing dates
        INTEGER         YR, MON, DAY
        CHARACTER*80    DATBUF
        CHARACTER*80    STEPBUF

C***********************************************************************
C   begin body of program DATSHIFT

        CALL GETDTTIME( JDATE, JTIME )

        ARGCNT = IARGC()
        IF ( ARGCNT .EQ. 2 ) THEN

            CALL GETARG( 1, DATBUF )
            CALL UPCASE( DATBUF )

            IF ( DATBUF .EQ. '--HELP' )  THEN
                ARGCNT = 0
            ELSE IF ( DATBUF .EQ. 'TODAY' ) THEN
                CALL DAYMON( JDATE, MON, DAY )
                YR     = JDATE / 1000
                SGDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            ELSE IF ( DATBUF .EQ. 'YESTERDAY' ) THEN
                CALL NEXTIME( JDATE, JTIME, -240000 )
                CALL DAYMON( JDATE, MON, DAY )
                YR     = JDATE / 1000
                SGDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            ELSE IF ( DATBUF .EQ. 'TOMORROW' ) THEN
                CALL NEXTIME( JDATE, JTIME, 240000 )
                CALL DAYMON( JDATE, MON, DAY )
                YR     = JDATE / 1000
                SGDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            ELSE
                SGDATE = STR2INT( DATBUF )
            END IF

            CALL GETARG( 2, STEPBUF )
            NDAYS = STR2INT( STEPBUF )

        END IF

        IF ( ARGCNT .NE.        2 .OR.
     &       SGDATE .GT. 99999999 .OR.
     &       SGDATE .LT.        1      ) THEN

            WRITE( *,92000 ) ' ', ' ',
     & 'Program DATSHIFT takes calendar date (in form YYYYMMDD)',
     & 'and a number of days as an increment and returns the date',
     & 'in Gregorian-date form "YYYYMMDD".',
     & ' ',
     & 'Usage:  "datshift [<gregdate> <+/- days>]" ',
     &'    Options for gregdate:  ',
     &'         YYYYMMDD, e.g., 20070723',
     &'         YESTERDAY',
     &'         TODAY',
     &'         TOMORROW',
     &' ',
     & 'If the command-line arguments are missing, program prompts',
     & 'the user for them.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: datshift.f 44 2014-09-12 18:03:16Z coats               $',
     &' '

            CALL DAYMON( JDATE, MON, DAY )
            YR     = JDATE / 1000
            GDATE  = YR * 10000 + MON*100 + DAY
            SGDATE = GETDATE( GDATE,
     &                       'Enter date (YYYYMMDD) or (YYYYDDD)' )

            NDAYS  = GETNUM( -9000, 9000, 1,
     &                       'Enter days increment' )

            WRITE( DATBUF, 92010 ) SGDATE

        END IF      !  if argcnt=2, or not

C.........  Convert date formats

        IF ( SGDATE .GT. 9999366 ) THEN
            YR  = STR2INT( DATBUF( 1:4 ) )
            MON = STR2INT( DATBUF( 5:6 ) )
            DAY = STR2INT( DATBUF( 7:8 ) )

            JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            JTIME = 120000

        ELSE
            JDATE = SGDATE
            JTIME = 120000

        END IF

        DO WHILE ( NDAYS .GT. 365 )
            NDAYS = NDAYS - 365
            CALL NEXTIME( JDATE, JTIME, 365 * 240000 )
        END DO

        DO WHILE ( NDAYS .LT. -365 )
            NDAYS = NDAYS + 365
            CALL NEXTIME( JDATE, JTIME, -365 * 240000 )
        END DO

        TSTEP = NDAYS * 240000

        CALL NEXTIME( JDATE, JTIME, TSTEP )

        YR = JDATE / 1000

        CALL DAYMON( JDATE, MON, DAY )

        EGDATE = YR * 10000 + MON*100 + DAY

        WRITE( *,92010 ) EGDATE

      CALL EXIT( EGDATE )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT( 5X, A )

92010	FORMAT( I8.8 )


        END PROGRAM DATSHIFT

