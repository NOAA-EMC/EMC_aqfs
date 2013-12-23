
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER FUNCTION JSTEP3( JDATE, JTIME, SDATE, STIME, TSTEP )

C***********************************************************************
C  function body starts at line  74
C
C  FUNCTION:  returns the timestep-record number corresponding to JDATE:JTIME
C             for the timestep sequence starting at SDATE:STIME, with time
C             step increment TSTEP
C
C  RETURN VALUE:
C             time step record number, if successful, or  -1  if the
C             indicated  JDATE:JTIME  is not a positive multiple of
C             TSTEP from SDATE:STIME
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         JDATE   !  requested date YYYYDDD
        INTEGER         JTIME   !  requested time HHMMSS
        INTEGER         SDATE   !  starting date  YYYYDDD
        INTEGER         STIME   !  starting time  HHMMSS
        INTEGER         TSTEP   !  time step      H*MMSS


C...........   PARAMETERS and their descriptions:

        INTEGER         YDAYS   !  "normal"-year # of days
        INTEGER         LDAYS   !      leap-year # of days

#ifdef IO_360
        PARAMETER     ( YDAYS = 360, LDAYS = 360 )
#endif
#ifndef IO_360
        PARAMETER     ( YDAYS = 365, LDAYS = 366 )
#endif


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DAYS
        INTEGER         HRS
        INTEGER         MINS
        INTEGER         SECS
        INTEGER         DTIME   !  accumulated time interval( secs )
        INTEGER         DSTEP   !  time step, in secs
        INTEGER         JYEAR
        INTEGER         SYEAR


C***********************************************************************
C   begin body of function  JSTEP3

        IF ( TSTEP .LT. 0 ) THEN
            JSTEP3 = -1
        ELSE IF ( TSTEP .EQ. 0 ) THEN
            JSTEP3 = 1
        ELSE
            JYEAR = JDATE/1000
            SYEAR = SDATE/1000
            IF ( JYEAR .LT. SYEAR ) THEN
                JSTEP3 = -1
                RETURN
            END IF

            DTIME = 0   !  accumulated time interval

111         CONTINUE    !  loop adjusting DTIME for JYEAR:SYEAR difference

                IF ( JYEAR .GT. SYEAR ) THEN
                    IF ( MOD( SYEAR, 4 ) .NE. 0 ) THEN
                        SYEAR = SYEAR + 1
                        DTIME = DTIME + YDAYS * 86400
                    ELSE IF ( MOD( SYEAR, 100 ) .NE. 0 ) THEN
                        SYEAR = SYEAR + 1
                        DTIME = DTIME + LDAYS * 86400
                    ELSE IF ( MOD( SYEAR, 400 ) .NE. 0 ) THEN
                        SYEAR = SYEAR + 1
                        DTIME = DTIME + YDAYS * 86400
                    ELSE
                        SYEAR = SYEAR + 1
                        DTIME = DTIME + LDAYS * 86400
                    END IF

                    GO TO  111

                END IF          !  end loop adjusting years

            SECS  = MOD( JTIME,     100 ) - MOD( STIME,     100 )
            MINS  = MOD( JTIME/100, 100 ) - MOD( STIME/100, 100 )
            HRS   =      JTIME/10000      -      STIME/10000
            DAYS  = MOD( JDATE,    1000 ) - MOD( SDATE,    1000 )
            DTIME = DTIME +
     &              SECS + 60 * ( MINS + 60 * ( HRS + 24 * DAYS ) )

            SECS  = MOD( TSTEP,     100 )
            MINS  = MOD( TSTEP/100, 100 )
            HRS   =      TSTEP/10000
            DSTEP = SECS + 60 * ( MINS + 60 * HRS )

            IF ( DTIME .LT. 0 ) THEN
                JSTEP3 = -1
            ELSE IF ( MOD( DTIME, DSTEP ) .NE. 0 ) THEN
                JSTEP3 = -1
            ELSE
                JSTEP3 = 1  +  DTIME / DSTEP
            END IF

        END IF          !  if tstep<0, =0, or >0

        RETURN

        END

