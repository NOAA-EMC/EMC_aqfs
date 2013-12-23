
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL   FUNCTION CURRSTEP ( JDATE, JTIME, 
     &                                SDATE, STIME, TSTEP, 
     &                                CDATE, CTIME )

C***********************************************************************
C  subroutine body starts at line  56
C
C  FUNCTION:  Compute the date&time CDATE:CTIME for the time step in 
C             the time step sequence starting at SDATE:STIME and having
C             time step TSTEP.  In particular, it is the largest time
C             step in the sequence having the property:
C
C                 CDATE:CTIME <= JDATE:JTIME
C
C  PRECONDITIONS REQUIRED:  Dates represented YYYYDDD, 
C                           times represented HHMMSS.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  NEXTIME, SEC2TIME, SECSDIFF, TIME2SEC
C
C  REVISION  HISTORY:
C       prototype 5/92 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER       SDATE, STIME    !  starting d&t for the sequence
        INTEGER       TSTEP           !  time step for the sequence
        INTEGER       JDATE, JTIME    !  d&t requested
        INTEGER       CDATE, CTIME    !  d&t for timestep of JDATE:JTIME

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER       SECSDIFF, SEC2TIME, TIME2SEC
        EXTERNAL      SECSDIFF, SEC2TIME, TIME2SEC


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       SECS, STEP


C***********************************************************************
C   begin body of subroutine  CURRSTEP

        IF ( TSTEP .EQ. 0 ) THEN   !  time-independent case:

            CURRSTEP = .TRUE.
            CDATE    = SDATE
            CTIME    = STIME

        ELSE IF ( JDATE .LT.        0  .OR. ! out-of-range probable-error case
     &            JDATE .GT. 10000000  .OR.      !  10e4 years
     &            JTIME .LT.  -240000  .OR.
     &            JTIME .GT.   240000  ) THEN

                CURRSTEP = .FALSE.

        ELSE			   !  time-dependent case:

            SECS = SECSDIFF( SDATE, STIME, JDATE, JTIME )

            IF ( SECS .LT. 0 ) THEN     !  before start of time step sequence

                CURRSTEP = .FALSE.

            ELSE                        !  usual case:  integer arithmetic

                CURRSTEP = .TRUE.       !  to determine time-step number,
                CDATE    = SDATE        !  sec2time to find offset from start,
                CTIME    = STIME        !  nextime() to compute date&time.
                STEP     = TIME2SEC( ABS( TSTEP ) ) 
                CALL NEXTIME( CDATE, CTIME, 
     &                        SEC2TIME( ( SECS / STEP ) * STEP ) )

            END IF

        END  IF

        RETURN

        END

