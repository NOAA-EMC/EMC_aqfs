module NEXT__TIME
contains

      SUBROUTINE NEXT_TIME  ( JDATE , JTIME, DTIME )

!.....................................................................
! Version "@(#)$Header$"
! EDSS/Models-3 I/O API.
! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
! (C) 2003-2010 Baron Advanced Meteorological Systems
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................
!       subroutine body starts approx line 74
!
!  FUNCTION:
!
!       Subroutine to add  DTIME  to the current time, and then
!       update the clock  JTIME  and calendar  IDATE  accordingly.  
!       Output is fully normalized (0 <= JTIME <= 235959).
!
!   IDATE is stored in the form   YYYYDDD = YEAR*1000  +  DAY
!   JTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
!   DTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
!
!  CALLS:  none
!
!  REVISION HISTORY: 
!       prototype 10/1990 by Carlie J. Coats, Jr., MCNC Environmental Programs
!
!       Version    3/1993 by CJC for CRAY, etc.
!
!       Bugfix     1/2002 by CJC:  stepping _backwards_ across a year
!       transition
!
!       Unification 2/2002 by CJC with global-climate DAYMON, which
!       uses a 360-day "year"
!
!       Unification 2/2002 by CJC with global-climate DAYMON, which
!       uses a 360-day "year"
!
!       Version 5/2005 by CJC:  special case for time-independent data
!       (necessary for date:time=0000000:000000 normalization in CRTFIL3())
!
!       Version 1/2007 by CJC:  handle negative JDATEs correctly
!
!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
!       Modified 12/2014 by JPHUANG: bias correction (nc -->grib1) 
!........................................................................

        IMPLICIT  NONE

!.......   ARGUMENTS:

        INTEGER, INTENT(INOUT) :: JDATE           !  date (encoded YYYYDDD)
        INTEGER, INTENT(INOUT) :: JTIME           !  time (encoded  HHMMSS)
        INTEGER, INTENT(IN   ) :: DTIME           !  time increment (encoded HHMMSS)


!.......   LOCAL VARIABLES:  day and year components of date

        INTEGER         IBIAS, IDATE
        INTEGER         YEAR            !  year-component    of IDATE
        INTEGER         DAYS            !  day-component     of IDATE
        INTEGER         HOUR            !  hours-component   of JTIME
        INTEGER         MINS            !  minutes-component of JTIME
        INTEGER         SECS            !  seconds-component of JTIME
        INTEGER         SCR             !  scratch accumulator
        INTEGER         SIGN            !  sign of DTIME
        INTEGER         ATIME           !  absolute value of DTIME


!........................................................................
!       begin  NEXTIME
!.......   Construct equivalent problem with positive IDATE = JDATE + IBIAS

        IF ( JDATE .GE. 1000 ) THEN
            IDATE = JDATE
            IBIAS = 0
        ELSE    ! adjust date by multiple of 400-year leap-year cycle
            YEAR  = -JDATE
            YEAR  = YEAR / 1000 + 1
            YEAR  = 400 * ( YEAR / 400 + 1 )
            IBIAS = 1000 * YEAR
            IDATE = JDATE + IBIAS
        END IF

!.......   Increment seconds part of JTIME by DTIME (secs), and
!.......   re-normalize minute, hour, day part of IDATE:JTIME

        IF ( DTIME .GE. 0 ) THEN
            ATIME = DTIME
            SIGN  = 1
        ELSE
            ATIME = - DTIME
            SIGN  = - 1
        END IF

        SECS  =  MOD ( JTIME , 100 )  +  SIGN * MOD ( ATIME , 100 )
        IF ( SECS .GE. 0 ) THEN
            SCR = SECS / 60
        ELSE
            SCR = - ( 60 - SECS ) / 60
        END IF

        SECS = SECS  -  SCR * 60
        MINS = SCR  +  MOD ( JTIME / 100 , 100 )  &
                   +  SIGN * MOD ( ATIME / 100 , 100 )

        IF ( MINS .GE. 0 ) THEN
            SCR = MINS / 60
        ELSE
            SCR = - ( 60 - MINS ) / 60
        END IF

        MINS  =  MINS  -  SCR * 60
        HOUR  =  JTIME / 10000  +  SIGN * ( ATIME / 10000 )  +  SCR

        DAYS  =  MOD ( IDATE , 1000 )

        IF  ( HOUR .LT. -23 )  THEN
            SCR  = -HOUR / 24
            HOUR =  HOUR  +  SCR * 24
            DAYS =  DAYS  -  SCR
        END IF

        IF  ( HOUR .LT. 0 )  THEN

            SCR   =  ( 24 - HOUR ) / 24
            HOUR  =  HOUR  +  SCR * 24
            DAYS  =  DAYS  -  SCR

        ELSE IF  ( HOUR .GT. 23 )  THEN

            SCR   =  HOUR / 24
            HOUR  =  HOUR  -  SCR * 24
            DAYS  =  DAYS  +  SCR

        END IF

        JTIME  =  10000 * HOUR  +  100 * MINS  +  SECS

!...........   Update IDATE:
!...........   Note that each year must be treated individually

        YEAR  =  IDATE / 1000

100     CONTINUE        !  loop normalizing negative day numbers

            IF ( DAYS .LE. 0 ) THEN

!#ifdef IO_360
                DAYS  =  361   +  DAYS
                YEAR  =  YEAR  -  1
!#endif

!#ifndef IO_360

                IF (           ( MOD (YEAR,4)   .NE. 1 ) &        !  nonleap year
                   .OR. (      ( MOD (YEAR,100) .EQ. 1 ) &
                         .AND. ( MOD (YEAR,400) .NE. 1 ) ) ) THEN

                    DAYS  =  365   +  DAYS
                    YEAR  =  YEAR  -  1

                ELSE            !  leap-year case

                    DAYS  =  366   +  DAYS
                    YEAR  =  YEAR  -  1

                END IF

!#endif

                GO TO  100

            END IF

        print*,"9999,DAYS=",DAYS,"JTIME=",JTIME

        JDATE = DAYS

        RETURN 

200     CONTINUE        !  loop normalizing day numbers > 365,366

!#ifdef IO_360

           IF ( DAYS .GE. 361 ) THEN

                DAYS  =  DAYS  - 360
                YEAR  =  YEAR  +   1

              GO TO  200

            END IF      !  end DAYS > 365,366 date-normalization loop

!#endif

!#ifndef IO_360

            IF ( DAYS .GE. 366 ) THEN

                IF (           ( MOD (YEAR,4)   .NE. 0 ) &        !  nonleap year
                    .OR. (     ( MOD (YEAR,100) .EQ. 0 ) &
                         .AND. ( MOD (YEAR,400) .NE. 0 ) ) ) THEN

                    DAYS  =  DAYS   - 365
                    YEAR  =  YEAR  +   1

                    GO TO  200

                ELSE IF ( DAYS .GE. 367 ) THEN           !  leap year case

                    DAYS  =  DAYS   - 366
                    YEAR  =  YEAR  +   1

                    GO TO  200

                END IF

            END IF      !  end DAYS > 365,366 date-normalization loop

!#endif

        JDATE  =   1000 * YEAR  +  DAYS - IBIAS


        RETURN

END  SUBROUTINE NEXT_TIME

end module NEXT__TIME
