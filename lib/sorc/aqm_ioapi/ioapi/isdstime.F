
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION ISDSTIME( JDATE )

C***********************************************************************
C  function body starts at line  57
C
C  RETURNS:  
C       TRUE iff Daylight Savings Time is in effect for JDATE,
C       calculated using Zeller's Congruence to find the first
C       Sunday in April and the last Sunday in October (the
C       starting and ending dates for Daylight Savings Time).
C
C  PRECONDITIONS REQUIRED:
C       JDATE represents a date YYYYDDD according to Models-3 conventions
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       None
C
C  REVISION  HISTORY:
C       Prototype  8/1995 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       
C       Unification 2/2002 by CJC with global-climate ISDST, which
C       uses a 360-day "year"
C
C       Bugfix     7/2002 by CJC
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         JDATE   !  Julian date, coded YYYYDDD


C...........   PARAMETERS and their descriptions:

        INTEGER         APR1
        INTEGER         OCT31

#ifdef IO_360
        PARAMETER     ( APR1  =  91 , OCT31 = 300 )
#endif
#ifndef IO_360
        PARAMETER     ( APR1  =  91 , OCT31 = 304 )
#endif


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         YEAR, IDAY
        INTEGER         DAY0, DAY1
        INTEGER         K
        

C***********************************************************************
C   begin body of function  ISDSTIME

        YEAR = JDATE / 1000
        IDAY = MOD ( JDATE , 1000 )

C.......   Uses Zeller's Congruence calculation of day of wk for APR1, OCT31:
C.......   wkday = 1 + mod( k + day, 7 )

        DAY0  = APR1
        DAY1  = OCT31

#ifndef IO_360
        IF (          ( MOD (YEAR,4)   .EQ. 0 )        !  leap year adjustment
     &     .AND. (    ( MOD (YEAR,100) .NE. 0 )        !  month >= FEB
     &           .OR. ( MOD (YEAR,400) .EQ. 0 ) ) ) THEN

            DAY0 = DAY0 + 1
            DAY1 = DAY1 + 1

        END IF
#endif

        K     = YEAR - 1
        K     = K * 365  +  K / 4  -  K / 100  +  K / 400  -  1
        DAY0  = DAY0 + 6 - MOD( K + DAY0, 7 )            !  first Sun. in Apr.
        K     = 1 + MOD( K + DAY1, 7 )        !  day-number 1...7 for OCT31
        DAY1  = DAY1     - MOD( K, 7 )        !  last  Sun. in Oct.

        ISDSTIME = ( ( IDAY .GE. DAY0 ) .AND. ( IDAY .LT. DAY1 ) )

        RETURN

        END

