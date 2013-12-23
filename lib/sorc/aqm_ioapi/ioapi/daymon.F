
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      SUBROUTINE DAYMON( JDATE, MNTH, MDAY )
 
C***********************************************************************
C  function body starts at line  44
C
C  FUNCTION:
C
C    This routine determines the month and day of the month 
C    for the Julian date YYYYDDD that is input
C
C  REVISION HISTORY:
C
C       3/1995   Adapted for Models-3/EDSS from ROM GREG.FOR by CJC
C
C       2/2002 Unification by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C***********************************************************************
 

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER	JDATE	!  Julian date, format YYYYDDD = 1000*Year + Day
        INTEGER MNTH    !  month (1...12)
        INTEGER MDAY    !  day-of-month (1...28,29,30,31)


C...........   SCRATCH LOCAL VARIABLES:
        
        INTEGER YEAR, DAY, L, J


C***********************************************************************
C   begin body of subroutine  DAYMON
      
#ifdef IO_360
      DAY  = MOD( JDATE, 1000 ) - 1
      MNTH =      DAY / 30 + 1
      MDAY = MOD( DAY , 30 ) + 1
#endif

#ifndef IO_360
      YEAR = JDATE / 1000
      DAY  = MOD( JDATE, 1000 )
      IF      ( MOD( YEAR, 400 ) .EQ. 0 ) THEN
          L = 366
      ELSE IF ( MOD( YEAR, 100 ) .EQ. 0 ) THEN
          L = 365
      ELSE IF ( MOD( YEAR, 4 )   .EQ. 0 ) THEN
          L = 366
      ELSE 
          L = 365
      END IF
 
      J = MOD( DAY + 305, L )
      J = MOD( J, 153 ) / 61 + ( J / 153 ) * 2 + J
 
      MNTH = MOD( J / 31 + 2, 12 ) + 1
      MDAY = MOD( J, 31 ) + 1
#endif
 
      RETURN
      END

