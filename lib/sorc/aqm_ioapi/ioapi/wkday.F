
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION WKDAY( JDATE )
 
C***********************************************************************
C  function body starts at line  57
C
C  RETURNS:
C	day (1=Monday, ..., 7=Sunday) of the week for the given Julian date
C
C  PRECONDITIONS REQUIRED:
C	JDATE integer of the form  1000 * YEAR +  DAY
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	adapted  2/27/1995  by CJC from ROM 2.2 UTILIO function KDAY.FOR
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C***********************************************************************
 
      IMPLICIT NONE
 
C...........   ARGUMENTS and their descriptions:

      INTEGER  JDATE	!  Julian date, YYYYDDD = 1000 * YEAR + DAY


C...........   PARAMETERS and their descriptions:

        INTEGER         YDAYS   !  "normal"-year # of days
#ifdef IO_360
        PARAMETER     ( YDAYS = 360 )
#endif
#ifndef IO_360
        PARAMETER     ( YDAYS = 365 )
#endif

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER  YEAR	!  year 
      INTEGER  JDAY	!  Julian day-number 1 ... 365,366
      INTEGER  K	!  subexpression

                                                                                
C***********************************************************************
C   begin body of function  WKDAY

      YEAR  = JDATE / 1000
      JDAY  = MOD( JDATE, 1000 )
      K     = YEAR - 1
      K     = K * YDAYS  +  K / 4  -  K / 100  +  K / 400  +  JDAY  -  1
      WKDAY = 1 + MOD (K, 7)
 
      RETURN
      END

