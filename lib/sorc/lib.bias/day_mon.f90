module DAY__MON 
contains


      SUBROUTINE DAY_MON( JDATE, MNTH, MDAY )
 
!***********************************************************************
! Version "@(#)$Header$"
! EDSS/Models-3 I/O API.
! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
! (C) 2003-2010 Baron Advanced Meteorological Systems
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................
!  function body starts at line  49
!
!  FUNCTION:
!
!    This routine determines the month and day of the month 
!    for the Julian date YYYYDDD that is input
!
!  REVISION HISTORY:
!
!       3/1995   Adapted for Models-3/EDSS from ROM GREG.FOR by CJC
!
!       2/2002 Unification by CJC with global-climate DAYMON, which
!       uses a 360-day "year"
!
!       Version 1/2007 by CJC:  handle negative JDATEs correctly
!
!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
!***********************************************************************
 

      IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: JDATE	!  Julian date, format YYYYDDD = 1000*Year + Day
        INTEGER, INTENT(  OUT) :: MNTH    !  month (1...12)
        INTEGER, INTENT(  OUT) :: MDAY    !  day-of-month (1...28,29,30,31)


!...........   SCRATCH LOCAL VARIABLES:
        
        INTEGER IBIAS, IDATE, YEAR, DAY, L, J


!***********************************************************************
!  begin body of subroutine  DAYMON
      
        IF ( JDATE .GE. 1000 ) THEN
            IDATE = JDATE
            IBIAS = 0
        ELSE
            YEAR  = -JDATE
            YEAR  = YEAR / 1000 + 1
            IBIAS = 2800000 * YEAR
            IDATE = JDATE + IBIAS
        END IF

!#ifdef IO_360
!      DAY  = MOD( IDATE, 1000 ) - 1
!      MNTH =      DAY / 30 + 1
!      MDAY = MOD( DAY , 30 ) + 1
!#endif

!#ifndef IO_360
      YEAR = JDATE / 1000
      DAY  = MOD( IDATE, 1000 )
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
!#endif
 
      RETURN
END SUBROUTINE DAY_MON

end module DAY__MON

