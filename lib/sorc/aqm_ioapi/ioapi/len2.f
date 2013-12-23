
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/len2.f,v 1.2 2000/11/28 21:22:53 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION LEN2 (J1, J2, STRING)

C***********************************************************************
C    function body starts at line 42
C
C  FUNCTION:
C
C    Returns the number of leading blanks in STRING( J1:J2 )
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C    8/90   Modified for ROM 2.2:  simpler algorithm uses DO loop.
C    2/93   Modified for CRAY by CJC.
C    9/94   Simpler algorithm for Models-3 by CJC
C
C***********************************************************************

      IMPLICIT NONE
            
C.......   Arguments and their descriptions:

      INTEGER       J1		!  First position in string to be searched
      INTEGER       J2		!  Last     "
      CHARACTER*(*) STRING	!  Character string to search

C.......   Local variable:  loop counter

      INTEGER       I

C........................................................................
C.......   begin body:  Scan from left to right until non blank character

      DO  100  I = J1 , J2

          IF ( STRING ( I:I ) .NE. ' ' )  THEN
              LEN2 = I - J1
              RETURN
          END IF

100   CONTINUE

      LEN2 = J2 - J1 + 1
      RETURN

      END

