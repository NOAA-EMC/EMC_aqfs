
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/lblank.f,v 1.2 2000/11/28 21:22:52 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION LBLANK( STRING )

C***********************************************************************
C    function body starts at line 37
C
C  FUNCTION:
C
C    Returns the number of leading blanks in STRING
C
C  REVISION HISTORY:
C
C    9/95   Adapted from ROM utility routine LEN2() by CJC
C
C***********************************************************************

      IMPLICIT NONE
            
C.......   Arguments and their descriptions:

      CHARACTER*(*) STRING	!  Character string to search

C.......   PARAMETERs:

      CHARACTER*1       BLANK
      CHARACTER*1       TAB
      
      PARAMETER ( BLANK = ' ' )
      

C.......   Local variable:  loop counter

      INTEGER       I, L

C........................................................................
C.......   begin body:  Scan from left to right until non blank character

      L = LEN( STRING )
      DO  100  I = 1 , L

          IF ( ( STRING( I:I ) .NE. BLANK ) .AND.
     &         ( STRING( I:I ) .NE. CHAR( 9 ) ) ) THEN
              LBLANK = I - 1
              RETURN
          END IF

100   CONTINUE

      LBLANK = L
      RETURN

      END

