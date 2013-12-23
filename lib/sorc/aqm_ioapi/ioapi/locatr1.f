
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/locatr1.f,v 1.2 2000/11/28 21:22:55 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER FUNCTION LOCATR1( K1, N, LIST1 )

C***********************************************************************
C  function body starts at line 49
C
C  RETURNS:
C       subscript at which the targeted REAL key should be inserted
C       or -1 if the key is found.
C       EXAMPLE:  search for <FIP> in table of FIP values.
C
C  PRECONDITIONS REQUIRED:
C       Sorted table <N,LIST1> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 12/95 by MRH copied from FIND1 of CJC
C
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        REAL     K1             !  first  key
        INTEGER  N              !  table size
        REAL     LIST1( N )     !  table to search for K1


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C***********************************************************************
C   begin body of function  LOCATR1

        LO = 1
        HI = N

        IF( N .EQ. 0 ) THEN

            LOCATR1 = -1
            RETURN

        ENDIF 

11      CONTINUE
           
            IF ( LO .GT. HI ) THEN
            
                LOCATR1 = LO
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K1 .GT. LIST1( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K1 .LT. LIST1( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this <K1>
        
        
        LOCATR1 = -1         ! key found
        RETURN
        END

