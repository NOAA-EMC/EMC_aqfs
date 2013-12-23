
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/findr1.f,v 1.2 2000/11/28 21:22:43 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER FUNCTION FINDR1( K, N, LIST )

C***********************************************************************
C  function body starts at line 49
C
C  RETURNS:
C       subscript at which the targeted key appears, or 
C       -1 in case of failure.
C       EXAMPLE:  search for FIP in table of FIP values
C
C  PRECONDITIONS REQUIRED:
C       Sorted table <N,LIST> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/1/95 by CJC
C
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        REAL     K             !  first  key
        INTEGER  N             !  table size
        REAL     LIST( N )     !  table to search for K


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C***********************************************************************
C   begin body of function  FINDR1

        LO = 1
        HI = N
        
11      CONTINUE
            
            IF ( LO .GT. HI ) THEN
            
                FINDR1 = -1
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K .GT. LIST( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K .LT. LIST( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this K
        
        
        FINDR1 = M
        RETURN
        END

