
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/trimlen.f,v 1.2 2000/11/28 21:23:07 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION  TRIMLEN ( STRING )

C***********************************************************************
C  function body starts at line 43
C
C  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
C             after trailing blanks have been trimmed.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  
C             Prototype 8/91 by CJC
C             Version 2/93 for CRAY by CJC
C
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)	STRING


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER  	L, K


C***********************************************************************
C   begin body of function  TRIMLEN

        L = LEN( STRING )
        DO  11  K = L, 1, -1
            IF ( STRING( K:K ) .NE. ' ' ) THEN
                GO TO  12
            END IF
11      CONTINUE

        K = 1

12      CONTINUE

        TRIMLEN = K

        RETURN

        END

