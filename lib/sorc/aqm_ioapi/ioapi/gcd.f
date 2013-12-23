
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/gcd.f,v 1.2 2000/11/28 21:22:45 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION GCD ( P , Q )

C***********************************************************************
C  function body starts at line 37
C
C  FUNCTION:  computes Greatest Common Divisors of integers P,Q
C
C  PRECONDITIONS REQUIRED:  none
C
C  REVISION  HISTORY:  prototype 3/91 by CJC
C
C***********************************************************************

        IMPLICIT NONE

C...........   Arguments:

        INTEGER 	P , Q


C.......   Local Variables:

        INTEGER 	X, Y


C***********************************************************************
C   begin body of program  GCD2

        X = ABS ( P )
        Y = ABS ( Q )

111     CONTINUE

            IF ( X .GT. Y ) THEN
                X = MOD ( X , Y )
                IF ( X .NE. 0 ) GO TO 111
            ELSE IF ( X .LT. Y ) THEN
                Y = MOD ( Y , X )
                IF ( Y .NE. 0 ) GO TO 111
            END IF

        GCD = MAX ( X , Y )

        RETURN

        END

