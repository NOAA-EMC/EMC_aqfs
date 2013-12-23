
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/dble2real.f,v 1.2 2000/11/28 21:22:38 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  DBLE2REAL( SIZE, DBLG, GRID )

C***********************************************************************
C  subroutine body starts at line  44
C
C  FUNCTION:
C	convert INTEGER input array DBLG( SIZE ) to REAL
C
C  PRECONDITIONS REQUIRED:
C	none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	none
C
C  REVISION  HISTORY:
C	prototype 6/95 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER		 SIZE		!  array dimension
        DOUBLE PRECISION DBLG( SIZE )	!  input double array
        REAL		 GRID( SIZE )	! output real   array

C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		I


C***********************************************************************
C   begin body of subroutine  INT2REAL

        DO  11  I = 1, SIZE
            GRID( I ) = REAL( DBLG( I ) )
11      CONTINUE

        RETURN
        END

