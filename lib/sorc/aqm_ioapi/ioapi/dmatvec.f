
        SUBROUTINE DMATVEC( N, A, V, C )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  44
C
C  FUNCTION:  apply a diagonal matrix to a vector
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	prototype 2/1995 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: N		! length of input vector
        REAL   , INTENT(IN   ) :: A( N )		! diagonal coeff matrix
        REAL   , INTENT(IN   ) :: V( N )		! input  vector
        REAL   , INTENT(  OUT) :: C( N )		! output vector

C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		R


C***********************************************************************
C   begin body of subroutine  DMATVEC

        DO  22  R = 1, N
            
            C( R ) = A( R ) * V( R )
        
22      CONTINUE

        RETURN
        END SUBROUTINE DMATVEC

