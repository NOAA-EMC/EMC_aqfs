
        SUBROUTINE  PMATVEC( NCOLS, NROWS, NCOFF, N, I, U, V )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  50
C
C  FUNCTION:  multiply a sparse matrix <N,I> by a vector U and
C             return the result V
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 2/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
        INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
        INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: N( NROWS )      ! # of entries per row
        INTEGER, INTENT(IN   ) :: I( NCOFF )      ! columns list

        REAL, INTENT(IN   ) :: U( NCOLS )      !  input vector
        REAL, INTENT(  OUT) :: V( NROWS )      ! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, K
        REAL            SUM


C***********************************************************************
C   begin body of subroutine  PMATVEC

        K = 0
        DO  22  R = 1, NROWS

            SUM = 0.0

            DO  11  C = 1, N( R )
                K = K + 1
                SUM = SUM  +  U( I( K ) )
11          CONTINUE

            V( R ) = SUM

22      CONTINUE

        RETURN

        END  SUBROUTINE  PMATVEC

