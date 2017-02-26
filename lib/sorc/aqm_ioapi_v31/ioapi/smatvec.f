
        SUBROUTINE SMATVEC( NCOLS, NROWS, NCOFF, N, I, M, U, V )

C***********************************************************************
C Version "$Id: smatvec.f 45 2014-09-12 20:05:29Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  53
C
C  FUNCTION:  multiply a sparse matrix <N,I,C> by a vector U and
C             return the result V
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 2/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Version   9/2014 by CJC:  modifications for OpenMP parallel
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
        INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
        INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: N( NROWS )      ! # of entries per row
        INTEGER, INTENT(IN   ) :: I( NCOFF )      ! columns list
        REAL   , INTENT(IN   ) :: M( NCOFF )      ! coefficient array

        REAL   , INTENT(IN   ) :: U( NCOLS )      !  input vector
        REAL   , INTENT(  OUT) :: V( NROWS )      ! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, K
        REAL*8          SUM
        INTEGER         CNT( 0:NROWS )


C***********************************************************************
C   begin body of subroutine  SMATVEC

        CNT( 0 ) = 0
        DO  R = 1, NROWS
            CNT( R ) = CNT( R-1 ) + N( R )
        END DO

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NROWS, CNT, M, U, V, I ),
!$OMP&    PRIVATE( R, SUM )

        DO  R = 1, NROWS

            SUM = 0.0d0

            DO  K = CNT( R-1 )+1, CNT( R )
                SUM = SUM  +  M( K ) * U( I( K ) )
            END DO

            V( R ) = SUM

        END DO

        RETURN

        END SUBROUTINE SMATVEC

