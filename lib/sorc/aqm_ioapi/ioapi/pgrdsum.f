
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/pgrdsum.f,v 1.2 2000/11/28 21:23:00 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
        SUBROUTINE  PGRDSUM( NCOLS, NROWS, NCOFF, N, I, PTR, U, V )

C***********************************************************************
C  subroutine body starts at line  49
C
C  FUNCTION:  multiply a sparse matrix <N,I> by a vector U, sum and 
C             then return the result V
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 4/97 by JMV
C
C****************************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER         NCOLS           ! length of input vector
        INTEGER         NROWS           ! length of output vector
        INTEGER         NCOFF           ! max number of coefficients
        
        INTEGER         N( NROWS )      ! # of entries per row
        INTEGER         I( NCOFF )      ! columns list

        INTEGER         PTR ( NCOLS )   !  summation pointer
        REAL            U( NCOLS )      !  input vector
        REAL            V( NROWS )      ! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         R, C, K, L, P   


C***********************************************************************
C   begin body of subroutine  PSUMGRD

        K = 0

        DO  22  R = 1, NROWS
            
            DO  11  C = 1, N( R )
                K = K + 1
                L = I( K ) 
                P = PTR( L )
                V( P ) = V( P ) + U( L )
11          CONTINUE
            
22      CONTINUE

      RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in subroutine PSUMGRD ***',
     &            /5X , A ,
     &           // )        !  generic error message format


C...........   Informational (LOG) message formats... 92xxx


C...........   Formatted file I/O formats............ 93xxx


C...........   Internal buffering formats............ 94xxx


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

