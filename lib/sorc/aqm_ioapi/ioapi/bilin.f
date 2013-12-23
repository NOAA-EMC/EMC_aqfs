
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/bilin.f,v 1.2 2000/11/28 21:22:31 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  BILIN( M, N, P, IX, AX, V, C )

C***********************************************************************
C  subroutine body starts at line  74
C
C  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
C
C  NOTE:  Maintains I/O subscript order V( M,P )
C
C       For bilinear interpolation of gridded data having dimension NC,NR
C       to a list of locations having grid-normal coordinates <X(S),Y(S)>:
C       let C(S) and R(S) be INT( X(S) ) and INT( Y(S) ), P(S) and Q(S)
C       be AMOD( X(S), 1.0 ) and AMOD( Y(S), 1.0 ).  Then IX has the 
C	single-indexing subscripts into the grid for the cell=corners
C       surrounding the <X(S),Y(S)>
C           IX(1,S) = C + NC * ( R - 1 ) + 1
C           IX(2,S) = C + NC * ( R - 1 ) 
C           IX(3,S) = C + NC *   R
C           IX(4,S) = C + NC *   R       + 1
C       and AX has the bilinear-interpolation coefficients:
C           AX(1,S) = ( 1.0 - P( S ) )*( 1.0 - Q( S ) )
C           AX(2,S) =         P( S )  *( 1.0 - Q( S ) )
C           AX(3,S) = ( 1.0 - P( S ) )*        Q( S )       
C           AX(4,S) =         P( S )  *        Q( S )
C
C  SEE ALSO:
C       UNGRIDB() which produces such matrices
C       BMATVEC() which performs combined interpolate-and-transpose,
C                 e.g., for SMOKE program LAYPOINT
C
C  PRECONDITIONS REQUIRED:
C       Number of layers same for input and output.
C       Index and coefficients set up so that the equation for
C       the multiplication (at each row R and layer L) is
C
C       C(L,R) = SUM_{ J=1...4 }  A( J,R ) * V( I( J,R ),L )
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 8/99 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER         M               ! length of input  vector
        INTEGER         N               ! length of output vector
        INTEGER         P               ! number of layers
        INTEGER         IX( 4,N )       ! index array
        REAL            AX( 4,N )       ! 4-band coeff matrix
        REAL            V( M,P )        ! P-layered input  vector
        REAL            C( N,P )        ! P-layered output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         R, L, J1, J2, J3, J4


C***********************************************************************
C   begin body of subroutine  BILIN

        DO  L = 1, P
        DO  R = 1, N
            
            J1 = IX( 1,R )
            J2 = IX( 2,R )
            J3 = IX( 3,R )
            J4 = IX( 4,R )

            C( R,L ) = AX( 1,R ) * V( J1,L )  +
     &                 AX( 2,R ) * V( J2,L )  +
     &                 AX( 3,R ) * V( J3,L )  +
     &                 AX( 4,R ) * V( J4,L )
        
        END DO
        END DO

        RETURN
        END

