
        SUBROUTINE  BMATVEC( M, N, P, IX, AX, V, C )

C***********************************************************************
C Version "$Id: bmatvec.f 45 2014-09-12 20:05:29Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 Baron Advanced Meteorological Systems, and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  76
C
C  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
C	      as used for bilinear interpolation of meteorology in LAYPOINT
C
C  NOTE:  Transposes from I/O subscript order V( M,P )
C         to computational subscript order    C( P,N )
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
C       BILIN()   which performs combined interpolate-only,
C                 preserving the subscript-order.
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
C       prototype 12/1995 by CJC
C       Version    9/2014 by CJC:  modifications for OpenMP parallel
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: C( P,N )        ! P-layered output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         S, L, J1, J2, J3, J4


C***********************************************************************
C   begin body of subroutine  BMATVEC

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, P, IX, AX, V, C ),
!$OMP&    PRIVATE( S, L, J1, J2, J3, J4 )

        DO  S = 1, N
            
            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )

            DO  L = 1, P
                C( L,S ) = AX( 1,S ) * V( J1,L )  +
     &                     AX( 2,S ) * V( J2,L )  +
     &                     AX( 3,S ) * V( J3,L )  +
     &                     AX( 4,S ) * V( J4,L )
            END DO

        END DO

        RETURN
        END SUBROUTINE BMATVEC

