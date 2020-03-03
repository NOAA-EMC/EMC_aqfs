
        SUBROUTINE  UNGRIDB( NCOLS, NROWS, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NU, CU )

C***********************************************************************
C Version "$Id: ungridb.f 45 2014-09-12 20:05:29Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems, and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  65
C
C  FUNCTION:
C 	computes "ungridding" matrices to be used by BMATVEC() and BILIN(),
C	for program LAYPOINT, etc., to perform bilinear interpolation
C	from a grid to a set of locations { <XLOC(S),YLOC(S)>, S=1:NPTS }
C
C  SEE ALSO:
C       BILIN()   which performs combined interpolate-only,
C                 preserving the subscript-order.
C       BMATVEC() which performs combined interpolate-and-transpose,
C                 e.g., for SMOKE program LAYPOINT, changing LAYER
C                 from an outermost subscript to an innermost
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 12/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Version   9/2014 by CJC:  modifications for OpenMP parallel
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NU( 4,NPTS )    !  single-indexed subscripts into grid
        REAL   , INTENT(  OUT) :: CU( 4,NPTS )    !  coefficients

C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		S		!  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        INTEGER		K		!  subscript  into singly-indexed grid
        REAL		DDX, DDY	!  inverse cell size
        REAL		XD0, YD0	!  center of LL cell
        REAL		X, Y		!  grid-normal coords of point
        REAL		P, Q 		!  linear-interpolation coeffs


C***********************************************************************
C   begin body of subroutine  UNGRIDB

        DDX = 1.0 / XCELL
        DDY = 1.0 / YCELL
        
        XD0 = XORIG + 0.5D0 * XCELL
        YD0 = YORIG + 0.5D0 * YCELL

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XD0, YD0, NPTS, XLOC, YLOC, NU, CU, 
!$OMP&             NCOLS, NROWS ),
!$OMP&    PRIVATE( S, X, Y, C, R, K, P, Q )

        DO  11  S = 1, NPTS
            
            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XD0 )	!  normalized grid coords
            IF ( X .GE. 0.0 ) THEN
                C = 1 + INT( X )                ! truncated to integer
                X = MOD( X, 1.0 )               ! trapped between 0 and 1
            ELSE
                C = -1 - INT( -X )              ! truncated to integer
                X = 1.0 - MOD( -X, 1.0 )        ! trapped between 0 and 1
            END IF

            Y = DDY * ( YLOC( S ) - YD0 )	!  normalized grid coords
            IF ( Y .GE. 0.0 ) THEN
                R = 1 + INT( Y )                ! truncated to integer
                Y = MOD( Y, 1.0 )               ! trapped between 0 and 1
            ELSE
                R = -1 - INT( -Y )              ! truncated to integer
                Y = 1.0 - MOD( -Y, 1.0 )        ! trapped between 0 and 1
            END IF

            IF ( R .LT. 1 ) THEN                !  r below    grid

                IF ( C .LT. 1 ) THEN            !  c left of  grid

                    K = 1
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) = 1.0
                    CU( 2,S ) = 0.0
                    CU( 3,S ) = 0.0
                    CU( 4,S ) = 0.0

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    K = NCOLS
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) =  1.0
                    CU( 2,S ) =  0.0
                    CU( 3,S ) =  0.0
                    CU( 4,S ) =  0.0

                ELSE                              !  c "in" the grid

                    K = C
                    NU( 1,S ) = K
                    NU( 2,S ) = K + 1
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) = 1.0 - X
                    CU( 2,S ) = X
                    CU( 3,S ) = 0.0 
                    CU( 4,S ) = 0.0

                END IF

            ELSE IF ( R .GT. NROWS - 1 ) THEN     !  r above    grid

                IF ( C .LT. 1 ) THEN                !  c left of  grid

                    K = ( NROWS - 1 ) * NCOLS + 1
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) = 1.0
                    CU( 2,S ) = 0.0
                    CU( 3,S ) = 0.0
                    CU( 4,S ) = 0.0

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    K = NROWS * NCOLS
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) = 1.0
                    CU( 2,S ) = 0.0
                    CU( 3,S ) = 0.0
                    CU( 4,S ) = 0.0

                ELSE                                !  c "in" the grid

                    K = ( NROWS - 1 ) * NCOLS  +  C
                    NU( 1,S ) = K
                    NU( 2,S ) = K + 1
                    NU( 3,S ) = K
                    NU( 4,S ) = K
                    CU( 1,S ) = 1.0 - X
                    CU( 2,S ) = X
                    CU( 3,S ) = 0.0
                    CU( 4,S ) = 0.0

                END IF

            ELSE                                  !  r "in" the grid

                IF ( C .LT. 1 ) THEN              !  c left of  grid

                    K = ( R - 1 ) * NCOLS + 1
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K + NCOLS
                    NU( 4,S ) = K + NCOLS
                    CU( 1,S ) =  1.0 - Y
                    CU( 2,S ) =  0.0
                    CU( 3,S ) =  Y
                    CU( 4,S ) =  0.0

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    K = R * NCOLS
                    NU( 1,S ) = K
                    NU( 2,S ) = K
                    NU( 3,S ) = K + NCOLS
                    NU( 4,S ) = K + NCOLS
                    CU( 1,S ) =  1.0 - Y
                    CU( 2,S ) =  0.0
                    CU( 3,S ) =  Y
                    CU( 4,S ) =  0.0

                ELSE                              !  c "in" the grid

                    K = ( R - 1 ) * NCOLS  +  C
                    NU( 1,S ) = K
                    NU( 2,S ) = K + 1
                    NU( 3,S ) = K + NCOLS
                    NU( 4,S ) = K + NCOLS + 1
                    P = 1.0 - X
                    Q = 1.0 - Y
                    CU( 1,S ) =  P * Q
                    CU( 2,S ) =  X * Q
                    CU( 3,S ) =  P * Y
                    CU( 4,S ) =  X * Y

                END IF

            END IF      !  end computing bilinear interpolation matrix

11      CONTINUE        !  end matrix computation loop on point sources

        RETURN
        END  SUBROUTINE  UNGRIDB

