
        SUBROUTINE  UNGRIDI( NCOLS, NROWS, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX )


C***********************************************************************
C Version "$Id: ungridi.f 45 2014-09-12 20:05:29Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  63
C
C  FUNCTION:
C 	computes "ungridding" incidence matrices to be used for program
C       METCPLE, etc., to incidence-matrix (conservative precip) re-gridding
C	from a grid to a set of locations { <XLOC(S),YLOC(S)>, S=1:NPTS }
C
C  SEE ALSO:
C       UNGRIDB() which computes matrices for bilinear interpolation
C       BILIN()   which performs combined interpolate-only,
C                 preserving the subscript-order.
C       BMATVEC() which performs combined interpolate-and-transpose,
C                 e.g., for SMOKE program LAYPOINT, changing LAYER
C                 from an outermost subscript to an innermost
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION  HISTORY:
C	prototype 10/2005 by CJC
C       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid

C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		S		!  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        REAL		DDX, DDY	!  inverse cell size
        REAL		X, Y		!  grid-normal coords of point


C***********************************************************************
C   begin body of subroutine  UNGRIDB

        DDX = 1.0 / XCELL
        DDY = 1.0 / YCELL
        
        DO  11  S = 1, NPTS
            
            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XORIG )	!  normalized grid coords
            IF ( X .GE. 0.0 ) THEN
                C = 1 + INT( X )                ! truncated to integer
            ELSE
                C = -1 - INT( -X )              ! truncated to integer
            END IF

            Y = DDY * ( YLOC( S ) - YORIG )	!  normalized grid coords
            IF ( Y .GE. 0.0 ) THEN
                R = 1 + INT( Y )                ! truncated to integer
            ELSE
                R = -1 - INT( -Y )              ! truncated to integer
            END IF

            IF ( R .LT. 1 ) THEN                !  r below    grid

                IF ( C .LT. 1 ) THEN            !  c left of  grid

                    NX( S ) = 1

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    NX( S ) = NCOLS

                ELSE                              !  c "in" the grid

                    NX( S ) = C

                END IF

            ELSE IF ( R .GT. NROWS - 1 ) THEN     !  r above    grid

                IF ( C .LT. 1 ) THEN                !  c left of  grid

                    NX( S ) = ( NROWS - 1 ) * NCOLS + 1

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    NX( S ) = NROWS * NCOLS

                ELSE                                !  c "in" the grid

                    NX( S ) = ( NROWS - 1 ) * NCOLS  +  C

                END IF

            ELSE                                  !  r "in" the grid

                IF ( C .LT. 1 ) THEN              !  c left of  grid

                    NX( S ) = ( R - 1 ) * NCOLS + 1

                ELSE IF ( C .GT. NCOLS - 1 ) THEN !  c right of grid

                    NX( S ) = R * NCOLS

                ELSE                              !  c "in" the grid

                    NX( S ) = ( R - 1 ) * NCOLS  +  C

                END IF

            END IF      !  end computing incidence indexes

11      CONTINUE        !  end matrix computation loop on target locations

        RETURN
        END SUBROUTINE  UNGRIDI

