
C***********************************************************************
C Version "$Id: fills.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine IFILL starts at line  41
C  subroutine RFILL starts at line 102
C  subroutine DFILL starts at line 162
C
C  FUNCTION:
C       fill GRID( NCOLS, NROWS, NLAYS ) with value-pattern determined
C       by OP:
C               1 -- column number
C               2 -- row number
C               3 -- layer number
C               otherwise:  value of VAL
C
C       where GRID has type
C               INTEGER for IFILL
C               REAL    for RFILL
C               DOUBLE  for DFILL
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION  HISTORY:
C       prototype 8/95 by CJC
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 11/2013 by CJC:  OpenMP parallel.  Reorganize loops
C       with R outermost to allow parallelism whether or not NLAYS=1
C***********************************************************************

C...........................  begin IFILL()  .....................

        SUBROUTINE  IFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        INTEGER, INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
        INTEGER, INTENT(IN   ) :: OP
        REAL   , INTENT(IN   ) :: VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L, V


C***********************************************************************
C   begin body of subroutine  IFILL

        IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = C
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = R
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = L
            END DO
            END DO
            END DO
        ELSE
            V = INT( VAL )
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, V ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = V
            END DO
            END DO
            END DO
        END IF

        RETURN
        END


C...........................  begin RFILL()  .....................

        SUBROUTINE  RFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        REAL   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
        INTEGER, INTENT(IN   ) :: OP
        REAL   , INTENT(IN   ) :: VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L


C***********************************************************************
C   begin body of subroutine  RFILL

        IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( C )
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( R )
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( L )
            END DO
            END DO
            END DO
        ELSE
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, VAL ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = VAL
            END DO
            END DO
            END DO
        END IF

        RETURN
        END


C...........................  begin DFILL()  .....................

        SUBROUTINE  DFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        DOUBLE PRECISION, INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
        INTEGER         , INTENT(IN   ) :: OP
        REAL            , INTENT(IN   ) :: VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER          R, C, L
        DOUBLE PRECISION V


C***********************************************************************
C   begin body of subroutine  DFILL

        IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( C )
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( R )
            END DO
            END DO
            END DO
        ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( L )
            END DO
            END DO
            END DO
        ELSE
            V = DBLE( VAL )
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, V ),
!$OMP&                  PRIVATE( I )
            DO  R = 1, NROWS
            DO  L = 1, NLAYS
            DO  C = 1, NCOLS
                GRID( C,R, L ) = V
            END DO
            END DO
            END DO
        END IF

        RETURN
        END


