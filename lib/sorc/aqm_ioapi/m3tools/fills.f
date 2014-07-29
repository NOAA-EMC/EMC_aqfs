
C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
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
C***********************************************************************

C...........................  begin DFILL()  .....................

        SUBROUTINE  IFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS, NROWS, NLAYS
        INTEGER         GRID( NCOLS, NROWS, NLAYS )
        INTEGER         OP
        REAL		VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L, V


C***********************************************************************
C   begin body of subroutine  IFILL

        IF ( OP .EQ. 1 ) THEN
            DO  13  L = 1, NLAYS
            DO  12  R = 1, NROWS
            DO  11  C = 1, NCOLS
                GRID( C,R, L ) = C
11          CONTINUE
12          CONTINUE
13          CONTINUE
        ELSE IF ( OP .EQ. 2 ) THEN
            DO  23  L = 1, NLAYS
            DO  22  R = 1, NROWS
            DO  21  C = 1, NCOLS
                GRID( C,R, L ) = R
21          CONTINUE
22          CONTINUE
23          CONTINUE
        ELSE IF ( OP .EQ. 3 ) THEN
            DO  33  L = 1, NLAYS
            DO  32  R = 1, NROWS
            DO  31  C = 1, NCOLS
                GRID( C,R, L ) = L
31          CONTINUE
32          CONTINUE
33          CONTINUE
        ELSE
            V = INT( VAL )
            DO  43  L = 1, NLAYS
            DO  42  R = 1, NROWS
            DO  41  C = 1, NCOLS
                GRID( C,R, L ) = V
41          CONTINUE
42          CONTINUE
43          CONTINUE
        END IF

        RETURN
        END


C...........................  begin RFILL()  .....................

        SUBROUTINE  RFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS, NROWS, NLAYS
        REAL            GRID( NCOLS, NROWS, NLAYS )
        INTEGER         OP
        INTEGER         VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L


C***********************************************************************
C   begin body of subroutine  RFILL

        IF ( OP .EQ. 1 ) THEN
            DO  13  L = 1, NLAYS
            DO  12  R = 1, NROWS
            DO  11  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( C )
11          CONTINUE
12          CONTINUE
13          CONTINUE
        ELSE IF ( OP .EQ. 2 ) THEN
            DO  23  L = 1, NLAYS
            DO  22  R = 1, NROWS
            DO  21  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( R )
21          CONTINUE
22          CONTINUE
23          CONTINUE
        ELSE IF ( OP .EQ. 3 ) THEN
            DO  33  L = 1, NLAYS
            DO  32  R = 1, NROWS
            DO  31  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( L )
31          CONTINUE
32          CONTINUE
33          CONTINUE
        ELSE
            DO  43  L = 1, NLAYS
            DO  42  R = 1, NROWS
            DO  41  C = 1, NCOLS
                GRID( C,R, L ) = FLOAT( VAL )
41          CONTINUE
42          CONTINUE
43          CONTINUE
        END IF

        RETURN
        END


C...........................  begin DFILL()  .....................

        SUBROUTINE  DFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER          NCOLS, NROWS, NLAYS
        DOUBLE PRECISION GRID( NCOLS, NROWS, NLAYS )
        INTEGER          OP
        REAL		 VAL


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER          R, C, L
        DOUBLE PRECISION V


C***********************************************************************
C   begin body of subroutine  DFILL

        IF ( OP .EQ. 1 ) THEN
            DO  13  L = 1, NLAYS
            DO  12  R = 1, NROWS
            DO  11  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( C )
11          CONTINUE
12          CONTINUE
13          CONTINUE
        ELSE IF ( OP .EQ. 2 ) THEN
            DO  23  L = 1, NLAYS
            DO  22  R = 1, NROWS
            DO  21  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( R )
21          CONTINUE
22          CONTINUE
23          CONTINUE
        ELSE IF ( OP .EQ. 3 ) THEN
            DO  33  L = 1, NLAYS
            DO  32  R = 1, NROWS
            DO  31  C = 1, NCOLS
                GRID( C,R, L ) = DBLE( L )
31          CONTINUE
32          CONTINUE
33          CONTINUE
        ELSE
            V = DBLE( VAL )
            DO  43  L = 1, NLAYS
            DO  42  R = 1, NROWS
            DO  41  C = 1, NCOLS
                GRID( C,R, L ) = V
41          CONTINUE
42          CONTINUE
43          CONTINUE
        END IF

        RETURN
        END


