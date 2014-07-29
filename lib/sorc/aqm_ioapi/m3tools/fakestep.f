
        SUBROUTINE  FAKESTEP( FNAME, JDATE, JTIME, OPS, VAL )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  66
C
C  FUNCTION:  Perform memory allocation and work for program FAKEFILE
C
C
C  PRECONDITIONS REQUIRED:
C	file FNAME already open; 
C	description of FNAME in FDESC3.EXT valid.        
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	IFILL, RFILL, DFILL
C
C  REVISION  HISTORY:
C	prototype 8/1995 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Modified 11/2005 by CJC:  removed unused vbles
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*16	FNAME
        INTEGER		JDATE
        INTEGER		JTIME
        INTEGER		OPS( NVARS3D )
        INTEGER		VAL( NVARS3D )


C...........   LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        INTEGER, SAVE :: NCOLS, NROWS, NLAYS
	INTEGER, SAVE :: STEP = 0
        REAL             GRID( NCOLS3D, NROWS3D, NLAYS3D )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*80	MESG
        INTEGER		C, R, L, V
        INTEGER		TYPE, OP, IDEV
        INTEGER		LTYPE, LOP


C***********************************************************************
C   begin body of subroutine  FAKESTEP

        STEP  = STEP + 1
        LTYPE = -9999
        LOP   = -9999

        DO  99  V = 1, NVARS3D

            TYPE = VTYPE3D( V )
            OP   = OPS( V )

            IF ( OP .LT. 0 ) THEN

                IDEV = -OP
                DO  L = 1, NLAYS3D
                DO  R = 1, NROWS3D
                    READ( IDEV,* ) (GRID( C,R,L ), C = 1, NCOLS3D )
                END DO
                END DO

            ELSE IF ( TYPE .NE. LTYPE  .OR.  
     &                LOP  .NE. OP     .OR.
     &                TYPE .EQ. 5         ) THEN

                IF ( OP .EQ. 4 ) THEN
                    VAL( V ) = FLOAT( STEP )
                END IF
                IF ( TYPE .EQ. M3INT ) THEN
                    CALL IFILL( GRID, NCOLS, NROWS, NLAYS, 
     &                          OPS( V ), VAL( V ) )
                ELSE IF ( TYPE .EQ. M3REAL ) THEN
                    CALL RFILL( GRID, NCOLS, NROWS, NLAYS,
     &                          OPS( V ), VAL( V ) )
                ELSE IF ( TYPE .EQ. M3DBLE ) THEN
                    CALL DFILL( GRID, NCOLS, NROWS, NLAYS,
     &                          OPS( V ), DBLE( VAL( V ) ) )
                END IF
                LTYPE = TYPE
                LOP   = OP

            END IF

            IF( .NOT. WRITE3( FNAME, VNAME3D( V ), 
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Error writing ' // VNAME3D( V ) //
     &                 ' to ' // FNAME
                CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
            END IF
            
99      CONTINUE

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in subroutine FAKESTEP ***',
     &            /5X , A ,
     &           // )        !  generic error message format


C...........   Informational (LOG) message formats... 92xxx


C...........   Formatted file I/O formats............ 93xxx


C...........   Internal buffering formats............ 94xxx

94010	FORMAT( A, I4, 2X, A )


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

