
        SUBROUTINE  DIFFSTEP ( NCOLS,  NROWS,  NLAYS,  NVARS,
     &                         JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                         NAMEA,  NAMEB,  
     &                         WNAMES, WTYPES, OPNAME, RDEV,
     &                         NAMEC,  VNAME, JDATEC, JTIMEC )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  102
C
C  FUNCTION:
C       Statistics report to RDEV on variables WNAMES( 1,* ) and
C       WNAMES( 2,* ) from files NAMEA and NAMEB, respectively,
C       and on the results of using GRIDOPS to apply the operations
C       OPNAME( * ) to them.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3()
C       Utility routines:  DT2STR()
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C	Version  11/1994 by CJC for new version of I/O API
C       Modified  9/1999 by CJC for enhanced portability
C       Modified  9/1999 by CJC:  REAL*8 accumulators
C
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   ! grid dimensions, from file header
        INTEGER         NROWS   ! grid dimensions, from file header
        INTEGER         NLAYS   ! grid dimensions, from file header
        INTEGER         NVARS   !  number of vbles to be totaled
        INTEGER         JDATEA  ! current model date, file A
        INTEGER         JTIMEA  ! current model time, file A
        INTEGER         JDATEB  ! current model date, file B
        INTEGER         JTIMEB  ! current model time, file B
        CHARACTER*16    NAMEA   !  logical name of the input file
        CHARACTER*16    NAMEB   !  logical name of the input file
        CHARACTER*16    WNAMES( 2, MXVARS3 ) !  list of vble names
        INTEGER         WTYPES( 2, MXVARS3 ) !  list of vble types
        CHARACTER*16    OPNAME( MXVARS3 )    !  list of grid-operation names
        INTEGER         RDEV    ! unit number for output
        CHARACTER*16    NAMEC   ! name of output file, or "NONE"
        CHARACTER*16    VNAME( NVARS )   !  names of output variables
        INTEGER         JDATEC  ! current model date, file C
        INTEGER         JTIMEC  ! current model time, file C


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL             GRID1( NCOLS, NROWS, NLAYS )
        REAL             GRID2( NCOLS, NROWS, NLAYS )
        REAL             GRID3( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE1( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE2( NCOLS, NROWS, NLAYS )
        INTEGER          INTG1( NCOLS, NROWS, NLAYS )
        INTEGER          INTG2( NCOLS, NROWS, NLAYS )

        LOGICAL         FLAG1, FLAG2

        INTEGER         C, R, L, V      !  col, row, level, variable, counters
        INTEGER         MC, MR, ML      !  indexes for maximum
        INTEGER         NC, NR, NL      !  indexes for minimum
        INTEGER         NN
        REAL            T
        REAL            AMAX
        REAL            AMIN
        REAL*8          ASUM, BSUM, CSUM
        REAL*8          ASSQ, BSSQ, CSSQ
        REAL*8          DNOM
	CHARACTER*24    DTBUFA
	CHARACTER*24    DTBUFB
	CHARACTER*80    MESG
        CHARACTER*1     DIGITS
	CHARACTER*80    STATFMT

	INTEGER         SIZE


C***********************************************************************
C   begin body of subroutine  DIFFSTEP

	SIZE = NCOLS * NROWS * NLAYS
        IF (      MAX( NCOLS, NROWS ) .LT. 100 ) THEN
            DIGITS = '2'
        ELSE IF ( MAX( NCOLS, NROWS ) .LT. 1000 ) THEN
            DIGITS = '3'
        ELSE IF ( MAX( NCOLS, NROWS ) .LT. 10000 ) THEN
            DIGITS = '4'
        ELSE IF ( MAX( NCOLS, NROWS ) .LT. 100000 ) THEN
            DIGITS = '5'
        ELSE
            CALL M3EXIT( 'DIFFSTEP', JDATEA, JTIMEA,
     &                   'Format overflow: dims > 99999', 2 )
        END IF
        STATFMT = '( 1X, A3, 2( 1X, 1PE12.5, ''@('', I' // DIGITS //
     &            ''','', I'  // DIGITS //
     &            ''','', I'  // DIGITS //
     &            ''')'' ),2( 1X, 1PE12.5 ) )'
        
        IF ( JDATEA .NE. 0  .OR. JTIMEA .NE. 0 ) THEN

            DTBUFA = DT2STR( JDATEA, JTIMEA )

            IF ( JDATEB .EQ. 0  .AND. JTIMEB .EQ. 0 ) THEN
                WRITE( RDEV,92010 )
     &              'Date and time', JDATEA, JTIMEA, TRIM( DTBUFA )
            ELSE IF( JDATEB .NE. JDATEA .OR. JTIMEB .NE. JTIMEA ) THEN
                DTBUFB = DT2STR( JDATEB, JTIMEB )
                WRITE( RDEV,92010 )
     &              'Date and time A', JDATEA, JTIMEA, TRIM( DTBUFA ),
     &              'Date and time B', JDATEB, JTIMEB, TRIM( DTBUFB )
            ELSE
                WRITE( RDEV,92010 )
     &              'Date and time', JDATEA, JTIMEA, TRIM( DTBUFA )
            END IF

        ELSE IF ( JDATEB .NE. 0  .OR. JTIMEB .NE. 0 ) THEN	! but *A==0

            WRITE( RDEV,92010 )
     &          'Date and time', JDATEB, JTIMEB, TRIM( DTBUFB )

        END IF	!  if *A nonzero; else if *B nonzero

        IF ( NVARS .EQ. 1 ) WRITE( RDEV,92020 )
     &          'A:' // TRIM( NAMEA ) // '/' // TRIM( WNAMES( 1,1 ) ) //
     &          '  vs  B:' // TRIM( NAMEB ) 
     &          // '/' // TRIM( WNAMES( 2,1 ) ) //
     &          '  vs  ' // TRIM( OPNAME( 1 ) )

        WRITE( RDEV,92030 )
     &  'Max', '@ (c,r,l)', 'Min', '@ (c,r,l)', 'Mean', 'Sigma'


        DO  399  V = 1, NVARS

             IF ( NVARS .GT. 1 ) WRITE( RDEV,92020 )
     &          'A:' // TRIM( NAMEA ) // '/'        // 
     &          TRIM( WNAMES( 1,V ) ) // '  vs  B:' //
     &          TRIM( NAMEB )         // '/'        //
     &          TRIM( WNAMES( 2,V ) ) // '  vs  '   //
     &          TRIM( OPNAME(  V ) )

            IF ( WTYPES( 1,V ) .EQ. M3REAL ) THEN

                FLAG1 = READ3( NAMEA,  WNAMES( 1,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, GRID1 )

            ELSE IF ( WTYPES( 1,V ) .EQ. M3DBLE ) THEN

                FLAG1 = READ3( NAMEA,  WNAMES( 1,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, DBLE1 )
                CALL DBLE2REAL( SIZE, DBLE1, GRID1 )

            ELSE IF ( WTYPES( 1,V ) .EQ. M3INT ) THEN

                FLAG1 = READ3( NAMEA,  WNAMES( 1,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, INTG1 )
                CALL INTG2REAL( SIZE, INTG1, GRID1 )

            ELSE

                FLAG1 = .FALSE.

            END IF

            IF ( WTYPES( 2,V ) .EQ. M3REAL ) THEN

                FLAG2 = READ3( NAMEB,  WNAMES( 2,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, GRID2 )

            ELSE IF ( WTYPES( 2,V ) .EQ. M3DBLE ) THEN

                FLAG2 = READ3( NAMEB,  WNAMES( 2,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, DBLE2 )
                CALL DBLE2REAL( SIZE, DBLE2, GRID2 )

            ELSE IF ( WTYPES( 2,V ) .EQ. M3INT ) THEN

                FLAG2 = READ3( NAMEB,  WNAMES( 2,V ), ALLAYS3,
     &                         JDATEA, JTIMEA, INTG2 )
                CALL INTG2REAL( SIZE, INTG2, GRID2 )

            ELSE

                FLAG2 = .FALSE.

            END IF

            IF ( FLAG1 ) THEN
                MC   = 1
                MR   = 1
                ML   = 1
                NC   = 1
                NR   = 1
                NL   = 1
                T    = GRID1( 1,1,1 )
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0

                DO  L = 1, NLAYS   !  3-D traversal:  all other layers

                    BSUM = 0.0
                    BSSQ = 0.0

                    DO  R = 1, NROWS

                        CSUM = 0.0
                        CSSQ = 0.0

                        DO  C = 1, NCOLS

                            T    = GRID1( C,R,L )
                            CSUM = CSUM + T
                            CSSQ = CSSQ + T*T
                            IF ( T .GT. AMAX ) THEN
                                AMAX = T
                                MC   = C
                                MR   = R
                                ML   = L
                            ELSE IF ( T .LT. AMIN ) THEN
                                AMIN = T
                                NC   = C
                                NR   = R
                                NL   = L
                            END IF

                        END DO          !  end loop on C

                        BSUM = BSUM + CSUM
                        BSSQ = BSSQ + CSSQ

                    END DO          !  end loop on R

                    ASUM = ASUM + BSUM
                    ASSQ = ASSQ + BSSQ

                END DO          !  end loop on L

                DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )
                WRITE( RDEV,STATFMT ) 
     &                  'A  ',
     &                  AMAX, MC, MR, ML,
     &                  AMIN, NC, NR, NL,
     &                  ASUM, ASSQ
                CALL M3FLUSH( RDEV )
            ELSE
                CALL M3WARN( 'M3DIFF:DIFFSTEP', JDATEA, JTIMEA,
     &                       'Read failure:  file ' // NAMEA //
     &                       ' variable ' // WNAMES( 1,V ) )
            END IF

            IF ( FLAG2 ) THEN
                MC   = 1
                MR   = 1
                ML   = 1
                NC   = 1
                NR   = 1
                NL   = 1
                T    = GRID2( 1,1,1 )
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0

                DO  L = 1, NLAYS   !  3-D traversal:  all other layers

                    BSUM = 0.0
                    BSSQ = 0.0

                    DO  R = 1, NROWS

                        CSUM = 0.0
                        CSSQ = 0.0

                        DO  C = 1, NCOLS

                            T    = GRID2( C,R,L )
                            CSUM = CSUM + T
                            CSSQ = CSSQ + T*T
                            IF ( T .GT. AMAX ) THEN
                                AMAX = T
                                MC   = C
                                MR   = R
                                ML   = L
                            ELSE IF ( T .LT. AMIN ) THEN
                                AMIN = T
                                NC   = C
                                NR   = R
                                NL   = L
                            END IF

                        END DO          !  end loop on C

                        BSUM = BSUM + CSUM
                        BSSQ = BSSQ + CSSQ

                    END DO          !  end loop on R

                    ASUM = ASUM + BSUM
                    ASSQ = ASSQ + BSSQ

                END DO          !  end loop on L

                DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )
                WRITE( RDEV,STATFMT ) 
     &                  'B  ',
     &                  AMAX, MC, MR, ML,
     &                  AMIN, NC, NR, NL,
     &                  ASUM, ASSQ
                CALL M3FLUSH( RDEV )
            ELSE
                CALL M3WARN ( 'M3DIFF:DIFFSTEP', JDATEA, JTIMEA,
     &                       'Read failure:  file ' // NAMEB //
     &                       ' variable ' // WNAMES( 2,V ) )
            END IF

            IF ( FLAG1 .AND. FLAG2 ) THEN

                CALL NAMEDOP( OPNAME( V ),
     &                        NCOLS, NROWS, 1, NLAYS,
     &                        GRID1, GRID2, GRID3 )
                IF ( NAMEC( 1:5 ) .NE. 'NONE ' ) THEN
                    IF ( .NOT. WRITE3( NAMEC, VNAME( V ), 
     &                                 JDATEC, JTIMEC, GRID3 ) ) THEN
                        MESG = 'Could not write "' // 
     &                      TRIM( VNAME( V ) ) // '" to "' //
     &                      TRIM( NAMEC ) // '" for ' //
     &                      DT2STR( JDATEC, JTIMEC )
                        CALL M3MESG( MESG )
                    END IF
                END IF

                DO  C = 1, NCOLS * NROWS * NLAYS
                    T    = GRID3( C,1,1 )
                    IF ( T .GT. AMISS3 ) THEN
                        MC = MOD( C, NROWS * NLAYS )
                        NC = MC
                        NN = C / NCOLS
                        MR = MOD( NN, NLAYS )
                        NR = MR
                        ML = NN / NROWS
                        NL = ML
                        GO TO 302
                    END IF
                END DO
                WRITE( RDEV,92010 ) 'A:B nowhere defined'
                GO TO  399

302             CONTINUE
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0
                NN   = 0

                DO  L = 1, NLAYS   !  3-D traversal:  all other layers

                    BSUM = 0.0
                    BSSQ = 0.0

                    DO  R = 1, NROWS

                        CSUM = 0.0
                        CSSQ = 0.0

                        DO  311 C = 1, NCOLS

                            T    = GRID3( C,R,L )
                            IF ( T .LT. AMISS3 )  GO TO 311
                            NN   = NN + 1
                            CSUM = CSUM + T
                            CSSQ = CSSQ + T*T
                            IF ( T .GT. AMAX ) THEN
                                AMAX = T
                                MC   = C
                                MR   = R
                                ML   = L
                            ELSE IF ( T .LT. AMIN ) THEN
                                AMIN = T
                                NC   = C
                                NR   = R
                                NL   = L
                            END IF

311                     CONTINUE

                        BSUM = BSUM + CSUM
                        BSSQ = BSSQ + CSSQ

                    END DO          !  end loop on R

                    ASUM = ASUM + BSUM
                    ASSQ = ASSQ + BSSQ

                END DO          !  end loop on L
                
                DNOM = 1.0 / DBLE( NN )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )
                WRITE( RDEV,STATFMT ) 
     &                  'A:B',
     &                  AMAX, MC, MR, ML,
     &                  AMIN, NC, NR, NL,
     &                  ASUM, ASSQ
                CALL M3FLUSH( RDEV )

            END IF      !  if flag1 & flag2

399     CONTINUE        !  end loop on variables

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

C...............   timestep header:

92010   FORMAT ( /, 2( /1X, A, I9.7, ':', I6.6,  ' (', A, ')', : ) )

C...............   variables and operations header:

92020   FORMAT ( 1X, A )

C...............   stats legend header:

92030   FORMAT ( 1X, A10, A11, A14, A11, A14, A14 )

C...............   Idea for stats report format STATFMT:

92042   FORMAT ( 1X, A3, 
     &           2( 1X, 1PE12.5, '@(', I2, ',', I2, ',', I2, ')' ),
     &           2( 1X, 1PE12.5 ) )

        END

