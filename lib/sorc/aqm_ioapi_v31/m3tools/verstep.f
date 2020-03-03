
        SUBROUTINE  VERSTEP ( NCOLS, NROWS, NLAYS, NVARS,
     &                        JDATE, JTIME,
     &                        INNAME, VNAME,
     &                        FILEOUT, RUNIT, OUTNAME )

C***********************************************************************
C Version "$Id: verstep.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  75
C
C  FUNCTION:
C       Vertical integration of 3-D variables VNAME(*) from file
C       INNAME, with optional statistics report and optional 2-D
C       output to file OUTNAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C	Stack-allocation operating environment (such as CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
C       Modified  9/99 by CJC for enhanced portability
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 12/2011 by CJC:  INTENT for arguments
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT(IN   ) :: NCOLS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NROWS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NLAYS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NVARS   ! number of vbles to be totaled
        INTEGER     , INTENT(IN   ) :: JDATE   ! current model date
        INTEGER     , INTENT(IN   ) :: JTIME   ! current model time
        LOGICAL     , INTENT(IN   ) :: FILEOUT ! flag:  generate output file
        INTEGER     , INTENT(IN   ) :: RUNIT   ! unit number for report-file (or -1: no report)
        CHARACTER*16, INTENT(IN   ) :: VNAME( MXVARS3 ) !  list of vble names
        CHARACTER*16, INTENT(IN   ) :: INNAME  !  logical name of the input file
        CHARACTER*16, INTENT(IN   ) :: OUTNAME !  logical name of the output file


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, L, V      !  col, row, level, variable, counters
        INTEGER         MC, MR, ML      !  indexes for maximum
        INTEGER         NC, NR, NL      !  indexes for minimum
        REAL            TOTS( NCOLS, NROWS )
        REAL            GRID( NCOLS, NROWS, NLAYS )
        REAL            T
        REAL            AMAX
        REAL            AMIN
        REAL            ASUM
        REAL            ASSQ
        REAL            DNOM
				
        CHARACTER*120   MESG

C***********************************************************************
C   begin body of subroutine  VERSTEP

         IF ( RUNIT .GE. 0 ) THEN
            WRITE( RUNIT,92010 )
     &              INNAME,
     &              JDATE, JTIME,
     &              DT2STR( JDATE, JTIME )
        END IF

        DO  288  V = 1, NVARS

            IF ( .NOT. READ3( INNAME, VNAME( V ), ALLAYS3,
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // INNAME //
     &                 ' variable ' // VNAME( V )
                CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,
     &                       MESG, 2 )
                RETURN
            END IF      !  if read3() failed

            DO  122  R = 1, NROWS       !  initialization:  layer 1
            DO  111  C = 1, NCOLS
                TOTS( C,R ) = GRID( C,R,1 )
111         CONTINUE
122         CONTINUE

            DO  155  L = 2, NLAYS       !  all other layers
            DO  144  R = 1, NROWS
            DO  133  C = 1, NCOLS
                TOTS( C,R ) = TOTS( C,R ) + GRID( C,R,L )
133         CONTINUE
144         CONTINUE
155         CONTINUE

            IF ( RUNIT .GE. 0 ) THEN

C...........   Initialization for 3-D GRID stats:

                MC   = 1
                MR   = 1
                ML   = 1
                NC   = 1
                NR   = 1
                NL   = 1
                T    = GRID( 1,1,1 )
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0

                DO  222  R = 1, NROWS   !  gridded initialization:  layer 1
                DO  211  C = 1, NCOLS
                    T = GRID( C,R,1 )
                    ASUM = ASUM + T
                    ASSQ = ASSQ + T*T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = C
                        MR   = R
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = C
                        NR   = R
                    END IF
211             CONTINUE
222             CONTINUE

                DO  255  L = 2, NLAYS   !  3-D traversal:  all other layers
                DO  244  R = 1, NROWS
                DO  233  C = 1, NCOLS
                    T    = GRID( C,R,L )
                    ASUM = ASUM + T
                    ASSQ = ASSQ + T*T
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
233             CONTINUE
244             CONTINUE
255             CONTINUE

                DNOM = 1.0 / FLOAT( NCOLS * NROWS * NLAYS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0 ) )
                WRITE( RUNIT,92020 )
     &              VNAME( V ) // ' 3-D grid statistics' ,
     &              'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',
     &              'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',
     &              'Mean  ', ASUM,
     &              'Sigma ', ASSQ

C...........   Initialization for 2-D TOTS stats:

            MC   = 1
            MR   = 1
            NC   = 1
            NR   = 1
            T    = TOTS( 1,1 )
            AMAX = T
            AMIN = T
            ASUM = 0.0
            ASSQ = 0.0

            DO  277  R = 1, NROWS       ! traverse 2-D TOTS
            DO  266  C = 1, NCOLS
                T    = TOTS( C,R )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    MR   = R
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NR   = R
                END IF
266         CONTINUE
277         CONTINUE

                DNOM = 1.0 / FLOAT( NCOLS * NROWS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0 ) )
                WRITE( RUNIT,92030 )
     &              VNAME( V ) // ' 2-D vertical-total statistics' ,
     &              'Max   ', AMAX, ' @(c,r)=(', MC, MR, ')',
     &              'Min   ', AMIN, ' @(c,r)=(', NC, NR, ')',
     &              'Mean  ', ASUM,
     &              'Sigma ', ASSQ

            END IF      !  if runit > 0

            IF ( FILEOUT ) THEN
                IF ( .NOT. WRITE3( OUTNAME, VNAME( V ),
     &                             JDATE, JTIME, TOTS ) ) THEN
                    MESG = 'Could not write totals to ' // OUTNAME
                    CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,
     &                          MESG, 2 )
                END IF      !  if write3() failed
            END IF          !  if file output desired

288     CONTINUE        !  end loop on variables V



        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X ,
     &         'File:  ', A, 5X,
     &         'Date and time:', I7.7, ':', I6.6, A )

92020   FORMAT ( /5X , 'Variable ', A16,
     &           2( /5X, A, 1PE12.5, A, I2, ',', I2, ',', I2, A ),
     &           2( /5X, A, 1PE12.5 ) )

92030   FORMAT ( 5X , 'Variable ', A16,
     &           2( /5X, A, 1PE12.5, A, I2, ',', I2, A ),
     &           2( /5X, A, 1PE12.5 ) )

        END SUBROUTINE VERSTEP

