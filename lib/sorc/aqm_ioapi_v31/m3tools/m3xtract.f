
        PROGRAM  M3XTRACT

C***********************************************************************
C Version "$Id: m3xtract.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  114
C
C  FUNCTION:
C       extracts a subset of variables from the input file for a
C       specified time period, and writes them to the output file,
C       with optional renaming in the process.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       or Cray-pointers and MALLOC(),  consistency with I/O API
C       INCLUDE-file PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       XTSTEPL, Models-3 I/O API.
C
C  REVISION  HISTORY:
C      Prototype 1/95 by CJC
C
C      Version   5/95 by CJC:  command line arguments
C
C       Version  8/95 by CJC:  optional renaming on output
C
C       Version  1/2000 by CJC:  support for layer subranges
C
C       Version 11/2001 by CJC for I/O API Version 2.1
C
C       Version 11/2003 by CJC for I/O API Version 3:  fix handling of
C       "extract all variables".
C
C       Version  1/2004 by CJC for I/O API Version 3:  add support for
C       Sparse Matrix data type ("all variables" only)
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 06/2011 by CJC:  Bug-fix from Christian Hogrefe at line 314 &ff
C
C       Version 09/2012 by CJC:  Bug-fix from Sarika Kulkarni, CA ARB
C
C       Version 11/2013 by CJC:  support for M3INT, M3DBLE
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME   = 'M3XTRACT'
        CHARACTER*16, PARAMETER :: BLANK16 = ' '

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  buffer for m3exit(), etc

        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file

        INTEGER         SIZE    ! volume of one variable
        INTEGER         DLAY    ! layer-offset
        INTEGER         LAY1    ! lowest layer number to write
        INTEGER         LAY2    ! highest layer number to write
        INTEGER         LAYER   ! layer, or ALLAYS3
        INTEGER         NCOLS   ! grid dimensions, from INAME header
        INTEGER         NROWS   ! grid dimensions, from INAME header
        INTEGER         NLAYS   ! grid dimensions, from INAME header
        INTEGER         NVARS   ! number of vbles in ONAME
        INTEGER         NVSAV   ! saved copy number of vbles in INAME
        INTEGER         NVLOW   ! lwoer bound for getnum
        INTEGER         VTYPE ( MXVARS3 ) !  variable type:  M3(INT|REAL|DBLE)
        CHARACTER*16    VNAMEI( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    VNAMEO( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS ( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC ( MXVARS3 ) !  list of vble descs
        CHARACTER*80    PROMPT  !  scratch buffer for prompt
        CHARACTER*80    ALINE   !  scratch buffer for prompt
        CHARACTER*16    ANAME   !  scratch buffer for variable names
        INTEGER         SDATE   !  starting date, from user
        INTEGER         STIME   !  starting time, from user
        INTEGER         JDATE   !  current date
        INTEGER         JTIME   !  current time
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NSTEPS  !  duration in TSTEPs
        INTEGER         I, L, V !  scratch variables
        INTEGER         VMAX    !  string length for names
        INTEGER         UMAX    !  string length for units
        INTEGER         DMAX    !  string length for descriptions
        INTEGER         IOS     !  I/O status
        INTEGER         ISTAT

        LOGICAL :: EFLAG = .FALSE.

        INTEGER, ALLOCATABLE :: IBUF( :,: )
        REAL   , ALLOCATABLE :: RBUF( :,: )
        REAL*8 , ALLOCATABLE :: DBUF( :,: )

C.........................................................................
C   begin body of program  M3XTRACT

        LOGDEV = INIT3()
        WRITE ( *,92000 )
     &  ' ',
     &  'Program M3XTRACT to extract selected variables from a',
     &  'GRIDDED, BOUNDARY, CUSTOM, or SPARSE MATRIX Models-3 file',
     &  'for a specified time period, optionally rename them, and',
     &  'write them to another such file.  You may select either',
     &  'to extract all layers, to extract a single selected layer,',
     &  'or to extract a selected subrange of the layers.'
        WRITE ( *,92000 )
     &  ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the set of variables to be extracted, and the time period to',
     &  'extract them for.',
     &  ' ',
     &  'USAGE:  m3xtract [INFILE OUTFILE]',
     &  '(and then answer the prompts). ',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &  ' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: m3xtract.f 44 2014-09-12 18:03:16Z coats                $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 2 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'usage:  m3xtract [INFILE OUTFILE]', 2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', PNAME )

        ELSE            !  argcnt 2

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                 // TRIM( INAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
            END IF

            CALL GETARG( 2, ENVBUF )
            ONAME = ENVBUF( 1:16 )

        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file ' // INAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NVSAV  = NVARS3D
        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        NLAYS  = NLAYS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D

        IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            SIZE = NCOLS3D * NROWS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            SIZE = 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
        ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            SIZE = NCOLS3D
        ELSE IF ( FTYPE3D .EQ. SMATRX3 ) THEN
            SIZE  = NROWS3D + NCOLS3D * ( 1 + NVARS3D * NLAYS3D )
            NVARS = NVARS3D
            LAYER = -1
            DO  V = 1, NVARS
                VNAMEI( V ) = VNAME3D( V )
                VNAMEO( V ) = VNAME3D( V )
                UNITS ( V ) = UNITS3D( V )
                VDESC ( V ) = VDESC3D( V )
                VTYPE ( V ) = VTYPE3D( V )
            END DO
            GO TO 200   !!  bypass "nvars=0" test
        ELSE
            MESG = 'Input file ' // INAME // 'has unsupported type.'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        LAYER = GETNUM( -1, NLAYS3D, 1,
     &  'Enter layer to extract, 0 for all layers, -1 for subrange' )

        IF ( LAYER .EQ. 0 ) THEN
            LAYER = ALLAYS3     !  fixup:  "read3() all layers"
            LAY1  = 1
            LAY2  = NLAYS3D
            DLAY  = 1
        ELSE IF ( LAYER .EQ. -1 ) THEN
            LAYER = ALLAYS3     !  fixup:  "read3() all layers"
            LAY1  = GETNUM( 1, NLAYS3D, 1,
     &                      'Enter lowest layer in subrange' )
            DLAY = LAY1
            LAY2  = GETNUM( LAY1, NLAYS3D, NLAYS3D,
     &                      'Enter highest layer in subrange' )
            NLAYS3D = LAY2 - LAY1 + 1         !  fixup on file description
            L = 0
            DO  I = LAY1, LAY2+1
                 L = L + 1
                 VGLVS3D(L) = VGLVS3D(I)
            END DO
        ELSE
            NLAYS   = 1         !  for number of input layers
            NLAYS3D = 1         !  fixup on file description
            LAY1    = LAYER
            LAY2    = LAYER
            DLAY    = 1
            VGLVS3D(1) = VGLVS3D(LAYER)
            VGLVS3D(2) = VGLVS3D(LAYER+1)
        END IF

C...............   Allocate I/O Buffer:

        ALLOCATE ( IBUF( SIZE, LAY2-LAY1+1 ),
     &             RBUF( SIZE, LAY2-LAY1+1 ),
     &             DBUF( SIZE, LAY2-LAY1+1 ), STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )
     &         'Memory allocation failed:  STAT=', ISTAT
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C.......   Get max string-lengths for use in variables-listing:

        VMAX = 0
        UMAX = 0
        DMAX = 0
        DO V = 1, NVARS3D
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( V ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( V ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( V ) ) )
        END DO


        NVARS  =  0
        V      =  0
        NVLOW  = -1
        PROMPT = 'Enter # for vble (0 to quit, -1 for ALL VBLES)'

111     CONTINUE        !  loop getting variables-list for extraction

            IF( MOD( NVARS,10 ) .EQ. 0 ) THEN
                WRITE( *,92000 )
     &              ' ', 'The list of variables in file "' //
     &              TRIM( INAME ) // '" is:', ' '
                WRITE( *,92010 )
     &              ( I,
     &                VNAME3D( I )( 1:VMAX ) // ' (' //
     &                UNITS3D( I )( 1:UMAX ) // '): ' //
     &                VDESC3D( I )( 1:DMAX ), I = 1, NVSAV  )
            END IF

            IF ( NVARS .EQ. 0 ) THEN
                WRITE( *,92010 )  NVSAV+1, 'Extract all variables'
                V = GETNUM( NVLOW, NVSAV+1, NVSAV+1, PROMPT )
            ELSE
                V = GETNUM( NVLOW, NVSAV, 1 + MOD( V, NVSAV ), PROMPT )
            END IF

            IF ( V .EQ. 0  ) GO TO  199      !  to end of loop
            IF ( V .EQ. NVSAV+1 .OR. V .EQ. -1 ) THEN
                WRITE( *,92000 ) '', 'Extracting all variables', ' '
                NVARS = NVSAV
                DO  V = 1, NVARS
                    VNAMEI( V ) = VNAME3D( V )
                    VNAMEO( V ) = VNAME3D( V )
                    UNITS ( V ) = UNITS3D( V )
                    VDESC ( V ) = VDESC3D( V )
                    VTYPE ( V ) = VTYPE3D( V )
                END DO
                GO TO 199
            END IF

            !!  Didn't select "ALL"; reset for remaining GETNUM()'s

            NVLOW  = 0
            PROMPT = 'Enter # for vble (0 to quit)'
            NVARS  = NVARS + 1

C...........   Optional renaming of this variable:

            ALINE = 'Enter output-name for this variable [' //
     &              TRIM( VNAME3D( V ) ) // '] >> '
            CALL GETSTR( ALINE, VNAME3D( V ), ANAME )
            IF( ANAME .EQ. BLANK16 ) THEN
                VNAMEO( NVARS ) = VNAME3D( V )
            ELSE
                VNAMEO( NVARS ) = ANAME
            END IF

            VNAMEI( NVARS ) = VNAME3D( V )
            UNITS ( NVARS ) = UNITS3D( V )
            VDESC ( NVARS ) = VDESC3D( V )
            VTYPE ( NVARS ) = VTYPE3D( V )

            IF ( NVARS .LT. MXVARS3 )  GO TO  111   !  to head of loop

199     CONTINUE        !  end loop getting variables-list for analysis

        IF ( NVARS .EQ. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'No variables selected', 2 )
        ELSE
            NVARS3D = NVARS
        END IF

200     CONTINUE        !  end loop getting variables-list for analysis


C.......   Get starting date and time, and duration:

        IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

            SDATE  = 0
            STIME  = 0
            NSTEPS = 1

        ELSE                            !  time-dependent file

            SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                  'Enter starting date (YYYYDDD) for run' )
            STIME  = GETNUM( 0, 239999, STIME3D,
     &                  'Enter starting time (HHMMSS) for run' )
            RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
            RUNLEN = GETNUM( 0, 999999999, RUNLEN,
     &                  'Enter duration (HHMMSS) for run' )
            NSTEPS = TIME2SEC( TSTEP )
            NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS

        END IF          !  time-independent file, or not


C.......   Build description for the output file, and create accordingly:
C.......   Re-use most of the input-file description.

        SDATE3D = SDATE
        STIME3D = STIME

        DO  V = 1, NVARS

            VNAME3D( V ) = VNAMEO( V )
            UNITS3D( V ) = UNITS ( V )
            VDESC3D( V ) = VDESC ( V )
            VTYPE3D( V ) = VTYPE ( V )

        END DO

        ISTAT = FSUNKN3
        IF ( ARGCNT .EQ. 0 ) THEN
            ONAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           ISTAT, 'OUTFILE', PNAME )
        ELSE    !  argcnt = 2:
            IF ( .NOT. OPEN3( ONAME, ISTAT, PNAME ) ) THEN
                MESG = 'Could not open output file ' // ONAME
                CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
            END IF
        END IF          !  if argcnt zero, or 2


C.......   Process this period in the input file:

        JDATE = SDATE
        JTIME = STIME

        DO  I = 1, NSTEPS

            IF ( FTYPE3D .EQ. SMATRX3 ) THEN

                IF ( .NOT.READ3( INAME, VNAMEI, LAYER,
     &                           JDATE, JTIME, RBUF ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                ELSE  IF ( .NOT. WRITE3( ONAME, VNAMEO,
     &                                   JDATE, JTIME, RBUF ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF

            ELSE

                DO  V = 1, NVARS

                   IF ( VTYPE( V ) .EQ. M3REAL ) THEN

                       DO L = LAY1, LAY2
                           IF ( .NOT.READ3( INAME, VNAMEI(V), L,
     &                               JDATE, JTIME, RBUF(1,L) ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                            END IF
                        END DO

                        IF ( .NOT. WRITE3( ONAME, VNAMEO(V),
     &                                     JDATE, JTIME, RBUF ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                        END IF

                   ELSE IF ( VTYPE( V ) .EQ. M3INT ) THEN

                       DO L = LAY1, LAY2
                           IF ( .NOT.READ3( INAME, VNAMEI(V), L,
     &                               JDATE, JTIME, IBUF(1,L) ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                            END IF
                        END DO

                        IF ( .NOT. WRITE3( ONAME, VNAMEO(V),
     &                                     JDATE, JTIME, IBUF ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                        END IF

                   ELSE IF ( VTYPE( V ) .EQ. M3DBLE ) THEN

                       DO L = LAY1, LAY2
                           IF ( .NOT.READ3( INAME, VNAMEI(V), L,
     &                               JDATE, JTIME, DBUF(1,L) ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                            END IF
                        END DO

                        IF ( .NOT. WRITE3( ONAME, VNAMEO(V),
     &                                     JDATE, JTIME, DBUF ) ) THEN
                                EFLAG = .TRUE.
                                CYCLE
                        END IF

                   END IF

                END DO                !  end loop on variables

            END IF      !  if ftype=smatrx3, or not

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO         !  end loop on time steps


C...............  Shut down program:

        IF ( EFLAG ) THEN
            MESG  = 'Failure in program'
            ISTAT = 2
        ELSE
            MESG  = 'Success in program'
            ISTAT = 0
        END IF
        CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


C..........................   FORMAT STATEMENTS  ....................
C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( 1X , I5, ':  ', A )


        END  PROGRAM  M3XTRACT

