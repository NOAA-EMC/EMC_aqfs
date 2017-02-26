
        PROGRAM  VERTOT

C***********************************************************************
C Version "$Id: vertot.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2010 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  103
C
C  FUNCTION:
C       For a user-specified GRIDDED Models-3 file and list of variables
C       within it, compute vertical-column totals for each specified
C       variable.  Optionally put the output to a user-specified 1-layer
C       GRIDDED output file, and  optionally write statistics to the log.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETYN, GETNUM, Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C
C       Version   5/1995 by CJC:  command line arguments
C
C       Version   6/1996 by CJC:  special treatment for NVARS3D=1
C
C       Version   4/1998 by M Houyoux: Changed exit to use M3EXIT
C
C       Modified  9/1999 by CJC for enhanced portability
C
C       Version  11/2001 by CJC for I/O API Version 2.1
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'VERTOT'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LUNIT   !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments

        LOGICAL         FILEOUT ! flag:  generate output file
        LOGICAL         STATOUT ! flag:  generate statistics report
        LOGICAL         EFLAG   ! flag:  error has happened

        CHARACTER*16    INAME   !  logical name of the input file
        CHARACTER*16    ONAME   !  logical name of the output file
        INTEGER         RUNIT    !  report-file unit number

        INTEGER         NCOLS   ! grid dimensions, from INAME header
        INTEGER         NROWS   ! grid dimensions, from INAME header
        INTEGER         NLAYS   ! grid dimensions, from INAME header
        INTEGER         NVARS   !  number of vbles to be totaled, from user
        INTEGER         NVLOW   ! lwoer bound for getnum
        CHARACTER*16    VNAME( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
        CHARACTER*80    PROMPT  !  scratch buffer for prompt
        INTEGER         JDATE   !  starting date, from user
        INTEGER         JTIME   !  starting time, from user
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         NSTEPS  !  duration in TSTEPs
        INTEGER         I, V    !  scratch variable
        INTEGER         VMAX    !  string length for names
        INTEGER         UMAX    !  string length for units
        INTEGER         DMAX    !  string length for descriptions
        CHARACTER*256   MESG    !  buffer for m3exit(), etc

C.........................................................................
C   begin body of program  VERTOT

        LUNIT = INIT3()
        ARGCNT = IARGC()

        WRITE ( LUNIT,92000 )
     &  ' ',
     &  'Program VERTOT to compute vertical totals of selected REAL',
     &  'variables from a user-specified GRIDDED Models-3 file, with',
     &  'optional generation of either a vertically-summed output',
     &  'file for those variables, a statistics report for those ',
     &  'variables, or both.  You need to have set the environment ',
     &  'variables for the input and (if any) output file file',
     &  'logical names.  You will be asked for those logical names,',
     &  'to select the set of variables to be extracted, and the ',
     &  'time period to analyze.',
     &  ' ',
     &  'USAGE:  vertot [INFILE]',
     &  '(and then answer the prompts).',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: vertot.f 44 2014-09-12 18:03:16Z coats                  $',
     &' '

        IF ( ARGCNT .GT. 1 ) THEN

            CALL M3EXIT( PNAME, 0,0, 'usage:  vertot [INFILE]', 2 )

        ELSE IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', PNAME )

        ELSE

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file ' // INAME
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file ' // INAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
        TSTEP = TSTEP3D

C.......   Get max string-lengths for use in variables-listing:

        VMAX = 0
        UMAX = 0
        DMAX = 0
        DO  I = 1, NVARS3D
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
        END DO

        WRITE( *,92000 )
     &  ' ', 'The list of variables in this file is:', ' ',
     &  ( VNAME3D( I )( 1:VMAX ) // ' (' //
     &    UNITS3D( I )( 1:UMAX ) // '): ' //
     &    VDESC3D( I )( 1:DMAX ), I = 1, NVARS3D )

        IF ( NVARS3D .EQ. 1 ) THEN

            NVARS = 1
            VNAME( NVARS ) = VNAME3D( 1 )
            UNITS( NVARS ) = UNITS3D( 1 )
            VDESC( NVARS ) = VDESC3D( 1 )

            IF ( VTYPE3D( 1 ) .NE. M3REAL ) THEN
                MESG = 'Variable "' // TRIM( VNAME3D( 1 ) ) //
     &                 '" not of type REAL; ' //
     &                 'VERTOT processes REAL only'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        ELSE    !  else nvars3d > 1:


            NVARS  =  0
            I      =  0
            NVLOW  = -1
            PROMPT = 'Enter # for vble (0 to quit, -1 for ALL VBLES)'
            EFLAG  = .FALSE.

111         CONTINUE        !  loop getting variables-list for analysis

                I = GETNUM( NVLOW, NVARS3D, 1 + MOD( I, NVARS3D ),
     &                      PROMPT )

                IF ( I .EQ. -1 ) THEN
                    NVARS = NVARS3D
                    DO  V = 1, NVARS3D
                        VNAME( V ) = VNAME3D( V )
                        UNITS( V ) = UNITS3D( V )
                        VDESC( V ) = VDESC3D( V )
                        EFLAG = EFLAG .OR. ( VTYPE3D( V ) .NE. M3REAL )
                    END DO
                ELSE IF ( I .GT. 0 )THEN
                    PROMPT = 'Enter # for vble (0 to quit)'
                    NVLOW  = 0
                    IF ( VTYPE3D( I ) .NE. M3REAL ) THEN
                        MESG = 'Variable "' //TRIM( VNAME3D( 1 ) ) //
     &                     '" not of type REAL; please try again'
                        CALL M3MSG2( MESG )
                        GO TO  111
                    END IF
                    NVARS  = NVARS + 1
                    VNAME( NVARS ) = VNAME3D( I )
                    UNITS( NVARS ) = UNITS3D( I )
                    VDESC( NVARS ) = VDESC3D( I )
                    IF ( NVARS .LT. MXVARS3 )  GO TO  111
                END IF

            IF ( NVARS .EQ. 0 ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'No variables selected', 2 )
                GO TO  999
            ELSE IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &          'File has INTEGER or DOUBLE variables; ' //
     &          'VERTOT processes REAL only', 2 )
            END IF  !  if nvars=0, or not

        END IF      !  if nvars3d=1, or not


C.......   Get mode of operation:

        FILEOUT = GETYN( 'Do you want an output totals data file?',
     &                   .TRUE. )
        STATOUT = GETYN( 'Do you want an output  statistics file?',
     &                   .TRUE. )

        IF ( TSTEP .EQ. 0 ) THEN
            JDATE  = 0
            JTIME  = 0
            NSTEPS = 1
        ELSE
            JDATE = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                      'Enter starting date (YYYYDDD) for run' )
            JTIME = GETNUM( 0, 239999, STIME3D,
     &                      'Enter starting time (HHMMSS) for run' )
            RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
            RUNLEN = GETNUM( 0, 999999999, RUNLEN,
     &                      'Enter duration (HHMMSS) for run' )
            NSTEPS = TIME2SEC( TSTEP )
            NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS
        END IF          !  time-independent file, or not

        IF ( FILEOUT ) THEN     !  create output file

            SDATE3D = JDATE
            STIME3D = JTIME
            NLAYS3D = 1
            NVARS3D = NVARS
            DO I = 1, NVARS
                VNAME3D( I ) = VNAME( I )
                UNITS3D( I ) = UNITS( I )
                VDESC3D( I ) = VDESC( I )
                VTYPE3D( I ) = M3REAL
            END DO

            ONAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OUTFILE', PNAME )

        END IF      !  if fileout

        IF( STATOUT ) THEN

            PROMPT = 'Enter the REPORT FILE logical name '
     &                // '(or "NONE" for screen output)'
            RUNIT = PROMPTFFILE( PROMPT,
     &                          .FALSE., .TRUE., 'REPORT', PNAME )

        ELSE

            RUNIT = -1

        END IF      !  if statout or not

C.......   Process this period in the input file:

        IF ( RUNIT .LT. 0 ) RUNIT = LUNIT
        DO  I = 1, NSTEPS

            CALL VERSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                    JDATE, JTIME,
     &                    INAME, VNAME,
     &                    FILEOUT, RUNIT, ONAME )

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO         !  end loop on time steps


999     CONTINUE        !  shut down the program:

        CALL M3EXIT( PNAME, 0, 0,
     &               'Program VERTOT completed successfully', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in program VERTOT ***',
     &            /5X , A ,
     &           // )        !  generic error message format


C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92999   FORMAT ( //5X , A, // )


C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT ( A16 )


C...........   Internal buffering formats............ 94xxx


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END PROGRAM VERTOT

