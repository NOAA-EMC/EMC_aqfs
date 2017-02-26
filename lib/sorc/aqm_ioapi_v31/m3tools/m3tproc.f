
        PROGRAM  M3TPROC

C***********************************************************************
C Version "$Id: m3tproc.f 47 2014-10-03 22:21:37Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line 144
C
C  FUNCTION:
C       Sums, give max, or gives average over a specified time period
C       for a subset of variables from the input file, and writes the
C       processed data to the output file.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with I/O API PARMS3.EXT for name and description lengths.
C       File type is CUSTOM, GRIDDED, or BOUNDARY.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       AGGVARS, Models-3 I/O API.
C
C  REVISION  HISTORY:
C       Prototype 5/1997 by M Houyoux
C
C       Version 11/2001 by Carlie J. Coats, Jr., for I/O API Version 2.1
C
C       Version 11/2002 by Carlie J. Coats, Jr., for I/O API Version 2.2:
C       F90 only.
C       Does not presume all variables are of type M3REAL.
C       Now uses worker routine AGGVARS() instead of multiple entries
C       to routine TAGGREG().
C
C       Version 11/2004 by Carlie J. Coats, Jr., for I/O API Version 3.0:
C       now also supports MIN() operation, per-variable selection of
C       operation; partial de-Houyouxization
C
C       Version 12/2004 with changes by Dr. Michael Bane, U.Manchester, UK:
C       Fixups to make the Intel v8.1 compiler happy (remove duplicate
C       declaration of ASTEPS; change back from F-90 style to F-77-style
C       declarations for MENUITMS and OPNAMES.
C
C       Version 6/2005 by CJC:  improved/bug-fixed default for NRECS
C
C       Version  11/2005 by CJC:  eliminate unused vbles
C
C       Version  7/2006 by CJC:  correct fencepost problem with NRECS
C
C       Version  2/2007:  Bug-fix for All-Variables case. from
C       George Pouliot, US EPA
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 01/2013 by CJC:  use new LASTTIME() to find EDATE:ETIME
C
C       Version 10/2014 by CJC:  Check status of ENV*() calls.
C       PARAMETER menu-arguments.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        INTEGER, PARAMETER ::  M3SUM = 1
        INTEGER, PARAMETER ::  M3AVG = 2
        INTEGER, PARAMETER ::  M3MAX = 3
        INTEGER, PARAMETER ::  M3MIN = 4

        CHARACTER*48, PARAMETER ::  MENUITMS( M3MIN ) =
     &      (/  'Calculate   sum   over time window',
     &          'Calculate average over time window',
     &          'Determine maximum over time window',
     &          'Determine minimum over time window'   /)

        CHARACTER*8, PARAMETER ::  OPNAMES( M3MIN ) =
     &         (/  'SUM', 'BAR', 'MAX', 'MIN'  /)

        CHARACTER*16, PARAMETER :: BLANK16 = ' '
        CHARACTER*16, PARAMETER :: PNAME   = 'M3TPROC'
        CHARACTER*64, PARAMETER :: BAR =
     &'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    ANAME   !  scratch buffer for variable names
        CHARACTER*16    IFILE   !  logical name of the  input file
        CHARACTER*16    OFILE   !  logical name of the output file
        CHARACTER*16    INAME( MXVARS3 ) !  list of  input vble names, from user
        CHARACTER*16    ONAME( MXVARS3 ) !  list of output vble names, from user
        CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
        INTEGER         VTYPE( MXVARS3 ) !  list of vble types
        INTEGER         AGGOP( MXVARS3 ) !  aggregation-operation
        CHARACTER*80    ALINE   !  scratch buffer for prompt
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

        INTEGER         I, N, V !  loop counters (time step #)

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         EDATE   !  final date
        INTEGER         ETIME   !  final time
        INTEGER         AGLEN   !  aggregation-length (hhmmss)
        INTEGER         INSTEP  !  aggregation-length (hhmmss)
        INTEGER         DMAX    !  string length for descriptions
        INTEGER         IOS     !  I/O status
        INTEGER         JDATE   !  current  input date
        INTEGER         JTIME   !  current  input time
        INTEGER         KDATE   !  scratch  date
        INTEGER         KTIME   !  scratch  time
        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         NVSAV   !  number of variables in input file
        INTEGER         NVARS   !  number of vbles in OFILE
        INTEGER         VSIZE   !  volume of one variable
        INTEGER         SDATE   !  starting  input date, from user
        INTEGER         STIME   !  starting  input time, from user
        INTEGER         NRECS   !  duration, output time steps
        INTEGER         TSTEP   !  input time step, from IFILE header
        INTEGER         TSECS   !  tstep in seconds
        INTEGER         OSTEP   !  output time step, from IFILE header
        INTEGER         UMAX    !  string length for units
        INTEGER         ASTEPS  !  aggregation-window duration in TSTEPs
        INTEGER         VMAX    !  string length for names
        INTEGER         ITYPE

        LOGICAL         EFLAG
        LOGICAL         NPFLAG  !  iff no prompting for variables

C.........................................................................
C   begin body of program  M3TPROC

        LOGDEV = INIT3()
        EFLAG  = .FALSE.
        WRITE ( *, '( 5X,  A )' )
     &  ' ',
     &  'Program M3TPROC to sum, average, or find the maximum values',
     &  'over a repeating time period from a selected time window.',
     &  'The time period and starting time window set the start and',
     &  'duration of all subsequent time windows. The program inputs',
     &  'and outputs Models-3 files.',
     &  ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the time period to be copied and the start of the time ',
     &  'period to receive the results.',
     &  ' ',
     &  'USAGE:  m3tproc [INFILE OUTFILE] ',
     &  '(and then answer the prompts).',
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
     &'$Id:: m3tproc.f 47 2014-10-03 22:21:37Z coats                 $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 2 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'usage:  m3tproc [INFILE OUTFILE]', 2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            IFILE = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', PNAME )

        ELSE        !  argcnt 2

            CALL GETARG( 1, ENVBUF )
            IFILE = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( IFILE, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                       // TRIM( IFILE ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            OFILE = ENVBUF( 1:16 )

        END IF


        IF ( .NOT. DESC3( IFILE ) ) THEN
            MESG = 'Could not get description of input file "' //
     &             TRIM( IFILE ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( TSTEP3D .EQ. 0 ) THEN
            MESG = 'Input file "' // TRIM( IFILE ) //
     &             '" is only one time step-no output written.'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            VSIZE = NCOLS3D * NLAYS3D
        ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            VSIZE = NCOLS3D * NROWS3D * NLAYS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            VSIZE = 2 * NTHIK3D * NLAYS3D * ( NCOLS3D + NROWS3D + 2 )
        ELSE
            WRITE( MESG, '( 3A, I5 )' )
     &      'Input file "', TRIM( IFILE ),
     &      '" has unsupported type', FTYPE3D
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NVSAV  = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        INSTEP = TSTEP3D

        CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE,ETIME )

C.......   Get max string-lengths for use in variables-listing:

        VMAX = LEN_TRIM( VNAME3D( 1 ) )
        UMAX = LEN_TRIM( UNITS3D( 1 ) )
        DMAX = LEN_TRIM( VDESC3D( 1 ) )
        DO  V = 1, NVARS3D
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( V ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( V ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( V ) ) )
        END DO


C.......  Determine if all variables are to be used

        NPFLAG = ENVYN( 'M3TPROC_ALLV',
     &                  'true if no prompting for variables to output',
     &                  .FALSE., IOS )
        IF ( IOS .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "M3TPROC_ALLV"' )
        END IF

        AGGOP( 1 ) = ENVINT( 'M3TPROC_TYPE', 'Type of analysis',
     &                       M3MAX, IOS )
        IF ( IOS .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "M3TPROC_TYPE"' )
        END IF
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0,0, 'Environment error(s)', 2 )
        END IF

        IF( NPFLAG ) THEN

            N = NVARS3D
            DO 22 V = 1, NVARS3D
               INAME( V )  = VNAME3D( V )
               ONAME( V )  = VNAME3D( V )
               UNITS( V )  = UNITS3D( V )
               VDESC( V )  = VDESC3D( V )
               VTYPE( V )  = VTYPE3D( V )
               AGGOP( V )  = AGGOP( 1 )
22          CONTINUE

        ELSE

            N = 0
            V = 0

111         CONTINUE        !  loop getting variables-list for extraction

                IF( MOD( N,10 ) .EQ. 0 ) THEN
                    WRITE( *, '( 5X,  A )' )
     &              ' ', 'The list of variables in file "'
     &              // TRIM( IFILE ) // '" is:', ' '
                    WRITE( *,92010 )
     &              ( I,
     &                VNAME3D( I )( 1:VMAX ) // ' (' //
     &                UNITS3D( I )( 1:UMAX ) // '): ' //
     &                VDESC3D( I )( 1:DMAX ), I = 1, NVSAV  )
                    WRITE( *, '( 5X,  A )' )
     &              ' ', 'The list of available operations is', ' '
                   WRITE( *,92010 )
     &              ( I, MENUITMS( I ), I = 1, M3MIN )
                END IF

                CALL M3MSG2( ' ' )
                V = GETNUM( 0, NVSAV, 1 + MOD( V, NVSAV ),
     &     'Enter number for variable to extract (0 to end vbles)' )

                IF ( V .EQ. 0 ) GO TO  199      !  to end of loop

                N = N + 1

C...............  Type of analysis to perform

                AGGOP( N ) = GETNUM( 1, M3MIN, AGGOP( 1 ),
     &                        'Enter type of operation to perform' )

C...............   Optional renaming of this variable:

122             CONTINUE

                    ANAME = TRIM( VNAME3D( V ) )//OPNAMES( AGGOP( N ) )
                    ALINE = 'Enter output-name for this variable [' //
     &                  TRIM( ANAME ) // '] >> '
                    CALL M3PROMPT( ALINE, ANAME, IOS )

                    IF ( IOS .GT. 0 ) THEN
                        CALL M3WARN( PNAME, 0, 0,
     &                  'Error reading output-name; please try again' )
                        GO TO 122
                    END IF

                IF( ANAME .EQ. BLANK16 ) THEN
                    ONAME( N ) = VNAME3D( V )
                ELSE
                    ONAME( N ) = ANAME
                END IF
                INAME( N ) = VNAME3D( V )
                UNITS( N ) = UNITS3D( V )
                VDESC( N ) = VDESC3D( V )
                VTYPE( N ) = VTYPE3D( V )

                IF ( N .LT. MXVARS3 )  GO TO  111   !  to head of loop

199         CONTINUE        !  end loop getting variables-list for analysis

        END IF  ! If prompting or not

        IF ( N .EQ. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'No variables selected', 2 )
        ELSE
            NVARS   = N
            NVARS3D = NVARS
        END IF

C.......   Get starting date and time, and duration:

        PRINT *, 'v=',V, 'n=', NVARS, 'ops=',( AGGOP(I), I=1, NVARS )

        CALL M3MSG2( BAR )
        WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &        'File has start date&time', SDATE3D, ':', STIME3D
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &        'File has final date&time', EDATE, ':', ETIME
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I8.6 )' )
     &        'File has       time step', TSTEP3D
        CALL M3MSG2( MESG )

        SDATE = GETNUM( SDATE3D, 9999999, SDATE,
     &                  'Enter start date for analysis' )

        STIME = GETNUM( 0, 9999999, STIME,
     &                  'Enter start time for analysis' )

        AGLEN = GETNUM( 0, 999999999, 240000,
     &                  'Enter analysis-window duration' )

        OSTEP = GETNUM( 0, 999999999, AGLEN,
     &                  'Enter output time step' )

        NRECS = CURREC( EDATE, ETIME,
     &                  SDATE, STIME, OSTEP, KDATE, KTIME )

        EDATE = GETNUM( SDATE3D, 9999999, KDATE,
     &                  'Enter final date for analysis' )

        ETIME = GETNUM( 0, 9999999, KTIME,
     &                  'Enter final time for analysis' )

        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, AGLEN )
        ASTEPS = CURREC( JDATE, JTIME,
     &                   SDATE, STIME, INSTEP, KDATE, KTIME )

        N     = JSTEP3( EDATE, ETIME,
     &                  SDATE, STIME, OSTEP )
        NRECS = CURREC( EDATE, ETIME,
     &                  SDATE, STIME, OSTEP, KDATE, KTIME )

        IF ( N .EQ. NRECS ) THEN        !!  correct fencepost problem:
            NRECS = NRECS - 1
        END IF


C.......   Build description for the output file, and create accordingly:
C.......   Re-use all but the starting date&time of the input-file description.

        PRINT *, ( AGGOP(V), V=1, NVARS )
        SDATE3D = SDATE
        STIME3D = STIME
        TSTEP3D = OSTEP

        DO  211  V = 1, NVARS

            VNAME3D( V ) = ONAME( V )
            UNITS3D( V ) = UNITS( V )
            VDESC3D( V ) = VDESC( V )
            VTYPE3D( V ) = VTYPE( V )

211     CONTINUE

        IF ( ARGCNT .EQ. 0 ) THEN
            OFILE = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OUTFILE', PNAME )
        ELSE    !  argcnt = 2:
            IF ( .NOT. OPEN3( OFILE, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open output file "' //
     &                 TRIM( OFILE ) // '"'
                CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
            END IF
        END IF      !  if argcnt zero, or 2


C.......   Process this period in the input file:

        JDATE = SDATE
        JTIME = STIME
        WRITE( MESG, '( A, I5, 2X, A, I9.7, A,I6.6 )' )
     &        'Processing', NRECS,
     &        'output steps starting', JDATE, ':', JTIME
        CALL M3MSG2( BAR )
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, I6.6, 2X, A, I3, 2X, A  )' )
     &        'Aggregation period', AGLEN,
     &        '(', ASTEPS, 'input time steps)'
        CALL M3MSG2( MESG )
        PRINT *, ( AGGOP(V), V=1, NVARS )
        CALL M3MSG2( BAR )

        DO  322  I = 1, NRECS

            DO  311  V = 1, NVARS

                ITYPE = AGGOP(V)
                CALL AGGVAR( IFILE, INAME(V), VTYPE(V), VSIZE, ITYPE,
     &                       JDATE, JTIME, INSTEP, ASTEPS,
     &                       OFILE, ONAME(V) )

311         CONTINUE        !  end loop on variables

            CALL NEXTIME( JDATE, JTIME, OSTEP )

322     CONTINUE            !  end loop on analysis periods


        CALL M3EXIT( PNAME, 0, 0,
     &               'Program completed successfully', 0 )


C..............  FORMAT STATEMENTS:  ....................................
C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( 1X , I5, ':  ', A )

        END PROGRAM  M3TPROC

