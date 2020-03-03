
        PROGRAM  M3PAIR

C***********************************************************************
C Version "$Id: m3pair.f 43 2014-09-12 14:06:19Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  102
C
C  FUNCTION:
C       For a user-specified pair of GRIDDED Models-3 files and
C       pair of variables within them, generate an ASCII file of
C       matched value-pairs, one per line.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O
C
C  REVISION  HISTORY:
C      Prototype 4/1998 by CJC
C       Version 11/2001 by CJC for I/O API Version 2.1
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'M3PAIR'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*16    RNAME   !  logical name of report file
        INTEGER         RDEV    !  report-file unit number

        CHARACTER*16    NAMEA   !  logical name of the first  input file
        CHARACTER*16    NAMEB   !  logical name of the second input file

        INTEGER         NCOLS   !  grid dimensions, from file headers
        INTEGER         NROWS   !  grid dimensions, from file headers
        INTEGER         NLAYS   !  grid dimensions, from file headers
        INTEGER         NTHIK   !  grid dimensions, from file headers
        INTEGER         CLO     !  column range limit
        INTEGER         CHI     !  column range limit
        INTEGER         RLO     !  row    range limit
        INTEGER         RHI     !  row    range limit
        INTEGER         LLO     !  layer  range limit
        INTEGER         LHI     !  layer  range limit
        INTEGER         NVARS1  !  number of vbles in NAMEA
        INTEGER         NVARS2  !  number of vbles in NAMEB
        CHARACTER*16    VNAME1( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS1( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC1( MXVARS3 ) !  list of vble descs
        CHARACTER*16    VNAME2( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS2( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC2( MXVARS3 ) !  list of vble descs
        CHARACTER*16    WNAMES( 2 ) !  list of vble names, from user
        INTEGER         WTYPES( 2 ) !  variable-types
        INTEGER         VTYPE1( MXVARS3 ) !  datatype (must be M3REAL)
        INTEGER         VTYPE2( MXVARS3 )
        INTEGER         SDATE   !  common starting date, from user
        INTEGER         STIME   !  common starting time, from user
        INTEGER         SDATEA  !  File A starting date, from user
        INTEGER         STIMEA  !  File A starting time, from user
        INTEGER         SDATEB  !  File B starting date, from user
        INTEGER         STIMEB  !  File B starting time, from user
        INTEGER         JDATEA  !  File A current date
        INTEGER         JTIMEA  !  File A current time
        INTEGER         JDATEB  !  File B current date
        INTEGER         JTIMEB  !  File B current time
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         TSTEP   !  common time step
        INTEGER         TSTEPA  !  File A time step
        INTEGER         TSTEPB  !  File B time step
        INTEGER         NSTEPS  !  duration in TSTEPs

        INTEGER         I, J, L !  scratch variables
        INTEGER         VMAX    !  string length for names
        INTEGER         UMAX    !  string length for units
        INTEGER         DMAX    !  string length for descriptions

        CHARACTER*256   MESG    !  buffer for m3exit(), etc

C.........................................................................
C   begin body of program  M3PAIR

        LOGDEV = INIT3()
        WRITE ( *,92000 )
     &  ' ',
     &  'Program M3PAIR to construct file of ASCII value pairs for ',
     &  'selected variables from a pair of user-specified GRIDDED ',
     &  'Models-3 files.  You need to have assigned logical names to ',
     &  'the physical file names of both files, according to Models-3',
     &  'conventions, using the operation "setenv <lname> <pname>".',
     &  ' ',
     &  'USAGE:  m3pair [INFILEA INFILEB OUTFILE]',
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
     &'$Id:: m3pair.f 43 2014-09-12 14:06:19Z coats                  $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            NAMEA = PROMPTMFILE( 'Enter logical name for INPUT FILE A',
     &                           FSREAD3, 'INFILEA', PNAME )

            NAMEB = PROMPTMFILE( 'Enter logical name for INPUT FILE B',
     &                           FSREAD3, 'INFILEB', PNAME )

            RDEV = PROMPTFFILE(
     &          'Enter logical name for  OUTPUT FILE',
     &                          .FALSE., .TRUE., 'OUTFILE', PNAME )
            IF ( RDEV .LE. 0 ) RDEV = LOGDEV

        ELSE IF ( ARGCNT .EQ. 3 ) THEN

            CALL GETARG( 1, ENVBUF )
            NAMEA = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( NAMEA, FSREAD3, PNAME ) ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                       'Could not open input file "'
     &                       // TRIM( NAMEA ) // '"', 3 )
            END IF

            CALL GETARG( 2, ENVBUF )
            NAMEB = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( NAMEB, FSREAD3, PNAME ) ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                       'Could not open input file "'
     &                       // TRIM( NAMEB ) // '"', 3 )
            END IF

            CALL GETARG( 3, ENVBUF )
            RNAME = ENVBUF( 1:16 )
            RDEV  = GETEFILE( RNAME, .FALSE., .TRUE., PNAME )
            IF ( RDEV .LT. 0 ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                        'Could not open output file "'
     &                        // TRIM( RNAME ) // '"', 3 )
            END IF          !  if rdev < 0 (getefile() failed)

        ELSE

            MESG = 'usage:  m3pair [INFILEA INFILEB OUTFILE]'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        END IF      !  if argcnt=0, or else 3, or not

        IF ( .NOT. DESC3( NAMEA ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &      'Could not get description of input file ' // NAMEA, 2 )
        END IF

        IF ( FTYPE3D .NE. GRDDED3 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &      'Input file '// NAMEA //'not a GRIDDED file', 2 )
        END IF

        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        NLAYS  = NLAYS3D
        NTHIK  = NTHIK3D
        TSTEPA = TIME2SEC( TSTEP3D )
        NVARS1 = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        SDATEA = SDATE3D
        STIMEA = STIME3D
        RUNLEN = SEC2TIME( MXREC3D * TSTEPA )


C.......   Copy variable-names.  Get max string-lengths for use in
C.......   variables-listing:

        VMAX = LEN_TRIM( VNAME3D( 1 ) )
        UMAX = LEN_TRIM( UNITS3D( 1 ) )
        DMAX = LEN_TRIM( VDESC3D( 1 ) )
        DO  11  I = 1, NVARS3D
            VNAME1( I ) = VNAME3D( I )
            UNITS1( I ) = UNITS3D( I )
            VDESC1( I ) = VDESC3D( I )
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
            VTYPE1( I ) = VTYPE3D( I )
11      CONTINUE


        IF ( .NOT. DESC3( NAMEB ) ) THEN
            MESG = 'Could not get description of input file ' // NAMEB
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.FILCHK3( NAMEB,  GRDDED3,
     &                          NCOLS, NROWS, NLAYS, NTHIK ) ) THEN
            MESG = 'Inconsistent dimensions  for ' // NAMEB
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

C.......   Copy variable-names.  Get max string-lengths for use in
C.......   variables-listing:

        SDATEB = SDATE3D
        STIMEB = STIME3D
        IF ( SDATEA .LT. SDATEB ) THEN
            SDATE = SDATEB
            STIME = STIMEB
        ELSE IF ( SDATEA .LT. SDATEB ) THEN
            SDATE = SDATEA
            STIME = STIMEA
        ELSE
            SDATE = SDATEB
            STIME = MAX( STIMEA, STIMEB )
        END IF

        TSTEPB = TIME2SEC( TSTEP3D )
        RUNLEN = MIN( RUNLEN, SEC2TIME( MXREC3D * TSTEPB ) )
        L      = GCD( TSTEPA, TSTEPB )
        IF ( L .GT. 0 ) THEN
            TSTEP  = SEC2TIME( TSTEPA * ( TSTEPB / L ) )
        ELSE
            TSTEP  = 0
        END IF
        TSTEPA = SEC2TIME( TSTEPA )
        TSTEPB = SEC2TIME( TSTEPB )
        NVARS2 = NVARS3D
        DO  22  I = 1, NVARS3D
            VNAME2( I ) = VNAME3D( I )
            UNITS2( I ) = UNITS3D( I )
            VDESC2( I ) = VDESC3D( I )
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
            VTYPE2( I ) = VTYPE3D( I )
22      CONTINUE


        WRITE( *,92020 )
     &         'The list of variables in file "', TRIM(NAMEA), '" is:',
     &         ( L,
     &           VNAME1( L )( 1:VMAX ) , ' (' ,
     &           UNITS1( L )( 1:UMAX ) , '):  ' ,
     &           VDESC1( L )( 1:DMAX ), L = 1, NVARS1 )
        I = GETNUM( 1, NVARS1, 1, 'Enter number for first variable' )
        WNAMES( 1 ) = VNAME1( I )
        WTYPES( 1 ) = VTYPE1( I )

        IF ( NAMEB .NE. NAMEA ) THEN
            WRITE( *,92020 )
     &         'The list of variables in file "', TRIM(NAMEB), '" is:',
     &          ( L,
     &            VNAME2( L )( 1:VMAX ) , ' (' ,
     &            UNITS2( L )( 1:UMAX ) , '):  ' ,
     &            VDESC2( L )( 1:DMAX ), L = 1, NVARS2 )
        END IF
        J = GETNUM( 1, NVARS1, 1, 'Enter number for second variable' )

        WNAMES( 2 ) = VNAME2( J )
        WTYPES( 2 ) = VTYPE2( J )

C.......   Get mode of operation:

        NSTEPS = -1         !  magic number -- "not yet set"

        IF ( TSTEPA .EQ. 0 ) THEN
            SDATEA  = 0
            STIMEA  = 0
            NSTEPS = 1
        ELSE
            SDATEA = GETNUM( SDATEA, 9999999, SDATEA,
     &          'Enter FILE A starting date (YYYYDDD) for run' )
            STIMEA = GETNUM( 0, 239999, STIMEA,
     &          'Enter FILE A starting time (HHMMSS) for run' )
        END IF          !  time-independent file, or not

        IF ( TSTEPB .EQ. 0 ) THEN
            SDATEB = 0
            STIMEB = 0
            NSTEPS = 1
        ELSE
            SDATEB = GETNUM( SDATEB, 9999999, SDATEB,
     &          'Enter FILE B starting date (YYYYDDD) for run' )
            STIMEB = GETNUM( 0, 239999, STIMEB,
     &          'Enter FILE B starting time (HHMMSS) for run' )
        END IF

        IF ( NSTEPS .EQ. -1 ) THEN  !  "not yet set"

            RUNLEN = GETNUM( 0, 999999999, RUNLEN,
     &                       'Enter duration (HHMMSS) for run' )
            NSTEPS = TIME2SEC( TSTEP )
            NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS

        END IF          !  default or manually-selected analysis

        CLO = GETNUM( 1,   NCOLS, 1,
     &                'Enter min for column range' )
        CHI = GETNUM( CLO, NCOLS, NCOLS,
     &                'Enter MAX for column range' )

        RLO = GETNUM( 1,   NROWS, 1,
     &                'Enter min for row range' )
        RHI = GETNUM( RLO, NROWS, NROWS,
     &                'Enter MAX for row range' )

        IF ( NLAYS .EQ. 1 ) THEN
            LLO = 1
            LHI = 1
        ELSE
            LLO = GETNUM( 1,   NLAYS, 1,
     &                    'Enter min for layer range' )
            LHI = GETNUM( LLO, NLAYS, NLAYS,
     &                    'Enter MAX for layer range' )
        END IF


C.......   Process this period in the input file:

        WRITE( *,92000 ) ' ', 'Processing . . .' , ' '

        IF ( RDEV .LT. 0 ) RDEV = LOGDEV

        JDATEA = SDATEA
        JTIMEA = STIMEA
        JDATEB = SDATEB
        JTIMEB = STIMEB

        DO  233  J = 1, NSTEPS

            CALL PAIRSTEP( NCOLS, NROWS, NLAYS,
     &                     CLO, CHI, RLO, RHI, LLO, LHI,
     &                     JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                     NAMEA, NAMEB,
     &                     WNAMES, WTYPES, RDEV )

            CALL NEXTIME( JDATEA, JTIMEA, TSTEP )
            CALL NEXTIME( JDATEB, JTIMEB, TSTEP )

233         CONTINUE        !  end loop on time steps

        CALL M3EXIT( PNAME, 0, 0,
     &               'M3PAIR  completed successfully', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( 3( I6, ':  ', A, : ) )

92020   FORMAT ( 5X , 3A, /, 100( 1X, I3, 2X, 5A, /  ) )

C...........   Formatted file I/O formats............ 93xxx
C...........   Internal buffering formats............ 94xxx
C...........   Miscellaneous formats................. 95xxx


      END PROGRAM  M3PAIR

