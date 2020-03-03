
        PROGRAM  BCWNDW

C***********************************************************************
C Version "$Id: bcwndw.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  75
C
C  FUNCTION:
C       Window a subrectangle of the grid from gridded input file
C       specified time period, and write it to the output file.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with FORIO:PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 5/96 by CJC
C       Modified  9/99 by CJC for enhanced portability
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME   = 'BCWNDW'
        CHARACTER*16, PARAMETER :: BLANK16 = ' '

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  message buffer for M3EXIT(), etc.

        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    WNAME   !  logical name of the output file

        INTEGER         NTHIK   ! boundary thickness dimension
        INTEGER         NCOLS   ! grid dimensions, from INAME header
        INTEGER         NROWS   ! grid dimensions, from INAME header
        INTEGER         NLAYS   ! grid dimensions, from INAME header
        INTEGER         NVARS   !  number of vbles in WNAME
        INTEGER         SDATE   !  starting date, from user
        INTEGER         STIME   !  starting time, from user
        INTEGER         JDATE   !  current date
        INTEGER         JTIME   !  current time
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NSTEPS  !  duration in TSTEPs
        INTEGER         I       !  scratch variables
        INTEGER         LOCOL   !  window boundary
        INTEGER         HICOL   !  window boundary
        INTEGER         LOROW   !  window boundary
        INTEGER         HIROW   !  window boundary

C.........................................................................
C   begin body of program  BCWNDW

        LOGDEV = INIT3()
        WRITE ( LOGDEV,'( 5X , A )' )
     &  ' ',
     &  'Program BCWNDW to construct BOUNDARY CONDITIONs for',
     &  'windowed models on a specified subgrid from "normal" ',
     &  'concentration file output for a specified time period',
     &  'to a specified subgrid and write them to a file.',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the window into the grid and time period to be windowed.',
     &  ' ',
     &  'USAGE:  m3wndw [INFILE OUTFILE]',
     &  '(and then answer the prompts). ',
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
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: bcwndw.f 44 2014-09-12 18:03:16Z coats                  $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', PNAME )

        ELSE IF ( ARGCNT .EQ. 2 ) THEN

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                       'Could not open input file ' // INAME,
     &                       3 )
            END IF

            CALL GETARG( 2, ENVBUF )
            WNAME = ENVBUF( 1:16 )

        ELSE

            CALL M3EXIT( PNAME, 0, 0,
     &                   'usage:  m3wndw [INFILE OUTFILE]', 2 )

        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                  'Could not get description of input file '
     &                  // INAME,
     &                  2 )
        END IF

        IF ( FTYPE3D .NE. GRDDED3 ) THEN

            WRITE( MESG, 94010 )
     &          'Input file "' // TRIM( INAME ) //
     &          '" has type', FTYPE3D,
     &          '(type GRDDED3==1 required)'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )

        END IF

        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        NLAYS  = NLAYS3D
        NVARS  = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D


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

        WRITE ( MESG,94010 )
     &  'Input file "' // TRIM( INAME ) //
     &  '" has grid "' // TRIM( GDNAM3D ) //
     &  '" with', NCOLS3D, 'cols and', NROWS3D, 'rows.'
        WRITE ( *,'( 5X , A )' )
     &  ' ',
     &  TRIM( MESG ),
     &  'Now enter the window specifications.  These will be of the',
     &  'form GRID-NAME, LOCOL, HICOL, LOROW, HIROW, where',
     &  ' ',
     &  '        LOCOL <= col <= HICOL',
     &  '        LOROW <= row <= HIROW',
     &  ' '

        MESG = 'WNDW_' // GDNAM3D
        CALL GETSTR( 'Enter name for windowed grid',
     &               MESG( 1:16 ),  GDNAM3D )

        I = -MIN( NCOLS-1, NROWS-1 ) / 2
        NTHIK   = GETNUM( 1, I, 1, 'Enter boundary thickness NTHIK' )
        NTHIK3D = NTHIK
        FTYPE3D = BNDARY3

        LOCOL   = GETNUM( NTHIK, NCOLS - NTHIK, 1, 'Enter LOCOL' )
        HICOL   = GETNUM( LOCOL, NCOLS - NTHIK, 1, 'Enter HICOL' )
        LOROW   = GETNUM( NTHIK, NROWS - NTHIK, 1, 'Enter LOROW' )
        HIROW   = GETNUM( LOROW, NROWS - NTHIK, 1, 'Enter HIROW' )

        XORIG3D = XORIG3D + DBLE( LOCOL - 1 ) * XCELL3D
        YORIG3D = YORIG3D + DBLE( LOROW - 1 ) * YCELL3D
        NCOLS3D = HICOL - LOCOL + 1
        NROWS3D = HIROW - LOROW + 1

        IF ( ARGCNT .EQ. 0 ) THEN
            WNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OUTFILE', PNAME )
        ELSE    !  argcnt = 2:
            IF ( .NOT. OPEN3( WNAME, FSUNKN3, PNAME ) ) THEN
                CALL M3EXIT( PNAME, SDATE, STIME,
     &                      'Could not open output file ' // WNAME, 2 )
            END IF
        END IF          !  if argcnt zero, or 2


C.......   Process this period in the input file:

        JDATE = SDATE
        JTIME = STIME

        DO  322  I = 1, NSTEPS

            CALL BCSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                   LOCOL, HICOL, LOROW, HIROW, NTHIK,
     &                   JDATE, JTIME, INAME, WNAME, LOGDEV )

            CALL NEXTIME( JDATE, JTIME, TSTEP )

322     CONTINUE        !  end loop on time steps


        CALL M3EXIT( PNAME, 0, 0,
     &               'Program  BCWNDW  completed successfully', 0 )


C..........................   FORMAT STATEMENTS  ....................
C...........   Internal bufferring formats  ......... 94xxx


94010   FORMAT ( 5X, 100( A, :, I7, :, 2X ) )

        END PROGRAM  BCWNDW

