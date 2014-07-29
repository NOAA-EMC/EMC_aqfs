
        PROGRAM  M3TSHIFT

C***********************************************************************
C Version "@(#)$Header$ $Id: m3tshift.f 49 2007-07-06 16:20:50Z coats@borel $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2010 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  88
C
C  FUNCTION:
C       extracts a subset of variables from the input file for a
C       specified time period, and writes them to the output file.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with FORIO:PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM, NEXTIME, STATSTEP, Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 1/1995 by CJC
C       Version   5/1995 by CJC:  command line arguments
C       Modified 10/1999 by CJC:  Fortran standards conformance
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'M3TSHIFT'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file

        INTEGER         SIZE    ! volume of one variable
        INTEGER         SDATE   !  starting  input date, from user
        INTEGER         STIME   !  starting  input time, from user
        INTEGER         TDATE   !  starting output date, from user
        INTEGER         TTIME   !  starting output time, from user
        INTEGER         JDATE   !  current  input date
        INTEGER         JTIME   !  current  input time
        INTEGER         KDATE   !  current output date
        INTEGER         KTIME   !  current output time
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         TSOUT   !  output time step
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NSTEPS  !  duration in TSTEPs
        INTEGER         I       !  loop counter (time step #)

C.........................................................................
C   begin body of program  M3TSHIFT

        LOGDEV = INIT3()
        WRITE ( *, '( 5X, A )' )
     &  ' ',
     &  'Program M3TSHIFT to copy a selected time period from a ',
     &  'Models-3 file to a different time period in a different',
     &  'Models-3 file.',
     &  ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the time period to be copied and the start of the time ',
     &  'period to receive the results.',
     &  ' ',
     &  'USAGE:  m3tshift [INFILE OUTFILE] ',
     &  '(and then answer the prompts).',
     &  ' ',
     &'Program copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.',
     &'and (C) 2002-2010 Baron Advanced Meteorological Systems, LLC',
     &'Released under Version 2 of the GNU General Public License.',
     &'See enclosed GPL.txt, or URL',
     &'http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    coats@baronams.com',
     &'    1021 Main Campus Drive, Suite 300',
     &'    Raleigh, NC 27606',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program version: ',
     &'$Id::                                                         $',
     &'Program release tag: $Name$',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 2 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'usage:  m3tshift [INFILE OUTFILE]', 2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', 'M3XTRACT' )

        ELSE		!  argcnt 2

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                 // TRIM( INAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            ONAME = ENVBUF( 1:16 )

        END IF


        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file "' //
     &             TRIM( INAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            SIZE = NCOLS3D * NLAYS3D * NVARS3D
        ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            SIZE = NCOLS3D * NROWS3D * NLAYS3D * NVARS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            SIZE = 2 * NTHIK3D * NLAYS3D * NVARS3D
     &               * ( NCOLS3D + NROWS3D + 2 )
        ELSE IF ( FTYPE3D .EQ. IDDATA3 ) THEN
            SIZE = 1 + NROWS3D * NVARS3D * ( NLAYS3D + 1 )
        ELSE IF ( FTYPE3D .EQ. PROFIL3 ) THEN
            SIZE = 1 + NROWS3D * ( 7 + NVARS3D * NLAYS3D )
        ELSE IF ( FTYPE3D .EQ. GRNEST3 ) THEN
            SIZE = 1 + NROWS3D * ( 12 + NCOLS3D * NVARS3D )
        ELSE
            WRITE( MESG, 94011 )
     &      'Input file "', TRIM( INAME ),
     &      '" has unsupported type', FTYPE3D
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D


C.......   Get starting date and time, and duration:

        IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

            SDATE  = 0
            STIME  = 0
            TDATE  = 0
            TTIME  = 0
            NSTEPS = 1

        ELSE                            !  time-dependent file

            SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D,
     &               'Enter starting (source) date (YYYYDDD) for run' )
            STIME  = GETNUM( 0, 239999, STIME3D,
     &               'Enter starting (source) time  (HHMMSS) for run' )

            TDATE  = GETNUM( 0, 9999999, SDATE3D,
     &               'Enter target date (YYYYDDD) for run' )
            TTIME  = GETNUM( 0, 239999, STIME3D,
     &               'Enter target time  (HHMMSS) for run' )
            TSOUT  = GETNUM( 0, 999999999, TSTEP,
     &               'Enter output time step (HHMMSS) for run' )

            IF ( TSOUT .NE. 0 ) THEN
                RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
                RUNLEN = GETNUM( 0, 999999999, RUNLEN,
     &                          'Enter duration (HHMMSS) for run' )
                NSTEPS = TIME2SEC( TSTEP )
                NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS
            ELSE
                NSTEPS = 1
            END IF
            IF ( TSOUT .NE. TSTEP ) THEN
                CALL M3WARN( PNAME, 0, 0,
     &          'Input and output time steps are unequal.' )
            END IF

        END IF          !  time-independent file, or not


C.......   Build description for the output file, and create accordingly:
C.......   Re-use all but the starting date&time of the input-file description.

        SDATE3D = TDATE
        STIME3D = TTIME
        TSTEP3D = TSOUT

        IF ( ARGCNT .EQ. 0 ) THEN
            ONAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OUTFILE', PNAME )
        ELSE	!  argcnt = 2:
            IF ( .NOT. OPEN3( ONAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open output file "' //
     &                 TRIM( ONAME ) // '"'
                CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
            END IF
        END IF		!  if argcnt zero, or 2


C.......   Process this period in the input file:

        JDATE = SDATE
        JTIME = STIME
        KDATE = TDATE
        KTIME = TTIME

        DO  322  I = 1, NSTEPS

            CALL TSHIFT( SIZE, JDATE, JTIME, KDATE, KTIME,
     &                   INAME, ONAME, LOGDEV )

            CALL NEXTIME( JDATE, JTIME, TSTEP )
            CALL NEXTIME( KDATE, KTIME, TSOUT )

322     CONTINUE        !  end loop on time steps


        CALL M3EXIT( PNAME, 0, 0,
     &               'Program  M3TSHIFT  completed successfully', 0 )


C..............  FORMAT STATEMENTS:  ....................................

C...........   Informational (LOG) message formats... 92xxx

94010   FORMAT ( 100( A, :, 2X, I5, :, 2X ) )

94011   FORMAT ( 3A, I5 )

        END PROGRAM M3TSHIFT

