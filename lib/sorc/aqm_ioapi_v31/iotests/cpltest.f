
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 IOTESTS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems, LLC.
C See file "GPL.txt" for conditions of use.
C.........................................................................

        PROGRAM CPLTEST

C***********************************************************************
C  program body starts at line  158
C
C  DESCRIPTION:
C       Initialize from initialization file, and create new output
C       file with the same description.
C       For each time step in the specified time step sequence,
C       reads one variable from the specified input file, and
C       write it to the specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input, 
C       output, and GRIDDESC files.
C       Input file and output grid use the same coordinate system.
C       Specified time step sequence is valid for both the input and
C       synch files.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype  7/2002 by Carlie J. Coats, Jr., NCSC EMC
C
C       Modified  11/2003 by CJC:  removed redundant re-declaration of
C       FILCHK3
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER*24   DT2STR
        INTEGER        GETNUM
        LOGICAL        GETYN
        CHARACTER*16   PROMPTMFILE

        EXTERNAL       DT2STR, GETNUM, GETYN, PROMPTMFILE


C...........   PARAMETERS and their descriptions:

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &  '$Id$'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  init  data  file logical name
        CHARACTER*16    INAME   !  input data  file logical name
        CHARACTER*16    ONAME   !  output data file logical name
        CHARACTER*16    VNAME   !  input/output variable name

        LOGICAL         EFLAG
        CHARACTER*256   MESG

        INTEGER         LDEV    !  log-device
        INTEGER         STATUS  !  allocation-status

        INTEGER         NCOLS   ! number of grid columns
        INTEGER         NROWS   ! number of grid rows
        INTEGER         NLAYS   ! number of layers
        INTEGER         NTHIK   ! bdy thickness
        INTEGER         FTYPE   ! bdy thickness

        INTEGER         JDATE
        INTEGER         JTIME
        INTEGER         TSTEP
        INTEGER         NRECS, N

        REAL,    ALLOCATABLE::   BUF( :, :, : )


C***********************************************************************
C   begin body of program CPLTEST

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'Program CPLTEST to read all variables in each time step in ',
     & 'the specified time step sequence from the specified input',
     & 'file, optionally under the control of the specified',
     & 'synchronization file, copy or interpolate them to the ',
     & 'output grid, and write them to the specified output file.',
     & ' ',
     & 'THE PROGRAM WILL PROMPT YOU for the logical names of the',
     & 'input data file, the input synch file, and the output file,',
     & 'the time step sequence, and the GRIDDESC name of the output',
     & 'grid.  Default responses are indicated in square brackets',
     & '[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
     & 'If you wish to copy time steps, instead of interpolate them,',
     & 'respond "SAME" to the prompt for output grid name.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <init   data  file>    <path-name>',
     & '    setenv <input  data  file>    <path-name>',
     & '    setenv <output data  file>    <path-name>',
     & ' ',
     &'See URL  http://www.emc.mcnc.org/products/ioapi/AA.html#tools',
     &' ',
     &'Program copyright (C) 2001 MCNC and released under Version 2',
     &'of the GNU General Public License.  See enclosed GPL.txt, or',
     &'URL  http://www.gnu.org/copyleft/gpl.html',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    envpro@emc.mcnc.org',
     &' ',
     &'    MCNC -- Environmental Modeling Center',
     &'    3021 Cornwallis Rd    P. O. Box 12889',
     &'    Research Triangle Park, NC 27709-2889',
     &' ',
     &'Program version: ' // PROGVER, 
     &' ',
     &'Program release tag: $Name$', 
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( 'CPLTEST', 0, 0, 
     &                   'Program terminated at user request', 2 )
        END IF
        

C...............  Open and get description for input data file

        FNAME = PROMPTMFILE( 'Enter INIT FILE logical name', FSREAD3, 
     &                       'INITFILE', 'CPLTEST' )
        IF ( .NOT. DESC3( FNAME ) ) THEN
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
            MESG = 'INIT FILE not of type GRIDDED'
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        END IF
        
        NCOLS   = NCOLS3D
        NROWS   = NROWS3D
        NLAYS   = NLAYS3D
        NTHIK   = NTHIK3D
        FTYPE   = FTYPE3D
        JDATE   = SDATE3D
        JTIME   = STIME3D
        TSTEP   = TSTEP3D
        VNAME   = VNAME3D( 1 )
        NVARS3D = 1

C...............  Open input and output file with one variable, and
C...............  the same file description otherwise

        ONAME = PROMPTMFILE( 'Enter OUTPUT FILE logical name', FSNEW3, 
     &                       'OUTFILE', 'CPLTEST' )

        INAME = PROMPTMFILE( 'Enter INPUT FILE logical name', FSREAD3, 
     &                       'INFILE', 'CPLTEST' )

        IF ( .NOT.FILCHK3( INAME, 
     &                     FTYPE, NCOLS, NROWS, NLAYS, NTHIK ) ) THEN	!  set up for copy
	
	    MESG = 'INPUT FILE description mismatch'
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        END IF

        NRECS = GETNUM( 1, 9999999, MXREC3D, 
     &                  'Enter  NRECS  for time step sequence' )


C...............  Allocate buffer.

        ALLOCATE( BUF( NCOLS, NROWS, NLAYS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10)' )  
     &               'Buffer allocation failed:  STAT=', STATUS
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        END IF


C...............  Read initialization data:

        IF ( .NOT.READ3( FNAME, VNAME, ALLAYS3, 
     &                   JDATE, JTIME, BUF ) ) THEN
            MESG = 'Could not read "' // TRIM( VNAME ) //
     &                     '" from "' // TRIM( FNAME ) // '"'
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        END IF



C...............  Process output time step sequence

        DO  N = 1, NRECS

            CALL NEXTIME( JDATE, JTIME, TSTEP )

            MESG = 'Writing ' // DT2STR( JDATE , JTIME )
            CALL M3MSG2( MESG )
            IF ( .NOT.WRITE3( ONAME, VNAME,
     &                        JDATE, JTIME, BUF ) ) THEN
                MESG = 'Could not write "' // TRIM( VNAME ) //
     &                            '" to "' // TRIM( ONAME ) // '"'
                CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
            END IF

            MESG = 'Reading ' // DT2STR( JDATE , JTIME )
            CALL M3MSG2( MESG )
            IF ( .NOT. READ3( INAME, VNAME, ALLAYS3, 
     &                        JDATE, JTIME, BUF ) ) THEN
                MESG = 'Could not read "' // TRIM( VNAME ) //
     &                         '" from "' // TRIM( INAME ) // '"'
                CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
            END IF

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( 'CPLTEST', 0, 0, 
     &               'Successful completion of program', 0 )

        END

