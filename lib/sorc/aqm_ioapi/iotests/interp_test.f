
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.  Copyright (C) 2002 MCNC
C All Rights Reserved.
C.........................................................................

        PROGRAM INTERP_TEST

C***********************************************************************
C  program body starts at line  99
C
C  DESCRIPTION:
C       Interactive test of INTERP3() and INTERPX().
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input file.
C       Follow the prompts.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 4/2002 by Carlie J. Coats, Jr.,
C       MCNC Environmental Modeling Center
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER        GETMENU, GETNUM, SEC2TIME, TIME2SEC
        LOGICAL        GETYN
        CHARACTER*16   PROMPTMFILE

        EXTERNAL       GETMENU, GETNUM, GETYN, PROMPTMFILE, 
     &                 SEC2TIME, TIME2SEC


C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'INTERP_TEST'
        CHARACTER*80       PROGVER
        DATA PROGVER /
     &  '$Id$'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    VNAME   !  input data  file logical name
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        INTEGER         C, R, L, V, N  !  loop counters

        CHARACTER*6     GDNAM      ! grid name
        INTEGER         NCOLS      ! number of grid columns
        INTEGER         NROWS      ! number of grid rows
        INTEGER         NLAYS      ! number of layers
        INTEGER         NVARS      ! number of variables
        INTEGER         NSIZE      ! number of grid cells (total)
        
        INTEGER         JDATE, JTIME
        INTEGER         SDATE, STIME, TSTEP, HSTEP

        INTEGER         COLA, COLZ
        INTEGER         ROWA, ROWZ
        INTEGER         LAYA, LAYZ
        
        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status
        
        CHARACTER*72    VBLES( MXVARS3 )

        INTEGER         MODE
        CHARACTER*72    CHOICES( 2 )
        DATA            CHOICES 
     &       /
     &       'Perform test of INTERP3()',
     &       'Perform test of INTERPX()'
     &       /  

        REAL, ALLOCATABLE::   BUFFER( :, :, : )


C***********************************************************************
C   begin body of program INTERP_TEST

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program INTERP_TEST for testing INTERP3() and INTERPX().',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical name of the input',
     &'data file, and repeatedly for the date&time and type of the',
     &'test to be performed.',
     &' ',
     &'Default responses are indicated in square brackets [LIKE THIS]',
     &'[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <input data  file>    <path-name>',
     & '    Input file has type "GRIDDED"',
     &' ',
     &'Program copyright (C) 2002 MCNC and released under Version 2',
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
            CALL M3EXIT( PNAME, 0, 0, 
     &                   'Program terminated at user request', 2 )
        END IF
        
        FNAME = PROMPTMFILE( 'Enter name for file to use for testing',
     &                       FSREAD3, 'INFILE', PNAME )
        
        IF ( .NOT. DESC3( FNAME ) ) THEN
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
            MESG = 'File type not GRIDDED'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
        NVARS = NVARS3D
        NSIZE = NCOLS * NROWS * NLAYS
        SDATE = SDATE3D
        STIME = STIME3D
        TSTEP = TSTEP3D
        HSTEP = SEC2TIME( TIME2SEC( TSTEP ) / 2 )
        
        DO V = 1, NVARS
            VBLES(V) = VNAME3D( V ) // '(' // TRIM( UNITS3D( V ) )
     &                 // '): ' // VDESC3D( V )
        END DO
        
        MODE = GETMENU( 2, 1, 'Enter type of test', CHOICES )
        
        IF ( MODE .EQ. 1 ) THEN
            ALLOCATE( BUFFER( NCOLS, NROWS, NLAYS ), STAT = STATUS )
        ELSE
            C    = NCOLS/4
            COLA = GETNUM( 1,    NCOLS, C, 'Enter window COLA' )
            C    = MAX( COLA, 3*NCOLS/4 )
            COLZ = GETNUM( COLA, NCOLS, C, 'Enter window COLZ' )
            R    = NROWS/4
            ROWA = GETNUM( 1,    NROWS, R, 'Enter window ROWA' )
            R    = MAX( ROWA, 3*NROWS/4 )
            ROWZ = GETNUM( ROWA, NROWS, R, 'Enter window ROWZ' )
            L    = 1
            LAYA = GETNUM( 1,    NLAYS, L, 'Enter window LAYA' )
            L    = MAX( LAYA, 1 )
            LAYZ = GETNUM( LAYA, NLAYS, L, 'Enter window LAYZ' )
            ALLOCATE( BUFFER( COLA:COLZ, ROWA:ROWZ, LAYA:LAYZ ), 
     &                      STAT = STATUS )
        END IF          !  if MODE=1, or not

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )  
     &               'Buffer allocation failed:  STAT=', STATUS
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

C...............  Loop on tests

        JDATE = SDATE
        JTIME = STIME
        V     = 1
        CALL NEXTIME( JDATE, JTIME, HSTEP )
111     CONTINUE

            JDATE = GETNUM( 0, 9999999, JDATE, 
     &                     'Enter DATE for test or 0 to quit' )

            IF ( JDATE .EQ. 0 ) THEN
                GO TO  999
            END IF

            JTIME = GETNUM( 0, 9999999, JTIME, 'Enter TIME for test' )
            
            V = GETMENU( NVARS, V, 'Choose variable for test', VBLES )
            VNAME = VNAME3D( V )
            
            IF ( MODE .EQ. 1 ) THEN

                IF ( INTERP3( FNAME, VNAME, PNAME,
     &                        JDATE, JTIME, NSIZE, BUFFER ) ) THEN
                    CALL M3MSG2( 'INTERP3() succeeded' )
                ELSE
                    MESG = 'Could not INTERP3 variable "' // 
     &                     TRIM( VNAME ) //
     &                     '" from "' // TRIM( FNAME ) // '"'
                    CALL M3WARN( PNAME, JDATE, JTIME, MESG )
                END IF

            ELSE

                IF ( INTERPX( FNAME, VNAME, PNAME,
     &                        COLA, COLZ, ROWA, ROWZ, LAYA, LAYZ,
     &                       JDATE, JTIME, BUFFER ) ) THEN
                    CALL M3MSG2( 'INTERPX() succeeded' )
                ELSE
                    MESG = 'Could not INTERPX variable "' // 
     &                     TRIM( VNAME ) //
     &                     '" from "' // TRIM( FNAME ) // '"'
                    CALL M3WARN( PNAME, JDATE, JTIME, MESG )
                END IF

            END IF              !  if mode = 1, or not
            
            GO TO  111

999     CONTINUE                !  exit from loop

C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0, 
     &               'Successful completion of program '//PNAME, 0 )

        END

