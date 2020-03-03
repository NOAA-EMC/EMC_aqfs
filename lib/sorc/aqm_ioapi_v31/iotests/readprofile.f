C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.  Copyright (C) 2002 MCNC
C All Rights Reserved.
C.........................................................................

        PROGRAM PROFIL_TEST

C***********************************************************************
C  program body starts at line  99
C
C  DESCRIPTION:
C       Interactive test of PROFIL3 file types.
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

      INCLUDE 'PROFIL.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER        GETNUM
        LOGICAL        GETYN
        CHARACTER*16   PROMPTMFILE
        CHARACTER*24   DT2STR
        EXTERNAL       DT2STR, GETNUM, GETYN, PROMPTMFILE


C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'INTERP_TEST'
        CHARACTER*80       PROGVER
        DATA               PROGVER / '$Id$' /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    VNAME   !  input data  file logical name
        LOGICAL         EFLAG
        CHARACTER*256   MESG
        CHARACTER*24    DTBUF
        
        INTEGER         NRECS
        INTEGER         JDATE, JTIME, TSTEP

        INTEGER         C, R, L, V, N  !  loop counters
        
C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        L = INIT3()

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
     & '    setenv <output data file>    <path-name>',
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
        
        FDESC3D( 1 ) = 'Test file for PROFIL3 data'
        DO  L = 2, MXDESC3
            FDESC3D( L ) = ' '
        END DO
        
        FNAME = PROMPTMFILE( 'Enter INput file name', FSREAD3, 
     &                       'PROFIL', PNAME )

        IF ( .NOT.DESC3( FNAME ) ) THEN
            MESG = 'Could not get description for file ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( NCOLS3D .NE. NLEVS ) THEN
            MESG = 'Bad max level count in ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( NROWS3D .NE. NSTNS ) THEN
            MESG = 'Bad max site count in ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NRECS = MXREC3D
        JDATE = SDATE3D
        JTIME = STIME3D
        TSTEP = TSTEP3D
        
        WRITE( MESG, '( A, I9 )' )  
     &       'PROCESSING.  Number of input time steps', MXREC3D
        CALL M3MSG2( MESG )

        DO  N = 1, NRECS

           DTBUF = DT2STR( JDATE, JTIME )
           MESG = 'Reading data for ' // DTBUF
           CALL M3MSG2( MESG )

           IF ( .NOT.READ3( FNAME, 'ALL', ALLAYS3, 
     &                      JDATE, JTIME, STNCNT ) ) THEN
                MESG = 'Error reading test profile data'
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
           END IF

           CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO

C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0, 
     &               'Successful completion of program '//PNAME, 0 )

        END

       
