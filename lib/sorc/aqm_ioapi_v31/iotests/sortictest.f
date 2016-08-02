
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.  Copyright (C) 2002 MCNC
C All Rights Reserved.
C.........................................................................

        PROGRAM SORTIC_TEST

C***********************************************************************
C  program body starts at line  80
C
C  DESCRIPTION:
C       Interactive test of SORTIC()
C
C  PRECONDITIONS REQUIRED:
C       Follow the prompts.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 7/2002 by Carlie J. Coats, Jr.,
C       MCNC Environmental Modeling Center
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL        GETYN
        INTEGER        PROMPTFFILE
        EXTERNAL       GETYN, PROMPTFFILE


C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'SORTIC_TEST'
        CHARACTER*80       PROGVER
        DATA PROGVER /
     &  '$Id$'
     &  /

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         IDEV
        INTEGER         N, NWORDS
        CHARACTER*64    LINE
        CHARACTER*256   MESG
        INTEGER         ISTAT, LDEV, LMODE
        
        CHARACTER*80, ALLOCATABLE::  WORDS( : )
        INTEGER,      ALLOCATABLE::   INDX( : )


C***********************************************************************
C   begin body of program INTERP_TEST

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program SORTIC_TEST to test sorting routine SORTIC(), using',
     &'a user-supplied iput data file.  That file should contain the',    
     &'set of test data to be sorted.',
     &' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv  <input test-data  file>  <path-name>',
     & '    Input data lines have at most 64 characters',
     & ' ',
     &'Default responses are indicated in square brackets [LIKE THIS]',
     &'[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
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
     &'Program version: $Id$', 
     &' ',
     &'Program release tag: $Name$', 
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 
     &                   'Program terminated at user request', 2 )
        END IF
        

C...............  Open and get description for input data file

        IDEV = PROMPTFFILE( 'Enter logical name of test-data file', 
     &                      .TRUE., .TRUE., 'INFILE', PNAME )
        IF ( IDEV .LT. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 
     &                   'Failure opening test-data file', 2 )
        END IF


C...............  Count number of lines in test-data file

        CALL M3MSG2( 'Counting input...' )
        N = 0
11      CONTINUE

            READ( IDEV, '( A )', END=12, IOSTAT=ISTAT ) LINE
            N = N + 1
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 1X, A, I10, 1X, A )' ) 
     &          'Error', ISTAT, 'at line', N, 'of input file'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        GO TO 11
12      CONTINUE
        NWORDS = N
        WRITE( MESG, '( A, I10 )' ) 'Number of input lines:', N
        CALL M3MSG2( MESG )
        REWIND( IDEV )
        

C...............  Allocate input buffers

        ALLOCATE( WORDS( N ), INDX( N ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10)' )  
     &               'Buffer allocation failed:  STAT=', ISTAT
            CALL M3EXIT( 'CPLTEST', 0, 0, MESG, 2 )
        END IF


C...............  Read and store contents of test-data file

        CALL M3MSG2( 'Reading input...' )
        DO N = 1, NWORDS
            READ( IDEV, '( A )', IOSTAT=ISTAT ) WORDS( N )
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 1X, A, I10, 1X, A )' ) 
     &          'Error', ISTAT, 'at line', N, 'of input file'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
            INDX( N ) = N
        END DO


C...............  Sort and output contents of test-data file

        CALL M3MSG2( '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')
        CALL M3MSG2( 'Sorting...' )
        CALL SORTIC( NWORDS, INDX, WORDS )
        CALL M3MSG2( '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')
        CALL M3MSG2( 'Sorted output:' )
        
        DO N = 1, NWORDS
            WRITE( MESG, '( I4, 3A )' ) 
     &          N, ': "', TRIM( WORDS( INDX( N ) ) ), '"'
            CALL M3MSG2( MESG )
        END DO


C...............  Completion

        MESG = 'Successful completion of program ' // PNAME
        CALL M3EXIT( PNAME, 0, 0, MESG, 0 )

        END

