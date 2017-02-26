
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.  Copyright (C) 2002 MCNC
C All Rights Reserved.
C.........................................................................

        PROGRAM SETENV_TEST

C***********************************************************************
C  program body starts at line  80
C
C  DESCRIPTION:
C       Interactive test of SETENVVAR(), NAMEVAL(), ENVSTR()
C
C  PRECONDITIONS REQUIRED:
C       Follow the prompts.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 5/2002 by Carlie J. Coats, Jr.,
C       MCNC Environmental Modeling Center
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         GETMENU
        LOGICAL         SETENVVAR

        EXTERNAL        GETMENU, SETENVVAR


C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'SETENV_TEST'
        CHARACTER*80       PROGVER
        DATA PROGVER /
     &  '$Id$'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG

        CHARACTER*16    ANAME   !  default value
        CHARACTER*16    VNAME   !  logical name
        DATA            ANAME, VNAME / 'foo', CMISS3 / 

        CHARACTER*512   AVALUE  !  default value
        CHARACTER*512   EVALUE  !  value for logical name
        DATA            AVALUE / '/bar/tux' / 

        INTEGER         MODE
        CHARACTER*72    CHOICES( 3 )
        DATA            CHOICES 
     &       /
     &       'SET VALUE of environment variable',
     &       'GET VALUE of environment variable',
     &       'Exit program'
     &       /  

        INTEGER         ISTAT, LDEV, LMODE

C***********************************************************************
C   begin body of program INTERP_TEST

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program INTERP_TEST for testing INTERP3() and INTERPX().',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the type of test to be done,',
     &'and then the specifics for that test',    
     &' ',
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
     &'Program version: ' // PROGVER, 
     &' ',
     &'Program release tag: $Name$', 
     &' '

        LMODE = 1

11      CONTINUE
        
            MESG  = 'What do you want to do?'
            MODE  = GETMENU( 3, LMODE, MESG, CHOICES )
            LMODE = MODE

            IF ( MODE .EQ. 1 ) THEN

                CALL M3MSG2( 'Setting environment variables' )
                CALL GETSTR( 'Enter logical name', ANAME,  VNAME )
                CALL GETSTR( 'Enter value',        AVALUE, EVALUE )

                IF ( SETENVVAR( VNAME, EVALUE ) ) THEN
                    ANAME  = VNAME
                    AVALUE = EVALUE
                ELSE
                    CALL M3MSG2( 'Error setting environment vble.' )
                END IF
                
            ELSE IF ( MODE .EQ. 2 ) THEN

                CALL M3MSG2( 'Retrieving environment variables' )
                CALL GETSTR( 'Enter logical name', ANAME,  VNAME )
                MESG = 'testing logical name...'

                CALL ENVSTR( VNAME, MESG, VNAME, EVALUE, ISTAT )
                IF ( ISTAT .GT. 0 ) THEN
                    MESG = 'Badly formatted env vble "' // 
     &                      TRIM( VNAME ) // '"'
                    CALL M3MSG2( MESG )
                ELSE IF ( ISTAT .LT. 0 ) THEN
                    MESG = 'Returned default for env vble "' // 
     &                      TRIM( VNAME ) // '"'
                    CALL M3MSG2( MESG )
                    MESG = '"' // TRIM(  VNAME ) // '" = "' // 
     &                            TRIM( EVALUE ) // '"'
                    CALL M3MSG2( MESG )
                ELSE
                    MESG = '"' // TRIM(  VNAME ) // '" = "' // 
     &                            TRIM( EVALUE ) // '"'
                    CALL M3MSG2( MESG )
                    ANAME  = VNAME
                    AVALUE = EVALUE
                END IF

            ELSE IF ( MODE .EQ. 3 ) THEN

                GO TO 99

            ELSE

                CALL M3MSG2 ( 'Unrecognized response' )

            END IF

            GO TO 11
            

99      CONTINUE

C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0, 
     &               'Successful completion of program '//PNAME, 0 )

        END

