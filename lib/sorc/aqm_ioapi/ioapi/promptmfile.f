
C.........................................................................
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C 2003 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        CHARACTER*16 FUNCTION PROMPTMFILE( PROMPT, FMODE, 
     &                                     DEFAULT, CALLER )

C***********************************************************************
C  function body starts at line 99
C
C       If environment variable PROMPTFLAG is 'Y', returns DEFAULT.
C
C       Prompts user for logical file name, then opens the I/O API file
C       associated with it, using the indicated file mode (FSREAD3, 
C       FSRDWR3, FSNEW3, FSUNKN3)
C
C       Provided that '"NONE"'occurs within the prompt, if name entered 
C       is 'NONE', does not attempt to open the file (but still returns 
C       'NONE' as the function value).
C
C  RETURNS:
C       logical name of file opened
C
C  PRECONDITIONS REQUIRED:
C       "setenv <lname> <pathname>" for the file before program launch
C       file description set in FDESC3.EXT structures if appropriate
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETYN, TRIMLEN, OPEN3
C
C  REVISION  HISTORY:
C       prototype 6/1995 by CJC
C	Revised  10/1995 by CJC:  more robust treatment of 'NONE'
C       Modified  8/1996 by CJC:  ! is a comment-designator for input
C       Modified  8/1997 by MH:   environment variable PROMPTFLAG
C       Revised   6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C       M3FLUSH to ensure flush() of PROMPT and of log-messages for
C       IRIX F90v7.4  
C       Revised   7/2003 by CJC:  clean up LUNIT=INIT3() and
C       FIRST-TIME logic
C***********************************************************************

      IMPLICIT NONE

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*) PROMPT         !  prompt for user
        INTEGER       FMODE          !  file opening-mode
        CHARACTER*(*) DEFAULT        !  default logical file name
        CHARACTER*(*) CALLER         !  caller-name for logging messages


C...........   PARAMETER

        CHARACTER*16    BLANK16
        CHARACTER*16    NONE16

        PARAMETER     ( BLANK16 = ' ' , 
     &                  NONE16  = 'NONE' )


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         TRIMLEN
        LOGICAL         ENVYN
        LOGICAL         GETYN
        EXTERNAL        ENVYN, GETYN, TRIMLEN


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I            !  position at which "!" found
        CHARACTER*16    LNAME        !  logical file name
        INTEGER         IOS          !  I/O status
        CHARACTER*120   MESG         !  messages
        CHARACTER*512   BUFFER       !  prompt/environment buffer
        LOGICAL         NFLAG        !  "NONE" is in the prompt

 
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         LUNIT        !  log-file unit number
        DATA            LUNIT    / IMISS3 /
        LOGICAL         PROMPTON     !  Actually prompt or open default

        SAVE            LUNIT, PROMPTON
 
C***********************************************************************
C   begin body of function  PROMPTMFILE

        IF( LUNIT .LT. 0 ) THEN
 
            LUNIT = INIT3()

            CALL M3MSG2( BLANK16 )
            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )

         END IF         !  if firstime:  lunit < 0


C.......   Decide whether 'NONE' is a valid response

        NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

        IF( PROMPTON ) THEN

C.......   Construct actual prompt; Loop:  get file name until file opens
        
            BUFFER = PROMPT ( 1: TRIMLEN( PROMPT  ) ) // ' [' //
     &               DEFAULT( 1: TRIMLEN( DEFAULT ) ) // '] >> '

11          CONTINUE
       
                LNAME = ' '
                CALL M3PROMPT( BUFFER, LNAME, IOS )

                IF ( IOS .NE. 0 ) THEN

                    MESG = 'Could not read your response'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Could not read logical name for file'
                        CALL M3EXIT( 'CTLAMAT', 0, 0, MESG, 2 )
                    END IF

                END IF      !  if could not read response

                I = INDEX( LNAME, '!' )
                IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

                IF ( LNAME .EQ. BLANK16 ) THEN
                    LNAME = DEFAULT
                END IF

                IF ( NFLAG .AND. ( LNAME .EQ. NONE16 ) ) THEN
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

                IF ( .NOT. OPEN3( LNAME, FMODE, CALLER ) ) THEN !  failure to open

                    MESG = 'Could not open file "' //
     &                     LNAME( 1 : TRIMLEN( LNAME ) ) // '".'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Ending program "' //
     &                          CALLER( 1 : TRIMLEN( CALLER ) ) // '".'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if open3() failed

        ELSE  ! Do not prompt for output

            LNAME = DEFAULT

            IF ( NFLAG )  THEN
 
                IF( LNAME .EQ. NONE16      ) THEN
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

C           ..  Check if logical name is set in order to permit
C           ..  Study Planner to skip file without having to input "NONE"
 
                CALL ENVSTR( LNAME, 'Input file name', ' ',
     &                       BUFFER, IOS )
 
                IF( IOS .LT. 0 ) THEN   ! either not set (-2) or empty (-1)
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

            END IF              !  if nflag (checking for "none"

            IF ( .NOT. OPEN3( LNAME, FMODE, CALLER ) ) THEN !  failure to open
 
                MESG = 'Could not open file "' //
     &                 LNAME( 1 : TRIMLEN( LNAME ) ) // '".'
                CALL M3MSG2( MESG )
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )

            END IF      !  if open3() failed

        ENDIF

        PROMPTMFILE = LNAME
        RETURN

        END

