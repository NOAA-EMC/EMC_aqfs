
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER FUNCTION PROMPTDFILE( PROMPT, RDONLY, FMTTED, RECLEN,
     &                                DEFAULT, CALLER )

C***********************************************************************
C  function body starts at line 95
C
C       Prompts user for logical file name, then opens the direct 
C	access Fortran file associated with it, for read-only or not, 
C	formatted or not, with the indicated record length, as
C       indicated by RDONLY and FMTTED.
C    !! WARNING !!  interpretation of RECLEN is MACHINE-DEPENDENT
C
C  RETURNS:
C       unit number for the file opened, or 
C       -1 for failure, 
C       -2 for 'NONE', provided that '"NONE"' occurs within the prompt; or
C       -3 for 'ALL',  provided that '"ALL"'  occurs within the prompt
C
C  PRECONDITIONS REQUIRED:
C       "setenv <lname> <pathname>" for the file before program launch
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETDFILE
C
C  REVISION  HISTORY:
C       prototype 11/95 by CJC
C       Modified   8/96 by CJC:  ! is a comment-designator for input
C       Modified   4/99 by CJC:  turn on/off prompting with environment
C       variable "PROMPTFLAG"
C       Revised 6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C       M3FLUSH to ensure flush() of PROMPT and of log-messages for
C       IRIX F90v7.4  
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*) PROMPT         !  prompt for user
        LOGICAL       RDONLY         !  TRUE iff file is input-only
        LOGICAL       FMTTED         !  TRUE iff file should be formatted
        INTEGER	      RECLEN         !  record length
        CHARACTER*(*) DEFAULT        !  default logical file name
        CHARACTER*(*) CALLER         !  caller-name for logging messages


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         GETDFILE, TRIMLEN
        LOGICAL         ENVYN, GETYN
        EXTERNAL        GETDFILE, ENVYN, GETYN, TRIMLEN


C...........   PARAMETER

        CHARACTER*16    BLANK16, NONE16, ALL16

        PARAMETER     ( BLANK16 = ' ' , 
     &                  NONE16 = 'NONE',
     &                  ALL16  = 'ALL' )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*16    LNAME        !  logical file name
        INTEGER         IOS          !  I/O error status
        INTEGER         IDEV         !  unit number
        INTEGER         PLEN, DLEN   !  trimlen( prompt | default )
        INTEGER         I            !  position at which "!" found
        CHARACTER*512   BUF          !  prompt/environment buffer
        CHARACTER*256   MESG         !  messages
        LOGICAL         AFLAG        !  "ALL"  is in the prompt
        LOGICAL         NFLAG        !  "NONE" is in the prompt

        LOGICAL         PROMPTON     !  Actually prompt or open default
        LOGICAL         FIRSTIME     !  true if first call this execution
        DATA            FIRSTIME / .TRUE. /

        SAVE     FIRSTIME, PROMPTON


C***********************************************************************
C   begin body of function  PROMPTDFILE

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )
            FIRSTIME = .FALSE.

        ENDIF

C.......   Get file name; open input control definition file

        AFLAG = ( INDEX( PROMPT, '"ALL"'  ) .GT. 0 )
        NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

        PLEN  = TRIMLEN( PROMPT  )
        DLEN  = TRIMLEN( DEFAULT )

        IF ( DLEN .GT. 16 ) THEN
            WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &      'Length of DEFAULT "',  DEFAULT( 1:DLEN ) , 
     &      '" exceeds 16; truncating'
            BUF = CALLER( 1 : TRIMLEN( CALLER ) ) // '/PROMPTDFILE'
            CALL M3WARN( BUF, 0, 0, MESG )
            DLEN = 16
        END IF

        IF( PROMPTON ) THEN

            IF ( DLEN + PLEN .GT. 250 ) THEN
                WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &          'Prompt too long; truncating'
                BUF = CALLER( 1 : TRIMLEN( CALLER ) ) // '/PROMPTDFILE'
                CALL M3WARN( BUF, 0, 0, MESG )
                PLEN = 250 - DLEN
            END IF

            BUF = PROMPT ( 1: TRIMLEN( PROMPT  ) ) // ' [' //
     &            DEFAULT( 1: TRIMLEN( DEFAULT ) ) // '] >> '

11          CONTINUE
        
                CALL M3PROMPT( BUF, LNAME, IOS )

                IF ( IOS .NE. 0 ) THEN

                    MESG = 'Could not read your response'
                    WRITE( 6,'( 5X, A )' ) MESG
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Could not read logical name for file'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if could not read response

                I = INDEX( LNAME, '!' )
                IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

                IF ( LNAME .EQ. BLANK16 )  THEN
                    LNAME = DEFAULT
                END IF

                IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
                    PROMPTDFILE = -3
                    RETURN
                ELSE IF ( NFLAG .AND. LNAME .EQ. NONE16 )  THEN
                    PROMPTDFILE = -2
                    RETURN
                END IF
    
                IDEV = GETDFILE( LNAME, RDONLY, FMTTED, RECLEN, CALLER )
                IF ( IDEV .LT. 0 ) THEN     !  failure to open

                    MESG = 'Could not open input file "' //
     &                     LNAME( 1 : TRIMLEN( LNAME ) ) // '".'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Ending program "' //
     &                          CALLER( 1 : TRIMLEN( CALLER ) ) // '".'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if getefile() failed

        ELSE   ! Do not prompt for output

            LNAME = DEFAULT 

            IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
                 PROMPTDFILE = -3
                 RETURN

            ELSE IF ( NFLAG )  THEN

                IF ( LNAME .EQ. NONE16 )  THEN
                    PROMPTDFILE = -2
                    RETURN
                END IF

C           ..  Check if logical name is set in order to permit
C           ..  Study Planner to skip file without having to input "NONE"

                CALL ENVSTR( LNAME, 'Input file name', BLANK16,
     &                       BUF, IOS )

                IF( IOS .LT. 0 ) THEN
                    PROMPTDFILE = -2
                    RETURN
                END IF

            END IF

            IDEV = GETDFILE( LNAME, RDONLY, FMTTED, RECLEN, CALLER )
            IF ( IDEV .LT. 0 ) THEN     !  failure to open
                MESG = 'Could not open input file "' //
     &                  LNAME( 1 : TRIMLEN( LNAME ) ) // '".'
                CALL M3MSG2( MESG )
                MESG = 'Ending program "' //
     &                  CALLER( 1 : TRIMLEN( CALLER ) ) // '".'
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
            END IF

        END IF

        PROMPTDFILE = IDEV
        RETURN

        END

