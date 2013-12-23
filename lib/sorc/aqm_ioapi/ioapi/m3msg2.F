
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  M3MSG2( MESSAGE )

C***********************************************************************
C  Subroutine body starts at line   95
C  Entry  M3PROMPT starts at line  137
C  Entry  M3PARAG  starts at line  166
C  Entry  M3FLUSH  starts at line  198
C
C  FUNCTION:
C       M3MSG2:    Generate simple (1-line text) messages. 
C       M3PROMPT:  Generate prompt-and-response;
C       M3PARAG:   Generate multi-line text messages;
C       M3FLUSH:   wrapper around FLUSH() -- needed for IBM XLF.
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INIT3, TRIMLEN
C
C  REVISION  HISTORY:   
C       Adapted   1/1997 by CJC from M3MESG()
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified 10/1999 by CJC for AIX "flush"
C
C       Modified  5/2003 by CJC for control of optional messages via
C       STATE3 variable STDOUT; changes to critical sections.
C
C       Modified  6/2003 by CJC to deal with IRIX F90v7.4.x failure to
C       properly ANSWER and flush output on program exit:  new entries
C       M3PROMPT used by the GET*() and PROMPT*() routines, and 
C       M3FLUSH for use by user-code.
C
C       Modified  9/2003 by CJC: new entry "M3PARAG"
C
C       Modified 12/2003 by CJC:  Hack "CALL INITBLK3" to deal with 
C       failure of SGI version 7.4 compilers to follow industry standards;
C       re-structuring with INITLOG3() to avoid problems with nested
C       critical sections.
C***********************************************************************

      IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   MESSAGE
        CHARACTER*(*)   ANSWER
        INTEGER         IDEV
        INTEGER         NMESG
        CHARACTER*(*)   MSGS( NMESG )

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         ENVYN   !  get YES/NO environment variable
        INTEGER         INITLOG3
        INTEGER         TRIMLEN !  string length, exclusive of trailing blanks

        EXTERNAL        ENVYN, INITLOG3, TRIMLEN
        EXTERNAL        INITBLK3        !!  initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER		L, M
        INTEGER         ISTAT


C***********************************************************************
C   begin body of subroutine  M3MSG2

        L = TRIMLEN( MESSAGE )

!$OMP CRITICAL( S_LOGDEV )

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

#if defined(__sgi)    || defined(__mips__)

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

            CALL INITBLK3

#endif

            IF ( LOGDEV .LT. 0 ) THEN
                PRINT *, 'Warning:  I/O API not yet initialized'
                PRINT *, MESSAGE( 1:L )
            ELSE
                WRITE( LOGDEV,'( 5X, A )' ) MESSAGE( 1:L )
	        CALL FLUSH( LOGDEV )
            END IF          !!  if logdev < 0 (initialization required)

#ifdef IOAPI_STDOUT
            IF ( LOGDEV .NE. 6 ) THEN
                WRITE( 6,'( 5X, A )' ) MESSAGE( 1:L )
	        CALL FLUSH( 6 )
            END IF		!  if logdev is not pointing at standard output
#endif

!$OMP END CRITICAL( S_LOGDEV )

        RETURN

C***********************************************************************
C   begin body of entry  M3PROMPT

        ENTRY M3PROMPT( MESSAGE, ANSWER, ISTAT )

        L = TRIMLEN( MESSAGE )

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A, $ )' ) MESSAGE( 1:L )
	    CALL FLUSH( LOGDEV )
            ANSWER = ' '
            READ  ( *, '( A )', IOSTAT=ISTAT )  ANSWER

!$OMP   END CRITICAL( S_LOGDEV )

        IF ( ISTAT .LT. 0 ) THEN
            CALL M3EXIT( 'M3PROMPT',0,0,'END-OF-FILE encountered', 1 )
        ELSE IF ( ISTAT .EQ. 0 ) THEN
            L = INDEX( ANSWER, '!' )
            IF ( L .GT. 0 ) ANSWER( L : LEN( ANSWER ) ) = ' '
        END IF

        RETURN

C***********************************************************************
C   begin body of entry  M3PARAG

        ENTRY M3PARAG( NMESG, MSGS )

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3()
            END IF          !!  if logdev < 0 (initialization required)

            DO L = 1, NMESG
                M = TRIMLEN( MSGS( L ) )
                WRITE( LOGDEV,'( 5X, A )' ) MSGS( L )( 1:M )
            END DO

#ifdef IOAPI_STDOUT

            IF ( LOGDEV .NE. 6 ) THEN
                DO L = 1, NMESG
                    M = TRIMLEN( MSGS( L ) )
                    WRITE( 6,'( 5X, A )' ) MSGS( L )( 1:M )
                END DO
            END IF		!  if logdev is not pointing at standard output
#endif

	    CALL FLUSH( LOGDEV )

!$OMP   END CRITICAL( S_LOGDEV )

        RETURN

C***********************************************************************
C   begin body of entry  M3FLUSH

        ENTRY M3FLUSH( IDEV )
!$OMP CRITICAL( S_LOGDEV )
	CALL FLUSH( IDEV )
!$OMP END CRITICAL( S_LOGDEV )

        END

