
        SUBROUTINE POLMESG( NLIST, NAMES )

C***********************************************************************
C  subroutine body starts at line 
C
C  DESCRIPTION:
C      This subroutine writes out a message stating that the pollutants or
C      emission types in the argument list are being processed.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C      Subroutines: I/O API subroutines
C
C  REVISION  HISTORY:
C      Created 3/99 by M. Houyoux
C
C************************************************************************
C
C Project Title: EDSS Tools Library
C File: @(#)$Id: polmesg.f,v 1.8 2004/06/18 17:21:56 cseppan Exp $
C
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C
C smoke@unc.edu
C
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/polmesg.f,v $
C Last updated: $Date: 2004/06/18 17:21:56 $ 
C
C***************************************************************************

        IMPLICIT NONE

C...........   INCLUDES
        INCLUDE 'IOCNST3.EXT'   !  emissions constant parameters

C...........   EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER(2) CRLF
        EXTERNAL     CRLF

C.........  SUBROUTINE ARGUMENTS
        INTEGER     , INTENT (IN) :: NLIST          !  no. of pols or emis types
        CHARACTER(*), INTENT (IN) :: NAMES( NLIST ) !  pollutant names

C...........   Other local variables
        INTEGER       I, J, L0, L1, L2
        INTEGER       LCNT              ! length count

        CHARACTER(5120) :: MESG         !  message buffer
        CHARACTER(20)   :: SPACE = ' '

        CHARACTER(16) :: PROGNAME = 'POLMESG' !  program name

C***********************************************************************
C   begin body of subroutine POLMESG

C.........  Set up initial message. 
        MESG = 'Processing data for:' 

        L0 = LEN_TRIM( MESG )

        L1 = LEN_TRIM( NAMES( 1 ) )
        IF( NAMES( 1 ) .NE. ' ' ) THEN
            MESG = MESG( 1:L0 ) // ' "' // NAMES( 1 )( 1:L1 ) // '"'
        END IF

C.........  Initialize length of initial message
        LCNT = L0 + L1 + 4
        DO I = 2, NLIST

            L1 = LEN_TRIM( MESG )
            IF( NAMES( I ) .EQ. ' ' ) CYCLE
            L2 = LEN_TRIM( NAMES( I ) )

            LCNT = LCNT + L2 + 4

            IF( LCNT .GT. 74 ) THEN
                LCNT = L0 + L2 + 4
                MESG = MESG( 1:L1 ) // ',' // CRLF() // BLANK5 // 
     &                 SPACE // '"' // NAMES( I )( 1:L2 ) // '"'

            ELSE

                MESG = MESG( 1:L1 )// ', "'// NAMES( I )( 1:L2 )// '"'

            END IF

        END DO

C........  Ensure that M3MSG2 is called with 256 characters or
C          less, even if MESG is longer than that.  This will put
C          unneeded spaces in the message.
        L1 = LEN_TRIM( MESG )
        DO I = 1, L1, 256

            CALL M3MSG2( MESG( I:MIN( I+255,L1 ) ) )

        END DO

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93030   FORMAT( I8.8 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I8, :, 1X ) )

        END SUBROUTINE POLMESG
