
        SUBROUTINE CHECKMEM( MSTATUS, ONVAR, CALLER )
 
C***********************************************************************
C  subroutine body starts at line  105
C
C  DESCRIPTION:
C       Reports an error and exits if memory status flag is non-zero.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Adapted 10/98 by M Houyoux
C
C***********************************************************************
C
C Project Title: EDSS Tools Library
C File: @(#)$Id: checkmem.f,v 1.4 2004/06/18 17:19:31 cseppan Exp $
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
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/checkmem.f,v $
C Last updated: $Date: 2004/06/18 17:19:31 $ 
C
C***************************************************************************
 
      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

       INTEGER       MSTATUS !  ALLOCATE function exit status
       CHARACTER(*)  ONVAR   !  Variable name of previous ALLOCATE statement
       CHARACTER(*)  CALLER  !  Name of calling program

C...........   ARGUMENTS and their descriptions:
       INTEGER      TRIMLEN
       EXTERNAL     TRIMLEN

C...........   Local variables

       INTEGER         L1
       INTEGER         L2
       CHARACTER(256)  MESG

       CHARACTER(16) :: PROGNAME = 'CHECKMEM' ! program name

C***********************************************************************
C   begin body of function CHECKMEM

C.........  Get lengths of input character strings
        L1 = TRIMLEN( ONVAR )
        L2 = TRIMLEN( CALLER )

C.........  Abort if memory status is non-zero

        IF( MSTATUS .GT. 0 ) THEN            
            MESG = 'Failure allocating memory for "' // ONVAR( 1:L1 ) //
     &             '" variable'
            CALL M3EXIT( CALLER( 1:L2 ), 0, 0, MESG, 2 )
        ENDIF

        RETURN

        END

