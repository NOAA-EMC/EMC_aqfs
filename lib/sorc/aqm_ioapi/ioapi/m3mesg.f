
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/m3mesg.f,v 1.2 2000/11/28 21:22:57 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  M3MESG( MESSAGE )

C***********************************************************************
C  subroutine body starts at line  56
C
C  FUNCTION:  Generate simple (1-line text) messages for Models-3.
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INIT3, TRIMLEN
C
C  REVISION  HISTORY:   
C       prototype 5/92 by CJC
C       Version  10/95 by CJC uses TRIMLEN()
C       Modified  5/98 by CJC for OpenMP thread-safety:  becomes a wrapper
C       around M3MSG2
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER  TRIMLEN  !  string length, exclusive of trailing blanks
        EXTERNAL TRIMLEN


C...........   SAVED LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV
        DATA            LOGDEV / -1 /
        SAVE            LOGDEV


C***********************************************************************
C   begin body of subroutine  M3MESG

	CALL M3MSG2( MESSAGE )


C******************  FORMAT  STATEMENTS   ******************************

C...........   Log message formats..... 92xxx

92000   FORMAT ( /5X , A )

        END

