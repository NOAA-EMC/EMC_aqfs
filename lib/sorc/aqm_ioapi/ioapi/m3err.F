
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/m3err.F,v 1.3 2000/11/28 21:13:40 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  M3ERR ( CALLER, JDATE, JTIME, MSGTXT, FATAL )

C***********************************************************************
C
C       DEPRECATED!!
C       Use M3EXIT(), instead.
C
C  subroutine body starts at line  51
C                                    
C  FUNCTION:  Generate simple error messages for Models-3 core;
C             terminate program execution via exit( 2 ) iff FATAL
C
C  PRECONDITIONS REQUIRED:  
C       JDATE:JTIME represented as YYYYDDD:HHMMSS
C
C  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3, SHUT3
C
C  REVISION  HISTORY:	
C	prototype 5/92 by CJC
C	Revised   8/96 to close currently-open POSIX-OK Fortran units.
C       Modified  1/97 by CJC to trim trailing blanks from MSGTXT
C       Modified 10/98 by CJC:  rewritten in terms of m3exit(), m3warn()  
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   CALLER          !  name of the caller
        INTEGER         JDATE, JTIME    !  model date&time for the error
        CHARACTER*(*)   MSGTXT          !  error message
        LOGICAL         FATAL           !  terminate program iff TRUE

                 
C***********************************************************************
C   begin body of subroutine  M3ERR

        IF ( FATAL ) THEN
            CALL M3EXIT( CALLER, JDATE, JTIME, MSGTXT, 2 )
        ELSE    !  not endflag
            CALL M3WARN( CALLER, JDATE, JTIME, MSGTXT )
        END IF
        RETURN

        END

