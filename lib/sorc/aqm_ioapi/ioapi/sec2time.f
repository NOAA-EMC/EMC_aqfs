
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/sec2time.f,v 1.2 2000/11/28 21:23:04 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION  SEC2TIME( SECS )

C***********************************************************************
C  function body starts at line  38
C
C  FUNCTION:  convert integer seconds to time difference format HHMMSS
C
C  RETURN VALUE:  integer HHMMSS formatted secs
C
C  REVISION  HISTORY:  
C       Prototype  5/92 by CJC
C       Version    3/93 by CJC for CRAY, etc.
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER    	SECS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER        	ABSS


C***********************************************************************
C   begin body of function  SEC2TIME

        IF ( SECS .GE. 0 ) THEN		!  div-mod arithmetic OK
            SEC2TIME = MOD( SECS, 60 )  +  
     &                   100 * ( MOD( SECS / 60, 60 )  +  
     &                   100 * ( SECS / 3600 ) )
        ELSE				!  work with absolute values:
            ABSS     = - SECS
            SEC2TIME = - ( MOD( ABSS, 60 )  +  
     &                       100 * ( MOD( ABSS / 60, 60 )  +  
     &                       100 * ( ABSS / 3600 ) ) )
        END IF

        RETURN

        END

