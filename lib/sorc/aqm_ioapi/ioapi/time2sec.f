
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/time2sec.f,v 1.2 2000/11/28 21:23:06 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION  TIME2SEC ( TIME )

C***********************************************************************
C  function body starts at line  40
C
C  FUNCTION:  convert time difference format HHMMSS to integer seconds 
C
C  RETURN VALUE:  seconds
C
C  PRECONDITION:  integer TIME formatted HHMMSS
C
C  REVISION  HISTORY:  
C      Prototype  5/92 by CJC
C      Version    2/93 by CJC for CRAY, etc.
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER  	TIME

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER      	ABST


C***********************************************************************
C   begin body of function  TIME2SEC

        IF ( TIME .GE. 0 ) THEN		!  div-mod arithmetic OK
            TIME2SEC = MOD( TIME, 100 )  +  
     &                      60 * ( MOD( TIME / 100, 100 )  +  
     &                             60 * ( TIME / 10000 ) )
        ELSE				!  work with absolute values:
            ABST     = - TIME
            TIME2SEC = - ( MOD( ABST, 100 )  +  
     &                          60 * ( MOD( ABST / 100, 100 )  +  
     &                                 60 * ( ABST / 10000 ) ) )
        END IF

        RETURN

        END

