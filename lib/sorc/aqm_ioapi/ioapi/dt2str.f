
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/dt2str.f,v 1.2 2000/11/28 21:22:40 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        CHARACTER*24 FUNCTION  DT2STR ( JDATE , JTIME )

C***********************************************************************
C  function body starts at line  62
C
C  FUNCTION:  format and return the date and time as a character string
C             "HH:MM:SS  M+ D+, YYYY"
C
C
C  PRECONDITIONS REQUIRED:  valid Julian date YYYYDDD, time HHMMSS
C
C
C  RETURN VALUE:  date & time, as "HH:MM:SS  MMM DD, YYYY"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:  
C        prototype 10/90 by CJC
C
C        Version    2/93 by CJC for CRAY, etc.
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         JDATE   !  Julian date, coded YYYYDDD
        INTEGER         JTIME   !  time, coded HHMMSS


C...........  EXTERNAL FUNCTIONS:

        CHARACTER*10 HHMMSS
        CHARACTER*14 MMDDYY

        EXTERNAL     HHMMSS, MMDDYY



C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       J, T

        CHARACTER*10    TIMBUF
        CHARACTER*24    DATBUF


C***********************************************************************
C   begin body of function  DT2STR

        J = JDATE
        T = JTIME
        CALL NEXTIME( J, T, 0 )
        TIMBUF = HHMMSS( T )
        DATBUF = MMDDYY( J )
        DT2STR = TIMBUF // DATBUF
        RETURN

        END

