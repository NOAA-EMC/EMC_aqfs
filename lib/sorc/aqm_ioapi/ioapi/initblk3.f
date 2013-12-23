
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/initblk3.f,v 1.2 2000/11/28 21:22:50 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        BLOCK DATA  INITBLK3

C***********************************************************************
C
C  FUNCTION:  initialize I/O state for STATE3 common, Models-3 I/O API
C
C  REVISION  HISTORY:  prototype 3/92 by CJC
C
C***********************************************************************

        IMPLICIT NONE

        INCLUDE  'PARMS3.EXT'
        INCLUDE  'STATE3.EXT'

        DATA  COUNT3 / 0 /
        DATA  LOGDEV / IMISS3  /
        DATA  FINIT3 / .FALSE. /

        END

