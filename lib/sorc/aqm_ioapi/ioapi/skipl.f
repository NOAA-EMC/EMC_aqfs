
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/skipl.f,v 1.2 2000/11/28 21:23:05 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

       SUBROUTINE SKIPL( UNIT, NLINES )

C***********************************************************************
C  subroutine body starts at line  61
C
C  SUBROUTINE:  Skips NLINES number of lines in file UNIT
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/97 by M Houyoux
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: skipl.f,v 1.2 2000/11/28 21:23:05 smith_w Exp $
C
C COPYRIGHT (C) 1996, MCNC--North Carolina Supercomputing Center
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C Environmental Programs Group
C MCNC--North Carolina Supercomputing Center
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C
C env_progs@mcnc.org
C
C Pathname: /env/proj/ioapi/SCCS/s.skipl.f
C Last updated: 11/26/97 13:04:07
C
C****************************************************************************

        IMPLICIT NONE

C.........  Subroutine arguments

        INTEGER    UNIT
        INTEGER    NLINES

C.........  Local variables

        INTEGER    I

C***********************************************************************
C   begin body of subroutine SKIPL


        DO 22 I = 1, NLINES

            READ( UNIT, * )

22      CONTINUE

        RETURN

        END
