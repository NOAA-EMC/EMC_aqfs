
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/ckname.f,v 1.2 2000/11/28 21:22:33 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION CKNAME( NAME )

C***********************************************************************
C  function body starts at line  55
C
C  RETURNS:  TRUE iff NAME has content but no embedded blanks.
C
C  PRECONDITIONS REQUIRED:  ASCII character representations
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C  	Prototype 2/97 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)	NAME


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		K, L, M
        CHARACTER*1     CH


C...........   SAVED LOCAL VARIABLES:  alpha, alphanumeric tables

        CHARACTER*56    ALPHA
        DATA		ALPHA
     &          /
     &  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_#@'
     &          /

        CHARACTER*10    NUMER
        DATA		NUMER / '0123456789' /

        CHARACTER*66    ALNUM
        DATA		ALNUM / ' '  /

C***********************************************************************
C   begin body of function  CKNAME

        IF ( ALNUM( 1:1 ) .EQ. ' ' ) THEN
            ALNUM = ALPHA // NUMER
        END IF

        L = LEN( NAME )
        DO  11  K = L, 1, -1
            CH = NAME( K:K )
            IF ( CH .NE. ' ' ) THEN
                GO TO 12
            END IF
11      CONTINUE

C.......   If you get to here:  entire name blank.

        CKNAME = .FALSE.
        RETURN

C.......   Number of trailing blanks found.
C.......   Check rest of naem for legality:

12      CONTINUE

        IF ( INDEX( ALPHA, NAME( 1:1 ) ) .EQ. 0 ) THEN
            CKNAME = .FALSE.
            RETURN
        END IF

        DO  22  M = 2, K
            IF ( INDEX( ALNUM, NAME( M:M ) ) .EQ. 0 ) THEN
                CKNAME = .FALSE.
                RETURN
            END IF
22      CONTINUE

C...........   If you get to here:  entire name OK

        CKNAME = .TRUE.
        RETURN
        END

