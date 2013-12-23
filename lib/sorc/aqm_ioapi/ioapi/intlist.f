
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION INTLIST( ENAME, EDESC, NMAX, NCNT, LIST )

C***********************************************************************
C  function body starts at line  57
C
C  RETURNS:  TRUE for success, FALSE for failure
C            Success implies NCNT > 0 ("we actually found something")
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <quoted, comma-delimited list of integers>
C       string-length( <list> <= 511
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       ENVINT, M3EXIT, STR2INT
C
C  REVISION  HISTORY:
C       prototype 04/15/1998 by Carlie J. Coats, Jr., NCSC
C       Revised   02/09/1999 by CJC:  NCNT <= 0:  failure
C       Revised   02/11/2002 by CJC:  Deal with values "LIST:<list>"
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   ENAME   !  in:  environment variable for the list
        CHARACTER*(*)   EDESC   !  in:  environment variable description
        INTEGER         NMAX    !  in:  dimension for list
        INTEGER         NCNT    ! out:  actual number of entries in list
        INTEGER         LIST( NMAX )    ! out:  array of values found    

C...........   EXTERNAL FUNCTION:

        INTEGER         STR2INT
        EXTERNAL        STR2INT

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*512   BUF     !  buffer for environment-variable value
        CHARACTER*256   MSG     !  buffer for environment-variable value
        CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
        INTEGER         ISTAT   !  return status for ENVSTR
        INTEGER         L       !  subscript/loop counter
        INTEGER         LO, HI  !  substring bounds

C***********************************************************************
C   begin body of function  dummy

        CALL ENVSTR( ENAME, EDESC, ' ', BUF, ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            MSG = 'Could not get environment variable "'// ENAME// '"'
            CALL M3MSG2( MSG )
            INTLIST = .FALSE.
            RETURN
        END IF

        PREFIX = BUF(1:5 )
        CALL UPCASE( PREFIX )
        IF ( PREFIX .EQ. 'LIST:' ) THEN
            HI = 5
            LO = 6
        ELSE
            HI = 0
            LO = 1
        END IF
        DO  L = 1, NMAX
            LO = LO + HI 
            HI = INDEX( BUF( LO : 512 ), ',' )
            IF ( HI .EQ. 0 ) HI = 513 - LO                   !  no more commas
            IF ( BUF( LO : LO+HI-1 ) .EQ. ' ' ) THEN
                NCNT = L - 1
                GO TO 99                !  list exhausted
            END IF
            LIST( L ) = STR2INT( BUF( LO : LO + HI - 1 ) )
            IF ( LO+HI .GE. 512 )  THEN
                NCNT = L
                GO TO 99                !  list exhausted
            END IF
        END DO

        IF ( LO+HI+1 .LT. 512 )  THEN   !  fall-through:  list done?
           IF ( BUF( LO+HI+1 : 512 ) .NE. ' ' )  THEN
               INTLIST = .FALSE.
               RETURN
            END IF
         END IF

99      CONTINUE        !  exit from loop
        INTLIST = ( NCNT .GT. 0 )
        RETURN
        END
