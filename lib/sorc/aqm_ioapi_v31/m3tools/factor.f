
        PROGRAM FACTOR

C***********************************************************************
C Version "$Id: factor.f 43 2014-09-12 14:06:19Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line 74
C
C  DESCRIPTION:
C       factor [integer]
C
C  PRECONDITIONS REQUIRED:
C       well-formed argument (or no argument)
C       integer size less than 100 bits
C       IARGC() supported
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM, STR2INT
C
C  REVISION  HISTORY:
C       prototype 1/1997 by CJC
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        INTEGER, PARAMETER :: NPRIMES = 175

        INTEGER, PARAMETER :: PLIST( NPRIMES ) = (/
     &       2,   3,   5,   7,  11,  13,  17,  19,  23,  29,   !   1-10
     &      31,  37,  41,  43,  47,  53,  59,  61,  67,  71,   !  11-20
     &      73,  79,  83,  89,  97, 103, 107, 109, 113, 119,   !  21-30
     &     127, 131, 133, 137, 139, 149, 151, 161, 163, 167,   !  31-40
     &     169, 173, 179, 181, 191, 193, 197, 199, 209, 211,   !  41-50
     &     221, 223, 227, 229, 233, 239, 241, 251, 257, 263,   !  51-60
     &     269, 271, 277, 281, 283, 289, 293, 301, 307, 311,   !  61-70
     &     313, 317, 331, 337, 347, 349, 353, 359, 367, 373,   !  71-80
     &     379, 383, 389, 393, 401, 409, 419, 421, 431, 433,   !  81-90
     &     439, 443, 449, 457, 461, 463, 467, 479, 487, 491,   !  91-100
     &     499, 503, 509, 521, 523, 529, 541, 547, 557, 563,   ! 101-110
     &     569, 571, 577, 587, 593, 599, 601, 607, 613, 617,   ! 111-120
     &     619, 631, 641, 643, 647, 653, 659, 661, 673, 677,   ! 121-130
     &     683, 691, 701, 709, 719, 727, 733, 739, 743, 751,   ! 131-140
     &     757, 761, 769, 773, 787, 797, 809, 811, 821, 823,   ! 141-150
     &     827, 829, 839, 841, 853, 857, 859, 863, 877, 881,   ! 151-160
     &     883, 887, 911, 929, 937, 941, 947, 953, 961, 967,   ! 161-170
     &     971, 977, 983, 991, 997                             ! 171-175
     &          /)

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER :: IARGC           !  should be intrinsic

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  for M3EXIT()
        INTEGER         ARG, F, I, K, P, SIGN

C***********************************************************************
C   begin body of program FACTOR

        I = INIT3()
        WRITE( *,92000 )
     &  ' ',
     &  'Program FACTOR to find the prime factors of the specified ',
     &  '(positive) integer taken either from the command line or from',
     &  'the user.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: factor.f 43 2014-09-12 14:06:19Z coats                  $',
     &' '

        ARGCNT = IARGC()
        IF ( ARGCNT .EQ. 0 ) THEN
            ARG = GETNUM( -999999999, 999999999, 1,
     &                    'Enter integer to be factored' )
        ELSE IF ( ARGCNT .EQ. 1 ) THEN
            CALL GETARG( 1, ENVBUF )
            IF ( ENVBUF( 1:1 ) .EQ. '-' ) THEN
                SIGN = -1
                ARG  = STR2INT( ENVBUF( 2:256 ) )
            ELSE
                SIGN = 1
                ARG  = STR2INT( ENVBUF )
            END IF
            IF ( ARG .LT. 0 ) THEN
                MESG = 'Usage:  "FACTOR [INTEGER]" ' //
     &                 '(argument was invalid)'
                CALL M3EXIT( 'FACTOR', 0, 0, MESG, 2 )
            END IF
            ARG = SIGN * ARG
        ELSE
            MESG = 'Usage:  "FACTOR [INTEGER] ' //
     &             '(too many arguments given)'
            CALL M3EXIT( 'FACTOR', 0, 0, MESG, 2 )
        END IF
        WRITE( *,92011 ) 'Factoring', ARG

C.......   Now factor ARG:

        IF ( ARG .EQ. 0 ) THEN
            WRITE( *,92011 )  'Factor:', 0
            GO TO 199
        END IF          !  if arg 0 or not

        IF ( ARG .LT. 0 ) THEN
            WRITE( *,92012 )  'Factor:', -1
            ARG = -ARG
        ELSE IF ( ARG .EQ. 1 ) THEN
            WRITE( *,92011 )  'Factor:', 1
            GO TO 199
        END IF

C.......   Factors from PLIST:

        DO  122   I = 1, NPRIMES

            F = PLIST( I )

            IF ( F*F .GT. ARG ) THEN
                IF ( ARG .GT. 1 ) THEN
                    WRITE( *,92011 ) 'Factor', ARG, 1, ARG
                END IF
                GO TO 199
            END IF

            K = 0
            P = 1

111         CONTINUE                    !  loop:  extract copies of F

                IF ( MOD( ARG, F ) .EQ. 0 ) THEN
                    K   = K + 1
                    P   = P * F
                    ARG = ARG / F
                    GO TO  111          !  to head of "extract copies" loop
                END IF

            IF ( K .GT. 0 ) THEN
                IF ( K .LT. 10 ) THEN
                    WRITE( *,92011 ) 'Factor', F, K, P
                ELSE
                    WRITE( *,92012 ) 'Factor', F, K, P
                END IF
            END IF

122     CONTINUE

C.......   Other factors:

133     CONTINUE        !  loop on potential factors F

            F = F + 2
            P = 1
            K = 0
            IF ( F*F .GT. ARG ) THEN
                IF ( ARG .GT. 1 ) THEN
                    WRITE( *,92011 ) 'Factor', ARG, 1, ARG
                END IF
                GO TO  199
            END IF

144         CONTINUE            !  loop:  extract copies of F

                IF ( MOD( ARG, F ) .EQ. 0 ) THEN
                    P   = P * F
                    K   = K + 1
                    ARG = ARG / F
                    GO TO  144          !  to head of "extract copies" loop
                END IF

            IF ( K .GT. 0 ) THEN
                IF ( K .LT. 10 ) THEN
                    WRITE( *,92011 ) 'Factor', F, K, P
                ELSE
                    WRITE( *,92012 ) 'Factor', F, K, P
                END IF
            END IF

            GO TO 133           !  end of loop on potential factors F

199     CONTINUE                !  program termination
        WRITE ( *,* )
        CALL EXIT( 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X, A )

92011   FORMAT ( 5X, A, :, I10, :, '^', I1, '  = ', I10 )

92012   FORMAT ( 5X, A, :, I10, :, '^', I2,  ' = ', I10 )


        END PROGRAM FACTOR

