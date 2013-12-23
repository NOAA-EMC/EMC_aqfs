
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/poly.f,v 1.2 2000/11/28 21:23:00 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      REAL    FUNCTION  POLY (XPT, XPTS, YPTS, NDEG)

C***********************************************************************
C  function body starts at line  63
C
C  FUNCTION:
C        Performs arbitrary-degree polynomial interpolation for XPT on
C        curve determined by XPTS and YPTS using Newton divided-differences.
C  NOTE:  high-order purely polynomial interpolations have stability
C         problems.  NDEG <= 5 is recommended. -- CJC
C
C  REVISION HISTORY:
C
C    11/88   Modified for ROMNET
C    ??/90   Modified for ROM 2.2 by CJC:  scalar coefficient arithmetic --
C            no restrictions on NDEG
C    4/91    Modified by CJC:  optimized initialization
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C        :XPT       point on curve whose value is to be determined
C        :XPTS      points at which function values are known
C                   (there are NDEG + 1 values necessary)
C        :YPTS      function values at each of XPTS
C        :NDEG      degree of polynomial
C
C    Function Value:  POLY      interpolated value at XPT
C
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS:

        INTEGER         NDEG
        REAL            XPT
        REAL            XPTS ( NDEG + 1 )
        REAL            YPTS ( NDEG + 1 )


C...........   LOCAL VARIABLES:

        INTEGER         I, J, K
        REAL            TDIFF, COEFF
        REAL            DSCR, XSCR, YSCR


C........................................................................
C.......   begin body of POLY

C.......   Compute divided differences: denominator is a product for J <> K.
C.......   Initialization uses unrolled degree I=1 (linear interpolation terms)

        XSCR  =  XPTS ( 1 )
        COEFF =  XPT  -  XSCR
        DSCR  =  COEFF / ( XSCR  -  XPTS( 2 ) )
        YSCR  =  YPTS ( 1 )
        POLY  =  YSCR  +  DSCR * ( YSCR  -  YPTS( 2 ) )


C.......   Now compute higher order terms using divided differences:
C.......   denom is a product for J <> K.

        DO  144  I = 2, NDEG

C.......   Initialization uses unrolled K=1 case.

            XSCR = XPTS ( 1 )
            DSCR = XSCR - XPTS ( 2 )

            DO  100  J = 3 , I + 1
                DSCR = DSCR * ( XSCR - XPTS ( J ) )
100         CONTINUE

            TDIFF  =  YPTS ( 1 ) / DSCR

            DO  133  K = 2, I + 1       !  loop on points K

                XSCR = XPTS ( K )
                DSCR = XSCR - XPTS ( 1 )

                DO  111  J = 2 , K - 1
                    DSCR = DSCR * ( XSCR - XPTS ( J ) )
111             CONTINUE                ! end loop: j not k, part 1

                DO  122  J = K + 1 , I + 1
                    DSCR = DSCR * ( XSCR - XPTS ( J ) )
122             CONTINUE                ! end loop: j not k, part 2


C...........   Compute differences term:

                TDIFF  =  TDIFF  +  YPTS ( K ) / DSCR

133         CONTINUE            !  end loop on points K


C...........   Compute polynomial coefficients:

            COEFF  =  COEFF * ( XPT  -  XPTS ( I ) )

C...........   Compute interpolated value

            POLY = POLY  +  COEFF * TDIFF

144     CONTINUE        !  end loop on terms of degree I


        RETURN
        END

