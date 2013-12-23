
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/year4.f,v 1.2 2000/11/28 21:23:11 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER FUNCTION YEAR4 ( YY )

C********************************************************************
C       function body starts at line  86
C
C  FUNCTION:
C
C      Returns the 4-digit year from the 2-digit year
C       
C
C  REVISION HISTORY:
C
C       Create by M Houyoux: 5/97
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C        YEAR   - 2 digit year
C
C    Output arguments:  none
C
C  RETURNS   user response after checking its range; or default.
C
C********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: year4.f,v 1.2 2000/11/28 21:23:11 smith_w Exp $
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
C Pathname: /env/proj/ioapi/SCCS/s.year4.f
C Last updated: 11/26/97 13:04:23
C
C****************************************************************************

        IMPLICIT NONE

C.......   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'

C.......   ARGUMENTS:

        INTEGER         YY    ! 2 digit year

C.......   EXTERNAL FUNCTIONS

        INTEGER		ENVINT
        EXTERNAL 	ENVINT

C.......   LOCAL VARIABLES:

        CHARACTER*256   MESG
        INTEGER         ISTAT

        INTEGER         BASEYR, PIVOTYR
        LOGICAL         FIRSTIME
	DATA		FIRSTIME / .TRUE. /
        
        SAVE		BASEYR, PIVOTYR, FIRSTIME

C......................................................................
C       begin YEAR4

        IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            BASEYR = ENVINT( 'YEAR4_BASE',
     &                        'Base century year for YEAR4 algorithm',
     &                        1900, ISTAT )
            IF ( BASEYR .GT. 100 ) THEN
                BASEYR = BASEYR / 100
            END IF
            BASEYR  = BASEYR * 100
            PIVOTYR = ENVINT( 'YEAR4_PIVOT',
     &                        'Pivot year for YEAR4 algorithm',
     &                        BASEYR + 70, ISTAT )
            PIVOTYR = MOD( PIVOTYR , 100 )
        END IF
        
        IF( YY .GT. BASEYR ) THEN
            YEAR4 = YY
        ELSE IF( YY .GT. 99 .OR. YY .LT. 0 ) THEN
            WRITE( MESG,94010 ) 'Year "', YY, 
     &                          '" is not a 2-digit positive number'
            CALL M3EXIT( 'YEAR4', 0, 0, MESG, 2 )

        ELSE IF( YY .GE. PIVOTYR ) THEN
            YEAR4 = BASEYR + YY
        ELSE
            YEAR4 = BASEYR + 100 + YY
        ENDIF

C................   end body of YEAR4 .......................................

C...........   Internal buffering formats............ 94xxx
 
94010   FORMAT( 10( A, :, I7, :, 1X ) )

        END

