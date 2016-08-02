C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.
C Copyright (C) 1(C) 2003 Baron Advanced Meteorological Systems.
C All Rights Reserved.
C.........................................................................

        PROGRAM INQ_TEST

C***********************************************************************
C  program body starts at line  99
C
C  DESCRIPTION:
C       Interactive test of INQUIRE and FLUSH problems.
C
C  REVISION  HISTORY:
C       Prototype 6/2003 by Carlie J. Coats, Jr.,
C       BAMS Environmental Modeling Center
C
C***********************************************************************

      IMPLICIT NONE

      INTEGER   IDEV, ISTAT

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        WRITE( *, '( 5X, A )' )
     &  ' ',
     &  'Program INQ_TEST for testing INQIURE() and FLUSH().',
     &  ' '

        INQUIRE( UNIT =*, NUMBER=IDEV, IOSTAT=ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( *, '( 5X, A, I5 )' )'INQUIRE ERROR:  ISTAT=', ISTAT
        ELSE
            WRITE( *, '( 5X, A, I5 )' )'INQUIRE:  Unit number = ', IDEV
        END IF

        WRITE( *, '( 5X, A )' ) 'Flushing...'
        CALL FLUSH( 101 )
        WRITE( *, '( 5X, A, I5 )' )'FLUSH:  status = ', ISTAT
        STOP
        END

        
