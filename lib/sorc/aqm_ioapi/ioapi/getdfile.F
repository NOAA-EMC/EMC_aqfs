
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION GETDFILE( LNAME, RDONLY, FMTFLAG, RECLEN, 
     &                           CALLER )

C***********************************************************************
C  function body starts at line 84
C
C  DESCRIPTION:
C    Gets value of logical name LNAME from the environment, checks for 
C    existence of a file whose file name is that value, then opens the 
C    file as a direct access file on unit IUNIT according to the flags 
C    RDONLY (open for read-only iff TRUE, read/write if FALSE), 
C    FMTFLAG (formatted iff TRUE, else unformatted), and with the 
C    indicated record length RECLEN.
C    Logs the file-opening, together with the CALLER version, and 
C    returns the unit number (or -1 for failure)
C
C    RETURNS:   unit number, (or -1 for failure)
C
C  PRECONDITIONS REQUIRED:
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       TRIMLEN
C
C  REVISION  HISTORY:
C       Prototype  2/95 by CJC.
C
C       Modified  9/99 by CJC:  portability/standardization changes
C
C       Modified 7/2003 by CJC:  OMP thread safety -- critical sections
C       associated with INIT3()
C***********************************************************************

       IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
                   

       CHARACTER*(*) LNAME          !  logical file name
       LOGICAL       RDONLY         !  TRUE iff file is input-only
       LOGICAL       FMTFLAG        !  TRUE iff file should be formatted
       INTEGER       RECLEN         !  record length for direct access
       CHARACTER*(*) CALLER         !  caller-name for logging

C...........   EXTERNAL FUNCTIONS and their descriptions:
        
        INTEGER         INIT3
        INTEGER         JUNIT
        INTEGER         TRIMLEN
            
        EXTERNAL        INIT3, JUNIT, TRIMLEN


C...........   LOCAL VARIABLES and their descriptions:

           
        INTEGER         IUNIT
        INTEGER         LLEN, PLEN
        INTEGER		ISTAT, JSTAT
        
        LOGICAL         LCHECK
        
        INTEGER         LOGDEV          !  also a state variable !
        DATA            LOGDEV / -1 /
        SAVE            LOGDEV

        CHARACTER*256   PNAME
        CHARACTER*16    FMTSTRING

C***********************************************************************
C   begin body of GETDFILE()
                              
!$OMP   CRITICAL( S_INIT )
        IF ( LOGDEV .LT. 0 ) THEN
            LOGDEV = INIT3()
        END IF
!$OMP   END CRITICAL( S_INIT )

C...........   Read filename from environment ---

      CALL NAMEVAL(  LNAME, PNAME )

      LLEN = TRIMLEN( LNAME )
      PLEN = TRIMLEN( PNAME )
        
      IF ( FMTFLAG ) THEN
          FMTSTRING = 'FORMATTED'
      ELSE
          FMTSTRING = 'UNFORMATTED'
      END IF

C.......   Check for existence of files ---

      INQUIRE( FILE=PNAME, EXIST=LCHECK )

      IF ( RDONLY ) THEN

          IF ( .NOT. LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 ) 
     &            'ERROR: input file not found: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
              GETDFILE = -1
              RETURN
          END IF
          
          IUNIT = JUNIT()

#if _CRAY || _AIX || F90
          OPEN( UNIT   = IUNIT, 
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'OLD',
     &          ACTION = 'READ',
     &          IOSTAT = ISTAT )
#endif
#if ( __alpha || sgi ) && ( !F90 )
          OPEN( UNIT   = IUNIT, 
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'OLD',
     &          READONLY,
     &          IOSTAT = ISTAT )
#endif
#if !( __alpha || sgi || _CRAY || _AIX || F90 )
          OPEN( UNIT   = IUNIT, 
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'OLD',
     &          IOSTAT = ISTAT )
#endif

          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 ) 
     &        LNAME( 1:LLEN ), 
     &        PNAME( 1:PLEN ),
     &        'I/O status:  ', ISTAT
              GETDFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 ) 
     &        'File ', LNAME( 1:LLEN ), 
     &        ' opened for input on unit', IUNIT,
     &        ' with record length', RECLEN,
     &        PNAME(  1:PLEN  )

      ELSE      !  not read-only:  open for write

          IF ( LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'WARNING: output file already exists: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
          END IF

          IUNIT = JUNIT()

#ifdef __alpha
          OPEN( UNIT   = IUNIT, 
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'UNKNOWN',
     &          IOSTAT = ISTAT )
#endif    /*  ifdef __alpha */
#ifndef __alpha
          OPEN( UNIT   = IUNIT, 
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
#endif    /*  ifndef __alpha */

          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 ) 
     &        LNAME( 1:LLEN ), 
     &        PNAME( 1:PLEN ), 
     &        'I/O status:  ', ISTAT 
              GETDFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 ) 
     &        'File "', LNAME( 1:LLEN ), 
     &        '" opened for output on unit:', IUNIT,
     &        ' with record length', RECLEN,
     &        PNAME(  1:PLEN  )

      END IF

      GETDFILE = IUNIT
      RETURN

C----------------------------------------------------------------------
C     handle messaging errors:

9999  CONTINUE

      IF ( JSTAT .NE. 0 ) THEN
           CALL PERROR( 'ERROR writing log message.')
           WRITE( LOGDEV, 9200, IOSTAT=ISTAT ) 
     &         'I/O status:', JSTAT
           GETDFILE = -1
           RETURN
      END IF

C-----------------------------------------------------------------------
C    Format statements:

8000   FORMAT( A )
9000   FORMAT( /, 1X, 2A, /, :, 1X, A, / )
9001   FORMAT( /, 5X, 3A, I4, A, I8,
     &         /, 5X,  A, /)
9100   FORMAT( /, 1X,  A, ':', A,
     &         /, 1X,  A, I7 , / )
9200   FORMAT( /, 1X,  A, I7 , / )
      
       END
