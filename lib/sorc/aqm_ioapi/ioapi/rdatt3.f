
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION   RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX,
     &                             ASIZE, AVAL )
        IMPLICIT NONE
        LOGICAL            RDATTC

C***********************************************************************
C  subroutine body starts at line   88
C   Entry  RDATTC  starts at line  105
C
C  FUNCTION:
C       Reads the attribute named ANAME for the variable VNAME in the
C       file FNAME into AVAL( AMAX ).  If VNAME == ALLVAR3, reads
C       global attribute ANAME.
C       AVAL must have type ATYPE, which should be one of M3REAL, M3INT, or
C       M3DBLE.
C
C       CHARACTER-string attributes use 
C               ENTRY   RDATTC( FNAME, VNAME, ANAME, CVAL )
C
C  PRECONDITIONS REQUIRED:
C       File must have been previously opened.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       netCDF
C
C  REVISION  HISTORY:
C       prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C***********************************************************************

C...........   INCLUDES:

      INCLUDE 'NETCDF.EXT'      ! netCDF  constants
      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'STATE3.EXT'      ! I/O API internal state


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME         !  logical file name
        CHARACTER*(*)   VNAME         !  variable name, or ALLVARS3
        CHARACTER*(*)   ANAME         !  attribute name
        INTEGER         ATYPE         !  attribute type (M3REAL, M3INT, M3DBLE)
        INTEGER         AMAX          !  attribute dimensionality
        INTEGER         ASIZE         !  attribute actual size
        REAL            AVAL( AMAX )  !  attribute value (numeric)
        CHARACTER*(*)   CVAL          !  attribute value (character-string)


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INIT3      !  Initialize I/O API
        INTEGER         INDEX1     !  look up names in name tables
        INTEGER         TRIMLEN    !  trimmed string length

        EXTERNAL  INIT3, INDEX1, TRIMLEN


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, V            !  subscripts for STATE3 arrays
        INTEGER         FID, VID        !  netCDF ID's
        INTEGER         IERR            !  netCDF error status return
        INTEGER         ITYP, ILEN
        LOGICAL         EFLAG
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*256   MESG            !  message-buffer


C***********************************************************************
C   begin body of subroutine  RDATT3

C...........   Check attribute type
            
        IF ( ( ATYPE .NE. NF_CHAR  ) .AND. 
     &       ( ATYPE .NE. NF_INT   ) .AND. 
     &       ( ATYPE .NE. NF_FLOAT ) .AND. 
     &       ( ATYPE .NE. NF_DOUBLE  ) ) THEN

            WRITE( MESG , '( 3 A, I10 )' ) 
     &           'RDATT3:  Attribute "'  , ANAME, 
     &           '" has unsupported type', ATYPE
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN

        END IF

        ITYP = ATYPE
        GO TO 111
        
        ENTRY RDATTC( FNAME, VNAME, ANAME, CVAL )
        ITYP = NF_CHAR
        !! fall through to  111

111     CONTINUE

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG  = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            RDATT3 = .FALSE.
            CALL M3WARN( 'RDATT3', 0,0, 'I/O API not yet initialized' )
            RETURN
        END IF

        IF ( TRIMLEN( FNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME // '" Variable "'// VNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A , I10 )' )
     &          'Max file name length 16; actual:', TRIMLEN( FNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( fname ) > 16

        IF ( TRIMLEN( VNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', TRIMLEN( VNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN
        END IF

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name
        FIL16 = FNAME   !  fixed-length-16 scratch copy of name

        F     = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( F .EQ. 0 ) THEN  !  file not available

            MESG = 'File "'// FIL16 // '" not yet opened.'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN

        ELSE IF ( CDFID3( F ) .LT. 0 ) THEN

            MESG = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN

        ELSE

            FID = CDFID3( F )

        END IF          !  if file not opened, or if readonly, or if volatile

C...........   Get ID for variable(s) to be read.
            
        IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = NCGLOBAL

        ELSE

            V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
            IF ( V .EQ. 0 ) THEN
                MESG = 'Variable "'      // VAR16 //
     &                 '" not in file "' // FIL16 // '"'
                CALL M3WARN( 'RDATT3', 0, 0, MESG )
                RDATT3 = .FALSE.
                RETURN
            ELSE
                VID = VINDX3( V, F )
            END IF

        END IF          !  if VAR16 is 'ALL', or not.


C...........   Check attribute:  supported type; actual type and size;
C...........   value.
C...........   Somewhat tortured logic-structure due to the fact that
C...........   one can't execute a RETURN within a critical section.

!$OMP   CRITICAL( S_NC )

        IERR = NF_INQ_ATT( FID, VID, ANAME, ITYP, ILEN )

        IF ( IERR .NE. NF_NOERR ) THEN

            MESG = 'Error inquiring type&size for attribute "' //
     &             ANAME // '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( ITYP .NE. ATYPE ) THEN

            MESG = 'Bad type for attribute "' //
     &             ANAME // '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( ( ATYPE .EQ. NF_CHAR .AND. ILEN .GT. LEN( CVAL ) )
     &            .OR.
     &            ( ATYPE .NE. NF_CHAR .AND. ILEN .GT. AMAX ) ) THEN

            MESG = 'Bad size for attribute "' // ANAME // 
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE

            IF ( ITYP .EQ. NF_CHAR ) THEN
                IERR = NF_GET_ATT_TEXT( FID, VID, ANAME, CVAL )
            ELSE IF ( ITYP .EQ. NF_INT ) THEN
                IERR = NF_GET_ATT_INT( FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_FLOAT ) THEN
                IERR = NF_GET_ATT_REAL( FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_DOUBLE  ) THEN
                IERR = NF_GET_ATT_DOUBLE( FID, VID, ANAME, AVAL )
            END IF

        END IF

!$OMP   END CRITICAL( S_NC )

        IF ( IERR .NE. NF_NOERR ) THEN
            EFLAG = .TRUE.
        END IF

        IF ( EFLAG ) THEN
            MESG = 'Error reading attribute "' // ANAME //
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
        END IF          !  ierr nonzero:  NCAPTC) failed
        IF ( ATYPE .EQ. NF_CHAR ) THEN
            RDATTC = ( .NOT. EFLAG )
        ELSE
            RDATT3 = ( .NOT. EFLAG )
        END IF

        RETURN

        END

