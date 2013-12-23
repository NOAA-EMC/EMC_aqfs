C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/air4/yoj/rel/models/PARIO/src/initm3io.f,v 1.1.1.1 2003/05/05 17:18:09 yoj Exp $

      SUBROUTINE INITM3IO( CALLER, MY_PE, IO_PE, LOGDEV )
C ....................................................................
 
C  PURPOSE:   Start up Models-3 I/O API and log device. If this is not
C             the designated I/O processor, direct I/O API output to a
C             separate log file.
 
C  REVISION HISTORY: 
C       Original version  6/98 by Al Bourgeois for parallel implementation.
C       Modified 01/1998 by Al Bourgeois to apply TRIM on PUTENV argument.
C       Modified 02/1999 by Al Bourgeois to use PUTVAL instead of PUTENV.
C       Modified 03/1999 by Al Bourgeois to use APPL for logfile names.
C       Modified 06/1999 by Al Bourgeois to use PUTENV instead of PUTVAL.
C       Modified 08/1999 by Al Bourgeois to use PUTENVVAR instead of PUTENV.
C       Modified 03/2002 by Jeff Young   to use SETENVVAR instead of PUTENVVAR.
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C      CHARACTER*16    CALLER    ! Program name.
C      INTEGER         MY_PE     ! Local processor id.
C      INTEGER         IO_PE     ! Id of primary processor used for file I/O.
  
C  OUT:
C      INTEGER         LOGDEV    ! FORTRAN unit number for log file.
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: NAMEVAL, TRIMLEN, TRIM, SETENVVAR, WRITE
 
C .......................................................................

      IMPLICIT  NONE

C ARGUMENTS:

      CHARACTER*16    CALLER     ! Program name.
      INTEGER         MY_PE      ! Local processor id.
      INTEGER         IO_PE      ! Id of primary processor used for file I/O.
      INTEGER         LOGDEV     ! FORTRAN unit number for log file.

C EXTERNAL FUNCTIONS:

      INTEGER       INIT3        ! Initializes IOAPI and returns unit for log.
      INTEGER       TRIMLEN      ! Effective char. string length.
      EXTERNAL      INIT3, TRIMLEN

C LOCAL VARIABLES:

      CHARACTER*68  IOLOGEQ       ! String to set LOGFILE environment var.
      CHARACTER*120 EQNAME        ! Filename of parallel I/O output file.
      CHARACTER*3   CMYPE         ! String to identify processor.
      CHARACTER*8   PRESTR        ! Prefix name for output file.
      CHARACTER*16  APPL          ! String for constructing log file names.
      DATA          APPL / 'CTM_APPL        ' /

      SAVE IOLOGEQ

C .......................................................................
C       begin subroutine INITM3IO
 
C Direct I/O API output to a separate log file, unless this
C processor is the designated IO_PE. Get APPL from the
C environment to construct the log filenames. The default
C prefix is CTM_LOG.

      IF ( MY_PE .NE. IO_PE ) THEN
         CALL NAMEVAL( APPL, EQNAME )
         WRITE( PRESTR,'(A8)' ) 'CTM_LOG_'
         WRITE( CMYPE,'(I3.3)' ) MY_PE
         IF ( APPL .EQ. EQNAME(1:16 ) ) THEN
            IOLOGEQ = PRESTR // CMYPE
         ELSE
            IOLOGEQ = PRESTR // CMYPE // '.' // EQNAME( 1:TRIMLEN( EQNAME ) )
         END IF
         CALL SETENVVAR ( 'LOGFILE', IOLOGEQ )
      END IF

C Start up the I/O API.

      LOGDEV = INIT3()
      WRITE( LOGDEV,'( 5X, A )' )
     &   'Program ' // CALLER( 1:TRIMLEN( CALLER )),' '

      RETURN

      END

