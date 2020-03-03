
C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/work/rep/PARIO/src/pinterpb.f,v 1.6 2011/03/30 18:13:01 sjr Exp $

      LOGICAL FUNCTION PINTERPB ( FILNAME, VARNAME, CALLER,
     &                            JDATE, JTIME, VSIZE, VARRAY )
C.....................................................................
 
C Purpose:   Performs Models-3 file-variable interpolation in a
C            parallel environment (parallel version of INTERP3). Each
C            processor does its own file reading and no communication
C            is needed. Processors do their own
C            interpolation of file data on their own subdomain.
 
C Return Value:  The function can fail in the following ways:
C      Dynamic memory routines ALLOBBUF, ALLOMBUF, or GROWBUF fail (fatal).
C      GTNDXHDV fails to retrieve a file variable index (fatal).
C      VSIZE is not consistent with file header information (fatal).
C      An MPI error occurs (fatal).
C      POPEN3 fails on file open or PDESC3 fails to get file description.
C      CURRSTEP fails to compute the current date and time.
C      File type is not gridded or boundary.
C      Routines READHDV, READFGV, or READBNDY fail on reading file data.
C      Function INTERPOL fails to interpolate the file data.
 
C Preconditions:  FILNAME is a Models-3 bopundary data file containing
C      variable VARNAME for a set of time steps enclosing JDATE:JTIME
C      File type must be BNDARY3. Array VARRAY must be large
C      enough to hold the file data requested (see NOTES below).

C Revision History: 
C      Created 11 Apr 2001 by Jeff Young from PINTERPB
C      Modified 06 Feb 2004 by David Wong
C        -- use f90 syntax to allocate memory rather than uses
C           DYNMEM library
C       Modified 11/03/2004 by David Wong
C          -- fixed a bug, which only manifested in Sun system, in 
C             allocating new memory space
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO; removed
C             deprecated TRIMLEN
 
C Argument List Description:
C In:
C    CHARACTER*16   FILNAME         ! Name of file to be read.
C    CHARACTER*(*)  VARNAME         ! File variable name.
C    CHARACTER*(*)  CALLER          ! Name of calling routine.
C    INTEGER        JDATE           ! Current Julian date (YYYYDDD).
C    INTEGER        JTIME           ! Current time (HHMMSS).
C    INTEGER        VSIZE           ! Size of VARRAY. See note (2) below.
 
C    Common Block PIOGRID:
C    INTEGER  NPCOLD        ! Number of processors across grid columns.
C    INTEGER  MY_NROWS      ! Local number of grid rows actually used.
C    INTEGER  MY_NCOLS      ! Local number of grid columns actually used.
C    INTEGER  NUMROWS       ! Row dimension of local-processor arrays.
C    INTEGER  NUMCOLS       ! Column dimension of local-processor arrays.
C    INTEGER  BTHICK        ! Cell thickness of grid boundary.
 
C Out:
C    REAL           VARRAY(VSIZE)   ! Interpolated values.
 
C Local Variable Description:  see below
 
C Calls:  OPEN3, DESC3, M3WARN,
C         ENVYN, NEXTIME, SECSDIFF, TIME2SEC, CURRSTEP, TRIMLEN,
C         ALLOBBUF, ALLOMBUF, GROWBUF, SETINT, SETFLT, GROWREAL,
C         GTNDXHDV, READBNDY, INTERPOL
 
C Notes: (1) PINTERPB works in either a "single" processor mode.
 
C             In "single" processor mode, PINTERPB may be executed by
C             any number of processors, and each calling processor does
C             its own file reading.
 
C         (2) PINTERPB handles boundary variables.

C             There are distinct ways file variables
C             are stored and communicated to other processors, depending
C             on the values of FTYPE3D and UPNAM3D in the variable's file
C             header. For each of these file types, a check is performed
C             to ensure that VSIZE is consistent with dimensions in the
C             file header and with the domain decomposition.
 
C              (iii) For boundary variables, VSIZE is interpreted as
C              NBNDY*NLAYS, where NBNDY is the local PE subdomain boundary
C              dimension and NLAYS is calculated as VSIZE/NBNDY. Function
C              READBNDY is called to get the full (global-grid) file
C              boundary values to the processor(s), from which a processor 
C              constructs its own local boundary. The returned array
C              VARRAY(*) is ordered as VARRAY(NBNDY, NLAYS).
 
C         (3) If a new read operation is necessary (RFLAG .NE. 0), the
C             circular file buffer BUFFERHD (declared in a COMMON block
C             defined in PINTERPB.EXT) is updated (by READBNDY(.
 
C         (4) For time-independent files, READBNDY
C             is called with start and end date:time set to zero
 
C........................................................................

      USE PINTERPB_MODULE
      USE M3UTILIO              ! i/o api

      IMPLICIT NONE

C Include Files

!     INCLUDE 'PIODECL3.EXT'     ! I/O definitions and declarations
      INCLUDE 'PIOGRID.EXT'      ! Parallel grid-related variables
!     INCLUDE 'PINTERPB.EXT'     ! Variables for parallel file reading
  
C Arguments:

      CHARACTER( 16 ) :: FILNAME ! Name of file to be read
      CHARACTER( * )  :: VARNAME ! File variable name
      CHARACTER( * )  :: CALLER  ! Name of calling routine
      INTEGER        JDATE       ! Current Julian date (YYYYDDD)
      INTEGER        JTIME       ! Current time (HHMMSS)
      INTEGER        VSIZE       ! Size of VARRAY
      REAL           VARRAY( VSIZE ) ! Interpolated values. See note (1) above

C External Functions:

      LOGICAL        READBNDY    ! Reads boundary file variables
      LOGICAL        INTERPOL    ! Linear interpolation
!     LOGICAL        ALLOBBUF    ! Allocate memory for read buffers
!     LOGICAL        ALLOMBUF    ! Allocation for buffer management arrays
      LOGICAL        GROWBUF     ! Extends memory for file-variable buffers

      EXTERNAL       READBNDY, INTERPOL           ! Parallel M3IO library
!     EXTERNAL       ALLOBBUF, ALLOMBUF, GROWBUF  ! Memory allocation library
      EXTERNAL       GROWBUF                      ! Memory allocation library
!     EXTERNAL       ALLOBBUF, ALLOMBUF           ! Memory allocation library
!     EXTERNAL       SETINT, SETFLT               ! Utilities library

C Internal Functions:

      LOGICAL        GTNDXHDV    ! Get index of variable from list

C Local Variables:

      INTEGER        I, J         ! Loop counters
      INTEGER        VX           ! Index for file variable
      INTEGER        DT           ! File time step in seconds
      INTEGER        MDATE        ! Date argument for CURRSTEP
      INTEGER        MTIME        ! Time argument for CURRSTEP
      INTEGER        LDATE        ! Temporary variable for starting date
      INTEGER        LTIME        ! Temporary variable for starting time
      INTEGER        DATE( 2 )    ! Start and end dates for current buffer
      INTEGER        TIME( 2 )    ! Start and end times for current buffer
      INTEGER        DTJL         ! Difference LDAT:LTIM to JDATE:JTIME
      INTEGER        FLIP         ! Toggle for circular buffer order (0,1)
      INTEGER        IERROR       ! Error code from allocation subroutines
      INTEGER, SAVE :: NBVS       ! Number of file variable buffers
      INTEGER, SAVE :: ENDBUF     ! Offset for end of buffer plus one
      INTEGER        MYSIZE       ! Size of new variable, 2 buffers worth
      INTEGER        RFLAG        ! Number of time records to read
      INTEGER        IL           ! Loop counter over layers
      INTEGER        NLAYS        ! Local PE layer dimension of VARRAY
      INTEGER        NBNDY        ! Local PE boundary dimension of VARRAY
      INTEGER        MBUFSIZE     ! Message buffer size
      INTEGER, SAVE :: LOGDEV     ! Unit number for log file
      INTEGER        STATUS       ! Status returned from routine ENVYN

      SAVE  MBUFSIZE
      
      CHARACTER( 80 ) :: MSG      ! Buffer for building error messages
      CHARACTER( 16 ) :: FIL16    ! Scratch area for file-name
      CHARACTER( 16 ) :: VAR16    ! Scratch area for vble-name

      LOGICAL        NEWVAR       ! Flag to indicate new variable

      LOGICAL, SAVE :: FIRSTIME = .TRUE.    ! First pass flag

      INTEGER        LOC1, LOC2, LOC3

C........................................................................

      PINTERPB = .TRUE.

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         LOGDEV = INIT3()

C Allocate memory for read and message buffers

         MBUFSIZE = 2*BTHICK * ( GNCOLS + GNROWS + 2*BTHICK ) * GNLAYS
         ALLOCATE ( MSGBUFHD( MBUFSIZE ), STAT = IERROR )
         IF ( IERROR .NE. 0 ) THEN
            MSG = 'Error allocating MSGBUFHD.'
            CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
            PINTERPB = .FALSE.; RETURN
         END IF

C Initialize number of buffered variables and pointer

         NBVS = 0
         ENDBUF = 1

C Set error code flag to zero

         IERROR = 0

C Initialize arrays for managing file buffers

         LDATHD = -9999
         LTIMHD = -9999
         NDATHD = -9999
         NTIMHD = -9999
         SWBUFHD = 0

C Initialize (static) character*33 array VLISTHD

         DO I = 1, MXNVARHD
            VLISTHD( I ) = ' '
         END DO

      END IF   ! FIRSTIME

C Open the file

      FIL16 = FILNAME
      IF ( .NOT. OPEN3( FIL16, FSREAD3, 'PINTERPB' ) ) THEN
         MSG = 'Could not open '// TRIM( FIL16 )
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN
      END IF

C Get the file description

      IF ( .NOT. DESC3( FIL16 ) ) THEN
         MSG = 'Could not get '// TRIM( FIL16 ) //
     &         ' file description'
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN
      END IF

C Check that variable is on file

      IF ( INDEX1( VARNAME, NVARS3D, VNAME3D ) .EQ. 0 ) THEN
         MSG = 'Variable '// TRIM( VARNAME )
     &       // ' not found in '// TRIM( FIL16 )
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN
      END IF

C Get the index of this variable from the list of variables with existing
C read buffers and check dimensions against stored values.
C If this is the first time PINTERPB has been called for this variable, add
C it to the list, set buffer pointers, check dimensions against processor map,
C and store the dimensions.

      VAR16 = VARNAME
      IF ( .NOT. GTNDXHDV ( FIL16, VAR16, JDATE, JTIME, VSIZE,
     &                      NBVS, ENDBUF, VX, NEWVAR ) ) THEN
         MSG = 'GTNDXHDV failed on ' // TRIM( VAR16 ) //
     &         ' from file '// TRIM( FIL16 )
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN
      END IF

C Extend buffer space if new variable, and initialize

      IF ( NEWVAR ) THEN
         IF ( .NOT. GROWBUF ( BUFFERHD( PTR_COUNT ), ENDBUF-1 ) ) THEN
            MSG = 'Error in memory allocation routine GROWBUF.'
            CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
            PTR_COUNT = PTR_COUNT - 1
            PINTERPB = .FALSE.; RETURN
         ELSE
            MYSIZE = 2*VSIZE
!           BUFFERHD( PTR_COUNT )%MEM( 0 )%DATA_PTR = 0.0
!           BUFFERHD( PTR_COUNT )%MEM( 1 )%DATA_PTR = 0.0
         END IF
      END IF

C Get start date:time of buffer

      LDATE = LDATHD( VX )
      LTIME = LTIMHD( VX )

C Determine if buffer needs to be updated

      IF ( TSTEP3D .EQ. 0 ) THEN    ! Time-independent file

C Variables in time-independent files are read once only:
C the start date has been initialized with a negative value,
C and the start and end buffer date and time are set here to zero. 

         IF ( LDATE .NE. 0 ) THEN
            RFLAG = 2                   ! Read two records
!!!!!!!!!!!!!! HOW CAN I READ 2 RECORDS ON A TIME-INDEPENDENT FILE ??? !!!!!!!!
         ELSE
            RFLAG = 0                   ! No read necessary
         END IF

         SWBUFHD( VX ) = 0                ! No buffer flip
         FLIP = 0

         DATE( 1 ) = 0
         TIME( 1 ) = 0
         DATE( 2 ) = 0
         TIME( 2 ) = 0

         LDATHD( VX ) = 0
         LTIMHD( VX ) = 0
         NDATHD( VX ) = 0
         NTIMHD( VX ) = 0

      ELSE            ! Time-stepped file

C Check to see relationship between current circular buffer start and ending
C times, and requested time.  Two cases require updates:
C    (1) need new data at both ends; and
C    (2) need new data at the futureward end only.
C (Needing data at the pastward end is still treated under (1).)

         DT = TIME2SEC( TSTEP3D )

         IF ( LDATE .GT. 0 ) THEN
            DTJL = SECSDIFF ( LDATE, LTIME, JDATE, JTIME )
         ELSE                      ! ldate set to "invalid"
            DTJL = -1              ! dtjl "invalid"
         END IF

C Determine whether or not to read

         IF ( ( DTJL .LT. 0 ) .OR. ( DTJL .GT. 2*DT ) ) THEN ! read both

C Get file date:time just previous to jdate:jtime

            IF ( .NOT. CURRSTEP ( JDATE, JTIME,
     &                            SDATE3D, STIME3D, TSTEP3D,
     &                            MDATE, MTIME ) ) THEN
               MSG = 'CURRSTEP failed on file variable '
     &             // TRIM( VAR16 ) //
     &             ' from file '// TRIM( FIL16 )
               CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
               PINTERPB = .FALSE.; RETURN
            END IF          !  if CURRSTEP failed

            RFLAG = 2                               ! Read two time records

            FLIP = 0
            SWBUFHD( VX ) = 0                       ! no buffer flip

C Store previous and subsequent date:time

            DATE( 1 ) = MDATE
            TIME( 1 ) = MTIME
            DATE( 2 ) = MDATE
            TIME( 2 ) = MTIME
            CALL NEXTIME( DATE( 2 ), TIME( 2 ), TSTEP3D )

            LDATHD( VX ) = DATE( 1 )
            LTIMHD( VX ) = TIME( 1 )
            NDATHD( VX ) = DATE( 2 )
            NTIMHD( VX ) = TIME( 2 )

         ELSE IF ( DTJL .GT. DT ) THEN               ! advance by one time ste

            RFLAG = 1                                ! read one time record

            FLIP = 1 - SWBUFHD( VX )   ! flip buffer order
            SWBUFHD( VX ) = FLIP

            DATE( 1 ) = LDATHD( VX )
            TIME( 1 ) = LTIMHD( VX )
            CALL NEXTIME ( DATE( 1 ), TIME( 1 ), TSTEP3D )

            DATE( 2 ) = DATE( 1 )
            TIME( 2 ) = TIME( 1 )
            CALL NEXTIME ( DATE( 2 ), TIME( 2 ), TSTEP3D )

            LDATHD( VX ) = DATE( 1 )
            LTIMHD( VX ) = TIME( 1 )
            NDATHD( VX ) = DATE( 2 )
            NTIMHD( VX ) = TIME( 2 )

         ELSE        ! Do not need to read this variable from this file

            RFLAG = 0                              ! No read necessary
            FLIP = SWBUFHD( VX )    ! Flip buffer order

            DATE( 1 ) = LDATHD( VX )
            TIME( 1 ) = LTIMHD( VX )
            DATE( 2 ) = NDATHD( VX )
            TIME( 2 ) = NTIMHD( VX )

         END IF      ! If buffer needs updating

      END IF        ! Time stepped file or not 

C Clear read and message buffers

      IF ( RFLAG .NE. 0 ) THEN
         MSGBUFHD( 1:MBUFSIZE ) = 0.0
      END IF

C Read the buffer if necessary

      IF ( FTYPE3D .EQ. BNDARY3 ) THEN

         NBNDY = 2*BTHICK * ( NUMCOLS + NUMROWS + 2*BTHICK )
         NLAYS = VSIZE / NBNDY

C Check calling dimension against file header dimensions

         IF ( NLAYS .NE. NLAYS3D ) THEN
            MSG = 'Mismatch between VSIZE and file dimensions.'
            WRITE( LOGDEV,9020 ) VSIZE, NBNDY, NLAYS3D 
            CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
            PINTERPB = .FALSE.; RETURN
         END IF

         IF ( RFLAG .NE. 0 ) THEN
            IF ( .NOT. READBNDY( FIL16, VAR16, VX,
     &                           NBNDY, NLAYS, DATE,
     &                           TIME, RFLAG, FLIP ) ) THEN
               MSG = 'Subroutine READBNDY failed.'
               CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
               PINTERPB = .FALSE.; RETURN
            END IF

         END IF

      ELSE

         MSG = 'Illegal file type, not gridded or boundary.'
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN

      END IF   ! if ( ftype3d .eq. grdded3 )

C The number of values to be interpolated is VSIZE

C Interpolate to time JDATE:JTIME. Use MSGBUFHD as intermediate holding place
C for interpolated values

      LOC1 = MOD( 0+FLIP, 2 )
      LOC2 = MOD( 1+FLIP, 2 )
      LOC3 = BUFPOSHD( VX )

      IF ( .NOT. INTERPOL ( JDATE, JTIME, DATE( 1 ), TIME( 1 ),
     &                      DATE( 2 ), TIME( 2 ), VSIZE,
     &                      BUFFERHD( LOC3 )%MEM( LOC1 )%DATA_PTR,
     &                      BUFFERHD( LOC3 )%MEM( LOC2 )%DATA_PTR,
     &                      MSGBUFHD ) ) THEN
         MSG = 'Subroutine INTERPOL failed.'
         CALL M3WARN ( 'PINTERPB', JDATE, JTIME, MSG )
         PINTERPB = .FALSE.; RETURN

      ELSE        ! Interpolation was successful. Store values

C Store the interpolated values from MSGBUFHD into VARRAY

         DO I = 1, VSIZE
            VARRAY( I ) = MSGBUFHD( I )
         END DO

      END IF     ! if ( .not. interpol )

      RETURN

9020  FORMAT ( /5X, 'VSIZE= ', I5, ' NBNDY = ', I5, ' NLAYS3D = ', I5,
     &         ' NLAYS3D = ', I5 / )

      END    ! Logical function PINTERPB
