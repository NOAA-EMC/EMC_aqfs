
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      LOGICAL FUNCTION READ3(FNAME, VNAME, LAYER, JDATE, JTIME, BUFFER)

C***********************************************************************
C  Main entry     starts at line  141
C  Entry SNOOP3   starts at line  147
C  Function body  starts at line  157
C
C  FUNCTION:
C       reads data from Models-3 data file with logical name FNAME for
C       variable with name VNAME and layer LAYER, for the date and time
C       JDATE (coded YYYYDDD) and time JTIME (HHMMSS).
C
C       For time-independent files, JDATE:JTIME are ignored.
C
C       If VNAME is 'ALL', reads all variables; if LAYER is -1,
C       reads all layers.
C
C       If FNAME is a dictionary file, READ3() treats VNAME as a
C       dictionary index, and returns  RDDICT3( FNAME, VNAME ).
C
C       If called as SNOOP3(), uses SNOOPTFLAG to repeatedly attempt
C       to read the TFLAG if end-of-file, for SNOOPTRY3 attempts at
C       interval SNOOPSECS3 seconds.
C
C  RETURN VALUE
C       TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a Models-3 data file already opened by OPEN3()
C       If called as SNOOP3, set optional environment variables:
C
C          setenv SNOOPSECS3  <seconds>  (default 60)
C          setenv SNOOPTRY3   <attempts> (default 10)
C
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C
C       Modified  7/1994 by CJC to handle restart files (tstep < 0)
C
C       Modified  8/1994 by CJC to handle BUFFERED "files", and pass
C       VID argument to new RD*() routines (instead of VNAME)
C
C       Modified 10/1994 by CJC to handle new files with write granularity
C       at the level of individual variables.
C
C       Modified  2/1995 by CJC to handle files of type SMATRX3
C
C       Modified  2/1995 by CJC to handle files of type TSRIES3
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes; uses RDTFLAG().
C
C       Modified  7/2003 by CJC:  ENTRY SNOOP3().
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C***********************************************************************

      IMPLICIT  NONE
      LOGICAL   SNOOP3

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME           !  logical file name
        CHARACTER*(*)   VNAME           !  variable name, or 'ALL'
        INTEGER         LAYER           !  layer number, or 0
        INTEGER         JDATE           !  date, formatted YYYYDDD
        INTEGER         JTIME           !  time, formatted HHMMSS
        REAL            BUFFER(*)       !  input buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INIT3      !  initialize I/O API
        INTEGER         INDEX1     !  look up names in name tables
        LOGICAL         RDDICT3    !  read records for    DCTNRY3  files
        LOGICAL         RDBUF3     !  read time steps for BUFFERED files
        LOGICAL         RDCUSTOM   !  read time steps for CUSTOM3  files
        LOGICAL         RDGRDDED   !  read time steps for GRDDED3  files
        LOGICAL         RDBNDARY   !  read time steps for BNDARY3  files
        LOGICAL         RDIDDATA   !  read time steps for IDDATA3  files
        LOGICAL         RDPROFIL   !  read time steps for PROFIL3  files
        LOGICAL         RDGRNEST   !  read time steps for GRNEST3  files
        LOGICAL         RDSMATRX   !  read time steps for SMATRX33  files
        LOGICAL         RDTFLAG    !  check time step record availability
        INTEGER         TRIMLEN    !  trimmed string length

        EXTERNAL        INIT3, INDEX1, RDDICT3, RDBUF3, RDGRDDED,
     &                  RDBNDARY, RDIDDATA, RDGRNEST, RDSMATRX,
     &                  RDTFLAG, TRIMLEN
        EXTERNAL        INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript  for STATE3 arrays
        INTEGER         VID, VAR        !  subscripts for STATE3 arrays
        INTEGER         V               !  loop counters
        INTEGER         STEP            !  record number
        LOGICAL         AFLAG, EFLAG, SFLAG
        CHARACTER*16    SNAME           !  scratch file-name buffer
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*256   MESG

#ifdef IOAPICPL
        INTEGER         COUNT, SKIP, TYPE
        INTEGER         F1, F2
        LOGICAL         VFLAG
        LOGICAL         READ3V
        EXTERNAL        READ3V
#endif

C***********************************************************************
C   main entry of function, when called as  READ3

        SFLAG = .FALSE.         !  initialization:  "READ3-call"
        SNAME = 'READ3'
        GO TO  11

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        ENTRY SNOOP3(FNAME, VNAME, LAYER, JDATE, JTIME, BUFFER)

        SFLAG = .TRUE.
        SNAME = 'SNOOP3'

        GO TO  11               !  to head of main body

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C.......   Check that the I/O API has been initialized:

11      CONTINUE                !  head of body of routine

        FLEN  = TRIMLEN( FNAME )
        VLEN  = TRIMLEN( VNAME )
        FID   = NAME2FID( FNAME )

        EFLAG = ( FID .LE. 0 )

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( SNAME, JDATE, JTIME, MESG )
            GO TO  99
        END IF

        FIL16 = FNAME
        VAR16 = VNAME

C.......   Check availability of requested  layer, variable:

        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

            EFLAG = ( .NOT. RDDICT3( FID, VAR16 ) )
            GO TO  99

        END IF

        IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE

            VID = INDEX1 ( VAR16, NVARS3( FID ), VLIST3( 1,FID ) )

            IF ( VID .EQ. 0 ) THEN
                 MESG = 'File ' // FLIST3( FID )//' contains variables'
                 CALL M3MSG2( MESG )
                 DO  VAR=1,NVARS3( FID ), 3
                    WRITE( MESG,91040 )
     &              (VLIST3( V,FID ),V=VAR,MIN( VAR+2, NVARS3( FID ) ) )
                    CALL M3MSG2( MESG )
                 END DO
                 MESG = 'Requested variable "' //  VAR16 //
     &                  '" not available'
                CALL M3WARN( SNAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                GO TO  99
            END IF

        END IF          !  end check on VNAME

        IF ( ( LAYER .NE. ALLAYS3 )
     &      .AND. ( LAYER .LT. 1  .OR.
     &              LAYER .GT. NLAYS3( FID ) ) ) THEN

            WRITE( MESG,91010 ) 'Requested layer:', LAYER
            CALL M3MSG2( MESG )
	    WRITE( MESG,91010 )
     &          'Layers in the file:  1 ...', NLAYS3( FID )
            CALL M3MSG2( MESG )
            MESG = 'LAYER not available in file ' // FIL16
            CALL M3WARN( SNAME, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO  99

        END IF          !  end check on layer-number


C.......   If buffered file, use RDBUF3():

        IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  BUFFERED "file"

            AFLAG = RDBUF3( FID, VID, LAYER, JDATE, JTIME, BUFFER )
            EFLAG = ( .NOT.AFLAG )
            GO TO  99

C.......   If virtual file, use READ3V():

#ifdef IOAPICPL

        ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"

            IF ( TSTEP3( FID ) .NE. 0 ) THEN
                F1    = JDATE
                F2    = JTIME
            ELSE
                F1    = 0
                F2    = 0
            END IF

            IF ( LAYER .EQ. ALLAYS3 ) THEN
               COUNT = BSIZE3(FID)*NLAYS3(FID)
               SKIP = 0
            ELSE
               COUNT = BSIZE3(FID)
               SKIP = (LAYER - 1) * BSIZE3(FID)
            ENDIF

            IF ( VID .EQ. ALLAYS3 ) THEN
                AFLAG = .TRUE.
                DO VID = 1, NVARS3(FID)
                    TYPE = VTYPE3(VID,FID)
                    VFLAG = READ3V( PLIST3(FID), VLIST3(VID,FID),
     &                       SKIP, F1, F2,
     &                       BUFFER(1+(VID-1)*BSIZE3(FID)*NLAYS3(FID)),
     &                       COUNT, TYPE )
                    AFLAG = ( EFLAG .AND. VFLAG )
                END DO
            ELSE
                TYPE  = VTYPE3(VID,FID)
                AFLAG = READ3V( PLIST3(FID), VNAME,
     &                          SKIP, F1, F2, BUFFER,
     &                          COUNT, TYPE )
            END IF

            EFLAG = ( .NOT.AFLAG )
            GO TO  99

#endif

        END IF                  !  if buffered, or virtual, or volatile


C...........   Compute record number, and check availability:
C...........   Note:  rdtflag() calls NCSNC()

        IF ( SFLAG ) THEN

            IF ( .NOT.SNOOPTFLAG( FID, VID, JDATE, JTIME, STEP ) ) THEN

                MESG = 'Time step not available for file:  ' // FIL16
                CALL M3WARN( SNAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                GO TO  99

            END IF

        ELSE IF ( .NOT.  RDTFLAG( FID, VID, JDATE, JTIME, STEP ) ) THEN

            MESG = 'Time step not available for file:  ' // FIL16
            CALL M3WARN( SNAME, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO  99

        END IF


C...........   Read data from file into BUFFER()

        IF ( FTYPE3( FID ) .EQ. CUSTOM3 ) THEN

            AFLAG = RDCUSTOM( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3 ) THEN

            AFLAG = RDGRDDED( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN

            AFLAG = RDBNDARY( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. IDDATA3 ) THEN

            AFLAG = RDIDDATA( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. PROFIL3 ) THEN

            AFLAG = RDPROFIL( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. GRNEST3 ) THEN

            AFLAG = RDGRNEST( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. SMATRX3 ) THEN

            AFLAG = RDSMATRX( FID, VID, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. TSRIES3 ) THEN

            AFLAG = RDGRDDED( FID, VID, LAYER, STEP, BUFFER )

        ELSE IF ( FTYPE3( FID ) .EQ. PTRFLY3 ) THEN

            AFLAG = RDGRDDED( FID, VID, LAYER, STEP, BUFFER )

        ELSE    !  illegal file type

            WRITE( MESG,91010 )
     &      'File:  ' // FIL16 // ' of unknown type:', FTYPE3( FID )
            CALL M3WARN( SNAME, JDATE, JTIME, MESG )
            AFLAG = .FALSE.

        END IF          !  if file type dictionary, custom, gridded, ...
        
        EFLAG = ( .NOT. AFLAG )


99      CONTINUE

        IF ( SFLAG ) THEN
            SNOOP3 = (.NOT. EFLAG )
        ELSE
            READ3  = (.NOT. EFLAG )
        END IF

        RETURN

C-=-=-=-=-=-=-=-=-=-  FORMAT  STATEMENTS   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( 5A )

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

91020   FORMAT ( A , I9, ':' , I6.6, :, A )

91030   FORMAT ( A , I6.6 )

91040   FORMAT ( 3 ( '"', A16, '"', :, 4X ) )

        END

