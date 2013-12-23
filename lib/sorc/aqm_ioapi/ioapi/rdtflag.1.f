
C.........................................................................
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL   FUNCTION  RDTFLAG( FID, VID, JDATE, JTIME, STEP )

C***********************************************************************
C  Main entry        starts at line  114
C  Entry SNOOPTFLAG  starts at line  121
C  Function body     starts at line  159
C
C  FUNCTION:
C       returns TRUE with STEP = record number for this time step
C       iff time step for (JDATE,JTIME) and variable with ID=VID is
C       available in file with ID=FID.
C
C       If FID is time independent, only VID is significant (but
C       not JDATE:JTIME).
C
C       If FID is a "list file-set" upon entry, returns the FID of the
C       appropriate "list" entry.
C
C       If called as SNOOPTFLAG, repeatedly tries to read the TFLAG if
C       end-of-file, for SNOOPTRY3 attempts at interval SNOOPSECS3 seconds.
C
C  PRECONDITIONS REQUIRED:
C       FID is the file ID of either a "real" netCDF file or of a
C       "list" file; in either case, for a file already opened by
C       OPEN3().
C
C       VID is the ID for a valid variable in FID, or else
C       is -1 for "all variables".
C
C       For list-files, also returns the FID which contains the actual
C       time step.
C
C       If called as SNOOPTFLAG, set optional environment variables:
C
C          setenv SNOOPSECS3  <seconds>  (default 60)
C          setenv SNOOPTRY3   <attempts> (default 10)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       JSTEP3
C
C  REVISION  HISTORY:
C
C       Adapted  3/2002 by CJC from READ3, READ4D, INTERP3, and CHECK3
C
C       Modified 8/2002 by CJC:  fixed JSTEP3 RESTART-file bug
C
C       Modified 7/2003 by CJC:  improved error-handling; ENTRY SNOOPTFLAG()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type
C***********************************************************************

      IMPLICIT NONE
      LOGICAL  SNOOPTFLAG

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER         FID             !  subscript for file in STATE3 arrays
        INTEGER         VID             !  subscript for vble in STATE3 arrays
        INTEGER         JDATE           !  date (YYYYDDD) for query
        INTEGER         JTIME           !  time (HHMMSS) for query
        INTEGER         STEP            !  time step record number


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         JSTEP3          !  compute time step record numbers
        INTEGER         RDBFLAG         !  for BINFIL3 files
        LOGICAL         SYNCFID

        EXTERNAL        JSTEP3, RDBFLAG, SYNCFID


C...........   SAVED LOCAL VARIABLES and their descriptions:

        INTEGER         SLEEPSECS       !  "snoop" sleep-delay
        DATA            SLEEPSECS  / -1 /
        INTEGER         SLEEPTRY        !  "snoop" max # of tries
        DATA            SLEEPTRY   / -1 /

        SAVE            SLEEPSECS, SLEEPTRY


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, I, V         !  loop counters over files, variables
        INTEGER         FLAG1, FLAG2    !  date:time scratch vbles
        INTEGER         IERR            !  netCDF error status return
        INTEGER         SCNT            !  "snoop" tries
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        INTEGER         FLAGS( 2,MXVARS3 )!  flags from NCVGT()
        LOGICAL         EFLAG
        LOGICAL         SFLAG           !  is this a "snoop"?
        CHARACTER*16    SNAME           !  entry-name as called
        CHARACTER*256   MESG            !  buffer for building error messages

C***********************************************************************
C   main entry of function, when called as  RDTFLAG

        SFLAG = .FALSE.         !  initialization:  "RDTFLAG-call"
        SNAME = 'RDTFLAG'

        GO TO  11               !  to head of main body

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        ENTRY SNOOPTFLAG( FID, VID, JDATE, JTIME, STEP )

        EFLAG = .FALSE.         !  no errors yet...

        IF ( SLEEPSECS .LT. 0 ) THEN

            SLEEPSECS = ENVINT( 'SNOOPSECS3',
     &                          'Snoop delay (secs)', 60, IERR )
            IF ( IERR .GT. 0 ) THEN
                MESG = "Bad environment variable "SNOOPSECS3"'
                CALL M3WARN('SNOOPTFLAG', JDATE, JTIME, MESG )
                EFLAG = .TRUE.
            END IF

            SLEEPTRY = ENVINT( 'SNOOPTRY3',
     &                         'Maximum number of snoop attempts',
     &                         10, IERR )
            IF ( IERR .GT. 0 ) THEN
                MESG = "Bad environment variable "SNOOPTRY3"'
                CALL M3WARN('SNOOPTFLAG', JDATE, JTIME, MESG )
                EFLAG = .TRUE.
            END IF

            IF ( EFLAG ) THEN
                SLEEPSECS  = -2
                SNOOPTFLAG = .FALSE.
                RETURN
            END IF

        END IF          !  if sleepsecs < 0:  first call

        SFLAG = .TRUE.
        SNAME = 'SNOOPTFLAG'

        GO TO  11               !  to head of main body

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

11      CONTINUE                !  head of main body of routine

        EFLAG = .FALSE.         !  no errors yet...


C.......   If list file-set, find which actual file contains this time step:

        IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN     !  list "file set"

            F = IFRST3( MXFILE3 ) - 1
            DO  I = 1, NLIST3( FID )
                F = F + 1
                IF ( BEGRC3( FID ) .LE. STEP .AND.
     &               ENDRC3( FID ) .GE. STEP ) THEN
                    FID   = ILIST3( F )
                    GO TO  22
                END IF
            END DO

            !!  if you get to here:  data not available in this file-set

            WRITE( MESG,91020 )
     &              'Requested date & time:', JDATE, JTIME
            CALL M3MSG2( MESG )
            MESG = 'Variable ' // VLIST3( VID,FID ) //
     &             ' not available in file-set ' // FLIST3( FID )
            CALL M3WARN( SNAME, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO  99

22          CONTINUE

        END IF                  !  if cdfid3(fid) = lstfil3


C...........   Compute record number, and check availability:

        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

            STEP = VID

        ELSE

            STEP = JSTEP3( JDATE, JTIME,
     &                     SDATE3( FID ),
     &                     STIME3( FID ),
     &                     ABS( TSTEP3( FID ) ) )

            IF ( STEP .LT. 0 ) THEN

                MESG = 'Time step error reading file:  ' // FLIST3(FID)
                CALL M3MSG2( MESG )
                WRITE( MESG,91020 )
     &              'Requested date & time:    ', JDATE, JTIME
                CALL M3MSG2( MESG )
                WRITE( MESG,91020 )
     &              'File starting date & time:',
     &              SDATE3( FID ), STIME3( FID )
                CALL M3MSG2( MESG )
                WRITE( MESG,91030 )
     &              'File time step:           ', TSTEP3( FID )
                CALL M3MSG2( MESG )
                MESG = 'Time step error reading file:  ' // FLIST3(FID)
                CALL M3WARN( SNAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                GO TO  99

            END IF          !  check on step number

            IF ( TSTEP3( FID ) .LT. 0 ) THEN
                STEP  = 1 + MOD( STEP - 1, 2 )
                FLAG1 = JDATE
                FLAG2 = JTIME
            ELSE IF ( TSTEP3( FID ) .GT. 0 ) THEN
                STEP  = STEP
                FLAG1 = JDATE
                FLAG2 = JTIME
            ELSE    ! tstep3( fid ) = 0
                FLAG1 = 0
                FLAG2 = 0
            END IF

        END IF          ! if dictionary-file, or not

        SCNT  = 0

33      CONTINUE        !  head of "snoop" loop

        IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk

            IF ( .NOT. SYNCFID( FID ) ) THEN

                MESG = 'Error with disk synchronization for file:  '
     &                 // FLIST3( FID )
                CALL M3WARN( SNAME, JDATE, JTIME, MESG )
                RDTFLAG = .FALSE.
                RETURN

            END IF              !  if synch failed

        END IF                  !  if file is volatile


C.......   Deal with netCDF, native-binary-layer BINFIL3 files:

        IF ( VID .GT. 0 ) THEN  !  reading just one variable

            DIMT( 2 ) = VID     !  variable-number
            DELT( 2 ) = 1       !  extent:  one variable (also loop count)

        ELSE            !  reading all variables

            DIMT( 2 ) = 1               !  initial variable-number
            DELT( 2 ) = MAX( 1, NVARS3( FID ) )   !  extent:  all variables

        END IF

        EFLAG = .FALSE.         !  no errors yet...

        IF ( CDFID3( FID ) .GE. 0 ) THEN          !  netCDF file:

            DIMT( 1 ) = 1           !  field:  date or time
            DELT( 1 ) = 2           !  extent:  entire field
            DIMT( 3 ) = STEP        !  timestep dimension
            DELT( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), TINDX3( FID ),
     &                  DIMT, DELT, FLAGS, IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                IF ( SFLAG .AND. SCNT .LT. STRY ) THEN
                    SCNT = SCNT + 1
                    GO TO  33
                END IF  

                EFLAG = .TRUE.


            ELSE IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,'( A, I7 )' ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading netCDF time step flag for ' //
     &                 FLIST3( FID )
                CALL M3WARN( SNAME, FLAG1, FLAG2, MESG )
                EFLAG = .TRUE.

            END IF

        ELSE IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN       ! BINFIL3 file:

!$OMP CRITICAL( S_NC )
            IERR = RDBFLAG( FID, VID, STEP, FLAGS )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .EQ. 0 ) THEN

                IF ( SFLAG .AND. SCNT .LT. STRY ) THEN
                    SCNT = SCNT + 1
                    GO TO  33
                END IF  

                EFLAG = .TRUE.


            END IF          !  if ierr nonzero or not for NCVGT1()

        END IF          !  if netCDF file; else if BINIO3 file


C...........   Check time step flags for all variables:

        IF ( .NOT. EFLAG ) THEN !  no errors

            DO  V = 1, DELT( 2 )

                IF ( FLAGS( 1,V ) .NE. FLAG1  .OR.
     &               FLAGS( 2,V ) .NE. FLAG2  ) THEN

                    MESG = 'Time step not available in file ' //
     &                      FLIST3( FID ) // ' for variable ' // 
     &                      VLIST3( V,FID )
                    CALL M3WARN( SNAME, FLAG1, FLAG2, MESG )
                    EFLAG = .TRUE.

                END IF          !  if bad flag value

            END DO

        END IF          !  if no errors


99      CONTINUE

        IF ( SFLAG ) THEN
            SNOOPTFLAG = (.NOT. EFLAG )
        ELSE
            RDTFLAG    = (.NOT. EFLAG )
        END IF

        RETURN

C-=-=-=-=-=-=-=-=-=-  FORMAT  STATEMENTS   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

C...........   Error and warning message formats..... 91xxx

91020   FORMAT ( A , I9, ':' , I6.6, :, A )

91030   FORMAT ( A , I6.6 )

        END

