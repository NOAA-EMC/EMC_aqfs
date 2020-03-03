
      LOGICAL FUNCTION CKTFLAG3( FID, VID, 
     &                           JDATE, JTIME, TSTEP, NSTEPS,
     &                           JSTEP, DELTA )

C***********************************************************************
C Version "$Id: cktflag3.f 1 2014-03-14 20:22:54Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2013 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  81
C
C  FUNCTION:
C       reads and checks time step flags for file # FID and 
C	variable VID for the time series starting at date and time 
C	JDATE (coded YYYYDDD) and time JTIME (HHMMSS), and time interval
C	TSTEP (HHMMSS).
C       For time-independent files, JDATE:JTIME:TSTEP are ignored.
C       If VID is -1, checks all variables.
C
C  RETURN VALUE:  TRUE iff the operation succeeds.
C
C  PRECONDITIONS REQUIRED:  (FID,VID) valid.
C
C  REVISION  HISTORY:  
C       prototype 5/1996 by CJC
C       revised   6/1999 by CJC:  OpenMP thread-safety
C       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
C       Modified 01/20013 by CJC: Fix possible integer log-output overflow
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file subscript  for STATE3 arrays
        INTEGER, INTENT(IN   ) :: VID             !  vble subscripts for STATE3 arrays
        INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        INTEGER, INTENT(IN   ) :: TSTEP           !  time step
        INTEGER, INTENT(IN   ) :: NSTEPS          !  number of steps
        INTEGER, INTENT(  OUT) :: JSTEP           !  starting step number
        INTEGER, INTENT(  OUT) :: DELTA           !  time-step increment


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: JSTEP3          !  compute time step record numbers
        INTEGER, EXTERNAL :: TIME2SEC        !  HHMMSS ~~> seconds


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         VAR             !  subscripts for STATE3 arrays
        INTEGER         STEP            !  time step record number
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        INTEGER         FLAGS( 2,MXVARS3 )!  values array from NCVGT()
        INTEGER         FLAG1, FLAG2    !  test values for FLAGS
        INTEGER         VV3             !  file vble-count
        INTEGER         TS3             !  file time step
        INTEGER         DT, DT3         !  time steps (in secs)
	LOGICAL         EFLAG


C***********************************************************************
C   begin body of function  CKTFLAG3
C...........   Compute record number, and check availability:

        STEP = JSTEP3( JDATE, JTIME,
     &                 SDATE3( FID ),
     &                 STIME3( FID ),
     &                 TSTEP3( FID ) )

        EFLAG = .FALSE.

!$OMP   CRITICAL( S_NC )

        IF ( STEP .LT. 0 ) THEN
            WRITE( LOGDEV,91030 )
     &          'Time step error reading file:  ' // FLIST3( FID ) ,
     &          'Requested date & time:    ', JDATE, JTIME ,
     &          'File starting date & time:',
     &          SDATE3( FID ), STIME3( FID ),
     &          'File time step:           ', TSTEP3( FID )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  check on step number


        TS3 = TSTEP3( FID )
        IF ( TS3 .NE. 0 ) THEN

            IF ( TS3 .GT. 0 ) THEN
                JSTEP = 1 + MOD( STEP - 1, 2 )
            ELSE
                JSTEP = STEP
            END IF

            FLAG1 = JDATE
            FLAG2 = JTIME
            IF ( NSTEPS .GT. 1 ) THEN

                DT  = TIME2SEC( TSTEP )
                DT3 = TIME2SEC(TSTEP3( FID ) )
                IF ( MOD( DT, DT3 ) .NE. 0 ) THEN
                    WRITE( LOGDEV,91031 )
     &                  'Time step error reading file:  ' // 
     &                  FLIST3( FID ) ,
     &                  'Requested time step:', TSTEP,
     &                  'File      time step:', TS3
                    EFLAG = .TRUE.
                    GO TO 999
                END IF

            ELSE IF ( NSTEPS .EQ. 1 ) THEN

                DELTA = 1

            ELSE
                    WRITE( LOGDEV,91032 )
     &                  'Time step error reading file:  ' // 
     &                  FLIST3( FID ) ,
     &                  'Requested number of steps:', NSTEPS
                    EFLAG = .TRUE.
                    GO TO 999

            END IF    	!  if nsteps >1, or else =1, or not

        ELSE    ! tstep3( fid ) = 0


            IF ( NSTEPS .NE. 1 ) THEN

                WRITE( LOGDEV,91030 )
     &              'Time step error reading file:  ' // FLIST3( FID ) ,
     &              'Requested number of steps:               ', NSTEPS,
     &              'Number of steps in time independent file:', 0
                EFLAG = .TRUE.
                GO TO 999

            END IF

            JSTEP = STEP
            FLAG1 = 0
            FLAG2 = 0
        END IF
        
        DIMT( 1 ) = 1           !  field:  date or time
        DELT( 1 ) = 2           !  extent:  entire field
        DIMT( 3 ) = JSTEP       !  timestep dimension
        DELT( 3 ) = 1       !  extent in timestep dimension
        
        IF ( VID .GT. 0 ) THEN  !  reading just one variable

            DIMT( 2 ) = VID     !  variable-number
            DELT( 2 ) = 1       !  extent:  one variable

            DO  11  STEP = 1, NSTEPS
            
                CALL NCVGT( CDFID3( FID ), TINDX3( FID ), 
     &                      DIMT, DELT, FLAGS( 1,VID ), IERR )
                IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                    WRITE( LOGDEV,91020 )
     &              'Reading ' // VLIST3( VID,FID ) // 
     &              ' -- date & time:', FLAG1, FLAG2,
     &              'Not yet written in file ' // FLIST3( FID )
                    EFLAG = .TRUE.
                    GO TO 999

                ELSE IF ( IERR .NE. 0 ) THEN

                    WRITE( LOGDEV,91020 )
     &                  'Error reading netCDF time step flag for ' //
     &                  VLIST3( VID,FID ) // ' from ' // FLIST3( FID ), 
     &                  'Date and time', FLAG1, FLAG2,
     &                  'netCDF error number', IERR

                    EFLAG = .TRUE.
                    GO TO 999

                ELSE  IF(  FLAGS( 1,VID ) .NE. FLAG1 .OR.
     &                     FLAGS( 2,VID ) .NE. FLAG2 ) THEN

                        WRITE( LOGDEV,91020 )
     &                      'Requested date & time:', FLAG1, FLAG2,
     &                      'Variable ' // VLIST3( VID,FID ) // 
     &                      ' not available in file ' // FLIST3( FID )
                   
                        EFLAG = .TRUE.
                        GO TO 999
                   
                    END IF          !  if ierr bad or if timestep flags bad
         
                CALL NEXTIME( FLAG1, FLAG2, TSTEP )
                DIMT( 3 ) = DIMT( 3 ) + 1

11          CONTINUE	!  end loop on steps
                   
        ELSE            !  reading all variables
        
            VV3 = MAX( 1, NVARS3( FID ) )
            DIMT( 2 ) = 1               !  initial variable-number
            DELT( 2 ) = VV3             !  extent:  all variables
        
            DO  33  STEP = 1, NSTEPS
            
                CALL NCVGT( CDFID3( FID ), TINDX3( FID ), 
     &                      DIMT, DELT, FLAGS, IERR )

                IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                    WRITE( LOGDEV,91020 )
     &                  'Error reading netCDF time step flag for ' //
     &                  'ALL VBLES from ' // FLIST3( FID ), 
     &                  'Date and time', FLAG1, FLAG2,
     &                  'not yet written.'
                    EFLAG = .TRUE.
                    GO TO 999

                ELSE IF ( IERR .NE. 0 ) THEN

                    WRITE( LOGDEV,91020 )
     &                  'Error reading netCDF time step flag for ' //
     &                  'ALL VBLES from ' // FLIST3( FID ), 
     &                  'Date and time', FLAG1, FLAG2,
     &                  'netCDF error number', IERR
                    EFLAG = .TRUE.
                    GO TO 999

                END IF          !  if ierr nonzero or not for NCVGT1()


C...........   Check time step flags for all variables:

                IF ( NVARS3( FID ) .GT. 0 ) THEN

                    DO  22  VAR = 1, VV3

                        IF ( FLAGS( 1,VAR ) .NE. FLAG1  .OR.
     &                       FLAGS( 2,VAR ) .NE. FLAG2  ) THEN

                            WRITE( LOGDEV,91020 )
     &                      'Reading ALL variables -- ' //
     &                      'requested date & time:', 
     &                      JDATE, JTIME ,
     &                      'Time step not available in file ' // 
     &                      FLIST3( FID ) //
     &                      ' for variable ' // VLIST3( VAR, FID )

                            EFLAG = .TRUE.
                            GO TO 999

                        END IF          !  if bad flag value

22                  CONTINUE	!  end loop on user-variables VAR

                ELSE	!  nvars zero (structured no-user-vble time step)

                    IF ( FLAGS( 1,1 ) .NE. FLAG1  .OR.
     &                   FLAGS( 2,1 ) .NE. FLAG2  ) THEN

                        WRITE( LOGDEV,91020 )
     &                  'Reading entire time step -- ' //
     &                  'requested date & time:', JDATE, JTIME ,
     &                  'Time step not available in file ' // 
     &                  FLIST3( FID )

                        EFLAG = .TRUE.
                        GO TO 999

                    END IF          !  if bad flag value

                END IF	!  if nvars positive, or not

                CALL NEXTIME( FLAG1, FLAG2, TSTEP )
                DIMT( 3 ) = DIMT( 3 ) + 1

33          CONTINUE	!  end loop on STEP

        END IF          !  if reading one variable or many (checking flags)

999     CONTINUE

!$OMP   END CRITICAL( S_NC )

        CKTFLAG3 = ( .NOT. EFLAG ) 
	RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91020   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',
     &            3 ( /5X , A , :, I9, :, ':' , I6.6 ) // )

91030   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',
     &            /5X , A ,
     &            /5X , A , I9, ':' , I6.6,
     &            /5X , A , I9, ':' , I6.6,
     &            /5X , A , I110 // )

91031   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',
     &            /5X , A ,
     &            /5X , A , I10, 
     &            /5X , A , I10, // )

91032   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',
     &            /5X , A ,
     &            /5X , A , I10, // )

        END FUNCTION CKTFLAG3

