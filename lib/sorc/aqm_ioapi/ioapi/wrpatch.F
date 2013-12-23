
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION WRPATCH( FID, VID, TSTAMP, STEP2, 
     &                            SIZE_MAP, GRID_MAP, BUFFER )

C***********************************************************************
C  function body starts at line  68
C
C  FUNCTION:  writes data from Models-3 GRIDDED data file with STATE3
C             index FID, for all variables and layers, for the time step
C             record STEP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is GRDDED3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRPATCH
C
C  REVISION  HISTORY:  
C       Version 8/2001 WRGRDDED_P adapted from WRGRDDED by David Wong,
C       Lockheed Martin
C
C       Prototype 2/2002 by Carlie J. Coats, Jr, MCNC-EMC:
C       Adapted from WRGRDDED_P and WRPATCH and modified to ensure
C       correctness of time-stamp handling (which must be performed
C       by a separate WRTFLAG call subsequent to completion of all
C       the WRPATCH calls).
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER         FID             !  file index within the STATE3 commons
        INTEGER         VID             !  vble index within the STATE3 commons
        INTEGER         TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER         STEP2           !  file record number (maybe mod 2)
        INTEGER         GRID_MAP(2)     !  starting corners for grid-patch
        INTEGER         SIZE_MAP(3)     !  diagonals for grid-patch
        REAL            BUFFER(*)       !  buffer array for input

C...........   EXTERNAL FUNCTIONs:

        INTEGER         WRBPATCH
        EXTERNAL        WRBPATCH

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS( 5 )       !  corner arg array for NCVGT()
        INTEGER         DELS( 5 )       !  corner arg array for NCVGT()

        INTEGER         IERR            !  netCDF error status return
        INTEGER         VAR             !  loop counter for file variables
        INTEGER         INDX            !  subscript location in BUFFER(*)
        LOGICAL         AFLAG

        INTEGER         TYPSIZE( 6 )    !  sizeof( variable ) / sizeof( real )
#if _CRAY || REAL8
        DATA            TYPSIZE / 1, 1, 1, 1, 1, 1 /
#endif
#if ! ( _CRAY || REAL8 )
        DATA            TYPSIZE / 1, 1, 1, 1, 1, 2 /
#endif


C***********************************************************************
C   begin body of function  WRGRDDED

        IF ( CDFID3( FID )  .EQ. BINFIL3 ) THEN    ! native-binary file

!$OMP CRITICAL( S_LOGOUT )

            AFLAG = .FALSE.
            DELTA = SIZE_MAP(1) * SIZE_MAP(2) * SIZE_MAP(3)

            IF (VID .GT. 0) THEN
               IERR = WRBPATCH( FID, VID, GRID_MAP, SIZE_MAP, STEP2, 
     &                          BUFFER )
            ELSE
               INDX  = 1           !  starting subscript for BUFFER(*)
               DO  VAR = 1 , NVARS3( FID )
                   IERR = WRBPATCH( FID, VAR, GRID_MAP, SIZE_MAP, STEP2, 
     &                              BUFFER (INDX) )
                   IF ( 0 .EQ. IERR ) THEN     !  error
                       AFLAG = .TRUE.
                       GOTO 99
                   END IF
                   INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )
               END DO
            END IF

 99         CONTINUE

!$OMP END CRITICAL( S_LOGOUT )

            IF ( AFLAG ) THEN
!$OMP CRITICAL( S_LOGOUT )
                WRITE( LOGDEV, '( 5X, 5 A )' )
     &                  'Error patch writing variable ', 
     &                  VLIST3( VID,FID ), 
     &                  'to BINIO file ', FLIST3( FID )
                WRITE( LOGDEV,* ) 'IOAPI file ID ', FID
                WRITE( LOGDEV,* ) 'variable      ', VINDX3( VID,FID )
                WRITE( LOGDEV,* )
                WRPATCH = .FALSE.
!$OMP END CRITICAL( S_LOGOUT )
            ELSE
                WRPATCH = .TRUE.
            END IF

            RETURN

        END IF                          !  if native-binary file

        DIMS( 1 ) = GRID_MAP(1) 
        DELS( 1 ) = SIZE_MAP(1)

        DIMS( 2 ) = GRID_MAP(2)
        DELS( 2 ) = SIZE_MAP(2)

        DIMS( 3 ) = 1
        DELS( 3 ) = SIZE_MAP(3)

        DIMS( 4 ) = STEP2
        DELS( 4 ) = 1

        DELTA = SIZE_MAP(1) * SIZE_MAP(2) * SIZE_MAP(3)

C...........   Perform the writes, according to VID

        IF ( VID .GT. 0 ) THEN          !  write just one variable

!$OMP CRITICAL( S_NC )
            CALL NCVPT( CDFID3( FID ), VINDX3( VID,FID ),
     &                  DIMS, DELS, BUFFER, IERR )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
!$OMP CRITICAL( S_LOGOUT )
                WRITE( LOGDEV, '( 5X, A, /, 5X, A, I9 )' )
     &                  'Error writing patch of variable '
     &                  // VLIST3( VID,FID ) //
     &                  'to file ' // FLIST3( FID ) ,
     &                  'netCDF error number', IERR
                WRITE( LOGDEV,* ) 'IOAPI file ID ', FID
                WRITE( LOGDEV,* ) 'netCDF ID     ', CDFID3( FID )
                WRITE( LOGDEV,* ) 'variable      ', VINDX3( VID,FID )
                WRITE( LOGDEV,* ) 'dims array    ', DIMS
                WRITE( LOGDEV,* ) 'delts array   ', DELS
                WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
        
                WRPATCH = .FALSE.
                RETURN
        
            END IF              !  if ierr nonzero:  NCVPT() failed

        ELSE                    !  else write all variables:
        
            INDX  = 1           !  starting subscript for BUFFER(*)
            AFLAG = .TRUE.
            
            DO  VAR = 1 , NVARS3( FID )
                   
!$OMP CRITICAL( S_NC )
                CALL NCVPT( CDFID3( FID ), VINDX3( VAR,FID ),
     &                      DIMS, DELS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
                IF ( IERR .NE. 0 ) THEN
!$OMP CRITICAL( S_LOGOUT )
                    WRITE( LOGDEV, '( 5X, A, /, 5X, A, I9 )' )
     &                  'Error writing patch of variable '
     &                  // VLIST3( VAR,FID ) //
     &                  'to file ' // FLIST3( FID ) ,
     &                  'netCDF error number', IERR
                    WRITE( LOGDEV,* ) 'IOAPI ID    ', FID
                    WRITE( LOGDEV,* ) 'netCDF ID   ', CDFID3( FID )
                    WRITE( LOGDEV,* ) 'vble        ', VINDX3( VAR,FID )
                    WRITE( LOGDEV,* ) 'dims array  ', DIMS
                    WRITE( LOGDEV,* ) 'delts array ', DELS
                    WRITE( LOGDEV,* ) 'offset      ', INDX
                    WRITE( LOGDEV,* ) 'WRITE3 request: ALL VARIABLES'
                    WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
        
                    WRPATCH = .FALSE.
                    RETURN
        
                END IF                  !  ierr nonzero:  NCVPT() failed
                   
                INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )

            END DO              ! end loop on variables VAR
                   
        END IF                  !  if writing just one vble, or all vbles

        WRPATCH = .TRUE.        ! (if you get to here)
        RETURN

        END

