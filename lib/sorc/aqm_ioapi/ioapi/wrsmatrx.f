
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION WRSMATRX( FID, TSTAMP, STEP2, BUFFER )

C***********************************************************************
C  function body starts at line  68
C
C  FUNCTION:  writes data from Models-3 SMATRIX data file with STATE3
C             index FID, for alll variables for time step record TSTAMP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is SMATRX3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
C
C  REVISION  HISTORY:
C       prototype  2/1995 by CJC
C       Version    2/2002 by CJC: OpenMP thread-safety
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER       FID             !  file index within the STATE3 commons
        INTEGER       TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER       STEP2           !  file record number (maybe mod 2)
        REAL          BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL       WRVARS     !  write "variables" part of timestep record
        EXTERNAL      WRVARS

        INTEGER       WRBSMATRX, WRBFLAG
        EXTERNAL      WRBSMATRX, WRBFLAG

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IERR            !  netCDF error status return
        INTEGER       INDX            !  subscript location in BUFFER(*)
        INTEGER       DELTA           !  d(INDX) / d(NCVPTcall)
        INTEGER       DIMS ( 5 )      !  corner arg array for NCVPT()
        INTEGER       DELTS( 5 )      !  corner arg array for NCVPT()
	LOGICAL       EFLAG


C***********************************************************************
C   begin body of function  WRSMATRX

        WRSMATRX = .TRUE.

        IF ( CDFID3( FID )  .EQ. BINFIL3 ) THEN    ! native-binary file

c           IERR = WRBSMATRX( FID, NROWS3(FID), NCOLS3(FID), BUFFER )
            WRSMATRX = WRVARS( FID, ALLAYS3, TSTAMP, STEP2,
     &                         DIMS, DELTS, DELTA, BUFFER )

c           IF ( IERR .EQ. 1 ) THEN
c              WRITE( LOGDEV,91010 )
c    &         'Error writing max-col-count array to file '
c    &         // FLIST3( FID )
c              WRSMATRX = .FALSE.
c              GO TO 998
c           ELSE IF ( IERR .EQ. 2 ) THEN
c              WRITE( LOGDEV,91010 )
c    &         'Error writing column-index array to file '
c    &         // FLIST3( FID )
c              WRSMATRX = .FALSE.
c              GO TO 999
c           END IF          !  ierr nonzero:  NCVPTC() failed

            IERR = WRBFLAG ( FID, 0, STEP2, TSTAMP )

            IF ( IERR .EQ. 0 ) THEN
               WRITE( LOGDEV,91010 )
     &         'Error writing time step data to file'
     &         // FLIST3( FID )
               WRSMATRX = .FALSE.
               GO TO 998
            END IF
 998        RETURN
        END IF

        EFLAG = .FALSE.

C.......   Write the max-col-count array for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP2
        DELTS( 2 ) = 1

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )

        CALL NCVPT( CDFID3( FID ), SINDX3( FID ),
     &              DIMS, DELTS, BUFFER, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &      'netCDF error number', IERR,
     &      'Error writing max-col-count array to file '//FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPTC() failed


C.......   Write the column-index array this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = STEP2
        DELTS( 2 ) = 1

        INDX = 1 +  NROWS3( FID )

        CALL NCVPT( CDFID3( FID ), LINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &      'netCDF error number', IERR,
     &      'Error writing column-index array to file '//FLIST3( FID )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCVPTC() failed

999     CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

        IF ( EFLAG ) THEN
            WRSMATRX = .FALSE.
            RETURN
	END IF

C...........   Perform the writes of the "variables" part of the data:

        DELTA = NCOLS3( FID )
        INDX  = INDX  +  DELTA

        WRSMATRX = WRVARS( FID, ALLAYS3, TSTAMP, STEP2,
     &                     DIMS, DELTS, DELTA,
     &                     BUFFER ( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine WRSMATRX <<<',
     &            ( /5X , A , : ) , I5, // )


        END

