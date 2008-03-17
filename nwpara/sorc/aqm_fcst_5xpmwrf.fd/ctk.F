
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/cmaq/rel/models/CCTM/src/vadv/vppm/zadvppm.F,v 1.1.1.1 2002/06/27 11:25:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CTK ( JDATE, JTIME, TSTEP, ISTEP, LOGDEV )

C-----------------------------------------------------------------------
C Function: chemistry-transport computational kernel - worker tasks

C Preconditions:

C Subroutines and functions called: SCIPROC

C Revision History:
C   Feb 03 - Jeff, Dave Wong: created
C-----------------------------------------------------------------------

      USE CGRID_DEFN          ! inherits GRID_CONF and CGRID_SPCS
      USE DDEP_DEFN
      USE ACONC_DEFN
      USE WDEP_DEFN
      USE VIS_DEFN
      USE WVEL_DEFN           ! derived vertical velocity component
      USE MPIM

      IMPLICIT NONE

      INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
      INCLUDE SUBST_IODECL    ! I/O definitions and declarations
      INCLUDE SUBST_IOFDESC   ! file header data structure
      INCLUDE SUBST_FILES_ID  ! file name parameters
!     INCLUDE SUBST_VGRD_ID   ! vertical dimensioning parameters
      INCLUDE SUBST_AE_SPC    ! aerosol species table
      INCLUDE 'TAG.EXT'

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP( 2 ), ISTEP, LOGDEV

      LOGICAL, EXTERNAL :: ENVYN

      INTEGER MDATE, MTIME, NDATE, NTIME
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: CSZE, DSZE, ASZE, WSZE, LSZE, PSZE
      INTEGER IREP, NREPS
      INTEGER ASTAT, ERROR, STATUS( MPI_STATUS_SIZE )
      INTEGER, SAVE :: TAG, CONC_REQ, DDEP_REQ, ACONC_REQ,
     &                      WDEP_REQ, CLDD_REQ, AVIS_REQ
      INTEGER IERR
      INTEGER, ALLOCATABLE, SAVE :: ASTEP( : )
      INTEGER C, R, L, K, S, V

      CHARACTER( 16 ) :: CTM_CKSUM = 'CTM_CKSUM'     ! env var for cksum on
      LOGICAL, SAVE   :: CKSUM     ! flag for cksum on, default = [F]

      CHARACTER( 16 ) :: PNAME = 'CTK'
      CHARACTER( 120 ) :: XMSG = ' '

      REAL, ALLOCATABLE, SAVE :: JACF( :,:,: ) ! full-layer Jacobian
      REAL, ALLOCATABLE, SAVE :: MSFX2( :,: )  ! map scale factor ** 2
      INTEGER      ALLOCSTAT

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

      INTERFACE
         SUBROUTINE ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )
            IMPLICIT NONE
            INTEGER, INTENT( IN )  :: JDATE, JTIME
            INTEGER, INTENT( IN )  :: TSTEP( 2 )
            INTEGER, INTENT( OUT ) :: ASTEP( : )
            INTEGER, INTENT( OUT ) :: NREPS
         END SUBROUTINE ADVSTEP
         SUBROUTINE SCIPROC ( JDATE, JTIME, TSTEP, ASTEP )
            IMPLICIT NONE
            INTEGER, INTENT( IN )  :: JDATE, JTIME
            INTEGER, INTENT( IN )  :: TSTEP( 2 ), ASTEP( : )
         END SUBROUTINE SCIPROC
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         CKSUM = .FALSE.         ! default
         CKSUM = ENVYN( CTM_CKSUM, 'Cksum flag', CKSUM, IERR )
         IF ( IERR .NE. 0 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

C Get number of variables in output files

         IF ( W_VEL ) THEN
            CSZE = MY_NCOLS * MY_NROWS * NLAYS  * ( NSPCSD + 1 )
            ELSE
            CSZE = MY_NCOLS * MY_NROWS * NLAYS  * NSPCSD
            END IF
         DSZE = MY_NCOLS * MY_NROWS * 1      * N_SPC_DEPV
         ASZE = MY_NCOLS * MY_NROWS * A_NLYS * N_ASPCS
         WSZE = MY_NCOLS * MY_NROWS * 1      * N_SPC_WDEPD
         LSZE = MY_NCOLS * MY_NROWS * 1      * ( N_SPC_WDEP + 6 )
!        PSZE = MY_NCOLS * MY_NROWS * 1      * N_AE_VIS_SPC
         PSZE = MY_NCOLS * MY_NROWS * NLAYS  * N_AE_VIS_SPC

         CONC_REQ  = MPI_REQUEST_NULL
         DDEP_REQ  = MPI_REQUEST_NULL
         ACONC_REQ = MPI_REQUEST_NULL
         WDEP_REQ  = MPI_REQUEST_NULL
         CLDD_REQ  = MPI_REQUEST_NULL
         AVIS_REQ  = MPI_REQUEST_NULL

         ALLOCATE ( ASTEP( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ASTEP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( W_VEL ) THEN

            CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                      STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

            ALLOCATE ( JACF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating JACF'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                      STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

            ALLOCATE ( MSFX2( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating MSFX2'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            IF ( .NOT. INTERPX( GRID_CRO_2D, 'MSFX2', PNAME,
     &                          STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                          JDATE, JTIME, MSFX2 ) ) THEN
               XMSG = 'Could not interpolate MSFX2 from ' // GRID_CRO_2D
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

!           ALLOCATE ( DBUFF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
!           IF ( ALLOCSTAT .NE. 0 ) THEN
!              XMSG = 'Failure allocating DBUFF'
!              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!              END IF

            END IF   ! W_VEL

         END IF   ! Firstime

C Make sure sends have completed to keep data buffer coherent

      IF ( CONC_REQ .NE. MPI_REQUEST_NULL )
     &    CALL MPI_WAIT ( CONC_REQ, STATUS, ERROR )

      IF ( DDEP_REQ .NE. MPI_REQUEST_NULL )
     &    CALL MPI_WAIT ( DDEP_REQ, STATUS, ERROR )

      IF ( ACONC_REQ .NE. MPI_REQUEST_NULL )
     &    CALL MPI_WAIT ( ACONC_REQ, STATUS, ERROR )

      IF ( WDEP_REQ .NE. MPI_REQUEST_NULL )
     &    CALL MPI_WAIT ( WDEP_REQ, STATUS, ERROR )

      IF ( CLDD_REQ .NE. MPI_REQUEST_NULL )
     &    CALL MPI_WAIT ( CLDD_REQ, STATUS, ERROR )

C Reinitialize deposition arrays

      IF ( ALLOCATED ( TOT_DEP ) ) THEN
         TOT_DEP  = 0.0
         CONV_DEP = 0.0
         END IF

      IF ( N_AE_SPC .GT. 0 ) THEN
         IF ( AVIS_REQ .NE. MPI_REQUEST_NULL )
     &       CALL MPI_WAIT ( AVIS_REQ, STATUS, ERROR )
         END IF

C Get synchronization and advection time steps, TSTEP(2), TSTEP(3) and NREPS

      CALL ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )

      DO V = 1, N_ASPCS
         S = ACONC_SPC_MAP( V )
         DO K = ACONC_BLEV, ACONC_ELEV
            L = K - ACONC_BLEV + 1
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  AGRID( C,R,L,V ) = CGRID( C,R,K,S )
                  END DO
               END DO
            END DO
         END DO

C science process sequence:

      DO IREP = 1, NREPS - 1

         CALL SCIPROC ( JDATE, JTIME, TSTEP, ASTEP )

         DO V = 1, N_ASPCS
            S = ACONC_SPC_MAP( V )
            DO K = ACONC_BLEV, ACONC_ELEV
               L = K - ACONC_BLEV + 1
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     AGRID( C,R,L,V) = AGRID( C,R,L,V )
     &                               + 2.0 * CGRID( C,R,K,S )
                     END DO
                  END DO
               END DO
            END DO

         END DO

      CALL SCIPROC ( JDATE, JTIME, TSTEP, ASTEP )

      DIVFAC = 0.5 / FLOAT( NREPS )

      DO V = 1, N_ASPCS
         S = ACONC_SPC_MAP( V )
         DO K = ACONC_BLEV, ACONC_ELEV
            L = K - ACONC_BLEV + 1
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  AGRID( C,R,L,V ) = DIVFAC * ( AGRID( C,R,L,V )
     &                             +            CGRID( C,R,K,S ) )
                  END DO
               END DO
            END DO
         END DO

C Start synchronous mode, non-blocking sends

      IF ( W_VEL ) THEN
C convert from contravariant vertical velocity component to true wind
         IF ( .NOT. INTERPX( MET_CRO_3D, 'JACOBF', PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                       1,NLAYS, JDATE, JTIME, JACF ) ) THEN
!           XMSG = 'Could not interpolate JACOBF from MET_CRO_3D - '
!    &           // 'Using JACOBM <- KLUDGE!'
!           CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            XMSG = 'Could not interpolate JACOBM from MET_CRO_3D'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!           IF ( .NOT. INTERPX( MET_CRO_3D, 'JACOBM', PNAME,
!    &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
!    &                       1,NLAYS, JDATE, JTIME, DBUFF ) ) THEN
!              XMSG = 'Could not interpolate JACOBM from MET_CRO_3D'
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!              END IF
!           DO L = 1, NLAYS - 1
!              DO R = 1, MY_NROWS
!                 DO C = 1, MY_NCOLS
!                    JACF( C,R,L ) = 0.5 * ( DBUFF( C,R,L )
!    &                                   +   DBUFF( C,R,L+1 ) )
!                    END DO
!                 END DO
!              JACF( C,R,NLAYS ) = 0.4 * JACF( C,R,NLAYS-1 )
!    &                           + DBUFF( C,R,NLAYS )
!              END DO
            END IF

C load WVEL into CGRID_X - assumed to be the last variable (NSPCSD+1)
         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CGRID_X( C,R,L,NSPCSD+1 ) = JACF( C,R,L ) * MSFX2( C,R )
     &                                      * WVEL( C,R,L )
                  END DO
               END DO
            END DO

         END IF   ! W_VEL

      TAG = G_MYPE * TAGFAC + CONC_TAG + ISTEP
!     write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
      IF ( W_VEL ) THEN
         CALL MPI_ISSEND ( CGRID_X, CSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                     CONC_REQ, ERROR )
         ELSE
!        CALL MPI_ISSEND ( CGRID, CSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
!    &                     CONC_REQ, ERROR )
         CALL MPI_SEND ( CGRID, CSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                   ERROR )
         END IF

      TAG = G_MYPE * TAGFAC + DDEP_TAG + ISTEP
!     write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
      CALL MPI_ISSEND ( DDEP, DSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                  DDEP_REQ, ERROR )

      TAG = G_MYPE * TAGFAC + ACONC_TAG + ISTEP
!     write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
      CALL MPI_ISSEND ( AGRID, ASZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                  ACONC_REQ, ERROR )

C Check if it's time to write the wet deposition file
CCCCCCCc PROBABLY DON'T NEED THIS IF TSTEP(1) = 1 HOUR

!     write( logdev,* ) '    ctk - cloud timing:'
!     write( logdev,* ) ' Jdate, Jtime: ', JDATE, JTIME

      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME ( MDATE, MTIME, TSTEP( 2 ) )
      NDATE = JDATE
      NTIME = 10000 * ( JTIME / 10000 )
      CALL NEXTIME ( NDATE, NTIME, 10000 ) ! set Ndate:Ntime to next hour

!     write( logdev,* ) ' Mdate, Ndate, Mtime, Ntime: ',
!    &                    MDATE, NDATE, MTIME, NTIME

!     DO R = 1, MY_NROWS
!        DO C = 1, MY_NCOLS
!           DO V = 1, N_SPC_WDEP + 1
!              TOT_DEP( V,C,R ) = TOT_DEP ( V,C,R ) + CONV_DEP( V,C,R )
      DO V = 1, N_SPC_WDEP + 1
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               TOT_DEP( C,R,V ) = TOT_DEP ( C,R,V ) + CONV_DEP( C,R,V )
               END DO
            END DO
         END DO

      TAG = G_MYPE * TAGFAC + WDEP_TAG + ISTEP
!     write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
      CALL MPI_ISSEND ( TOT_DEP, WSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                  WDEP_REQ, ERROR )

      IF ( CLD_DIAG ) THEN
         TAG = G_MYPE * TAGFAC + CLDD_TAG + ISTEP
!        write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
         CALL MPI_ISSEND ( CONV_DEP, LSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                     CLDD_REQ, ERROR )
         END IF

C Reinitialize deposition arrays

!     TOT_DEP  = 0.0
!     CONV_DEP = 0.0

      IF ( N_AE_SPC .GT. 0 ) THEN
         TAG = G_MYPE * TAGFAC + AVIS_TAG + ISTEP
!        write( logdev,* ) '       ctk - ISTEP, TAG: ', istep, tag
         CALL MPI_ISSEND ( VIS_SPC, PSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                     AVIS_REQ, ERROR )
         END IF
      RETURN

      END SUBROUTINE CTK
