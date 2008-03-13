
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
      SUBROUTINE COLLECT ( JDATE, JTIME, ISTEP, NSTEPS, LOGDEV )

C-----------------------------------------------------------------------
C Function:

C Preconditions:

C Subroutines and functions called: ASSEMBLE

C Revision History:
C   Feb 03 - Jeff, Dave Wong: created
C   May 03 - Jeff, add write start step file
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE DDEP_DEFN
      USE ACONC_DEFN
      USE WDEP_DEFN
      USE VIS_DEFN
      USE MPIM
      USE WVEL_DEFN             ! derived vertical velocity component

      IMPLICIT NONE

!     INCLUDE SUBST_VGRD_ID   ! vertical dimensioning parameters
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/PARMS3.EXT"   ! I/O parameters definitions
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"    ! I/O definitions and declarations
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/FDESC3.EXT"   ! file header data structure
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/FILES_CTM.EXT"  ! file name parameters
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_SPC.EXT"    ! gas chemistry species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/AE_SPC.EXT"    ! aerosol species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/NR_SPC.EXT"    ! non-reactive species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/TR_SPC.EXT"    ! tracer species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_CONC.EXT"   ! gas chem conc file species and map table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/AE_CONC.EXT"   ! aerosol conc file species and map table
      INCLUDE 'TAG.EXT'

      INTEGER, INTENT( IN ) :: JDATE, JTIME, ISTEP, NSTEPS, LOGDEV

      INTEGER       :: CONC_NVARS, CONC_NPE, CONC_GSZE = 0
      INTEGER, SAVE :: CONC_NLVLS, CONC_OFFSET_CR, CONC_FG_NCOLS, CGRD_DSZE = 0
      INTEGER, SAVE :: CGRD_NVARS
      REAL,    ALLOCATABLE, SAVE :: CONC_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: CONC_SPCHIT( : )
      LOGICAL       :: CONC_DONE, CONC_WRITTEN( NPROCS-1 )

      INTEGER       :: DDEP_NVARS, DDEP_NPE, DDEP_GSZE = 0
      INTEGER, SAVE :: DDEP_NLVLS, DDEP_OFFSET_CR, DDEP_FG_NCOLS, DDEP_DSZE = 0
      REAL,    ALLOCATABLE, SAVE :: DDEP_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: DDEP_SPCHIT( : )
      LOGICAL       :: DDEP_DONE, DDEP_WRITTEN( NPROCS-1 )

      INTEGER       :: ACONC_NVARS, ACONC_NPE, ACONC_GSZE = 0
      INTEGER, SAVE :: ACONC_NLVLS, ACONC_OFFSET_CR, ACONC_FG_NCOLS, ACONC_DSZE = 0
      REAL,    ALLOCATABLE, SAVE :: ACONC_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: ACONC_SPCHIT( : )
      LOGICAL       :: ACONC_DONE, ACONC_WRITTEN( NPROCS-1 )

      INTEGER       :: WDEP_NVARS, WDEP_NPE, WDEP_GSZE = 0
      INTEGER, SAVE :: WDEP_NLVLS, WDEP_OFFSET_CR, WDEP_FG_NCOLS, WDEP_DSZE = 0
      REAL,    ALLOCATABLE, SAVE :: WDEP_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: WDEP_SPCHIT( : )
      LOGICAL       :: WDEP_DONE, WDEP_WRITTEN( NPROCS-1 )

      INTEGER       :: CLDD_NVARS, CLDD_NPE, CLDD_GSZE = 0
      INTEGER, SAVE :: CLDD_NLVLS, CLDD_OFFSET_CR, CLDD_FG_NCOLS, CLDD_DSZE = 0
      REAL,    ALLOCATABLE, SAVE :: CLDD_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: CLDD_SPCHIT( : )
      LOGICAL       :: CLDD_DONE, CLDD_WRITTEN( NPROCS-1 )

      INTEGER       :: VIS_NVARS, VIS_NPE, VIS_GSZE = 0
      INTEGER, SAVE :: VIS_NLVLS, VIS_OFFSET_CR, VIS_FG_NCOLS, VIS_DSZE = 0
      REAL,    ALLOCATABLE, SAVE :: VIS_DATA( : )
      LOGICAL, ALLOCATABLE, SAVE :: VIS_SPCHIT( : )
      LOGICAL       :: VIS_DONE, VIS_WRITTEN( NPROCS-1 )

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, EXTERNAL :: FINDEX
      CHARACTER( 16 ) :: PNAME = 'Collect'
      CHARACTER( 96 ) :: XMSG = ' '
      LOGICAL ALL_DONE
      INTEGER ASTAT
!     CHARACTER( 16 ) :: S_STEP_1 = 'S_STEP_1'

C for now assume primary run always starts at 12Z and update run starts at 06Z
      INTEGER, PARAMETER :: SSTEP_06 = 06
      INTEGER, PARAMETER :: SSTEP_12 = 12

      INTEGER MDATE, MTIME 
      INTEGER, SAVE :: TSTEP
      INTEGER C, R, I, V, S
      integer count1, count2, count3, count4, count5, count6
      integer bnce1, bnce2, bnce3, bnce4, bnce5, bnce6

      INTERFACE
         SUBROUTINE ASSEMBLE ( ISTEP, TAG, logdev,
     &                         FG_NCOLS, OFFSET_CR,
     &                         NVARS, NLVLS, DSZE,
     &                         SPC_MAP, WRITTEN, NPE, DATA, bnce )
         IMPLICIT NONE
         INTEGER, INTENT( IN ) :: ISTEP, TAG, logdev,
     &                            FG_NCOLS, OFFSET_CR,
     &                            NVARS, NLVLS, DSZE
         LOGICAL, INTENT( IN ) :: SPC_MAP( : )
         LOGICAL, INTENT( INOUT ) :: WRITTEN( : )
         INTEGER, INTENT( INOUT ) :: NPE, bnce
         REAL,    INTENT( OUT ) :: DATA( : )
         END SUBROUTINE ASSEMBLE
      END INTERFACE

!     integer, parameter :: N_AE_SPCx = 0

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C CONC -----------------------------------------------------------------

         IF ( W_VEL ) THEN
            CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )
            CGRD_NVARS = NSPCSD + 1
            ELSE
            CGRD_NVARS = NSPCSD
            END IF

         IF ( .NOT. OPEN3( CTM_CONC_1, FSREAD3, 'COLLECT' ) ) THEN
            XMSG = 'Error opening CTM_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
            XMSG =  'File description error for CTM_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         CONC_FG_NCOLS  = NCOLS3D
         CONC_OFFSET_CR = NCOLS3D * NROWS3D
         CONC_NLVLS     = NLAYS3D
         CONC_NVARS     = NVARS3D

         CGRD_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * NLAYS * CGRD_NVARS
         CONC_GSZE = NCOLS3D * NROWS3D * CONC_NLVLS * CONC_NVARS

!        write( logdev,* ) ' CGRD_DSZE, CONC_GSZE:   ', CGRD_DSZE, CONC_GSZE

         ALLOCATE ( CONC_DATA( CONC_GSZE ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for CONC_DATA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( CONC_SPCHIT( CGRD_NVARS ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for CONC_SPCHIT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         CONC_SPCHIT = .FALSE.
         V = 0
         DO S = 1, N_GC_CONC
            I = GC_CONC_MAP( S )
            CONC_SPCHIT( V + I ) = .TRUE.
            END DO
         V = V + N_GC_SPC + 1     ! advected RhoJ
         IF ( .NOT. W_VEL ) THEN
            CONC_SPCHIT( V ) = .TRUE.
            END IF
         DO S = 1, N_AE_CONC
            I = AE_CONC_MAP( S )
            CONC_SPCHIT( V + I ) = .TRUE.
            END DO
         V = V + N_AE_SPC
         DO S = 1, N_NR_SPC
            CONC_SPCHIT( V + S ) = .TRUE.
            END DO
         V = V + N_NR_SPC
         DO S = 1, N_TR_SPC
            CONC_SPCHIT( V + S ) = .TRUE.
            END DO
         IF ( W_VEL ) THEN
            V = V + N_TR_SPC + 1    ! derived vert vel comp
            CONC_SPCHIT( V ) = .TRUE.
            END IF

!        do s = 1, CGRD_NVARS
!           write( logdev,* ) '   CONC_SPCHIT: ', s, conc_spchit( s )
!           end do

C DDEP -----------------------------------------------------------------

         IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSREAD3, 'COLLECT' ) ) THEN
            XMSG = 'Error opening CTM_DRY_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( CTM_DRY_DEP_1 ) ) THEN
            XMSG = 'File description error for CTM_DRY_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         DDEP_FG_NCOLS  = NCOLS3D
         DDEP_OFFSET_CR = NCOLS3D * NROWS3D
         DDEP_NLVLS     = NLAYS3D
         DDEP_NVARS     = NVARS3D

         DDEP_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * 1 * N_SPC_DEPV
         DDEP_GSZE = NCOLS3D * NROWS3D * DDEP_NLVLS * DDEP_NVARS

!        write( logdev,* ) ' DDEP_DSZE, DDEP_GSZE:   ', DDEP_DSZE, DDEP_GSZE

         ALLOCATE ( DDEP_DATA( DDEP_GSZE ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for DDEP_DATA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( DDEP_SPCHIT( N_SPC_DEPV ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for DDEP_SPCHIT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         DDEP_SPCHIT = .FALSE.
         V = 0
         DO S = 1, N_GC_DEPV
            V = V + 1
            I = FINDEX ( GC_DEPV_MAP( S ), N_GC_DDEP, GC_DDEP_MAP )
            IF ( I .GT. 0 ) DDEP_SPCHIT( V ) = .TRUE.
            END DO

         DO S = 1, N_AE_DEPV
            V = V + 1
            I = FINDEX ( AE_DEPV_MAP( S ), N_AE_DDEP, AE_DDEP_MAP )
            IF ( I .GT. 0 ) DDEP_SPCHIT( V ) = .TRUE.
            END DO

         DO S = 1, N_NR_DEPV
            V = V + 1
            I = FINDEX ( NR_DEPV_MAP( S ), N_NR_DDEP, NR_DDEP_MAP )
            IF ( I .GT. 0 ) DDEP_SPCHIT( V ) = .TRUE.
            END DO

         DO S = 1, N_TR_DEPV
            V = V + 1
            I = FINDEX ( TR_DEPV_MAP( S ), N_TR_DDEP, TR_DDEP_MAP )
            IF ( I .GT. 0 ) DDEP_SPCHIT( V ) = .TRUE.
            END DO

!        do s = 1, N_SPC_DEPV
!           write( logdev,* ) '   DDEP_SPCHIT: ', s, ddep_spchit( s )
!           end do

C ACONC ----------------------------------------------------------------

         IF ( .NOT. OPEN3( A_CONC_1, FSREAD3, 'COLLECT' ) ) THEN
            XMSG = 'Error opening A_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( A_CONC_1 ) ) THEN
            XMSG = 'File description error for A_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         TSTEP = TSTEP3D

         ACONC_FG_NCOLS  = NCOLS3D
         ACONC_OFFSET_CR = NCOLS3D * NROWS3D
         ACONC_NLVLS     = NLAYS3D
         ACONC_NVARS     = NVARS3D

         ACONC_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * A_NLYS * N_ASPCS
         ACONC_GSZE = NCOLS3D * NROWS3D * ACONC_NLVLS * ACONC_NVARS

!        write( logdev,* ) ' ACONC_DSZE, ACONC_GSZE: ', ACONC_DSZE, ACONC_GSZE

         ALLOCATE ( ACONC_DATA( ACONC_GSZE ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for ACONC_DATA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( ACONC_SPCHIT( N_ASPCS ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for ACONC_SPCHIT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         ACONC_SPCHIT = .TRUE.  ! ACONC_DATA already subsetted

C WDEP -----------------------------------------------------------------

         IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSREAD3, 'COLLECT' ) ) THEN
            XMSG = 'Error opening CTM_WET_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( CTM_WET_DEP_1 ) ) THEN
            XMSG = 'File description error for CTM_WET_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         TSTEP = TSTEP3D

         WDEP_FG_NCOLS  = NCOLS3D
         WDEP_OFFSET_CR = NCOLS3D * NROWS3D
         WDEP_NLVLS     = NLAYS3D ! = 1
         WDEP_NVARS     = NVARS3D

         WDEP_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * 1 * N_SPC_WDEPD
         WDEP_GSZE = NCOLS3D * NROWS3D * WDEP_NLVLS * WDEP_NVARS

!        write( logdev,* ) ' WDEP_DSZE, WDEP_GSZE:   ', WDEP_DSZE, WDEP_GSZE

         ALLOCATE ( WDEP_DATA( WDEP_GSZE ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for WDEP_DATA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         ALLOCATE ( WDEP_SPCHIT( N_SPC_WDEPD ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Error allocating memory for WDEP_SPCHIT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         WDEP_SPCHIT = .TRUE.  ! WDEP_DATA already subsetted

C CLDD -----------------------------------------------------------------

         IF ( CLD_DIAG ) THEN

            IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSREAD3, 'COLLECT' ) ) THEN
               XMSG = 'Error opening CTM_WET_DEP_2'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            IF ( .NOT. DESC3( CTM_WET_DEP_2 ) ) THEN
               XMSG = 'File description error for CTM_WET_DEP_2'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            TSTEP = TSTEP3D

            CLDD_FG_NCOLS  = NCOLS3D
            CLDD_OFFSET_CR = NCOLS3D * NROWS3D
            CLDD_NLVLS     = NLAYS3D ! = 1
            CLDD_NVARS     = NVARS3D

            CLDD_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * 1
     &                * ( N_SPC_WDEP + 6 )
            CLDD_GSZE = NCOLS3D * NROWS3D * CLDD_NLVLS * CLDD_NVARS

!           write( logdev,* ) ' CLDD_DSZE, CLDD_GSZE:   ', CLDD_DSZE, CLDD_GSZE

            ALLOCATE ( CLDD_DATA( CLDD_GSZE ), STAT=ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Error allocating memory for CLDD_DATA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            ALLOCATE ( CLDD_SPCHIT( N_SPC_WDEP + 6 ), STAT=ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Error allocating memory for CLDD_SPCHIT'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            CLDD_SPCHIT = .TRUE.  ! CLDD_DATA already subsetted

               END IF

C VIS ------------------------------------------------------------------

         IF ( N_AE_SPC .GT. 0 ) THEN
!        IF ( N_AE_SPCx .GT. 0 ) THEN

            IF ( .NOT. OPEN3( CTM_VIS_1, FSREAD3, 'COLLECT' ) ) THEN
               XMSG = 'Error opening CTM_VIS_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            IF ( .NOT. DESC3( CTM_VIS_1 ) ) THEN
               XMSG = 'File description error for CTM_VIS_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            TSTEP = TSTEP3D

            VIS_FG_NCOLS  = NCOLS3D
            VIS_OFFSET_CR = NCOLS3D * NROWS3D
            VIS_NLVLS     = NLAYS3D ! = NLAYS
            VIS_NVARS     = NVARS3D

            VIS_DSZE = ( MY_NCOLS + 1 )*( MY_NROWS + 1 ) * NLAYS * N_AE_VIS_SPC
            VIS_GSZE = NCOLS3D * NROWS3D * VIS_NLVLS * VIS_NVARS

!           write( logdev,* ) ' VIS_DSZE, VIS_GSZE:   ', VIS_DSZE, VIS_GSZE

            ALLOCATE ( VIS_DATA( VIS_GSZE ), STAT=ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Error allocating memory for VIS_DATA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            ALLOCATE ( VIS_SPCHIT( N_AE_VIS_SPC ), STAT=ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Error allocating memory for VIS_SPCHIT'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            VIS_SPCHIT = .TRUE.  ! VIS_DATA already subsetted

            END IF

         END IF   ! FIRSTIME

      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0; count6 = 0
      bnce1 = 0; bnce2 = 0; bnce3 = 0; bnce4 = 0; bnce5 = 0; bnce6 = 0
      CONC_DONE  = .FALSE.; CONC_WRITTEN  = .FALSE.; CONC_NPE  = 0
      DDEP_DONE  = .FALSE.; DDEP_WRITTEN  = .FALSE.; DDEP_NPE  = 0
      ACONC_DONE = .FALSE.; ACONC_WRITTEN = .FALSE.; ACONC_NPE = 0
      WDEP_DONE  = .FALSE.; WDEP_WRITTEN  = .FALSE.; WDEP_NPE  = 0
      CLDD_DONE  = .FALSE.; CLDD_WRITTEN  = .FALSE.; CLDD_NPE  = 0
      VIS_DONE   = .FALSE.; VIS_WRITTEN   = .FALSE.; VIS_NPE   = 0
      ALL_DONE   = .FALSE.
      DO WHILE ( .NOT. ALL_DONE )

         IF ( CONC_NPE .LT. NPROCS-1 ) THEN
            CALL ASSEMBLE ( ISTEP, CONC_TAG, logdev,
     &                      CONC_FG_NCOLS, CONC_OFFSET_CR,
     &                      CGRD_NVARS, CONC_NLVLS, CGRD_DSZE,
     &                      CONC_SPCHIT, CONC_WRITTEN, CONC_NPE, CONC_DATA,
     &                      bnce1 )
            count1 = count1 + 1
            ELSE IF ( .NOT. CONC_DONE ) THEN
            IF ( .NOT. WRITE3( CTM_CONC_1, ALLVAR3, JDATE, JTIME,
     &                         CONC_DATA ) ) THEN
               XMSG = 'Error writing CTM_CONC_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', CTM_CONC_1,
     &            'for date and time', JDATE, JTIME
            CONC_DONE = .TRUE.
!           IF ( ISTEP .EQ. SSTEP_06 .OR. ISTEP .EQ. SSTEP_12 ) THEN
!              CALL S_STEP ( JDATE, JTIME, ISTEP )
!              WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
!    &               'Timestep written to', S_STEP_1,
!    &               'for date and time', JDATE, JTIME
!              END IF
            END IF

         IF ( DDEP_NPE .LT. NPROCS-1 ) THEN
            CALL ASSEMBLE ( ISTEP, DDEP_TAG, logdev,
     &                      DDEP_FG_NCOLS, DDEP_OFFSET_CR,
     &                      N_SPC_DEPV, DDEP_NLVLS, DDEP_DSZE,
     &                      DDEP_SPCHIT, DDEP_WRITTEN, DDEP_NPE, DDEP_DATA,
     &                      bnce2 )
            count2 = count2 + 1
            ELSE IF ( .NOT. DDEP_DONE ) THEN
            IF ( .NOT. WRITE3( CTM_DRY_DEP_1, ALLVAR3, JDATE, JTIME,
     &                         DDEP_DATA ) ) THEN
               XMSG = 'Error writing CTM_DRY_DEP_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', CTM_DRY_DEP_1,
     &            'for date and time', JDATE, JTIME
            DDEP_DONE = .TRUE.
            END IF

         IF ( ACONC_NPE .LT. NPROCS-1 ) THEN
            CALL ASSEMBLE ( ISTEP, ACONC_TAG, logdev,
     &                      ACONC_FG_NCOLS, ACONC_OFFSET_CR,
     &                      N_ASPCS, A_NLYS, ACONC_DSZE,
     &                      ACONC_SPCHIT, ACONC_WRITTEN, ACONC_NPE, ACONC_DATA,
     &                      bnce3 )
            count3 = count3 + 1
            ELSE IF ( .NOT. ACONC_DONE ) THEN
C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000
            MDATE = JDATE
            MTIME = JTIME
            CALL NEXTIME ( MDATE, MTIME, -TSTEP )
            IF ( .NOT. WRITE3( A_CONC_1, ALLVAR3, MDATE, MTIME,
     &                         ACONC_DATA ) ) THEN
               XMSG = 'Error writing A_CONC_1'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               END IF
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', A_CONC_1,
     &            'for date and time', MDATE, MTIME
            ACONC_DONE = .TRUE.
            END IF

         IF ( WDEP_NPE .LT. NPROCS-1 ) THEN
            CALL ASSEMBLE ( ISTEP, WDEP_TAG, logdev,
     &                      WDEP_FG_NCOLS, WDEP_OFFSET_CR,
     &                      N_SPC_WDEPD, WDEP_NLVLS, WDEP_DSZE,
     &                      WDEP_SPCHIT, WDEP_WRITTEN, WDEP_NPE, WDEP_DATA,
     &                      bnce4 )
            count4 = count4 + 1
            ELSE IF ( .NOT. WDEP_DONE ) THEN
            IF ( .NOT. WRITE3( CTM_WET_DEP_1, ALLVAR3, JDATE, JTIME,
     &                         WDEP_DATA ) ) THEN
               XMSG = 'Error writing CTM_WET_DEP_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', CTM_WET_DEP_1,
     &            'for date and time', JDATE, JTIME
            WDEP_DONE = .TRUE.
            END IF

         IF ( CLD_DIAG ) THEN
C Write data to the diagnostic file if requested by the user
            IF ( CLDD_NPE .LT. NPROCS-1 ) THEN
               CALL ASSEMBLE ( ISTEP, CLDD_TAG, logdev,
     &                         CLDD_FG_NCOLS, CLDD_OFFSET_CR,
     &                         N_SPC_CONV, CLDD_NLVLS, CLDD_DSZE,
     &                         CLDD_SPCHIT, CLDD_WRITTEN, CLDD_NPE, CLDD_DATA,
     &                         bnce5 )
               count5 = count5 + 1
               ELSE IF ( .NOT. CLDD_DONE ) THEN
               IF ( .NOT. WRITE3( CTM_WET_DEP_2, ALLVAR3, JDATE, JTIME,
     &                            CLDD_DATA ) ) THEN
                  XMSG = 'Error writing CTM_WET_DEP_2'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &               'Timestep written to', CTM_WET_DEP_2,
     &               'for date and time', JDATE, JTIME
               CLDD_DONE = .TRUE.
               END IF
            ELSE
            CLDD_DONE = .TRUE.
            END IF   ! CLD_DIAG

         IF ( N_AE_SPC .GT. 0 ) THEN
!        IF ( N_AE_SPCx .GT. 0 ) THEN
            IF ( VIS_NPE .LT. NPROCS-1 ) THEN
               CALL ASSEMBLE ( ISTEP, AVIS_TAG, logdev,
     &                         VIS_FG_NCOLS, VIS_OFFSET_CR,
     &                         N_AE_VIS_SPC, VIS_NLVLS, VIS_DSZE,
     &                         VIS_SPCHIT, VIS_WRITTEN, VIS_NPE, VIS_DATA,
     &                         bnce6 )
               count6 = count6 + 1
               ELSE IF ( .NOT. VIS_DONE ) THEN
               IF ( .NOT. WRITE3( CTM_VIS_1, ALLVAR3, JDATE, JTIME,
     &                            VIS_DATA ) ) THEN
                     XMSG = 'Could not write ' // CTM_VIS_1 // ' file'
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &               'Timestep written to', CTM_VIS_1,
     &               'for date and time', JDATE, JTIME
C *** Write data to the aerosol parameters file.
!              IF ( AER_DIAG ) THEN
!                 IF ( .NOT. WRITE3( CTM_DIAM_1, ALLVAR3, JDATE, JTIME,
!    &                               DIAM_SPC ) ) THEN
!                    XMSG = 'Could not write ' // CTM_DIAM_1 // ' file'
!                    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!                 END IF
!                 WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
!    &                  'Timestep written to', CTM_DIAM_1,
!    &                  'for date and time', JDATE, JTIME
!                 END IF
               VIS_DONE = .TRUE.
               END IF
            ELSE
            VIS_DONE = .TRUE.
            END IF   ! N_AE_SPC .GT. 0

         IF ( CONC_DONE  .AND.
     &        DDEP_DONE  .AND.
     &        ACONC_DONE .AND.
     &        WDEP_DONE  .AND.
     &        CLDD_DONE  .AND.
     &        VIS_DONE ) THEN

            write( logdev,* ) ' '
            write( logdev,* ) ' '
            write( logdev,* ) ' write-wait for conc:  ', count1
            write( logdev,* ) ' write-wait for ddep:  ', count2
            write( logdev,* ) ' write-wait for aconc: ', count3
            write( logdev,* ) ' write-wait for wdep:  ', count4
            if ( cld_diag )
     &         write( logdev,* ) ' write-wait for cldd:  ', count5
            if ( n_ae_spc .gt. 0 )
     &         write( logdev,* ) ' write-wait for vis:   ', count6
            write( logdev,* ) ' '
            write( logdev,* ) ' msg-wait for conc:  ', bnce1
            write( logdev,* ) ' msg-wait for ddep:  ', bnce2
            write( logdev,* ) ' msg-wait for aconc: ', bnce3
            write( logdev,* ) ' msg-wait for wdep:  ', bnce4
            if ( cld_diag )
     &         write( logdev,* ) ' msg-wait for cldd:  ', bnce5
            if ( n_ae_spc .gt. 0 )
     &         write( logdev,* ) ' msg-wait for vis:   ', bnce6
            ALL_DONE = .TRUE.

            END IF

         END DO

      IF ( ISTEP .EQ. NSTEPS ) THEN 

         IF ( .NOT. CLOSE3 ( CTM_CONC_1 ) ) THEN
            XMSG = 'Error closing CTM_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. CLOSE3 ( CTM_DRY_DEP_1 ) ) THEN
            XMSG = 'Error closing CTM_DRY_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. CLOSE3 ( A_CONC_1 ) ) THEN
            XMSG = 'Error closing A_CONC_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. CLOSE3 ( CTM_WET_DEP_1 ) ) THEN
            XMSG = 'Error closing CTM_WET_DEP_1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         IF ( CLD_DIAG ) THEN
            IF ( .NOT. CLOSE3 ( CTM_WET_DEP_2 ) ) THEN
               XMSG = 'Error closing CTM_WET_DEP_2'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF

         IF ( N_AE_SPC .GT. 0 ) THEN
!        IF ( N_AE_SPCx .GT. 0 ) THEN

            IF ( .NOT. CLOSE3 ( CTM_VIS_1 ) ) THEN
               XMSG = 'Error closing CTM_VIS_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            IF ( AER_DIAG ) THEN
               IF ( .NOT. CLOSE3 ( CTM_DIAM_1 ) ) THEN
                  XMSG = 'Error closing CTM_DIAM_1'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF

            END IF

         END IF

      RETURN

      END SUBROUTINE COLLECT
