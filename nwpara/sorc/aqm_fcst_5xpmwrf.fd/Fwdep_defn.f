
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
C $Header: /project/cmaq/rel/models/CCTM/src/driver/ctm/PCGRID_DEFN.F,v 1.1.1.1 2002/06/27 11:25:59 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE WDEP_DEFN

C-----------------------------------------------------------------------
C Function:

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)

C Subroutines and functions called:

C Revision history:
C   Jan 03 - Jeff

C-----------------------------------------------------------------------

      USE HGRD_DEFN    ! horizontal domain specifications
      USE CGRID_SPCS

      IMPLICIT NONE

      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_WDEP.EXT"   ! wet deposition table for gases
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_WDEP.EXT"   ! wet deposition table for aerosols
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_WDEP.EXT"   ! wet deposition table for non-reactives
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_WDEP.EXT"   ! wet deposition table for tracers

C all wet deposition species
      INTEGER, PARAMETER :: N_SPC_WDEP = N_GC_WDEP
     &                                 + N_AE_WDEP
     &                                 + N_NR_WDEP
     &                                 + N_TR_WDEP
      INTEGER, PARAMETER :: N_SPC_WDEPD = N_SPC_WDEP + 1
      INTEGER, PARAMETER :: N_SPC_CONV = N_SPC_WDEP + 8

      INTEGER, SAVE :: WDEP_MAP( N_SPC_WDEP ) ! wet deposition map to CGRID

      REAL, ALLOCATABLE, SAVE :: CONV_DEP( :,:,: ) ! convective wdep only
      REAL, ALLOCATABLE, SAVE :: TOT_DEP ( :,:,: ) ! total wdep

C flag for cloud diagnostics file [F]
      LOGICAL, SAVE :: CLD_DIAG

      CONTAINS
         FUNCTION WDEP_INIT () RESULT ( SUCCESS )

         INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"    ! I/O definitions and declarations

         LOGICAL SUCCESS
         LOGICAL, EXTERNAL :: ENVYN
         INTEGER STATUS, ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 16 ) :: PNAME = 'Wdep_Init'
         CHARACTER( 16 ) :: CTM_CLD_DIAG = 'CTM_CLD_DIAG'
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER S, V

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           LOGDEV = INIT3()
            LOGDEV = SETUP_LOGDEV ()

!           ALLOCATE ( TOT_DEP( N_SPC_WDEPD,MY_NCOLS,MY_NROWS ),
            ALLOCATE ( TOT_DEP( MY_NCOLS,MY_NROWS,N_SPC_WDEPD ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TOT_DEP'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            TOT_DEP = 0.0

!           ALLOCATE ( CONV_DEP( N_SPC_CONV,MY_NCOLS,MY_NROWS ),
            ALLOCATE ( CONV_DEP( MY_NCOLS,MY_NROWS,N_SPC_CONV ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CONV_DEP'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            CONV_DEP = 0.0

C create wet dep species map to CGRID

            CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

            WDEP_MAP = 0

            S = 0
            DO V = 1, N_GC_WDEP
               S = S + 1
               WDEP_MAP( S ) = GC_STRT - 1 + GC_WDEP_MAP( V )
               END DO
            DO V = 1, N_AE_WDEP
               S = S + 1
               WDEP_MAP( S ) = AE_STRT - 1 + AE_WDEP_MAP( V )
               END DO
            DO V = 1, N_NR_WDEP
               S = S + 1
               WDEP_MAP( S ) = NR_STRT - 1 + NR_WDEP_MAP( V )
               END DO
            DO V = 1, N_TR_WDEP
               S = S + 1
               WDEP_MAP( S ) = TR_STRT - 1 + TR_WDEP_MAP( V )
               END DO

C Get cloud diagnostics file flag

            CLD_DIAG = .FALSE.
            XMSG = 'Flag for writing cloud diagnostics file'
            CLD_DIAG = ENVYN( CTM_CLD_DIAG, XMSG, CLD_DIAG, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) XMSG
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, L9)' ) XMSG, CLD_DIAG
               ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, L9)' ) XMSG, CLD_DIAG
               END IF

            ELSE
            XMSG = '*** WDEP already initialized'
            CALL M3WARN ( 'WDEP_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
            END IF

         RETURN
         END FUNCTION WDEP_INIT

      END MODULE WDEP_DEFN
