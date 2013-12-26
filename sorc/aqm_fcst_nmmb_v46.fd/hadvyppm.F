
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/hadv/yamo/hadvyppm.F,v 1.1.1.1 2005/09/09 18:56:06 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HADV ( JDATE, JTIME, TSTEP, ASTEP )

C-----------------------------------------------------------------------
C Function:
C   Advection in the horizontal plane
C   The process time step is set equal to TSTEP(2). Boundary concentrations
C   are coupled in RDBCON with SqRDMT = Sq. Root [det ( metric tensor )]
C   = Jacobian / (map scale factor)**2
C   where Air Density X SqRDMT is loaded into last BCON slot for advection.
      
C Preconditions:
C   Dates and times represented YYYYDDD:HHMMSS.
C   No "skipped" dates and times.  All boundary input variables have the
C   same boundary perimeter structure with a thickness of 1
C   CGRID in transport units: SQRT{DET[metric tensor]}*concentration (Mass/Vol)
      
C Subroutines and functions called:
 
C Revision history:
C  19 Jan 2004: Jeff Young
C  28 Oct 2005: Jeff Young - layer dependent advection, dyn. vert. layers
      
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE SUBST_MODULES         ! stenex
!     USE SUBST_COMM_MODULE     ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE
      
C Includes:

!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_GC_SPC      ! gas chemistry species table
      INCLUDE SUBST_GC_ADV      ! gas chem advection species and map table
      INCLUDE SUBST_AE_ADV      ! aerosol advection species and map table
      INCLUDE SUBST_NR_ADV      ! non-react advection species and map table
      INCLUDE SUBST_TR_ADV      ! tracer advection species and map table
!     INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IODECL      ! I/O definitions and declarations
      INCLUDE SUBST_FILES_ID    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coordinate & domain definitions (req IOPARMS)
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      INTEGER     JDATE         ! current model date, coded YYYYDDD
      INTEGER     JTIME         ! current model time, coded HHMMSS
      INTEGER     TSTEP( 2 )    ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
      INTEGER     ASTEP( : )    ! layer advection time step

C External Functions not declared in IODECL3.EXT:
      
      INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC, setup_logdev
      
C Parameters:

C Advected species dimension

      INTEGER, PARAMETER :: N_SPC_ADV = N_GC_ADV
     &                                + N_AE_ADV
     &                                + N_NR_ADV
     &                                + N_TR_ADV
     &                                + 1       ! for advecting RHO*SqRDMT

C File Variables:

      REAL         BCON( NBNDY,N_SPC_ADV )      ! boundary concentrations
      REAL         RHOJ( NCOLS,NROWS )          ! RhoJ

C Local Variables:

!     REAL         XTRHOJ( 0:NCOLS,NROWS )
!     REAL         YTRHOJ( 0:NROWS,NCOLS ) <- violates stenex setup
      REAL, ALLOCATABLE, SAVE :: XTRHOJ( :,: )
      REAL, ALLOCATABLE, SAVE :: YTRHOJ( :,: )
      INTEGER       ALLOCSTAT

      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID

      CHARACTER( 16 ) :: PNAME = 'HADVYPPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL XYFIRST

      integer, save :: logdev

      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      STEP                         ! FIX dt
      INTEGER      DSTEP                        ! dt accumulator
      INTEGER      FDATE                        ! interpolation date
      INTEGER      FTIME                        ! interpolation time
      INTEGER      SYNCSTEP

      INTEGER      COL, ROW, LVL                ! loop counters

C for INTERPX
      INTEGER      GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE RDBCON ( FDATE, FTIME, TSTEP, LVL, BCON )
            IMPLICIT NONE
            INTEGER, INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( OUT )    :: BCON( :,: )
         END SUBROUTINE RDBCON
!        SUBROUTINE X_PPM ( FDATE, FTIME, TSTEP, LVL, BCON )
!           USE GRID_CONF
!           IMPLICIT NONE
!           INTEGER, INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
!           REAL,    INTENT( IN )     :: BCON( NBNDY,* )
!        END SUBROUTINE X_PPM
         SUBROUTINE X_YAMO ( FDATE, FTIME, TSTEP, LVL, BCON, TRRHOJ )
            USE GRID_CONF
            IMPLICIT NONE
            INTEGER, INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )     :: BCON( NBNDY,* )
            REAL,    INTENT( IN OUT ) :: TRRHOJ( :,: )
         END SUBROUTINE X_YAMO
!        SUBROUTINE Y_PPM ( FDATE, FTIME, TSTEP, LVL, BCON )
!           USE GRID_CONF
!           IMPLICIT NONE
!           INTEGER, INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
!           REAL,    INTENT( IN )     :: BCON( NBNDY,* )
!        END SUBROUTINE Y_PPM
         SUBROUTINE Y_YAMO ( FDATE, FTIME, TSTEP, LVL, BCON, TRRHOJ )
            USE GRID_CONF
            IMPLICIT NONE
            INTEGER, INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            REAL,    INTENT( IN )     :: BCON( NBNDY,* )
            REAL,    INTENT( IN OUT ) :: TRRHOJ( :,: )
         END SUBROUTINE Y_YAMO
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        logdev = init3()
         logdev = setup_logdev()

C Get CGRID offsets

         CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

         ASPC = GC_STRT - 1 + N_GC_SPCD

         ALLOCATE ( XTRHOJ( 0:NCOLS,  NROWS ), STAT = ALLOCSTAT )
         ALLOCATE ( YTRHOJ(   NCOLS,0:NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XTRHOJ or YTRHOJ'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C get file local domain offsets

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         END IF                    ! if firstime

      SYNCSTEP = TIME2SEC( TSTEP( 2 ) )

      DO 301 LVL = 1, NLAYS

         STEP = TIME2SEC ( ASTEP( LVL ) )
         DSTEP = STEP
         FDATE = JDATE
         FTIME = JTIME
         XYFIRST = .TRUE.

101      CONTINUE

!        write( logdev,* ) ' lvl, fdate, ftime: ', lvl, fdate, ftime

         IF ( XYFIRST ) THEN

            XYFIRST = .FALSE.

            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  YTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
                  END DO
               END DO
            CALL SUBST_COMM ( YTRHOJ, DSPL_N0_E0_S1_W0, DRCN_S, '2 0' )

            CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
            CALL X_PPM ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'X_PPM', CGRID, FDATE, FTIME, LVL )

            CALL Y_YAMO ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON, YTRHOJ )
!           CALL LCKSUMMER ( 'Y_YAMO', CGRID, FDATE, FTIME, LVL )

            ELSE

            XYFIRST = .TRUE.

            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  XTRHOJ( COL,ROW ) = CGRID( COL,ROW,LVL,ASPC )
                  END DO
               END DO
            CALL SUBST_COMM ( XTRHOJ, DSPL_N0_E0_S0_W1, DRCN_W, '1 0' )

            CALL RDBCON ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
            CALL Y_PPM ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON )
!           CALL LCKSUMMER ( 'Y_PPM', CGRID, FDATE, FTIME, LVL )

            CALL X_YAMO ( FDATE, FTIME, ASTEP( LVL ), LVL, BCON, XTRHOJ )
!           CALL LCKSUMMER ( 'X_YAMO', CGRID, FDATE, FTIME, LVL )

            END IF

         DSTEP = DSTEP + STEP
         IF ( DSTEP .LE. SYNCSTEP ) THEN
            CALL NEXTIME( FDATE, FTIME, SEC2TIME( STEP ) )
            GO TO 101
            END IF

301      CONTINUE

      RETURN
      END
