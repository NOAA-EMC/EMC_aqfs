
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
C $Header: /project/work/rep/CCTM/src/vdiff/acm2_inline/EMIS_DEFN.F,v 1.8 2008/12/11 15:49:57 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_DEFN

C-----------------------------------------------------------------------
C Function: emissions interface to the chemistry-transport model

C Revision History:
C     28 Jul 2006 J.Young: initial implementation
C     18 Aug 2007 J.Young: move beis part to separate module; add plume rise

C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array

      INTEGER, SAVE :: N_SPC_EMIS ! global emissions species
      INTEGER, SAVE :: EMLAYS     ! no. of emis integration layers
                                  ! EMLAYS must be .LT. NLAYS
      INTEGER, PRIVATE, SAVE :: EMLYRS     ! no. of layers on base emis input file -
                                           ! a 2d file if inline, otherwise a 3d file
      INTEGER, PRIVATE, SAVE :: NEMIS_AE   ! no. of aero emis species

      LOGICAL, PRIVATE, SAVE :: EM_TRAC = .FALSE. ! do tracer emissions?
      REAL,    PRIVATE, SAVE :: CNVTE      ! combined conversion factor
      REAL,    PRIVATE, SAVE :: DT         ! TSTEP (output) in sec

      REAL,    PRIVATE, ALLOCATABLE, SAVE :: VDEMIS_AE( :,:,:,: ) ! aerosol emis
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: VDEMIS_NR( :,:,:,: ) ! nonreactives
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: VDEMIS_TR( :,:,:,: ) ! tracer emis

      INTEGER, PRIVATE, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                          STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, PRIVATE, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER, PRIVATE       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, PRIVATE, SAVE :: NDATE, NTIME  ! step next date/time (beis)

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS ) 

         USE BEIS_DEFN           ! biogenic emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE PMEM_DEFN           ! defines emissions file(s) PM mapping

         IMPLICIT NONE
      
         INCLUDE SUBST_RXCMMN    ! for chem mechanism name
         INCLUDE SUBST_AE_SPC    ! aerosol species table
         INCLUDE SUBST_GC_EMIS   ! gas chem emis surrogate names & map table
         INCLUDE SUBST_AE_EMIS   ! aerosol emission surrogate names & map table
         INCLUDE SUBST_NR_EMIS   ! non-react emis surrogate names & map table
         INCLUDE SUBST_TR_EMIS   ! tracer emis surrogate names & map table
         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
#include         SUBST_IODECL    # I/O definitions and declarations

         INTEGER JDATE, JTIME, TSTEP
         LOGICAL :: SUCCESS

         INTEGER, EXTERNAL :: TIME2SEC, ENVINT, INDEX1
         LOGICAL, EXTERNAL :: ENVYN

         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio

         REAL    CONVEM          ! conversion for emissions rates to Kg/s
         REAL    DX1, DX2        ! CX x1- and x2-cell widths
         INTEGER EMISLYRS        ! no. of file emissions layers

         CHARACTER( 16 ), SAVE :: CTM_EMLAYS = 'CTM_EMLAYS'    ! env var for no. of
                                                               ! emiss. layers
         CHARACTER( 16 ) :: PNAME = 'EMIS_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, K, LOGDEV, STATUS

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF

C In-line biogenic emissions?

         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open Emissions files

         CALL OPEMIS ( JDATE, JTIME, N_GC_EMIS, EM_TRAC, CONVEM, EMLAYS )

C Get number of emissions layers

         IF ( EMLAYS .LE. 1 ) THEN   ! better be inline plume rise run
            EMLAYS = NLAYS   ! env var default
            VARDESC = 'Number of emission layers'
            EMLAYS = ENVINT( CTM_EMLAYS, VARDESC, EMLAYS, STATUS )
            IF ( STATUS .NE. 0 ) THEN
               WRITE( LOGDEV, '(5X, A)' ) VARDESC
               IF ( STATUS .EQ. 1 ) THEN
                  XMSG = 'Environment variable improperly formatted'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE
                  XMSG = 'Environment variable not set or empty ... Using default:'
                  WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
               END IF
            END IF
         END IF

         EMLAYS = MIN ( EMLAYS, NLAYS )

         WRITE( LOGDEV,1009 ) EMLAYS, NLAYS
1009     FORMAT( / 10X, 'Number of Emissions Layers:         ', I3
     &           / 10X, 'out of total Number of Model Layers:', I3 )

         IF ( N_AE_SPC .GT. 0 ) THEN
            WRITE( LOGDEV,'( /5X, A )' ) 'Aerosol Emissions Processing in '
     &                                // 'Vertical diffusion ...'
            NEMIS_AE = N_AE_EMIS  ! from AE_EMIS.EXT
C set up PM species mapping from emissions file(s) for use in GET_AERO_EMIS
            CALL GET_PMEM_MAP ( JDATE, JTIME )   ! set in PMEM_DEFN
         ELSE
            NEMIS_AE = 0
         END IF

         N_SPC_EMIS = N_GC_EMIS
     &              + NEMIS_AE
     &              + N_NR_EMIS
     &              + N_TR_EMIS

C 3D point source emissions (N_SPC_EMIS+1 serves as the null map for PTEM_MAP)
         IF ( .NOT. PT3D_INIT( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( VDEMIS ( N_SPC_EMIS+1,EMLAYS,MY_NCOLS,MY_NROWS ),
     &              STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( N_SPC_EMIS .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         IF ( N_AE_SPC .GT. 0 ) THEN
            ALLOCATE ( VDEMIS_AE( NEMIS_AE,EMLAYS,MY_NCOLS,MY_NROWS ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'VDEMIS_AE memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END IF

         IF ( N_NR_EMIS .GT. 0 ) THEN
            ALLOCATE ( VDEMIS_NR( N_NR_EMIS,EMLAYS,MY_NCOLS,MY_NROWS ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'VDEMIS_NR memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END IF

         IF ( EM_TRAC .AND. N_TR_EMIS .GT. 0 ) THEN
            ALLOCATE ( VDEMIS_TR( N_TR_EMIS,EMLAYS,MY_NCOLS,MY_NROWS ),
     &                 STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'VDEMIS_TR memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
         END IF

C combined gas emssions conversion factor

         CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 )

         STRT_GC = 1
         FINI_GC = N_GC_EMIS
         STRT_AE = N_GC_EMIS + 1
         FINI_AE = N_GC_EMIS + NEMIS_AE
         STRT_NR = N_GC_EMIS + NEMIS_AE + 1
         FINI_NR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS
         STRT_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + 1
         FINI_TR = N_GC_EMIS + NEMIS_AE + N_NR_EMIS + N_TR_EMIS

         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA,
     &                         RDX3F, RRHOJ, RJACM )

         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE PT3D_DEFN           ! plume rise emissions
         USE PMEM_DEFN           ! defines emissions file(s) PM mapping
         USE PTMAP               ! defines emissions file(s) PM mapping

         INCLUDE SUBST_AE_SPC    ! aerosol species table
         INCLUDE SUBST_GC_EMIS   ! gas chem emis surrogate names & map table
         INCLUDE SUBST_NR_EMIS   ! non-react emis surrogate names & map table
         INCLUDE SUBST_TR_EMIS   ! tracer emis surrogate names & map table
#include         SUBST_IODECL    # I/O definitions and declarations

         INTEGER JDATE, JTIME
         INTEGER TSTEP( 2 )                 ! time step vector (HHMMSS)
         REAL    CONVPA             ! conversion factor to pressure in Pascals
         REAL    RDX3F( NLAYS )             ! reciprocal layer thickness
         REAL    RRHOJ( NCOLS,NROWS,NLAYS ) ! reciprocal density X Jacobian
         REAL    RJACM( NCOLS,NROWS,NLAYS ) ! reciprocal mid-layer Jacobian

         INTEGER, EXTERNAL :: TIME2SEC, SECSDIFF

         REAL             CNVTC         ! combined beis conversion factor
         REAL             CNVTI         ! intermediate combined beis conv. factor
         REAL             CNVTP         ! intermediate combined beis conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V ! loop induction variables

D        integer, save :: logdev
D        logical, save :: firstime = .true.

         INTERFACE
            SUBROUTINE RDEMIS_GC ( MDATE, MTIME, EMISLYRS, NSPC_EMIS, VDEMIS )
               IMPLICIT NONE
               INTEGER, INTENT( IN )  :: MDATE, MTIME, EMISLYRS, NSPC_EMIS
               REAL, INTENT( OUT )    :: VDEMIS( :,:,:,: )
            END SUBROUTINE RDEMIS_GC
!           SUBROUTINE RDEMIS_AE ( MDATE, MTIME, TSTEP, EMISLYRS, RJACM,
!    &                             VDEMIS, VDEMIS_AE )
!              IMPLICIT NONE
!              INTEGER, INTENT( IN )  :: MDATE, MTIME, TSTEP( 2 ), EMISLYRS
!              REAL, INTENT( IN )     :: RJACM( :,:,: )
!              REAL, INTENT( OUT )    :: VDEMIS( :,:,:,: )
!              REAL, INTENT( OUT )    :: VDEMIS_AE( :,:,:,: )
!           END SUBROUTINE RDEMIS_AE
            SUBROUTINE RDEMIS_NR ( MDATE, MTIME, EMISLYRS, NSPC_EMIS, VDEMIS )
               IMPLICIT NONE
               INTEGER, INTENT( IN )  :: MDATE, MTIME, EMISLYRS, NSPC_EMIS
               REAL, INTENT( OUT )    :: VDEMIS( :,:,:,: )
            END SUBROUTINE RDEMIS_NR
            SUBROUTINE RDEMIS_TR ( MDATE, MTIME, EMISLYRS, NSPC_EMIS, VDEMIS )
               IMPLICIT NONE
               INTEGER, INTENT( IN )  :: MDATE, MTIME, EMISLYRS, NSPC_EMIS
               REAL, INTENT( OUT )    :: VDEMIS( :,:,:,: )
            END SUBROUTINE RDEMIS_TR
         END INTERFACE

C-----------------------------------------------------------------------

D        if ( firstime ) then
D           firstime = .false.
D           logdev = init3()
D        end if

C read & interpolate emissions (create VDEMIS in the species class order)

         CNVTP = CONVPA * CNVTE
         VDEMIS = 0.0   ! array assignment

         IF ( PT3DEMIS ) THEN
            EMLYRS = 1
         ELSE
            EMLYRS = EMLAYS
         END IF
         CALL RDEMIS_GC ( JDATE, JTIME, EMLYRS, N_GC_EMIS, VDEMIS )

C reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS

         IF ( N_GC_EMIS .GT. 0 ) THEN

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO V = STRT_GC, FINI_GC
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R ) * CNVTC
                     END DO
                  END DO
               END DO
            END DO

         END IF

C beis

         IF ( BIOGEMIS ) THEN

            CALL GET_BEIS ( JDATE, JTIME, TSTEP )

C tmpbeis312 is based on hourly input - need to interpolate return value
C merge: VDEMIS_BI in mol/hr. Convert to ppm/sec

            L = 1
            CNVTI = CONVPA * CNVTE * RDX3F( L )  ! per sec
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  CNVTC = CNVTI * RRHOJ( C,R,L )
                  N = 0
                  DO S = 1, MSPCS
                     V = BEIS_MAP( S )
                     IF ( V .GT. 0 ) THEN
                        N = N + 1
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + VDEMIS_BI( N,C,R ) * CNVTC
                      END IF
                  END DO
               END DO
            END DO

         END IF

C plume rise

         IF ( PT3DEMIS ) THEN

            CALL GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

C merge: VDEMIS_PT in mol/sec. Convert to ppm/sec
C If aerosols, then the 6 PM emissions obtained from GET_PT3D_EMIS need to be
C accessed by GET_AERO_EMIS to be "speciated" and merged with the 27 AE emissions.

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO S = 1, N_SPC_PTEM
                        V = PTEM_MAP( S )  ! <- pointer to non-PM (non-aerosol) species
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + VDEMIS_PT( C,R,L,S ) * CNVTC
                     END DO
                  END DO
               END DO
            END DO

         END IF

C aerosol emissions - all units conversions done in GET_AERO_EMIS for aerosols
C GET_AERO_EMIS in f90 module AERO_EMIS

         IF ( N_AE_SPC .GT. 0 ) THEN
            CALL GET_AERO_EMIS ( JDATE, JTIME, TSTEP, EMLAYS, RJACM,
     &                           VDEMIS, VDEMIS_AE )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     DO V = STRT_AE, FINI_AE
                        S = V + 1 - STRT_AE
                        VDEMIS( V,L,C,R ) = VDEMIS_AE( S,L,C,R )
                     END DO
                  END DO
               END DO
            END DO

         END IF

C non-reactive gases (conversion to ppmv/s) VDEMIS in this order from RDEMIS

         IF ( N_NR_EMIS .GT. 0 ) THEN
            CALL RDEMIS_NR ( JDATE, JTIME, EMLYRS, N_NR_EMIS, VDEMIS_NR )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO V = STRT_NR, FINI_NR
                        S = V + 1 - STRT_NR
                        VDEMIS( V,L,C,R ) = VDEMIS_NR( S,L,C,R ) * CNVTC
                     END DO
                  END DO
               END DO
            END DO

         END IF

C tracer gases (conversion to ppmv/s)

         IF ( EM_TRAC .AND. N_TR_EMIS .GT. 0 ) THEN
            CALL RDEMIS_TR ( JDATE, JTIME, EMLYRS, N_TR_EMIS, VDEMIS_TR )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLYRS
                     CNVTC = CNVTP * RDX3F( L ) * RRHOJ( C,R,L )
                     DO V = STRT_TR, FINI_TR
                        S = V + 1 - STRT_TR
                        VDEMIS( V,L,C,R ) = VDEMIS_TR( S,L,C,R ) * CNVTC
                     END DO
                  END DO
               END DO
            END DO

         END IF

C zero out emissions values for species not included in diffused list ...
C accounts for emissions species names as a subset of the vert. diffused species list

        VDEMIS( N_SPC_EMIS+1,:,:,: ) = 0.0

        END SUBROUTINE GET_EMIS

      END MODULE EMIS_DEFN
