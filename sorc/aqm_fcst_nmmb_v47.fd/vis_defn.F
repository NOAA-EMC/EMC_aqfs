
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
      MODULE VIS_DEFN

C-----------------------------------------------------------------------
C Function:

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)

C Subroutines and functions called:

C Depends on HGRD_DEFN and VGRD_DEFN being inititalized

C Revision history:
C   Mar 03 - Jeff
C   Oct 05 - Jeff: dynamic layers

C-----------------------------------------------------------------------

      USE GRID_CONF    ! horizontal & vertical domain specifications

      IMPLICIT NONE

!     INTEGER, PARAMETER :: N_AE_VIS_SPC = 4
      INTEGER, PARAMETER :: N_AE_VIS_SPC = 2
      INTEGER, PARAMETER :: N_AE_DIAM_SPC = 18

C Visual range information
      REAL, ALLOCATABLE, SAVE :: VIS_SPC( :,:,:,: )

C aerosol size distribution variables
      REAL, ALLOCATABLE, SAVE :: DIAM_SPC( :,:,:,: )

C flag for AER_DIAG file [F], default
      LOGICAL, SAVE :: AER_DIAG

      CONTAINS
         FUNCTION VIS_INIT () RESULT ( SUCCESS )

!        INCLUDE SUBST_VGRD_ID   ! vertical dimensioning parameters
         INCLUDE SUBST_IODECL    ! I/O definitions and declarations

         LOGICAL SUCCESS
         LOGICAL, EXTERNAL :: ENVYN
         INTEGER STATUS, ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 16 ) :: PNAME = 'Vis_Init'
         CHARACTER( 16 ) :: CTM_AERDIAG = 'CTM_AERDIAG'
         CHARACTER( 44 ) :: VARDESC
         CHARACTER( 120 ) :: XMSG = ' '

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           LOGDEV = INIT3()
            LOGDEV = SETUP_LOGDEV ()

            SUCCESS = .TRUE.

            ALLOCATE ( VIS_SPC( MY_NCOLS,MY_NROWS,NLAYS,N_AE_VIS_SPC ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating VIS_SPC'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

           VIS_SPC = 0.0

C *** Get aerosol diagnostic file flag.

            AER_DIAG = .FALSE.         ! default
            VARDESC = 'Flag for writing the aerosol diagnostic file'
            AER_DIAG = ENVYN( CTM_AERDIAG, VARDESC, AER_DIAG, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '( 5X, A )' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '( 5X, A )' ) XMSG
               ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '( 5X, A )' ) XMSG
               END IF

            IF ( AER_DIAG ) THEN

               ALLOCATE ( DIAM_SPC( MY_NCOLS,MY_NROWS,NLAYS,N_AE_DIAM_SPC ),
     &                    STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DIAM_SPC'
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
                  END IF

               DIAM_SPC = 0.0

               END IF

            ELSE

            XMSG = '*** VIS_SPC already initialized'
            CALL M3WARN ( 'VIS_INIT', 0, 0, XMSG )
            IF ( AER_DIAG ) THEN
               XMSG = '*** DIAM_SPC already initialized'
               CALL M3WARN ( 'VIS_INIT', 0, 0, XMSG )
               END IF
            SUCCESS = .FALSE.; RETURN

            END IF

         RETURN
         END FUNCTION VIS_INIT

      END MODULE VIS_DEFN
