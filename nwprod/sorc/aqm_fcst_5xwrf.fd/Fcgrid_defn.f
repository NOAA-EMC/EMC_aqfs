
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
      MODULE CGRID_DEFN

C-----------------------------------------------------------------------
C Function:

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)

C Subroutines and functions called:

C Depends on HGRD_DEFN and VGRD_DEFN being inititalized

C Revision history:
C   Nov 02 - Jeff
C   Oct 05 - Jeff: dynamic layers

C-----------------------------------------------------------------------

      USE GRID_CONF    ! horizontal & vertical domain specifications
      USE CGRID_SPCS   ! CGRID species number and offsets

      IMPLICIT NONE

      REAL, PARAMETER :: CMIN = 1.0E-30
!     REAL, ALLOCATABLE, SAVE :: CGRID( :,:,:,: )
      REAL, POINTER :: CGRID( :,:,:,: )

C for appending WVEL to CGRID for writes
      REAL, ALLOCATABLE, SAVE, TARGET :: CGRID_X( :,:,:,: )

      CONTAINS
         FUNCTION CGRID_INIT () RESULT ( SUCCESS )

!        INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
         INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"      ! I/O definitions and declarations

         LOGICAL SUCCESS
         INTEGER ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER LCOL, HCOL, LROW, HROW

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           LOGDEV = INIT3()
            LOGDEV = SETUP_LOGDEV ()

            CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

            LCOL = 1; HCOL = MY_NCOLS
            LROW = 1; HROW = MY_NROWS

!           ALLOCATE ( CGRID( NSPCSD,NLAYS,LCOL:HCOL,LROW:HROW ),
!           ALLOCATE ( CGRID( LCOL:HCOL,LROW:HROW,NLAYS,NSPCSD ),
            ALLOCATE ( CGRID_X( LCOL:HCOL,LROW:HROW,NLAYS,NSPCSD+1 ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CGRID_X'
               CALL M3WARN ( 'CGRID_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            CGRID_X = CMIN
            CGRID => CGRID_X( :,:,:,1:NSPCSD )

            ELSE
            XMSG = 'CGRID already ALLOCATED'
            CALL M3WARN ( 'CGRID_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

          RETURN
          END FUNCTION CGRID_INIT

      END MODULE CGRID_DEFN
