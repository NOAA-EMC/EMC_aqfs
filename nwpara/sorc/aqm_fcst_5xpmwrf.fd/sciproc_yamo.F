
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
C $Header: /project/work/rep/CCTM/src/driver/ctm/sciproc.F,v 1.19 2002/04/05 18:23:09 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SCIPROC ( JDATE, JTIME, TSTEP, ASTEP )

C-----------------------------------------------------------------------
C Function:
C    Controls all of the physical and chemical processes for a grid
C    Operator splitting symmetric around chemistry
 
C Preconditions:
C    Dates and times represented YYYYDDD:HHMMSS.
C    No "skipped" dates and times.  All boundary input variables (layered or
C    non-layered) have the same perimeter structure with a thickness of NTHIK
 
C Subroutines and functions called:
C    All physical and chemical subroutines, 
C    LOAD_RHOJ, ADJADV, DECOUPLE, COUPLE
 
C Revision History:
C   Oct. 24, 1995 by M. Talat Odman and Clint L. Ingram at NCSC: created
C   Jeff
C   13 Dec 97 - Jeff - uncouple diffusion processes
C   27 Jun 98 - Jeff - sync step = chem step
C    7 Jul 01 - Shawn - mv cloud processing before chem
C      Jan 02 - Jeff - dyn alloc; remove PCGRID argument to ping
C   1/03 - JP mods for Yamo mass conservation
C          Moved LOAD_RHOJ to driver
C          Removed X/Y alternation since op-split correction is added to yadv
C   3/03   JY elminate symmetric processing option, ADJADV
C   12/03  JY move vdiff before advection
C   30 May 05 J.Young: mass-conserving advection (yamo)
C   10/05  JY dyn. vert. layers
C-----------------------------------------------------------------------

!     USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS

      IMPLICIT NONE   

C Include files:

!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!     INCLUDE SUBST_GC_SPC      ! gas chemistry species table
!     INCLUDE SUBST_AE_SPC      ! aerosol species table
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IODECL      ! I/O definitions and declarations

C Arguments:

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 2 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
!     INTEGER      ASTEP( NLAYS )  ! layer advection time step
      INTEGER      ASTEP( : )      ! layer advection time step

C Parameters:

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: SETUP_LOGDEV
      LOGICAL, EXTERNAL :: ENVYN

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'SCIPROC_YAMO'

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER, SAVE :: LOGDEV

      INTEGER      SDATE        ! current science process date, coded YYYYDDD
      INTEGER      STIME        ! current science process time, coded HHMMSS
 
      INTEGER STATUS
      CHARACTER( 16 ) :: CTM_CKSUM = 'CTM_CKSUM'     ! env var for cksum on
      LOGICAL, SAVE   :: CKSUM     ! flag for cksum on, default = [F]

      INTERFACE
         SUBROUTINE HADV ( JDATE, JTIME, TSTEP, ASTEP )
            IMPLICIT NONE
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 2 ), ASTEP( : )
         END SUBROUTINE HADV
      END INTERFACE

C-----------------------------------------------------------------------

C If ISPCA .ne. 0, then air is advected and concs. are adjusted

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
!        LOGDEV = INIT3 ()
         LOGDEV = SETUP_LOGDEV ()

         CKSUM = .FALSE.         ! default
         CKSUM = ENVYN( CTM_CKSUM, 'Cksum flag', CKSUM, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            WRITE( LOGDEV, '(5X, A)' ) 'Cksum flag'
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

         END IF       ! if firstime

C Physical Processes

      CALL VDIFF ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'VDIFF', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'VDIF', JDATE, JTIME, TSTEP )

C couple CGRID for advection

      CALL COUPLE ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'COUPLE', JDATE, JTIME )

      CALL HADV ( JDATE, JTIME, TSTEP, ASTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'HADV', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'HADV', JDATE, JTIME, TSTEP )

      CALL ZADV ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'ZADV', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'ZADV', JDATE, JTIME, TSTEP )

      CALL HDIFF ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'HDIFF', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'HDIF', JDATE, JTIME, TSTEP )

C decouple CGRID for cloud and chemistry

      SDATE = JDATE
      STIME = JTIME
      CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

      CALL DECOUPLE ( SDATE, STIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'DECOUPLE', JDATE, JTIME )

!     CALL PING ( PCGRID, JDATE, JTIME, TSTEP )
!     CALL PING ( JDATE, JTIME, TSTEP )
!     IF ( CKSUM ) CALL CKSUMMER ( 'PING', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'PING', JDATE, JTIME, TSTEP )

      CALL CLDPROC ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CLDPROC', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'CLDS', JDATE, JTIME, TSTEP )

      CALL CHEM ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'CHEM', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'CHEM', JDATE, JTIME, TSTEP )

      CALL AERO ( JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'AERO', JDATE, JTIME )
!     IF ( LIPR ) CALL PA_UPDATE ( 'AERO', JDATE, JTIME, TSTEP )

      CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
      WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

      RETURN
      END
