
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/driver/yamo/advstep.F,v 1.1.1.1 2005/09/09 18:56:06 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )
      
C-----------------------------------------------------------------------
C Function:
C    Use Courant-number conditions to determine the (time-splitting) model
C    base synchronization time step TSTEP(2) and number NREPS of sync time
C    steps per output time step TSTEP(1). The base synchronization time step
C    is considered to be the Courant condition safe advection time step.
C    Minimum returned sync time step = 900 sec
C    Determines a Courant condition safe advection step in TSTEP(3) with
C    respect to a user-defined minimum sync time step [TSTEP(2)].

C    June 2003 J.Young
C    Restructure to produce layer dependent advection step
C    change arguments:
C       TSTEP( 1 ) = output timestep (same) - input
C       TSTEP( 2 ) = synchronization timestep (same) - output
C       eliminate TSTEP( 3 ). Add new argument ASTEP( NLAYS ) - output
      
C Preconditions:
      
C Subroutines and functions called:
C    SEC2TIME, TIME2SEC, DESC3, M3EXIT, HCONTVEL
 
C Revision history:
C    prototype 2/93 by CJC
C    configuration management prototype 6/94 by Dongming Hwang
 
C    Apr 15, 1996 by M. Talat Odman at NCSC:  the number of model time
C    steps is made even to assure symmetry of transport operators within
C    each output time step.
 
C    Jan 20, 1996 by M. Talat Odman at NCSC:  now applies the CFL condition
C    independently in all three contravariant directions and 
C    finds minimum time step.  
      
C    22 Apr 97 Jeff: only for horizontal advection
C                    keep Talat's even step mod for symmetric sciproc
C     2 Sep 97 Jeff: calc. separate sync and adv time steps, TSTEP(3)
C    12 Feb 98 Jeff: make sync step = chem step
C    27 Jun 98 Jeff: allow for constant vel field (time-independent file)

C    2 October, 1998 by Al Bourgeois at LM: 1 implementation
C
C    8 Jan 99 by David Wong at LM: GLOBAL_MAX function call is added
C
C    1/19/99 David Wong
C      -- add a loop_index call
C      -- change loop index ending point to avoid accessing invalid region.
C         (reason to do this is to prevent using boundary data from PINTERP,
C          which sets pseudo-boundary data to 0)
C
C    9 Jun 99 by David Wong at LM: change GLOBAL_MAX to GLOBAL_RMAX
C    21 Nov 00 J.Young: GLOBAL_RMAX -> Dave Wong's f90 stenex GLOBAL_MAX
C    Jeff - Feb 01 - module LOCALFILE to set CCTM IOAPI file defn's
C                  - assumed shape arrays
C   30 Mar 01 J.Young: Use HGRD_DEFN; replace INTERP3 with INTERPX
C    9 Sep 03 J.Young: return layer dependent advection tstep, ASTEP
C   27 Aug 03 J.Young: symmetric processing is now deprecated
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C-----------------------------------------------------------------------
      
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE SE_MODULES              ! stenex
!     USE SUBST_GLOBAL_MAX_MODULE    ! stenex
!     USE SUBST_UTIL_MODULE          ! stenex

      IMPLICIT NONE
      
C Includes:
      
!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/CONST.EXT"       ! constants
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/FDESC3.EXT"     ! file header data structure
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"      ! I/O definitions and declarations
!     INCLUDE SUBST_COORD_ID    ! domain coordinate definitions
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/FILES_CTM.EXT"    ! file name parameters

C Arguments:

      INTEGER    JDATE          ! current model simulation date (YYYYDDD)
      INTEGER    JTIME          ! current model simulation time (HHMMSS)
      INTEGER    TSTEP( 2 )     ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
!     INTEGER    ASTEP( NLAYS ) ! layer advection step
      INTEGER    ASTEP( : )     ! layer advection step
      INTEGER    NREPS          ! sync time steps per output time step

C Parameter:  maximum Courant number allowed

      REAL, PARAMETER :: CC = 0.75

      INTEGER, SAVE :: MAXSYNC         ! force max TSTEP(2) (sec)
      INTEGER, SAVE :: MINSYNC         ! force min TSTEP(2) (sec)
      REAL, SAVE    :: SIGST           ! sigma_sync_top value
      INTEGER, SAVE :: ADVLAYR         ! adv=sync at least up to this level

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC, SECSDIFF, ENVINT, SETUP_LOGDEV
      LOGICAL, EXTERNAL :: ENVYN, CURRSTEP
      REAL, EXTERNAL    :: ENVREAL

C Local variables:

      INTEGER, SAVE :: LOGDEV         ! unit number for log device

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER, SAVE :: WSTEP = 0      ! wind file interpolation time step
      INTEGER, SAVE :: SDATE, STIME   ! wind file start date and time
      INTEGER, SAVE :: FDATE, FTIME   ! wind file current date and time
      INTEGER, SAVE :: FSTEP          ! wind file time step

      REAL, SAVE :: IDX1              ! 1/dx1
      REAL, SAVE :: IDX2              ! 1/dx2

!     REAL, SAVE :: UWIND ( NCOLS+1,NROWS+1,NLAYS ) ! Contravariant x1-velocity
!     REAL, SAVE :: VWIND ( NCOLS+1,NROWS+1,NLAYS ) ! Contravariant x2-velocity
!     REAL, ALLOCATABLE :: WIND( :,:,: )
!     REAL WIND ( NCOLS+1,NROWS+1,NLAYS ) ! Contravariant generic-velocity
      REAL WIND ( NCOLS+1,NROWS+1 )       ! Contravariant generic-velocity

      INTEGER     MINSECS             ! min TSTEP(2) (sec) that divides TSTEP(1)

      INTEGER     C, R, L
      INTEGER     EDATE, ETIME
      INTEGER     REP, ADV, STEP, T2, K
      INTEGER     SYNC, NADVS( NLAYS )
      LOGICAL     ADJFLG
      REAL        UOVDX( NLAYS )      ! max { component velocity / dXi }
      REAL        MXUOVDX

      CHARACTER( 16 ) :: PNAME = 'ADVSTEP'
      CHARACTER( 16 ) :: UORV    
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      STATUS             !  ENVINT status

      CHARACTER( 80 ) :: VARDESC      !  environment variable description
      CHARACTER( 80 ) :: MSG = ' '

C environment variable max sync step
      CHARACTER( 16 ) :: CTM_MAXSYNC = 'CTM_MAXSYNC'
C environment variable min sync step
      CHARACTER( 16 ) :: CTM_MINSYNC = 'CTM_MINSYNC'
C environment variable adv layer
      CHARACTER( 16 ) :: SIGMA_SYNC_TOP = 'SIGMA_SYNC_TOP'

      INTEGER MY_TEMP
      INTEGER, SAVE :: STARTCOL, ENDCOL
      INTEGER, SAVE :: STARTROW, ENDROW

!     integer mxcol, mxrow, mxlvl
!     real mxwind, vovdx( nlays )
 
      INTERFACE
         SUBROUTINE HCONTVEL ( FDATE, FTIME, FSTEP, LVL, UORV, WIND )
            IMPLICIT NONE
            INTEGER, INTENT( IN ) :: FDATE, FTIME, FSTEP, LVL
            CHARACTER( 16 ), INTENT( IN ) :: UORV
            REAL, INTENT( OUT ) :: WIND( :,: )
         END SUBROUTINE HCONTVEL
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV()

         MAXSYNC = 720         ! default
         VARDESC = 'Maximum Synchronization Time Step (sec)'
         MAXSYNC = ENVINT( CTM_MAXSYNC, VARDESC, MAXSYNC, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!              ELSE IF ( STATUS .EQ. -1 ) THEN
!              MSG = 'Environment variable set, but empty ... Using default:'
!              WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
!              ELSE IF ( STATUS .EQ. -2 ) THEN
!              MSG = 'Environment variable not set ... Using default:'
!              WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
               END IF

!        MINSYNC = 300         ! default
         MINSYNC = 60          ! default
         VARDESC = 'Minimum Synchronization Time Step (sec)'
         MINSYNC = ENVINT( CTM_MINSYNC, VARDESC, MINSYNC, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!              ELSE IF ( STATUS .EQ. -1 ) THEN
!              MSG = 'Environment variable set, but empty ... Using default:'
!              WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
!              ELSE IF ( STATUS .EQ. -2 ) THEN
!              MSG = 'Environment variable not set ... Using default:'
!              WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
               END IF

         SIGST = 0.7          ! default
         VARDESC = 'Minimum layer limit for which adv = sync'
         SIGST = ENVREAL( SIGMA_SYNC_TOP, VARDESC, SIGST, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

         IF ( VGTYP_GD .NE. VGSGPN3 .AND. VGTYP_GD .NE. VGSGPH3 ) THEN
            XMSG = 'Wrong vertical coordinate type'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

         IF ( SIGST .GT. VGLVS_GD( 2 ) .OR.
     &        SIGST .LT. VGLVS_GD( NLAYS + 1 ) ) THEN
            WRITE( XMSG, '( A, 1PE12.3 )' ) 'SIGMA_SYNC_TOP incorrect', SIGST
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

         IF ( SIGST .EQ. VGLVS_GD( NLAYS + 1 ) ) THEN
            L = NLAYS
            ELSE
            DO K = 1, NLAYS
               IF ( SIGST .GE. VGLVS_GD( K+1 ) ) THEN
                  IF ( SIGST - VGLVS_GD( K+1 ) .LE.
     &                 VGLVS_GD( K ) - SIGST )  THEN
                     L = K
                     ELSE
                     L = K - 1
                     END IF
                     GO TO 101
                  END IF
               END DO
            END IF

101      CONTINUE
         ADVLAYR = L
         WRITE( LOGDEV, 92005 ) ADVLAYR

C Open wind field file and get header data

         IF ( .NOT. OPEN3( MET_DOT_3D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // MET_DOT_3D // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT0 )
            END IF

         IF ( .NOT. DESC3( MET_DOT_3D ) ) THEN
            XMSG = 'Could not get ' // MET_DOT_3D // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         SDATE = SDATE3D
         STIME = STIME3D
         FSTEP = TSTEP3D

C Check file data against COORD.EXT

         IF ( XCELL3D .NE. XCELL_GD .OR. YCELL3D .NE. YCELL_GD ) THEN
            XMSG = 'File grid sizes do not match CTM domain definition'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

C Get cell size in meters (from COORD.EXT)

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            IDX2 = 1.0 / (DG2M * YCELL_GD)
            IDX1 = 1.0 / (DG2M * XCELL_GD
     &           * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( NROWS/2 ))) )
            ELSE
            IDX1 = 1.0 / XCELL_GD
            IDX2 = 1.0 / YCELL_GD
            END IF

C Get the starting wind field: do not interpolate

         IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, FSTEP,
     &                        FDATE, FTIME ) ) THEN
            XMSG = 'Cannot get step-starting date and time'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

          CALL SE_LOOP_INDEX ( 'C', 1, MY_NCOLS, 1, MY_TEMP,
     &                            STARTCOL, ENDCOL )

          CALL SE_LOOP_INDEX ( 'R', 1, MY_NROWS, 1, MY_TEMP,
     &                            STARTROW, ENDROW )

         END IF                    !  if firstime

C Compute the least number of equal time steps that satisfy the Courant
C condition (force TSTEP(2) to be no greater than 15 min.):

      STEP = TIME2SEC( TSTEP( 1 ) )

C Make sure STEP .GE. MINSYNC

      IF ( STEP .LT. MINSYNC ) THEN
         WRITE( LOGDEV,92009 ) STEP, MINSYNC
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C Make sure that MINSYNC divides STEP and is even

      DO REP = MINSYNC, STEP
         IF ( MOD( STEP, REP ) .EQ. 0 ) THEN
            MINSECS = REP
            IF ( MINSECS .NE. MINSYNC )
     &         WRITE( LOGDEV,92011 ) JDATE, JTIME, MINSYNC, MINSECS
            GO TO 201
            END IF
         END DO

C If you get here: could not determine satisfactory MINSECS

      WRITE( XMSG,94011 ) MINSYNC, STEP
      CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

201   CONTINUE
      WRITE( LOGDEV,92013 ) JDATE, JTIME

C Establish ending time for this Courant number calculation:

      EDATE = JDATE
      ETIME = JTIME
      CALL NEXTIME ( EDATE, ETIME, TSTEP( 1 ) )

      IF ( SECSDIFF( FDATE, FTIME, EDATE, ETIME ) .GT. 0 )
C Get the next wind field: do not interpolate
     &   CALL NEXTIME( FDATE, FTIME, FSTEP )

      UORV = 'X1VEL'

      DO L = 1, ADVLAYR
         CALL HCONTVEL ( FDATE, FTIME, WSTEP, L, UORV, WIND )
         UOVDX( L ) = -1.0
         DO R = 1, MY_NROWS
            DO C = STARTCOL, ENDCOL     !     DO C = 1, MY_NCOLS+1
               UOVDX( L ) = MAX( UOVDX( L ), ABS( WIND( C,R ) ) * IDX1 )
!              if ( uovdx( l ) .lt. abs( wind( c,r ) * idx1 ) ) then
!                 uovdx( l ) = abs( wind( c,r ) * idx1 )
!                 mxwind = wind( c,r )
!                 mxcol = c
!                 mxrow = r
!                 mxlvl = l
!                 end if
               END DO
            END DO
         END DO

!     write( logdev,* ) 'umax- c,r,l,wind: ', mxcol, mxrow, mxlvl, mxwind

      UORV = 'X2VEL'

      DO L = 1, ADVLAYR
         CALL HCONTVEL ( FDATE, FTIME, WSTEP, L, UORV, WIND )
!        vovdx( l ) = -1.0
         DO R = STARTROW, ENDROW        !  DO R = 1, MY_NROWS+1
            DO C = 1, MY_NCOLS
               UOVDX( L ) = MAX( UOVDX( L ), ABS( WIND( C,R ) ) * IDX2 )
!              if ( vovdx( l ) .lt. abs( wind( c,r ) * idx2 ) ) then
!                 vovdx( l ) = abs( wind( c,r ) * idx2 )
!                 mxwind = wind( c,r )
!                 mxcol = c
!                 mxrow = r
!                 mxlvl = l
!                 end if
               END DO
            END DO

         END DO

!     write( logdev,* ) 'vmax- c,r,l,wind: ', mxcol, mxrow, mxlvl, mxwind

      MXUOVDX = -1.0
      DO L = 1, ADVLAYR
!        uovdx( l ) = max( uovdx( l ), vovdx( l ) )
         UOVDX( L ) = SE_GLOBAL_MAX ( UOVDX( L ) )
         IF ( UOVDX( L ) .GT. MXUOVDX ) MXUOVDX = UOVDX( L )
         END DO

C Determine sync step, S and adv step, A such that S = A and A satisfies the
C Courant condition (CC) up to layer ADVLAYR and such that S >= MINSYNC. If
C S should be < MINSYNC in order to satisfy the CC, set S = MINSYNC and adjust
C A to satisfy the CC and evenly divide S

      ADJFLG = .FALSE.
      DO REP = 1, STEP
!        write( logdev,* ) 'rep: ', rep
         IF ( MOD( STEP, REP ) .EQ. 0 ) THEN  ! make TSTEP(2) divide TSTEP(1)
            SYNC = STEP / REP
!           write( logdev,* ) 'rep: ', rep, ', sync: ', sync
            IF ( SYNC .LE. MAXSYNC ) THEN        ! force max TSTEP(2)
               ADV = SYNC
!              write( logdev,* ) 'adv, uovdx: ', adv, mxuovdx
               IF ( MXUOVDX * FLOAT( ADV ) .LT. CC ) THEN
!                 write( logdev,* ) 'CC satisfied'
                  IF ( SYNC .GE. MINSECS ) THEN  ! force min TSTEP(2)
                     NREPS = REP
!                    WRITE( LOGDEV,92015 ) SYNC, ADV, NREPS
                     ELSE   ! make ADV divide TSTEP(2) evenly
                     SYNC = MINSECS
                     NREPS = STEP / MINSECS
                     IF ( MOD ( MINSECS,ADV ) .EQ. 0 ) THEN ! make ADV
                        K = MINSECS / ADV            ! divide TSTEP(2)
                        ELSE
                        K = MINSECS / ADV + 1
                        END IF
                     ADV = MINSECS / K
                     ADJFLG = .TRUE.
!                    WRITE( LOGDEV,92017 ) MINSECS, SYNC, ADV, NREPS
                     END IF
                  GO TO 301
                  END IF      ! if Courant condition satisfied
               END IF      ! if SYNC .le. MAXSYNC
            END IF      ! if REP divides STEP evenly
         END DO      ! step loop

C If you get here: could not determine satisfactory advection time step.

         WRITE( LOGDEV,94013 ) TSTEP( 1 ), MXUOVDX
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

301      CONTINUE

      TSTEP( 2 ) = SEC2TIME( SYNC )
      DO L = 1, ADVLAYR
         ASTEP( L ) = SEC2TIME( ADV )
         NADVS( L ) = SYNC / ADV
         END DO

      IF ( ADJFLG ) THEN
         WRITE( LOGDEV,92019 ) MINSECS, TSTEP( 2 ), NREPS
         ELSE
         WRITE( LOGDEV,92021 ) TSTEP( 2 ), NREPS
         END IF

      UORV = 'X1VEL'

      DO L = ADVLAYR + 1, NLAYS
         CALL HCONTVEL ( FDATE, FTIME, WSTEP, L, UORV, WIND )
         UOVDX( L ) = -1.0
         DO R = 1, MY_NROWS
            DO C = STARTCOL, ENDCOL     !     DO C = 1, MY_NCOLS+1
               UOVDX( L ) = MAX( UOVDX( L ), ABS( WIND( C,R ) ) * IDX1 )
!              if ( uovdx( l ) .lt. abs( wind( c,r ) * idx1 ) ) then
!                 uovdx( l ) = abs( wind( c,r ) * idx1 )
!                 mxwind = wind( c,r )
!                 mxcol = c
!                 mxrow = r
!                 mxlvl = l
!                 end if
               END DO
            END DO
         END DO

!     write( logdev,* ) 'umax- c,r,l,wind: ', mxcol, mxrow, mxlvl, mxwind

      UORV = 'X2VEL'

      DO L = ADVLAYR + 1, NLAYS
         CALL HCONTVEL ( FDATE, FTIME, WSTEP, L, UORV, WIND )
!        vovdx( l ) = -1.0
         DO R = STARTROW, ENDROW        !  DO R = 1, MY_NROWS+1
            DO C = 1, MY_NCOLS
               UOVDX( L ) = MAX( UOVDX( L ), ABS( WIND( C,R ) ) * IDX2 )
!              if ( vovdx( l ) .lt. abs( wind( c,r ) * idx2 ) ) then
!                 vovdx( l ) = abs( wind( c,r ) * idx2 )
!                 mxwind = wind( c,r )
!                 mxcol = c
!                 mxrow = r
!                 mxlvl = l
!                 end if
               END DO
            END DO

         END DO

!     write( logdev,* ) 'vmax- c,r,l,wind: ', mxcol, mxrow, mxlvl, mxwind

      DO L = ADVLAYR + 1, NLAYS
!        uovdx( l ) = max( uovdx( l ), vovdx( l ) )
         UOVDX( L ) = SE_GLOBAL_MAX ( UOVDX( L ) )
         END DO

      T2 = TIME2SEC( TSTEP( 2 ) )
      DO L = ADVLAYR + 1, NLAYS
!        write( logdev,* ) 'layer: ', l
         ADV = T2 + 1
         DO REP = 1, STEP
            ADV = ADV - 1    ! subtract 1 sec
            IF ( UOVDX( L ) * FLOAT( ADV ) .LT. CC ) THEN
!              write( logdev,* ) 'CC satisfied - layer: ', l,
!    &                           ',  adv, uovdx: ', adv, uovdx( l )
               IF ( ADV .EQ. T2 ) THEN
                  NADVS( L ) = T2 / ADV
                  ELSE
                  NADVS( L ) = T2 / ADV + 1
                  END IF
               ASTEP( L ) = SEC2TIME( T2 / NADVS( L ) )
!              WRITE( LOGDEV,92015 ) L, ASTEP( L ), NADVS( L )
               GO TO 401
               END IF              !  if Courant condition satisfied
            END DO

C If you get here: could not determine satisfactory advection time step.

         WRITE( LOGDEV,94013 ) TSTEP( 1 ), L, UOVDX( L )
         XMSG = ' '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )

401      CONTINUE

         END DO   ! layer loop

      WRITE( LOGDEV,92025 )
      DO L = NLAYS, 1, -1
         WRITE( LOGDEV,92027 ) L, ASTEP( L ), NADVS( L )
         END DO
      WRITE( LOGDEV,* ) ' '

      RETURN

C------------------  FORMAT  STATEMENTS   ------------------------------

92005 FORMAT (   5X, 'Top layer thru which sync step determined:', I3 )

92009 FORMAT ( / 5X, 'Output time step:', I8,
     &           1X, 'less than minimum synchronization step:', I8 )

92011 FORMAT ( / 5X, 'From ADVSTEP - date/time: ', I8, '/', I6.6
     &         / 5X, 'Minimum Synchronization Step adjusted from:', I8,
     &           1X, 'to:', I8 )
 
92013 FORMAT ( / 5X, 'From ADVSTEP - date/time: ', I8, '/', I6.6 )
 
92019 FORMAT ( / 5X, 'Synchronization step adjusted up to mimimum (SEC):', I7
     &         /46X, '(HHMMSS): ', I6.6
     &         / 5X, 'Number of Synchronization steps:', I3 /)
 
92021 FORMAT ( / 5X, 'Computed synchronization step (HHMMSS): ', I6.6
     &         / 5X, 'Number of Synchronization steps:', I3 /)
 
92025 FORMAT ( / 5X, 'Layer', 3X, 'Advection', 3X, 'per Sync'
     &         /11X, 'Step (HHMMSS)', 2X, 'Step' )

92027 FORMAT (   5X, I4, 6X, I6.6, 6X, I2 )
 
94011 FORMAT( / 5X, 'Starting from:', I6, ',',
     &          1X, 'could not determine minimum step that divides TSTEP',
     &          1X, 'for model step:', I7.6, ' HHMMSS')
 
94013 FORMAT( / 5X, 'Could not determine Courant-condition safe sync step',
     &          1X, 'for model step:', I7.6, ' HHMMSS',
     &          1X, 'in layer:', I3
!    &          1X, '(Max vel) / (dX1 or dX2) =', 1PE10.3)
     &        / 5X, '(Max vel)/(dX) =', 1PE10.3)

      END
