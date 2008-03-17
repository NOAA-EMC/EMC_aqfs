
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
C $Header: /project/cmaq/rel/models/CCTM/src/init/init/initscen.F,v 1.1.1.1 2002/06/27 11:25:50 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)initscen.F        1.4 /project/mod3/CMAQ/src/init/init/SCCS/s.initscen.F 25 Jul 1997 14:32:52

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INITSCEN ( STDATE, STTIME, TSTEP, NSTEPS, LOGDEV )

C-----------------------------------------------------------------------
C Function:
C   Initialize simulation time period and time stepping constants for
C   core model driver
C   Environment variable can reference a previous CONC file to use as
C   initial data.
C   Write initial conc data as step "0" on output conc file
 
C Preconditions:
 
C Subroutines and functions called:
C   INIT3, M3EXIT, OPEN3, CLOSE3, DESC3, ENVINT, TIME2SEC, HHMMSS
 
C Revision history:
C   23 Nov 02 J.Young: 
C-----------------------------------------------------------------------

!     USE CGRID_DEFN            ! inherits HGRD_DEFN and CGRID_SPCS

!     USE SE_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

C Includes:

      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"     ! I/O parameters definitions

C Arguments:

      INTEGER      STDATE       ! starting date,    format YYYYDDD
      INTEGER      STTIME       ! starting time,    format HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = advection time step
      INTEGER      NSTEPS       ! run duration, as number of output time steps
      INTEGER      LOGDEV       ! FORTRAN unit number for log file

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: ENVINT       !  get environment variable as integer
      INTEGER STATUS                    !  ENVINT status
      INTEGER, EXTERNAL :: TIME2SEC     !  converts HHMMSS to raw seconds

      CHARACTER( 10 ), EXTERNAL :: HHMMSS  !  converts to string "HH:MM:SS"

C Local Variables

C  environment variable start date
      CHARACTER( 16 ) :: CTM_STDATE = 'CTM_STDATE'
C  environment variable start time
      CHARACTER( 16 ) :: CTM_STTIME = 'CTM_STTIME'
C  environment variable run duration
      CHARACTER( 16 ) :: CTM_RUNLEN = 'CTM_RUNLEN'
C  environment variable output time step
      CHARACTER( 16 ) :: CTM_TSTEP = 'CTM_TSTEP'
C driver program name
      CHARACTER( 16 ) :: CTM_PROGNAME = 'CTM_PROGNAME'
C  environment variable description
      CHARACTER( 80 ) :: VARDESC

      CHARACTER( 16 ) :: PNAME = 'INITSCEN'
      CHARACTER( 16 ) :: PROGNAME
      CHARACTER( 96 ) :: MSG = ' '

      INTEGER      RUNLEN            ! run duration, HHMMSS
      INTEGER      STEPSECS          ! seconds per time step
      INTEGER      TOTSECS           ! run duration seconds

C-----------------------------------------------------------------------

!     LOGDEV = INIT3()

      VARDESC = 'Main Program Name'
      CALL ENVSTR( CTM_PROGNAME, VARDESC, 'DRIVER', PROGNAME, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
            ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            END IF

      STDATE = 1995192        ! default
      VARDESC = 'Scenario Starting Date (YYYYDDD)'
      STDATE = ENVINT( CTM_STDATE, VARDESC, STDATE, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
            ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            END IF

      STTIME = 000000         ! default
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      STTIME = ENVINT( CTM_STTIME, VARDESC, STTIME, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
            ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            END IF

      RUNLEN = 480000         ! default
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = ENVINT( CTM_RUNLEN, VARDESC, RUNLEN, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
            ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            END IF

      TSTEP( 1 )  = 010000         ! default
      VARDESC = 'Scenario Output Time Step (HHMMSS)'
      TSTEP( 1 ) = ENVINT( CTM_TSTEP, VARDESC, TSTEP( 1 ), STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            MSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT2 )
            ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, STTIME
            END IF

C Calculate number of output time steps for this model run:

      IF ( RUNLEN .LT. 1000000 ) THEN
         TOTSECS  = TIME2SEC( RUNLEN )
         ELSE
         RUNLEN = RUNLEN - 1000000
         TOTSECS  = TIME2SEC( RUNLEN )
         TOTSECS  = TOTSECS + 360000
         END IF
      STEPSECS = TIME2SEC( TSTEP( 1 )  )

      IF ( MOD( TOTSECS, STEPSECS ) .EQ. 0 ) THEN
         NSTEPS = TOTSECS / STEPSECS
         ELSE
         MSG = 'Output time step ' // HHMMSS( TSTEP( 1 ) ) //
     &         ' does not divide duration ' // HHMMSS( RUNLEN )
         CALL M3EXIT( PNAME, STDATE, STTIME, MSG, XSTAT1 )
         END IF

      RETURN

      END SUBROUTINE INITSCEN
