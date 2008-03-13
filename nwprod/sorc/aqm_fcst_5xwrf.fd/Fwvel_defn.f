
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
C $Header: /project/work/rep/CCTM/src/driver/ctm/AVG_CONC.F,v 1.3 2002/04/05 18:23:07 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE WVEL_DEFN

C Function: capture the derived vertical velocity component to write to the
C           conc file

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: WVEL( :,:,: )
      LOGICAL, SAVE :: W_VEL

      CONTAINS

         FUNCTION WVEL_INIT () RESULT ( SUCCESS )

         USE HGRD_DEFN             ! horizontal domain specifications
         USE VGRD_DEFN             ! vertical layer specifications

         INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/PARMS3.EXT"     ! I/O parameters definitions
         INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"      ! I/O definitions and declarations

         LOGICAL SUCCESS

         CHARACTER( 16 ) :: PNAME = 'WVEL_INIT'
         CHARACTER( 16 ) :: CTM_WVEL = 'CTM_WVEL'
         CHARACTER( 96 ) :: XMSG = ' '

         LOGICAL, EXTERNAL :: ENVYN

         INTEGER ALLOCSTAT, IERR
         INTEGER :: JDATE = 0, JTIME = 0

C-----------------------------------------------------------------------

!        logdev = setup_logdev ()

         SUCCESS = .TRUE.

         W_VEL = .FALSE.         ! default
         W_VEL = ENVYN( CTM_WVEL, 'Write vert. vel. flag', W_VEL, IERR )
         IF ( IERR .GT. 0 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
            END IF

         IF ( W_VEL ) THEN

            ALLOCATE ( WVEL( MY_NCOLS,MY_NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'WVEL memory allocation failed'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            WVEL = 0.0

            END IF

         END FUNCTION WVEL_INIT

      END MODULE WVEL_DEFN
