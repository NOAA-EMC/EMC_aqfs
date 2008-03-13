
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
      FUNCTION SETUP_LOGDEV () RESULT ( LOGDEV )

C-----------------------------------------------------------------------
C Function:

C Preconditions:

C Subroutines and functions called:

C Revision History:
C   Feb 03 - Dave Wong, Jeff: created
C-----------------------------------------------------------------------

      USE MPIM

      IMPLICIT NONE
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"      ! I/O definitions and declarations

      CHARACTER( 20 ) :: EQNAME
      CHARACTER(  5 ) :: LOGSTR = '_log_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER( 11 ) :: APPL = 'APPLICATION'
      CHARACTER( 40 ) :: IOLOGEQ
      INTEGER, EXTERNAL :: TRIMLEN
      INTEGER LOGDEV

      IF ( G_MYPE .NE. 0 ) THEN
         CALL NAMEVAL ( APPL, EQNAME  )
         WRITE ( CMYPE, '( I3.3 )' ) G_MYPE
         IOLOGEQ = EQNAME( 1:TRIMLEN( EQNAME ) ) // LOGSTR // CMYPE
!        CALL PUTENVVAR( 'LOGFILE', IOLOGEQ )
         CALL SETENVVAR( 'LOGFILE', IOLOGEQ )
      END IF

      LOGDEV = INIT3()

      END FUNCTION SETUP_LOGDEV
