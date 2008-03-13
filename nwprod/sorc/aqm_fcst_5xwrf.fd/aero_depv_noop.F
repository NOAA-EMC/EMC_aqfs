
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
C $Header: /project/cmaq/rel/models/CCTM/src/aero_depv/aero_depv_noop/aero_depv_noop.F,v 1.1.1.1 2002/06/27 11:26:00 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)aero_depv_noop.F  1.1 /project/mod3/CMAQ/src/ae_depv/aero_depv_noop/SCCS/s.aero_depv_noop.F 03 Jun 1997 12:00:45

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_DEPV ( JDATE, JTIME, TSTEP, VDEP_AE )
         
C no-op aerosol dry deposition

C 31 Mar 01 J.Young: Use HGRD_DEFN; generic CGRID, VDEP_AE declarations

      USE CGRID_DEFN            ! inherits HGRD_DEFN and CGRID_SPCS

      IMPLICIT NONE
 
C Includes:
 
!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_AE_DEPV     ! aerosol dep vel surrogate names and map table
!     INCLUDE SUBST_IODECL      ! I/O definitions and declarations
#include      SUBST_IODECL      ! I/O definitions and declarations

c Arguments

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! sciproc sync. step (chem)

      REAL      :: VDEP_AE( :,:,: ) ! surrogate dep vel's [ m s**-1 ]

      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

      INTEGER C, R, V      ! loop counters

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV ()
         WRITE( LOGDEV,* ) '    '
         WRITE( LOGDEV,* ) '    Using no-op aerosol deposition velocity process'
         WRITE( LOGDEV,* ) '    '
         END IF

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            DO V = 1, N_AE_DEPV
               VDEP_AE( V,C,R ) = 0.0
               END DO
            END DO
         END DO


      RETURN
      END
