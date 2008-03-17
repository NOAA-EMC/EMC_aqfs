
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
      MODULE DDEP_DEFN

C-----------------------------------------------------------------------
C Function:

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)

C Subroutines and functions called:

C Revision history:
C   Jan 03 - Jeff

C-----------------------------------------------------------------------

      USE HGRD_DEFN    ! horizontal domain specifications

      IMPLICIT NONE

      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_DDEP.EXT"   ! gas chem dry dep species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_DEPV.EXT"   ! gas chem dep vel surrogate names and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_DDEP.EXT"   ! aerosol dry dep species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_DEPV.EXT"   ! aerosol dep vel surrogate names and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_DDEP.EXT"   ! non-react dry dep species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_DEPV.EXT"   ! non-react dep vel surrogate names and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_DDEP.EXT"   ! tracer dry dep species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_DEPV.EXT"   ! tracer dep vel surrogate names and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/CONST.EXT"     ! constants

C all dry dep species
      INTEGER, PARAMETER :: N_SPC_DDEP = N_GC_DDEP
     &                                 + N_AE_DDEP
     &                                 + N_NR_DDEP
     &                                 + N_TR_DDEP
C all dep vel species
      INTEGER, PARAMETER :: N_SPC_DEPV = N_GC_DEPV
     &                                 + N_AE_DEPV
     &                                 + N_NR_DEPV
     &                                 + N_TR_DEPV

!     REAL, SAVE :: DD_CONV( N_SPC_DEPV+1 )     ! ddep spc conversion factors
      REAL, SAVE :: DD_CONV( N_SPC_DEPV )     ! ddep spc conversion factors

      REAL, ALLOCATABLE, SAVE :: DDEP( :,:,: )

      CONTAINS
         FUNCTION DDEP_INIT () RESULT ( SUCCESS )

         INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_SPC.EXT"    ! gas chemistry species table
         INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_SPC.EXT"    ! aerosol species table
         INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_SPC.EXT"    ! non-reactive species table
         INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_SPC.EXT"    ! tracer species table
         INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/CONST.EXT"     ! constants

         INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"    ! I/O definitions and declarations

         LOGICAL SUCCESS
         INTEGER ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '
         REAL, PARAMETER :: M2PHA = 1.0E+04       ! 1 hectare = 1.0e4 m**2
         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio
         REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep
                                                          ! conversion factor
         REAL, PARAMETER :: GPKG = 1.0E+03        ! g/Kg
         REAL, PARAMETER :: MGPG = 1.0E+06        ! micro-g/g
         INTEGER LCOL, HCOL, LROW, HROW
         INTEGER S, V

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           LOGDEV = INIT3()
            LOGDEV = SETUP_LOGDEV ()

!           ALLOCATE ( DDEP( N_SPC_DEPV,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            ALLOCATE ( DDEP( MY_NCOLS,MY_NROWS,N_SPC_DEPV ), STAT = ALLOCSTAT )

            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DDEPD'
               CALL M3WARN ( 'DDEP_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            DDEP = 0.0

C set conversion factor

            S = 0
            DO V = 1, N_GC_DEPV
               S = S + 1
               DD_CONV( S ) = CNVTD * GC_MOLWT( GC_DEPV_MAP( V ) )
               END DO
            DO V = 1, N_AE_DEPV
               S = S + 1
               IF ( AE_SPC( AE_DEPV_MAP( V ) )( 1:3 ) .EQ. 'NUM' ) THEN
                  DD_CONV( S ) = M2PHA
                  ELSE IF ( AE_SPC( AE_DEPV_MAP( V ) )( 1:3 ) .EQ. 'SRF' ) THEN
                  DD_CONV( S ) = M2PHA
                  ELSE
                  DD_CONV( S ) = M2PHA / GPKG / MGPG
                  END IF
               END DO
            DO V = 1, N_NR_DEPV
               S = S + 1
               DD_CONV( S ) = CNVTD * NR_MOLWT( NR_DEPV_MAP( V ) )
               END DO
            DO V = 1, N_TR_DEPV
               S = S + 1
               DD_CONV( S ) = CNVTD * TR_MOLWT( TR_DEPV_MAP( V ) )
               END DO

            ELSE
            XMSG = 'DDEP already ALLOCATED'
            CALL M3WARN ( 'DDEP_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

          RETURN
          END FUNCTION DDEP_INIT

      END MODULE DDEP_DEFN
