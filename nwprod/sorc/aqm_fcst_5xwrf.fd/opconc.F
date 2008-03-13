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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/init/yamo/opconc.F,v 1.1.1.1 2005/09/09 18:56:06 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPCONC ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output CONC file

C Revision history:
C   Jeff - Dec 00 - split out from initscen.F
C                 - move CGRID_MAP into f90 module
C   Jeff - Feb 01 - assumed shape arrays
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C
C    3 Sep 01 David Wong
C     -- let PE 0 open CTM_CONC_1 as new and later on let the rest open
C        it for read and write
C     -- put an explicit barrier before opening a new netCDF file to avoid
C        NCOPEN error
C    7 May 03 J.Young: open and close conc file in processor 0; spin-wait to
C                      deal with nfs network latency for mpich cluster
C   28 Aug 03 J.Young: following Zion Wang at CERT, remove the spin-wait and
C                      simplify opening and closing CONC file (see initscen)
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   30 May 05 J.Young: optional save derived vert. vel. to conc file
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID species number and offsets
      USE WVEL_DEFN             ! derived vertical velocity component

      USE SUBST_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_GC_SPC      ! gas chemistry species table
      INCLUDE SUBST_AE_SPC      ! aerosol species table
      INCLUDE SUBST_NR_SPC      ! non-reactive species table
      INCLUDE SUBST_TR_SPC      ! tracer species table
      INCLUDE SUBST_GC_CONC     ! gas chem conc file species and map table
      INCLUDE SUBST_AE_CONC     ! aerosol conc file species and map table
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC     ! file header data structure
#include      SUBST_IODECL      # I/O definitions and declarations
      INCLUDE SUBST_FILES_ID    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coord. and domain definitions (req IOPARMS)

C Arguments:

!     REAL      :: CGRID( :,:,:,: )  ! for initial CONC
      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
      INTEGER      JDATE        ! starting date (YYYYDDD)
      INTEGER      JTIME        ! starting time (HHMMSS)
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: TRIMLEN      !  string length, excl. trailing blanks

C Local Variables:

      REAL, ALLOCATABLE :: DBUFF( :,:,: )
      INTEGER      ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'OPCONC'
      CHARACTER( 96 ) :: XMSG = ' '
 
      INTEGER      LOGDEV

      INTEGER      L, SPC, V    ! loop counters
      INTEGER      STRT, FINI   ! loop counters
      INTEGER      INDX

C-----------------------------------------------------------------------

      LOGDEV = INIT3()

C Get CGRID offsets

      CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

C Set output file characteristics based on COORD.EXT and open it

      FTYPE3D = GRDDED3
      SDATE3D = JDATE
      STIME3D = JTIME
      TSTEP3D = TSTEP
!     CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D )
      NVARS3D = N_GC_CONC + N_AE_CONC + N_NR_SPC + N_TR_SPC + 1 ! for RHOJ
      IF ( W_VEL ) NVARS3D = NVARS3D + 1   ! for W_YAMO
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = NLAYS
      NTHIK3D = 1
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD 
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
!     VGTPUN3D = VGTPUN_GD ! currently, not defined
      DO L = 1, NLAYS3D + 1
         VGLVS3D( L ) = VGLVS_GD( L )
         END DO
!     GDNAM3D = GDNAME_GD
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN

      FDESC3D( 1 ) = 'Concentration file output'
      FDESC3D( 2 ) = 'From CMAQ model dyn alloc version CTM'
      DO SPC = 3, MXDESC3
         FDESC3D( SPC ) = ' '
         END DO

      V = 0
      STRT = 1
      FINI = N_GC_CONC
      DO SPC = STRT, FINI
         V = V + 1
         INDX = GC_CONC_MAP( V )
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = GC_SPC( INDX )
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

      FINI = FINI + 1
      SPC = FINI
      VTYPE3D( SPC ) = M3REAL
      VNAME3D( SPC ) = 'RHOJ'
      UNITS3D( SPC ) = 'm'
      VDESC3D( SPC ) = 'advected air density X total Jacobian'

      V = 0
      STRT = FINI + 1     ! STRT = N_GC_CONC + 2
      FINI = FINI + N_AE_CONC
      DO SPC = STRT, FINI
         V = V + 1
         INDX = AE_CONC_MAP( V )
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = AE_SPC( INDX )   ! from include file
         IF ( VNAME3D( SPC )(1:3) .EQ. 'NUM' ) THEN
            UNITS3D( SPC ) = 'number/m**3'
            ELSE IF ( VNAME3D( SPC )(1:3) .EQ. 'SRF' ) THEN
            UNITS3D( SPC ) = 'm**2/m**3'
            ELSE
            UNITS3D( SPC ) = 'micrograms/m**3'
            END IF
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

      V = 0
      STRT = FINI + 1     ! STRT = N_GC_CONC + N_AE_CONC + 2
      FINI = FINI + N_NR_SPC ! write all NR species
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = NR_SPC( V )   ! from include file
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

      V = 0
      STRT = FINI + 1     ! STRT = N_GC_CONC + N_AE_CONC + N_NR_SPC + 2
      FINI = FINI + N_TR_SPC ! write all TR species
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = TR_SPC( V )   ! from include file
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

      IF ( W_VEL ) THEN   ! for W_YAMO
         SPC = FINI + 1 
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = 'W_VEL'
         UNITS3D( SPC ) = 'm/s'
         VDESC3D( SPC ) = 'Derived vertical velocity component'
         END IF

C create header

      IF ( MYPE .EQ. 0 ) THEN   ! open new
         IF ( .NOT. OPEN3( CTM_CONC_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open ' // CTM_CONC_1 // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

C write the initial concentrations as step 0 on the conc file
C (inital data assumed to be in correct output units)

      ALLOCATE ( DBUFF( MY_NCOLS,MY_NROWS,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DBUFF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

      DO SPC = 1, N_GC_CONC
         V = GC_STRT - 1 + GC_CONC_MAP( SPC )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,V )

         IF ( .NOT. WRITE3( CTM_CONC_1, GC_CONC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             GC_CONC( SPC )(1:TRIMLEN( GC_CONC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO

      V = GC_STRT - 1 + N_GC_SPCD

      DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,V )

      IF ( .NOT. WRITE3( CTM_CONC_1, 'RHOJ',
     &                   JDATE, JTIME, DBUFF ) ) THEN
         XMSG = 'Could not write RHOJ' //
     &          ' to ' // CTM_CONC_1
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      DO SPC = 1, N_AE_CONC
         V = AE_STRT - 1 + AE_CONC_MAP( SPC )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,V )

         IF ( .NOT. WRITE3( CTM_CONC_1, AE_CONC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             AE_CONC( SPC )(1:TRIMLEN( AE_CONC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO

      V = NR_STRT - 1
      DO SPC = 1, N_NR_SPC
         V = V + 1

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,V )

         IF ( .NOT. WRITE3( CTM_CONC_1, NR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             NR_SPC( SPC )(1:TRIMLEN( NR_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO

      V = TR_STRT - 1
      DO SPC = 1, N_TR_SPC
         V = V + 1

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,V )

         IF ( .NOT. WRITE3( CTM_CONC_1, TR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             TR_SPC( SPC )(1:TRIMLEN( TR_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO

!     write( logdev,* ) '-> before w_vel write'
!     write( logdev,* ) '-> wvel: ',
!    &                     wvel(        1,       1,    1 ),
!    &                     wvel( my_ncols,       1,    1 ),
!    &                     wvel(        1,my_nrows,    1 ),
!    &                     wvel( my_ncols,my_nrows,    1 )
!     write( logdev,* ) '-> wvel: ',
!    &                     wvel(        1,       1,nlays ),
!    &                     wvel( my_ncols,       1,nlays ),
!    &                     wvel(        1,my_nrows,nlays ),
!    &                     wvel( my_ncols,my_nrows,nlays )

      IF ( W_VEL ) THEN   ! for W_YAMO
         IF ( .NOT. WRITE3( CTM_CONC_1, 'W_VEL',
     &                      JDATE, JTIME, WVEL ) ) THEN
            XMSG = 'Could not write W_VEL to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

!     write( logdev,* ) '->  after w_vel write'

      DEALLOCATE ( DBUFF )

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &  'Timestep written to', CTM_CONC_1( 1:TRIMLEN( CTM_CONC_1 ) ),
     &  'for date and time', JDATE, JTIME
      WRITE( LOGDEV, '(  5X,  A, 1X, I8, ":", I6.6 )' )
     &  'from timestep on initial data files for date and time',
     &   JDATE, JTIME

      RETURN
      END
