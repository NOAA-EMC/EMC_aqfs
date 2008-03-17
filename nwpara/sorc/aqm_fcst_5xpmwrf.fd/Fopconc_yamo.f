
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
C $Header: /project/work/rep/CCTM/src/init/init/opconc.F,v 1.5 2002/04/05 18:23:17 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPCONC ( JDATE, JTIME, TSTEP, LOGDEV )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output CONC file

C Revision history:
C   Jeff - Dec 00 - split out from initscen.F
C                 - move CGRID_MAP into f90 module
C   Jeff - Feb 01 - assumed shape arrays
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C
C   3 Sep 01 David Wong
C     -- let PE 0 open CTM_CONC_1 as new and later on let the rest open
C        it for read and write
C     -- put an explicit barrier before opening a new netCDF file to avoid
C        NCOPEN error
C
C   1/03 - JP Added RHOJ to CONC file for diagnostics        
C   3/03 - JY Fixed for non-zero initial data
C   9/07 - JY changed NPROCS-1 to globally-known N_WORKERS
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE MPIM
      USE WVEL_DEFN             ! derived vertical velocity component

      USE SE_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_SPC.EXT"      ! gas chemistry species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_SPC.EXT"      ! aerosol species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_SPC.EXT"      ! non-reactive species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_SPC.EXT"      ! tracer species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_CONC.EXT"     ! gas chem conc file species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_CONC.EXT"     ! aerosol conc file species and map table
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/FDESC3.EXT"     ! file header data structure
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"      ! I/O definitions and declarations
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/FILES_CTM.EXT"    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coord. and domain definitions (req IOPARMS)
      INCLUDE 'TAG.EXT'

C Arguments:

      INTEGER      JDATE        ! starting date (YYYYDDD)
      INTEGER      JTIME        ! starting time (HHMMSS)
      INTEGER      TSTEP        ! output timestep (HHMMSS)
      INTEGER      LOGDEV

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: TRIMLEN      !  string length, excl. trailing blanks

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'OPCONC_YAMO'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      I, S, L, SPC, V    ! loop counters
      INTEGER      C,R
      INTEGER      STRT, FINI         ! loop counters
      INTEGER      INDX

      INTEGER   :: ISTEP = 0
      INTEGER   :: CONC_GSZE = 0, CGRD_DSZE = 0, CGRD_NVARS = 0
      INTEGER      CONC_OFFSET_CR, CONC_FG_NCOLS
      INTEGER      CONC_NLVLS, CONC_NVARS, CONC_NPE
      REAL, ALLOCATABLE    :: CONC_DATA( : )
      LOGICAL, ALLOCATABLE :: CONC_SPCHIT( : ), CONC_WRITTEN( : )
      LOGICAL DONE
      INTEGER ASTAT

      integer bounce

      INTERFACE
         SUBROUTINE ASSEMBLE ( ISTEP, TAG, logdev,
     &                         FG_NCOLS, OFFSET_CR,
     &                         NVARS, NLVLS, DSZE,
     &                         SPCHIT, WRITTEN, NPE, DATA, bounce )
         IMPLICIT NONE
         INTEGER, INTENT( IN ) :: ISTEP, TAG, logdev,
     &                            FG_NCOLS, OFFSET_CR,
     &                            NVARS, NLVLS, DSZE
         LOGICAL, INTENT( IN ) :: SPCHIT( : )
         LOGICAL, INTENT( INOUT ) :: WRITTEN( : )
         INTEGER, INTENT( INOUT ) :: NPE, bounce
         REAL,    INTENT( OUT ) :: DATA( : )
         END SUBROUTINE ASSEMBLE
      END INTERFACE
C-----------------------------------------------------------------------

C Get CGRID offsets

      CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

      IF ( W_VEL ) THEN
         CGRD_NVARS = NSPCSD + 1
      ELSE
         CGRD_NVARS = NSPCSD
      END IF

C Set output file characteristics based on COORD.EXT and open it

      FTYPE3D = GRDDED3
      SDATE3D = JDATE
      STIME3D = JTIME
      TSTEP3D = TSTEP
!     CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D )
      NVARS3D = N_GC_CONC + N_AE_CONC + N_NR_SPC + N_TR_SPC + 1 ! either RHOJ or W_YAMO
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = NLAYS
      NTHIK3D =     1
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

      IF ( .NOT. W_VEL ) THEN
         FINI = FINI + 1
         SPC = FINI 
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = 'RHOJ'
         UNITS3D( SPC ) = 'coupled kg/m3'
         VDESC3D( SPC ) = 'advected air density X total Jacobian'
      END IF

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
      FINI = FINI + N_TR_SPC ! write all
      DO SPC = STRT, FINI                                ! TR species
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = TR_SPC( V )   ! from include file
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
      END DO

      IF ( W_VEL ) THEN
         SPC = FINI + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = 'W_VEL'
         UNITS3D( SPC ) = 'm/s'
         VDESC3D( SPC ) = 'Derived vertical velocity component'
      END IF

      IF ( .NOT. OPEN3( CTM_CONC_1, FSNEW3, PNAME ) ) THEN
         XMSG = 'Could not open ' // CTM_CONC_1 // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C write the initial concentrations as step 0 on the conc file
C (inital data assumed to be in correct output units)

      CONC_FG_NCOLS  = NCOLS3D
      CONC_OFFSET_CR = NCOLS3D * NROWS3D
      CONC_NLVLS     = NLAYS3D
      CONC_NVARS     = NVARS3D

      CGRD_DSZE = ( MY_NCOLS + 1 ) * ( MY_NROWS + 1 ) * NLAYS * CGRD_NVARS
      CONC_GSZE = NCOLS3D * NROWS3D * CONC_NLVLS * CONC_NVARS

      ALLOCATE ( CONC_DATA( CONC_GSZE ), STAT=ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         XMSG = 'Error allocating memory for CONC_DATA'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      ALLOCATE ( CONC_SPCHIT( CGRD_NVARS ), STAT=ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         XMSG = 'Error allocating memory for CONC_SPCHIT'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      CONC_SPCHIT = .FALSE.
      V = 0
      DO S = 1, N_GC_CONC
         I = GC_CONC_MAP( S )
         CONC_SPCHIT( V + I ) = .TRUE.
      END DO
      V = V + N_GC_SPC + 1     ! advected RhoJ
      IF ( .NOT. W_VEL ) THEN
         CONC_SPCHIT( V ) = .TRUE.
      END IF
      DO S = 1, N_AE_CONC
         I = AE_CONC_MAP( S )
         CONC_SPCHIT( V + I ) = .TRUE.
      END DO
      V = V + N_AE_SPC
      DO S = 1, N_NR_SPC
         CONC_SPCHIT( V + S ) = .TRUE.
      END DO
      V = V + N_NR_SPC
      DO S = 1, N_TR_SPC
         CONC_SPCHIT( V + S ) = .TRUE.
      END DO
      IF ( W_VEL ) THEN
         V = V + N_TR_SPC + 1    ! derived vert vel comp
         CONC_SPCHIT( V ) = .TRUE.
      END IF

!     do s = 1, CGRD_NVARS
!        write( logdev,* ) '   CONC_SPCHIT: ', s, conc_spchit( s )
!        end do

!     write( logdev,* ) ' CGRD_DSZE, CONC_GSZE:   ', CGRD_DSZE, CONC_GSZE

      ALLOCATE ( CONC_WRITTEN( N_WORKERS ), STAT=ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         XMSG = 'Error allocating memory for CONC_WRITTEN'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      DONE = .FALSE.
      CONC_WRITTEN = .FALSE.
      CONC_NPE = 0
      DO WHILE ( .NOT. DONE )

         CALL ASSEMBLE ( ISTEP, CONC_TAG, logdev,
     &                   CONC_FG_NCOLS, CONC_OFFSET_CR,
     &                   CGRD_NVARS, NLAYS, CGRD_DSZE,
     &                   CONC_SPCHIT, CONC_WRITTEN, CONC_NPE, CONC_DATA,
     &                   bounce )

         IF ( CONC_NPE .GE. N_WORKERS ) THEN

            IF ( .NOT. WRITE3( CTM_CONC_1, 'ALL', JDATE, JTIME,
     &                         CONC_DATA ) ) THEN
               XMSG = 'Error writing CTM_CONC_1'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', TRIM( CTM_CONC_1 ),
     &            'for date and time', JDATE, JTIME

            DONE = .TRUE.

         END IF

      END DO

      RETURN

      END SUBROUTINE OPCONC
