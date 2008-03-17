
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
C $Header: /project/work/rep/CCTM/src/vadv/vppm/zadvppm.F,v 1.10 2002/04/05 18:23:42 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ZADV ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Advection in the vertical, x3-direction:
C   The process time step is set equal to TSTEP
 
C Preconditions:
C   Dates and times represented YYYYDDD:HHMMSS.
C   No "skipped" dates and times. Process time step divides TSTEP exactly
C   CGRID in transport units: SQRT{DET[metric tensor]}*concentration (Mass/Vol)
 
C Subroutines and functions called:
C   TIME2SEC
 
C Revision history:
C   02/19/93 by M. Talat Odman  at NCSC
C   05/17/93 by Carlie J. Coats at NCSC:  now uses INTERP3()
C   06/14/94 by Dongming Hwang at NCSC: 
C              include statement and subroutine name template
C   10/15/95 by M. Talat Odman at NCSC: generalized coordinates

C   Sep 97 Jeff
C   Aug 98 Jeff better Courant condition tstep limit

C    David Wong, Sep. 1998
C      -- parallelized the code

C    15 Dec 00 J.Young: move CGRID_MAP into f90 module
C                       GLOBAL_RSUM -> Dave Wong's f90 stenex GLOBAL_SUM
C                       GLOBAL_ISUM -> Dave Wong's f90 stenex GLOBAL_SUM

C    28 Jul 01 J.Young: allocatable arrays ...
C                       Since F90 does not preserve dummy argument array
C                       indices, the 3rd dimension of WHAT has been changed
C                       from 0:NLAYS to 1:NLAYS+1 for the sake of vcontvel

C    03 Sep 01 David Wong
C      -- inserted F90 DEALLOCATE statement for NX3
C 
C   1/03 - JP modified for Yamo mass conservation
C          Vertical velocity is diagnosed from mass continuity
C          vertical advection is upstream (no call to adv scheme)
C          Diagnosed vertical velocities are written hourly to file YAMO

C  28 Oct 2005: Jeff Young - layer dependent advection, dyn. vert. layers

C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE WVEL_DEFN             ! derived vertical velocity component
      USE SE_MODULES              ! stenex
!     USE SUBST_GLOBAL_SUM_MODULE    ! stenex

      IMPLICIT NONE

C Includes:

!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_SPC.EXT"      ! gas chemistry species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_SPC.EXT"      ! aerosol species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_SPC.EXT"      ! non-reactive species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_SPC.EXT"      ! tracer species table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_ADV.EXT"      ! gas chem advection species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_ADV.EXT"      ! aerosol advection species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_ADV.EXT"      ! non-react advection species and map table
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_ADV.EXT"      ! tracer advection species and map table
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"      ! I/O definitions and declarations
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/FDESC3.EXT"     ! file header data structure
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/FILES_CTM.EXT"    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coordinate & domain definitions (req IOPARMS)
 
C Arguments:
 
      INTEGER     JDATE         ! current model date, coded YYYYDDD
      INTEGER     JTIME         ! current model time, coded HHMMSS
      INTEGER     TSTEP( 2 )    ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
 
C External Functions not declared in IODECL3.EXT:
 
      INTEGER, EXTERNAL :: TIME2SEC, SETUP_LOGDEV
 
C Parameters:
 
C Advected species dimension
 
      INTEGER, PARAMETER :: N_SPC_ADV = N_GC_ADV
     &                                + N_AE_ADV
     &                                + N_NR_ADV
     &                                + N_TR_ADV
     &                                + 1       ! for advecting air

C File Variables:

      REAL        RHOJM( NCOLS,NROWS,NLAYS ) ! RhoJ (Kg/m**3)
      REAL        WY   ( NLAYS,NCOLS,NROWS ) ! Diagnosed vert vel ala yamo

C Local variables:
 
      CHARACTER( 16 ) :: PNAME = 'ZADVYAMO'
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
C for INTERPX
      INTEGER      GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       MTIME, MDATE
      REAL          CON1( NLAYS,N_SPC_ADV ) ! concentrations subset
      REAL          CON0( NLAYS+1 )       ! intermediate concentrations subset
      REAL          VEL ( NLAYS+1 )       ! Velocities in a N-S column
      REAL          VRJ ( NLAYS+1 )        ! Velocities in a N-S column
!     REAL, SAVE :: DS  ( NLAYS )         ! dx3 (dimensionless in sigma coord.)
!     REAL, SAVE :: DSI ( NLAYS )         ! 1/dx3
      REAL, ALLOCATABLE, SAVE :: DS ( : )  ! dx3 (dimensionless in sigma coord.)
      REAL, ALLOCATABLE, SAVE :: DSI( : )  ! 1/dx3
      REAL          DTSEC                ! model time step in seconds
 
      INTEGER, SAVE :: ADV_MAP( N_SPC_ADV ) ! global adv map to CGRID

      INTEGER      COL, ROW, LVL, SPC, VAR   ! loop counters
      INTEGER      A2C

      CHARACTER( 96 ) :: XMSG = ' '
      INTEGER, SAVE :: ASPC              ! pointer in CGRID to transported RHOJ
      REAL         RJ1   ( NLAYS )       ! local RHOJM at tstep
      REAL         RJ2   ( NLAYS )       ! local RHOJM at tstep + 1
      REAL         TRRHOJ( NLAYS )       ! local transported RHOJ
      REAL         FLUX  ( NLAYS+1 )
      REAL         DRJ   ( NLAYS )
      REAL         DFLX
      REAL         TFRC                  ! time fraction
!     INTEGER      MMIN

C for Courant stability
 
      REAL, PARAMETER :: CFL = 0.75     ! maximum allowable Courant number
      REAL        FF                    ! vertical velocity component / DS
      REAL        DELT                  ! local DTX3
      REAL        DT                    ! sub timestep
      INTEGER     IX3                   ! loop and intermediate variable
      INTEGER     ITER                  ! local NX3
      INTEGER     ALLOCSTAT
 
      INTEGER, SAVE :: LOGDEV

      real mff, mvel, mvel1
      integer mlvl

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV()

         ALLOCATE ( DS( NLAYS ),
     &              DSI( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DS, or DSI'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C Get dx3 from COORD include file

!        WRITE( LOGDEV,* ) ' S, Delta S and inverse:'
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '    layer    S (X3FACE_GD) Delta S   Inverse Delta'
         DO LVL = 1, NLAYS
            DS ( LVL ) = ABS ( X3FACE_GD( LVL ) - X3FACE_GD( LVL-1 ) )
            DSI( LVL ) = 1.0 / DS( LVL )
            WRITE( LOGDEV,'(5X, I3, 3F14.7)' ) LVL, X3FACE_GD( LVL ),
     &                                         DS( LVL ), DSI( LVL )
            END DO
         WRITE( LOGDEV,* ) ' '

C Get CGRID offsets
 
         CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

C Pointer to transported RHOJ

         ASPC = GC_STRT - 1 + N_GC_SPCD

C Create global map to CGRID
 
         SPC = 0
         DO VAR = 1, N_GC_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = GC_STRT - 1 + GC_ADV_MAP( VAR )
            END DO
         DO VAR = 1, N_AE_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = AE_STRT - 1 + AE_ADV_MAP( VAR )
            END DO
         DO VAR = 1, N_NR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = NR_STRT - 1 + NR_ADV_MAP( VAR )
            END DO
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = TR_STRT - 1 + TR_ADV_MAP( VAR )
            END DO
 
         ADV_MAP( N_SPC_ADV ) = ASPC

C get file local domain offsets

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 ) 

         END IF                    ! if firstime

C Process time step is TSTEP(2).
C Time-stepped gridded computation for Z-direction advection.

      DTSEC  = FLOAT( TIME2SEC( TSTEP( 2 ) ) ) ! process time step (seconds)

C vertical velocities are at face centers, positive upward.
C No boundary conditions are needed because VEL(1) = VEL(NLAYS+1) = 0

C Get rho*J at end of sync step

      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) ) 
      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENSA_J', PNAME,
     &           STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &           MDATE, MTIME, RHOJM ) ) THEN
         XMSG = 'Could not read DENSA_J from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      FLUX( 1 ) = 0.0
      VEL( 1 ) = 0.0
      VRJ( 1 ) = 0.0
      DO 333 ROW = 1, MY_NROWS
         DO 222 COL = 1, MY_NCOLS

            DO LVL = 1, NLAYS
               RJ1( LVL ) = CGRID( COL,ROW,LVL,ASPC )
               RJ2( LVL ) = RHOJM( COL,ROW,LVL )
               DRJ( LVL ) = RJ1( LVL ) - RJ2( LVL )
               END DO

            DO LVL = 1, NLAYS - 1

!              write( logdev,* ) '@>@ c,r,l,RJ1,RJ2: ',
!    &                            c, r, lvl, rj1(lvl), rj2(lvl)

               FLUX( LVL+1 ) = FLUX( LVL ) + DRJ( LVL ) * DS( LVL ) / DTSEC
               IF ( FLUX( LVL+1 ) .GT. 0.0 ) THEN
                  VEL( LVL+1 ) = FLUX( LVL+1 ) / RJ1( LVL )
                  VRJ( LVL+1 ) = FLUX( LVL+1 ) / RJ2( LVL )
                  ELSE
                  VEL( LVL+1 ) = FLUX( LVL+1 ) / RJ1( LVL+1 )
                  VRJ( LVL+1 ) = FLUX( LVL+1 ) / RJ2( LVL+1 )
                  END IF
               END DO
            FLUX( NLAYS+1 ) = FLUX( NLAYS )
     &                      + DRJ( NLAYS ) * DS( NLAYS ) / DTSEC
            VEL( NLAYS+1 ) = FLUX( NLAYS+1 ) / RJ1( NLAYS )
            VRJ( NLAYS+1 ) = FLUX( NLAYS+1 ) / RJ2( NLAYS )
 
            mff = 0.0

            FF = MAX (
     &               ABS( VEL( 2 ) * DSI( 1 ) ),
     &               ABS( VRJ( 2 ) * DSI( 1 ) )
     &               )
            DO LVL = 2, NLAYS
               FF = MAX ( FF,
     &                  ABS( VEL( LVL )   * DSI( LVL ) ),
     &                  ABS( VEL( LVL+1 ) * DSI( LVL ) ),
     &                  ABS( VRJ( LVL )   * DSI( LVL ) ),
     &                  ABS( VRJ( LVL+1 ) * DSI( LVL ) )
     &                  )

               if ( ff .gt. mff ) then
                  mff = ff
                  mlvl = lvl
                  mvel = vel( lvl )
                  mvel1 = vel( lvl+1 )
                  end if

               END DO

            IF ( FF .GT. 0.5 ) THEN   ! 1/2 sigma level per second
 
C unable to get vertical advection step
               WRITE( LOGDEV,* ) ' Local col and row: ', COL, ROW
               WRITE( LOGDEV,* ) ' Derived vert vel at step and end step:'
               DO LVL = 1, NLAYS + 1
                  WRITE( LOGDEV,* ) ' ', LVL, VEL( LVL ), VRJ( LVL )
                  END DO
               WRITE( LOGDEV,* ) '   Transported RHOJ   Met RHOJ'
               DO LVL = 1, NLAYS
                  WRITE( LOGDEV,* ) ' ', LVL, RJ1( LVL ), '    ', RJ2( LVL )
                  END DO
               WRITE( XMSG,94010 ) TSTEP( 2 ), FF
94010          FORMAT( 'Could not compute vert adv step for model step:',
     &                  I7.6, ' HHMMSS', 2X,
     &                 'Max vel/dS =', 1PE10.3)
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )

               END IF

            DELT = MIN( DTSEC, FLOAT( CEILING( CFL / FF ) ) )
            ITER = CEILING( DTSEC / DELT )
            DT = DTSEC / FLOAT( ITER )

            if ( iter .gt. 15 ) then
               write( logdev,2001 ) col, row, ff, dt, iter, mlvl, mvel, mvel1
               end if
2001  format( 'zadv col  row     ff', 8x, 'dt    iter  Mlvl',
     &        4x, 'Mvel      Mvel1',
     &        / 'zzzz', 2i4, 1pe12.3, 0pf8.3, 1x, 2i5, 2x, 2(1pe10.2) )

            DO SPC = 1, N_SPC_ADV
               A2C = ADV_MAP( SPC )
               DO LVL = 1, NLAYS
                  CON1( LVL,SPC ) = CGRID( COL,ROW,LVL,A2C )
                  END DO
               END DO

            if ( iter .gt. 15 ) then
               do lvl = 1, nlays
                  write( logdev,* ) 'zzz2', lvl, con1( lvl,n_spc_adv )
                  end do
               end if

            DO IX3 = 1, ITER

               DO LVL = 1, NLAYS
                  TRRHOJ( LVL ) = CON1( LVL,N_SPC_ADV )
                  END DO

               TFRC = FLOAT( IX3 ) / DTSEC

!              write( logdev,* ) ' Ix3,Col,Row,Tfrc=', ix3, col, row, tfrc
               DO LVL = 1, NLAYS
                  DFLX = TFRC * ( TRRHOJ( LVL ) - RJ2( LVL ) ) * DS( LVL )
                  FLUX( LVL+1 ) = FLUX( LVL ) + DFLX
                  END DO
               DO LVL = 1, NLAYS - 1
                  IF ( FLUX( LVL+1 ) .GT. 0.0 ) THEN
                     VEL( LVL+1 ) = FLUX( LVL+1 ) / TRRHOJ( LVL )
                     ELSE
                     VEL( LVL+1 ) = FLUX( LVL+1 ) / TRRHOJ( LVL+1 )
                     END IF
                  END DO
                  VEL( NLAYS+1 ) = FLUX( NLAYS+1 ) / TRRHOJ( NLAYS )

               DO SPC = 1, N_SPC_ADV

                  DO LVL = 1, NLAYS
                     CON0( LVL ) = CON1( LVL,SPC )
                     END DO
                  CON0( NLAYS+1 ) = CON0( NLAYS )

C First-order upstream (donor cell)

                  DO LVL = 1, NLAYS
                     IF ( VEL( LVL+1 ) .GT. 0.0 ) THEN
                        FLUX( LVL+1 ) = VEL( LVL+1 ) * CON0( LVL )
                        ELSE
                        FLUX( LVL+1 ) = VEL( LVL+1 ) * CON0( LVL+1 )
                        END IF
                     CON1( LVL,SPC ) = CON0( LVL )
     &                               + ( FLUX( LVL ) - FLUX( LVL+1 ) )
     &                               * DSI( LVL ) * DT
                     END DO

                  END DO    ! SPC

               END DO   ! IX3

            DO SPC = 1, N_SPC_ADV
               A2C = ADV_MAP( SPC )
               DO LVL = 1, NLAYS
                  CGRID( COL,ROW,LVL,A2C ) = CON1( LVL,SPC )
                  END DO
               END DO

            DO LVL = 1, NLAYS
               WY( LVL,COL,ROW ) = VEL( LVL+1 )
               END DO

222         CONTINUE   ! COL
333      CONTINUE   ! ROW

      IF ( W_VEL ) THEN
         DO LVL = 1, NLAYS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  WVEL( COL,ROW,LVL ) = WY( LVL,COL,ROW )
                  END DO
               END DO
            END DO
         END IF

      RETURN
      END
