
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
C $Header: /project/work/rep/CCTM/src/driver/ctm/wr_aconc.F,v 1.3 2002/04/05 18:23:09 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPWDEP ( JDATE, JTIME, TSTEP )

C Revision History:
C   11 Feb 03 J.Young: initial
C-----------------------------------------------------------------------
      USE HGRD_DEFN             ! horizontal domain specifications
!     USE CGRID_SPCS            ! CGRID species number and offsets
!     USE CGRID_DEFN            ! inherits HGRD_DEFN and CGRID_SPCS
      USE WDEP_DEFN

      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/IODECL3.EXT"      ! I/O definitions and declarations
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/FDESC3.EXT"     ! file header data structure
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/FILES_CTM.EXT"    ! file name parameters

      INTEGER       JDATE       ! current model date, coded YYYYDDD
      INTEGER       JTIME       ! current model time, coded HHMMSS
      INTEGER       TSTEP       ! model output time step, coded HHMMSS

!     LOGICAL           :: CLD_DIAG = .FALSE. ! use cloud diagnostic file?
      INTEGER       LOGDEV
      INTEGER, EXTERNAL :: SETUP_LOGDEV
      CHARACTER( 16 )   :: PNAME = 'Opwdep'
      CHARACTER( 96 )   :: XMSG = ' '
      INTEGER L, SPC, VAR, STRT, FINI

      LOGDEV = SETUP_LOGDEV ()

C...Create wet deposition file:
C...copy most of header description from MET_CRO_2D

      IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Error opening MET_CRO_2D'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...get description from the met file

      IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
         XMSG = 'Could not get MET_CRO_2D file description'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS

      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD

C...advance to time variable for the output file to the next hour

      SDATE3D = JDATE
      STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
      TSTEP3D = 010000   ! 1-hour timesteps (hhmmss)
      CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

      IF ( TSTEP .NE. TSTEP3D )
     &   WRITE( LOGDEV,* ) '    Cloud file output timestep: ', TSTEP3D,
     &                     ' not same as Model output timestep: ', TSTEP

      NVARS3D = N_SPC_WDEPD

C...define wet deposition output descriptors for gaseous species

      SPC = 0
      STRT = 1
      FINI = N_GC_WDEP
      DO VAR = STRT, FINI
         SPC = SPC + 1
         VTYPE3D ( VAR ) = M3REAL
         VNAME3D ( VAR ) = GC_WDEP( SPC )
         UNITS3D ( VAR ) = 'kg/hectare'
         VDESC3D ( VAR ) = 'hourly wet deposition values'
         END DO

C...define wet deposition output descriptors for aerosol species

      SPC = 0
      STRT = FINI + 1
      FINI = FINI + N_AE_WDEP
      DO VAR = STRT, FINI
         SPC = SPC + 1
         VTYPE3D ( VAR ) = M3REAL
         VNAME3D ( VAR ) = AE_WDEP( SPC )
         IF ( INDEX( AE_WDEP( SPC ), 'NUM' ) .GT. 0 ) THEN
            UNITS3D ( VAR ) = 'number/hectare'
            ELSE IF ( INDEX( AE_WDEP( SPC ), 'SRF' ) .GT. 0 ) THEN
            UNITS3D ( VAR ) = 'm2/hectare'
            ELSE
            UNITS3D ( VAR ) = 'kg/hectare'
            END IF
         VDESC3D ( VAR ) = 'hourly wet deposition values'
         END DO

C...define wet deposition output descriptors for non-reactive species

      SPC = 0
      STRT = FINI + 1
      FINI = FINI + N_NR_WDEP
      DO VAR = STRT, FINI
         SPC = SPC + 1
         VTYPE3D ( VAR ) = M3REAL
         VNAME3D ( VAR ) = NR_WDEP( SPC )
         UNITS3D ( VAR ) = 'kg/hectare'
         VDESC3D ( VAR ) = 'hourly wet deposition values'
         END DO

C...define wet deposition output descriptors for tracer species

      SPC = 0
      STRT = FINI + 1
      FINI = FINI + N_TR_WDEP
      DO VAR = STRT, FINI
         SPC = SPC + 1
         VTYPE3D ( VAR ) = M3REAL
         VNAME3D ( VAR ) = TR_WDEP( SPC )
         UNITS3D ( VAR ) = 'kg/hectare'
         VDESC3D ( VAR ) = 'hourly wet deposition values'
         END DO

C...define output descriptor for hydrogen concentration

      VAR = N_SPC_WDEP + 1
      VTYPE3D( VAR ) = M3REAL
      VNAME3D( VAR ) = 'HPLUS'
      UNITS3D( VAR ) = 'kg/hectare'
      VDESC3D( VAR ) = 'hourly wet deposition values'

C...initialize the file description

      FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition '
     &                 // 'from all cloud types'
      DO L = 2 , MXDESC3
         FDESC3D( L ) = ' '
         END DO

      GDNAM3D = GRID_NAME  ! from HGRD_DEFN

C...try to open the output file new

      IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSNEW3, PNAME ) ) THEN
         XMSG = 'Could not create '// CTM_WET_DEP_1 // ' file'
         CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

C...open the diagnostic file if user asked for it

      IF ( CLD_DIAG ) THEN

C...copy most of header description from
C...CTM_WET_DEP_1 and initialize depositions to zero.

C...get description from the other wet dep file

         IF ( .NOT. DESC3( CTM_WET_DEP_1 ) ) THEN
            XMSG = 'Could not get ' // CTM_WET_DEP_1 //' file description'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C...advance to time variable for the output file to the next hour

         SDATE3D = JDATE
         STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
         TSTEP3D = 010000   ! 1-hour timesteps (hhmmss)
         CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

         NVARS3D = N_SPC_WDEP + 6

C...define output descriptor for pressure at the lifting condensation
C...level (LCL)

         VAR = N_SPC_WDEP + 2
         VTYPE3D( VAR ) = M3REAL
         VNAME3D( VAR ) = 'PLCL'
         UNITS3D( VAR ) = 'Pa'
         VDESC3D( VAR ) = 'pressure at lifting condensation level, '
     &                  // 'or MISSING = -9999.9'

C...define output descriptor for cloud bottom layer number

         VAR = VAR + 1
         VTYPE3D( VAR ) = M3REAL
         VNAME3D( VAR ) = 'CLOUD_BOTTOM'
         UNITS3D( VAR ) = 'layer-number'
         VDESC3D( VAR ) =
     &         'layer containing bottom of cloud, or MISSING = -9999.9'
C...define output descriptor for precipitating cloud top layer number

         VAR = VAR + 1
         VTYPE3D( VAR ) = M3REAL
         VNAME3D( VAR ) = 'PR_CLOUDTOP'
         UNITS3D( VAR ) = 'layer-number'
         VDESC3D( VAR ) =
     &         'layer containing top of PR cloud, or MISSING = -9999.9'

C...define output descriptor for non-precipitating cloud top layer number

         VAR = VAR + 1
         VTYPE3D( VAR ) = M3REAL
         VNAME3D( VAR ) = 'NP_CLOUDTOP'
         UNITS3D( VAR ) = 'layer-number'
         VDESC3D( VAR ) =
     &         'layer containing top of NP cloud, or MISSING = -9999.9'

C...define output descriptor for rain event flag

         VAR = VAR + 1
         VTYPE3D( VAR ) = M3REAL
         VNAME3D( VAR ) = 'RAIN_FLAG'
         UNITS3D( VAR ) = '1 or 0'
         VDESC3D( VAR ) = 'Rain-event flag'

C...initialize the file descriptors

         FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition '
     &                // 'from sub-grid clouds and cloud data'
         DO L = 2 , MXDESC3
            FDESC3D( L ) = ' '
            END DO

C...try to open the diagnostic output file

         IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_WET_DEP_2
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF   ! CLD_DIAG

      RETURN

      END SUBROUTINE OPWDEP

