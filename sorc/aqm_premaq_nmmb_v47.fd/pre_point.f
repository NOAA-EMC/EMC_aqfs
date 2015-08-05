
      SUBROUTINE PRE_LAYPOINT (premaq_sdate, premaq_stime)

!***********************************************************************
!  program body starts at line 262
!
!  DESCRIPTION:
!     This program computes the layer fractions for point sources.  It uses
!     a modified Briggs algorithm to compute plume rise.  The plume is
!     allocated to multiple layers when necessary.
!
!  PRECONDITIONS REQUIRED:  
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION  HISTORY:
!                     Jan. 2005 David Wong
!                       -- Allocated MYLFRAC for storing LFRAC to avoid
!                          reading and writing to file PLAY
!                     Jan 24, 2005 David Wong
!                       -- Replaced various loops with F90 reshape
!                          statement
!                     Feb 14, 2005 David Wong
!                       -- Removed data structure MYLFRAC
!                     7/6/06 G. Pouliot Correct array bounds problem
!                            with TA array; require EMLAYS < NLAYS
!
!***********************************************************************
!  
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id: laypoint.f,v 1.25 2004/06/27 02:06:24 cseppan Exp $
!  
! COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
! All Rights Reserved
! 
! Carolina Environmental Program
! University of North Carolina at Chapel Hill
! 137 E. Franklin St., CB# 6116
! Chapel Hill, NC 27599-6116
! 
! smoke@unc.edu
!  
! Pathname:
!   $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/point/laypoint.f,v $
! Last updated: $Date: 2004/06/27 02:06:24 $ 
!  
!***********************************************************************

!.... MODULES for public variables

!-----------------------------------------------------------------------
!.... This module is the inventory arrays
!-----------------------------------------------------------------------

      USE MODSOURC, ONLY: XLOCA, YLOCA, STKDM, STKHT, STKTK, STKVE,
     &                    CSOURC, IFIP

!-----------------------------------------------------------------------
!.... This module contains arrays for plume-in-grid and major sources
!-----------------------------------------------------------------------

      USE MODELEV, ONLY: NHRSRC, HRSTKTK, HRSTKVE, HRSTKFL, LMAJOR,
     &                   LAY1F, PLMBOT, PLMTOP

!-----------------------------------------------------------------------
!.... This module contains the information about the source category
!-----------------------------------------------------------------------

      USE MODINFO, ONLY: CATEGORY, CRL, CATLEN, SC_BEGP, SC_ENDP,
     &                   NSRC, NCHARS

!-----------------------------------------------------------------------
!.... This module contains the global variables for the 3-d grid
!-----------------------------------------------------------------------

      USE MODGRID, ONLY: NGRID, VGTYP, VGTOP,
     &                   XORIG, YORIG, GDTYP, P_ALP, P_BET,
     &                   P_GAM, XCENT, YCENT, XCELL, YCELL, GRDNM


      use premaqparm, only : ncols, nrows, nlays
      USE MODMERGE  , ONLY : LFRAC

      USE MOD_LAYPOINT
      use coord
      use groutcom


      IMPLICIT NONE

!.... EXTERNAL FUNCTIONS and their descriptions:

      LOGICAL         CHKMETEM, DSCM3GRD, DSCM3LAY, ENVYN

      CHARACTER(2)    CRLF
      CHARACTER(10)   HHMMSS
      CHARACTER(14)   MMDDYY
      CHARACTER(16)   PROMPTMFILE
      CHARACTER(50)   GETCFDSC

      INTEGER         ENVINT, FIND1, INDEX1, PROMPTFFILE, WKDAY

      EXTERNAL   CHKMETEM, CRLF, DSCM3GRD, DSCM3LAY, ENVINT, ENVYN, 
     &           FIND1, GETCFDSC, HHMMSS, INDEX1, MMDDYY, 
     &           PROMPTFFILE, PROMPTMFILE, VERCHAR, WKDAY

!.... Claim varilables

      CHARACTER(16) :: PROGNAME = 'PRE_LAYPOINT'   !  program name

      integer premaq_sdate, premaq_stime, i_loop, j_loop, nrowsncols


!***********************************************************************
!   begin body of program PRE_LAYPOINT
!***********************************************************************

      CALL M3MSG2('BEGIN -> of PRE_LAYPOINT')

      BFLAG   = .FALSE.  ! true: use plume bottom and top
      CFLAG   = .FALSE.  ! true: recalc vel w/ flow & diam
      COMPUTE = .FALSE.  ! true: compute plume rise 
      EFLAG   = .FALSE.  ! error flag
      FFLAG   = .FALSE.  ! true: use hourly flow rate
      HFLAG   = .FALSE.  ! true: hourly input used
      IFLAG   = .FALSE.  ! true: hr data okay for timestep
      LFLAG   = .FALSE.  ! true: use hourly layer 1 fraction
      PFLAG   = .FALSE.  ! true: compute plm ris for iteration
      TFLAG   = .FALSE.  ! true: use hourly temperatures
      VFLAG   = .FALSE.  ! true: use elevated file (PELV)
      XFLAG   = .FALSE.  ! true: process ONLY explicit sources
      YFLAG   = .FALSE.  ! true: use hourly velocities
      ZSTATIC = .TRUE.   ! true: Get heights from GRID_CRO file

!	PDEV = 0
!	RDEV = 0

      NSTEPS = 1
      SDATE  = 0
      STIME  = 0
	
      LDEV = INIT3()

!-----------------------------------------------------------------------
!.... Write out copywrite, version, web address, header info, and prompt
!     to continue running the program.
!-----------------------------------------------------------------------

!AQF        CALL INITEM( LDEV, CVSW, PROGNAME )

!-----------------------------------------------------------------------
!.... Get setting from environment variables
!-----------------------------------------------------------------------

      EMLAYS = ENVINT('SMK_EMLAYS', 'Number of emission layers',-1, IOS)

      MESG = 'Use Elevpoint output to determine elevated sources'
      VFLAG = ENVYN( 'SMK_SPECELEV_YN', MESG, .FALSE., IOS )

      MESG = 'Indicator for defining hourly plume rise data'
      HFLAG = ENVYN( 'HOUR_PLUMEDATA_YN', MESG, .FALSE., IOS )

      MESG = 'Indicator for processing ONLY explicit plume rise sources'
      XFLAG = ENVYN( 'EXPLICIT_PLUMES_YN', MESG, .FALSE., IOS )


! new plumerise code (02/05)

      IPVERT = ENVINT( 'IPVERT', 'Method for vertical spread',0, IOS )

!-----------------------------------------------------------------------
!.... Must have HOUR_PLUMEDATA_YN = Y to have EXPLICIT_PLUMES_YN = Y
!-----------------------------------------------------------------------

      IF ( XFLAG .AND. .NOT. HFLAG ) THEN
         HFLAG = .TRUE.
         MESG = 'NOTE: Setting HOUR_PLUMEDATA_YN to Y because '//
     &          'EXPLICIT_PLUMES_YN is Y'
         CALL M3MSG2( MESG )
      ENDIF

      CFLAG = ENVYN( 'VELOC_RECALC', 
     &               'Flag for recalculating velocity', .FALSE., IOS )

!-----------------------------------------------------------------------
!.... Cannot use default and cannot set to less than 4 because of
!     limits of plume rise algorithm
!-----------------------------------------------------------------------

      IF ( EMLAYS .LT. 4 ) THEN
         MESG = 'Environment variable SMK_EMLAYS must be set to ' //
     &          'a number from 4 to the ' // CRLF() // BLANK10 //
     &          'number of layers in the meteorology inputs.'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
	
      IF ( EMLAYS .GT. NLAYS-1 ) THEN
         MESG = 'Environment variable SMK_EMLAYS must be set to ' //
     &          'a number no larger than 1 less' //
     &          'than the ' // CRLF() // BLANK10 //
     &          'number of layers in the meteorology inputs.'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF	
	
            
      REP_LAYR = ENVINT( 'REP_LAYER_MAX', 
     &                   'Layer number for reporting high plume rise',
     &                   -1, IOS )

      IF ( IOS .EQ. 0 ) THEN
         IF ( REP_LAYR .LT. 1 ) THEN
            MESG = 'NOTE: Environment variable REP_LAYR_MAX is ' //
     &             'less than 1.  Turning off reporting...'
 
         ELSEIF ( REP_LAYR .GT. EMLAYS ) THEN
            WRITE( MESG,94010 ) 
     &             'NOTE: Environment variable REP_LAYR_MAX is '//
     &             'greater than the number of emissions ' // 
     &             CRLF() //BLANK10 // 'layers (', EMLAYS, '). '//
     &             'Resetting to equal number of emissions layers.'
         ENDIF

         CALL M3MSG2( MESG )
      ENDIF

!-----------------------------------------------------------------------
!.... Set source category based on environment variable setting
!-----------------------------------------------------------------------

      CALL GETCTGRY

!-----------------------------------------------------------------------
!.... Get inventory file names given source category
!-----------------------------------------------------------------------

      CALL GETINAME( CATEGORY, ENAME, ANAME )

!-----------------------------------------------------------------------
!.... Make sure only run for point sources
!-----------------------------------------------------------------------

      IF ( CATEGORY .NE. 'POINT' ) THEN
         MESG = 'ERROR: ' // PROGNAME( 1:LEN_TRIM( PROGNAME ) ) //
     &          ' is not valid for ' // CATEGORY( 1:CATLEN ) // 
     &          ' sources'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

!-----------------------------------------------------------------------
!.... Prompt for and open inventory file
!-----------------------------------------------------------------------

      INAME = ENAME
      MESG = 'Enter logical name for the MAP INVENTORY file'
!      IDEV = PROMPTFFILE( MESG, .TRUE., .TRUE., INAME, PROGNAME )


!AQF this has already been done in pre_temporal
!.... Open and read map file
!AQF     CALL RDINVMAP( INAME, IDEV, ENAME, ANAME, SDEV )

        
!-----------------------------------------------------------------------
!.... Store source-category-specific header information, 
!     including the inventory pollutants in the file (if any).  Note that 
!     the I/O API header info is passed by include file and the
!     results are stored in module MODINFO.
!-----------------------------------------------------------------------

      CALL GETSINFO( ENAME )

      IFDESC2 = GETCFDSC( FDESC3D, '/FROM/', .TRUE. )
      IFDESC3 = GETCFDSC( FDESC3D, '/VERSION/', .TRUE. )


!-----------------------------------------------------------------------
!.... Get file name and open daily input inventory file
!-----------------------------------------------------------------------

      IF ( HFLAG ) THEN
         HNAME = PROMPTMFILE (
     &                      'Enter logical name for HOUR-SPECIFIC file',
     &                       FSREAD3, CRL // 'HOUR', PROGNAME )

       !----------------------------------------------------
       !... Check to see if appropriate variable list exists
       !----------------------------------------------------

         CALL RETRIEVE_IOAPI_HEADER( HNAME )

         NHRSRC = NROWS3D

       !----------------------------------------------------
       !... Check input variables and allocate memory...
       !... Check for layer-1 fraction
       !----------------------------------------------------

         I = INDEX1 ( SPDATNAM( 1 ), NVARS3D, VNAME3D )

         IF ( I .GT. 0 ) THEN
            LFLAG = .TRUE.
            WRITE( MESG,94010 ) 'NOTE: Layer-1 fraction ' //
     &             'hourly input will be used '//CRLF()// BLANK10//
     &             'to allocate plumes for some sources.'
            CALL M3MSG2( MESG )

            ALLOCATE( LAY1F( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'LAY1F', PROGNAME )
         ENDIF

       !----------------------------------------------------
       !... Check for plume top and plume bottom
       !----------------------------------------------------

         J = INDEX1( SPDATNAM( 2 ), NVARS3D, VNAME3D )
         K = INDEX1( SPDATNAM( 3 ), NVARS3D, VNAME3D )

         IF ( J .GT. 0 .AND. K .LE. 0 ) THEN
            MESG = 'WARNING: Plume bottom in hourly input file '//
     &             'will not be used '// CRLF()// BLANK10//
     &             'because plume top is not also present.'
            CALL M3MSG2( MESG )

         ELSEIF ( J .LE. 0 .AND. K .GT. 0 ) THEN
            MESG = 'WARNING: Plume top in hourly input file '//
     &             'will not be used '// CRLF()// BLANK10//
     &             'because plume bottom is not also present.'
            CALL M3MSG2( MESG )

         ELSEIF ( J .GT. 0 .AND. K .GT. 0 ) THEN
            BFLAG = .TRUE.
            WRITE( MESG,94010 ) 'NOTE: Plume top and bottom in ' //
     &             'hourly input will be used '//CRLF()// BLANK10//
     &             'to allocate plumes for some sources.'
            CALL M3MSG2( MESG )

            ALLOCATE( PLMBOT( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'PLMBOT', PROGNAME )

            ALLOCATE( PLMTOP( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'PTOP', PROGNAME )
         ENDIF

       !----------------------------------------------------
       !..  Check for temperatures
       !----------------------------------------------------

         I = INDEX1( SPDATNAM( 4 ), NVARS3D, VNAME3D )

         IF ( I .GT. 0 ) THEN
            TFLAG = .TRUE.
            WRITE( MESG,94010 ) 'NOTE: Temperatures ' //
     &             'hourly input will be used '//CRLF()// BLANK10//
     &             'to allocate plumes for some sources.'
            CALL M3MSG2( MESG )

            ALLOCATE( HRSTKTK( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'HRSTKTK', PROGNAME )
         ENDIF

       !----------------------------------------------------
       !... Check for velocity
       !----------------------------------------------------

         I = INDEX1 ( SPDATNAM( 5 ), NVARS3D, VNAME3D )
         IF ( I .GT. 0 ) THEN
            YFLAG = .TRUE.
            WRITE( MESG,94010 ) 'NOTE: Velocities ' //
     &             'hourly input will be used '//CRLF()// BLANK10//
     &             'to allocate plumes for some sources.'
            CALL M3MSG2( MESG )

            ALLOCATE( HRSTKVE( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'HRSTKVE', PROGNAME )
         ENDIF

       !----------------------------------------------------
       !... Check for flow rate
       !----------------------------------------------------

         I = INDEX1 ( SPDATNAM( 6 ), NVARS3D, VNAME3D )
         IF ( I .GT. 0 ) THEN
            FFLAG = .TRUE.
            WRITE( MESG,94010 ) 'NOTE: Flow rate ' //
     &             'hourly input will be used '//CRLF()// BLANK10//
     &             'to allocate plumes for some sources.'
            CALL M3MSG2( MESG )

            ALLOCATE( HRSTKFL( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'HRSTKFL', PROGNAME )
         ENDIF

       !----------------------------------------------------
       !... If no correct variables, then ignore file
       !.. .Give warning if no valid data
       !----------------------------------------------------

         HFLAG= ( LFLAG .OR. BFLAG .OR. TFLAG .OR. YFLAG .OR. FFLAG )

         IF ( .NOT. HFLAG ) THEN
            MESG = 'WARNING: No hourly data used because ' //
     &             'no correct variables names ' // CRLF() // 
     &             BLANK10 // '(defined in EMCNST3.EXT) were found.'
            CALL M3MSG2( MESG )
         ENDIF
      ENDIF      ! End if hourly data use was requested by E.V. settings

!##################################################
!------------ END OF HFLAG LOGICAL ----------------
!##################################################


      IF ( VFLAG ) THEN
         PDEV = PROMPTFFILE( 
     &          'Enter logical name for the ELEVATED POINT SOURCE file',
     &          .TRUE., .TRUE., CRL // 'ELV', PROGNAME )
      ENDIF

!-----------------------------------------------------------------------
!.... If not explicit plume rise only, open and process other met files
!-----------------------------------------------------------------------

      IF ( .NOT. XFLAG ) THEN

!AQF     SNAME = PROMPTMFILE( 
!AQF    &               'Enter name for CROSS-POINT SURFACE MET file',
!AQF    &                FSREAD3, 'MET_CRO_2D', PROGNAME )

!AQF     GNAME = PROMPTMFILE( 
!AQF    &               'Enter name for CROSS-POINT LAYERED GRID file',
!AQF    &                FSREAD3, 'GRID_CRO_3D', PROGNAME )

!AQF     XNAME = PROMPTMFILE( 
!AQF    &               'Enter name for CROSS-POINT LAYERED MET file',
!AQF    &                FSREAD3, 'MET_CRO_3D', PROGNAME )

!AQF     DNAME = PROMPTMFILE( 
!AQF    &               'Enter name for DOT-POINT LAYERED MET file',
!AQF    &                FSREAD3, 'MET_DOT_3D', PROGNAME )

       !----------------------------------------------------
       !... Check multiple met files for consistency
       !----------------------------------------------------

!AQF     EFLAG = ( .NOT. CHKMETEM( 'NONE',SNAME,GNAME,XNAME,DNAME ) )

!AQF     IF ( EFLAG ) THEN
!AQF        MESG = 'Input met files have inconsistent grids or ' //
!AQF    &          'layers.'
!AQF        CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
!AQF     ENDIF

       !----------------------------------------------------
       !... Get grid parameters from 3-d cross-point met file and store 
       !        needed header information.  Use time parameters for time 
       !        defaults.
       !----------------------------------------------------

!AQF     CALL RETRIEVE_IOAPI_HEADER( XNAME )

       !-- in place of ioapi header --

         P_ALP3D = p_alp_gd           ! coord_mod
         P_BET3D = p_bet_gd           ! coord_mod
         P_GAM3D = p_gam_gd           ! coord_mod
         GDTYP3D = gdtyp_gd           ! coord_mod
         XCENT3D = xcent_gd           ! coord_mod
         YCENT3D = ycent_gd           ! coord_mod
         XORIG3D = xorig_gd           ! coord_mod
         VGTYP3D = vgtyp_gd           ! coord_mod

         GDNAM3D = gdname_gd

         YORIG3D = yorig_gd
         XCELL3D = xcell_gd
         YCELL3D = ycell_gd

         NCOLS3D = ncols
         NROWS3D = nrows

         SDATE3D = premaq_sdate
         STIME3D = premaq_stime
         VGTOP3D = vgtop_gd

         VGLVS3D ( 1 : nlays+1 ) = vglvs_gd ( 1 : nlays+1 )
           
       !-- end in place of ioapi header --


       !----------------------------------------------------
       !... Initialize reference grid with met file
       !----------------------------------------------------

!AQF     CALL CHKGRID( XNAME, 'GRID', 0, EFLAG )

       !-- from chkgrid --

         GRDNM = GDNAM3D
         GDTYP = GDTYP3D
         P_ALP = P_ALP3D
         P_BET = P_BET3D
         P_GAM = P_GAM3D

         XCENT = XCENT3D
         YCENT = YCENT3D
         XORIG = XORIG3D
         YORIG = YORIG3D
         XCELL = XCELL3D
         YCELL = YCELL3D

         NCOLS = NCOLS3D
         NROWS = NROWS3D

         NGRID = NCOLS * NROWS

       !-- end from chkgrid --


         SDATE  = SDATE3D
         STIME  = STIME3D
!AQF         NSTEPS = MXREC3D
         VGTYP  = VGTYP3D
         VGTOP  = VGTOP3D
         METNCOLS = NCOLS
         METNROWS = NROWS
         METNGRID = NGRID
         METXORIG = XORIG
         METYORIG = YORIG

         NDOTS = ( NCOLS + 1 ) * ( NROWS + 1 )

!AQF         METSCEN  = GETCFDSC( FDESC3D, '/MET SCENARIO/', .FALSE. ) 
!AQF         CLOUDSHM = GETCFDSC( FDESC3D, '/CLOUD SCHEME/', .FALSE. ) 

       !----------------------------------------------------
       !... Determine whether height information is time dependent or time
       !    independent. Non-hydrostatic is time-independent and hydrostatic
       !    is time-dependent.
       !----------------------------------------------------

         SELECT CASE( VGTYP )

            CASE ( VGSGPH3, VGHVAL3 ) 
            ZSTATIC = .FALSE.

            CASE ( VGSGPN3 )
            ZSTATIC = .TRUE.

            CASE DEFAULT
            WRITE ( MESG,94010 ) 'Cannot process vertical ' //
     &                           'coordinate type', VGTYP
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

         END SELECT

       !---- CALL RETRIEVE_IOAPI_HEADER( DNAME )

         XORIG3D = xorig_gd - 0.5*xcell_gd  ! from metdot.F
         YORIG3D = yorig_gd - 0.5*ycell_gd
         XCELL3D = xcell_gd
         YCELL3D = ycell_gd  
         VGLVS3D ( 1 : nlays+1 ) = vglvs_gd ( 1 : nlays+1 )
	    
         XCELLDG = XCELL3D
         YCELLDG = YCELL3D 
         XORIGDG = XORIG3D
         YORIGDG = YORIG3D

!-----------------------------------------------------------------------
!.... If not using met data, (for explicit plume rise only)
!-----------------------------------------------------------------------

      ELSE

       !----------------------------------------------------
       !... Get vertical layer structure from the G_GRIDPATH file
       !----------------------------------------------------

         IF ( .NOT. DSCM3LAY( NLAYS, VGTYP, VGTOP, VGLVS3D ) ) THEN
            MESG = 'Could not get vertical layer structure from '//
     &             'Models-3 grid description file.'
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
         ENDIF

       !----------------------------------------------------
       !... Check to make sure input vertical structure has been provided
       !         that is "meters above ground."
       !----------------------------------------------------

         IF ( VGTYP .NE. VGHVAL3 ) THEN
            WRITE( MESG,94010 ) 'Explicit plume rise requires ' //
     &                          'vertical type ', VGHVAL3, 'in grid ' //
     &                          'description' // CRLF() // BLANK10 //
     &                          'file, but type', VGTYP, 'was found.'
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
         ENDIF

      ENDIF      ! If using met data or not (not only for explicit plumes)

!##################################################
!------------ END OF XFLAG LOGICAL ----------------
!##################################################



!-----------------------------------------------------------------------
!.... Get horizontal grid structure from the G_GRIDPATH file
!-----------------------------------------------------------------------

!AQF      IF ( .NOT. DSCM3GRD( GDNAM3D, GDESC, COORD, GDTYP3D, COORUN3D,
!AQF     &                     P_ALP3D, P_BET3D, P_GAM3D, XCENT3D,
!AQF     &                     YCENT3D, XORIG3D, YORIG3D, XCELL3D,
!AQF     &                     YCELL3D, NCOLS3D, NROWS3D, NTHIK3D)) THEN
!AQF         MESG = 'Could not get Models-3 grid description.'
!AQF         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
!AQF      ENDIF

!-----------------------------------------------------------------------
!.... Set subgrid if using met files, define grid if not using met files
!-----------------------------------------------------------------------

!AQF      CALL CHKGRID( 'GRIDDESC', 'GRIDDESC' , 1, EFLAG ) 

!-----------------------------------------------------------------------
!.... Store local layer information
!-----------------------------------------------------------------------

      J = LBOUND( VGLVS3D, 1 )
      VGLVSXG( 0 ) = VGLVS3D( J )

      DO I = 1, NLAYS
         J = J + 1
         VGLVSXG( I   ) = VGLVS3D( J )
      ENDDO

!-----------------------------------------------------------------------
!.... Compare number of meteorology layers to number of emissions layers
!-----------------------------------------------------------------------

      IF ( EMLAYS .LE. NLAYS ) THEN
         WRITE( MESG,94010 ) 'NOTE: The number of emission layers '//
     &                     'is', EMLAYS, ', and the maximum '// CRLF()//
     &                        BLANK10//'possible layers is', NLAYS
         CALL M3MSG2( MESG )

      ELSE
         WRITE( MESG,94010 ) 'Resetting number of emission layers '//
     &             'from', EMLAYS, 'to number of '// CRLF()// BLANK10 //
     &             'layers in the meteorology file,', NLAYS
         CALL M3WARN( PROGNAME, 0, 0, MESG )

         EMLAYS = NLAYS
      ENDIF

!-----------------------------------------------------------------------
!.... Abort if error found analyzing inputs
!-----------------------------------------------------------------------

      IF ( EFLAG ) THEN
         MESG = 'Problem with inputs.'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

!-----------------------------------------------------------------------
!.... Update start date/time and duration from the environment
!-----------------------------------------------------------------------

!AQF     CALL GETM3EPI( -1, SDATE, STIME, TSTEP, NSTEPS )

      SDATE = premaq_sdate
      STIME = premaq_stime

      TSTEP = 10000   ! Only 1-hour time step supported

!-----------------------------------------------------------------------
!.... Set up and open output file, which will primarily using I/O API 
!     settings from the cross-point met file (XNAME), which are 
!     already retrieved
!-----------------------------------------------------------------------

      CALL OPENLAYOUT ( SDATE, STIME, TSTEP, EMLAYS, REP_LAYR, XFLAG, 
     &                  IFDESC2, IFDESC3, METSCEN, CLOUDSHM, VGLVSXG, 
     &                  LNAME, RDEV )

!-----------------------------------------------------------------------
!.... Allocate memory for and read required inventory characteristics
!-----------------------------------------------------------------------

      CALL RDINVCHR( 'POINT', ENAME, SDEV, NSRC, NINVARR, IVARNAMS )

!-----------------------------------------------------------------------
!.... Call subroutine to convert grid coordinates from lat-lon to
!     coordinate system of the destination grid
!-----------------------------------------------------------------------

      CALL CONVRTXY ( NSRC, GDTYP, GRDNM, P_ALP, P_BET, P_GAM, 
     &                XCENT, YCENT, XLOCA, YLOCA )

!-----------------------------------------------------------------------
!.... Call elevated sources indicator file, even thought it might not
!     be opened - routine will initialize LMAJOR and LPING regardless
!     of whether the file is available.
!-----------------------------------------------------------------------

      CALL RDPELV( PDEV, NSRC, .FALSE., NMAJOR, NPING )

!-----------------------------------------------------------------------
!.... If explicit plume rise, only explicit plume sources will be
!     output, but LMAJOR needs to be true for error checking. So, set it
!-----------------------------------------------------------------------

      IF( XFLAG ) LMAJOR = .TRUE.

!-----------------------------------------------------------------------
!.... Allocate memory for all remaining variables using dimensions 
!     obtained previously...
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!.... Allocate per-source arrays
!-----------------------------------------------------------------------

      ALLOCATE( HFX( NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'HFX', PROGNAME )

      ALLOCATE( HMIX( NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'HMIX', PROGNAME )

      ALLOCATE( TSFC( NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'TSFC', PROGNAME )

      ALLOCATE( USTAR( NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'USTAR', PROGNAME )

      ALLOCATE( PRSFC( NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'PRSFC', PROGNAME )

!-----------------------------------------------------------------------
!.... Allocate per-source and per-layer arrays
!-----------------------------------------------------------------------

      ALLOCATE( DDZH( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'DDZH', PROGNAME )

      ALLOCATE( DDZF( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'DDZF', PROGNAME )

      ALLOCATE( PRES( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'PRES', PROGNAME )

      ALLOCATE( PREF( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'PREF', PROGNAME )

      ALLOCATE( DENS( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'DENS', PROGNAME )	

      ALLOCATE( QV( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'QV', PROGNAME )

      ALLOCATE( TA( EMLAYS+1,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'TA', PROGNAME )

      ALLOCATE( UWIND( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'UWIND', PROGNAME )

      ALLOCATE( VWIND( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'VWIND', PROGNAME )

      ALLOCATE( ZF( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ZF', PROGNAME )

      ALLOCATE( ZH( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ZH', PROGNAME )

      ALLOCATE( ZSTK( EMLAYS,NSRC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ZSTK', PROGNAME )

!-----------------------------------------------------------------------
!.... If hourly data input, allocate index array
!-----------------------------------------------------------------------

      IF ( HFLAG ) THEN
         ALLOCATE( LOCINDXH( NHRSRC,EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'LOCINDXH', PROGNAME )
         LOCINDXH = 0                                     ! array
      ENDIF

!-----------------------------------------------------------------------
!.... Allocate layer fractions array: by source if not explicit, by
!     hour-specific source if it is explicit
!-----------------------------------------------------------------------

      IF ( XFLAG ) THEN
         ALLOCATE( LFRAC( NHRSRC,EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'LFRAC', PROGNAME )
         ALLOCATE( TFRAC( EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PROGNAME )

   !----------------------------------------------------
   !... If computing plume rise...
   !----------------------------------------------------

      ELSE

       !----------------------------------------------------
       !... Layer fractions for all sources
       !----------------------------------------------------

         ALLOCATE( LFRAC( NSRC,EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'LFRAC', PROGNAME )

       !----------------------------------------------------
       !... Allocate ungridding arrays
       !----------------------------------------------------

         ALLOCATE( ND( 4,NSRC ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ND', PROGNAME )

         ALLOCATE( NX( 4,NSRC ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NX', PROGNAME )

         ALLOCATE( CD( 4,NSRC ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CD', PROGNAME )

         ALLOCATE( CX( 4,NSRC ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CX', PROGNAME )

       !----------------------------------------------------
       !... Allocate per-layer arrays from 1:EMLAYS
       !----------------------------------------------------

         ALLOCATE( WSPD( EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'WSPD', PROGNAME )

         ALLOCATE( DTHDZ( EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'DTHDZ', PROGNAME )

         ALLOCATE( TFRAC( EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PROGNAME )

       !----------------------------------------------------
       !... Allocate per-layer arrays from 0:EMLAYS
       !----------------------------------------------------

         ALLOCATE( PRESF( 0:EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PRESF', PROGNAME )

         ALLOCATE( ZZF( 0:EMLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ZZF', PROGNAME )

       !----------------------------------------------------
       !... Allocate array for tmp gridded, layered cross-point met data
       !----------------------------------------------------

         ALLOCATE( XBUF( METNGRID,NLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'XBUF', PROGNAME )

         ALLOCATE( DBUF( NDOTS,NLAYS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'DBUF', PROGNAME )

       !----------------------------------------------------
       !... Compute un-gridding matrices for dot and cross point met data
       !----------------------------------------------------

         CALL UNGRIDB( METNCOLS+1, METNROWS+1, XORIGDG , YORIGDG,
     &                 XCELLDG, YCELLDG, NSRC, XLOCA, YLOCA, ND, CD )

         CALL UNGRIDB( METNCOLS  , METNROWS  , METXORIG, METYORIG, 
     &                 XCELL  , YCELL  , NSRC, XLOCA, YLOCA, NX, CX )

       !----------------------------------------------------
       !... Read time-independent ZF and ZH for non-hydrostatic Met data
       !... Compute per-source heights
       !----------------------------------------------------

         IF ( ZSTATIC ) THEN

!AQF           CALL RETRIEVE_IOAPI_HEADER( GNAME )
!AQF           CALL GET_VARIABLE_NAME( 'ZH', VNAME )
		
!            do j_loop = 1, nrows
!               do i_loop = 1, ncols
!                  XBUF((ncols*(j_loop-1)+i_loop),1:nlays)
!    &                  = gx3htm_c(i_loop, j_loop,1:nlays)
!               enddo
!            enddo

            XBUF = reshape (gx3htm_c, (/nrowsncols, nlays/))
		
!AQF           CALL SAFE_READ3( GNAME, VNAME, ALLAYS3, SDATE3D, 
!AQF     &                      STIME3D, XBUF )


            CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, ZH )

!AQF           CALL GET_VARIABLE_NAME( 'ZF', VNAME )
!AQF           CALL SAFE_READ3( GNAME, VNAME, ALLAYS3, SDATE3D,
!AQF     &                      STIME3D, XBUF )

!            do j_loop = 1, nrows
!               do i_loop = 1, ncols
!                  XBUF((ncols*(j_loop-1)+i_loop),1:nlays)
!    &                  = gx3htf_c(i_loop, j_loop,1:nlays)
!               enddo
!            enddo

            XBUF = reshape (gx3htf_c, (/nrowsncols, nlays/))

            CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, ZF )

          !----------------------------------------------------
          !... Pre-process ZF and ZH to compute DDZH and DDZF
          !----------------------------------------------------

            CALL COMPUTE_DELTA_ZS

         ENDIF
      ENDIF     ! if explicit plume rise or not

!##################################################
!------------ END OF XFLAG ----------------
!##################################################


!-----------------------------------------------------------------------
!.... Write out header to report, if any. This includes generating
!     format statement for the
!-----------------------------------------------------------------------

      IF ( REP_LAYR .GT. 0 ) THEN
         MESG = 'Cy/St/Co, Plant'
         DO I = 1, NCHARS - 2
            L = LEN_TRIM( MESG )
            WRITE( MESG,'(A,I1,A)' ) MESG( 1:L ) // ', Char', I
         ENDDO

         L = LEN_TRIM( MESG )

         WRITE( RDEV,93040 ) REP_LAYR, MESG( 1:L )
      ENDIF

!-----------------------------------------------------------------------
!.... Set logical array for setting valid source characeristics columns
!-----------------------------------------------------------------------

      LFG( 1:NCHARS ) = .TRUE.   ! array

      IF ( NCHARS .LE. 8 ) LFG( NCHARS+1:9 ) = .FALSE.  ! array

!-----------------------------------------------------------------------
!.... Get variable names from surface meteorology file
!-----------------------------------------------------------------------

!AQF     IF ( .NOT. XFLAG ) CALL RETRIEVE_IOAPI_HEADER( SNAME )

!-----------------------------------------------------------------------
!.... For each time step, compute the layer fractions...
!-----------------------------------------------------------------------

      MESG = 'Calculating hourly layer fractions...'
      CALL M3MSG2( MESG )

      XBEG  = XORIG
      YBEG  = YORIG

      XEND  = XORIG + NCOLS * XCELL
      YEND  = YORIG + NROWS * YCELL

      LDATE = 0
      JDATE = SDATE
      JTIME = STIME

      call m3msg2('NOTE: Normal Completion of PRE_LAYPOINT')

!-----------------------------------------------------------------------
!.... Exit program with normal completion
!-----------------------------------------------------------------------

!      CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )


!***********************************************************************
!******************  FORMAT  STATEMENTS   ******************************
!***********************************************************************

!------------------------------------------------
!....  Formatted file I/O formats ...... 93xxx
!------------------------------------------------

93000 FORMAT( A )

93020 FORMAT( 8X, 'at time ', A8 )

93040 FORMAT( 'Sources with top of plume greater than layer', I3, //,
     &        'Src ID, ', A, ', H[m], ', 'V[m/s], ', 'Ts[K], ', 
     &        'Ta[K], ', 'U[m/s], ', 'LPBL, ', 'LTOP' )

93042 FORMAT( '( I6, ",", I6.6, ",", A', I2.2, ', ","', I2.2, '(A', 
     &        I2.2, ',", ") , F6.1, ", ", F6.2, ", ", F6.1, ", ",', 
     &        'F5.1, ", ", F6.2, ", ", I3, ", ", I3 )' )

!------------------------------------------------
!.... Internal buffering formats ...... 94xxx
!------------------------------------------------

94010   FORMAT( 12( A, :, I8, :, 1X ) )




!***********************************************************************
!******************  INTERNAL SUBPROGRAMS  *****************************
!***********************************************************************

      CONTAINS

!######################################################################
!.... This internal subprogram tries to retrieve the I/O API header
!     and aborts if it was not successful
!######################################################################

      SUBROUTINE RETRIEVE_IOAPI_HEADER( FILNAM )

!.... arguments

      CHARACTER(*) FILNAM

!----------------------------------------------------------------------

      IF ( .NOT. DESC3( FILNAM ) ) THEN

         MESG = 'Could not get description of file "' //
     &          FILNAM( 1:LEN_TRIM( FILNAM ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

      ENDIF

      END SUBROUTINE RETRIEVE_IOAPI_HEADER



!######################################################################
!.... This internal subprogram tries to retrieve the I/O API header
!     and aborts if it was not successful
!######################################################################

      SUBROUTINE RETRIEVE_SET_HEADER( FILNAM )

      INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions

!.... arguments

      CHARACTER(*) FILNAM

C----------------------------------------------------------------------

      IF ( .NOT. DESCSET( FILNAM,-1 ) ) THEN

         MESG = 'Could not get description of file "' //
     &          FILNAM( 1:LEN_TRIM( FILNAM ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

      ENDIF

      END SUBROUTINE RETRIEVE_SET_HEADER



!######################################################################
!.... This internal subprogram resolves the differences in 
!     variable names for different version of the Met files
!######################################################################

      SUBROUTINE GET_VARIABLE_NAME( INNAME, OUTNAME )

!.... arguments

      CHARACTER(*), INTENT (IN) :: INNAME    ! variable name to check
      CHARACTER(*), INTENT(OUT) :: OUTNAME   ! variable name to read

!.... Local variables

      INTEGER J

!----------------------------------------------------------------------

!.... Search for variable name in the list of names

      J = INDEX1( INNAME, NVARS3D, VNAME3D )

!.... If the input name is there, then set output name and return

      IF ( J .GT. 0 ) THEN
         OUTNAME = INNAME
         RETURN
      ENDIF

!.... Set output name
!.... Currently there is only one alternative for each

      SELECT CASE( INNAME )
         CASE( 'ZH' )
           OUTNAME = 'X3HT0M'
         CASE( 'ZF' )
           OUTNAME = 'X3HT0F'
         CASE( 'TGD' ) 
           OUTNAME = 'TEMP10'
         CASE DEFAULT
         MESG = 'INTERNAL ERROR: Do not have an alternative ' //
     &          'name for met variable ' // INNAME
         CALL M3MSG2( MESG )
         CALL M3EXIT( PROGNAME, 0, 0, ' ', 2 )
      END SELECT

      RETURN

      END SUBROUTINE GET_VARIABLE_NAME



!######################################################################
!.... This internal subprogram tries to read a variable from an
!               I/O API file, and aborts if not successful.
!######################################################################

      SUBROUTINE SAFE_READ3 (FILNAM, VARNAM, LAYER, JDATE, JTIME, XBUF)

!.... arguments

      CHARACTER(*) FILNAM    ! logical file name
      CHARACTER(*) VARNAM    ! variable name
      INTEGER      LAYER     ! layer number (or ALLAYS3)
      INTEGER      JDATE     ! Julian date
      INTEGER      JTIME     ! time
      REAL         XBUF( * ) ! read buffer

!----------------------------------------------------------------------

      IF ( .NOT. READ3( FILNAM, VARNAM, LAYER,
     &                  JDATE, JTIME, XBUF ) ) THEN

         L1 = LEN_TRIM( VARNAM )
         L2 = LEN_TRIM( FILNAM )
         MESG = 'Could not read "' // VARNAM( 1:L1 ) // 
     &          '" from file "' // FILNAM( 1:L2 ) // '."'
         CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

      ENDIF

      END SUBROUTINE SAFE_READ3



!######################################################################
!.... This internal subprogram computes DDZH and DDZF
!######################################################################

      SUBROUTINE COMPUTE_DELTA_ZS

      DO S = 1, NSRC

         ZZ0 = ZF( 1,S )
         ZSTK ( 1,S ) = ZZ0 - STKHT( S )
         ZF0 = ZF( 1,S )
         DDZF( 1,S ) = 1.0 / ZF0

         DO L = 2, EMLAYS
            ZZ1 = ZF( L,S )
            ZSTK( L  ,S ) = ZZ1 - STKHT( S )
            DDZH( L-1,S ) = 1.0 / ( ZZ1 - ZZ0 )
            ZZ0 = ZZ1
            ZF1 = ZF( L,S )
            DDZF( L,S ) = 1.0 / ( ZF1 - ZF0 )
            ZF0 = ZF1
         ENDDO

      ENDDO  ! End processing for intermediate layer height calcs

      END SUBROUTINE COMPUTE_DELTA_ZS

      END SUBROUTINE PRE_LAYPOINT





