
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/CCTM/src/vdiff/acm2_inline/tmpbeis.F,v 1.5 2009/10/05 17:24:48 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TMPBEIS312( JDATE, JTIME, TSTEP, N_SPC_BIOG, BEIS_MAP, EMISL )

C-----------------------------------------------------------------------
 
C Description:
C   Computes hourly time stepped gridded biogenic emissions using 
C   normalized gridded emissions from Normbeis3 (3.12) and postprocessed MM5
C   meteorology.
 
C Preconditions:
C   Postprocessed MM5 meteorology that contains temperature, 
C   solar radiation, and pressure data. 
C   Normalized gridded emissions B3GRD from Normbeis3 (3.12) 
 
C Subroutines and Functions Called:
C   BEIS, HRNO, CZANGLE, CHKGRID, SUBHFILE, WRDAYMSG, INTERPX, PROMPTMFILE, ENVINT,
C   ENVYN, CURRSTEP, HHMMSS, NAMEVAL, UPCASE, CHECKMEM, M3EXIT, M3MSG2,
C   WRITE3, OPEN3, CLOSE3, NEXTIME, TIME2SEC
 
C Revision History:
C   3/01: Prototype by Jeff Vukovich
C         Tested only on 36km Lambert domain 
C         Summer/winter switch file option not tested
C   8/04: Updated for BEIS v3.12
C  10/06: J.Young - CMAQ inline
C                 - assumes first call is scenario start date/time
C                 - optional integral average mass diagnostic emissions file
C  10/09: J.Young - intel compiler problem; move last EMISS calculation as
C                   separate assignment

C-----------------------------------------------------------------------
C Modified from:
 
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: tmpbeis.F,v 1.5 2009/10/05 17:24:48 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/work/rep/CCTM/src/vdiff/acm2_inline/tmpbeis.F,v $
C Last updated: $Date: 2009/10/05 17:24:48 $ 
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE BIOG_EMIS             ! beis
      USE SUBST_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_IOPARMS    ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC    ! file header data structure
#include      SUBST_IODECL    # I/O definitions and declarations
        
C Arguments:
      INTEGER, INTENT(  IN ) :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT(  IN ) :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT(  IN ) :: TSTEP( 2 )      ! timestep vector
      INTEGER, INTENT(  IN ) :: N_SPC_BIOG      ! model biogenic species
      INTEGER, INTENT(  IN ) :: BEIS_MAP( : )   ! mapping from beis3 to model species
      REAL,    INTENT( OUT ) :: EMISL( :,:,: )  ! emissions in moles/sec

C External Functions
      INTEGER,         EXTERNAL :: TIME2SEC 
      INTEGER,         EXTERNAL :: ENVINT 
      LOGICAL,         EXTERNAL :: ENVYN
      LOGICAL,         EXTERNAL :: CHKGRID
      CHARACTER( 16 ), EXTERNAL :: PROMPTMFILE
      CHARACTER( 10 ), EXTERNAL :: HHMMSS

C Parameters:
      INTEGER, PARAMETER :: MXRHRS = 24     ! no. of rainfall hours for YL95 algorithm
      REAL,    PARAMETER :: HR2SEC = 1.0 / 3600.0

C Latitude and longitude for zenith angle calculation:
      REAL,    ALLOCATABLE, SAVE :: LAT( :,: ) ! grid lat (deg) -90 <= LAT <= 90
      REAL,    ALLOCATABLE, SAVE :: LON( :,: ) ! grid lon (deg) -180 <= LON <= 180 

C Gridded meteorology data:
      REAL,    ALLOCATABLE, SAVE :: TASFC ( :,: )     ! level-1 air temperature (K)
      REAL,    ALLOCATABLE, SAVE :: SSOLAR( :,: )     ! Surface radiation [W/m**2]
      REAL,    ALLOCATABLE, SAVE :: COSZEN( :,: )     ! cosine of zenith angle
      REAL,    ALLOCATABLE, SAVE :: PRES  ( :,: )     ! surface pressure [Pa]
      REAL,    ALLOCATABLE, SAVE :: ISLTYP( :,: )     ! soil type
      REAL,    ALLOCATABLE, SAVE :: SOILM ( :,: )     ! soil moisture
      REAL,    ALLOCATABLE, SAVE :: SOILT ( :,: )     ! soil temperature
      REAL,    ALLOCATABLE, SAVE :: RN    ( :,: )     ! nonconvective rainfall
      REAL,    ALLOCATABLE, SAVE :: RC    ( :,: )     ! convective rainfall
      REAL,    ALLOCATABLE, SAVE :: RAINFALL( :,:,: ) ! rainfall for 24 hours
      REAL,    ALLOCATABLE, SAVE :: RNTOT ( :,: )     ! RN + RC

      INTEGER, ALLOCATABLE, SAVE :: SWITCH( :,: )     ! Seasonal switch
      INTEGER, ALLOCATABLE, SAVE :: PTYPE ( :,: )     ! NO emissions 'pulse type'
      INTEGER, ALLOCATABLE, SAVE :: PULSEDATE( :,: )  ! date when NO emission pulse begins
      INTEGER, ALLOCATABLE, SAVE :: PULSETIME( :,: )  ! time when NO emission pulse begins

C Gridded beis emissions
      REAL,    ALLOCATABLE, SAVE :: AVGEMIS( :,:,:,: ) ! avg emissions (3.12)
      REAL,    ALLOCATABLE, SAVE :: AVGLAI ( :,:,:,: ) ! avg leaf index

C Gridded normalized emissions:
      REAL,    ALLOCATABLE, SAVE :: SEMIS( :,:,: )    ! Normailized Beis emissions
      REAL,    ALLOCATABLE, SAVE :: SLAI ( :,:,: )    ! Normailized Beis LAI
      REAL,    ALLOCATABLE, SAVE :: GROWAGNO ( :,: )  ! growing season ag NO emis
      REAL,    ALLOCATABLE, SAVE :: NGROWAGNO( :,: )  ! non growing season ag NO emis
      REAL,    ALLOCATABLE, SAVE :: NONAGNO  ( :,: )  ! non agriculture NO emis

C Mole and mass factors:
      REAL MLFC, MSFC, EMPL

C Diagnostic file avg factor
      REAL DIVFAC

C BEIS3 internal, output species:
      REAL,    ALLOCATABLE, SAVE :: EMPOL( :,:,: )    ! emissions of biogenic categories
C EMPOL( :,:,1:NSEF-1 ) = beis3 emissions
C EMPOL( :,:,NSEF )     = hrno  emissions
      REAL,    ALLOCATABLE, SAVE :: EMISS( :,:,: )    ! emissions in g/s

C Logical names and unit numbers:
            
      CHARACTER( 16 ), SAVE :: SNAME   ! logical name for diagnostic emis output (mass)
      CHARACTER( 16 ), SAVE :: NNAME   ! logical name for normalized-emissions input
      CHARACTER( 16 ), SAVE :: GNAME   ! logical name for GRID_CRO_2D
      CHARACTER( 16 ), SAVE :: BNAME   ! logical name for frost switch input
      CHARACTER( 16 ), SAVE :: MNAME   ! logical name for MET_CRO_2D
      CHARACTER( 16 ), SAVE :: SOILINP ! logical name for input NO soil data
      CHARACTER( 16 ), SAVE :: SOILOUT = 'SOILOUT' ! logical name for output NO soil data

C Other variables:
      INTEGER, SAVE :: IHR       ! current simulation hour
      INTEGER, SAVE :: RSTRT     ! SOILINP file pointer to RAINFALL var to start next
      INTEGER, SAVE :: REND      ! SOILINP file pointer to previous last RAINFALL var
      INTEGER          NDX       ! RAINFALL array timestep index
      INTEGER          IOS       ! IO or memory allocation status
      INTEGER, SAVE :: LDATE     ! previous simulation date
      INTEGER, SAVE :: MDATE     ! test date to update beis diag avg
      INTEGER, SAVE :: MTIME     ! test time to update beis diag avg
      INTEGER          NDATE     ! test date to update rainfall
      INTEGER          NTIME     ! test time to update rainfall
      INTEGER, SAVE :: SDATE     ! scenario start date
      INTEGER, SAVE :: STIME     ! scenario start time
      INTEGER, SAVE :: RDATE     ! date to update rainfall
      INTEGER, SAVE :: RTIME     ! time to update rainfall
      INTEGER, SAVE :: EDATE     ! end scenario date
      INTEGER, SAVE :: ETIME     ! end scenario time
      INTEGER, SAVE :: OFFSET    ! difference between start time and inital jtime
      INTEGER, SAVE :: NSTEPS    ! run duration
      INTEGER, SAVE :: NTICS = 0 ! no. of substeps within an output tstep
      INTEGER, SAVE :: RHOURS    ! SOILINP(OUT) file no. of RAINFALL hour variables
      INTEGER          PARTYPE   ! method number to calculate PAR
      INTEGER, SAVE :: LOGDEV
      INTEGER          C, R, I, J, K, S  ! loop counters and subscripts

      LOGICAL          OK        ! check flag
      LOGICAL, SAVE :: SWITCH_FILE   = .TRUE. ! use frost switch file
      LOGICAL       :: ASSUME_SUMMER = .TRUE. ! use summer normalized emissions
      LOGICAL          GETATN                 ! true: sun above horizon at some cell
      LOGICAL, SAVE :: PX_VERSION             ! true: use PX version of MCIP
      LOGICAL, SAVE :: BEMIS_DIAG             ! true: write diagnostic emiss file
      LOGICAL, SAVE :: INITIAL_RUN            ! true: use SOILINP file
      LOGICAL, SAVE :: INITIAL_DAY = .FALSE.  ! false: previous 24 hours of rainfall
                                              ! are available for HRNO
      LOGICAL, SAVE :: LASTTIC                ! true: last sync step this output tstep
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 256 ) :: MESG       ! message buffer
      CHARACTER( 256 ) :: EQNAME     ! equivalent filename
      CHARACTER(  16 ) :: VAR        ! variable name
      CHARACTER(  16 ), SAVE, ALLOCATABLE :: DDTTM( : ) ! description date and time
      CHARACTER(  33 ), SAVE        !123456789012345678901234567890123
     &                 :: DESCSTR = 'hrly cnv. & non-cnv. rainfall for'

      CHARACTER(  16 ) :: PNAME = 'TMPBEIS312'   ! prodedure name

      INTEGER      GXOFF, GYOFF           ! global origin offset from file
C for INTERPX
      INTEGER       :: STRTCOLNB3, ENDCOLNB3, STRTROWNB3, ENDROWNB3  ! Normalized BEIS3
      INTEGER       :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2  ! GRID_DRO_2D
      INTEGER       :: STRTCOLSIN, ENDCOLSIN, STRTROWSIN, ENDROWSIN  ! SOILINP
      INTEGER, SAVE :: STRTCOLBS,  ENDCOLBS,  STRTROWBS,  ENDROWBS   ! BIOSEASON
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2  ! MET_CRO_2D

D     REAL,    ALLOCATABLE, SAVE :: EMISX( : )
D     INTEGER, ALLOCATABLE, SAVE :: CX( : ), RX( : )

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         LOGDEV = INIT3()

C Determine last timestamp
         SDATE = ENVINT( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, IOS )
         STIME = ENVINT( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, IOS )
         NSTEPS = ENVINT( 'CTM_RUNLEN', 'Scenario Duration (HHMMSS)', 24, IOS )
!        OFFSET = JTIME - STIME
         EDATE = SDATE; ETIME = STIME
!        CALL NEXTIME( EDATE, ETIME, -OFFSET )
         CALL NEXTIME( EDATE, ETIME, NSTEPS )   ! end date & time
         NSTEPS = NSTEPS / TSTEP( 1 )           ! initscen guarantees divisibility

C Check to see if frost date switch file to be used
         MESG = 'Using a frost date switch file?'
         SWITCH_FILE = ENVYN ( 'BIOSW_YN', MESG, .TRUE., IOS )

C Get normalized emissions file, BGRD
         NNAME = PROMPTMFILE( 
     &           'Enter name for Normalized Emissions input file',
     &           FSREAD3, 'B3GRD', PNAME )

C Read description of normalized emissions file
         IF ( .NOT. DESC3( NNAME ) ) THEN
            MESG = 'Could not get description of file "' //
     &             TRIM( NNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

C Initialize grid definition 
         OK = CHKGRID( NNAME )

C Get domain decomp info for the normalized emissions file
         CALL SUBHFILE ( NNAME, GXOFF, GYOFF,
     &                   STRTCOLNB3, ENDCOLNB3, STRTROWNB3, ENDROWNB3 )

C Open and check bioseason file if using
         IF ( SWITCH_FILE ) THEN

            BNAME = PROMPTMFILE( 
     &              'Enter name for season switch input file',
     &              FSREAD3, 'BIOSEASON', PNAME )
            
C Read description of switch file
            IF ( .NOT. DESC3( BNAME ) ) THEN
               MESG = 'Could not get description of file "' //
     &                 TRIM( BNAME ) // '"'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF

C Check grid definition 
            IF ( .NOT. CHKGRID( BNAME ) ) THEN
               MESG = 'Grid in file "' // TRIM( BNAME ) //
     &                '" does not match previously set grid.'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF

C Get domain decomp info for the bioseasons switch file
            CALL SUBHFILE ( BNAME, GXOFF, GYOFF,
     &                      STRTCOLBS, ENDCOLBS, STRTROWBS, ENDROWBS )

            ALLOCATE( SWITCH( NCOLS,NROWS ), STAT=IOS )
            CALL CHECKMEM( IOS, 'SWITCH', PNAME )
            SWITCH = 0   ! array

         ELSE

            MESG = 'Use summer normalized emissions?'
            ASSUME_SUMMER = ENVYN ( 'SUMMER_YN', MESG, .TRUE., IOS )

         END IF

C Open met file
         MNAME = PROMPTMFILE( 
     &           'Enter name for gridded met input file',
     &           FSREAD3, 'MET_CRO_2D', PNAME )

C Get description of met file 
         IF ( .NOT. DESC3( MNAME ) ) THEN
            MESG = 'Could not get description of file "'
     &           // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Check that grid description matches BGRD file
         IF ( .NOT. CHKGRID( MNAME ) ) THEN
            MESG = 'Grid in file "' // TRIM( MNAME )
     &           // '" does not match previously set grid.'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Get domain decomp info for the met file
         CALL SUBHFILE ( MNAME, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

C Check if using PX version of MCIP
         PX_VERSION = ENVYN( 'PX_VERSION', 'MCIP is PX version?',
     &                       .FALSE., IOS )
           
C Write emissions diagnostic file?
         BEMIS_DIAG = ENVYN( 'B3GTS_DIAG', 'Write emissions diagnostic file?',
     &                       .FALSE., IOS )
           
         IF ( BEMIS_DIAG ) THEN
C Build description for, and open beis diagnostic file
C (all but variables-table and horizontal domain in description is borrowed from MNAME)
            MDATE = SDATE; MTIME = STIME
            CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
            SDATE3D = MDATE
            STIME3D = MTIME
            TSTEP3D = TSTEP( 1 )
            XORIG3D = XORIG_GD
            YORIG3D = YORIG_GD
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = 1
!           NVARS3D = MSPCS
            NVARS3D = N_SPC_BIOG

            J = 0
            DO I = 1, MSPCS
               IF ( BEIS_MAP( I ) .GT. 0 ) THEN
                  J = J + 1
                  VNAME3D( J ) = EMSPC( I )
                  VDESC3D( J ) = 'biogenic emissions of the indicated species'
                  VTYPE3D( J ) = M3REAL
!                 UNITS3D( J ) = 'tons/hr'
                  UNITS3D( J ) = 'gm/s'
               END IF
            END DO

            FDESC3D = ' '   ! array

            FDESC3D( 1 ) = 'Gridded biogenic emissions from CMAQ-BEIS3'
            FDESC3D( 2 ) = '/from/ ' // PNAME
            FDESC3D( 3 ) = '/Version/ CMAQ'

C Open mass output file (tons/hour)

            SNAME = 'B3GTS_S'
            IF ( MYPE .EQ. 0 ) THEN
               IF ( .NOT. OPEN3( SNAME, FSNEW3, PNAME ) ) THEN
                  MESG = 'Could not open "' // TRIM( SNAME ) // '" file'
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
               END IF
               IF ( .NOT. CLOSE3( SNAME ) ) THEN
                  MESG = 'Could not close "' // TRIM( SNAME )
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
               END IF
            END IF

            CALL SUBST_BARRIER

C Make sure all processors get header description

            IF ( .NOT. OPEN3( SNAME, FSRDWR3, PNAME ) ) THEN
               MESG = 'Could not open "' // TRIM( SNAME ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
            END IF

            IF ( .NOT. DESC3( SNAME ) ) THEN
               MESG = 'Could not get description of "' // TRIM( SNAME ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF

         END IF

C Build name table for variables in normalized emissions file
         ALLOCATE( AVGEMIS( NCOLS,NROWS,NSEF-1,NSEASONS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'AVGEMIS', PNAME )

         ALLOCATE( AVGLAI( NCOLS,NROWS,NLAI,NSEASONS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'AVGLAI', PNAME )

         AVGEMIS = 0.0 ! array
         AVGLAI  = 0.0 ! array

C Open 2-D grid parameters file to get LAT and LON
         GNAME = PROMPTMFILE( 
     &           'Enter name for 2D GRID PARAMETERS input file',
     &           FSREAD3, 'GRID_CRO_2D', PNAME )

         IF ( .NOT. DESC3( GNAME ) ) THEN
            MESG = 'Could not get description of file "' //
     &             TRIM( GNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

C Check grid description against BGRD File 
         IF ( .NOT. CHKGRID( GNAME ) ) THEN
            MESG = 'Grid in file "' // TRIM( GNAME ) //
     &             '" does not match previously set grid.'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

C Get domain decomp info from the normalized emissions file (assume met files have
C same grid dimensions)
         CALL SUBHFILE ( NNAME, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

C Allocate memory for data and read
         ALLOCATE( LAT( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'LAT', PNAME )

         ALLOCATE( LON( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'LON', PNAME )

         ALLOCATE( COSZEN( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'COSZEN', PNAME )

         IF ( .NOT. INTERPX( GNAME, 'LAT', PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       0, 0, LAT ) ) THEN
            MESG = 'Could not read LAT from file "' //
     &              TRIM( GNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

         IF ( .NOT. INTERPX( GNAME, 'LON', PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       0, 0, LON ) ) THEN
            MESG = 'Could not read LON from file "' //
     &              TRIM( GNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

C Read the various categories of normalized emissions
         DO I = 1, NSEASONS 
             
            DO J = 1, NSEF-1
               VAR = 'AVG_' // TRIM( BIOTYPES( J ) ) // SEASON( I )
         
               IF ( .NOT. XTRACT3( NNAME, VAR,
     &                             1,1, STRTROWNB3,ENDROWNB3, STRTCOLNB3,ENDCOLNB3,
     &                             0, 0, AVGEMIS( 1,1,J,I ) ) ) THEN
                  MESG = 'Could not read "' // TRIM( VAR ) //
     &                   '" from file "' // TRIM( NNAME ) // '"'
                  CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
               END IF
            END DO

            DO K = 1, NLAI
               VAR = 'LAI_' // TRIM( LAITYPES( K ) ) // SEASON( I )
 
               IF ( .NOT. XTRACT3( NNAME, VAR,
     &                             1,1, STRTROWNB3,ENDROWNB3, STRTCOLNB3,ENDCOLNB3,
     &                             0, 0, AVGLAI( 1,1,K,I ) ) ) THEN
                  MESG = 'Could not read "' // TRIM( VAR ) //
     &                   '" from file "' // TRIM( NNAME ) // '"'
                  CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
               END IF
            END DO

         END DO   ! end loop over seasons

C Allocate memory for data and read
         ALLOCATE( GROWAGNO( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'GROWAGNO', PNAME )

         ALLOCATE( NGROWAGNO( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NGROWAGNO', PNAME )

         ALLOCATE( NONAGNO( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NONAGNO', PNAME )

         VAR = 'AVG_NOAG_GROW' 
         IF ( .NOT. XTRACT3( NNAME, VAR,
     &                       1,1, STRTROWNB3,ENDROWNB3, STRTCOLNB3,ENDCOLNB3,
     &                       0, 0, GROWAGNO ) ) THEN
            MESG = 'Could not read "' // TRIM( VAR ) //
     &             '" from file "' // TRIM( NNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

         VAR = 'AVG_NOAG_NONGROWNB3' 
         IF ( .NOT. XTRACT3( NNAME, VAR,
     &                       1,1, STRTROWNB3,ENDROWNB3, STRTCOLNB3,ENDCOLNB3,
     &                       0, 0, NGROWAGNO ) ) THEN
            MESG = 'Could not read "' // TRIM( VAR ) //
     &             '" from file "' // TRIM( NNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

         VAR = 'AVG_NONONAG' 
         IF ( .NOT. XTRACT3( NNAME, VAR,
     &                       1,1, STRTROWNB3,ENDROWNB3, STRTCOLNB3,ENDCOLNB3,
     &                       0, 0, NONAGNO ) ) THEN
            MESG = 'Could not read "' // TRIM( VAR ) //
     &             '" from file "' // TRIM( NNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
         END IF

C Allocate memory for met and emissions 
         ALLOCATE( TASFC( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'TASFC', PNAME )
         
         IF ( PX_VERSION ) THEN
            ALLOCATE( SOILM( NCOLS,NROWS ), STAT=IOS )
            CALL CHECKMEM( IOS, 'SOILM', PNAME )

            ALLOCATE( SOILT( NCOLS,NROWS ), STAT=IOS )
            CALL CHECKMEM( IOS, 'SOILT', PNAME )

            ALLOCATE( ISLTYP( NCOLS,NROWS ), STAT=IOS )
            CALL CHECKMEM( IOS, 'ISLTYP', PNAME )
         END IF
         
         ALLOCATE( RN( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RN', PNAME )

         ALLOCATE( RC( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RC', PNAME )

         ALLOCATE( RNTOT( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RNTOT', PNAME )

         ALLOCATE( PTYPE( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PTYPE', PNAME )

         ALLOCATE( PULSEDATE( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PULSEDATE', PNAME )

         ALLOCATE( PULSETIME( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PULSETIME', PNAME )

         ALLOCATE( SSOLAR( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SSOLAR', PNAME )

         ALLOCATE( EMPOL( NCOLS,NROWS,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'EMPOL', PNAME )

         ALLOCATE( PRES( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PRES', PNAME )

         IF ( BEMIS_DIAG ) THEN
            ALLOCATE( EMISS( NCOLS,NROWS,N_SPC_BIOG ), STAT=IOS )
            CALL CHECKMEM( IOS, 'EMISS', PNAME )
         END IF

         ALLOCATE( SEMIS( NCOLS,NROWS,NSEF-1 ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SEMIS', PNAME )

         ALLOCATE( SLAI( NCOLS,NROWS,NLAI ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SLAI', PNAME )

C Initial run until a full 24 hours has been recorded on the SOIL(OUT/INP) file
C for the Yienger and Levy algorithm
         WRITE( LOGDEV,'(/5X, A)' ) 'Temporal BEIS ...'
         MESG = 'Initial run?'
         INITIAL_RUN = ENVYN( 'INITIAL_RUN', MESG, .FALSE., IOS )

C If initial run, initialize some variables, otherwise get them from file
         IF ( INITIAL_RUN ) THEN

            RHOURS = MIN( MXRHRS, NSTEPS )
            RSTRT = 1

            PULSEDATE = 0   ! array
            PULSETIME = 0   ! array
            PTYPE     = 0   ! array
         
         ELSE

C Open saved NO rain data file
            SOILINP = PROMPTMFILE( 
     &                'Enter name for NO EMISSIONS SAVE file',
     &                FSREAD3, 'SOILINP', PNAME )

C Get description of NO rain data file
            IF ( .NOT. DESC3( SOILINP ) ) THEN
               MESG = 'Could not get description of file "' //
     &                 TRIM( SOILINP ) // '"'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF

C Check that file's start date and time are consistent             
            IF ( SDATE3D .NE. SDATE ) THEN
               WRITE( MESG,94010 ) 'Cannot use SOILINP file; ' //
     &             'found date ', SDATE3D, ' expecting ', SDATE
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF
             
            IF ( STIME3D .NE. STIME ) THEN
               WRITE( MESG,94010 ) 'Cannot use SOILINP file; ' //
     &             'found time ', STIME3D, ' expecting ', STIME
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF

C Get domain decomp info for the NO rain data file
            CALL SUBHFILE ( SOILINP, GXOFF, GYOFF,
     &                      STRTCOLSIN, ENDCOLSIN, STRTROWSIN, ENDROWSIN )

            REND = NVARS3D - 3
            RSTRT = REND + 1
            IF ( REND .GE. MXRHRS ) THEN
               REND = MXRHRS
               RSTRT = 1
            END IF
            RHOURS = MIN( MXRHRS, REND + NSTEPS )

         END IF   ! initial run

         ALLOCATE( RAINFALL( NCOLS,NROWS,RHOURS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RAINFALL', PNAME )
         RAINFALL = 0.0   ! array

         ALLOCATE( DDTTM( RHOURS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'DDTTM', PNAME )
         DDTTM = ' '   ! array

         IF ( .NOT. INITIAL_RUN ) THEN   ! SOILINP file available
C Read data from file
            VAR = 'PTYPE'
            IF ( .NOT. XTRACT3( SOILINP, VAR,
     &                          1,1, STRTROWSIN,ENDROWSIN, STRTCOLSIN,ENDCOLSIN,
     &                          0, 0, PTYPE ) ) THEN
               MESG = 'Could not read "' // TRIM( VAR ) // 
     &                '" from file "' // TRIM( SOILINP ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF
             
            VAR = 'PULSEDATE'
            IF ( .NOT. XTRACT3( SOILINP, VAR,
     &                          1,1, STRTROWSIN,ENDROWSIN, STRTCOLSIN,ENDCOLSIN,
     &                          0, 0, PULSEDATE ) ) THEN
               MESG = 'Could not read "' // TRIM( VAR ) // 
     &                '" from file "' // TRIM( SOILINP ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF
             
            VAR = 'PULSETIME'
            IF ( .NOT. XTRACT3( SOILINP, VAR,
     &                          1,1, STRTROWSIN,ENDROWSIN, STRTCOLSIN,ENDCOLSIN,
     &                          0, 0, PULSETIME ) ) THEN
               MESG = 'Could not read "' // TRIM( VAR ) // 
     &                '" from file "' // TRIM( SOILINP ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF
             
            DO I = 1, REND

C Get previous 24 hour rainfall amounts
               WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I

               IF ( .NOT. XTRACT3( SOILINP, VAR,
     &                             1,1, STRTROWSIN,ENDROWSIN, STRTCOLSIN,ENDCOLSIN,
     &                             0, 0, RAINFALL( 1,1,I ) ) ) THEN
                  MESG = 'Could not read "' // TRIM( VAR ) //
     &                   '" from file "' // TRIM( SOILINP ) // '"'
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
               END IF

               J = LEN_TRIM( VDESC3D( I+3 ) )
               K = INDEX( VDESC3D( I+3 ), 'for' ) + 3
               DDTTM( I ) = VDESC3D( I+3)(K:J )

            END DO

C Close input file
            IF ( .NOT. CLOSE3( SOILINP ) ) THEN
               MESG = 'Could not close file "' // 
     &                 TRIM( SOILINP ) // '"'
               CALL M3EXIT( PNAME, 0, 0, MESG, XSTAT2 )
            END IF
             
         END IF   ! not initial run

C Initialize normalized emissons to be used 
         IF ( ASSUME_SUMMER ) THEN
            SEMIS = AVGEMIS( 1:MY_NCOLS,1:MY_NROWS,1:NSEF-1,NSUMMER )
            SLAI  = AVGLAI ( 1:MY_NCOLS,1:MY_NROWS,1:NLAI,  NSUMMER )
         ELSE
            SEMIS = AVGEMIS( 1:MY_NCOLS,1:MY_NROWS,1:NSEF-1,NWINTER )
            SLAI  = AVGLAI ( 1:MY_NCOLS,1:MY_NROWS,1:NLAI,  NWINTER )
         END IF

         RDATE = SDATE; RTIME = STIME
         LDATE = 0
         IHR = 0

D        ALLOCATE( EMISX( MSPCS ), STAT=IOS )
D        CALL CHECKMEM( IOS, 'EMISX', PNAME )
D        ALLOCATE( CX( MSPCS ), STAT=IOS )
D        CALL CHECKMEM( IOS, 'CX', PNAME )
D        ALLOCATE( RX( MSPCS ), STAT=IOS )
D        CALL CHECKMEM( IOS, 'RX', PNAME )

         IF ( BEMIS_DIAG ) EMISS = 0.0   ! array

      END IF   ! FIRSTIME

C Loop thru the number of time steps (hourly)

      EMISL = 0.0   ! array
      EMPOL = 0.0   ! array
D     EMISX = 0.0   ! array

C Compute zenith angle
      CALL CZANGLE( JDATE, JTIME, MY_NCOLS, MY_NROWS, LAT, LON, COSZEN ) 

      IF ( JDATE .NE. LDATE ) THEN

         CALL WRDAYMSG( JDATE, MESG )               

C If new date, read season switch 
         IF ( SWITCH_FILE ) THEN
            MESG = 'Reading gridded season switch data...'
            CALL M3MSG2( MESG ) 
             
            IF ( .NOT. XTRACT3( BNAME, 'SEASON',
     &                          1,1, STRTROWBS,ENDROWBS, STRTCOLBS,ENDCOLBS,
     &                          JDATE, 0, SWITCH ) ) THEN
               MESG = 'Could not read SEASON from file "' //
     &                TRIM( BNAME ) // '"'
               CALL M3EXIT( PNAME, JDATE, 0, MESG, XSTAT2 )
            END IF

            MESG = 'Applying gridded season switch data...' 
            CALL M3MSG2( MESG )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  IF ( SWITCH( C,R ) .EQ. 0 ) THEN   ! use winter normalized emissions
                     SEMIS( C,R,1:NSEF-1 ) = AVGEMIS( C,R,1:NSEF-1,NWINTER )
                     SLAI ( C,R,1:NLAI   ) =  AVGLAI( C,R,1:NLAI,  NWINTER )
                  ELSE
                     SEMIS( C,R,1:NSEF-1 ) = AVGEMIS( C,R,1:NSEF-1,NSUMMER )
                     SLAI ( C,R,1:NLAI   ) =  AVGLAI( C,R,1:NLAI,  NSUMMER )
                  END IF                      
               END DO
            END DO
 
         END IF   ! if using switch file

      END IF   ! if new day

      WRITE( MESG,94030 ) HHMMSS( JTIME )
      CALL M3MSG2( MESG )

C Read temperature data
!     IF ( .NOT. INTERPX( MNAME, 'TEMP10', PNAME,
      IF ( .NOT. INTERPX( MNAME, 'TEMP2', PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    JDATE, JTIME, TASFC ) ) THEN
!        MESG = 'Could not read "' // 'TEMP10' // 
         MESG = 'Could not read "' // 'TEMP2' // 
     &          '" from file "' // TRIM( MNAME ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

C Read surface radiation data
      IF ( .NOT. INTERPX( MNAME, 'RGRND', PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    JDATE, JTIME, SSOLAR ) ) THEN
         MESG = 'Could not read "' // 'RGRND' // 
     &          '" from file "' // TRIM( MNAME ) // '"' 
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

C Read surface pressure data 
      IF ( .NOT. INTERPX( MNAME, 'PRSFC', PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    JDATE, JTIME, PRES ) ) THEN
         MESG = 'Could not read "' // 'PRSFC' // 
     &           '" from file "' // TRIM( MNAME ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

C Calculate temporal non-speciated beis emissions
      CALL BEIS3( JDATE, JTIME, MY_NCOLS, MY_NROWS, MSPCS, COSZEN, SEMIS,
     &            SLAI, TASFC, SSOLAR, PRES, EMPOL )

      IF ( PX_VERSION ) THEN
       
C Read soil moisture data
         IF ( .NOT. INTERPX( MNAME, 'SOIM1', PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, SOILM ) ) THEN
            MESG = 'Could not read "' // 'SOIM1' // 
     &             '" from file "' // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Read soil temperature data
         IF ( .NOT. INTERPX( MNAME, 'SOIT1', PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, SOILT ) ) THEN
            MESG = 'Could not read "' // 'SOIT1' // 
     &                '" from file "' // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Read soil type data
         IF ( .NOT. INTERPX( MNAME, 'SLTYP', PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, ISLTYP ) ) THEN
            MESG = 'Could not read "' // 'SLTYP' // 
     &             '" from file "' // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
           
      END IF

C Non-convective (RN) and convective (RC) rain is the total amount for the met
C preprocessor's (typically MCIP) output timestep (typically one hour). It doesn't
C make much sense to time-interpolate these values, since rain generally does not
C fall at a constant rate for an output timestep.
      CALL CURRSTEP( JDATE, JTIME, SDATE, STIME, TSTEP( 1 ), NDATE, NTIME )
      IF ( NDATE .EQ. RDATE .AND. NTIME .EQ. RTIME ) THEN   ! on the TSTEP (hour)

C Read rainfall data
         IF ( .NOT. XTRACT3( MNAME, 'RN',
     &                       1,1, STRTROWMC2,ENDROWMC2, STRTCOLMC2,ENDCOLMC2,
     &                       RDATE, RTIME, RN ) ) THEN
            MESG = 'Could not read "' //  'RN' // 
     &              '" from file "' // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

         IF ( .NOT. XTRACT3( MNAME, 'RC',
     &                       1,1, STRTROWMC2,ENDROWMC2, STRTCOLMC2,ENDCOLMC2,
     &                       RDATE, RTIME, RC ) ) THEN
            MESG = 'Could not read "' //  'RC' // 
     &             '" from file "' // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Store hourly rainfall totals
         NDX = 1 + MOD( IHR + RSTRT-1, MXRHRS )
         RAINFALL( :,:,NDX ) = RN + RC
         WRITE( DDTTM( NDX ),94020 ) RDATE, RTIME

         CALL NEXTIME( RDATE, RTIME, TSTEP( 1 ) )

!        IF ( RSTRT+IHR .LT. MXRHRS ) THEN
         IF ( RSTRT+IHR .LE. MXRHRS ) THEN
            INITIAL_DAY = .TRUE.
         ELSE   ! store accumulated rain in RN array
            INITIAL_DAY = .FALSE.
            RNTOT = 0.0   ! array
            DO I = 1, MXRHRS
               RNTOT = RNTOT + RAINFALL( :,:,I )
            END DO
         END IF

         IHR = IHR + 1

      END IF   ! on the output tstep

C Calculate temporal non-speciated soil NO emissions
      CALL HRNO( JDATE, JTIME, MY_NCOLS, MY_NROWS, TASFC,
     &           PX_VERSION, SOILM, SOILT, ISLTYP,
     &           INITIAL_DAY, RNTOT, GROWAGNO, NGROWAGNO, NONAGNO,
     &           PTYPE, PULSEDATE, PULSETIME, EMPOL )

C Speciate emissions

D     EMISX = 0.0; CX = 0; RX = 0    ! arrays
      DO K = 1, NSEF
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               EMPL = EMPOL( C,R,K ) * HR2SEC
               IF ( EMPL .LE. 0.0 ) CYCLE
               I = 0
               DO S = 1, MSPCS
                  IF ( BEIS_MAP( S ) .GT. 0 ) THEN
                     I = I + 1
                     MLFC = MLFAC( S,K )
                     EMISL( I,C,R ) = EMISL( I,C,R ) + EMPL * MLFC
D                    IF ( EMISL( I,C,R ) .GT. EMISX( I ) ) THEN
D                        EMISX( I ) = EMISL( I,C,R )
D                        CX( I ) = C
D                        RX( I ) = R
D                    END IF
                  END IF
               END DO
            END DO
         END DO
      END DO

D     WRITE( LOGDEV,* ) ' '
D     WRITE( LOGDEV,* ) '       from TMPBEIS312'
D     WRITE( LOGDEV,* ) '    Spc   EMISL  MaxC MaxR'
D     DO S = 1, N_SPC_BIOG
D        WRITE( LOGDEV,'( 4X, I3, F10.5, 2I4 )' ) S, EMISX( S ), CX( S ), RX( S )
D     END DO

D     EMISX = 0.0    ! array
D     WRITE( LOGDEV,* ) ' '
D     WRITE( LOGDEV,* ) '                   from TMPBEIS312'
D     WRITE( LOGDEV,* ) '    Spc SpcName  Pol  MLFAC     EMPOL     EMISL( Spc,32,7 )'
D     C = 32; R = 7
D     DO K = 1, NSEF
D        EMPL = EMPOL( C,R,K ) * HR2SEC
D        IF ( EMPL .LE. 0.0 ) CYCLE
D        I = 0
D        DO S = 1, MSPCS
D           IF ( BEIS_MAP( S ) .GT. 0 ) THEN
D              I = I + 1
D              MLFC = MLFAC( S,K )
D              EMISX( I ) = EMISX( I ) + EMPL * MLFC
D              IF ( MLFC .NE. 0.0 )
D    &            WRITE( LOGDEV,'( 4X, I3, 2X, A6, 2X, I3, 1X, 3( 1PE10.3 ) )' )
D    &                              S, EMSPC( I ), K,  MLFC, EMPL, EMISX( I )
D           END IF
D        END DO
D     END DO

      NTICS = NTICS + 1
      LASTTIC = NTICS .EQ. TIME2SEC( TSTEP( 1 ) ) / TIME2SEC( TSTEP( 2 ) )

      IF ( BEMIS_DIAG ) THEN
         IF ( NTICS .EQ. 1 ) THEN
            DO K = 1, NSEF
               I = 0
               DO S = 1, MSPCS
                  IF ( BEIS_MAP( S ) .GT. 0 ) THEN
                     I = I + 1
                     MSFC = HR2SEC * MSFAC( S,K )
                     EMISS( :,:,I ) = EMISS( :,:,I ) + EMPOL( :,:,K ) * MSFC
                  END IF
               END DO
            END DO
         ELSE IF ( .NOT. LASTTIC ) THEN
            DO K = 1, NSEF
               I = 0
               DO S = 1, MSPCS
                  IF ( BEIS_MAP( S ) .GT. 0 ) THEN
                     I = I + 1
                     MSFC = HR2SEC * MSFAC( S,K )
                     EMISS( :,:,I ) = EMISS( :,:,I ) + 2.0 * EMPOL( :,:,K ) * MSFC
                  END IF
               END DO
            END DO
         ELSE   ! LASTTIC
            DIVFAC = 0.5 / FLOAT( NTICS )
            DO K = 1, NSEF
               I = 0
               DO S = 1, MSPCS
                  IF ( BEIS_MAP( S ) .GT. 0 ) THEN
                     I = I + 1
                     MSFC = HR2SEC * MSFAC( S,K )
!                    EMISS( :,:,I ) = DIVFAC
!    &                              * ( EMISS( :,:,I ) + EMPOL( :,:,K ) * MSFC )
                     EMISS( :,:,I ) = EMISS( :,:,I ) + EMPOL( :,:,K ) * MSFC
                  END IF
               END DO
            END DO
            EMISS = DIVFAC * EMISS   ! array assignment
            IF ( .NOT. WRITE3( SNAME, 'ALL', MDATE, MTIME, EMISS ) ) THEN
               MESG = 'Could not write to output file "' // TRIM( SNAME ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF
            WRITE( LOGDEV,94040 )
     &            'Timestep written to', SNAME,
     &            'for date and time', MDATE, MTIME
            EMISS = 0.0   ! array
            CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
         END IF
      END IF

C Save date
      LDATE = JDATE

!     IF ( JDATE .NE. EDATE .OR. JTIME .NE. ETIME ) RETURN
      IF ( IHR .LT. NSTEPS ) THEN
         IF ( LASTTIC ) THEN
            NTICS = 0
         END IF
         RETURN
      ELSE IF ( NTICS .LT. TIME2SEC( TSTEP( 1 ) ) / TIME2SEC( TSTEP( 2 ) ) ) THEN
         RETURN
      END IF

C Create rain data file for soil NO

C Final timestamp
      NDATE = EDATE; NTIME = ETIME
!     CALL NEXTIME ( NDATE, NTIME, OFFSET )

C Build description for, and create/open soil NO emissions output file
      FTYPE3D = GRDDED3
      SDATE3D = NDATE
      STIME3D = NTIME
!     TSTEP3D = TSTEP
      TSTEP3D = 0   ! make it a time-independent file
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = 1
      NVARS3D = 3 + RHOURS
      MXREC3D = 1

      VNAME3D = ' '
      VNAME3D( 1 ) = 'PTYPE'
      VNAME3D( 2 ) = 'PULSEDATE'
      VNAME3D( 3 ) = 'PULSETIME'
      
      DO I = 1, RHOURS
         WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I
         VNAME3D( I+3 ) = VAR
      END DO

      UNITS3D = ' '
      UNITS3D( 1 ) = 'INTEGER'
      UNITS3D( 2 ) = 'YYYYDDD'
      UNITS3D( 3 ) = 'HHMMSS'
      UNITS3D( 4:RHOURS+3 ) = 'cm'

      VDESC3D( 1 ) = 'NO emission pulse type'
      VDESC3D( 2 ) = 'CMAQ starting date for NO emission pulse'
      VDESC3D( 3 ) = 'CMAQ starting time for NO emission pulse' 
!     VDESC3D( 4:RHOURS+3 ) = 'hourly convective and non-convective rainfall'
      DO I = 1, RHOURS
         VDESC3D( I+3 ) = DESCSTR // DDTTM( I )
      END DO

      VTYPE3D = 0
      VTYPE3D( 1 ) = M3INT
      VTYPE3D( 2 ) = M3INT
      VTYPE3D( 3 ) = M3INT
      VTYPE3D( 4:RHOURS+3 ) = M3REAL

      FDESC3D = ' '
      FDESC3D( 1 ) = 'Gridded rainfall data for soil NO emissions'
      FDESC3D( 2 ) = '/From/ ' // PNAME
      FDESC3D( 3 ) = '/Version/ CMAQ'

C Open NO rain data save file
      IF ( MYPE .EQ. 0 ) THEN
         IF ( .NOT. OPEN3( SOILOUT, FSNEW3, PNAME ) ) THEN
            MESG = 'Could not open "' // TRIM( SOILOUT ) // '" file'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
         END IF
         IF ( .NOT. CLOSE3( SOILOUT ) ) THEN
            MESG = 'Could not close "' // TRIM( SOILOUT )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
         END IF
      END IF

      CALL SUBST_BARRIER

C Make sure all processors get header description
      IF ( .NOT. OPEN3( SOILOUT, FSRDWR3, PNAME ) ) THEN
         MESG = 'Could not open "' // TRIM( SOILOUT ) // '" file'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3( SOILOUT ) ) THEN
         MESG = 'Could not get description of file "' //
     &          TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

C Write soil NO rain data file

      VAR = 'PTYPE'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PTYPE ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF
         
      VAR = 'PULSEDATE'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PULSEDATE ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF
         
      VAR = 'PULSETIME'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PULSETIME ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF
         
      DO I = 1, RHOURS
         WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I
         IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, RAINFALL( 1,1,I ) ) ) THEN
            MESG = 'Could not write "' // TRIM( VAR ) //
     &             '" to file "' // TRIM( SOILOUT ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
      END DO

      WRITE( LOGDEV,94040 ) 
     &      'Timestep written to', SOILOUT,
     &      'for date and time', JDATE, JTIME

      RETURN

C------------------  FORMAT  STATEMENTS   ------------------------------

94010 FORMAT( 10( A, ':', I8, ':', 1X ) )
94020 FORMAT( 1X, I7, ":", I6.7 )
94030 FORMAT( 5X, 'Temporal BEIS at time ', A8 )
94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

      END SUBROUTINE TMPBEIS312  

