        MODULE MODBEIS3v11

C***********************************************************************
C  Module body starts at line 42 
C
C  DESCRIPTION:
C     This module contains the public variables and allocatable arrays 
C     used only in the biogenic emissions BEIS3v1 module.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION HISTORY:
C     03/01: prototype by Jeff Vukovich
C     04/03: added variables and arrays for air quality forecasting by GAP
C     01/05: Added an addtional TARGET attribute to variable EMISL (David Wong)
C
C***************************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: modbeis3.f,v 1.1.1.1 2001/03/27 19:08:49 smith_w Exp $
C
C COPYRIGHT (C) 2001, MCNC--North Carolina Supercomputing Center
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C MCNC-Environmental Programs Group
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C
C env_progs@mcnc.org
C
C Pathname: $Source: /env/proj/archive/cvs/jmv/beis3v0.9/modbeis3.f,v $
C Last updated: $Date: 2001/03/27 19:08:49 $ 
C
C***********************************************************************
C...........   Emission factor, vegetation types tables:

        INTEGER, PUBLIC ::    NVEG                     !  Number of veg types
        INTEGER, ALLOCATABLE, PUBLIC :: LAI( : )       !  Leaf area index
        REAL,    ALLOCATABLE, PUBLIC :: EMFAC( :, : )  !  Emission factors
        REAL,    ALLOCATABLE, PUBLIC :: LFBIO( : )     !  Dry leaf biomass
        REAL,    ALLOCATABLE, PUBLIC :: WFAC( : )      !  Winter biomass factor
        REAL,    ALLOCATABLE, PUBLIC :: SLW( : )       !  Specific leaf weight

        REAL,    ALLOCATABLE, PUBLIC :: AVGEMIS( :, :, :, : )   ! avg emissions
        REAL,    ALLOCATABLE, PUBLIC ::  NOEMIS( :, : ,:    )   ! no emissions

        REAL,    ALLOCATABLE, PUBLIC :: AVGLAI( :, :, :, : )    ! avg leaf index

        CHARACTER*16, ALLOCATABLE, PUBLIC :: VEGID( : )     !  Veg types
C
C   from includes
C
        PRIVATE :: SPNLEN3
        INTEGER, PARAMETER :: SPNLEN3 =  5  ! Speciation profile code


C
C  from BEIS311 main program
C
!AQF        REAL,     PARAMETER, PUBLIC :: HR2SEC = 0.0002778


C.........  Latitude and longitude for zenith angle calculation

        REAL, ALLOCATABLE :: LAT  ( :, : )    !  grid lat (deg) -90 <= LAT <= 90
        REAL, ALLOCATABLE :: LON  ( :, : )    !  grid lon (deg) -180 <= LON <= 180 


        REAL, ALLOCATABLE :: TASFC ( :, : )     !  level-1 air  temperature (K)
        REAL, ALLOCATABLE :: RN ( : , : )       !  nonconvective rainfall
        REAL, ALLOCATABLE :: RC ( : , : )       !  convective rainfall
        REAL, ALLOCATABLE :: rainfall( : , : , : ) ! rainfall for 24 hoursn
        REAL, ALLOCATABLE :: TSOLAR ( :, :)     !  Photosynthetic Active Radiation (PAR)
        REAL, ALLOCATABLE :: COSZEN( :, : )     !  cosine of zenith angle
        REAL, ALLOCATABLE :: PRES( :, : )       !  surface pressure

        INTEGER, ALLOCATABLE :: SWITCH( :, : )  !  Seasonal switch

C.......   Mole and mass factors
 
        REAL, ALLOCATABLE ::  MLFAC( :, : )           !  mole factors 
        REAL, ALLOCATABLE ::  MSFAC( :, : )           !  mass factors (tons/hour)

        REAL, ALLOCATABLE ::  SEMIS( :, :, : )        ! temporary emis
        REAL, ALLOCATABLE ::  SLAI ( :, :, : )        ! temporary LAI
        REAL, ALLOCATABLE ::  NONAGNO( : , : )        ! non agriculture NO emis
        REAL, ALLOCATABLE ::  NGROWAGNO( : , : )      ! non growing season ag NO emis
        REAL, ALLOCATABLE ::  GROWAGNO( : , : )       ! growing season NO emis

C.......   BEIS3 internal, output species

        REAL, ALLOCATABLE :: EMPOL( :, :, : )         ! emissions of biogenic categories
        REAL, ALLOCATABLE, TARGET :: EMISL( :, :, : )         ! emissions in moles/hour
        REAL, ALLOCATABLE :: EMISS( :, :, : )         ! emissions in tons/hour

C........ begin  added 12/01 by GAP for NO emission calculation
        INTEGER, ALLOCATABLE :: PTYPE ( : , : )       ! NO emissions 'pulse type'
        INTEGER, ALLOCATABLE :: PULSEDATE( : , : )    ! date when NO emission pulse begins
        INTEGER, ALLOCATABLE :: PULSETIME( : , : )    ! time when NO emission pulse begins
C........ end added 12/01 by GAP

        CHARACTER*5      CTZONE     ! string of time zone
        CHARACTER*5      BTMP       ! temporary variable string 
        CHARACTER*16     RADNAM     ! string for shortwave radiation reaching ground
        CHARACTER*16     TMPRNAM    ! string for temperature 
C........ begin added 12/01 by GAP
        CHARACTER*16     STNAM      ! string for soil type data
        CHARACTER*16     SMNAM      ! string for soil moisture data
        CHARACTER*16     STMPNAM    ! string for soil temperature data
        CHARACTER*16     RNNAM      ! string for nonconvective rainfall
        CHARACTER*16     RCNAM      ! string for    convective rainfall
C........ end added 12/01 by GAP
        CHARACTER*16     PRESNAM    ! string for sfc pressure
        CHARACTER*16     VTMP       ! temporary variable string
        CHARACTER*50  :: METSCEN    !  temporary string for met scenario name
        CHARACTER*50  :: CLOUDSHM   !  temporary string for cloud scheme name
        CHARACTER*50  :: LUSE       !  temporary string for land use description


        CHARACTER*16,ALLOCATABLE ::  EMSPC( : )   ! names of emitting species 
        CHARACTER(LEN=SPNLEN3)       SPPRO        ! speciation profile to use

        CHARACTER*80    PARMENU( 1 )            ! Methods to calc. PAR
        DATA     PARMENU / 'Use MM5 generated radiation' /

C...........   Logical names and unit numbers

        INTEGER         LDEV    !  unit number for log device
        INTEGER         RDEV    !  unit number for speciation profiles file
            
        CHARACTER*16    ENAME   !  logical name for emissions output (moles)
        CHARACTER*16    SNAME   !  logical name for emissions output (mass)
        CHARACTER*16    NNAME   !  logical name for normalized-emissions input
        CHARACTER*16    GNAME   !  logical name for GRID_CRO_2D
        CHARACTER*16    BNAME   !  logical name for frost switch input
        CHARACTER*16    M3NAME  !  logical name for MET_FILE1
        CHARACTER*16    M2NAME  !  logical name for MET_FILE2
        CHARACTER*16    PNAME   !  logical name for file with pressure variable
C...... begin added 12/01 by GAP
        CHARACTER*16    SOILINP  !  logical name for NO emission calculation soil information to read
        CHARACTER*16    SOILOUT  !  logical name for NO emissions calculation soil information to write

        LOGICAL         INITIAL_RUN, INITIAL_HOUR
        INTEGER         INDEX
        INTEGER         RHOURS
C....... end added 12/01 by GAP

!        CHARACTER*16    UNITSMENU( 2 )            ! output units
!        DATA     UNITSMENU
!     &           / 'mole/hr ',
!     &             'mole/s '  /

C...........   Other variables and their descriptions:
C..... begin added 12/01 by GAP
        INTEGER         sfile   ! unit  number for soil matrix file
C......end added 12/01 by GAP

        INTEGER         HR      !  current simulation hour
        INTEGER         UNITTYPE ! define output units

        INTEGER         IOS     !  temporay IO status
        INTEGER         JDATE   !  current simulation date (YYYYDDD)
        INTEGER         JTIME   !  current simulation time (HHMMSS)
C........ begin  added by GAP 12/01
        INTEGER         NDATE   !  date of SOILINP file
        INTEGER         NTIME   !  time of SOILINP file
        integer         i_loop
        character*1     ichar
        character*2     i2char
        character*10    vname
C........ end added by GAP 12/01
        INTEGER         LDATE   !  previous simulation date
        INTEGER         MDATE   !  met file 1 start date
        INTEGER         MSPCS   ! no. of emitting species
        INTEGER         MTIME   !  met file 1 start time
        INTEGER         MXSTEPS !  maximum number of time steps
        INTEGER         NCOLS   ! no. of grid columns
        INTEGER         NGRID   ! no. of grid cells
        INTEGER         NLINES  ! no. of lines in GSPRO speciation profiles file  
        INTEGER         NROWS   ! no. of grid rows
        INTEGER         NSTEPS  !  duration of met file
        INTEGER         BSTEPS  ! no. of hourly time steps for output
        INTEGER         PARTYPE !  method number to calculate PAR
        INTEGER         RDATE   !  met file 2 start date 
        INTEGER         RTIME   !  met file 2 start time
        INTEGER         TZONE   !  output-file time zone ; not used in program
        INTEGER         JRUNLEN

        LOGICAL         EFLAG   !  error flag 
        LOGICAL ::      SWITCH_FILE = .FALSE.  ! use frost switch file
        LOGICAL ::      ASSUME_SUMMER = .TRUE. ! use summer normalized emissions
        LOGICAL ::      GETATN

        INTEGER ::      SOILOUT_TIME  ! time to write soiloutput file
        END MODULE MODBEIS3v11
