       MODULE MOD_LAYPOINT
!      REVISED: 02/05 for new plmrise       
C...........   INCLUDES:
C...........   INCLUDES:
        INCLUDE 'EMCNST3.EXT'   !  emissions constant parameters
        INCLUDE 'PARMS3.EXT'    !  i/o api parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'FDESC3.EXT'    !  I/O API file description data structures.
        INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions
        INCLUDE 'CONST3.EXT'    !  physical and mathematical constants

C...........  LOCAL PARAMETERS and their descriptions:

C...........  LOCAL PARAMETERS and their descriptions:

        REAL, PARAMETER :: USTARMIN = 0.1   ! Min valid value for USTAR

        CHARACTER(50), PARAMETER ::CVSW = '$Name: SMOKE_v2_1_09302004 $' ! CVS release tag

C.........  Indicator for which public inventory arrays need to be read
        INTEGER,            PARAMETER :: NINVARR = 8
        CHARACTER(IOVLEN3), PARAMETER :: IVARNAMS( NINVARR ) = 
     &                                 ( / 'IFIP           '
     &                                   , 'XLOCA          '
     &                                   , 'YLOCA          '
     &                                   , 'STKHT          '
     &                                   , 'STKDM          '
     &                                   , 'STKTK          '
     &                                   , 'STKVE          '
     &                                   , 'CSOURC         ' / )

C...........   LOCAL VARIABLES and their descriptions:
C...........   Point source stack parameters:

C.........  Allocatable, per-source meteorology variables
        REAL   , ALLOCATABLE :: HFX  ( : )    !  sensible heat flux (M K / S )
        REAL   , ALLOCATABLE :: HMIX ( : )    !  mixing height (m)
        REAL   , ALLOCATABLE :: TSFC ( : )    !  surface temperature (deg K)
        REAL   , ALLOCATABLE :: USTAR( : )    !  friction velocity (m/s)
        REAL   , ALLOCATABLE :: PRSFC( : )    !  sfc pressure (Pascals)	(added 2/05)

C.........  Allocatable, per-source and per layer meteorology variables. 
C           Dimensioned by layers, then sources
        REAL   , ALLOCATABLE :: DDZH ( :,: )  !  1/( zh(l) - zh(l-1) )
        REAL   , ALLOCATABLE :: DDZF ( :,: )  !  1/( zh(l) - zh(l-1) )
        REAL   , ALLOCATABLE :: PRES ( :,: )  !  pressure (Pa) - Mid-level
        REAL   , ALLOCATABLE :: PREF ( :,: )  !  pressure (Pa) - Full-level
        REAL   , ALLOCATABLE :: QV   ( :,: )  !  mixing ratio (kg/kg)
        REAL   , ALLOCATABLE :: TA   ( :,: )  !  temperature (K)
        REAL   , ALLOCATABLE :: UWIND( :,: )  !  wind speed (m/s)
        REAL   , ALLOCATABLE :: VWIND( :,: )  !  wind speed (m/s)
        REAL   , ALLOCATABLE :: ZF   ( :,: )  !  layer surface height (m)
        REAL   , ALLOCATABLE :: ZH   ( :,: )  !  layer center  height (m)
        REAL   , ALLOCATABLE :: ZSTK ( :,: )  !  zf( l,s ) - stkht(s) (m)
        REAL   , ALLOCATABLE :: DENS ( :,: )  !  air density (kg/m3) (added 2/05)	

C.........  Allocatable, temporary per-layer variables from 1:EMLAYS
        REAL   , ALLOCATABLE :: WSPD ( : )    !  wind speed (m/s)
        REAL   , ALLOCATABLE :: DTHDZ( : )    !  gradient of THETV
        REAL   , ALLOCATABLE :: TFRAC( : )    !  Temporary LFRAC

C.........  Allocatable, temporary per-layer variables from 0:EMLAYS
        REAL   , ALLOCATABLE :: PRESF( : )    !  pressure at full-levels
        REAL   , ALLOCATABLE :: ZZF  ( : )    !  elevation at full-levels

C.........  Allocatable cross- OR dot-point meteorology input buffers
        REAL   , ALLOCATABLE :: XBUF( :,: )   ! cross-point
        REAL   , ALLOCATABLE :: DBUF( :,: )   ! dor-point

C.........  Allocatable un-gridding matrices (uses bilinear interpolation)
C           Dimensioned 4 by NSRC
        INTEGER, ALLOCATABLE :: ND( :,: )     !  dot-point, cell indexes
        INTEGER, ALLOCATABLE :: NX( :,: )     !  cross-point, cell indexes
   
        REAL   , ALLOCATABLE :: CD( :,: )     !  dot-point, coefficients
        REAL   , ALLOCATABLE :: CX( :,: )     !  cross-point, coefficients

C.........  Output layer fractions, dimensioned NSRC, emlays
C        REAL   , ALLOCATABLE :: LFRAC( :, : )

C.........  Input/output hour-specific data index, dimensioned by NSRC and
C           by EMLAYS, so that index can be written to PLAY_EX file
        INTEGER, ALLOCATABLE :: LOCINDXH( :,: )

C.........  Fixed-dimension arrays
        REAL         LFULLHT( 0:MXLAYS3 )     !  full-level heights [m]
        REAL         LHALFHT( 1:MXLAYS3 )     !  half-level heights [m]
        REAL         VGLVSXG( 0:MXLAYS3 )     !  vertical coord values
        REAL         WEIGHTS( 1:MXLAYS3 )     !  tmp weights for vertical aloc
        REAL         TEMPS  ( 1:MXLAYS3 )     !  half-level temps (K)
C...........   Logical names and unit numbers

        INTEGER         IDEV    !  tmp unit number if ENAME is map file
        INTEGER         LDEV    !  log file
        INTEGER      :: PDEV    !  elevated/PinG source file
        INTEGER      :: RDEV    !  optional report iff REP_LAYER_MAX is set
        INTEGER         SDEV    !  ASCII part of inventory file

        CHARACTER(16)   ANAME   !  ASCII point-source inventory file
!        CHARACTER(16)   DNAME   !  dot-point layered met file name
        CHARACTER(16)   ENAME   !  point-source inventory input file
        CHARACTER(16)   GNAME   !  cross-point layered grid file name
        CHARACTER(16)   HNAME   !  hourly input file name
        CHARACTER(16)   INAME   !  tmp name for inven file of unknown fmt
        CHARACTER(16)   LNAME   !  layer fractions matrix output file
        CHARACTER(16)   SNAME   !  cross-point surface met file name
        CHARACTER(16)   XNAME   !  cross-point layered met file name

C...........   Other local variables

        INTEGER          I, J, K, L, L1, L2, S, T  ! counters and indices

        INTEGER          EMLAYS    ! number of emissions layers
        INTEGER          IOS       ! tmp i/o status
        INTEGER          JDATE     ! Julian date (YYYYDDD)
        INTEGER          JTIME     ! time (HHMMSS)
        INTEGER          LBOT      ! plume bottom layer
        INTEGER          LDATE     ! previous date
        INTEGER          LPBL      ! first L: ZF(L) above mixing layer
        INTEGER          LSTK      ! first L: ZF(L) > STKHT
        INTEGER          LTOP      ! plume top    layer
        INTEGER          METNCOLS  ! met grid number of columns
        INTEGER          METNGRID  ! met grid number of cells
        INTEGER          METNROWS  ! met grid number of rows
        INTEGER          NDOTS     ! dot grid number of cells
        INTEGER          NHR       ! no. hour-specific sources for current hour
        INTEGER          NMAJOR    ! no. major sources
        INTEGER          NPING     ! no. plume-in-grid sources
        INTEGER       :: NSTEPS    ! mumber of time steps
        INTEGER          REP_LAYR  ! layer for reporting srcs w/ high plumes
        INTEGER       :: SDATE    ! Julian start date (YYYYDDD)
        INTEGER       :: STIME     ! start time (HHMMSS)
        INTEGER          TSTEP     ! output time step
        INTEGER          IPVERT    ! Number flag for plume vertical spread method ! added (2/05)


        REAL             X, Y, P, Q
        REAL             DM, HT, TK, VE, FL  ! tmp stack parameters
        REAL             XBEG, XEND, XL  ! tmp x-coords
        REAL             YBEG, YEND, YL  ! tmp y-coords
        REAL             FAC       !  tmp factor for renormalizing
        REAL             PSFC      !  surface pressure (Pa)
        REAL             SURFACE   !  tmp weight at surface
        REAL             TDIFF     !  tmp layer frac diff for renormalizing
        REAL             TSTK      !  temperature at top of stack (K)
        REAL             TSUM      !  tmp layer frac sum for renormalizing
        REAL             WSTK      !  wind speed  at top of stack (m/s)
        REAL             ZZ0, ZZ1, ZF0, ZF1
        REAL             ZBOT      !  plume bottom elevation (m)
        REAL             ZTOP      !  plume top    elevation (m)
        REAL             ZPLM      !  plume height above stack
! added (02/05)
        REAL             USTMP     !  temp storage for ustar (m/s)
		

        REAL(8)          METXORIG  ! cross grid X-coord origin of met grid 
        REAL(8)          METYORIG  ! cross grid Y-coord origin of met grid
        REAL(8)          XCELLDG   ! dot grid X-coordinate cell dimension
        REAL(8)          YCELLDG   ! dot grid Y-coordinate cell dimension
        REAL(8)          XORIGDG   ! dot grid X-coordinate origin of grid 
        REAL(8)          YORIGDG   ! dot grid Y-coordinate origin of grid

        LOGICAL       :: BFLAG   ! true: use plume bottom and top
        LOGICAL       :: CFLAG   ! true: recalc vel w/ flow & diam
        LOGICAL     :: COMPUTE   ! true: compute plume rise 
        LOGICAL       :: EFLAG   ! error flag
        LOGICAL       :: FFLAG  ! true: use hourly flow rate
        LOGICAL       :: HFLAG  ! true: hourly input used
        LOGICAL       :: IFLAG   ! true: hr data okay for timestep
        LOGICAL       :: LFLAG   ! true: use hourly layer 1 fraction
        LOGICAL       :: PFLAG   ! true: compute plm ris for iteration
        LOGICAL       :: TFLAG   ! true: use hourly temperatures
        LOGICAL       :: VFLAG   ! true: use elevated file (PELV)
        LOGICAL       :: XFLAG   ! true: process ONLY explicit sources
        LOGICAL       :: YFLAG   ! true: use hourly velocities
        LOGICAL       :: ZSTATIC  ! true: Get heights from GRID_CRO file
        LOGICAL          LFG( 9 )          ! true: source characteristic is valid

        CHARACTER(50)    CHARS( 9 )!  tmp source characeristics 
        CHARACTER(50) :: METSCEN   !  temporary string for met scenario name
        CHARACTER(50) :: CLOUDSHM  !  temporary string for cloud scheme name
        CHARACTER(80) :: GDESC     !  grid description
        CHARACTER(256)   OUTFMT    !  output format for RDEV report
        CHARACTER(256)   BUFFER    !  source characteristics buffer
        CHARACTER(256)   MESG      !  buffer for M3EXIT() messages

        CHARACTER(IOVLEN3) VNAME      ! variable name buffer 
        CHARACTER(IOVLEN3) COORD3D    ! coordinate system name
        CHARACTER(IOVLEN3) COORUN3D   ! coordinate system projection units
        CHARACTER(IODLEN3) IFDESC2, IFDESC3 ! fields 2 & 3 from PNTS FDESC




       END MODULE MOD_LAYPOINT
