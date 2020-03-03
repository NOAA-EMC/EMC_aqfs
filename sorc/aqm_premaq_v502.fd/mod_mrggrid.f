       MODULE MOD_MRGGRID
C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'

C...........   LOCAL VARIABLES and their descriptions:

C...........   Emissions arrays
        REAL, ALLOCATABLE :: E2D ( :,: )   ! 2-d emissions
        REAL, ALLOCATABLE :: EOUT( :,: ) ! output emissions

C...........   Input file descriptors
        INTEGER,       ALLOCATABLE :: DURATA( : ) ! no. time steps
        INTEGER,       ALLOCATABLE :: NCOLSA( : ) ! no. columns
        INTEGER,       ALLOCATABLE :: NROWSA( : ) ! no. rows
        INTEGER,       ALLOCATABLE :: NVARSA( : ) ! no. variables
        INTEGER,       ALLOCATABLE :: SDATEA( : ) ! start date
        INTEGER,       ALLOCATABLE :: STIMEA( : ) ! start time
        INTEGER,       ALLOCATABLE :: NLAYSA( : ) ! number of layers in the file
        CHARACTER(16), ALLOCATABLE :: FNAME ( : ) ! 2-d input file names
        LOGICAL,       ALLOCATABLE :: USEFIRST(:) ! true: use first time step of file

        LOGICAL,       ALLOCATABLE :: LVOUTA( :,: ) ! iff out var in input file
        CHARACTER(16), ALLOCATABLE :: VNAMEA( :,: ) ! variable names
        CHARACTER(16), ALLOCATABLE :: VUNITA( :,: ) ! variable units
        CHARACTER(80), ALLOCATABLE :: VDESCA( :,: ) ! var descrip
        CHARACTER(16)                 VNAMEP( MXVARS3 ) ! pt variable names
        CHARACTER(16)                 VUNITP( MXVARS3 ) ! pt variable units
        CHARACTER(80)                 VDESCP( MXVARS3 ) ! pt var descrip

C...........   Intermediate output variable arrays
        INTEGER       INDXN ( MXVARS3 ) ! sorting index for OUTIDX
        INTEGER       OUTIDX( MXVARS3 ) ! index to master model species list

        CHARACTER(16) OUTNAM( MXVARS3 ) ! unsorted output variable names
        CHARACTER(16) VUNITU( MXVARS3 ) ! unsorted output variable units
        CHARACTER(80) VDESCU( MXVARS3 ) ! unsorted output variable descriptions

        LOGICAL       LVOUTP( MXVARS3 ) ! iff output var exists in point input

C...........   Logical names and unit numbers

        INTEGER       IDEV            ! unit for logical names list for 2d files
        INTEGER       LDEV            ! unit for log file
        INTEGER       RDEV            ! unit for merge report file
        CHARACTER(16) ONAME           ! Merged output file name
        CHARACTER(16) PNAME           ! Point source input file name 

C...........   Other local variables 
!        INTEGER       C, F, J, K, L, L1, L2, NL, V, T ! pointers and counters
        INTEGER       C, J, K, L, L1, L2, NL, T ! pointers and counters
	
        INTEGER       DUMMY                      ! dummy value for use with I/O API functions
        INTEGER       EDATE                      ! ending julian date
        INTEGER       ETIME                      ! ending time HHMMSS
!        INTEGER    :: G_SDATE = 0                ! start date from environment
!        INTEGER    :: G_STIME = 0                ! start time from environment
        INTEGER    :: G_NSTEPS                   ! number of time steps from environment
!        INTEGER    :: G_TSTEP = 0                ! time step from environment


        INTEGER    :: G_SDATE                 ! start date from environment
        INTEGER    :: G_STIME                 ! start time from environment
        INTEGER    :: G_TSTEP                 ! time step from environment
	
		
        INTEGER       IOS                        ! i/o status
        INTEGER       IREC                       ! line number count
        INTEGER       JDATE                      ! iterative julian date
        INTEGER       JTIME                      ! iterative time HHMMSS
        INTEGER       LB                         ! leading blanks counter
        INTEGER       LE                         ! location of end of string
        INTEGER       MXNFIL                     ! max no. of 2-d input files
        INTEGER       NFILE                      ! no. of 2-d input files
        INTEGER       NSTEPS                     ! no. of output time steps
        INTEGER       NVOUT                      ! no. of output variables
        INTEGER       RDATE                      ! reference date
        INTEGER       SAVLAYS                    ! number of layers
        INTEGER       SDATE                      ! starting julian date
        INTEGER       SECS                       ! tmp seconds
        INTEGER       SECSMAX                    ! seconds maximum
        INTEGER       SECSMIN                    ! seconds minimum
        INTEGER       STIME                      ! starting time HHMMSS
        INTEGER       STEPS                      ! tmp number of steps
        INTEGER       TIMET                      ! tmp time from seconds
        INTEGER       TSTEP                      ! time step
        INTEGER       VLB                        ! VGLVS3D lower bound 

        CHARACTER(16)  FDESC                     ! tmp file description
        CHARACTER(16)  NAM                       ! tmp file name
!        CHARACTER(16)  VNM                       ! tmp variable name
        CHARACTER(256) LINE                      ! input buffer
        CHARACTER(256) MESG                      ! message field
        CHARACTER(15)  RPTCOL                    ! single column in report line
        CHARACTER(300) RPTLINE                   ! line of report file

!        LOGICAL    :: EFLAG   = .FALSE.   ! error flag
!        LOGICAL    :: FIRST3D = .TRUE.    ! true: first 3-d file not yet input
!        LOGICAL    :: LFLAG   = .FALSE.   ! true iff 3-d file input
!        LOGICAL    :: TFLAG   = .FALSE.   ! true: grid didn't match
        LOGICAL       MRGDIFF             ! true: merge files from different days


        LOGICAL    :: EFLAG      ! error flag
        LOGICAL    :: FIRST3D   ! true: first 3-d file not yet input
        LOGICAL    :: LFLAG     ! true iff 3-d file input
        LOGICAL    :: TFLAG      ! true: grid didn't match
	

!        INTEGER, PUBLIC :: NLAYS = 1       ! number of layers

        INTEGER, PUBLIC :: NLAYS        ! number of layers
	
        END MODULE MOD_MRGGRID
