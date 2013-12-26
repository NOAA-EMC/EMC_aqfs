       MODULE MOD_TEMPORAL
       
       

C.........  INCLUDES:
        INCLUDE 'EMCNST3.EXT'   !  emissions constant parameters
        INCLUDE 'PARMS3.EXT'    !  i/o api parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'FDESC3.EXT'    !  I/O API file description data structures.
!        INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions
C.........  LOCAL PARAMETERS and their descriptions:

        CHARACTER(50), PARAMETER ::CVSW = '$Name: SMOKE_v2_1_09302004 $'  ! CVS revision tag

       

C.........  Emission arrays
        REAL   , ALLOCATABLE :: EMAC ( :,: ) !  inven emissions or activities
        REAL   , ALLOCATABLE :: EMACV( :,: ) !  day-adjst emis or activities
        REAL   , ALLOCATABLE :: EMIST( :,: ) !  timestepped output emssions

C.........  Emission factor arrays        
        CHARACTER(256), ALLOCATABLE :: EFLIST( : )  ! listing of emission factor file names
        CHARACTER(16) , ALLOCATABLE :: EFLOGS( : )  ! listing of ef logical file names
        INTEGER       , ALLOCATABLE :: EFDAYS( :,: )! ef file by day for each time period
        REAL          , ALLOCATABLE :: EMFAC ( :,: )! mobile emission factors by source
        REAL          , ALLOCATABLE :: TEMPEF( : )  ! temporary holding array for efs
        CHARACTER     , ALLOCATABLE :: EFTYPE( : )  ! ef file type (day, week, etc.) for each src
        INTEGER       , ALLOCATABLE :: EFIDX ( : )  ! location of ef in file for each source
        INTEGER       , ALLOCATABLE :: SRCS  ( : )  ! temporary array for sources in each ef file

C.........  Temporal allocation Matrix.  
        REAL, ALLOCATABLE :: TMAT( :, :, : ) ! temporal allocation factors

C.........  Array that contains the names of the inventory variables needed for
C           this program
        CHARACTER(IOVLEN3) IVARNAMS( MXINVARR )

C.........  Actual-SCC  table
        INTEGER                            NSCC
        CHARACTER(SCCLEN3), ALLOCATABLE :: SCCLIST( : )

C.........  Day-specific, hour-specific data, and elevated sources data. 
C.........  These need only to allow enough dimensions for one read per 
C           pollutant per time step.

        INTEGER                 NPELV        ! optional elevated source-count
        INTEGER, ALLOCATABLE :: INDXE( : )   ! SMOKE source IDs
        REAL   , ALLOCATABLE :: EMISE( : )   ! elevated source emissions

C...........   Ungridding Matrix
        INTEGER, ALLOCATABLE :: UMAT( : )   ! contiguous ungridding matrix

C.........  Names of pollutants and activities associated with output variables
        CHARACTER(IOVLEN3), ALLOCATABLE:: ALLIN( : ) 

C.........  Reshaped input variables and output variables
        INTEGER         NGRP                ! no. of pol/emis-types groups 
        INTEGER         NGSZ                ! no. of pols/emis-types per group 
        CHARACTER(IOVLEN3), ALLOCATABLE:: ALLIN2D( :,: ) 
        CHARACTER(IOVLEN3), ALLOCATABLE:: EANAM2D( :,: ) 

C...........   Logical names and unit numbers

!        INTEGER      :: CDEV = 0!  unit number for region codes file (see modmerge)
        INTEGER      :: EDEV = 0!  unit number for ef file list
        INTEGER      :: HDEV = 0!  unit number for holidays file
        INTEGER         LDEV    !  unit number for log file
        INTEGER      :: MDEV = 0!  unit number for mobile codes file
        INTEGER         PDEV    !  unit number for supplemental tmprl file
        INTEGER         RDEV    !  unit number for temporal profile file
!        INTEGER         SDEV    !  unit number for ASCII inventory file   see mod_laypoint
        INTEGER         TDEV    !  unit number for emission processes file
        INTEGER         XDEV    !  unit no. for cross-reference file
        INTEGER         VDEV    !  unit no. for inventory data table

!        CHARACTER(16) :: ANAME = ' '    !  logical name for ASCII inven input see mod_laypoint
        CHARACTER(16) :: GNAME = ' '    !  ungridding matrix
        CHARACTER(16) :: DNAME = 'NONE' !  day-specific  input file, or "NONE"
!        CHARACTER(16) :: ENAME = ' '    !  logical name for I/O API inven input   see mod_laypoint
        CHARACTER(16) :: FNAME = ' '    !  emission factors file
        CHARACTER(16) :: HNAME = 'NONE' !  hour-specific input file, or "NONE"
        CHARACTER(16) :: TNAME = ' '    !  timestepped (low-level) output file
        CHARACTER(16) :: TMPNAME = ' '  !  temporary inventory logical name

C...........   Other local variables

        INTEGER         J, K, L, L1, L2, N, S

        INTEGER         IOS, IOS1, IOS2, IOS3, IOS4 ! i/o status
        INTEGER         IOS6, IOS7, IOS8, IOS9      ! i/o status
        INTEGER         AVERTYPE            ! time period averaging type
        INTEGER         DYSTPOS, DYENDPOS   ! start and end position in file name string
        INTEGER         EARLYDATE           ! earliest starting date based on time zones
        INTEGER         EARLYTIME           ! earliest starting time
        INTEGER         EDATE, ETIME        ! ending Julian date and time
        INTEGER         EFSDATE, EFEDATE    ! start and end date of current ef file
        INTEGER         ENLEN               ! length of ENAME string
        INTEGER         ENDPOS              ! ending position in ef day array
        INTEGER         FIRSTPOS            ! temporary position in file name string
        INTEGER         FDATE, FTIME        ! emission factor date and time
        INTEGER         HYPPOS              ! position of hyphen in file name string
        INTEGER         JDATE, JTIME        ! Julian date and time
        INTEGER         LATEDATE            ! latest ending date based on time zones
        INTEGER         LATETIME            ! latest ending time
        INTEGER         NDAYS               ! no. days in episode
        INTEGER         NINVARR             ! no. inventory variables to read
        INTEGER         NLINES              ! no. lines in ef list file
        INTEGER         NMATX               ! size of ungridding matrix
        INTEGER         NMAJOR              ! no. major sources
        INTEGER         NPING               ! no. ping sources
        INTEGER         NSTEPS              ! number of output time steps
        INTEGER      :: PYEAR = 0           ! projected year
        INTEGER         SDATE, STIME        ! starting Julian date and time
        INTEGER         STPOS               ! starting position in ef day array
        INTEGER         TNLEN               ! length of TNAME string
        INTEGER         TSTEP               ! output time step
        INTEGER         TZONE               ! output-file time zone
        INTEGER         TZMIN               ! minimum time zone in inventory      
        INTEGER         TZMAX               ! maximum time zone in inventory      

        REAL            RTMP                ! tmp float

        LOGICAL      :: DAYLIT    = .FALSE.  ! true: TZONES are in daylight time
        LOGICAL         DFLAG   !  true: day-specific  file available
        LOGICAL      :: EFLAG = .FALSE.  !  error-flag
        LOGICAL      :: EFLAG2= .FALSE.  !  error-flag (2)
        LOGICAL         ENDFLAG !  true: couldn't find file end date
        LOGICAL      :: FNDOUTPUT = .FALSE.  ! true: found output hydrocarbon
        LOGICAL         HFLAG   !  true: hour-specific file available
        LOGICAL         MFLAG   !  true: mobile codes file available
        LOGICAL         NFLAG   !  true: use all uniform temporal profiles
        LOGICAL         WFLAG   !  true: write QA on current time step
        LOGICAL      :: USETIME( 4 ) = .FALSE. ! true: time period is used

        CHARACTER(8)         TREFFMT   ! tmprl x-ref format (SOURCE|STANDARD)
        CHARACTER(14)        DTBUF     ! buffer for MMDDYY
        CHARACTER(3)         INTBUF    ! buffer for integer
        CHARACTER(20)        MODELNAM  ! emission factor model name
        CHARACTER(256)       CURFNM    ! current emission factor file name
        CHARACTER(16)        CURLNM    ! current ef logical file name
        CHARACTER(IOVLEN3)   VOLNAM    ! volatile pollutant name
        CHARACTER(300)       MESG      ! buffer for M3EXIT() messages
        CHARACTER(IOVLEN3)   CBUF      ! pollutant name temporary buffer 
        CHARACTER(IOVLEN3)   EBUF      ! pollutant name temporary buffer 
        CHARACTER(20)        SEARCHSTR ! string used in search
        CHARACTER(MXDLEN3)   TEMPLINE  ! line from file description


        CHARACTER(IOVLEN3), ALLOCATABLE, PUBLIC :: PVNAMES ( : )
        CHARACTER(IOULEN3), ALLOCATABLE, PUBLIC :: PVUNITS( : )		
        END MODULE MOD_TEMPORAL
