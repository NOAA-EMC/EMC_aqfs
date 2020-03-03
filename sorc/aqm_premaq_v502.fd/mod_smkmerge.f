       MODULE MOD_SMKMERGE

C...........   INCLUDES:
        
        INCLUDE 'EMCNST3.EXT'   !  emissions constant parameters
        INCLUDE 'PARMS3.EXT'    !  I/O API parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'FDESC3.EXT'    !  I/O API file desc. data structures




C...........   LOCAL VARIABLES and their descriptions:

C...........   Local temporary array for input and output variable names
        CHARACTER(IOVLEN3), ALLOCATABLE :: VARNAMES( : )
        CHARACTER(IOVLEN3), ALLOCATABLE :: INNAMES ( : )

C...........   Local allocatable arrays for creating list of all explicit srcs
        INTEGER, ALLOCATABLE :: TMPSRC( : )
        INTEGER, ALLOCATABLE :: TMPIDX( : )
        LOGICAL, ALLOCATABLE :: SRCFLG( : )

C...........    Local variables for array sizes     
        INTEGER         APOLSIZ ! work area inventory emissions array size
        INTEGER         MPOLSIZ ! work mobile inventory emissions array size
        INTEGER         PPOLSIZ ! work point inventory emissions array size
        INTEGER         ASPCSIZ ! work area speciation matrix array size
        INTEGER         MSPCSIZ ! work mobile speciation matrix array size
        INTEGER         PSPCSIZ ! work point speciation matrix array size
        INTEGER         AMULSIZ ! work area multipl control matrix array size
        INTEGER         MMULSIZ ! work mobile multipl control matrix array size
        INTEGER         PMULSIZ ! work point multipl control matrix array size

C...........   Logical names and unit numbers (not in MODMERGE)
        INTEGER         LDEV
     
C...........   Other local variables


        INTEGER          AJDATE        ! area-source Julian date for by-day
        INTEGER          DAY           ! day-of-week index (monday=1)
        INTEGER       :: IDUM = 0      ! dummy integer value
        INTEGER          IDUM1, IDUM2
        INTEGER          IOS           ! tmp I/O status
        INTEGER          JDATE         ! Julian date (YYYYDDD)
        INTEGER          JTIME         ! time (HHMMSS)
        INTEGER       :: K1 = 0        ! tmp index for valid ar spc matrix
        INTEGER       :: K2 = 0        ! tmp index for valid mb spc matrix
        INTEGER       :: K3 = 0        ! tmp index for valid pt spc matrix
        INTEGER       :: K4 = 0        ! tmp index for valid ar reactvty matrix
        INTEGER       :: K5 = 0        ! tmp index for valid mb reactvty matrix
        INTEGER          KA, KB, KM, KP! tmp index to src-category species
        INTEGER          LDATE         ! Julian date from previous iteration
        INTEGER          MJDATE        ! mobile-source Julian date for by-day
        INTEGER          MXGRP         ! max no. of variable groups
        INTEGER          MXVARPGP      ! max no. of variables per group
        INTEGER          NGRP          ! actual no. of pollutant groups
        INTEGER          NMAJOR        ! no. elevated sources
        INTEGER          NPING         ! no. plum-in-grid sources
        INTEGER          NVPGP         ! tmp actual no. variables per group
        INTEGER          OCNT          ! tmp count output variable names
        INTEGER       :: PDAY = 0      ! previous iteration day no.
        INTEGER          PGID          ! previous iteration group ID no.
        INTEGER          PJDATE        ! point-source Julian date for by-day
        INTEGER      :: SRGNROWS = 0   ! no. rows in surrogates file
        INTEGER      :: SRGNCOLS = 0   ! no. cols in surrogates file


        INTEGER          N             ! loop variable needed in both pre_smkmerge and smkmerge


        REAL          :: RDUM = 0      ! dummy real value
        REAL             RDUM1, RDUM2, RDUM3, RDUM4, RDUM5, RDUM6
        REAL             F1, F2, FB    ! tmp conversion factors

        CHARACTER(16)      SRGFMT           ! gridding surrogates format
        CHARACTER(16)   :: SRGGRDNM  = ' '  !  surrogates file grid name
        CHARACTER(300)     MESG    ! message buffer
        CHARACTER(IOVLEN3) LBUF    ! previous species or pollutant name
        CHARACTER(IOVLEN3) PBUF    ! tmp pollutant or emission type name
        CHARACTER(IOVLEN3) SBUF    ! tmp species or pollutant name
        CHARACTER(PLSLEN3) VBUF    ! pol to species or pol description buffer
	       
	END MODULE MOD_SMKMERGE
