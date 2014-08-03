
        SUBROUTINE WRREPOUT( FDEV, RCNT, NDATA, JDATE, JTIME, 
     &                       LAYER, DELIM, OUTFMT, ZEROFLAG, EFLAG )

C***********************************************************************
C  subroutine body starts at line 
C
C  DESCRIPTION:
C    The WRREPOUT routine outputs the report information, depending on the
C    specifications of the output reports and the contents of the bins.C    
C
C  PRECONDITIONS REQUIRED:
C    The output file FDEV is opened
C    The column widths and internal write formats have been created
C    The output delimeter has been specified,
C    The report count RCNT is set
C    The bins have been populated with characteristics and data values
C    The number of data values NDATA has been set
C    The output date string has been set, if needed
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C     Created 7/2000 by M Houyoux
C     Revised 7/2003 by A. Holland
C
C***********************************************************************
C  
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: wrrepout.f,v 1.17 2005/10/21 13:45:45 cseppan Exp $
C  
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C 
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C 
C smoke@unc.edu
C  
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/emqa/wrrepout.f,v $
C Last updated: $Date: 2005/10/21 13:45:45 $ 
C  
C***********************************************************************

C.........  MODULES for public variables
C...........   This module is the inventory arrays
        USE MODSOURC, ONLY: CPDESC, CSOURC, STKHT, STKDM, STKTK, STKVE,
     &                      XLOCA, YLOCA

C.........  This module contains the lists of unique source characteristics
        USE MODLISTS, ONLY: SCCDESC, SCCDLEV, SICDESC, MACTDESC, 
     &                      NAICSDESC

C.........  This module contains Smkreport-specific settings
        USE MODREPRT, ONLY: RPT_, LREGION, VARWIDTH,
     &                      DATEFMT, DATEWIDTH, HOURFMT, HOURWIDTH,
     &                      LAYRFMT, LAYRWIDTH, CELLFMT, CELLWIDTH,
     &                      SRCFMT, SRCWIDTH, REGNFMT, REGNWIDTH,
     &                      CYWIDTH, STWIDTH, COWIDTH, SCCWIDTH,
     &                      SRG1FMT, SRG1WIDTH, SRG2FMT, SRG2WIDTH,
     &                      MONFMT, MONWIDTH, WEKFMT, WEKWIDTH,
     &                      DIUFMT, DIUWIDTH, CHARFMT, CHARWIDTH,
     &                      STKPFMT, STKPWIDTH, ELEVWIDTH,
     &                      PDSCWIDTH, SDSCWIDTH, SPCWIDTH, MINC,
     &                      LOC_BEGP, LOC_ENDP, OUTDNAM, OUTUNIT,
     &                      ALLRPT, SICFMT, SICWIDTH, SIDSWIDTH,
     &                      MACTWIDTH, MACDSWIDTH, NAIWIDTH,
     &                      NAIDSWIDTH, STYPWIDTH, LTLNFMT,
     &                      LTLNWIDTH

C.........  This module contains report arrays for each output bin
        USE MODREPBN, ONLY: NOUTBINS, BINDATA, BINSCC, BINPLANT,
     &                      BINX, BINY, BINSMKID, BINREGN, 
     &                      BINCOIDX, BINSTIDX, BINCYIDX,
     &                      BINMONID, BINWEKID, BINDIUID,
     &                      BINSRGID1, BINSRGID2, BINSPCID, BINRCL,
     &                      BINELEV, BINSNMIDX, BINBAD, BINSIC, 
     &                      BINSICIDX, BINMACT, BINMACIDX, BINNAICS,
     &                      BINNAIIDX, BINSRCTYP

C.........  This module contains the arrays for state and county summaries
        USE MODSTCY, ONLY: CTRYNAM, STATNAM, CNTYNAM

C.........  This module contains the information about the source category
        USE MODINFO, ONLY: MXCHRS, NCHARS

        IMPLICIT NONE

C...........   INCLUDES
        INCLUDE 'EMCNST3.EXT'   !  emissions constant parameters

C...........  EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER(2)  CRLF
        EXTERNAL   CRLF

C...........   SUBROUTINE ARGUMENTS
        INTEGER     , INTENT (IN) :: FDEV
        INTEGER     , INTENT (IN) :: RCNT
        INTEGER     , INTENT (IN) :: NDATA
        INTEGER     , INTENT (IN) :: JDATE
        INTEGER     , INTENT (IN) :: JTIME
        INTEGER     , INTENT (IN) :: LAYER    ! layer number for output
        CHARACTER(*), INTENT (IN) :: DELIM
        CHARACTER(*), INTENT (IN) :: OUTFMT
        LOGICAL     , INTENT (IN) :: ZEROFLAG
        LOGICAL     , INTENT(OUT) :: EFLAG

C...........   Local parameters
        INTEGER, PARAMETER :: STRLEN = 2000   ! Maximum info string length

C...........   Arrays for source characteristics output formatting
        CHARACTER(300) CHARS ( MXCHRS ) !  source fields for output

        LOGICAL, ALLOCATABLE, SAVE :: LF ( : ) ! true if column should be output

C...........   Other local variables
        INTEGER     I, J, K, L, L1, S, V           ! counters and indices

        INTEGER     DAY                       ! day of month
        INTEGER     IOS                       ! i/o status
        INTEGER     LE                        ! output string accum. length
        INTEGER     LV                        ! width of delimiter
        INTEGER     LX                        ! extra space for first column
        INTEGER     MONTH                     ! month number
        INTEGER     MXLE                      ! max value of LE
        INTEGER     NC                        ! no. source char fields
        INTEGER     OUTHOUR                   ! output hour
        INTEGER     YEAR                      ! 4-digit year
        INTEGER     STIDX                     ! starting index of loop
        INTEGER     EDIDX                     ! ending index of loop

        INTEGER, SAVE :: PRCNT = 0

        LOGICAL, SAVE :: FIRSTIME  = .TRUE.   ! true: first time routine called

        REAL        ECHECK                    ! tmp sum of emissions in a bin

        CHARACTER(12)       OUTDATE           !  output date string
        CHARACTER(100)   :: BADRGNM = 'Name unknown'
        CHARACTER(100)      BUFFER            !  string building buffer
        CHARACTER(300)      MESG              !  message buffer
        CHARACTER(STRLEN)   STRING            !  output string

        CHARACTER(16) :: PROGNAME = 'WRREPOUT' ! program name

C***********************************************************************
C   begin body of subroutine WRREPOUT

C.........  Create hour for output
        OUTHOUR = JTIME / 10000 + 1

C.........  When a new report is starting...
        IF( RCNT .NE. PRCNT ) THEN

C.............  Transfer array info to scalar info for this report
            RPT_ = ALLRPT( RCNT )  ! multi-value 

            LREGION = ( RPT_%BYCNRY .OR. RPT_%BYSTAT .OR. RPT_%BYCNTY )

C.............  Allocate memory for LF if not available already
            IF( .NOT. ALLOCATED( LF ) ) THEN
                ALLOCATE( LF( MXCHRS ), STAT=IOS )
                CALL CHECKMEM( IOS, 'LF', PROGNAME )
            END IF

C.............  Initialize output status of source characteristics
            LF = .FALSE.    ! array

C.............  Width of delimeter
            LV = LEN_TRIM( DELIM )

C.............  Update logical source-characteristics fields
C.............  In future, there can be different cases here for "BY STACK", for
C               example
            IF( RPT_%BYSRC ) THEN
                LF( 1:NCHARS ) = .TRUE.
            END IF

        END IF

C.........  Loop through variables for the database format
        DO V = 1, NDATA

C.........  Loop through entries for all bins for current date and hour
            DO I = 1, NOUTBINS

C.............  Check for zero emissions if flag is not set
                IF( .NOT. ZEROFLAG ) THEN
                    ECHECK = SUM( BINDATA( I,1:NDATA ) )
                    IF ( ECHECK .EQ. 0. ) CYCLE
                END IF

C.............  Build tmp string based on date, hour, and other columns that
c               are included in the output file.  Whether these are included
c               is determined by the report settings.
                MXLE   = 1
                STRING = ' '
                LE     = 1
                LX     = 1

C.............  Include variable in string
                IF( RPT_%RPTMODE .EQ. 3 ) THEN

                    L = VARWIDTH
                    L1 = L - LV
                    STRING = STRING( 1:LE ) //
     &                       OUTDNAM( V, RCNT )( 1:L1 ) // DELIM
                    MXLE = MXLE + L
                    LE = MIN( MXLE, STRLEN )

                END IF

C..............  Include user-defined label in string
                IF( RPT_%USELABEL ) THEN

                    STRING = STRING( 1:LE )// TRIM( RPT_%LABEL )// DELIM
                    MXLE = MXLE + LEN_TRIM( RPT_%LABEL ) + LX + LV
                    LE = MIN( MXLE, STRLEN )
                    LX = 0

                END IF

C.............  Include date in string
                IF( RPT_%BYDATE ) THEN

C.................  Get month and day from Julian date
                    CALL DAYMON( JDATE, MONTH, DAY )

C.................  Compute year
                    YEAR = JDATE / 1000

C.................  Add date field to header
                    OUTDATE = ' '
                    WRITE( OUTDATE, DATEFMT ) MONTH, DAY, YEAR

                    STRING = STRING( 1:LE ) // OUTDATE
                    MXLE = MXLE + DATEWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0

                END IF

C.............  Include hour in string
                IF( RPT_%BYHOUR ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, HOURFMT ) OUTHOUR  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + HOURWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include layer in string
                IF( RPT_%BYLAYER ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, LAYRFMT ) LAYER    ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + LAYRWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include cell numbers in string
                IF( RPT_%BYCELL ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, CELLFMT ) BINX( I ), BINY( I )  ! Integers
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + CELLWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include source number in string
                IF( RPT_%BYSRC ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, SRCFMT ) BINSMKID( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + SRCWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include country/state/county code in string
                IF( LREGION ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, REGNFMT ) BINREGN( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + REGNWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF


C.............  Include country name in string
                IF( RPT_%BYCONAM ) THEN
                    J = BINCOIDX( I )
                    L = COWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    IF( J .LE. 0 ) THEN
                        STRING = STRING( 1:LE ) // 
     &                           BADRGNM( 1:L1 ) // DELIM
                    ELSE
                        STRING = STRING( 1:LE ) // 
     &                           CTRYNAM( J )( 1:L1 ) // DELIM
                    END IF

                    MXLE = MXLE + L
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include state name in string
                IF( RPT_%BYSTNAM ) THEN
                    J = BINSTIDX( I )
                    L = STWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    IF( J .LE. 0 ) THEN
                        STRING = STRING( 1:LE ) // 
     &                           BADRGNM( 1:L1 ) // DELIM
                    ELSE
                        STRING = STRING( 1:LE ) // 
     &                           STATNAM( J )( 1:L1 ) // DELIM
                    END IF
                    MXLE = MXLE + L
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include county name in string
                IF( RPT_%BYCYNAM ) THEN
                    J = BINCYIDX( I )
                    L = CYWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    IF( J .LE. 0 ) THEN
                        STRING = STRING( 1:LE ) // 
     &                           BADRGNM( 1:L1 ) // DELIM
                    ELSE
                        STRING = STRING( 1:LE ) // 
     &                           CNTYNAM( J )( 1:L1 ) // DELIM
                    END IF
                    MXLE = MXLE + L
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include SCC code in string
                IF( RPT_%BYSCC ) THEN
                    L = SCCWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       BINSCC( I )( 1:L1 ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include SIC code in string
                IF( RPT_%BYSIC ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, SICFMT ) BINSIC( I )    ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + SICWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0

                END IF

C.............  Include MACT code in string
                IF( RPT_%BYMACT ) THEN
                    L = MACTWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       BINMACT( I )( 1:L1 ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include NAICS code in string
                IF( RPT_%BYNAICS ) THEN
                    L = NAIWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       BINNAICS( I )( 1:L1 ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include SRCTYP code in string
                IF( RPT_%BYSRCTYP ) THEN
                    L = STYPWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE+4 ) // 
     &                    BINSRCTYP( I )( 1:L1-4 ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include primary surrogate code
                IF( RPT_%SRGRES .EQ. 1 ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, SRG1FMT ) BINSRGID1( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + SRG1WIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include fallback surrogate code
                IF( RPT_%SRGRES .GE. 1 ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, SRG2FMT ) BINSRGID2( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + SRG2WIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include monthly temporal profile
                IF( RPT_%BYMON ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, MONFMT ) BINMONID( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + MONWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include weekly temporal profile
                IF( RPT_%BYWEK ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, WEKFMT ) BINWEKID( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + WEKWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include diurnal temporal profile
                IF( RPT_%BYDIU ) THEN
                    BUFFER = ' '
                    WRITE( BUFFER, DIUFMT ) BINDIUID( I )  ! Integer
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + DIUWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include speciation profile
                IF( RPT_%BYSPC ) THEN
                    L = SPCWIDTH
                    L1 = L - LV - 1 - SPNLEN3                  ! 1 for space                
                    STRING = STRING( 1:LE ) // ' ' //
     &                       BINSPCID( I )// BLANK16( 1:L1 )// DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include plant ID
                IF( RPT_%BYPLANT ) THEN
                    L = CHARWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) //
     &                       BINPLANT( I )( 1:L1 ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include road class code
                IF( RPT_%BYRCL ) THEN

C.................  Write characteristics
                    BUFFER = ' '
                    WRITE( BUFFER, CHARFMT ) BINRCL( I )
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + CHARWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include source characteristics
                IF( RPT_%BYSRC ) THEN
                    S = BINSMKID( I ) 

C.................  Disaggregate source characteristics
                    CALL PARSCSRC( CSOURC( S ), MXCHRS, LOC_BEGP,
     &                             LOC_ENDP, LF, NC, CHARS )

C.................  Write characteristics
                    BUFFER = ' '
                    WRITE( BUFFER, CHARFMT )( CHARS( K ), K = MINC, NC )
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + CHARWIDTH + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include stack parameters
                IF( RPT_%STKPARM ) THEN
                    S = BINSMKID( I ) 
                    BUFFER = ' '
                    WRITE( BUFFER, STKPFMT ) STKHT( S ), STKDM( S ),
     &                                       STKTK( S ), STKVE( S )
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + STKPWIDTH
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include lat/lons for point sources
                IF( RPT_%LATLON ) THEN
                    S = BINSMKID( I )
                    BUFFER = ' '
                    WRITE( BUFFER, LTLNFMT ) YLOCA( S ), XLOCA( S )
                    STRING = STRING( 1:LE ) // BUFFER
                    MXLE = MXLE + LTLNWIDTH
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include elevated sources flag
                IF( RPT_%BYELEV ) THEN
                    L = ELEVWIDTH
                    L1 = L - LV  - 2      ! 1 for space, minus 1 for how used
                    STRING = STRING( 1:LE ) // 
     &                       BLANK16( 1:L1 ) // BINELEV( I ) // DELIM
                    MXLE = MXLE + L + LX
                    LE = MIN( MXLE, STRLEN )
                    LX = 0
                END IF

C.............  Include plant description (for point sources)
                IF( RPT_%SRCNAM ) THEN
                    S = BINSMKID( I )
                    L = PDSCWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       CPDESC( S )( 1:L1 ) // DELIM
                    MXLE = MXLE + L
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include SCC description
C.............  This is knowingly including extra blanks before final quote
                IF( RPT_%SCCNAM ) THEN
                    J = BINSNMIDX( I ) 
                    L = SDSCWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    K = SCCDLEV( J, RPT_%SCCRES )
                    STRING = STRING( 1:LE ) // 
     &                       SCCDESC( J )( 1:K ) //
     &                       REPEAT( " ", L1-K ) // DELIM
                    MXLE = MXLE + L + 2
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include SIC description
C.............  This is knowingly including extra blanks before final quote
                IF( RPT_%SICNAM ) THEN
                    J = BINSICIDX( I ) 
                    L = SIDSWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       '"'// SICDESC( J )( 1:L1 )// '"' // DELIM
                    MXLE = MXLE + L + 2
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include MACT description
C.............  This is knowingly including extra blanks before final quote
                IF( RPT_%MACTNAM ) THEN
                    J = BINMACIDX( I ) 
                    L = MACDSWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       '"'// MACTDESC( J )( 1:L1 )// '"' // DELIM
                    MXLE = MXLE + L + 2
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Include NAICS description
C.............  This is knowingly including extra blanks before final quote
                IF( RPT_%NAICSNAM ) THEN
                    J = BINNAIIDX( I ) 
                    L = NAIDSWIDTH
                    L1 = L - LV - 1                        ! 1 for space
                    STRING = STRING( 1:LE ) // 
     &                       '"'// NAICSDESC( J )( 1:L1 )// '"' // DELIM
                    MXLE = MXLE + L + 2
                    LE = MIN( MXLE, STRLEN )
                END IF

C.............  Remove leading spaces and get new length
                STRING = STRING( 2:LE )
                LE = LE - 1

C.............  Output error message of string is getting shortened
                IF( MXLE .GT. STRLEN ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG,94010 ) 'INTERNAL ERROR: Output ' //
     &                     'string getting truncated in report', RCNT
                    CALL M3MSG2( MESG )
                END IF

C.............  If current bin has bad values, update those now.
                IF( BINBAD( I ) .GT. 0 ) BINDATA( I,1:NDATA ) = -9.

C.............  Write out this record
                IF( RPT_%NUMFILES .EQ. 1 ) THEN

                    IF( RPT_%RPTMODE .EQ. 2 ) THEN

                        IF( FIRSTIME ) THEN
                            STIDX = 1
                            EDIDX = RPT_%RPTNVAR
                            FIRSTIME = .FALSE.

                        ELSE
                            IF( EDIDX + RPT_%RPTNVAR .GT.
     &                                     NDATA ) THEN

                                STIDX = EDIDX + 1
                                EDIDX = NDATA

                            ELSE
                                STIDX = EDIDX + 1
                                EDIDX = EDIDX + RPT_%RPTNVAR

                            END IF

                        END IF

                    ELSE IF( RPT_%RPTMODE .EQ. 1 ) THEN

                        STIDX = 1
                        EDIDX = NDATA

                    ELSE IF( RPT_%RPTMODE .EQ. 3 ) THEN

                        STIDX = 1
                        EDIDX = 1

                    ELSE

                        STIDX = 1
                        EDIDX = NDATA

                    END IF

                ELSE
                    IF( FIRSTIME ) THEN
                        STIDX = 1
                        EDIDX = RPT_%RPTNVAR
                        FIRSTIME = .FALSE.

                    ELSE
                        IF( EDIDX + RPT_%RPTNVAR .GT.
     &                                     NDATA ) THEN

                            STIDX = EDIDX + 1
                            EDIDX = NDATA

                        ELSE
                            STIDX = EDIDX + 1
                            EDIDX = EDIDX + RPT_%RPTNVAR

                        END IF

                    END IF

                END IF

                IF( RPT_%RPTMODE .NE. 3 ) THEN

                    WRITE( FDEV, OUTFMT ) STRING( 1:LE ), 
     &                              ( BINDATA( I,J ), J=STIDX, EDIDX )

                ELSE

                    WRITE( FDEV, OUTFMT ) STRING( 1:LE ), BINDATA(I,V),
     &                                DELIM, OUTUNIT( V )

                END IF

            END DO  ! End loop through bins

            IF( RPT_%RPTMODE .NE. 3 ) GOTO 777

        END DO   ! End loop through variables

C.........  Save report number for next time routine is called
777     PRCNT = RCNT

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT( A )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I10, :, 1X ) )

94180   FORMAT( I2.2, '/', I2.2, '/', I4.4 )

        END SUBROUTINE WRREPOUT






