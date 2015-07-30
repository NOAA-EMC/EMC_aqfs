
        SUBROUTINE PRE_SMKMERGE (premaq_sdate, premaq_stime)

C***********************************************************************
C  program SMKMERGE body starts at line 148
C
C  DESCRIPTION:
C      The purpose of this program is to merge the inventory or hourly
C      emissions files from the Temporal program with gridding matrices and 
C      with optionally any combination of speciation matrices and 3 control
C      matrices (different types).  The program can operate on from 1 to 4 
C      source categories (area, biogenic, mobile, or point sources), or any 
C      combination of these.  If a layer fractions file is input, then the 
C      output file is 3-d.  This program is not used for the MPS/MEPSE files 
C      for CMAQ.
C
C  PRECONDITIONS REQUIRED:  
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Copied from csgldaymrg.F version 1.7 by M Houyoux 2/99
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: smkmerge.f,v 1.26 2004/06/21 17:30:31 cseppan Exp $
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
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/smkmerge/smkmerge.f,v $
C Last updated: $Date: 2004/06/21 17:30:31 $ 
C
C****************************************************************************

C.........  MODULES for public variables
C.........  This module contains the major data structure and control flags
        USE MODMERGE, ONLY: 
     &          AFLAG, BFLAG, MFLAG, PFLAG,                     ! source flags
     &          AUFLAG, MUFLAG, PUFLAG,                         ! mult control flags
     &          ARFLAG, MRFLAG, PRFLAG,                         ! reac control flags
     &          APRJFLAG, MPRJFLAG, PPRJFLAG,                   ! growth flags
     &          AFLAG_BD, MFLAG_BD, PFLAG_BD,                   ! by-day hourly emis flags
     &          TFLAG, SFLAG, LFLAG,                            ! use temporal, spec, layers
     &          PINGFLAG, ELEVFLAG, EXPLFLAG,                   ! ping, elevated, expl. plume
     &          LMKTPON, LREPANY,                               ! mkt penetration, any reports
     &          CDEV, EDEV, GDEV,                               ! costcy, elev/ping, grid surg
     &          AENAME, ATNAME, AGNAME, ASNAME, ARNAME, AUNAME, ! area files
     &          BTNAME,                                         ! biogenic files
     &          MENAME, MTNAME, MGNAME, MSNAME, MRNAME, MUNAME, ! mobile files
     &          PENAME, PTNAME, PGNAME, PSNAME, PRNAME, PUNAME, ! point files
     &          PLNAME, PVNAME, PHNAME,
     &          NASRC, NMSRC, NPSRC, EMLAYS,                    ! no. of srcs, no. emis layers
     &          ANMSPC, BNMSPC, MNMSPC, PNMSPC, NMSPC,          ! no. species
     &          ANGMAT, MNGMAT,                                 ! no. gridding matrix entries
     &          ANSREAC, MNSREAC, PNSREAC,                      ! no. src w/ reac controls
     &          ARNMSPC, MRNMSPC, PRNMSPC,                      ! no. reac species
     &          AEMNAM, BEMNAM, MEMNAM, PEMNAM, EMNAM,          ! species names
     &          ANMAP, AMAPNAM, AMAPFIL,                        ! area map file
     &          MNMAP, MMAPNAM, MMAPFIL,                        ! mobile map file
     &          PNMAP, PMAPNAM, PMAPFIL,                        ! point map file
     &          VGRPCNT, IDVGP, GVNAMES,                        ! group count, ids, var names
     &          SIINDEX, SPINDEX, GVLOUT,                       ! EANAM & EMNAM idx, output pts
     &          A_EXIST, M_EXIST, P_EXIST,                      ! grp indices for inv emis
     &          AU_EXIST, MU_EXIST, PU_EXIST,                   ! grp indices for mult controls
     &          AR_EXIST, MR_EXIST, PR_EXIST,                   ! grp indices for reac controls
     &          AS_EXIST, BS_EXIST, MS_EXIST, PS_EXIST,         ! grp indices for spec matrices
     &          SDATE, STIME, NSTEPS, TSTEP, PVSDATE, PVSTIME,  ! episode information
     &          ASDATE, MSDATE, PSDATE,                         ! dates for by-day hrly emis
     &          BIOGFAC, BIOTFAC, GRDFAC, TOTFAC,               ! conversion factors
     &          AEMSRC, MEMSRC, PEMSRC,                         ! inv or hrly emissions
     &          AEISRC, MEISRC, PEISRC,                         ! inv only emissions
     &          AGMATX, MGMATX, PGMATX,                         ! gridding matrices
     &          ASMATX, MSMATX, PSMATX,                         ! speciation matrices
     &          ARINFO, MRINFO, PRINFO,                         ! reactivity matrices
     &          AEMGRD, BEMGRD, MEMGRD, PEMGRD, TEMGRD,         ! gridded emissions
     &          AEBCNY, BEBCNY, MEBCNY, PEBCNY,                 ! cnty total spec emissions
     &          AEUCNY, MEUCNY, PEUCNY,                         ! cnty total mult control emis
     &          AERCNY, MERCNY, PERCNY,                         ! cnty total reac control emis
     &          AECCNY, MECCNY, PECCNY,                         ! cnty total all-control emis
     &          LFRAC, EANAM, TONAMES                           ! layer frac, pol/act names

C.........  This module contains the control packet data and control matrices
        USE MODCNTRL, ONLY: ACRIDX, ACRREPEM, ACRPRJFC, ACRMKTPN,
     &                      MCRIDX, MCRREPEM, MCRPRJFC, MCRMKTPN, 
     &                      PCRIDX, PCRREPEM, PCRPRJFC, PCRMKTPN,
     &                      ACRFAC, MCRFAC, PCRFAC,
     &                      ACUMATX, MCUMATX, PCUMATX

C.........  This module contains arrays for plume-in-grid and major sources
        USE MODELEV, ONLY: INDXH, NHRSRC, GRPGID, ELEVFLTR, ELEVSRC,
     &                     GROUPID

C.........  This module contains the lists of unique source characteristics
        USE MODLISTS, ONLY: NINVIFIP, INVIFIP

C.........  This module contains the arrays for state and county summaries
        USE MODSTCY, ONLY: NCOUNTY, AICNY, MICNY, PICNY

C...........   This module contains the gridding surrogates tables
        USE MODSURG, ONLY: NSRGFIPS, SRGFIPS

C.........  This module contains the global variables for the 3-d grid
        USE MODGRID, ONLY: NGRID, OFFLAG

        USE MOD_SMKMERGE
	
        IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:
        
        CHARACTER(2)    CRLF
        CHARACTER(10)   HHMMSS
        INTEGER         INDEX1
        INTEGER         WKDAY

        EXTERNAL    CRLF, HHMMSS, INDEX1, WKDAY

C.........  LOCAL PARAMETERS and their descriptions:

        CHARACTER(50), PARAMETER ::CVSW = '$Name: SMOKE_v2_1_09302004 $' ! CVS release tag

C...........   LOCAL VARIABLES and their descriptions:

        integer premaq_sdate, premaq_stime
     
C...........   Other local variables
    
        INTEGER          I, J, K, L1, L2, M, V, S, T ! counters and indices


        CHARACTER(16) :: PROGNAME = 'PRE_SMKMERGE' ! program name

C***********************************************************************
C   begin body of program SMKMERGE
        CALL M3MSG2('BEGINNING of PRE_SMKMERGE')        

!AQF        LDEV = INIT3()

C.........  Write out copywrite, version, web address, header info, and prompt
C           to continue running the program.
!AQF        CALL INITEM( LDEV, CVSW, PROGNAME )

C.........  Retrieve control environment variables and set logical control
C           flags. Use a local module to pass the control flags.
        CALL GETMRGEV

C.........  Open input files and retrieve episode information
        CALL OPENMRGIN( SRGNROWS, SRGNCOLS, SRGGRDNM, SRGFMT,
     &          premaq_sdate, premaq_stime )


C.........  Do setup for biogenic state and county reporting
!AQF        IF( BFLAG .AND. LREPANY ) THEN

C.............  Read gridding surrogates
!AQF            CALL RDSRG( GDEV, SRGFMT, SRGNROWS, SRGNCOLS )

C.........  If output grid is different from surrogates, write message
!AQF            IF ( OFFLAG ) THEN
!AQF                L1 = LEN_TRIM( SRGGRDNM )
!AQF                MESG = 'NOTE: gridding surrogates (for biogenic '//
!AQF     &                 'totals) extracted for output'// CRLF()// 
!AQF     &                 BLANK10 //'grid from grid "' // 
!AQF     &                 SRGGRDNM( 1:L1 ) // '"'
!AQF                CALL M3MSG2( MESG )
!AQF            END IF

!AQF        END IF

C.........  Create arrays of sorted unique pol-to-species
C.........  Create arrays of sorted unique pollutants
C.........  Create arrays of sorted unique species
        CALL MRGVNAMS

C.........  Determine units conversion factors
        CALL MRGUNITS

C.........  Read in any needed source characteristics
        CALL RDMRGINV

	

C.........  Do setup for state and county reporting
C.........  Do this even if there LREPANY is false, in order to allocate
C           memory for the state and county total arrays to ensure
C           MRGMULT will work.

C.........  Read the state and county names file and store for the 
C           states and counties in the grid
C.........  For anthropogenic source categories, use FIPS list
C           from the inventory for limiting state/county list
        IF( AFLAG .OR. MFLAG .OR. PFLAG ) THEN
	
!!!! test if this is needed
!            CALL RDSTCY( CDEV, NINVIFIP, INVIFIP )
!!!!! end test	    

C.........  Otherwise, for biogenic merge only, use list of codes from the 
C           surrogates file needed for state and county totals
        ELSE
            CALL RDSTCY( CDEV, NSRGFIPS, SRGFIPS )

        END IF

C.........  Allocate memory for fixed-size arrays by source category...
        CALL ALLOCMRG( MXGRP, MXVARPGP, AMULSIZ, MMULSIZ, PMULSIZ,
     &                 ASPCSIZ, MSPCSIZ, PSPCSIZ, APOLSIZ, MPOLSIZ,
     &                 PPOLSIZ )


C.........  Read in elevated sources and plume-in-grid information, if needed
C.........  Reset flag for PinG if none in the input file
        IF( PFLAG .AND. ( ELEVFLAG .OR. PINGFLAG ) ) THEN

            CALL RDPELV( EDEV, NPSRC, ELEVFLAG, NMAJOR, NPING )

            IF( ELEVFLAG .AND. NMAJOR .EQ. 0 ) THEN
                MESG = 'WARNING: No sources are major elevated ' //
     &                 'sources in input file, ' // CRLF() // 
     &                 BLANK10 // 'so elevated source emissions ' //
     &                 'file will not be written.'
                CALL M3MSG2( MESG )
                ELEVFLAG = .FALSE.
            ELSE IF ( NMAJOR .EQ. 0 ) THEN
                NMAJOR = NPSRC
            END IF 

            IF( PINGFLAG .AND. NPING .EQ. 0 ) THEN
                MESG = 'WARNING: No sources are PinG sources in ' //
     &                 'input file, so PinG ' // CRLF() // BLANK10 //
     &                 'emissions file will not be written.'
                CALL M3MSG2( MESG )
                PINGFLAG = .FALSE.
            END IF

C.............  Read stack group IDs
            IF ( .NOT. READ3( PVNAME, 'ISTACK', 1,
     &                        PVSDATE, PVSTIME, GRPGID ) ) THEN

                L2 = LEN_TRIM( PVNAME )
                MESG = 'Could not read "ISTACK" from file "' //
     &                 PVNAME( 1:L2 ) // '"'
                CALL M3EXIT( PROGNAME, SDATE, 0, MESG, 2 )

            END IF

C.............  Update elevated sources filter for elevated sources
            DO S = 1, NPSRC
                IF( GROUPID( S ) .GT. 0 ) ELEVFLTR( S ) = 1.
            END DO

        END IF

C.........  Create complete source list for explicit elevated sources
        IF( EXPLFLAG ) THEN

C.............  Allocate memory for temporary unsorted list and index
            ALLOCATE( SRCFLG( NPSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'SRCFLG', PROGNAME )
            ALLOCATE( TMPSRC( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'TMPSRC', PROGNAME )
            ALLOCATE( TMPIDX( NHRSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'TMPIDX', PROGNAME )
            SRCFLG = .FALSE.

C.............  Loop through hours in PLAY_EX file and determine all sources
C               that are listed for all hours
            JDATE  = SDATE
            JTIME  = STIME
            N      = 0
            DO T = 1, NSTEPS
                IF( .NOT. READ3( PHNAME, 'INDXH', ALLAYS3, 
     &                           JDATE, JTIME, INDXH      ) ) THEN

                    MESG = 'Could not read INDXH from ' // PHNAME
                    CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

                END IF   ! if read3() failed

C.................  Loop through this hour's indices, and store any new ones
                DO I = 1, NHRSRC

                    S = INDXH( I )

C.....................  Exit loop if done sources for this hour
                    IF ( S .EQ. 0 ) EXIT

C.....................  Store if not already
                    IF ( .NOT. SRCFLG( S ) ) THEN
                        SRCFLG( S ) = .TRUE.
                        N = N + 1
                        TMPSRC( N ) = S
                        TMPIDX( N ) = N
                    END IF

                END DO

                CALL NEXTIME( JDATE, JTIME, TSTEP )

            END DO
            NHRSRC = N   ! Reset to permit FINDs in case whole PLAY_EX not used

C.............  Sort list index
            CALL SORTI1( NHRSRC, TMPIDX, TMPSRC )

C.............  Store final sorted list
            DO I = 1, NHRSRC
                J = TMPIDX( I )
                ELEVSRC( I ) = TMPSRC( J )
            END DO

C.............  Deallocate temporary memory
            DEALLOCATE( SRCFLG, TMPSRC, TMPIDX )

        END IF

C.........  Read reactivity matrices
        IF( ARFLAG ) CALL RDRMAT( ARNAME, ANSREAC, ARNMSPC, ACRIDX, 
     &                            ACRREPEM, ACRPRJFC, ACRMKTPN, ACRFAC )

        IF( MRFLAG ) CALL RDRMAT( MRNAME, MNSREAC, MRNMSPC, MCRIDX, 
     &                            MCRREPEM, MCRPRJFC, MCRMKTPN, MCRFAC )

        IF( PRFLAG ) CALL RDRMAT( PRNAME, PNSREAC, PRNMSPC, PCRIDX, 
     &                            PCRREPEM, PCRPRJFC, PCRMKTPN, PCRFAC )

C.........  Read gridding matrices (note, must do through subroutine because of
C           needing contiguous allocation for integer and reals)
        IF( AFLAG ) CALL RDGMAT( AGNAME, NGRID, ANGMAT, ANGMAT,
     &                           AGMATX( 1 ), AGMATX( NGRID + 1 ),
     &                           AGMATX( NGRID + ANGMAT + 1 ) )

        IF( MFLAG ) CALL RDGMAT( MGNAME, NGRID, MNGMAT, MNGMAT,
     &                           MGMATX( 1 ), MGMATX( NGRID + 1 ),
     &                           MGMATX( NGRID + MNGMAT + 1 ) )

        IF( PFLAG ) THEN

            PGMATX = 1.  ! initialize array b/c latter part not in file
            CALL RDGMAT( PGNAME, NGRID, NPSRC, 1,
     &                   PGMATX( 1 ), PGMATX( NGRID + 1 ), RDUM )
        END IF


C.........  Build indicies for pollutant/species groups
        CALL BLDMRGIDX( MXGRP, MXVARPGP, NGRP )


C.........  Open NetCDF output files, open ASCII report files, and write headers
        CALL OPENMRGOUT( NGRP )


C.........  In case reactivity does not exist, initialize temporary arrays
C           for reactivity information anyway.  These are used even without
C           reactivity matrix inputs so that the code does not need even
C           more conditionals in the matrix multiplication step.
        IF( AFLAG ) ARINFO = 0.  ! array
        IF( MFLAG ) MRINFO = 0.  ! array
        IF( PFLAG ) PRINFO = 0.  ! array

C.........  Intialize state/county summed emissions to zero
        CALL INITSTCY

C.........  Allocate memory for temporary list of species and pollutant names
        ALLOCATE( VARNAMES( MXVARPGP ), STAT=IOS )
        CALL CHECKMEM( IOS, 'VARNAMES', PROGNAME )
        ALLOCATE( INNAMES( MXVARPGP ), STAT=IOS )
        CALL CHECKMEM( IOS, 'INNAMES', PROGNAME )

C.........  Loop through processing groups (if speciation, this will be specia-
C           tion groups, but if no speciation, this will be pollutant groups,  
C           for purposes of memory usage if many pollutants and/or species)
        PGID = IMISS3
!AQF        DO N = 1, NGRP
	    N = 1

C.............  Set the number of variables per group
            NVPGP = VGRPCNT( N )

C.............  If pollutants in current group are different from those
C               in the previous group, read pollutant-specific control matrices
C.............  For reactivity matrices, read inventory emissions that will
C               be needed for getting ratios of inventory to hourly for applying
C               reactivity-based projection to hourly emissions
C.............  Note that only the pollutants in this group that are actually
C               in the control matrices are stored, and the index that says
C               which are valid is *U_EXIST and *A_EXIST
            IF( IDVGP( N ) .NE. PGID ) THEN

                IF( AUFLAG )
     &              CALL RD3MASK( AUNAME, 0, 0, NASRC, AMULSIZ, NVPGP,
     &                      GVNAMES( 1,N ), AU_EXIST( 1,N ), ACUMATX )

                IF( MUFLAG )
     &              CALL RD3MASK( MUNAME, 0, 0, NMSRC, MMULSIZ, NVPGP,
     &                      GVNAMES( 1,N ), MU_EXIST( 1,N ), MCUMATX )

                IF( PUFLAG )
     &              CALL RD3MASK( PUNAME, 0, 0, NPSRC, PMULSIZ, NVPGP,
     &                      GVNAMES( 1,N ), PU_EXIST( 1,N ), PCUMATX )

                IF( ARFLAG )
     &              CALL RD3MASK( AENAME, 0, 0, NASRC, APOLSIZ, NVPGP,
     &                      GVNAMES( 1,N ), A_EXIST( 1,N ), AEISRC   )

                IF( MRFLAG ) 
     &              CALL RD3MASK( MENAME, 0, 0, NMSRC, MPOLSIZ, NVPGP, 
     &                      GVNAMES( 1,N ), M_EXIST( 1,N ), MEISRC   )

                IF( PRFLAG )
     &              CALL RD3MASK( PENAME, 0, 0, NPSRC, PPOLSIZ, NVPGP, 
     &                      GVNAMES( 1,N ), P_EXIST( 1,N ), PEISRC   )

            END IF

C.............  Loop through variables in current group...
            OCNT = 0
            LBUF = ' '
            INNAMES  = ' '  ! array
            VARNAMES = ' '  ! array
            DO V = 1, NVPGP  ! No. variables per group 

                K1 = 0
                K2 = 0
                K3 = 0

C.................  Extract name of variable in group
                VBUF = GVNAMES( V,N )

C.................  For speciation...
                IF( SFLAG ) THEN

C.....................  Update list of output species names for message
                    SBUF = EMNAM( SPINDEX( V,N ) )
                    PBUF = EANAM( SIINDEX( V,N ) )
                    M = INDEX1( SBUF, OCNT, VARNAMES )

                    IF( M .LE. 0 .AND. SBUF .NE. LBUF ) THEN
                        OCNT = OCNT + 1                            
                        VARNAMES( OCNT ) = SBUF
                        LBUF = SBUF
                    END IF

C.....................  Set position for input of speciation matrix
                    IF( AFLAG ) K1 = AS_EXIST( V,N ) 
                    IF( MFLAG ) K2 = MS_EXIST( V,N ) 
                    IF( PFLAG ) K3 = PS_EXIST( V,N ) 

C.....................  Read speciation matrix for current variable and
C                       position
                    IF ( K1 .GT. 0 )
     &                    CALL RDSMAT( ASNAME, VBUF, ASMATX( 1,K1 ) )
                    IF ( K2 .GT. 0 )
     &                    CALL RDSMAT( MSNAME, VBUF, MSMATX( 1,K2 ) )
                    IF ( K3 .GT. 0 )
     &                    CALL RDSMAT( PSNAME, VBUF, PSMATX( 1,K3 ) )
     


C.................  For no speciation, prepare list of variables for output mesg
                ELSE

C.....................  Update list of pollutants names for message
                    PBUF = EANAM( SIINDEX( V,N ) )
                    M = INDEX1( PBUF, OCNT, VARNAMES )

                    IF( M .LE. 0 ) THEN
                        OCNT = OCNT + 1                            
                        VARNAMES( OCNT ) = PBUF
                    END IF

                END IF  ! end speciation or not

C.................  Set input variable names
                INNAMES ( V ) = TONAMES( SIINDEX( V,N ) )

            END DO      ! End variables in group loop

C.............  Write out message about data currently being processed
            CALL POLMESG( OCNT, VARNAMES )

C.............  Initializations before main time loop 
            JDATE  = SDATE
            JTIME  = STIME
            LDATE  = 0
            DAY    = 1

            TSTEP3D = 10000

!AQF        END DO   ! End of loop on pollutant/pol-to-spcs groups

C.........  Successful completion of program
!AQF        CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )
            CALL M3MSG2('Successful Completion of PRE_SMKMERGE')

C            CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93020   FORMAT( 8X, 'at time ', A8 )

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( A )

94010   FORMAT( 10 ( A, :, I10, :, 2X ) )

94020   FORMAT( A, I4, 2X, 10 ( A, :, 1PG14.6, :, 2X ) )

94030   FORMAT( 8X, 'at time ', A8 )

        END SUBROUTINE PRE_SMKMERGE

