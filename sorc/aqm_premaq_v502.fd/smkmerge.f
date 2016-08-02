
        SUBROUTINE SMKMERGE (premaq_jdate, premaq_jtime)

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
C       01/05 David Wong
C         -- Eliminated reading in data file PLAY and retrieved LFRAC data
C            from MYLFRAC
C         -- Initialized PEMGRD before the DO loop
C         -- Passed in a particular species of PEMGRD to subroutine MRGMULT
C            since variable PEMGRD has been changed from 2D to 3D
C         -- Introduced a new argument KP in calling subroutine WMRGEMIS since
C            variable PEMGRD has been changed from 2D to 3D
C    02/14/05 David Wong
C         -- Removed data structure MYLFRAC
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
C        USE MODLISTS, ONLY: NINVIFIP, INVIFIP

C.........  This module contains the arrays for state and county summaries
        USE MODSTCY, ONLY: NCOUNTY, AICNY, MICNY, PICNY

C...........   This module contains the gridding surrogates tables
        USE MODSURG, ONLY: NSRGFIPS, SRGFIPS

C.........  This module contains the global variables for the 3-d grid
        USE MODGRID, ONLY: NGRID, OFFLAG

        use mod_smkmerge
	
	USE MOD_TEMPORAL, ONLY: EMIST
	
        IMPLICIT NONE


C...........   EXTERNAL FUNCTIONS and their descriptions:
        
        CHARACTER(2)    CRLF
        CHARACTER(10)   HHMMSS
        INTEGER         INDEX1
        INTEGER         WKDAY

        EXTERNAL    CRLF, HHMMSS, INDEX1, WKDAY

C.........  LOCAL PARAMETERS and their descriptions:

        CHARACTER(50), PARAMETER ::CVSW = '$Name: SMOKE_v2_1_09302004 $' ! CVS release tag



        integer premaq_jdate, premaq_jtime
     
C...........   Other local variables
    
        INTEGER          I, J, K, L1, L2, M, V, S, T ! counters and indices
	
        CHARACTER(16) :: PROGNAME = 'SMKMERGE' ! program name

C***********************************************************************
C   begin body of program SMKMERGE

                JDATE = premaq_jdate
                JTIME = premaq_jtime
        
C.............  Loop through output time steps
!AQF            DO T = 1, NSTEPS   ! at least once for time-independent

C................. For time-dependent processing, write out a few messages...
                IF( TFLAG ) THEN

C.....................  Determine weekday index (Monday is 1)
                    DAY = WKDAY( JDATE )

C.....................  Write out message for new day.  Note, For time-
C                       independent, LDATE and JDATE will both be zero.
                    IF( JDATE .NE. LDATE ) THEN

                        CALL WRDAYMSG( JDATE, MESG )

                    END IF

C.....................  Write out files that are being used for by-day treatment
                    IF( DAY .NE. PDAY ) THEN

                        IF( AFLAG_BD ) THEN
                            MESG = '   with ATMP file ' // ATNAME( DAY )
                            CALL M3MSG2( MESG )
                        END IF

                        IF( MFLAG_BD ) THEN
                            MESG = '   with MTMP file ' // MTNAME( DAY )
                            CALL M3MSG2( MESG )
                        END IF

                        IF( PFLAG_BD ) THEN
                            MESG = '   with PTMP file ' // PTNAME( DAY )
                            CALL M3MSG2( MESG )
                        END IF

                        PDAY = DAY

                    END IF

C.....................  For new hour...
C.....................  Write to screen because WRITE3 only writes to LDEV
                    WRITE( *, 93020 ) HHMMSS( JTIME )

                END IF

C.................  Initialize source-category current dates
                AJDATE = JDATE
                MJDATE = JDATE
                PJDATE = JDATE

C.................  Reset the date for each source category when by-day 
C                   processing is being done for that category
                IF( AFLAG_BD ) AJDATE = ASDATE( DAY )
                IF( MFLAG_BD ) MJDATE = MSDATE( DAY )
                IF( PFLAG_BD ) PJDATE = PSDATE( DAY )

C.................  If area sources, read inventory emissions for this time 
C                   step for all area-source pollutants in current pol group
C.................  The *_EXIST are counters that point to the position in
C                   the source category emissions of the variables names 
C                   in INNAMES. Data are stored in *EMSRC in the global order.
                IF( AFLAG ) THEN

C.................  If using map-formatted inventory for time-independent    
                    IF( ANMAP .NE. 0 .AND. .NOT. TFLAG ) THEN

                        CALL RDMAPMASK( AENAME, ANMAP, AMAPNAM, AMAPFIL, 
     &                               NASRC, APOLSIZ, NVPGP, VARNAMES(1),
     &                               INNAMES(1), A_EXIST(1,N), AEMSRC  )

C.................  If using hourly data
                    ELSE
                        CALL RDSETMASK( ATNAME( DAY ), AJDATE, JTIME, 
     &                                NASRC, APOLSIZ, NVPGP, INNAMES(1), 
     &                                A_EXIST( 1,N ), AEMSRC )
                    END IF
                END IF

C.................  If mobile sources, read inventory emissions or activities
C                   for this time step for all mobile-source pollutants in 
C                   current pol group
                IF( MFLAG ) THEN 

C.................  If using map-formatted inventory for time-independent    
                    IF( MNMAP .NE. 0 .AND. .NOT. TFLAG ) THEN
                        CALL RDMAPMASK( MENAME, MNMAP, MMAPNAM, MMAPFIL, 
     &                               NMSRC, MPOLSIZ, NVPGP, VARNAMES(1), 
     &                                INNAMES(1), M_EXIST(1,N), MEMSRC )

C.................  If using hourly data
                    ELSE
                        CALL RDSETMASK( MTNAME( DAY ), MJDATE, JTIME, 
     &                                NMSRC, MPOLSIZ, NVPGP, INNAMES(1),
     &                                M_EXIST( 1,N ), MEMSRC )
                    END IF
                END IF

C.................  If point sources, read inventory emissions for this time 
C                   step for all point-source pollutants in current pol group
                IF( PFLAG ) THEN

C.................  If using map-formatted inventory for time-independent    
                    IF( PNMAP .NE. 0 .AND. .NOT. TFLAG ) THEN
                        CALL RDMAPMASK( PENAME, PNMAP, PMAPNAM, PMAPFIL, 
     &                               NPSRC, PPOLSIZ, NVPGP, VARNAMES(1), 
     &                                INNAMES(1), P_EXIST(1,N), PEMSRC )

C.................  If using hourly data
                    ELSE

!                        CALL RDSETMASK( PTNAME( DAY ), PJDATE, JTIME, 
!     &                                NPSRC, PPOLSIZ, NVPGP, INNAMES(1), 
!     &                                P_EXIST( 1,N ), PEMSRC )
     

! check that max of J = NGSZ
! check that NPSRC = NSRC     
                        
                        DO V =1, NVPGP
			    J = P_EXIST( V,N )
			    IF( J .EQ. 0 ) CYCLE
			    PEMSRC( 1:NPSRC,J)=EMIST(   1:NPSRC,J)

                        END DO
                    END IF
                END IF

C.................  If layer fractions, read them for this time step
                IF( LFLAG ) THEN

C                   IF( .NOT. READ3( PLNAME, 'LFRAC', ALLAYS3, 
C    &                               JDATE, JTIME, LFRAC      ) ) THEN

C                       MESG = 'Could not read LFRAC from ' // PLNAME
C                       CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

C                   END IF   ! if read3() failed

C.................  Otherwise, if explicit plume rise, read fractions and
C                   indices from the file
                ELSE IF ( EXPLFLAG ) THEN

                    IF( .NOT. READ3( PHNAME, 'INDXH', ALLAYS3, 
     &                               JDATE, JTIME, INDXH      ) ) THEN

                        MESG = 'Could not read INDXH from ' // PHNAME
                        CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

                    END IF   ! if read3() failed

                    IF( .NOT. READ3( PHNAME, 'LFRAC', ALLAYS3, 
     &                               JDATE, JTIME, LFRAC      ) ) THEN

                        MESG = 'Could not read LFRAC from ' // PHNAME
                        CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

                    END IF   ! if read3() failed

                END IF

                PEMGRD = 0.  ! array

C.................  Loop through variables in the current group
                LBUF = ' '
                DO V = 1, NVPGP

C.....................  Set species or pollutant/activity name for this 
C                       iteration
                    IF( SFLAG ) THEN
                        SBUF = EMNAM( SPINDEX( V,N ) )
                        IF( AFLAG ) KA  = INDEX1( SBUF, ANMSPC, AEMNAM )
                        IF( BFLAG ) KB  = INDEX1( SBUF, BNMSPC, BEMNAM )
                        IF( MFLAG ) KM  = INDEX1( SBUF, MNMSPC, MEMNAM )
                        IF( PFLAG ) KP  = INDEX1( SBUF, PNMSPC, PEMNAM )
                    ELSE
                        SBUF = EANAM( SIINDEX( V,N ) )
                    END IF

C.....................  Set conversion factors
                    IF( SFLAG ) THEN
                        F1 = GRDFAC( SPINDEX( V,N ) )
                        F2 = TOTFAC( SPINDEX( V,N ) )
                    ELSE
                        F1 = GRDFAC( SIINDEX( V,N ) )
                        F2 = TOTFAC( SIINDEX( V,N ) )
                    END IF

C.....................  If area reactivity matrix applies, pre-compute
C                       source array of reactivity emissions & mkt pentrtn
                    IF( ARFLAG ) THEN
                        K1 = A_EXIST ( V,N )
                        K2 = AR_EXIST( V,N )
                        IF( K2 .GT. 0 ) THEN
                            CALL APPLREAC( NASRC, ANSREAC, K1, K2, 
     &                             APRJFLAG, LMKTPON, AEISRC,AEMSRC, 
     &                             ACRIDX, ACRREPEM, ACRPRJFC, 
     &                             ACRMKTPN, ACRFAC, ARINFO )

                        ELSE
                            ARINFO = 0.  ! array
                        END IF
                    END IF

C.....................  Process for area sources...
                    IF( AFLAG ) THEN

                        K1 = A_EXIST ( V,N )
                        K2 = AU_EXIST( V,N )
                        K4 = AS_EXIST( V,N )
                        K5 = NGRID + ANGMAT + 1

C.............................  Apply valid matrices & store
                        CALL MRGMULT( NASRC, NGRID, 1, ANGMAT, 
     &                         ANGMAT, K1, K2, K4, KA, F1, F2, 
     &                         AEMSRC, ARINFO, ACUMATX, ASMATX, 
     &                         AGMATX(1), AGMATX(NGRID+1), 
     &                         AGMATX(K5), AICNY, AEMGRD, TEMGRD,
     &                         AEBCNY, AEUCNY, AERCNY, AECCNY )
                    END IF
                            
C.....................  For biogenic sources, read gridded emissions,
C                       add to totals and store
                    IF( BFLAG ) THEN

                        K4 = BS_EXIST( V,N )

                        IF( K4 .GT. 0 ) THEN
                            CALL MRGBIO( SBUF, BTNAME, JDATE, JTIME, 
     &                                   NGRID, BIOGFAC, BEMGRD, 
     &                                   TEMGRD( 1,1 ) )
                    

C.............................  Update country, state, & county totals  
C.............................  Also convert the units from the gridded output
C                               units to the totals output units
                            IF( LREPANY ) THEN
                                FB = BIOTFAC / BIOGFAC
                                CALL GRD2CNTY( 0, KB, NCOUNTY, 
     &                                         FB, BEMGRD, BEBCNY )

                            END IF
                        END IF

                    END IF
                            
C.....................  If mobile reactivity matrix applies, pre-compute
C                       source array of reacvty emissions and mkt pntrtn
                    IF( MRFLAG ) THEN
                        K1 = M_EXIST ( V,N )
                        K2 = MR_EXIST( V,N )
                        IF( K2 .GT. 0 ) THEN
                            CALL APPLREAC( NMSRC, MNSREAC, K1, K2, 
     &                             MPRJFLAG, LMKTPON, MEISRC,MEMSRC,
     &                             MCRIDX, MCRREPEM, MCRPRJFC, 
     &                             MCRMKTPN, MCRFAC, MRINFO )

                        ELSE
                            MRINFO = 0.  ! array
                        END IF

                    END IF

C.....................  Process for mobile sources...
                    IF( MFLAG ) THEN

                        K1 = M_EXIST ( V,N )
                        K2 = MU_EXIST( V,N )
                        K4 = MS_EXIST( V,N )
                        K5 = NGRID + MNGMAT + 1
                           
C.........................  Apply valid matrices & store
                        CALL MRGMULT( NMSRC, NGRID, 1, MNGMAT,
     &                         MNGMAT, K1, K2, K4, KM, F1, F2, 
     &                         MEMSRC, MRINFO, MCUMATX, MSMATX, 
     &                         MGMATX(1), MGMATX(NGRID+1), 
     &                         MGMATX(K5), MICNY, MEMGRD, TEMGRD,
     &                         MEBCNY, MEUCNY, MERCNY, MECCNY )

                    END IF

C.....................  If reactivity matrix applies, pre-compute source
C                       array of reactivity emissions and market penetration
                    IF( PRFLAG ) THEN
                        K1 = P_EXIST ( V,N )
                        K2 = PR_EXIST( V,N )
                        IF( K2 .GT. 0 ) THEN
                            CALL APPLREAC( NPSRC, PNSREAC, K1, K2,  
     &                             PPRJFLAG, LMKTPON, PEISRC,PEMSRC,
     &                             PCRIDX, PCRREPEM, PCRPRJFC, 
     &                             PCRMKTPN, PCRFAC, PRINFO )
                        ELSE
                            PRINFO = 0.  ! array
                        END IF
                    END IF

C.....................  Process for point sources...
                    IF( PFLAG ) THEN

                        K1 = P_EXIST ( V,N )
                        K2 = PU_EXIST( V,N )
                        K4 = PS_EXIST( V,N )
                        K5 = NGRID + NPSRC + 1

C.........................  Apply valid matrices & store
                        CALL MRGMULT( NPSRC, NGRID, EMLAYS, NPSRC, 
     &                         NPSRC, K1, K2, K4, KP, F1, F2, 
     &                         PEMSRC, PRINFO, PCUMATX, PSMATX, 
     &                         PGMATX(1), PGMATX(NGRID+1),
     &                         PGMATX(K5), PICNY, PEMGRD(:,:,KP), TEMGRD,
     &                         PEBCNY, PEUCNY, PERCNY, PECCNY )

C.........................  Apply matrices for elevated and plume-in-grid 
C                           outputs, if this pollutant is used for point srcs.
                        IF( K1. GT. 0 .AND.
     &                    ( ELEVFLAG .OR. PINGFLAG ) ) THEN
                            CALL MRGELEV( NPSRC, NMAJOR, NPING, 
     &                                    K1, K2, K4, F1 )
                        END IF

                    END IF

C.....................  Check the flag that indicates the entries for which
C                       we need to output the gridded data
                    IF( GVLOUT( V,N ) ) THEN

C.........................  Write out gridded data and Models-3 PinG file
                        CALL WMRGEMIS( SBUF, KP, JDATE, JTIME )

C.........................  Write out ASCII elevated sources file
                        IF( ELEVFLAG ) THEN
                            CALL WMRGELEV( SBUF, NPSRC, NMAJOR, 
     &                                     JDATE, JTIME        )
                        END IF

C.........................  Initialize gridded arrays
                        IF( AFLAG ) THEN
                            AEMGRD = 0.  ! array
                        ENDIF

                        IF( MFLAG ) THEN
                            MEMGRD = 0.  ! array
                        ENDIF

C                       IF( PFLAG ) THEN
C                           PEMGRD = 0.  ! array
C                       ENDIF

                        TEMGRD = 0.      ! array
                    END IF 

                END DO      ! End loop on variables in group

C.................  Write country, state, and county emissions (all that apply) 
C.................  The subroutine will only write for certain hours and 
C                   will reinitialize the totals after output
                IF( LREPANY ) THEN
                    CALL WRMRGREP( JDATE, JTIME, N )
                END IF

                LDATE = JDATE

!AQF                CALL NEXTIME( JDATE, JTIME, TSTEP )     !  update model clock

!AQF            END DO          ! End loop on time steps

!AQF        END DO   ! End of loop on pollutant/pol-to-spcs groups

C.........  Successful completion of program
!AQF        CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )


C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93020   FORMAT( 8X, 'at time ', A8, ':SMKMERGE' )

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( A )

94010   FORMAT( 10 ( A, :, I10, :, 2X ) )

94020   FORMAT( A, I4, 2X, 10 ( A, :, 1PG14.6, :, 2X ) )

94030   FORMAT( 8X, 'at time ', A8 )

        END SUBROUTINE SMKMERGE

