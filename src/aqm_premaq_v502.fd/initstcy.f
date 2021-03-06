
        SUBROUTINE INITSTCY

C***********************************************************************
C  subroutine INITSTCY body starts at line
C
C  DESCRIPTION:
C      The purpose of this subroutine is to initialize the necessary fields
C      for performing state and county totals.  The first call sets up the
C      indices from each source to each county.
C
C  PRECONDITIONS REQUIRED:  
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Created 8/99 by M. Houyoux
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: initstcy.f,v 1.9 2004/07/26 14:08:38 cseppan Exp $
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
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/smkmerge/initstcy.f,v $
C Last updated: $Date: 2004/07/26 14:08:38 $ 
C
C****************************************************************************

C.........  MODULES for public variables
C.........  This module contains the major data structure and control flags
        USE MODMERGE, ONLY: NASRC,          NMSRC,  NPSRC,          ! no. of sources by category
     &                      AIFIP,          MIFIP,  PIFIP,          ! country/state/county codes
     &                      AFLAG,  BFLAG,  MFLAG,  PFLAG,  XFLAG,  ! source type flags
     &                      LREPSTA,                                ! output state total emissions flag
     
     &                      AEBSTA, BEBSTA, MEBSTA, PEBSTA, TEBSTA, ! state total speciated emissions
     &                      AEUSTA,         MEUSTA, PEUSTA, TEUSTA, ! state total mult-control emissions
     &                      AERSTA,         MERSTA, PERSTA, TERSTA, ! state total reac-control emissions
     &                      AECSTA,         MECSTA, PECSTA, TECSTA, ! state total all-control emissions
     
     &                      AEBCNY, BEBCNY, MEBCNY, PEBCNY, TEBCNY, ! county total speciated emissions
     &                      AEUCNY,         MEUCNY, PEUCNY, TEUCNY, ! county total mult-control emissions
     &                      AERCNY,         MERCNY, PERCNY, TERCNY, ! county total reac-control emissions
     &                      AECCNY,         MECCNY, PECCNY, TECCNY  ! county total all-control emissions
     
C.........  This module contains the arrays for state and county summaries
        USE MODSTCY, ONLY: AICNY, MICNY, PICNY, NCOUNTY, CNTYCOD

        IMPLICIT NONE

C...........   INCLUDES:
        
        INCLUDE 'EMCNST3.EXT'   !  emissions constant parameters

C...........   EXTERNAL FUNCTIONS and their descriptions:
        
        CHARACTER(2)    CRLF
        LOGICAL         ENVYN
        INTEGER         FIND1  

        EXTERNAL   CRLF, ENVYN, FIND1

C...........   Other local variables

        INTEGER          IOS      ! i/o status
        INTEGER          J        ! counter

        LOGICAL, SAVE :: FIRSTIME = .TRUE. ! true: first time routine called

        CHARACTER(300)   MESG     ! message buffer

        CHARACTER(16) :: PROGNAME = 'INITSTCY' ! program name

C***********************************************************************
C   begin body of subroutine INITSTCY

C.........  Read surrogates (if needed) and state/county names
        IF( FIRSTIME ) THEN
        
C.............  Allocate memory for indices from Co/st/cy codes to counties
            ALLOCATE( AICNY( NASRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'AICNY', PROGNAME )
            ALLOCATE( MICNY( NMSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'MICNY', PROGNAME )
            ALLOCATE( PICNY( NPSRC ), STAT=IOS )
            CALL CHECKMEM( IOS, 'PICNY', PROGNAME )

C.............  Create indices to counties from Co/st/cy codes for each source 
C               category
            IF( AFLAG ) THEN                
                CALL SET_COUNTY_INDEX( NASRC, AIFIP, AICNY )
            END IF

            IF( MFLAG ) THEN                
                CALL SET_COUNTY_INDEX( NMSRC, MIFIP, MICNY )
            END IF

            IF( PFLAG ) THEN                
                CALL SET_COUNTY_INDEX( NPSRC, PIFIP, PICNY )
            END IF

            FIRSTIME = .FALSE.

        END IF

C.........  Initialize totals to zero...
C.........  State totals...
        IF( LREPSTA ) THEN

            IF( AFLAG ) THEN
                AEBSTA = 0.   ! state total inven or speciated emissions
                IF ( ALLOCATED( AEUSTA ) ) THEN
                    AEUSTA = 0.   ! state total multipltv-controlled emissions 
                END IF
                IF ( ALLOCATED( AERSTA ) ) THEN
                    AERSTA = 0.   ! state total reactivity-controlled emissions
                END IF
                IF ( ALLOCATED( AECSTA ) ) THEN
                    AECSTA = 0.   ! state total all-controlled emissions
                END IF
            END IF

            IF( BFLAG ) THEN
                BEBSTA = 0.
            END IF

            IF( MFLAG ) THEN
                MEBSTA = 0.
                IF ( ALLOCATED( MEUSTA ) ) THEN
                    MEUSTA = 0.
                END IF
                IF ( ALLOCATED( MERSTA ) ) THEN
                    MERSTA = 0.
                END IF
                IF ( ALLOCATED( MECSTA ) ) THEN
                    MECSTA = 0.
                END IF
            END IF

            IF( PFLAG ) THEN
                PEBSTA = 0.
                IF ( ALLOCATED( PEUSTA ) ) THEN
                    PEUSTA = 0.
                END IF
                IF ( ALLOCATED( PERSTA ) ) THEN
                    PERSTA = 0.
                END IF
                IF ( ALLOCATED( PECSTA ) ) THEN
                    PECSTA = 0.
                END IF
            END IF

            IF( XFLAG ) THEN
                TEBSTA = 0.
                IF ( ALLOCATED( TEUSTA ) ) THEN
                    TEUSTA = 0.
                END IF
                IF ( ALLOCATED( TERSTA ) ) THEN
                    TERSTA = 0.
                END IF
                IF ( ALLOCATED( TECSTA ) ) THEN
                    TECSTA = 0.
                END IF
            END IF

        END IF

C.........  County totals...
        IF( AFLAG ) THEN
            AEBCNY = 0.   ! county total inven or speciated emissions
            IF ( ALLOCATED( AEUCNY ) ) THEN
                AEUCNY = 0.   ! county total multiplicative-controlled emissions
            END IF
            IF ( ALLOCATED( AERCNY ) ) THEN
                AERCNY = 0.   ! county total reactivity-controlled emissions
            END IF
            IF ( ALLOCATED( AECCNY ) ) THEN
                AECCNY = 0.   ! county total all-controlled emissions
            END IF
        END IF

        IF( BFLAG ) THEN
            BEBCNY = 0.
        END IF

        IF( MFLAG ) THEN
            MEBCNY = 0.
            IF ( ALLOCATED( MEUCNY ) ) THEN
                MEUCNY = 0.
            END IF
            IF ( ALLOCATED( MERCNY ) ) THEN
                MERCNY = 0.
            END IF
            IF ( ALLOCATED( MECCNY ) ) THEN
                MECCNY = 0.
            END IF
        END IF

        IF( PFLAG ) THEN
            PEBCNY = 0.
            IF ( ALLOCATED( PEUCNY ) ) THEN
                PEUCNY = 0.
            END IF
            IF ( ALLOCATED( PERCNY ) ) THEN
                PERCNY = 0.
            END IF
            IF ( ALLOCATED( PECCNY ) ) THEN
                PECCNY = 0.
            END IF
        END IF

        IF( XFLAG ) THEN
            TEBCNY = 0.
            IF ( ALLOCATED( TEUCNY ) ) THEN
                TEUCNY = 0.
            END IF
            IF ( ALLOCATED( TERCNY ) ) THEN
                TERCNY = 0.
            END IF
            IF ( ALLOCATED( TECCNY ) ) THEN
                TECCNY = 0.
            END IF
        END IF

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats.............94xxx

94010   FORMAT( 10( A, :, I8, :, 1X ) )

C******************  INTERNAL SUBPROGRAMS   ******************************

        CONTAINS

C.............  This subroutine creates the county to src FIPs index
            SUBROUTINE SET_COUNTY_INDEX( NSRC, IFIP, ICNY )

C.............  Subprogram arguments
            INTEGER, INTENT (IN) :: NSRC
            INTEGER, INTENT (IN) :: IFIP( NSRC )
            INTEGER, INTENT(OUT) :: ICNY( NSRC )

C.............  Local variables
            INTEGER     J, S     ! counters and indices
            INTEGER     FIP      ! tmp cy/st/co code
            INTEGER     PFIP     ! previous cy/st/co code

C----------------------------------------------------------------------------

            PFIP = -9
            DO S = 1, NSRC

                FIP = IFIP( S )

                IF( FIP .NE. PFIP ) THEN

                    J = MAX( FIND1( FIP, NCOUNTY, CNTYCOD ), 0 )
                    PFIP = FIP

                END IF

                ICNY( S ) = J

            END DO

            END SUBROUTINE SET_COUNTY_INDEX

        END SUBROUTINE INITSTCY
