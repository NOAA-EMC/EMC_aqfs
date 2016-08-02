 
      SUBROUTINE HRBEIS3( JDATE, JTIME, NX, NY, COSZEN,MSPCS,
     &                    INITIAL_HOUR,                    ! added by GAP
     &                    SEMIS, GROWAGNO, NGROWAGNO, NONAGNO,
     &                    SLAI, TA,
     &                    RAIN,                            ! added by GAP
     &                    PTYPE, PULSEDATE, PULSETIME,     ! added by GAP
     &                    TSOLAR, PRES, EMPOL )

C***********************************************************************
C  subroutine body starts at line  143
C
C  DESCRIPTION:
C  
C     Uses PAR and sfc temperature data to calculate
C     biogenic ISOP and MBO emissions.  Other emissions are
C     calculated using the temperature data only.  
C
C  PRECONDITIONS REQUIRED:
C     PAR and Surface Temperature
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C    4/01 : Prototype by JMV
C 02/10/05: by David Wong
C              -- Parallelized certain loop(s) using OpenMP construct
C 03/10/05  by G. Pouliot - fixed dim on EMPOL to NSEF in declarations (bugfix)
C 03/10/05  by G. Pouliot include Shawn Roselle's fixes to PARSHADE, PARSUN,
C                 CANPARDIF errors were found in CLNEW program
C 04/20/05  by G. Pouliot updates to make code consistent with BEIS3.13 only
C                 changes to CLNEW & CGUEN with diagnostic message update
C                 for PAR out of bounds
C 
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: hrbeis3.f,v 1.1.1.1 2001/03/27 19:08:49 smith_w Exp $
C
C COPYRIGHT (C) 2001, MCNC--Environmental Programs
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
C Pathname: $Source: /env/proj/archive/cvs/jmv/beis3v0.9/hrbeis3.f,v $
C Last updated: $Date: 2001/03/27 19:08:49 $ 
C
C***********************************************************************

      IMPLICIT NONE

C.... INCLUDES:

      INCLUDE 'PARMS3.EXT'         ! I/O API constants
      INCLUDE 'FDESC3.EXT'         ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'        ! I/O API function declarations
      INCLUDE 'EMCNST3.EXT'        ! Emissions constants
      INCLUDE 'CONST3.EXT'         ! More constants
      INCLUDE 'B3V11DIMS3.EXT'     ! biogenic-related constants

C.... EXTERNAL FUNCTIONS and their descriptions:

      INTEGER  INDEX1
      EXTERNAL INDEX1

C.... ARGUMENTS and their descriptions:

      INTEGER, INTENT (IN)  :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT (IN)  :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT (IN)  :: NX      ! no. columnse
      INTEGER, INTENT (IN)  :: NY      ! no. rows
      INTEGER, INTENT (IN)  :: MSPCS   ! no. of output species

      REAL,    INTENT (IN)  :: TA   ( NX, NY )      ! air temperature (K)

!---- added gy GAP 12/01 --------------------

      REAL,    INTENT (IN  )  :: RAIN (NX, NY)      ! rainfall rate (cm/ 24hr)

      INTEGER, INTENT (INOUT) :: PTYPE (NX, NY)     ! 'pulse' type
      INTEGER, INTENT (INOUT) :: PULSEDATE (NX, NY) ! date of pulse start
      INTEGER, INTENT (INOUT) :: PULSETIME (NX, NY) ! date of pulse end

!---- end added by GAP 12/01 ---------------

      REAL, INTENT (IN)  :: SEMIS ( NX, NY, NSEF-1 )  ! norm emissions
      REAL, INTENT (IN)  :: SLAI  ( NX, NY, NLAI )    ! leaf area indices

      REAL, INTENT (IN)  :: TSOLAR   ( NX, NY )    ! PAR
      REAL, INTENT (IN)  :: GROWAGNO ( NX, NY )
      REAL, INTENT (IN)  :: NGROWAGNO( NX, NY )
      REAL, INTENT (IN)  :: NONAGNO  ( NX, NY )
      REAL, INTENT (IN)  :: COSZEN   ( NX, NY )    ! cosine of zenith angle
      REAL, INTENT (IN)  :: PRES     ( NX, NY )    ! surface pressure (mb)

      REAL, INTENT (OUT) :: EMPOL    ( NX, NY, NSEF ) ! output pol emissions

      LOGICAL, INTENT (IN) :: INITIAL_HOUR

C.... SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER         TRIMLEN
      EXTERNAL        TRIMLEN

      CHARACTER*16 :: PROGNAME = 'HRBEIS311'   !  program name
      CHARACTER*5     BTMP    ! temporary tag for naming output variables
      CHARACTER*256   MESG

      INTEGER         R, C, L, I   ! counters
      INTEGER         IAFTER
      REAL            CFOTHR       ! isop corr fac -- non-forest
      REAL            CFCLAI       ! ISOP CORR FAC -- LAI
      REAL            CFNO         ! NO correction factor
      REAL            CFOVOC       ! non-isop corr fac
      REAL            PAR          ! photo. actinic flux (UE/M**2-S)
      REAL            CT, DT       ! temperature correction
      REAL            TAIR         ! surface temperature
      REAL            RK           ! k from Geron and Guenther
      REAL            CSUBL        ! C sub l
      REAL            TLAI         ! temporary storage of LAI
      REAL            CTMP1, CTMP2 ! temporary storage of CT
      REAL            SOLTMP       ! temporary storage of radiation
      REAL            PSFC         ! temporary storage of sfc pressure (mb)
      REAL            ZEN          ! zenith angle
      REAL            PARDB        ! par direct beam
      REAL            PARDIF       ! par diffuse

C***********************************************************************
C
C   begin body of subroutine  HRBIOS
C
C***********************************************************************

C.... loop thru cells


!$omp parallel do private(SOLTMP, PSFC, ZEN, PARDB, PARDIF, TAIR, DT, CT,
!$omp&                    PAR, I, BTMP, TLAI, CSUBL, CFOVOC)
      DO R = 1, NY
         DO C = 1, NX
                
            TAIR = TA( C, R )         ! unit in degree K

C.... Perform checks on max and min bounds for temperature

            IF ( TAIR .LT. 200.0 ) THEN
               WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &                              'out of range at (C,R)=',  C, R
               CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

            ELSEIF ( TAIR .GT. 315.0 ) THEN
               WRITE( MESG, 94020 ) 'TAIR=', TAIR,
     &                              'out of range at (C,R)=',  C, R,
     &                              ' resetting to 315K'
               CALL M3WARN( PROGNAME, JDATE, JTIME, MESG )
               TAIR = 315.0
            ENDIF

C.... Calculate temperature correction term

            DT = 28668.514 / TAIR

            CTMP1 = EXP( 37.711 - 0.398570815 * DT )
            CTMP2 = 1.0 + EXP( 91.301 - DT )

            CT = CTMP1 / CTMP2

            SOLTMP = TSOLAR( C, R )

C.... cosine of zenith angle to zenith angle (radians)

            ZEN  = ACOS( COSZEN ( C, R ) ) 
            PSFC = PRES( C, R )              ! surface pressure

            CALL GETPAR( SOLTMP, PSFC, ZEN, PARDB, PARDIF )

            PAR = PARDB + PARDIF

C.... Check max/min bounds of PAR and calculate biogenic ISOP

            IF ( PAR .LT. 0.00 .OR. PAR .GT. 2600.0 ) THEN

               WRITE( MESG, 94030 ) 'PAR=', PAR,
     &                              'out of range at (C,R)=', C, R,
     &                              'PARDB  = ', PARDB,
     &                              'PARDIF = ', PARDIF,
     &                              'SOLTMP = ', SOLTMP,
     &                              'PSFC   = ', PSFC,
     &                              'ZEN    = ', ZEN
    
               CALL M3MSG2( MESG )
!              CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
            ENDIF

C.... Compute ISOP and MBO and METH emissions first
C.... Note assumption that these are the first 3 species in LAITYPE and
C.... BIOTYPE arrays

            DO I = 1, NLAI

               BTMP = LAITYPES( I )
               TLAI = SLAI( C, R, I )
!
!**** METHANOL ADJUSTMENT set TLAI to max (3.0, TLAI) for methanol
!     January 16, 2003  (T. Pierce recommendation)
!
               IF ( BTMP( 1: TRIMLEN( BTMP ) ) .EQ. 'METH' ) THEN
                  TLAI = MAX(3.0,TLAI)
               ENDIF
                     

               IF ( TLAI .GT. 10.0 ) THEN
                  WRITE( MESG, 94010 ) 'LAI=', TLAI,
     &                                 'out of range at (C,R)=', C, R
                  CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
               ENDIF

C.... Initialize csubl 

               CSUBL = 0.0

               IF (PAR .LE. 0.01 .OR. COSZEN(C,R) .LE. 0.02079483) THEN
                  EMPOL( C, R, I ) = 0.0
               ELSE
                  IF ( TLAI .GT. 0.1 ) THEN 
                     CSUBL = CLNEW( ZEN, PARDB, PARDIF, TLAI )
                  ELSE                               ! keep this or not?
                     CSUBL  = CGUEN( PAR ) 
                  ENDIF
 
                  EMPOL( C, R, I ) = SEMIS( C,R, I ) * CT * CSUBL
               ENDIF
            ENDDO                  ! end ISOP and MBO calculations loop


C.... calculate other biogenic emissions except NO 
C.... Note not speciated here
           
            CFOVOC = EXP( 0.09 * ( TAIR - 303.0 ) )

            IAFTER = NLAI + 1

            DO I = IAFTER, NSEF - 1
               EMPOL( C,R, I ) = SEMIS( C,R, I ) * CFOVOC
            ENDDO

                
C.... calculate NO emissions by going thru temperature cases

!           IF ( TAIR .GT. 303.00 ) TAIR = 303.00
!           IF ( TAIR .GT. 268.8690 ) THEN
!              CFNO = EXP( 0.05112 * TAIR - 15.68248 )   ! agriculture
!              EMPOL( C,R, NSEF ) = SEMIS( C,R, NSEF ) * CFNO 
!           ELSE
!              EMPOL( C,R, NSEF ) = 0.00
!           ENDIF      

         ENDDO                        ! end of C loop
      ENDDO                           ! end of R loop

C***********************************************
!$omp end parallel do
C***********************************************

      CALL HRNO( JDATE, JTIME, NX, NY,
     &           INITIAL_HOUR,GROWAGNO, NGROWAGNO, NONAGNO,
     &           TA,RAIN,
     &           PTYPE, PULSEDATE, PULSETIME,
     &           EMPOL ,MSPCS) 

      RETURN

C***********************************************************************
C******************  FORMAT  STATEMENTS   ******************************
C
C.... Informational (LOG) message formats... 92xxx
C.... Internal buffering formats............ 94xxx
C
C***********************************************************************

94010 FORMAT( A, F10.2, 1X, A, I3, ',', I3 )
94020 FORMAT( A, F10.2, 1X, A, I3, ',', I3, A )
94030 FORMAT( A, F10.2, 1X, A, I3, ',', I3, 5(A, F10.2))




C***************** CONTAINS ********************************************

      CONTAINS

C-----------------------------------------------------------------
C   The following two functions were changed to make this routine
C   consistent with BEIS3.13               (G. Pouliot)
C-----------------------------------------------------------------

C-----------------------------------------------------------------
C-----------------------------------------------------------------
      REAL FUNCTION CLNEW( ZEN, PARDB, PARDIF, TLAI )

      IMPLICIT NONE

C.... Function arguments

      REAL, INTENT (IN) :: PARDB    ! direct beam PAR( umol/m2-s)
      REAL, INTENT (IN) :: PARDIF   ! diffuse PAR ( umol/m2-s)
      REAL, INTENT (IN) :: ZEN      ! solar zenith angle (radians)
      REAL, INTENT (IN) :: TLAI     ! leaf area index for grid cell

C.... Local variables

      REAL ALPHA              ! leave absorptivity
      REAL KBE                ! extinction coefficient for direct beam
      REAL KD                 ! extinction coefficient for diffuse radiation
      REAL CANPARSCAT         ! exponentially wtd scattered PAR (umol/m2-s)
      REAL CANPARDIF          ! exponentially wtd diffuse PAR (umol/m2-s)
      REAL PARSHADE           ! PAR on shaded leaves (umol/m2-s)
      REAL PARSUN             ! PAR on sunlit leaves (umol/m2-s)
      REAL LAISUN             ! LAI that is sunlit
      REAL FRACSUN            ! fraction of leaves that are sunlit
      REAL FRACSHADE          ! fraction of leaves that are shaded
      REAL SQALPHA            ! square root of alpha

C######################################

      ALPHA = 0.8
      SQALPHA = SQRT(ALPHA)
      KD = 0.68

C.... CN98 - eqn 15.4, assume x=1

      KBE = 0.5 * SQRT(1. + TAN( ZEN ) * TAN( ZEN ))

C.... CN98 - p. 261 (this is usually small)

      CANPARSCAT = 0.5 * PARDB * ( EXP(-1.* SQALPHA * KBE * TLAI)
     &                           - EXP(-1.* KBE * TLAI) )

C.... CN98 - p. 261 (assume exponentially wtd avg)

      CANPARDIF  = PARDIF * (1. - EXP(-1. * SQALPHA * KD * TLAI) )
     &                    / (SQALPHA * KD * TLAI)

C.... CN98 - p. 261 (for next 3 eqns)
C     note that we use the incoming (not absorbed) PAR

      PARSHADE   = CANPARDIF + CANPARSCAT
      PARSUN     = KBE * PARDB + PARSHADE
      LAISUN     = ( 1. - EXP(-1. * KBE * TLAI) ) / KBE
      FRACSUN    = LAISUN / TLAI
      FRACSHADE  = 1. - FRACSUN

C.... cguen is guenther's eqn for computing light correction as a
C     function of PAR...fracSun should probably be higher since
C     sunlit leaves tend to be thicker than shaded leaves.  But
C     since we need to make crude asmptns regarding leave
C     orientation (x=1), will not attempt to fix at the moment.

      CLNEW =  FRACSUN * CGUEN( PARSUN )
     &       + FRACSHADE * CGUEN( PARSHADE )

      RETURN

      END FUNCTION CLNEW



C-----------------------------------------------------------------
C-----------------------------------------------------------------

      REAL FUNCTION CGUEN( PARTMP )

      IMPLICIT NONE

C.... Function arguments

      REAL, INTENT (IN) ::   PARTMP

      REAL, PARAMETER   ::   ALPHA = 0.001
      REAL, PARAMETER   ::   CL = 1.42

C######################################

      IF ( PARTMP .LE. 0.01) THEN
         CGUEN = 0.0
      ELSE
          CGUEN = (ALPHA * CL * PARTMP)
     &           / SQRT(1. + ALPHA * ALPHA * PARTMP * PARTMP)
      ENDIF

      RETURN

      END FUNCTION CGUEN


        END SUBROUTINE HRBEIS3

