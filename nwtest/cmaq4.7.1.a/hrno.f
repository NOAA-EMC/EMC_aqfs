
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/CCTM/src/vdiff/acm2_inline/hrno.f,v 1.3 2008/08/30 13:32:48 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HRNO( JDATE, JTIME, NX, NY, TA,
     &                 PX_VERSION, SOILM, SOILT, ISLTYP,         ! <- PX version
     &                 INITIAL_DAY, RAIN, GROWAGNO, NGROWAGNO, NONAGNO, 
     &                 PTYPE, PULSEDATE, PULSETIME, EMPOL )

C-----------------------------------------------------------------------
C Description:
   
C    Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
C    to estimate NO emissions 
C    Information needed to estimate NO emissions:
C       Julian Day          (integer)    JDATE
C       Surface Temperature (MCIP field) TA    (K)
C       Rainfall    (MCIP derived field) RAIN  (cm)
C       Soil Moisture       (MCIP field) SOILM (M**3/M**3) (PX_VERSION)
C            (ratio of volume of water per volume of soil)
C       Soil Temperature    (MCIP field) SOILT (K)         (PX_VERSION)
C       Soil Type           (MCIP field) ISLTYP            (PX_VERSION)
C       Saturation values for soil types (constants)       (PX_VERSION)
C    FOR PX Version, the Temperature adjustment factor accounts for wet and dry
C    soils and the precipitation adjustment factor accounts for saturated soils
C    FOR the non-PX version, the basic algorithm remains with a temperature
C    adjustment factor (dry soil) and no adjustment for saturated soils
 
C    The following arrays are potentially updated after a call to HRNO:
C       PTYPE     type of NO emission pulse 
C       PULSEDATE julian date for the beginning of an NO pulse 
C       PULSETIME        time for the beginning of an NO pulse
   
C    The calculation are based on the following paper:
C    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,
C    11447-11464, 1995
 
C    The Temperature Adjustment Factor is based on section 4.2 for wet and dry
C    soils with the following modification (PX version):
C       Instead of classifying soils as either 'wet' or 'dry', the wet and dry
C       adjustment is calculated at each grid cell.  A linear interpolation between
C       the wet and dry adjustment factor is made using the relative amount of soil
C       moisture in the top layer (1cm) as the interpolating factor.  The relative
C       amount of soil moisture is determined by taking the MCIP soil moisture field
C       and dividing by the saturation value defined for each soil type in the PX
C       version of MCIP the soil temperature is used in PX version
 
C    The Precipation Adjustment factor is based on section 4.1 with the following
C    modifications:
C       The rainrate is computed from the MCIP directly using a 24 hr daily total. 
C       The types of Pulses as described in YL95 were used to estimate the NO
C       emission rate.  
 
C    Also see the following paper for more information:
C    Proceedings of the Air and Waste Management Association/U.S. Environmental
C    Protection Agency EMission Inventory Conference, Raleigh October 26-28, 1999
C    Raleigh NC by Tom Pierce and Lucille Bender       
 
C    References:
 
C    Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.
C    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,
C    11447-11464, 1995
C    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and
C    Nitric Oxide Emissions from Agricultural Processes Proceedings of the Air and
C    Waste Management Association/U.S. Environmental Protection Agency Emission
C    Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC

C Preconditions:
C     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
C     NO emission pulse type, soil moisture from previous time step, julian date
C     of NO emission pulse start, time of NO emission pulse start,
C     soil type, SOIL TYPES, Land use data
 
C Subroutines and Functions Called (directly or indirectly):
C     PRECIP_ADJ     computes precipitation adjustment factor
C     FERTILIZER_ADJ computes fertlizer adjustment factor
C     VEG_ADJ        computes vegatation adjustment factor
C     GROWSEASON     computes Julian day of growing season
C     PRECIPFAC      computes precip adjustment factor from rainfall in last 24 hrs
C                    and time since pulse initiation
C     PULSETYPE      determines type & duration of NO emission pulse from rainrate
      
C Revision History:
C    10/01 : Prototype by GAP
C    10/03 : modified transition to non growing season for jul-oct of the year
C    08/04 : Converted to SMOKE code style by C Seppanen
C    Mar 07: Restructure; J.Young
  
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: hrno.f,v 1.3 2008/08/30 13:32:48 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/work/rep/CCTM/src/vdiff/acm2_inline/hrno.f,v $
C Last updated: $Date: 2008/08/30 13:32:48 $ 
C-----------------------------------------------------------------------

      USE BIOG_EMIS, ONLY: NSEF

      IMPLICIT NONE
        
C Includes:

C Arguments:
      INTEGER, INTENT( IN )    :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )    :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN )    :: NX      ! no. columns
      INTEGER, INTENT( IN )    :: NY      ! no. rows

      REAL,    INTENT( IN )    :: TA       ( NX,NY )   ! air temperature [K]
      LOGICAL, INTENT( IN )    :: PX_VERSION           ! true: using PX version of MCIP
      REAL,    INTENT( IN )    :: SOILM    ( NX,NY )   ! soil moisture [m3/m3] (PX)
      REAL,    INTENT( IN )    :: SOILT    ( NX,NY )   ! soil temperature [K] (PX)
      REAL,    INTENT( IN )    :: ISLTYP   ( NX,NY )   ! soil type (PX)
      LOGICAL, INTENT( IN )    :: INITIAL_DAY          ! true: 1st 24 hours
      REAL,    INTENT( IN )    :: RAIN     ( NX,NY )   ! rainfall rate [cm/24hr]
      REAL,    INTENT( IN )    :: GROWAGNO ( NX,NY )   ! norm NO emissions
      REAL,    INTENT( IN )    :: NGROWAGNO( NX,NY )   ! norm NO emissions
      REAL,    INTENT( IN )    :: NONAGNO  ( NX,NY )   ! norm NO emissions
        
      INTEGER, INTENT( INOUT ) :: PTYPE    ( NX,NY )   ! 'pulse' type
      INTEGER, INTENT( INOUT ) :: PULSEDATE( NX,NY )   ! date of pulse start
      INTEGER, INTENT( INOUT ) :: PULSETIME( NX,NY )   ! time of pulse start

      REAL,    INTENT( OUT )   :: EMPOL( NX,NY,NSEF )  ! output pol emissions

C Parameters:
      INTEGER, PARAMETER :: MAXSTYPES = 11
!     REAL,    PARAMETER :: EFAC = EXP( -0.103 * 30.0 )
      REAL,    SAVE      :: EFAC
      REAL,    PARAMETER :: CFNODRYFC = ( 1.0 / 3.0 ) * ( 1.0 / 30.0 )
        
C Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
C Pleim-Xiu Land-Surface and PBL Model (PX-LSM)
C See Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.

      REAL :: SATURATION( MAXSTYPES ) =
     &                  (/ 0.395, 0.410, 0.435, 0.485,
     &                     0.451, 0.420, 0.477, 0.476,
     &                     0.426, 0.482, 0.482 /)

C Local Variables:

      INTEGER R, C, L                    ! counters
      INTEGER SOILCAT                    ! soil category
      
      REAL    CFNO                       ! NO correction factor
      REAL    CFNOGRASS                  ! NO correction factor for grasslands
      REAL    TAIR                       ! surface temperature
      REAL    TSOI                       ! soil temperature
      REAL    CFNOWET, CFNODRY, RATIO
      REAL    FAC1, FAC2, FAC3, FAC4

      LOGICAL, SAVE :: USE_SOILT = .TRUE. ! use soil temperature in PX version
                                          ! rather than estimate as in BEIS2

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 256 ) :: MESG           ! message buffer
      CHARACTER( 16 )  :: PNAME = 'HRNO' ! procedure name

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         EFAC = EXP( -0.103 * 30.0 )
      END IF
 
C Loop through cells
      DO R = 1, NY
         DO C = 1, NX
            
            TAIR = TA( C,R )   ! unit in degree K

C Check max and min bounds for temperature
            IF ( TAIR .LT. 200.0 ) THEN
               WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &              'out of range at (C,R)=', C, R
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF

            IF ( TAIR .GT. 303.0 ) THEN
               IF ( TAIR .GT. 315.0 ) THEN
                  WRITE( MESG, 94020 ) 'TAIR=', TAIR,
     &                 'out of range at (C,R)=', C, R,
     &                 ' resetting to 303K'
                  CALL M3WARN( PNAME, JDATE, JTIME, MESG )
               END IF
               TAIR = 303.0
            END IF

            IF ( GROWSEASON( JDATE ) == 0 ) THEN   ! not growing season

               IF ( TAIR .GT. 268.8690 ) THEN  
                  CFNO = EXP( 0.04686 * TAIR - 14.30579 ) ! grass (from BEIS2)
               ELSE
                  CFNO = 0.0
               END IF

               EMPOL( C,R,NSEF ) = CFNO * ( NGROWAGNO( C,R )  ! agriculture
     &                                  +   NONAGNO( C,R ) )  ! non-agriculture

            ELSE   ! growing season
C Calculate NO emissions by going thru temperature cases

               IF ( PX_VERSION .AND. USE_SOILT ) THEN
                  TSOI = SOILT( C,R )
               ELSE
                  TSOI = 0.72 * TAIR + 82.28
               END IF
               TSOI = MIN( MAX( TSOI, 273.16 ), 303.16 ) - 273.16   ! deg C

C see YL 1995 Eqn 9a p. 11452
               CFNODRY = CFNODRYFC * TSOI

               IF ( TSOI .LE. 10.0 ) THEN        ! linear cold case
C see YL 1995 Eqn 7b
                  CFNOWET = 0.28 * EFAC * TSOI
               ELSE                            ! exponential case
                  CFNOWET = EFAC * EXP( 0.103 * TSOI )
               END IF

               IF ( TAIR .GT. 268.8690 ) THEN  
                  CFNOGRASS = EXP( 0.04686 * TAIR - 14.30579 ) ! grass (from BEIS2)
               ELSE
                  CFNOGRASS = 0.0
               END IF

               IF ( .NOT. PX_VERSION ) THEN

                  CFNO = 0.5 * ( CFNOWET + CFNODRY )

                  FAC1 = GROWAGNO( C,R ) * CFNO
     &                 * FERTILIZER_ADJ( JDATE )
     &                 * VEG_ADJ( JDATE )
     
                  IF ( INITIAL_DAY ) THEN
                     FAC2 = 1.0
                     PTYPE( C,R ) = 0
                     PULSEDATE( C,R ) = 0
                     PULSETIME( C,R ) = 0
                  ELSE
                     FAC2 = PRECIP_ADJ( JDATE, JTIME, RAIN( C,R ),
     &                                  PTYPE( C,R ), PULSEDATE( C,R ),
     &                                  PULSETIME( C,R ) )
                  END IF

               ELSE   ! using PX version of MCIP    

                  SOILCAT = INT( ISLTYP( C,R ) )
                  IF ( SOILCAT .LE. MAXSTYPES ) THEN
                     RATIO = SOILM( C,R ) / SATURATION( SOILCAT )
                     CFNO = CFNODRY + RATIO * ( CFNOWET - CFNODRY )
                  ELSE
                     CFNO = 0.0
                  END IF

                  FAC1 = GROWAGNO( C,R ) * CFNO
     &                 * FERTILIZER_ADJ( JDATE )
     &                 * VEG_ADJ( JDATE )
                  FAC2 = PRECIP_ADJ_PX( JDATE, JTIME, RAIN( C,R ),
     &                                  SOILM( C,R ), ISLTYP( C,R ),
     &                                  PTYPE( C,R ), PULSEDATE( C,R ),
     &                                  PULSETIME( C,R ) )

               END IF  ! PX version check

               FAC3 = NGROWAGNO( C,R ) * CFNOGRASS 
               FAC4 = NONAGNO( C,R ) * CFNOGRASS

               EMPOL( C,R,NSEF ) = MAX( ( FAC1 * FAC2 ), FAC3 ) + FAC4

            END IF  ! growing season check
         END DO  ! loop over columns
      END DO  ! loop over rows

94010 FORMAT( A, F10.2, 1X, A, I3, ',', I3 )
94020 FORMAT( A, F10.2, 1X, A, I3, ',', I3, A )

      RETURN

C-----------------------------------------------------------------------

      CONTAINS

         REAL FUNCTION PRECIP_ADJ_PX( JDATE, JTIME, RAIN, SOILM, ISLTYP,
     &                                PTYPE, PULSEDATE, PULSETIME )

C-----------------------------------------------------------------------
 
C Description:
   
C    Compute precipitation adjustment factor for estimate of NO emissions 
C    Uses: julian day, time, soil moisture
C    Requires the use of three arrays that are re-used each time step:
C    PTYPE, PULSEDATE, PULSETIME 
C    These arrays store the type of NO pulse initiated by the rainfall
C    and the starting date and time of the pulse.
 
C Preconditions:
C    Soil Moisture current time, Soil Moisture previous time,
C    Soil type, Land Use, PTYPE, PULSEDATE, PULSETIME 
 
C Subroutines and Functions Called:
C    precipfact - computes precip adjustment factor from rainrate and time
C                 since pulse initiation
C    pulsetype  - determines type & duration of NO emission pulse from rainrate
 
C Revision History:
C    11/01 : Prototype by GAP
C    3/05  : create separate functions for PX vs non-PX versions
C-----------------------------------------------------------------------

         IMPLICIT NONE

C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME
         REAL,    INTENT( IN )    :: RAIN
         REAL,    INTENT( IN )    :: SOILM     ! only avilable if PX version
         REAL,    INTENT( IN )    :: ISLTYP    ! only available if PX version
         INTEGER, INTENT( INOUT ) :: PTYPE     ! pulse type
         INTEGER, INTENT( INOUT ) :: PULSEDATE ! date of pulse start
         INTEGER, INTENT( INOUT ) :: PULSETIME ! date of pulse end

C External functions:
         INTEGER, EXTERNAL :: GETEFILE
!        INTEGER, EXTERNAL :: PULSETYPE
!        REAL,    EXTERNAL :: PRECIPFAC
         
C Parameters:
         REAL, PARAMETER :: SAT_THRES = 0.95

C Local variables:
         INTEGER SOILCAT     ! soil type category
         INTEGER PTYPE_TEST

C-----------------------------------------------------------------------

C Summary of algorithm
C   1. compute rate of change of soil moisture from soil moisture
C   2. estimate rainrate from soil moisture and soil moisture rate
C   3. compute adjustment using pulsetype, rainrate, ptype, and date/time
C        if stronger NO pulse compared to previous time step, then
C        start a new NO emission pulse,
C        otherwise continue present NO pulse
C   4. override adjustment for saturated soils 

         SOILCAT = INT( ISLTYP )

         IF ( INITIAL_DAY ) THEN

            PTYPE = 0
            PULSEDATE = 0
            PULSETIME = 0
            IF ( SOILCAT .LE. MAXSTYPES ) THEN
               PRECIP_ADJ_PX = 2.0
            ELSE
               PRECIP_ADJ_PX = 1.0
            END IF

         ELSE

C           IF ( PULSETYPE( RAIN ) <= PTYPE ) THEN
C No new rainfall or new rainfall class less than current one

            PTYPE_TEST = PULSETYPE( RAIN )
            IF ( PTYPE_TEST .GT. PTYPE ) THEN
C Rainfall class type increases (NO emission pulse generated)
               PULSEDATE = JDATE
               PULSETIME = JTIME
               PTYPE = PTYPE_TEST
            END IF

            PRECIP_ADJ_PX = PRECIPFAC( JDATE, JTIME, PULSEDATE, PULSETIME,
     &                                 PTYPE )

            IF ( SOILCAT .LE. MAXSTYPES .AND.
     &           SOILM .GE. SAT_THRES * SATURATION( SOILCAT ) ) THEN
               PRECIP_ADJ_PX = 0.0
            END IF

         END IF

         RETURN
         
         END FUNCTION PRECIP_ADJ_PX
         
C-----------------------------------------------------------------------

         REAL FUNCTION PRECIP_ADJ( JDATE, JTIME, RAIN,
     &                             PTYPE, PULSEDATE, PULSETIME )

C-----------------------------------------------------------------------
C Description:
   
C    Compute precipitation adjustment factor for estimate of NO emissions 
C    Uses: julian day, time, soil moisture
C    Requires the use of three arrays that are re-used each time step:
C    PTYPE, PULSEDATE, PULSETIME 
C    These arrays store the type of NO pulse initiated by the rainfall
C    and the starting date and time of the pulse.
 
C Preconditions:
C    Soil Moisture current time, Soil Moisture previous time,
C    Soil type, Land Use, PTYPE, PULSEDATE, PULSETIME 
 
C Subroutines and Functions Called:
C    precipfact - computes precip adjustment factor from rainrate and time
C                 since pulse initiation
C    pulsetype  - determines type & duration of NO emission pulse from rainrate
 
C Revision History:
C    11/01 : Prototype by GAP
C    3/05  : created a non-PX version of this function 
  
C-----------------------------------------------------------------------

         IMPLICIT NONE

C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME
         REAL,    INTENT( IN )    :: RAIN
         INTEGER, INTENT( INOUT ) :: PTYPE     ! pulse type
         INTEGER, INTENT( INOUT ) :: PULSEDATE ! date of pulse start
         INTEGER, INTENT( INOUT ) :: PULSETIME ! time of pulse start

C External functions:
         INTEGER, EXTERNAL :: GETEFILE

C Local variable
         INTEGER PTYPE_TEST

C-----------------------------------------------------------------------

C Summary of algorithm
C    1. if no rainfall or new rainfall class less than current one, continue
C       existing NO emission pulse
C    2. if new rainfall that increases rainfall class, then create new NO
C       emission pulse using pulsetype, rainrate, ptype, and date/time -
C       if stronger NO pulse compared to previous time step, then start
C       a new NO emission pulse

C        IF ( PULSETYPE( RAIN ) <= PTYPE ) THEN
C No new rainfall or new rainfall class less than current one

         PTYPE_TEST = PULSETYPE( RAIN )
         IF ( PTYPE_TEST .GT. PTYPE ) THEN
C Rainfall class type increases (NO emission pulse generated)
            PULSEDATE = JDATE
            PULSETIME = JTIME
            PTYPE = PTYPE_TEST
         END IF

         PRECIP_ADJ = PRECIPFAC( JDATE, JTIME, PULSEDATE, PULSETIME, PTYPE )

         RETURN
         
         END FUNCTION PRECIP_ADJ

C-----------------------------------------------------------------------

         REAL FUNCTION FERTILIZER_ADJ( DATE )

C Compute a fertilizer adjustment factor for the given date in yyyyddd format.
C If it is not growing season, the adjustment factor is 0; otherwise, it
C ranges from 0.0 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE

C External functions:
!        INTEGER, EXTERNAL :: GROWSEASON

C Local variables:
         INTEGER  GDAY

C-----------------------------------------------------------------------

         GDAY = GROWSEASON( DATE )
         
         IF ( GDAY .EQ. 0 ) THEN
            FERTILIZER_ADJ = 0.0
         ELSE IF ( GDAY .GE. 1 .AND. GDAY .LT. 30 ) THEN  ! first month of growing season
            FERTILIZER_ADJ = 1.0
         ELSE IF ( GDAY .GE. 30 ) THEN
            FERTILIZER_ADJ = 1.0 + 30.0 / 184.0 - FLOAT( GDAY ) / 184.0
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'growing season day = ', GDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )
         
         RETURN
         
         END FUNCTION FERTILIZER_ADJ

C-----------------------------------------------------------------------

         REAL FUNCTION VEG_ADJ( DATE )

C Compute a vegetation adjustment factor for the given date in yyyyddd format.
C The adjustment factor ranges from 0.5 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE

C External functions:
!        INTEGER, EXTERNAL :: GROWSEASON

C Local variables:
         INTEGER  GDAY

C-----------------------------------------------------------------------

         GDAY = GROWSEASON( DATE )
         
         IF ( GDAY .LE. 30 ) THEN
            VEG_ADJ = 1.0
         ELSE IF ( GDAY .GT. 30 .AND. GDAY .LT. 60 ) THEN
            VEG_ADJ = 1.5 - ( FLOAT( GDAY ) / 60.0 )
         ELSE IF ( GDAY .GE. 60 ) THEN
            VEG_ADJ = 0.5
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'growing season day = ', GDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )

         RETURN
      
         END FUNCTION VEG_ADJ
         
C-----------------------------------------------------------------------

         INTEGER FUNCTION GROWSEASON( DATE )

C Compute the day of the growing season corresponding to the given date
C in yyyyddd format.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE   ! YYYYDDD

C External functions:
         INTEGER, EXTERNAL :: JULIAN

C Parameters:
         INTEGER, PARAMETER :: GSTART_MONTH  = 04     ! April
         INTEGER, PARAMETER :: GSTART_DAY    = 01
         INTEGER, PARAMETER :: GEND_MONTH    = 10     ! October
         INTEGER, PARAMETER :: GEND_DAY      = 31

C Local variables:
         INTEGER  YEAR, MONTH, DAY
         INTEGER  JDAY, GDAY
         INTEGER  GSJULIAN_START
         INTEGER  GSJULIAN_END
         
C-----------------------------------------------------------------------

         YEAR = INT( FLOAT( DATE ) / 1000.0 )
         JDAY = DATE - YEAR * 1000
         
         GSJULIAN_START = JULIAN( YEAR, GSTART_MONTH, GSTART_DAY )
         GSJULIAN_END = JULIAN( YEAR, GEND_MONTH, GEND_DAY )
         
         IF ( JDAY .GE. GSJULIAN_START .AND. JDAY .LE. GSJULIAN_END ) THEN
            GROWSEASON = JDAY - GSJULIAN_START + 1  ! growing season
         ELSE IF ( JDAY .GE. 1 .AND. JDAY .LE. 366 ) THEN
            GROWSEASON = 0                          ! before or after growing season
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'jday = ', JDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )

         RETURN

         END FUNCTION GROWSEASON

C-----------------------------------------------------------------------

         REAL FUNCTION PRECIPFAC( JDATE, JTIME, PDATE, PTIME, PTYPE )

C Compute a precipitation adjustment factor from a previous 24 hour rainfall
C based on YL 1995
C The pulse type is an integer ranging from 0 to 3 indicating the type of
C rainfall rate:
C If rainfall < 0.1 cm in last 24 hr, "reset"
C Else if rainfall < 0.5 cm in last 24 hr, and time since last pulse is .ge. 2 days,
C    reset; else, precipfact=11.19*...
C Else if rainfall < 1.5 cm in last 24 hr, and time since last pulse is .ge. 6 days,
C    reset; else, precipfact=14.68*...
C Else if rainfall >=1.5 cm in last 24 hr, and time since last pulse is .ge. 13 days,
C    reset; else, precipfact=18.46*...

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME, PDATE, PTIME
         INTEGER, INTENT( INOUT ) :: PTYPE
         
C External functions:
         INTEGER, EXTERNAL :: SECSDIFF

C Parameters:
         REAL, PARAMETER :: DAYPERSEC = 1.0 / ( 24.0 * 3600.0 ) ! = 0.000011574074074

C Local variables:
         REAL DAYDIFF, DAYDIF1
         
C-----------------------------------------------------------------------

         DAYDIFF = FLOAT( SECSDIFF( PDATE, PTIME, JDATE, JTIME ) ) * DAYPERSEC
         DAYDIF1 = DAYDIFF + 1.0
         
         SELECT CASE( PTYPE )
         CASE( 0 )
            PRECIPFAC = 1.0
         CASE( 1 )
            IF ( ( DAYDIFF ) .LT. 2.0 ) THEN
               PRECIPFAC = 11.19 * EXP( -0.805 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE( 2 )
            IF ( ( DAYDIFF ) .LT. 6.0 ) THEN
               PRECIPFAC = 14.68 * EXP( -0.384 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE( 3 )
            IF ( ( DAYDIFF ) .LT. 13.0 ) THEN
               PRECIPFAC = 18.46 * EXP( -0.208 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE DEFAULT
            WRITE( MESG,'( A, I6 )' ) 'Invalid Pulse Type specified ',
     &                                 PTYPE
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END SELECT
         
         RETURN
         
         END FUNCTION PRECIPFAC
    
C-----------------------------------------------------------------------

         INTEGER FUNCTION PULSETYPE( RAINFALL )

C Compute the pulse type from the rainfall rate (see YL 1995).

         IMPLICIT NONE
         
C Function arguments
         REAL, INTENT( IN ) :: RAINFALL   ! [cm/24hr]
         
C-----------------------------------------------------------------------

         IF ( RAINFALL .LT. 0.1 ) THEN
            PULSETYPE = 0
         ELSE IF ( RAINFALL .LT. 0.5 ) THEN
            PULSETYPE = 1
         ELSE IF ( RAINFALL .LT. 1.5 ) THEN
            PULSETYPE = 2
         ELSE
            PULSETYPE = 3
         END IF
         
         RETURN
         
         END FUNCTION PULSETYPE

C-----------------------------------------------------------------------

      END SUBROUTINE HRNO

