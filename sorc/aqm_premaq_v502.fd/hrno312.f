           SUBROUTINE  HRNO( JDATE, JTIME, NX, NY,
     &                      INITIAL_HOUR,GROWAGNO, NGROWAGNO, NONAGNO,
     &                      TA, RAIN,
     &                      PTYPE, PULSEDATE, PULSETIME,
     &                      EMPOL , MSPCS)

!***********************************************************************
!  subroutine body starts at line  150
!
!  DESCRIPTION:
!  
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions 
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Rainfall    (MCIP derived field) RAIN  (cm)
!
!     FOR PX Version, the Temperature adjustment factor accounts for wet and dry soils
!                and  the precipitation adjustment factor accounts for saturated soils
!     FOR the non-PX version, the basic algorithm remains with a temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!
!     The following arrays are updated after each call to HRNO
!     PTYPE   type of NO emission pulse 
!     PULSEDATE julian date for the beginning of an NO pulse 
!     PULSETIME        time for the beginning of an NO pulse
!  
!     The calculation are based on the following paper by J.J. Yienger and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet and dry soils with
!       the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet and dry adjustment is 
!       calculated at each grid cell.  A linear interpolation between the wet and dry adjustment
!       factor is made using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture is determined by
!       taking the MCIP soil moisture field and dividing by the saturation value defined for each
!       soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total. 
!       THe types of Pulses as described in YL95 were used to estimate the NO emission
!       rate.  
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender       
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and Nitric Oxide Emissions from Agricultural Processes
!       Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!        Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!
C  PRECONDITIONS REQUIRED:
C     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
C     NO emission pulse type, soil moisture from previous time step, julian date
C     of NO emission pulse start, time of NO emission pulse start,
C     soil type, SOIL TYPES, Land use data
C
C  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
C     precip_adj     computes precipitation adjustment factor
C     fertilizer_adj computes fertlizer adjustment factor
C     veg_adj        computes vegatation adjustment factor
C     growseason     computes day of growing season
C     precipfact     computes precip adjustment factor from rainrate and time since pulse initiation
C     pulsetype      determines type & duration of NO emission pulse from rainrate
C     
C      
C
C  REVISION  HISTORY:
C    10/01 : Prototype by GAP
C    01/28/05: by David Wong 
C      -- Parallelized certain loop(s) using OpenMP construct
C    03/08/05  : changed 3rd index on EMPOL to NSEF (bug fix) in declarations
C    03/09/05: by David Wong 
C      -- Reduced number of floating point operation by saving 
C         constant calculations
C 
C***********************************************************************
C
C Project Title: BEIS3 Enhancements for NO emission calculation
C File: hrno.f
C
C
C***********************************************************************

      IMPLICIT NONE
!...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'      ! I/O API constants
        INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
        INCLUDE 'IODECL3.EXT'     ! I/O API function declarations
        INCLUDE 'EMCNST3.EXT'     ! Emissions constants
        INCLUDE 'CONST3.EXT'      ! More constants
        INCLUDE 'B3V11DIMS3.EXT'     ! biogenic-related constants

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INDEX1
        EXTERNAL        INDEX1


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT (IN)  :: JDATE   !  current simulation date (YYYYDDD)
        INTEGER, INTENT (IN)  :: JTIME   !  current simulation time (HHMMSS)
        INTEGER, INTENT (IN)  :: NX      !  no. columns
        INTEGER, INTENT (IN)  :: NY      !  no. rows
        INTEGER, INTENT (IN)  :: MSPCS   !  no. of output species


        REAL, INTENT (IN)  ::  TA    ( NX, NY )    !  air temperature (K)

        REAL, INTENT (IN)  ::  RAIN  ( NX, NY )    !  rainfall rate (cm/ 24 hr)

        INTEGER, INTENT (INOUT) :: PTYPE(NX, NY)      ! 'pulse' type
        INTEGER, INTENT (INOUT) :: PULSEDATE (NX, NY) ! date of pulse start
        INTEGER, INTENT (INOUT) :: PULSETIME (NX, NY) ! date of pulse end
        REAL, INTENT (IN)  ::  GROWAGNO  ( NX, NY )    !  norm NO emissions
        REAL, INTENT (IN)  ::  NGROWAGNO ( NX, NY )    !  norm NO emissions
        REAL, INTENT (IN)  ::  NONAGNO   ( NX, NY )    !  norm NO emissions

        REAL, INTENT (OUT)  ::  EMPOL ( NX, NY, NSEF )      !  output pol emissions (03/08/05 bugfix)
        
        LOGICAL, INTENT (IN) :: INITIAL_HOUR

        CHARACTER*16 :: PROGNAME = 'HRNO'   !  program name
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L      !  counters
        REAL            CFNO         !  NO correction factor
        REAL            CFNOGRASS    !  NO correction factor for grasslands
        REAL            TAIR         ! surface temperature
        REAL            TSOI         ! soil temperature


        LOGICAL       :: USE_SOILT     ! use soil temp rather than estimate as in BEIS2


        INTEGER           :: istatus

        REAL, PARAMETER :: CONST_1 = (1./3.) * (1./30.)
        REAL, SAVE :: CONST_2
        REAL, SAVE :: CONST_3
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

        integer maxstypes
        parameter (maxstypes = 16)

        LOGICAL :: ENVYN
        EXTERNAL ENVYN

        real saturation(maxstypes)

! saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
!       PLEIM-XIU LAND-SURFACE AND PBL MODEL (PX-LSM)
! see JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.

        DATA saturation/0.395,0.410,0.435,0.485,
     &                  0.451,0.420,0.477,0.476,
     &                  0.426,0.482,0.482,0.0,
     &                  0.0,0.0,0.0,0.0/

        REAL          :: CFNOWET, CFNODRY, RATIO

        INTEGER  GROWSEASON
        EXTERNAL GROWSEASON

        CHARACTER*256   MESG
!***********************************************************************
!   begin body of subroutine  HRNO

        IF (FIRSTIME) THEN
           CONST_2 = EXP(-0.103*30.0)
           CONST_3 = EXP(-0.103*30.0)*0.28
           FIRSTIME = .FALSE.
        END IF

        USE_SOILT = .true.  ! use soil temperature in PX version




C........... Find indices from BIOSPC array

 
C...........   loop thru cells

!$omp  parallel do
!$omp& firstprivate(ta, jdate, jtime, initial_hour, ngrowagno)
!$omp& private(tair, c, cfno, tsoi, cfnodry, cfnowet, cfnograss)
!$omp& shared(empol)

            DO R = 1, NY
               DO C = 1, NX
                
                  TAIR = TA( C, R )         ! unit in degree K


C..........    Perform checks on max and min bounds for temperature

                  IF (TAIR .LT. 200.0) THEN

                      WRITE( MESG, 94010 )
     &                 'TAIR=', TAIR,
     &                 'out of range at (C,R)=',  C, R
                      CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

                  END IF

                  IF (TAIR .GT. 315.0 ) THEN
                      WRITE( MESG, 94020 )
     &                 'TAIR=', TAIR,
     &                 'out of range at (C,R)=',  C, R,
     &                 ' resetting to 315K'
                      CALL M3WARN( PROGNAME, JDATE, JTIME, MESG )
                      TAIR = 315.0
                  ENDIF


C............. calculate NO emissions by going thru temperature cases

                        if (growseason(JDATE) .eq. 0) then         !not growing season
                           IF ( TAIR .GT. 303.00 ) TAIR = 303.00

                           IF ( TAIR .GT. 268.8690 ) THEN  
                              CFNO = EXP( 0.04686 * TAIR  -  14.30579 ) ! grass (from BEIS2)
                           ELSE
                              CFNO = 0.0
                           ENDIF

                           EMPOL( C,R, NSEF ) =  
     &                         NGROWAGNO( C,R) * CFNO   !  agriculture
     &                        +  NONAGNO( C,R) * CFNO   !  non-agriculture


                        else

                        TSOI = 0.72*TAIR+82.28
                        IF (TSOI .LE. 273.16) TSOI = 273.16
                        IF (TSOI .GE. 303.16) TSOI = 303.16

C                       CFNODRY = (1./3.)*(1./30.)*(TSOI-273.16)  ! see YL 1995 Equa 9a p. 11452
                        CFNODRY = CONST_1*(TSOI-273.16)  ! see YL 1995 Equa 9a p. 11452
                        IF (TSOI .LE. 283.16) THEN       ! linear cold case
C                          CFNOWET = (TSOI-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
                           CFNOWET = (TSOI-273.16)*CONST_3               ! see YL 1995 Equ 7b
                        ELSE                             ! exponential case
C                          CFNOWET =  EXP(0.103*(TSOI-273.16))
C    &                           *EXP(-0.103*30.0)
                           CFNOWET =  EXP(0.103*(TSOI-273.16))*CONST_2
                        ENDIF
                        CFNO = 0.5*CFNOWET + 0.5*CFNODRY


                           IF ( TAIR .GT. 303.00 ) TAIR = 303.00

                           IF ( TAIR .GT. 268.8690 ) THEN  
                              CFNOGRASS =
     &                        EXP( 0.04686 * TAIR  -  14.30579 ) ! grass (from BEIS2)
                           ELSE
                              CFNOGRASS = 0.0
                           ENDIF

                        EMPOL( C,R, NSEF ) =  MAX((GROWAGNO( C,R) * CFNO 
     &                     *fertilizer_adj(JDATE)*veg_adj(JDATE)
     &                     *precip_adj( JDATE,JTIME,C,R,NX,NY,
     &                                  INITIAL_HOUR,
     &                                 RAIN,PTYPE,PULSEDATE,PULSETIME)),
     &                         (NGROWAGNO( C,R) * CFNOGRASS))                                            
     &                    +  NONAGNO( C,R) * CFNOGRASS            

                        endif


    

               ENDDO
            ENDDO

!$omp end parallel do

       



        RETURN
C...........   Internal buffering formats............ 94xxx


94010   FORMAT( A, F10.2, 1X, A, I3, ',', I3 )
94020   FORMAT( A, F10.2, 1X, A, I3, ',', I3, A )

C***************** CONTAINS ********************************************
        CONTAINS
        real function precip_adj( JDATE,JTIME,C,R,NX,NY,INITIAL_HOUR,
     &                            RAIN,PTYPE, PULSEDATE, PULSETIME)


!***********************************************************************
!  function body starts at line  386
!
!  DESCRIPTION:
!  
!     computes precipitation adjustment factor for estimate of NO emissions 
!     uses  julian day, time, soil moisture
!     requires the use of three arrays that are re-used each time step
!     PTYPE, PULSEDATE, PULSETIME 
!     These arrays store the type of NO pulse initiated by the rainfall
!     and the starting date and time of the pulse.
!     
!      
C
C  PRECONDITIONS REQUIRED:
C     Soil Moisture current time, Soil Moisture previous time,
!     Soil type, Land Use, PTYPE, PULSEDATE, PULSETIME 
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C     precipfact     computes precip adjustment factor from rainrate and time since pulse initiation
C     pulsetype      determines type & duration of NO emission pulse from rainrate
C
C  REVISION  HISTORY:
C    11/01 : Prototype by GAP
C 
C***********************************************************************
        implicit none
        integer NX,NY,C,R,JDATE,JTIME


        REAL   , INTENT (IN)    ::   RAIN(NX,NY)
        
        INTEGER, INTENT (INOUT) ::      PTYPE( NX, NY ) ! 'pulse' type
        INTEGER, INTENT (INOUT) :: PULSEDATE ( NX, NY ) ! date of pulse start
        INTEGER, INTENT (INOUT) :: PULSETIME ( NX, NY ) ! date of pulse end
        LOGICAL, INTENT (IN)    :: INITIAL_HOUR




!
!******* save these variables between calls
! 
        real sat_thres, irfact 
        real saturation(maxstypes)


!
!******* external subprograms
!
        integer getefile
!
!******* for NO calculation only
!
        integer pulsetype
        real precipfact

!
        external getefile, precipfact, pulsetype
    
!
!******* data statements 
!
! 
! saturation values for 11 soil types from pxpbl.F
!
        DATA saturation/0.395,0.410,0.435,0.485,
     &                  0.451,0.420,0.477,0.476,
     &                  0.426,0.482,0.482,0.0,
     &                  0.0,0.0,0.0,0.0/
 
        DATA sat_thres/0.95/
        DATA irfact/2.0/



      
!
!******* summary of algorithm
!        1. compute rate of change of soil moisture from soil moisture
!        2. estimate rainrate from soilmoisture and soil moisture rate
!        3. compute adjustment using pulsetype, rainrate,ptype, and date/time
!             if stronger NO pulse compared to previous time step , then
!             start a new NO emission pulse
!             otherwise continue present NO pulse
!        4. override adjustment for saturated soils 

 
        
         if (INITIAL_HOUR) then

            ptype(C,R) = 0
            pulsedate(C,R) = 0
            pulsetime(C,R) = 0
            precip_adj = 2.0


         else





	      if (pulsetype(RAIN(C,R)) .le. ptype(C, R)) then
!
!   no new rainfall or new rainfall class less than current one
!     
                  precip_adj =                                           
     &                precipfact(ptype(C,R),JDATE,JTIME,                  
     &                           pulsedate(C,R),pulsetime(C,R))
	      else
!
!   rainfall class type increases (NO emission pulse generated)
!
                  pulsedate(C,R) = JDATE
                  pulsetime(C,R) = JTIME
		  ptype(C,R)    = pulsetype(RAIN(C,R))
        
                  precip_adj                                             
     &          = precipfact(ptype(C,R),JDATE,JTIME,pulsedate(C,R),
     &            pulsetime(C,R))		  
	      
	      endif



           endif





        return
        end function precip_adj





        real function fertilizer_adj(date)
!*****************************************************************
!
!  SUMMARY:
!  computes fertilizer adjustment factor from models-3 date format
!
!  FUNCTION CALLS:
!     growseason     computes day of growing season
!
!  NOTE: date = yyyyddd format
!       
!*****************************************************************
        implicit none
        integer date
!
!******** local scratch variables
!
       integer gday
!
!******** function calls
!
       integer growseason
       external growseason
       
!23456
      gday = growseason(date)
      
      if (gday .eq. 0) then
          fertilizer_adj = 0.0
      elseif ((gday .ge. 1) 
     &   .and. (gday .lt. 30)) then ! first month of growing season
          fertilizer_adj = 1.0
      elseif (gday .ge. 30)   then
          fertilizer_adj = 1.0+30.0/184.0-float(gday)/184.0
      else
          write (*,*) 'ERROR: invalid date'
	  write (*,*) 'date = ', date
	  write (*,*) 'growing season day = ',gday
	  stop
      endif
	
      return

      end function fertilizer_adj



      
!23456
      real function veg_adj(date)
!*****************************************************************
!
!  SUMMARY:
!  computes vegetation adjustment factor from models-3 date format
!
!  FUNCTION CALLS:
!     growseason     computes day of growing season
!
!  NOTE: date = yyyyddd format
!       
!*****************************************************************
      implicit none
  
       integer date


!
!******** locals
!
       integer gday

!
!******* function calls
!
       integer  growseason
       external growseason
       
      gday = growseason(date)
      
      if (gday .le. 30) then
          veg_adj = 1.0
      elseif ((gday .gt. 30) 
     &  .and. (gday .lt. 60)) then 
          veg_adj = 1.5-(float(gday)/60.0)
      elseif (gday .ge. 60) then 
          veg_adj = 0.5
      else
          write (*,*) 'ERROR: invalid date'
	  write (*,*) 'date = ', date
	  write (*,*) 'growing season day = ',gday
	  stop
      endif


      return


      end function veg_adj      

      END SUBROUTINE HRNO
!
!
!
      integer function growseason(date)
!*****************************************************************
!
!  SUMMARY:
!  computes day of growing season from models-3 date format
!
!  FUNCTIONS CALLED:
!      JULIAN  (IOAPI function)
!
!  NOTE: date = yyyyddd format
!       
!*****************************************************************
      implicit none       
       integer date

!******* date = yyyyddd
!       
!     
!  given date, compute day of growing season
!     
!         
!
!******** locals

      integer year, month,day
      integer jday, gday
      integer gstart_month, gstart_day, gsjulian_start
      integer gend_month, gend_day, gsjulian_end
!23456


      integer julian
      external julian
      integer gseason_start, gseason_end
       data gseason_start /0401/
       data gseason_end   /1031/    


      year = int(float(date)/1000.0)
      jday = date-year*1000

      gstart_month = int(float(gseason_start)/100.0)
      gstart_day   = gseason_start-gstart_month*100
      gsjulian_start = julian(year,gstart_month,gstart_day)

      gend_month = int(float(gseason_end)/100.0)
      gend_day   = gseason_end-gend_month*100
      gsjulian_end = julian(year,gend_month,gend_day)   
      


	 
      if      ((jday .ge. gsjulian_start) 
     &   .and. (jday .le. gsjulian_end)) then   !  growing season
       
         growseason = jday-gsjulian_start+1

	  
      elseif  ((jday .ge. 1)            ! before or after growing season
     &   .and. (jday .le. 366)) then      
     
         growseason = 0
	 
      else
          write (*,*) 'ERROR: Invalid date '
	  write (*,*) 'date = ',date
	  write (*,*) 'jday = ',jday
	  stop
      endif


      return
      end function growseason





      
       real function precipfact(pulsetype,jdate,jtime, sdate,stime)
!*****************************************************************
!
!  SUMMARY:
!  computes precip adjustment factor from YL 1995 for rainrates
!  pulsetype is an integer 0..3 indicating what type of rainfall rate is used
!
!  FUNCTIONS used:
!    secsdiff (IOAPI)
!
!*****************************************************************
       implicit none
       integer jdate,jtime, sdate,stime, pulsetype, hrdiff
       integer secsdiff
       external secsdiff

       hrdiff = secsdiff(sdate,stime,jdate,jtime)/3600.0
    

       if (pulsetype .eq. 0) then
           precipfact = 1.0
       elseif (pulsetype .eq. 1) then
           if (((hrdiff)/24.) .lt. 2.) then	   
                precipfact = 11.19*exp(-0.805*(hrdiff+24)/24.0)
           else
	        pulsetype = 0
		precipfact = 1.0
	   endif		
       elseif (pulsetype .eq. 2) then
           if (((hrdiff)/24.) .lt. 6.) then	   
                precipfact = 14.68*exp(-0.384*(hrdiff+24)/24.0)
           else
	        pulsetype = 0
		precipfact = 1.0
	   endif       
       else
           if (((hrdiff)/24.) .lt. 13.) then	   
                precipfact = 18.46*exp(-0.208*(hrdiff+24)/24.0)
           else
	        pulsetype = 0
		precipfact = 1.0
	   endif                      
       endif
       return
       end function precipfact
       integer function pulsetype(rainrate)
!*****************************************************************
!
!  SUMMARY:
!  computes pulsetype from rainfall rate (see YL 1995)
!
!*****************************************************************
       implicit none
       real rainrate
       
       if     (rainrate .lt. 0.1) then
          pulsetype = 0	      	      
       elseif (rainrate .lt. 0.5) then
	  pulsetype = 1    
       elseif (rainrate .lt. 1.5) then
          pulsetype = 2
       else
	  pulsetype = 3	 	      
       endif
       
       return
       end function pulsetype



