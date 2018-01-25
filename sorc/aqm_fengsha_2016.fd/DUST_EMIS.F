C     SUBROUTINE DUSTER SUBST_GRID_ID ( QAT,JDATE,MTIME,TSTEP,DX1,DX2 )
      SUBROUTINE DUSTER SUBST_GRID_ID ( QAT,JDATE,MTIME,TSTEP )

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
C  This module calculates wind-blown dust emissions from desert 
C       and agricultural lands based on Owen's Equationa
C  Owen's Equation: F=kAp/gU*(U*^2-U*t^2)
C       Where 
C	U*t=U*ts(Dp)/feff(Zo,zos)f(SM)
C    	Dp-particle size    (um)
C    	Zo-roughness length (m)
C    	SM-soil moisture    (%)
C    	U* --> U=U*/k ln(Z/Zo)
C    	U*=0.4U/ln(10/Zo)
C    	fractional landuse, 
C    	fractional roughness length, 
C    	soil moisture
C    	wind speed
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  History: 
C  12/2006: Standalone version dirven by MM5 output by Daniel Tong (DQT)
C           using prototype written by Shan He;
C  12/2009: Revisions to allow the model with with Eta-NAM met data (DT); 
C           users now have the options to use both MM5 and NAM; 
C  01/2010: Removed DX* from caller (DT);
C           Added options for Soil Type names between MM5 and NAM;
C           Made agricultural dust emissions as optional (by AG_DUST = Y);
C           Replace DREMOIN by TFBELOW to reflect the nature of TF below canopy;
C           Change TFACTOR to TFABOVE in this routine and tfactor.F (now TFBOVE.F)
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C      USE HGRD_DEFN             ! horizontal domain specifications
C      USE SUBST_MODULES         ! stenex

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_IOPARMS     	! I/O parameters definitions
      INCLUDE SUBST_IOFDESC     	! file header data structuer
      INCLUDE SUBST_IODECL      	! I/O definitions and declarations
      INCLUDE SUBST_DUSTCT		! Constant for dust calculation
      INCLUDE SUBST_FILES_ID    	! file name parameters
      INCLUDE SUBST_CONST       	! constants
      
C Arguments:
      INTEGER      JDATE        	! current model date , coded YYYYDDD
      INTEGER      MTIME        	! current model time , coded HHMMSS
      INTEGER      TSTEP        	! time step for output
      REAL DX1, DX2             	! X & Y grid size [m]
      REAL QAT (NCOLS, NROWS)   	! dust emission rate [g/s]

      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      SAVE FIRST

C Parameters:
      INTEGER NLTP        		! number of desert land type BELD3
      PARAMETER ( NLTP = 3 )
      CHARACTER*16 VNMLD( NLTP )
      DATA VNMLD/'USGS_shrubland','USGS_shrubgrass','USGS_sprsbarren'/
      CHARACTER*3 METMOD
      CHARACTER*1 AG_DUST

C Soil type and mapping between MM5 and NAM.
C  MM5 has only 11 erodible soil types (no silt) while NAM has 12; 
C  MM5: Sand,Loamy Sand,Sandy Loam,Silt Loam,     Loam,Sandy Clay Loam, Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C  NAM: Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam, Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
      INTEGER NSTP               	! number of soil type, USGS 17->MM5-PX 11 (no silt)
C     PARAMETER ( NSTP = 11 )		! number of soil types in MM5-PX (11 types)(no silt)
      PARAMETER ( NSTP = 13 )		! number of soil types; If the met model is MM5-PX, there are only 11 types;
      					! Otherwise, twelve soil types are assumed and the rest lumped into Other
      
      INTEGER I, J
      
      REAL SOILML(NSTP, 3)		! Soil moisture limit: 12 types + Other and 3 variables which are 
						! 1-saturation moisture limit 2-field capacity 3-wilting point
C Original values for MM5 PX
C      DATA ((SOILML(I,J), I=1, NSTP), J=1, 3)   
C     > / 0.395,0.410,0.435,0.485,0.451,0.420,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.477,0.476,0.426,0.482,0.482,         ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.135,0.150,0.195,0.255,0.240,0.255,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.322,0.325,0.310,0.370,0.367,         ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.068,0.075,0.114,0.179,0.155,0.175,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.218,0.250,0.219,0.283,0.286/         ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay

C Modified values compatiable with both MM5 & NAM
C  Silt values are based on NAM documentation on soil types;
C  Other includes all types higher than 12. The values of Other, serving as placeholder, are randomly chosen;
C  Values of Other, however, have no effect on dust emissions as the threshold velocity of Other will be high.
      DATA ((SOILML(I,J), I=1, NSTP), J=1, 3)   
     > / 0.395,0.410,0.435,0.485,0.476,0.451,0.420,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.477,0.476,0.426,0.482,0.482, 0.482,        ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     >   0.135,0.150,0.195,0.255,0.361,0.240,0.255,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.322,0.325,0.310,0.370,0.367, 0.367,        ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     >   0.068,0.075,0.114,0.179,0.084,0.155,0.175,   ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.218,0.250,0.219,0.283,0.286,0.286/         ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
      
      REAL UTHFV(NSTP, 3)           ! threshold friction velocity [m/s] for 12 soil types and 3
						! land use types (1-shrubland 2-shrubgrass land 3-barren land)
C... Local (surface) threshold friction velocity from field measurements
C	Ref. Gillette et al., JGR, 1980 for desert soils (J=2).
C       Ref. Gillette, JGR, 1988 for Loose Agr. Soils (J=3);
C	 The following soil types were not measured for desert land (we chose to use agr. data):
C		Sandy Clay Loam,Clay Loam,Sandy Clay,Silty Clay. 
C Original values for MM5 PX
C      DATA ((UTHFV(I,J), I=1, NSTP), J=1, 3)    	
C     > / 0.80, 1.00, 1.40, 1.70, 1.70, 1.70,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   1.70, 1.70, 1.70, 1.70, 1.70,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.42, 0.51, 0.66, 0.34, 0.49, 0.78,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.33, 0.71, 0.71, 0.56, 0.78,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.28, 0.34, 0.29, 1.08, 0.78, 0.78,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.64, 0.71, 0.71, 0.56, 0.54/          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C Modified values compatiable with both MM5 & NAM
C There is no measurement of this value for Silt. The values for Silt are chosen from Silty Loam since
C  the soil composition is close.
C  Other includes all types higher than 12. The values of Other are too high to allow any dust emission;
      DATA ((UTHFV(I,J), I=1, NSTP), J=1, 3)    	
     > / 0.80, 1.00, 1.40, 1.70, 1.70, 1.70, 1.70,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   1.70, 1.70, 1.70, 1.70, 1.70, 9.99,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     >   0.42, 0.51, 0.66, 0.34, 0.34, 0.49, 0.78,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.33, 0.71, 0.71, 0.56, 0.78, 9.99,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     >   0.28, 0.34, 0.29, 1.08, 1.08, 0.78, 0.78,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.64, 0.71, 0.71, 0.56, 0.54, 9.99/          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other

C... Shan He's original values to match up MM5 USTAR 
C      DATA ((UTHFV(I,J), I=1, 11), J=1, 3)
C     > / 0.80, 1.00, 1.40, 1.70, 1.70, 1.70,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   1.70, 1.70, 1.70, 1.70, 1.70,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   1.00, 1.10, 1.80, 1.80, 1.80, 1.80,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   1.80, 1.80, 1.80, 1.80, 1.80,          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.65, 0.75, 0.90, 1.15, 1.15, 1.15,    ! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   1.10, 1.10, 1.00, 1.00, 1.00/          ! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay

C...	Test a uniform threshold friction velocity.
C      DATA ((UTHFV(I,J), I=1, 11), J=1, 3)
C     > / 0.3, 0.3, 0.3, 0.3, 0.3, 0.3,   	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.3, 0.3, 0.3, 0.3, 0.3,          	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.3, 0.3, 0.3, 0.3, 0.3, 0.3,    	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.3, 0.3, 0.3, 0.3, 0.3,			! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     >   0.3, 0.3, 0.3, 0.3, 0.3, 0.3,    	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.3, 0.3, 0.3, 0.3, 0.3/          	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay

C Values for soil composition (clay, silt & sand)
C   Orginal values for 11 MM5 soil types		  
C      REAL CLAYC(NSTP)       			! clay content -- the amount of clay in each soil type
C      DATA (CLAYC(I), I=1, 11)
C     > / 0.03, 0.06, 0.10, 0.13, 0.18, 0.27,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.34, 0.34, 0.42, 0.47, 0.58/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     
C      REAL SILTC(NSTP)          			! silt content -- the amount of silt in each soil type
C      DATA (SILTC(I), I=1, 11)
C     > / 0.05, 0.12, 0.32, 0.70, 0.05, 0.15,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.56, 0.34, 0.06, 0.47, 0.20/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay
C     
C      REAL SANDC(NSTP)      				! sand content -- the amount of sand in each soil type
C      DATA (SANDC(I), I=1, 11)
C     > / 0.92, 0.82, 0.58, 0.17, 0.43, 0.58,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Loam,Sandy Clay Loam
C     >   0.10, 0.32, 0.52, 0.06, 0.22/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay

C   modified values for 12 NAM soil types; Silt values taken from Fig.1 of Gillette et al., 1980.
C   Values of Other are set to zero so no clay is available for fine particle emissions.
      REAL CLAYC(NSTP)       					! clay content -- the amount of clay in each soil type
      DATA (CLAYC(I), I=1, NSTP)
     > / 0.03, 0.06, 0.10, 0.13, 0.06, 0.18, 0.27,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.34, 0.34, 0.42, 0.47, 0.58, 0.00/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     
      REAL SILTC(NSTP)          				! silt content -- the amount of silt in each soil type
      DATA (SILTC(I), I=1, NSTP)
     > / 0.05, 0.12, 0.32, 0.70, 0.84, 0.05, 0.15,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.56, 0.34, 0.06, 0.47, 0.20, 0.00/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other
     
      REAL SANDC(NSTP)      					! sand content -- the amount of sand in each soil type
      DATA (SANDC(I), I=1, NSTP)
     > / 0.92, 0.82, 0.58, 0.17, 0.10, 0.43, 0.58,  	! Sand,Loamy Sand,Sandy Loam,Silt Loam,Silt,Loam,Sandy Clay Loam
     >   0.10, 0.32, 0.52, 0.06, 0.22, 0.00/        	! Silty Clay Loam,Clay Loam,Sandy Clay,Silty Clay,Clay,Other

     
      REAL EROPT(3)            				! erodible potential of soil components 
      DATA (EROPT(I), I=1, 3)/ 0.08, 1.00, 0.12/ 	! EROPT for clay, silt and sand
     
      REAL DMSK(4)            				! erodible potential of soil for each land use type (4) 
      DATA (DMSK(I), I=1, 4)/ 0.50, 0.25, 0.75, 0.75/ ! shrubland shrubgrass barrenland cropland
     
C      REAL PRB(4)               				! probability of U* distribution 
C      DATA (PRB(I), I=1, 4)/ 0.20, 0.28, 0.18, 0.04/ ! 1.10U* 1.35U* 1.75U* 2.30U*
     
C      REAL ADJ(4)              				! U* adjustment
C      DATA (ADJ(I), I=1, 4)/ 1.10, 1.35, 1.75, 2.30/ ! 1.10U* 20% 1.35U* 28% 1.75U* 18% 2.30U*  4%
           
      CHARACTER*16 PRMNM(29)					! Output parameter names
      DATA PRMNM / 'FSHLD','FSHGR','FSPBA','FCROP','QSHLD',
     >             'QSHGR','QSPBA','QCROP','ETOTL','LSHLD',
     >             'LSHGR','LSPBA','LCROP','TLDST','TFABV',
     >             'TFINF','USTAR','USHLD','USHGR','USPBA',
     >             'UCROP','FMOIT','STDEP','KVTHZ','WTMAX',
     >             'ULDBR','ULDVG','ULDUB','ULDFR'/
      
      REAL AMEN							! A value in the Owen's equation
C     PARAMETER ( AMEN = 2.8 )    	     			! values are from 0 to 3.5  mean 2.8
      PARAMETER ( AMEN = 32.0 )               		! values adjusted based on Dale's MNORT measurements

      REAL RADG							! Ratio of air density to gravity
      PARAMETER ( RADG = 122.87 )    			! gs2/m4 122.87->120.8

      REAL FDSA							! Fraction of dust source area to grid cell
C     PARAMETER ( FDSA = 0.5 )    				! factor of dust source area 1/.1/.01
      PARAMETER ( FDSA = 1.0 )                  	! factor of dust source area 1/.1/.01

      REAL GMAD							! Factor to convert geometric to aerodynamic diameter	
      PARAMETER ( GMAD = 0.78 )				! value of GMAD     

 
C Local variables:
 
      INTEGER  M, JJ, KK, LL, JJT   ! loop control for landuse type
      INTEGER  INDEX
      INTEGER  JTIME, KTIME, KDATE
     
      CHARACTER*16 FNAME
      DATA         FNAME   / 'CTM_DUST' /
      
      CHARACTER*16 ENAME
C     DATA         ENAME   / 'BELD01' /
      DATA         ENAME   / 'FLANDA' /

      CHARACTER*16 PNAME
      DATA         PNAME   / 'DUSTER' /

      CHARACTER*16 VNAME
      DATA         VNAME   / ' ' /

      CHARACTER*120 XMSG
      DATA          XMSG   / ' ' /

!      REAL, ALLOCATABLE, SAVE ::  LADUT( :,:,: ) ! fraction of desertland (BELD /100)
!      REAL, ALLOCATABLE, SAVE ::  SLTYP( :,: )   ! soil type (11 USDA category)
!      REAL, ALLOCATABLE, SAVE ::  LAT  ( :,: )   ! latitude (degree north)
!
!      REAL, ALLOCATABLE, SAVE :: AGLAND( :,: )   ! agriculture land fraction
!      REAL, ALLOCATABLE, SAVE :: TF2   ( :,: )   ! transport fraction in and below canopy
!      REAL, ALLOCATABLE, SAVE :: WMAX  ( :,: )   ! max adsorb water fraction
!      REAL, ALLOCATABLE, SAVE :: KVH   ( :,: )   ! ratio of vertical flux / horizontal (K factor)
!      REAL, ALLOCATABLE, SAVE :: SDEP  ( :,: )   ! soil->dust erodiblity potential
!      REAL, ALLOCATABLE, SAVE :: ULAND ( :,:,: ) ! fraction of 4 landtype for dust removal

      REAL LADUT ( MY_NCOLS,MY_NROWS,NLTP ) 	  ! fraction of desertland (BELD /100)
      REAL SLTYP ( MY_NCOLS,MY_NROWS )   	  ! soil type (11 USDA category)
      REAL LAT   ( MY_NCOLS,MY_NROWS )   	  ! latitude (degree north)
      REAL AGLAND( MY_NCOLS,MY_NROWS )   	  ! agriculture land fraction
      REAL TF2   ( MY_NCOLS,MY_NROWS )   	  ! transport fraction in and below canopy
      REAL WMAX  ( MY_NCOLS,MY_NROWS )   	  ! max adsorb water fraction
      REAL KVH   ( MY_NCOLS,MY_NROWS )     	  ! ratio of vertical flux / horizontal (K factor)
      REAL SDEP  ( MY_NCOLS,MY_NROWS )   	  ! soil->dust erodiblity potential
      REAL ULAND ( MY_NCOLS,MY_NROWS,4 ) 	  ! fraction of 4 landtype for dust removal

      REAL FMOIT ( NCOLS, NROWS )       	! factor of soil moisture on U*t
      REAL AA                           	! A in equation
      REAL RN    ( NCOLS, NROWS )       	! preciptation nonconvective
      REAL RC    ( NCOLS, NROWS )       	! preciptation convective
      REAL USTAR ( NCOLS, NROWS )       	! friction velocity [m/s]
      REAL UTSTAR( NCOLS, NROWS, NLTP+1 )	! threshold U* [m/s]
      REAL SOIMT ( NCOLS, NROWS )       	! soil moisture (m3/m3)
      REAL UTEM  ( NCOLS, NROWS, NLTP+1 )	! U term = U*(U*2-Ut*2) [(m/s)3]
      REAL QAI   ( NCOLS, NROWS, NLTP+1 )	! emission for each landuse type [g/m2s]
      REAL QAM   ( NCOLS, NROWS, NLTP+1 )	! emission for each landuse type [g/s]
      REAL ELUS  ( NCOLS, NROWS, NLTP+1 )	! Erodible landuse fraction (0~100)

      REAL WIND10 ( NCOLS, NROWS )       	! wind speed at 10m altitude [m/s]
      REAL ZRUF   ( NCOLS, NROWS )       	! surface roughness length [m]
      REAL TF1    ( NCOLS, NROWS )       	! transport fraction above canopy
      REAL PRMTR  ( NCOLS, NROWS , 29 )  	! output variables
   
      REAL Z, Z0s					! Mid-layer height and smooth roughness length
   
      INTEGER  GXOFF, GYOFF             	! global origin offset from file
      INTEGER  ALLOCSTAT    
        
C External Functions not previously declared in IODECL3.EXT:

      INTEGER      SEC2TIME, TIME2SEC
      EXTERNAL     SEC2TIME, TIME2SEC
      
C      IF(MTIME .GT. 0) THEN
C         JTIME = SEC2TIME(TIME2SEC(MTIME) - TIME2SEC(TSTEP)/2)
C	 ELSE
	 JTIME = MTIME
C	 END IF

C ------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
	  CALL GETENV('METMOD',METMOD)		! Determine which meteorological model is used;

C-----------------------------------------------------------------
C...Open BELD3 file, get fractional landuse

      IF ( .NOT. OPEN3( ENAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '//ENAME//' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. DESC3( ENAME ) ) THEN
         XMSG = 'Could not get '//ENAME//' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C...Open met file, get soil type, soil moisture, fraction velocity

      IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open  MET_CRO_2D file '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open grid file, get latitude
      IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open GRID_CRO_2D file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Get domain decomp info from the BELD3 file
!      CALL SUBHFILE ( ENAME, GXOFF, GYOFF,
!     &                STARTCOL, ENDCOL, STARTROW, ENDROW )

C Get domain decomp info from the MET file
!      CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
!     &                STARTCOLMC2, ENDCOLMC2, STARTROWMC2, ENDROWMC2 )

C Get domain decomp info from the GRID file
!      CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
!     &                STARTCOLMC3, ENDCOLMC3, STARTROWMC3, ENDROWMC3 )
	 
!      IF(((ENDCOLMC2-STARTCOLMC2).GE.NCOLS).OR.
!     >   ((ENDROWMC2-STARTROWMC2).GE.NROWS)) THEN
!         PRINT *, '** DIMENSION TOO SMALL **'
!	 STOP
!	 END IF
	 
C-----------------------------------------------------------------
C Allocatae and initialize array
!      ALLOCATE ( LADUT( MY_NCOLS,MY_NROWS,NLTP ), STAT = ALLOCSTAT )
!      ALLOCATE ( SLTYP( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( LAT( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( AGLAND( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( TF2( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( WMAX( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( KVH( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( SDEP( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
!      ALLOCATE ( ULAND( MY_NCOLS,MY_NROWS,4 ), STAT = ALLOCSTAT )

C Get desert land (fraction) data
      DO M = 1, NLTP
      XMSG = 'Could not read landuse from BELD01'
      IF ( .NOT. INTERPX (ENAME, VNMLD(M), PNAME,
     &    STARTCOL,ENDCOL,STARTROW,ENDROW, 1,1,
     &             JDATE, JTIME, LADUT(1,1,M) ) )
     &  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END DO

C Get soil type data
      IF (METMOD == 'MM5') THEN
        VNAME = 'SLTYP'
      ELSE IF (METMOD == 'NAM') THEN
        VNAME = 'SOTYP'
C	VNAME = 'SLTYP'
      END IF

      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, SLTYP ) ) THEN
         XMSG = 'Could not read '//VNAME//' from MET_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
      
C Get latitude data
      VNAME = 'LAT'
      IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC3,ENDCOLMC3, STARTROWMC3,ENDROWMC3, 1,1,
C5X     &                 JDATE, JTIME, LAT ) ) THEN
     &                 0, 0, LAT ) ) THEN
         XMSG = 'Could not read '//VNAME//' from GRID_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C==================================================================
C Call subroutine cropcal to determine erodible crop land
C   Dust emission from cropland is optional depending on available 
C   crop distribution data. This option becomes less important when
C   the model is applied to dust active region.
      IF (AG_DUST == 'Y') THEN
        CALL CROPCAL SUBST_GRID_ID( JDATE, AGLAND )
      ELSE
        DO C = 1, MY_NCOLS
        DO R = 1, MY_NROWS
           AGLAND(C, R)    =   0.0
	END DO
	END DO	
      END IF

C Call subroutine to calculate the transportable fraction
C  (after in-the-canopy removal) by surface obstacles based
C  on Pace (2005) and Tong et al. (2010).
C     CALL DREMOIN SUBST_GRID_ID( JDATE, TF2, ULAND )
      CALL TFBELOW SUBST_GRID_ID( JDATE, TF2, ULAND )

      DO C = 1, MY_NCOLS
      DO R = 1, MY_NROWS
	 WMAX  (C, R)    =   0.0
	 SDEP  (C, R)    =   0.0
	 KVH   (C, R)    =   0.0
	 
C-- check soil type -- Not used for met model other than MM5
C     IF( SLTYP(C, R).LT.0 .OR. SLTYP(C, R).GT.11 ) THEN
C      IF( SLTYP(C, R).LT.0 .OR. SLTYP(C, R).GT.20 ) THEN
C         PRINT *, ' Soil type incorrect ', SLTYP(C, R)
C	 PRINT *, 'C=', C, ' R=', R
C	 STOP
C      END IF

C-- when not SLTYP = 0 for water
      IF( SLTYP(C, R) .GT. 0 )  THEN   
        JJT = NINT( SLTYP(C, R) )		! Integer of JJ before mapping to USGS
        IF (METMOD == 'NAM') THEN
           JJ = JJT		! NAM USGS soil types
           IF (JJT .GE. 13.0) JJ = 13
        END IF
        IF (METMOD == 'MM5') THEN		! Convert MM5 soil types into USGS types
          IF (JJT .LE. 4.0) THEN
            JJ = JJT
          ELSE
            JJ = JJT + 1
          END IF
        END IF

C calculate maximum amount of the adsorbed water 
C w'=0.0014(%clay)^2+0.17(%clay)
C Fecan et al. 1999 Annales Geophys.,17,144-157.
C...........................................
C         WMAX(C,R)=0.0014*(CLAYC(JJ)*100.)**2+0.17*(CLAYC(JJ)*100.)
         WMAX(C,R)=0.0014*(CLAYC(JJ))**2+0.17*(CLAYC(JJ))
	 
C F/G ratio is a function of clay content 0~20%
C log(F/G)=0.134(%clay)-6
C Marticorena and Bergametti, 1995
C...............................................
	 IF(CLAYC(JJ).LE.0.2) THEN
	    KVH(C, R) = 10.0**(0.134*CLAYC(JJ)*100. - 6.0 )
	 ELSE
	    KVH(C, R) = 2.E-4
	 END IF
	 
C erodibility potential of soil component
C...............................................
         SDEP(C, R) = CLAYC(JJ) * EROPT(1) 
     >           + SILTC(JJ) * EROPT(2) + SANDC(JJ) * EROPT(3)
     
      END IF
      END DO
      END DO
C==================================================================

!      CALL SUBST_BARRIER
      
      IF ( .NOT. OPEN3( FNAME, FSRDWR3, PNAME ) ) THEN
            XMSG = 'Could not open ' // FNAME // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

      END IF  ! first time

C------------------------------------------------------------------
C...Get hourly data

      VNAME = 'USTAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, USTAR ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'ZRUF'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, ZRUF ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C... This part is to convert layer-average U* into surface local U*
C	Therefore, the U*t has to be surface values from measurements.
      Z   = 32          ! Height of Internal Boundary Layer (m)
      Z0s = 0.00001      ! Surface roughness for dust (m), Ref. Marticorena et al. JGR 1997;
      DO C = 1, MY_NCOLS
      DO R = 1, MY_NROWS
         USTAR(C, R) = USTAR(C, R)*( ( LOG(Z*ZRUF(C,R)/ZRUF(C,R)) )/
     &          ( LOG((Z*ZRUF(C,R)/Z0s)) ) )
      END DO
      END DO
C... End of converting

      VNAME = 'WSPD10'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, WIND10 ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
C      VNAME = 'SOIM1'
      VNAME = 'SOILW'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, SOIMT ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'RN'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, RN ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      VNAME = 'RC'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &        STARTCOLMC2,ENDCOLMC2, STARTROWMC2,ENDROWMC2, 1,1,
     &                 JDATE, JTIME, RC ) ) THEN
         XMSG = 'Could not read '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C----------------------------------------------------------------------
C  subroutine calculating transport factor above the canopy
C
C     CALL TFACTOR SUBST_GRID_ID( USTAR, WIND10, TF1, ULAND )
      CALL TFABOVE SUBST_GRID_ID( USTAR, WIND10, TF1, ULAND )
C----------------------------------------------------------------------

C ---------------------------------------------------------------------
C  formula of Draxler & Gillette (2001) Atmos. Environ.
C	F    =  KA(p/g)U*(U*^2-U*t^2)
C
C	F    => emission flux                         (g/m2-s)
C	K    => constant 2.0E-04                      (/m)
C       A    => 0~3.5  mean = 2.8
C       p/g  => ratio of air density to gravity=120.8 (gs2m4)
C	U*   => friction velocity                     (m/s)
C	U*t  => threshold friction velocity           (m/s)
C ---------------------------------------------------------------------
      DO C = 1, MY_NCOLS
      DO R = 1, MY_NROWS
!      DO C = 1, 100
!      DO R = 1, 100
         QAT   (C, R)    =   0.0
	 FMOIT (C, R)    = 999.9
         DO M = 1, 4 
            UTSTAR(C, R, M) = 999.9
	    QAI   (C, R, M) =   0.0
	    QAM   (C, R, M) =   0.0
            ELUS  (C, R, M) =   0.0
            UTEM  (C, R, M) =   0.0
	 END DO
      IF( AGLAND(C, R).LT.0 .OR. AGLAND(C, R).GT.100 .OR.
     >    TF2(C, R).LT.0 .OR. TF2(C, R).GT.1 .OR.
     >    SDEP(C, R).LT.0 .OR. SDEP(C, R).GT.1 .OR.
     >    WMAX(C, R).LT.0 .OR. WMAX(C, R).GT.1 .OR.
     >    KVH(C, R).LT.0 .OR. KVH(C, R).GT.(5.E-4) ) THEN
         PRINT *, '!!! error in duster !!!'
	 STOP
      END IF
C--- dusting when rain < 1/100inch, 1 inch =2.540cm ---
C           when not SLTYP = 0 for water
C......................................................
      IF(( (RN(C, R) + RC(C, R)) .LE. (0.00025) ).AND.
     >   ( SLTYP(C, R) .GT. 0))                 THEN   
                
C         JJ = NINT( SLTYP(C, R) )
         JJ = MIN(NINT( SLTYP(C, R) ), 13)	 

C soil moisture effect on U*t
         IF(SOIMT(C, R).LE.WMAX(C, R)) THEN
	    FMOIT(C, R) = 1.
	 ELSEIF(SOIMT(C, R).LE.SOILML(JJ,1)) THEN
	    FMOIT(C, R) = 
     >     (1.+1.21*(SOIMT(C, R)-WMAX(C, R))**0.68)**0.5
	 END IF
!        WRITE (*,*) "++++++++++++++ Location 9.2"	 
C..............................................................     	 
      IF( SOIMT(C, R) .LE. SOILML(JJ,1) ) THEN

      DO M = 1, 4                                  ! landuse type
         IF(M .LE. 3) THEN                         ! desert land
            ELUS(C,R,M) = LADUT(C,R,M) * DMSK(M)
         ELSE                                      ! crop land
            ELUS(C,R,M) = AGLAND(C,R) * DMSK(M)
         END IF
	        
         IF( ELUS(C, R, M).GT.100 .OR. ELUS(C, R, M).LT.0 )      THEN
            PRINT *, ' Erodible landuse incorrect ', ELUS(C,R,M)
	    PRINT *, 'C=',  C, ' R=', R, ' M=', M
	    STOP
         END IF
C--- dust when erodible land exist and soil moisture small than saturation
         IF( ELUS(C, R, M) .GT. 0 )  THEN
      
!           IF( M .LE. 3 ) KK = 2                          ! only 3 series of U*t
            IF( M .LE. 3 ) KK = 3                          ! Replace desert land U*t with ag land values 
            IF( M .EQ. 4 ) KK = 3                                         ! shrubgrass uses shrubland U*t
C--- adjust U*t based on soil moisture ---
	    UTSTAR(C, R, M) = UTHFV(JJ, KK) * FMOIT(C, R)
!	    PRINT*, UTSTAR(C,R,M)
C--- only when U*>U*t ---
	    IF (USTAR(C, R).GT.UTSTAR(C, R, M)) THEN                      ! wind erosion occurs U* > U*t 
               UTEM(C, R, M)= (USTAR(C, R))
     >            *((USTAR(C, R)* USTAR(C, R))
     >            -(UTSTAR(C, R, M)* UTSTAR(C, R, M)))                    ! m3/s3
!         PRINT*, 'U is gt UT'
!	  PRINT*, USTAR(C,R),UTSTAR(C,R,M)
!	  PRINT*, UTEM(C, R, M) 
               AA = AMEN
!	       IF( M .EQ. 2) AA = AMEN * 0.25                             ! coefficint A only 1/4 for shrubland
               QAI(C, R, M) = GMAD *  AA * RADG * KVH(C,R)
     >                      * UTEM(C, R, M) * SDEP(C, R) * FDSA           ! g/m2s
!               PRINT*,QAI(C,R,M),KVH(C,R),UTEM(C,R,M),SDEP(C,R)

               QAM(C, R, M) = QAI( C, R, M ) 
C     >                      * DX1*DX2 * (ELUS(C, R, M) * 0.01)            ! g/s
     >                      * 12000*12000 * (ELUS(C, R, M) * 0.01)            ! g/s
            
	        END IF                                                        ! DONE IF U* > U*t  
               ELSE
	       END IF                                                           ! DONE IF erodible land    
	 IF(ELUS(C,R,M).EQ.0.AND.QAM(C,R,M).NE.0) THEN
	    PRINT *, '%%% error in duster %%%'
	    STOP
	    END IF
	 	    
         QAT( C, R ) = QAT( C, R ) + QAM( C, R, M )                       ! g/s sum up
	 
      END DO                                                              ! DONE M landuse
C--------------------- dust removal by surrounding vegetation -----------------
C      GOTO 396

       QAT( C, R ) = QAT( C, R ) *TF1(C, R)*TF2(C, R)
  396 CONTINUE

      END IF                                                              ! DONE IF soil moisture     
      END IF                                                              ! DONE IF rain & land      
      END DO                                                              ! DONE R
      END DO                                                              ! DONE C

C      GOTO 789
C ------------------------------------------------------------------
C assigning values in arrays for DUSTOUT file (time dependent)

!      print *, 'JDATE=', JDATE, ' JTIME=', JTIME
C      print *, 'TSTEP=', TSTEP
      
      KDATE = JDATE
!      IF ( JTIME .EQ. 0 ) THEN 
         KTIME = JTIME
!	 ELSE
!	 KTIME = SEC2TIME(TIME2SEC(JTIME) + TIME2SEC(TSTEP))
!            IF (KTIME .EQ. 240000 ) THEN
!	       KTIME = 000000
!	       KDATE = JDATE + 1
!	       END IF
!	 END IF
	 
!      print *, 'KDATE=', KDATE,' KTIME=', KTIME
C      print *, MOD(KTIME, 10000), MY_NCOLS, MY_NROWS

      IF(MOD(KTIME, 10000).EQ.0) THEN
      
      DO C = 1, MY_NCOLS
      DO R = 1, MY_NROWS      
	 PRMTR( C, R,  1 ) = QAI( C, R,  1 )*1.E6     ! g/m2s->ug/m2s
	 PRMTR( C, R,  2 ) = QAI( C, R,  2 )*1.E6     ! g/m2s->ug/m2s
	 PRMTR( C, R,  3 ) = QAI( C, R,  3 )*1.E6     ! g/m2s->ug/m2s
	 PRMTR( C, R,  4 ) = QAI( C, R,  4 )*1.E6     ! g/m2s->ug/m2s
	 PRMTR( C, R,  5 ) = QAM( C, R,  1 )*1.E6     ! g/s->ug/s
	 PRMTR( C, R,  6 ) = QAM( C, R,  2 )*1.E6     ! g/s->ug/s
	 PRMTR( C, R,  7 ) = QAM( C, R,  3 )*1.E6     ! g/s->ug/s
	 PRMTR( C, R,  8 ) = QAM( C, R,  4 )*1.E6     ! g/s->ug/s
	 PRMTR( C, R,  9 ) = QAT(C, R)                ! g/s
	 PRMTR( C, R, 10 ) = ELUS(C, R, 1)            ! shrub
	 PRMTR( C, R, 11 ) = ELUS(C, R, 2)            ! shrubgrass
	 PRMTR( C, R, 12 ) = ELUS(C, R, 3)            ! barren
	 PRMTR( C, R, 13 ) = ELUS(C, R, 4)            ! cropland
	 PRMTR( C, R, 14 ) = LADUT(C, R, 1) + LADUT(C, R, 2)
     >                     + LADUT(C, R, 3) + AGLAND(C, R)
	 PRMTR( C, R, 15 ) = TF1 ( C, R )
	 PRMTR( C, R, 16 ) = TF2 ( C, R )
C        PRMTR( C, R, 16 ) = ELUS( C, R, 1)
	 PRMTR( C, R, 17 ) = USTAR ( C, R )
	 IF(UTSTAR( C, R, 1 ).NE. 999.9) THEN
	    PRMTR( C, R, 18 ) = UTSTAR ( C, R, 1 )
	    ELSE
	    PRMTR( C, R, 18 ) = 0.
	    END IF
	 IF(UTSTAR( C, R, 2 ).NE. 999.9) THEN
	    PRMTR( C, R, 19 ) = UTSTAR ( C, R, 2 )
	    ELSE
	    PRMTR( C, R, 19 ) = 0.
	    END IF
	 IF(UTSTAR( C, R, 3 ).NE. 999.9) THEN
	    PRMTR( C, R, 20 ) = UTSTAR ( C, R, 3 )
	    ELSE
	    PRMTR( C, R, 20 ) = 0.
	    END IF
	 IF(UTSTAR( C, R, 4 ).NE. 999.9) THEN
	    PRMTR( C, R, 21 ) = UTSTAR ( C, R, 4 )
	    ELSE
	    PRMTR( C, R, 21 ) = 0.
	    END IF
	 PRMTR( C, R, 22 ) = FMOIT ( C, R )
	 PRMTR( C, R, 23 ) = SDEP  ( C, R )
	 PRMTR( C, R, 24 ) = KVH   ( C, R )
	 PRMTR( C, R, 25 ) = WMAX  ( C, R )
	 PRMTR( C, R, 26 ) = ULAND  ( C, R, 1 )
	 PRMTR( C, R, 27 ) = ULAND  ( C, R, 2 )
	 PRMTR( C, R, 28 ) = ULAND  ( C, R, 3 )
	 PRMTR( C, R, 29 ) = ULAND  ( C, R, 4 )
	 END DO
	 END DO

      DO INDEX = 1, 29
C        IF ( .NOT. WRITE3 ( FNAME, PRMNM( INDEX ),
         IF ( .NOT. WRITE3 ( FNAME, PRMNM( INDEX ),
     &      KDATE, KTIME, PRMTR( 1,1,INDEX ) ) ) THEN
            XMSG = 'Could not write ' // PRMNM( INDEX ) // ' to '
     &               // FNAME
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
          END DO
	  
       END IF
       
  789 continue
  
C ---------------------------------------------------------------------
C     End of duster subroutine
C ---------------------------------------------------------------------

      RETURN
      END

