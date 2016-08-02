      PROGRAM DUST_MERGE
     
      IMPLICIT NONE 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C   19 SEP 07 D.Tong:   prototype -- Allocate the total dust emissions into  
C			  individual species.
C   19 JUN 14 D.Tong:   Implemented two changes: a) splitting ratio b/w fine
C                         and coarse particles from dust emission; b) vertical 
C                         distribution of dust emission in the first hour.                   
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C Includes:
C       INCLUDE 'PARMS3.EXT'
C       INCLUDE 'FDESC3.EXT'
C       INCLUDE 'IODECL3.EXT_Tong'
C       INCLUDE 'DUST_CONST.EXT'         ! Constants for dust emissions only
C       INCLUDE 'FILE3_CTM.EXT'          ! File names
      INCLUDE SUBST_IOPARMS             ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC             ! file header data structuer
      INCLUDE SUBST_IODECL              ! I/O definitions and declarations
      INCLUDE SUBST_DUSTCT              ! Constant for dust calculation
      INCLUDE SUBST_FILES_ID            ! file name parameters
      INCLUDE SUBST_CONST               ! constants
!      INCLUDE SUBST_XSTAT              ! M3EXIT status codes
C      INCLUDE SUBST_PACTL_ID           ! PA control parameters
!      INCLUDE SUBST_PE_COMM            ! PE communication displacement and direction


C      MODULE AERO_EMIS

C      USE GRID_CONF           ! horizontal & vertical domain specifications
       
C      INCLUDE SUBST_AE_EMIS   ! aerosol emission surrogate names and map table
            
C Array dimensions - Inherit from AERO_EMIS
C      INTEGER, PARAMETER :: NSSDIAG = 11   ! number of species in sea-salt
C                                           ! diagnostic emission file
C      INTEGER, PARAMETER :: NSSSPC = 4     ! number of chemical species in
C                                           ! fresh sea-salt aerosol
C      INTEGER, PARAMETER :: NSSMOD = 3     ! number of lognormal modes in
C                                           ! sea-salt aerosol

C *** ARGUMENTS
      CHARACTER (100) :: XMSG, PNAME, VNAME
      INTEGER ALLOCSTAT, I, J, K, ISTEP
      INTEGER JDATE           ! current model date, coded YYYYDDD
      INTEGER JTIME           ! current model time, coded HHMMSS
      INTEGER TSTEP( 2 )      ! time step vector (HHMMSS)
                                 ! TSTEP(1) = local output step
                                 ! TSTEP(2) = sciproc sync. step (chem)
      INTEGER EMISLYRS        ! number of vertical layers on emissions file

C *** LOCAL VARIABLES
      REAL, ALLOCATABLE, SAVE :: DUST ( :,: )  ! Total dust emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PSO4 ( :,:,: )  ! Particulate SO4 emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PNO3 ( :,:,: )  ! Particulate NO3 emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PEC  ( :,:,: )  ! Particulate EC emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: POA  ( :,:,: )  ! Particulate OC emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PMF  ( :,:,: )  ! Other fine PM emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PMC  ( :,:,: )  ! Coarse PM emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: PM10 ( :,:,: )  ! PM10 emissions [g/s]
      REAL, ALLOCATABLE, SAVE :: TEMP ( :,:,: )! Temporary space

C     Speciation factors for coarse mode emissions, from paragraph #15 of
C     Binkowski & Roselle (2003)
C      REAL, PARAMETER :: FAC_DUST = 0.90   ! (fugitive dust)/PMC
C      REAL, PARAMETER :: FAC_OTHER = 0.10  ! (non-fugitive dust)/PMC

C     Speciation factors for dust emissions
C	Daniel Tong (2007-09-20)
C	Ref. Pelt & Zoback, Environ. Monitor. Assess., 2007 for chemical
C	  Speciation;
C	Ref. Cheng et al., Atmos. Environ. 2005 for size distribution;
C       REAL, PARAMETER :: FAC_PSO4 = 0.0000773
C       REAL, PARAMETER :: FAC_PNO3 = 0.0000154
C       REAL, PARAMETER :: FAC_PEC  = 0.0000953         
C       REAL, PARAMETER :: FAC_POA  = 0.000638
C       REAL, PARAMETER :: FAC_PMF  = 0.45 
C       REAL, PARAMETER :: FAC_PMC  = 0.55
C       REAL, PARAMETER :: FAC_PM10 = 1.00

C      Speciation PMFINE to CMAQ502 species based on PMFINE 5% assumption
       REAL, PARAMETER :: FAC_PSO4 = 0.0
       REAL, PARAMETER :: FAC_PNO3 = 0.0
       REAL, PARAMETER :: FAC_PEC  = 0.0
       REAL, PARAMETER :: FAC_POA  = 0.0
       REAL, PARAMETER :: FAC_PMF  = 0.022265
       REAL, PARAMETER :: FAC_PAL  = 0.0028475
       REAL, PARAMETER :: FAC_PCA  = 0.00397
       REAL, PARAMETER :: FAC_PCL  = 0.00532       
       REAL, PARAMETER :: FAC_PFE  = 0.0016775
       REAL, PARAMETER :: FAC_PK  = 0.001885       
       REAL, PARAMETER :: FAC_PNA  = 0.0019675
       REAL, PARAMETER :: FAC_PNCOM  = 0.000215       
       REAL, PARAMETER :: FAC_PSI  = 0.0097125
       REAL, PARAMETER :: FAC_PTI  = 0.00014                      
       REAL, PARAMETER :: FAC_PMC  = 0.95
       REAL, PARAMETER :: FAC_PM10 = 1.00


C     Spcieation for toxic metals.
C       Ref. Pelt & Zoback, Environ. Monitor. Assess., 2007
C       REAL, PARAMETER :: FAC_PHG  = 0.0000000638
C       REAL, PARAMETER :: FAC_PPB  = 0.00000106
C       REAL, PARAMETER :: FAC_PFE  = 0.000879
C       REAL, PARAMETER :: FAC_PNA  = 0.000944
C       REAL, PARAMETER :: FAC_PNI  = 0.00000162
C       REAL, PARAMETER :: FAC_PCR  = 0.00000309
C       REAL, PARAMETER :: FAC_PCD  = 0.000000019
C       REAL, PARAMETER :: FAC_PAG  = 0.000000007
C       REAL, PARAMETER :: FAC_PAS  = 0.00000113
C       REAL, PARAMETER :: FAC_PCU  = 0.00000119
 
C     Vertical distribution based on estimated layer depth
       INTEGER, PARAMETER :: EMISLAY = 15
       REAL :: LFRAC (EMISLAY) 
       DATA LFRAC /0.02, 0.04, 0.06, 0.08, 0.10, 0.10, 0.10,
     &         0.10, 0.08, 0.08, 0.08, 0.06, 0.04, 0.02, 0.02/
      INTEGER, PARAMETER :: DSF = 1.0          ! Dust scaling factor
C       INTEGER, PARAMETER :: DSF = 0.0          ! Dust scaling factor

C     Adjust dust emissions -- a switch button
        DO I = 1, EMISLAY
          LFRAC(I) = LFRAC(I) * DSF             
        END DO

C *** Open the gridded emissions file with all emissions except dust
	PNAME = 'EMIS_1'
        IF ( .NOT. OPEN3( 'EMIS_1', FSRDWR3, 'EMIS_1' ) ) THEN
           XMSG = 'Could not open '// EMIS_1 // ' file'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. DESC3( 'EMIS_1' ) ) THEN
           XMSG = 'Could not get '// EMIS_1 // ' file description'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        END IF

	WRITE(*,*) "NCOLS3D, NROWS3D, NLAYS3D are: ", NCOLS3D, 
     &			NROWS3D, NLAYS3D       
 
C *** Allocate arrays to hold emissions
        ALLOCATE ( DUST(NCOLS3D, NROWS3D) )
        ALLOCATE ( PNO3(NCOLS3D, NROWS3D, NLAYS3D) )
        ALLOCATE ( PSO4(NCOLS3D, NROWS3D, NLAYS3D) )
        ALLOCATE ( PEC (NCOLS3D, NROWS3D, NLAYS3D) )
        ALLOCATE ( POA (NCOLS3D, NROWS3D, NLAYS3D) )
        ALLOCATE ( PMF (NCOLS3D, NROWS3D, NLAYS3D) )       
        ALLOCATE ( PM10(NCOLS3D, NROWS3D, NLAYS3D) )       
        ALLOCATE ( TEMP(NCOLS3D, NROWS3D, NLAYS3D) )  
        
C *** Open the dust emissions file 
        IF ( .NOT. OPEN3( 'EMIS_2', FSREAD3, 'EMIS_2') ) THEN
            XMSG = 'Could not open '// EMIS_1 // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

	JDATE = SDATE3D
	JTIME = STIME3D

	DO ISTEP = 1, MXREC3D

C *** Read dust emissions from EMIS_2
        IF ( .NOT. READ3 ('EMIS_2', 'ETOTL', 1, JDATE, JTIME,
     &		DUST ) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF

C *** Read and allocate dust emissions into individual species.
	VNAME ='PSO4' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,
     &	PSO4 ) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           PSO4(I,J,K) = PSO4(I,J,K) + DUST(I,J) * FAC_PSO4 * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,PSO4 ) 
     &	) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF
        
 	VNAME ='PNO3' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,
     &	TEMP ) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PNO3 * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP 
     &	) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF       

	VNAME ='PEC' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,
     &  TEMP ) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PEC * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) 
     &	) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF
        
C	VNAME ='POA' 
	VNAME ='POC'
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &   ) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_POA * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF

	VNAME ='PMOTHR' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PMF * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF        
	
	VNAME ='PAL' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PAL * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF
	
	VNAME ='PCA' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PCA * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF	
		
	VNAME ='PCL' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PCL * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF	
	
	VNAME ='PFE' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PFE * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF
	
	VNAME ='PK' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PK * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF	
		
	VNAME ='PNA' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PNA * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF		
	
	VNAME ='PNCOM' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PNCOM * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF
	
	VNAME ='PSI' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PSI * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF	
		
	VNAME ='PTI' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PTI * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF					
	
	VNAME ='PMC' 
        IF ( .NOT. READ3 ('EMIS_1', VNAME, ALLAYS3, JDATE, JTIME,TEMP 
     &	) ) THEN
            PRINT *, " Reading Dust emissions is not done!"         
        END IF
        DO I = 1, NCOLS3D
        DO J = 1, NROWS3D
        DO K = 1, EMISLAY
           TEMP(I,J,K) = TEMP(I,J,K) + DUST(I,J) * FAC_PMC * LFRAC(K)
        END DO
        END DO
        END DO
        IF ( .NOT. WRITE3 ( 'EMIS_1', VNAME, JDATE, JTIME,TEMP ) ) THEN
           PRINT *, "  Writting is not done: ", VNAME
        END IF        

	CALL NEXTIME(JDATE, JTIME, TSTEP3D)

	END DO

C *** deallocate arrays to hold emissions
        DEALLOCATE ( DUST )
        DEALLOCATE ( PNO3 )
        DEALLOCATE ( PSO4 )
        DEALLOCATE ( PEC  )
        DEALLOCATE ( POA  )
        DEALLOCATE ( PMF  )       
        DEALLOCATE ( PM10 )       
        DEALLOCATE ( TEMP )  

!******************* Close files ************************************
        IF ( CLOSE3('EMIS_1')) THEN
                PRINT *, ' Closed program ', 'EMIS_1'
        ELSE
                PRINT *, "      I can not close ", 'EMIS_1'
        END IF

        IF ( CLOSE3('EMIS_2')) THEN
                PRINT *, ' Closed program ', 'EMIS_2'
        ELSE
                PRINT *, " Can not close ", 'EMIS_2'
        END IF

        CALL M3EXIT( 'EMIS_1', 0, 0, '--> Successful completion', 0 )

      END 
      
