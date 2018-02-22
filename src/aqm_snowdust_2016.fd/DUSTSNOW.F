        PROGRAM DUSTSNOW 

C......................................................
C Function: Implement the effects of snow/ice cover on
C            fugitive dust emissions;
C Author  : Daniel Tong
C......................................................   

      IMPLICIT NONE

!...........   INCLUDES:
      INCLUDE 'PARMS3.EXT'
      INCLUDE 'FDESC3.EXT'
      INCLUDE 'IODECL3.EXT'

!...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV
	INTEGER		I, I0,MYDATE,MYTIME, J, K, M, N, PERCENT, NCELL
	INTEGER 	NCOLS, NROWS, NLAYS, NMETS, NCOUN, NCELS
	INTEGER		SDATE, SHOUR, NHOUR, SCOL, ECOL, SROW, EROW
        INTEGER         SLAY, ELAY
!    	PARAMETER ( NCOLS = 442, NROWS = 265, NLAYS = 1, NCELS = 132 )
    
!********** These species are to be changed **************************
	REAL, ALLOCATABLE,SAVE :: PMFINE(:,:,:)
        REAL, ALLOCATABLE,SAVE :: PMC   (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PAL   (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PCA   (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PCL   (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PFE   (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PK    (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PMG   (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PMN   (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PMOTHR(:,:,:)
	REAL, ALLOCATABLE,SAVE :: PNA   (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PNCOM (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PNH4  (:,:,:)
        REAL, ALLOCATABLE,SAVE :: PSI   (:,:,:)
	REAL, ALLOCATABLE,SAVE :: PTI   (:,:,:)		
	REAL, ALLOCATABLE,SAVE :: SNOWC (:,:,:)
!	REAL, ALLOCATABLE,SAVE :: SOIM1 (:,:,:)
	REAL, ALLOCATABLE,SAVE :: SOILW (:,:,:)	
        REAL, ALLOCATABLE,SAVE :: RN    (:,:,:)
        REAL, ALLOCATABLE,SAVE :: RC    (:,:,:)

!*** Variable for read index file containing the mapping relationship
        CHARACTER 	COUNTY*20, STATE*20, SP*10, DATETAG*1
        REAL 	        EF	
        INTEGER		COL, ROW, IO
	INTEGER		TGDATE, JULDATE

!****************** Variables to get environmental setting ********
	CHARACTER METCRO2D*120,VARFILE*120,MAXFILE*120,MINFILE*120
        CHARACTER AHSFILE*120,TDSFILE*120
	CHARACTER FIRSTDAY*1, G_STDATE*7


!*************** Open output files *********************************

!.......   First:  Initialize the I/O API:

        LOGDEV = INIT3()        !  initialization returns unit # for log
	PRINT *, "	 the INIT3 number is ", LOGDEV  

        IF ( .NOT. OPEN3( 'METC2D', FSREAD3, 'MYINPUT' ) ) THEN
             CALL M3EXIT( 'METC2D', 0, 0, 'Can not open file', 2 )
        END IF

        IF ( .NOT. OPEN3( 'EMIS3D', FSRDWR3, 'MYINPUT' ) ) THEN
             CALL M3EXIT( 'EMIS3D', 0, 0, 'Can not open file', 2 )
        END IF

        IF ( .NOT. DESC3( 'EMIS3D' ) ) THEN
             PRINT *, "   I can not get file description"
        ELSE
             PRINT *, "   I Got file description"
        END IF

        ALLOCATE (PMFINE(NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PMC   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PAL   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PCA   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PCL   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PFE   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PK    (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PMG   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PMN   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PMOTHR(NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PNA   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PNCOM (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PSI   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PTI   (NCOLS3D, NROWS3D, NLAYS3D))
        ALLOCATE (PNH4  (NCOLS3D, NROWS3D, NLAYS3D))			
        ALLOCATE (SNOWC (NCOLS3D, NROWS3D, 1))
!       ALLOCATE (SOIM1 (NCOLS3D, NROWS3D, 1))
        ALLOCATE (SOILW (NCOLS3D, NROWS3D, 1))	
        ALLOCATE (RC    (NCOLS3D, NROWS3D, 1))
        ALLOCATE (RN    (NCOLS3D, NROWS3D, 1))

	MYDATE = SDATE3D
	MYTIME = STIME3D

! ******************* Begin to read file desc ***********************
        DO I = 1, MXREC3D
          IF (.NOT. READ3('METC2D','SNOWC',1,MYDATE,
     &	    MYTIME,SNOWC)) THEN
       	    PRINT *, "	Reading is not done!"
       	  END IF

!          IF (.NOT. READ3('METC2D','SOIM1',1,MYDATE,
!     &      MYTIME,SOIM1)) THEN
!            PRINT *, "  Reading is not done!"
!          END IF
          IF (.NOT. READ3('METC2D','SOILW',1,MYDATE,
     &      MYTIME,SOILW)) THEN
            PRINT *, "  Reading is not done!"
          END IF	  

          IF (.NOT. READ3('METC2D','RC',1,MYDATE,
     &      MYTIME,RC)) THEN
            PRINT *, "  Reading is not done!"
          END IF

          IF (.NOT. READ3('METC2D','RN',1,MYDATE,
     &      MYTIME,RN)) THEN
            PRINT *, "  Reading is not done!"
          END IF

          IF (.NOT. READ3('EMIS3D','PAL',ALLAYS3,MYDATE,
     &      MYTIME,PAL)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PCA',ALLAYS3,MYDATE,
     &      MYTIME,PCA)) THEN
            PRINT *, "  Reading is not done!"
          END IF	  
	  
          IF (.NOT. READ3('EMIS3D','PCL',ALLAYS3,MYDATE,
     &      MYTIME,PCL)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PFE',ALLAYS3,MYDATE,
     &      MYTIME,PFE)) THEN
            PRINT *, "  Reading is not done!"
          END IF	
	   
          IF (.NOT. READ3('EMIS3D','PK',ALLAYS3,MYDATE,
     &      MYTIME,PK)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PMFINE',ALLAYS3,MYDATE,
     &      MYTIME,PMFINE)) THEN
            PRINT *, "  Reading is not done!"
          END IF	  
	  
          IF (.NOT. READ3('EMIS3D','PMG',ALLAYS3,MYDATE,
     &      MYTIME,PMG)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PMN',ALLAYS3,MYDATE,
     &      MYTIME,PMN)) THEN
            PRINT *, "  Reading is not done!"
          END IF	 
	  
          IF (.NOT. READ3('EMIS3D','PMOTHR',ALLAYS3,MYDATE,
     &      MYTIME,PMOTHR)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PNA',ALLAYS3,MYDATE,
     &      MYTIME,PNA)) THEN
            PRINT *, "  Reading is not done!"
          END IF	  
	  
          IF (.NOT. READ3('EMIS3D','PNCOM',ALLAYS3,MYDATE,
     &      MYTIME,PNCOM)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PNH4',ALLAYS3,MYDATE,
     &      MYTIME,PNH4)) THEN
            PRINT *, "  Reading is not done!"
          END IF	
	   
          IF (.NOT. READ3('EMIS3D','PSI',ALLAYS3,MYDATE,
     &      MYTIME,PSI)) THEN
            PRINT *, "  Reading is not done!"
          END IF
	  
          IF (.NOT. READ3('EMIS3D','PTI',ALLAYS3,MYDATE,
     &      MYTIME,PTI)) THEN
            PRINT *, "  Reading is not done!"
          END IF	  
	  	  
	  	  
          IF (.NOT. READ3('EMIS3D','PMC',ALLAYS3,MYDATE,
     &      MYTIME,PMC)) THEN
            PRINT *, "  Reading is not done!"
          END IF

 
!************** Get emission processing
	  DO N = 1, NCOLS3D
  	  DO M = 1, NROWS3D 
            EF = 1.0
            !IF ( SOIM1(N,M,1) .GT. 0.1  ) EF = 0.05
	    !IF ( SOILW(N,M,1) .GT. 0.1  ) EF = 0.05
            IF ( SNOWC(N,M,1) .GT. 0.25 ) EF = 0.01
            IF ((RN(N,M,1)+RC(N,M,1)) .GT. 0.01) EF = 0.01
            PMFINE(N,M,1) = PMFINE(N,M,1) * EF
            PMC   (N,M,1) = PMC   (N,M,1) * EF
            PMOTHR(N,M,1) = PMOTHR(N,M,1) * EF
            PAL   (N,M,1) = PAL   (N,M,1) * EF	
            PNCOM (N,M,1) = PNCOM (N,M,1) * EF
            PCA   (N,M,1) = PCA   (N,M,1) * EF
            PNH4  (N,M,1) = PNH4  (N,M,1) * EF
            PCL   (N,M,1) = PCL   (N,M,1) * EF	
            PFE   (N,M,1) = PFE   (N,M,1) * EF
            PK    (N,M,1) = PK    (N,M,1) * EF
            PMG   (N,M,1) = PMG   (N,M,1) * EF
            PMN   (N,M,1) = PMN   (N,M,1) * EF	
            PNA   (N,M,1) = PNA   (N,M,1) * EF
            PSI   (N,M,1) = PSI   (N,M,1) * EF
            PTI   (N,M,1) = PTI   (N,M,1) * EF		    	        
	  END DO
	  END DO	

!************** Write emission back ********************
          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PMC', MYDATE, 
     &      MYTIME,PMC ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PMFINE', MYDATE,
     &      MYTIME,PMFINE ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF
	  
         IF ( .NOT. WRITE3 ( 'EMIS3D', 'PAL', MYDATE, 
     &      MYTIME,PAL ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PMOTHR', MYDATE,
     &      MYTIME,PMOTHR ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PCA', MYDATE, 
     &      MYTIME,PCA ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PNCOM', MYDATE,
     &      MYTIME,PNCOM ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF
	  
         IF ( .NOT. WRITE3 ( 'EMIS3D', 'PCL', MYDATE, 
     &      MYTIME,PCL ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PFE', MYDATE,
     &      MYTIME,PFE ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF
	  
          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PK', MYDATE, 
     &      MYTIME,PK ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PNH4', MYDATE,
     &      MYTIME,PNH4 ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF
	  
         IF ( .NOT. WRITE3 ( 'EMIS3D', 'PMG', MYDATE, 
     &      MYTIME,PMG ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PMN', MYDATE,
     &      MYTIME,PMN ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PNA', MYDATE, 
     &      MYTIME,PNA ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

          IF ( .NOT. WRITE3 ( 'EMIS3D', 'PSI', MYDATE,
     &      MYTIME,PSI ) ) THEN
            PRINT *, "  Writting is not done: ",'PMFINE'
          END IF
	  
         IF ( .NOT. WRITE3 ( 'EMIS3D', 'PTI', MYDATE, 
     &      MYTIME,PTI ) ) THEN
            PRINT *, "  Writting is not done: ",'PMC' 
          END IF

	  
          CALL NEXTIME(MYDATE, MYTIME, TSTEP3D)

	END DO

        IF (.NOT. CLOSE3('METC2D')) THEN
          PRINT *, "	I can not close ", 'METCRO2D'	
	END IF
        IF (.NOT. CLOSE3('EMIS3D')) THEN
          PRINT *, "    I can not close ", 'AREAEMIS'   
        END IF

        DEALLOCATE (PMFINE)
        DEALLOCATE (PMC   )
        DEALLOCATE (PMOTHR)
        DEALLOCATE (PAL   )
        DEALLOCATE (PNCOM )
        DEALLOCATE (PCA   )
        DEALLOCATE (PNH4  )
        DEALLOCATE (PFE   )
        DEALLOCATE (PCL   )
        DEALLOCATE (PK    )
        DEALLOCATE (PMG   )
        DEALLOCATE (PMN   )
        DEALLOCATE (PNA   )
        DEALLOCATE (PSI   )
        DEALLOCATE (PTI   )			
        DEALLOCATE (SNOWC )
!       DEALLOCATE (SOIM1 )
        DEALLOCATE (SOILW )	
        DEALLOCATE (RC    )
        DEALLOCATE (RN    )

 	CALL M3EXIT( 'MYOUTPUT', 0, 0, '--> Successful completion', 0 )

	END PROGRAM
