
C     SUBROUTINE TFACTOR SUBST_GRID_ID(USTAR,SWIND,TMEG,ULAND)
      SUBROUTINE TFABOVE SUBST_GRID_ID(USTAR,SWIND,TMEG,ULAND)
      
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  FUNCTION:
C   This module calculates near source removal above canopy based on land use,
C   wind speed, friction velocity, and deposition formula from literature; 
C   
C  DESCRIPTION:
C   Called by DUST_EMIS, executed each time step;
C   Take in day, time, and read cropland fraction from BELD and 
C   3 gridded crop calendar file 
C
C  Subroutines and Functions Called:
C       INTERPX, OPEN3, M3EXIT
C
C  HISTORY:
C   10/2006: Implemented based on S. He's prototype (Daniel Tong);
C   01/2010: Renamed the routine from TFACTOR to TFABOVE (Daniel Tong);
C            Changed 
C            Changed ;
C		 
C                
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_IOPARMS             ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC             ! file header data structuer
      INCLUDE SUBST_IODECL              ! I/O definitions and declarations
      INCLUDE SUBST_DUSTCT              ! Constant for dust calculation
      INCLUDE SUBST_FILES_ID            ! file name parameters
      INCLUDE SUBST_CONST               ! constants

c Arguments

C     INTEGER C, R                      ! Column, Row, Landuse type index
      REAL USTAR( NCOLS,NROWS )         ! friction velocity [m/s]
      REAL SWIND( NCOLS,NROWS )         ! surface wind [m/s] SWIND
      REAL TMEG ( NCOLS,NROWS )         ! transport factor for above canopy
      REAL ULAND( NCOLS,NROWS,4 )       ! fraction of 4 landtype for dust removal 0~100

C Parameters:
      INTEGER I, J              	! loop index
     
      REAL VD(4, 3)             	! deposition velocity [m/s]
C Based on Slinn 1982, fugitive dust in coarse mode, Vd average from
C PM2~PM10; 3 wind velocities 10, 5, 1 m/s; 4 canopy characters
C water, lamda = 1.0, 3.5, 5.0
      DATA ((VD(I,J),I=1,4),J=1,3) 
     >        / 0.0051,         ! SWIND = 15 m/s, natural water Vd(1,1)
     >          0.0152,         ! SWIND = 10 m/s, lamda = 2.0   Vd(2,1)
     >          0.0268,         ! SWIND = 10 m/s, lamda = 3.5   Vd(3,1)
     >          0.0382,         ! SWIND = 10 m/s, lamda = 5.0   Vd(4,1)
     >          0.0018,         ! SWIND =  5 m/s, natural water Vd(1,2)
     >          0.0056,         ! SWIND =  5 m/s, lamda = 2.0   Vd(2,2)
     >          0.0099,         ! SWIND =  5 m/s, lamda = 3.5   Vd(3,2)
     >          0.0141,         ! SWIND =  5 m/s, lamda = 5.0   Vd(4,2)
     >          0.0018,         ! SWIND =  1 m/s, natural water Vd(1,3)
     >          0.0020,         ! SWIND =  1 m/s, lamda = 2.0   Vd(2,3)
     >          0.0021,         ! SWIND =  1 m/s, lamda = 3.5   Vd(3,3)
     >          0.0030 /        ! SWIND =  1 m/s, lamda = 5.0   Vd(4,3)

C local variables:
      CHARACTER*16 PNAME
      DATA         PNAME   / 'TFABOVE' /
C     DATA         PNAME   / 'TFACTOR' /

      REAL    KK                                 ! K=0.08U*
      REAL    OMEG                               ! transport factor
      
C ------------------------------------------------------------------      
      DO C = 1, MY_NCOLS
      DO R = 1, MY_NROWS
	 TMEG(C, R) = 0.0
	 IF (SWIND(C, R) .LE. 2.0) THEN              ! surface wind
	    J = 3
	 ELSEIF (SWIND(C, R) .LT. 7.0) THEN
	    J = 2
	 ELSE                                        ! wind velocity 10 m/s
	    J = 1
	 ENDIF
      DO I = 1, 4                                       ! landuse loop
         OMEG = 0.0
         IF(ULAND(C, R, I).GT.0) THEN                   ! non-zero landuse
	    KK = 0.08 * USTAR( C, R )
            OMEG = KK /( VD(I, J) + KK )                ! ratio dQup/dQrd    
C	    write (*,*) "OMEG : ", OMEG  
            IF (OMEG.GT.1.0.OR.OMEG.LT.0) THEN
               PRINT *, '-- in TFACTOR -- ratio incorrect'
C              STOP
	       OMEG = 1.0
            END IF
         TMEG(C, R) = TMEG(C, R) + OMEG * ULAND(C, R, I) *0.01
         END IF
      END DO
      END DO
      END DO
      
C-----------------------------------------------------------------------------
C     END of TFACTOR subroutine
C-----------------------------------------------------------------------------

      RETURN
      END
            
      
      
