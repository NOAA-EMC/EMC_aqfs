C       SUBROUTINE DREMOIN SUBST_GRID_ID( JDATE, TF, ULAND )
        SUBROUTINE TFBELOW SUBST_GRID_ID( JDATE, TF, ULAND )

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  FUNCTION:
C   This module calculates erodible agriculture land fraction and below canopy 
C   transportable fraction that accounts for near source removal by surface 
C   obstacles.
C  DESCRIPTION:
C   Called by DUST_EMIS, execute only once;
C   Take in day, time, and read cropland fraction from BELD and 
C   3 gridded crop calendar file 
C
C  Subroutines and Functions Called:
C       INTERPX, OPEN3, M3EXIT
C
C  HISTORY:
C   10/2006: Implemented based on S. He's prototype (Daniel Tong);
C   01/2010: Renamed the routine from DREMOIN to TFBELOW (DT);
C            Changed PNMAE,ENAME(now FLANDA),FNAME(FLANDT),HNAME(FRAIN);
C            Change FW() to CF() for in-canopy capture fraction (ref.Pace 2005);
C		 
C                
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


      IMPLICIT NONE

C Includes:
C     INCLUDE 'PARMS3.EXT'		! I/O parameters definitions
C     INCLUDE 'FDESC3.EXT'		! file header data structuer
C     INCLUDE 'IODECL3.EXT_Tong'	! I/O definitions and declarations
C     INCLUDE 'DUST_CONST.EXT'          ! Constants for dust emissions only
C     INCLUDE 'FILE3_CTM.EXT'           ! File names
      INCLUDE SUBST_IOPARMS             ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC             ! file header data structuer
      INCLUDE SUBST_IODECL              ! I/O definitions and declarations
      INCLUDE SUBST_DUSTCT              ! Constant for dust calculation
      INCLUDE SUBST_FILES_ID            ! file name parameters
      INCLUDE SUBST_CONST               ! constants

C... Arguments
      INTEGER JDATE, JTIME                        ! date, time, coded YYYYDDD, HHMMSS
      REAL TF( NCOLS, NROWS )                     ! dust transport factor flow into canopy
      REAL ULAND( NCOLS, NROWS,  4 )              ! fraction of 4 landtype
 
C...Parameter
      CHARACTER*16    PNAME
      DATA            PNAME /'TFBELOW'/
      
      CHARACTER*16    FLANDA
      DATA            FLANDA /'FLANDA'/
      
      CHARACTER*16    FLANDT
      DATA            FLANDT /'FLANDT'/
      
      CHARACTER*16    FRAIN
      DATA            FRAIN /'RAINMAP'/
      
      CHARACTER*16    VNAME
      CHARACTER*160   XMSG
      INTEGER STIME                               ! start, time, coded HHMMSS
      
      INTEGER NLTP                                ! number of landuse type
      PARAMETER ( NLTP = 21 )

      CHARACTER*16 VNMLD( NLTP )
      DATA VNMLD / 'USGS_urban', 'USGS_drycrop', 'USGS_irrcrop',
     >     'USGS_cropgrass', 'USGS_cropwdlnd', 'USGS_grassland', 
     >      'USGS_shrubland', 'USGS_shrubgrass', 'USGS_savanna', 
     >  'USGS_decidforest', 'USGS_evbrdleaf', 'USGS_coniferfor',
     >           'USGS_mxforest', 'USGS_water', 'USGS_wetwoods',
     >     'USGS_sprsbarren', 'USGS_woodtundr', 'USGS_mxtundra',
     >                  'USGS_snowice', 'AGRICULTURE', 'FOREST'/

C     REAL, ALLOCATABLE ::  LUT ( :,:,: )     	! fraction of landtype
C     REAL, ALLOCATABLE ::  RAINF ( :,: )       ! annual rainfall in inch
      REAL LUT (NCOLS,NROWS,NLTP )		! fraction of landtype
      REAL RAINF( NCOLS, NROWS )		! annual rainfall in inch
      REAL UFC( NCOLS, NROWS )                  ! total weighted removal fraction for each cell
      REAL TLAND( NCOLS, NROWS ) 
      
C      INTEGER I, J, C, R
      INTEGER I, J
 
C     REAL FW(4)                                 ! removal fraction in and below canopy
C     DATA FW / 0.0, 0.10, 0.50, 0.95 /          ! all source dust (water grass&crop building forest)
      REAL CF(4)                                 ! capture fraction in canopy (conception from Pace 2005)
      DATA CF / 0.0, 0.10, 0.50, 0.95 /          ! all source dust (water,grass & crop,building,forest)

C      REAL FC(4, 2)                              ! removal fraction in and below canopy
C      DATA FC / 0.0, 0.10, 0.15, 0.20,           ! agriculture source dust
C     >          0.0, 0.30, 0.75, 0.90 /          ! road and construction source dust

      INTEGER  GXOFF, GYOFF                               ! global origin offset from file
!      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
!      INTEGER, SAVE :: STARTCOL2, ENDCOL2, STARTROW2, ENDROW2
      
      INTEGER  ALLOCSTAT
C------------------------------------------------------------------------
        
        STIME = 000000


C... Initialize arrays
      DO C = 1, NCOLS
      DO R = 1, NROWS
        DO J = 1, NLTP
  	  LUT(C,R,J) = 0.0
        END DO
        DO J = 1,4 
          ULAND(C,R,J) = 0.0
        END DO      
      END DO
      END DO

C===== We have disabled the rain impact on FW(2) 
C         IF ( .NOT. OPEN3( HNAME, FSREAD3, PNAME ) ) THEN
C            XMSG = 'Could not open '//HNAME//' file'
C            CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )
C            END IF

C         IF ( .NOT. DESC3( HNAME ) ) THEN
C            XMSG = 'Could not get '//HNAME//' file description'
C            CALL M3EXIT( PNAME,  SDATE, STIME, XMSG, XSTAT1 )
C            END IF
      
C      CALL SUBHFILE ( HNAME, GXOFF, GYOFF,
C     &    STARTCOL, ENDCOL, STARTROW, ENDROW )
      
C      ALLOCATE ( RAINF(MY_NCOLS,MY_NROWS),STAT = ALLOCSTAT)
C      IF ( ALLOCSTAT .NE. 0 ) THEN
C         XMSG = 'Failure allocating RAINF'
C         CALL M3EXIT( PNAME, JDATE, STIME, XMSG, XSTAT1 )
C         END IF
      
C         VNAME='Rain' 
C         XMSG = 'Could not read rainfall from '//HNAME
C         IF ( .NOT. INTERPX ( HNAME, VNAME, PNAME,
C        &    STARTCOL,ENDCOL,STARTROW,ENDROW, 1,1,
C        &             SDATE, STIME, RAINF ) )
C     &  CALL M3EXIT ( PNAME, SDATE, STIME, XMSG, XSTAT1 )

C Open the BELD3 file
C       IF ( .NOT. OPEN3( ENAME, FSREAD3, PNAME ) ) THEN
        IF ( .NOT. OPEN3( FLANDA, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '//FLANDA//' file'
C         XMSG = 'Could not open '//ENAME//' file'
          CALL M3EXIT( PNAME, JDATE, STIME, XMSG, XSTAT1 )
          END IF
      
C       IF ( .NOT. DESC3( ENAME ) ) THEN
C          XMSG = 'Could not get '//ENAME//' file description'
        IF ( .NOT. DESC3( FLANDA ) ) THEN
           XMSG = 'Could not get '//FLANDA//' file description'
           CALL M3EXIT( PNAME,  JDATE, STIME, XMSG, XSTAT1 )
           END IF

C       IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
C         XMSG = 'Could not open '//FNAME//' file'
        IF ( .NOT. OPEN3( FLANDT, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '//FLANDT//' file'
          CALL M3EXIT( PNAME, JDATE, STIME, XMSG, XSTAT1 )
          END IF
      
C       IF ( .NOT. DESC3( FNAME ) ) THEN
C          XMSG = 'Could not get '//FNAME//'file description'
        IF ( .NOT. DESC3( FLANDT ) ) THEN
           XMSG = 'Could not get '//FLANDT//'file description'
           CALL M3EXIT( PNAME,  JDATE, STIME, XMSG, XSTAT1 )
           END IF

C...Get domain decomp info
C      CALL SUBHFILE ( ENAME, GXOFF, GYOFF,
C     &                STARTCOL, ENDCOL, STARTROW, ENDROW )

C...Allocate array size	 
C      ALLOCATE ( LUT( MY_NCOLS,MY_NROWS,NLTP ), STAT = ALLOCSTAT )
C      IF ( ALLOCSTAT .NE. 0 ) THEN
C         XMSG = 'Failure allocating LUT'
C         CALL M3EXIT( PNAME, JDATE, STIME, XMSG, XSTAT1 )
C         END IF

C Get landuse (fraction) data
      DO I = 1, NLTP-2
C     XMSG = 'Could not read '//VNMLD(I)//' from '//ENAME
C     IF ( .NOT. INTERPX (ENAME, VNMLD(I), PNAME,
      XMSG = 'Could not read '//VNMLD(I)//' from '//FLANDA
      IF ( .NOT. INTERPX (FLANDA, VNMLD(I), PNAME,
     &    STARTCOL,ENDCOL,STARTROW,ENDROW, 1,1,
     &             JDATE, STIME3D, LUT(1,1,I) ) )
     &  CALL M3EXIT ( PNAME, JDATE, STIME, XMSG, XSTAT1 )
      END DO

      DO I = NLTP-1, NLTP
C     XMSG = 'Could not read '//VNMLD(I)//' from '//FNAME
C     IF ( .NOT. INTERPX (FNAME, VNMLD(I), PNAME,
      XMSG = 'Could not read '//VNMLD(I)//' from '//FLANDT
      IF ( .NOT. INTERPX (FLANDT, VNMLD(I), PNAME,
     &    STARTCOL,ENDCOL,STARTROW,ENDROW, 1,1,
     &             JDATE, STIME3D, LUT(1,1,I) ) )
     &  CALL M3EXIT ( PNAME, JDATE, STIME, XMSG, XSTAT1 )
      END DO
      
      DO C = 1, NCOLS
      DO R = 1, NROWS

C... water & barren surrounding
         ULAND(C,R,1) = LUT(C,R,7) + LUT(C,R,14) + 
     &		LUT(C,R,16) + LUT(C,R,18) + LUT(C,R,19) 
	 ULAND(C, R, 2) = LUT(C, R, 6) + LUT(C, R, 8)         ! grass surrounding
     &                  + LUT(C, R, 9) + LUT(C, R,17)
     &                  + LUT(C, R, 2) + LUT(C, R, 3)         ! crop surrounding
     &                  + LUT(C, R, 4) + LUT(C, R, 5)
     &                  + LUT(C, R,20)
	 ULAND(C, R, 3) = LUT(C, R, 1)                        ! building surrounding
	 ULAND(C, R, 4) = LUT(C, R,10) + LUT(C, R,11)         ! forest surrounding
     >                  + LUT(C, R,12) + LUT(C, R,13)
     >                  + LUT(C, R,15) + LUT(C, R,21)

	
         TLAND(C, R) = 0.0
	 DO I = 1, 4
	   IF(ULAND(C,R,I).GT.100) THEN
	     ULAND(C,R,I) = 100.
	   ELSEIF(ULAND(C,R,I).LT.0) THEN
	     ULAND(C,R,I) = 0.
	   ENDIF
	   TLAND(C, R) = TLAND(C, R) + ULAND(C, R, I)
	 END DO
	
	 IF(TLAND(C,R).GT.100) THEN
	   DO I = 1, 4
             ULAND(C, R, I) = ULAND(C,R,I)*100./TLAND(C,R)
	   END DO
	 END IF
	
		
!DQT	 IF(ULAND(C,R,4).GT.5.OR.RAINF(C,R).GT.15) FW(2)=0.20 ! Windbreak effect on grass&crop
	 
	 UFC(C, R) = 0.0
	 DO J = 1, 4
	    UFC(C, R) = UFC(C, R) 
C    >                + FW(J)*ULAND(C, R, J)*0.01
     >                + CF(J)*ULAND(C, R, J)*0.01
	    END DO
	 TF(C, R) = 1.0 - UFC(C, R)

         IF( TF(C,R).GT.1 .OR.TF(C,R).LT.0 ) THEN
	   print *, '*** error in TFBELOW ***'
	   print *, 'C=',C, 'R=', R, TF(C,R)
	 END IF
	 
       END DO
       END DO
	 
       RETURN
       END
      
