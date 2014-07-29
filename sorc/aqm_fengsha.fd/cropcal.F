
C***********************************************************************
C
C  DESCRIPTION:
C       be called by duster
C       take in day of the year
C       read cropland fraction from BELD and 3 gridded crop calendar file 
C       calculate erodible agriculture land fraction and pass on
C
C  Shan He at RTP 2003
C
C  Subroutines and Functions Called:
C       INTERPX, OPEN3, M3EXIT
C***********************************************************************
      SUBROUTINE CROPCAL SUBST_GRID_ID( JDATE, AGLAND )

C      USE HGRD_DEFN             ! horizontal domain specifications
 
C      USE SUBST_MODULES         ! stenex

      IMPLICIT NONE

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

C... Arguments
      INTEGER      JDATE                          ! date , coded YYYYDDD
      REAL AGLAND( NCOLS, NROWS )                 ! total erodible crop land

C...Parameter
      INTEGER     JTIME                           ! time, HHMMSS
      PARAMETER ( JTIME = 000000 ) 

      CHARACTER*16    PNAME
      DATA            PNAME /'CROPCAL'/
      
      CHARACTER*16    ENAME
      DATA            ENAME /'BELD01'/
	
      CHARACTER*160   XMSG

!      INTEGER XSTAT1                              ! Exit status code for IO error
!      INTEGER XSTAT2                              ! Exit status code for program error
      
      INTEGER I
!      INTEGER C, R, I
      INTEGER II, MM, JDAY
      REAL TTMP, TWOS
      REAL PCTF, POTF
      
      INTEGER NLCP
      PARAMETER ( NLCP = 17 )
      CHARACTER*16 VNMLD( NLCP )
      DATA VNMLD / 'Alfalfa',   'Barley',   'Corn',      'Cotton', 
     >             'Grass',     'Hay',      'Misc_crop', 'Oats',  
     >             'Pasture',   'Peanuts',  'Potatoes',  'Rice',
     >             'Rye',       'Sorghum',  'Soybeans',   'Tobacco',
     >             'Wheat' /

      INTEGER NCROP
      PARAMETER ( NCROP = 18 )
      CHARACTER*16 VCROP( NCROP )
      DATA VCROP / 'Alfalfa',    'BarleySpring',  'BarleyFall',
     >             'Corn',       'Cotton',        'Hay',
     >             'OatsSpring', 'OatsFall',      'Peanuts',
     >             'Potatoes',   'Rice',          'Rye',
     >             'Sorghum',    'Soybeans',      'Sugerbeets',
     >             'Tobacco',    'WheatSpring',   'WheatWinter' /

      CHARACTER*16 CNAME(3)                   ! crop calendar file name
      DATA CNAME / 'CROPMAP01', 'CROPMAP04', 'CROPMAP08' /
      
      REAL CTILF(17)                          ! Conventional tillage (0-15% residue) fraction for 17 crops as VNMLD list
      DATA CTILF / 0.00,  0.37,  0.40,  0.70,             ! Alfalfa Barley Corn Cotton
     >             0.00,  0.00,  0.70,  0.47,             ! Grass Hay Misc_crop Oats
     >             0.36,  0.80,  0.74,  0.79,             ! Pasture Peanuts Potato Rice
     >             0.52,  0.46,  0.24,  0.96,             ! Rye Sorghum Soybean Tobacco 
     >             0.40 /                                 ! Wheat

      REAL NTILF(17)                          ! No tillage fraction for 17 crops as VNMLD list
      DATA NTILF / 0.80,  0.10,  0.20,  0.14,             ! Alfalfa Barley Corn Cotton
     >             0.80,  0.80,  0.05,  0.08,             ! Grass Hay Misc_crop Oats
     >             0.47,  0.09,  0.00,  0.05,             ! Pasture Peanuts Potato Rice
     >             0.11,  0.14,  0.33,  0.01,             ! Rye Sorghum Soybean Tobacco 
     >             0.13 /                                 ! Wheat
      
!      REAL, ALLOCATABLE ::  LADUT ( :,:,: )               ! cropland fraction from BELD3
!      REAL, ALLOCATABLE ::  CROPDT( :,:,:,: )             ! cropland calendar for species and 3 activities

      REAL LADUT( MY_NCOLS,MY_NROWS,NLCP ) ! fraction of desertland (BELD /100)
      REAL CROPDT ( MY_NCOLS,MY_NROWS,NCROP,3) ! fraction of 4 landtype for dust removal
      REAL CRPLD  ( NCOLS,NROWS,NLCP )                    ! each erodible crop land fraction
      
      INTEGER  GXOFF, GYOFF                               ! global origin offset from file
!      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
!      INTEGER, SAVE :: STARTCOL2, ENDCOL2, STARTROW2, ENDROW2
      
      INTEGER  ALLOCSTAT
        
C------------------------------------------------------------------------
C...Open BELD3
      IF ( .NOT. OPEN3( ENAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '//ENAME
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. DESC3( ENAME ) ) THEN
         XMSG = 'Could not get '//ENAME//' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C...Open crop calendar
      DO II = 1, 3
      IF ( .NOT. OPEN3( CNAME(II), FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// CNAME(II)
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      IF ( .NOT. DESC3( CNAME(II) ) ) THEN
         XMSG = 'Could not get '// CNAME(II) //' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
      END DO                                              ! DONE II

C...Get domain decomp info
C      CALL SUBHFILE ( ENAME, GXOFF, GYOFF,
C     &                STARTCOL, ENDCOL, STARTROW, ENDROW )

!      CALL SUBHFILE ( CNAME(1), GXOFF, GYOFF,
!     &              STARTCOL2, ENDCOL2, STARTROW2, ENDROW2 )

C...Allocate array size	 
!      ALLOCATE ( LADUT( MY_NCOLS,MY_NROWS,NLCP ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating LADUT'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

!      ALLOCATE ( CROPDT(MY_NCOLS,MY_NROWS,NCROP,3),STAT = ALLOCSTAT)
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating CROPDT'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
	    
C...read in 17 species crop land fraction
      DO MM = 1, NLCP
      XMSG = 'Could not read landuse from '//ENAME
      IF ( .NOT. INTERPX (ENAME, VNMLD(MM), PNAME,
     &    STARTCOL2,ENDCOL2,STARTROW2,ENDROW2, 1,1,
     &             JDATE, JTIME, LADUT(1,1,MM) ) )
     &  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END DO
      
C...read in crop calendar 1 begin planting 2 end planting 3 end harvesting
      DO II = 1, 3
      DO MM = 1, NCROP      
      XMSG = 'Could not read '// VCROP(MM) //' from '//CNAME(II)
      IF ( .NOT. INTERPX (CNAME(II), VCROP(MM), PNAME,
     &    STARTCOL2,ENDCOL2,STARTROW2,ENDROW2, 1,1,
     &             JDATE, JTIME, CROPDT(1,1,MM,II) ) )
     &  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END DO                                 ! DONE MM
      END DO                                 ! DONE II
      
C...locate number of days in year
      JDAY = MOD(JDATE, 1000)
C-----------------------------------------------------------------------------
      DO R = 1, NROWS
      DO C = 1, NCOLS
	 AGLAND( C, R ) = 0.0
         TTMP = 0.0
         PCTF = 0.0
	 POTF = 0.0
      DO II = 1, NLCP
	 CRPLD( C, R, II ) = 0.0
         IF( LADUT(C, R, II).GT.0 ) THEN              ! non zero crop cover IF0
            IF( II.EQ. 1 ) MM = 1                     ! Alfalfa
	    IF( II.EQ. 2 ) MM = 2                     ! Barley spring fall
	    IF( II.EQ. 3 ) MM = 4                     ! Corn
	    IF( II.EQ. 4 ) MM = 5                     ! Cotton
	    IF( II.EQ. 5 ) MM = 6                     ! Grass as hay
            IF( II.EQ. 6 ) MM = 6                     ! Hay
	    IF( II.EQ. 7 ) MM = 15                    ! Misc_crop as sugerbeets
	    IF( II.EQ. 8 ) MM = 7                     ! Oats spring fall
	    IF( II.EQ. 9 ) MM = 6                     ! Pasture
	    IF( II.EQ.10 ) MM = 9                     ! Peanuts
	    IF( II.EQ.11 ) MM = 10                    ! Potatoes
	    IF( II.EQ.12 ) MM = 11                    ! Rice
	    IF( II.EQ.13 ) MM = 12                    ! Rye
	    IF( II.EQ.14 ) MM = 13                    ! Sorghum
	    IF( II.EQ.15 ) MM = 14                    ! Soybeans
	    IF( II.EQ.16 ) MM = 16                    ! Tobacco
	    IF( II.EQ.17 ) MM = 17                    ! Wheat spring winter

            IF( CROPDT(C,R,MM,1).EQ.0 ) GOTO 3344
	    	    	    
 7744       CONTINUE

            IF((CROPDT(C,R,MM,1).LT.CROPDT(C,R,MM,2)).AND.
     >          CROPDT(C,R,MM,2).LT.(CROPDT(C,R,MM,1)+14)) THEN  ! extend planting end day if needed IF1
	       CROPDT(C,R,MM,2) = CROPDT(C,R,MM,2)+14
	       IF(CROPDT(C,R,MM,2).GT.365) 
     >            CROPDT(C,R,MM,2) = 365 - CROPDT(C,R,MM,2)
               END IF                                            ! DONE IF1

            IF( (CROPDT(C,R,MM,1)+14).GT.365 ) THEN
	       PRINT *,'- error in cropland erodibility -'
	       STOP
	    END IF

	    IF(((CROPDT(C,R,MM,1).LT.CROPDT(C,R,MM,3)).AND.
     >          (JDAY.LE.CROPDT(C,R,MM,1).OR.
     >           JDAY.GE.CROPDT(C,R,MM,3))).OR.
     >         ((CROPDT(C,R,MM,1).GT.CROPDT(C,R,MM,3)).AND.
     >          (JDAY.GE.CROPDT(C,R,MM,1).AND.
     >           JDAY.LE.CROPDT(C,R,MM,3)))) THEN                ! before planting after harvest IF1
               PCTF = 1.0
	       POTF = 0.0
	       IF((II.NE.4.AND.II.NE.10.AND.II.NE.11
     >             .AND.II.NE.16) .AND.                          ! other than cotton, peanut, potato, tobacco
     >           (((CROPDT(C,R,MM,1).LT.CROPDT(C,R,MM,3)).AND.
     >            (JDAY.LE.(CROPDT(C,R,MM,1)-7)
     >           .OR.JDAY.GE.(CROPDT(C,R,MM,3)+14))).OR.
     >            ((CROPDT(C,R,MM,1).GT.CROPDT(C,R,MM,3)).AND.
     >            (JDAY.LE.(CROPDT(C,R,MM,1)-7) 
     >           .AND.JDAY.GE.(CROPDT(C,R,MM,3)+14)))))
     >            PCTF = 0.0
	    ELSEIF( JDAY.GE.CROPDT(C,R,MM,1).AND.
     >              JDAY.LE.(CROPDT(C,R,MM,1)+14) ) THEN         ! in 2 weeks after planting begin IF1
               TTMP = ( JDAY - CROPDT(C,R,MM,1) ) / 14
               PCTF = 1.0
	       POTF = 0.6 * TTMP
	    ELSEIF(CROPDT(C,R,MM,1).LT.CROPDT(C,R,MM,2)) THEN    ! planting end same year IF1
	       IF( JDAY.GE.(CROPDT(C,R,MM,1)+14).AND.
     >             JDAY.LE.CROPDT(C,R,MM,2) )    THEN            ! after 2 weeks planting to the end planting IF2
                  TTMP = ( JDAY - (CROPDT(C,R,MM,1)+14) )
     >            / ( CROPDT(C,R,MM,2)-(CROPDT(C,R,MM,1)+14))
                  PCTF = 1.0 * TTMP
	          POTF = 0.6 * TTMP
	       ELSE                                              ! in harvesting IF2
                  PCTF = 0.0
	          POTF = 0.0
	       END IF                                            ! DONE IF2
	    ELSE                                                 ! planting start this year end next IF1
	       IF( JDAY.GE.(CROPDT(C,R,MM,2)) ) THEN             ! in harvesting IF3
                  PCTF = 0.0
	          POTF = 0.0
	       ELSE                                              ! in planting  IF3
	          IF( JDAY.GE.(CROPDT(C,R,MM,1)+14) ) THEN       ! in planting at year end IF4
                     TTMP = ( JDAY - (CROPDT(C,R,MM,1)+14) )
     >         / ((CROPDT(C,R,MM,2)+365)-(CROPDT(C,R,MM,1)+14))
	          ELSE                                           ! in planting at year begin IF4
                     TTMP = ((JDAY+365) - (CROPDT(C,R,MM,1)+14))
     >         / ((CROPDT(C,R,MM,2)+365)-(CROPDT(C,R,MM,1)+14))
                  END IF                                         ! DONE IF4
                  PCTF = 1.0 * TTMP
	          POTF = 0.6 * TTMP
	       END IF                                            ! DONE IF3
	    END IF                                               ! DONE IF1
	    
            TWOS = 1.0
	    IF((II.EQ.2.AND.CROPDT(C,R,2,1).NE.0. AND.
     >          CROPDT(C,R,3,1).NE.0).OR.
     >         (II.EQ.8.AND.CROPDT(C,R,7,1).NE.0. AND.
     >          CROPDT(C,R,8,1).NE.0).OR.
     >         (II.EQ.17.AND.CROPDT(C,R,17,1).NE.0. AND.
     >          CROPDT(C,R,18,1).NE.0))         TWOS = 0.5       ! If 2 season crop then 50-50 each

	    CRPLD( C, R, II ) = LADUT( C, R, II ) * TWOS *
     >           ( CTILF(II)*PCTF + 
     >           ( 1.0 - NTILF(II) - CTILF(II) ) * POTF ) 
     
            AGLAND( C, R ) = AGLAND( C, R ) + CRPLD( C, R, II )

            IF( II.EQ.2 .AND. MM.EQ.2 ) THEN                     ! Barley
	       MM = 3
	       GOTO 7744
            ELSEIF( II.EQ.8 .AND. MM.EQ.7 ) THEN                 ! Oats
	       MM = 8
	       GOTO 7744
            ELSEIF( II.EQ.17 .AND. MM.EQ.17 ) THEN               ! Wheat
	       MM = 18
	       GOTO 7744
	       END IF

 3344    CONTINUE
	    
         END IF                                                  ! DONE IF0
      END DO                                                     ! DONE II
      END DO                                                     ! DONE C
      END DO                                                     ! DONE R

C-----------------------------------------------------------------------------
	    
      RETURN
      END
  
