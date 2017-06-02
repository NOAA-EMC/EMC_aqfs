SUBROUTINE rdgribspecs (iunit, gribflnm, gribgrid,  &
                        inmetlays, inmetlaysht, lyrct, lyrctht,   &
                        iuniti, gribflnmi)

!-------------------------------------------------------------------------------
! Name:     Read GRIB Specifications
! Purpose:  Reads grid geometry and date information from GRIB record.
! Notes:    Largely taken from MM5's REGRID/pregrid/grib.misc/rd_grib.F.
!
! Argument list:
!    Input:
!       IUNIT    : "Unit Number" to open and read from. (grib data)
!       IUNITI   : "Unit Number" to open and read from. (grib index)
!       GRIBFLNM : File name
!       GRIBFLNMI: Index name
!
!    Output:
!       GRIBGRID   : Grid information
!       INMETLAYS  : Mid level information
!       INMETLAYSHT: Full level (interface) information
!
! Revised:  ?? ??? 1998  Original version.  (K. Manning, NCAR)
!           16 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           03 Dec 2003  Modified settings of grid geometry variables to
!                        eliminate real number imprecision issues.  (T. Otte)
!           30 Mar 2004  Added variable GRIBGRID.  (T. Otte)
!           10 May 2010  eliminate the hard-wired grib code and use the w3lib
!                        instead                   (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE gridinfo
  USE date_pack
  USE metinfo
  USE premaqparm
  USE grib_mod  !add by lipan

  IMPLICIT NONE

  INTEGER                      :: day
  INTEGER                      :: fcst
  CHARACTER*(*), INTENT(IN)    :: gribflnm 
  INTEGER,       INTENT(OUT)   :: gribgrid
  INTEGER                      :: hour
  INTEGER                      :: icc
  INTEGER                      :: ierr
  INTEGER                      :: igherr
  INTEGER                      :: ihold
  INTEGER,       INTENT(INOUT) :: inmetlays   ( maxmetlays )
  INTEGER,       INTENT(INOUT) :: inmetlaysht ( maxmetlays )
  INTEGER                      :: iparm
  INTEGER                      :: itabe  !add by lipan
  INTEGER                      :: ltype  !add by lipan  
  INTEGER,       INTENT(IN)    :: iunit
  INTEGER                      :: iyy
  INTEGER                      :: ksec1       ( 25 )
  INTEGER                      :: ksec2       ( 22 )
  INTEGER                      :: ktype
  INTEGER                      :: layer, layrs
  INTEGER,       INTENT(INOUT) :: lyrct
  INTEGER,       INTENT(INOUT) :: lyrctht
  INTEGER                      :: minute
  INTEGER                      :: month
  INTEGER                      :: n
  LOGICAL                      :: newlyr
  INTEGER                      :: nunit
  CHARACTER*16,  PARAMETER     :: pname        = 'RDGRIBSPECS'
  INTEGER                      :: year

  CHARACTER*(*), INTENT(IN)    :: gribflnmi
  INTEGER,       INTENT(IN)    :: iuniti
  INTEGER                      :: JPDS        ( 25 )
  INTEGER                      :: JGDS        ( 22 )
  INTEGER                      :: JK, KF, K, IRET, mhlayer

  INTEGER,       PARAMETER     :: JF  =  7500000
  REAL                         :: FU (JF)
  LOGICAL*1                    :: LB (JF)
  
  type(gribfield)              :: gfld      !add by lipan
  integer                      :: jids(200)  ! add by lipan
  integer                      :: jpdt(200)  ! add by lipan
  integer                      :: jgdt(200)  ! add by lipan
  
  integer                      :: jpdtn      ! add by lipan
  integer                      :: jgdtn      ! add by lipan
  integer                      :: kg      ! add by lipan  
  
  logical                      :: unpack     ! add by lipan

!-------------------------------------------------------------------------------
! Extract vertical level information.  This program will work with input
! fields on either sigma levels (KTYPE = 107) or hybrid levels (KTYPE = 109).
! Since we know temperature will be required in the input data set, we will
! use temperature (IPARM = 11) as an example of the mid-levels (or half-levels)
! that are in the input.  Since height (IPARM = 7) may come in on full or
! half-levels, we will need to also look for it separately.  We will assume that
! all other input data are on the mid-levels, and that will be checked
! elsewhere in the main program.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! For the vertical coupling, "layer" will be the # of level of hybrid coord.
! The previous 23 interface sigma, "layer" is the 22 mid-level sigma value, only
! "Geopotential height" & "vertical heat eddy diffusivity", the "layer" is the 23
! interface sigma.
!
! Contrast to previous sigma level data that sigma is from 0 to 1
!      ===> met data is from the top to bottom (lavel 60 to 1)
! The vertical coupling's data is from bottom to top (level 1 to 60), and the
! previous sigma value is replaced by level #
! Also the geopotential height is also on the hybrid level (mid-level) and not on
! the interface (full level)
!
! In order to construct the hybrid sigma value of the nmm coordinate,
! sigma value of aeta1 & aeta2 are now defined in running script as namelist file
! (aeta1 & aeta2 are the average of the 2 sigma levels)
!
! sigma value of each hybrid level (aeta1 & aeta2) need to be assigned to "layer"
!
!   *** Caution: "layer" are " mid-level" sigma based on aeta
! 
!-------------------------------------------------------------------------------

  newlyr = .TRUE.
  unpack = .TRUE.  !add by lipan

  JK = 0          ! skip no data record
  JPDS = -1
  JGDS = -1
  
  jids(:)=-9999
  jpdtn=-1
  jpdt(:)=-9999
  jgdtn=30
  jgdt(:)=-9999
  

  readhour: DO

    JPDS ( 6 ) = 109           ! hybrid level
!    jpdt ( 10:10 ) = 105          ! hybrid level add by lipan
!    print*,jpdt(1:15)

!    CALL GETGB (iunit, iuniti, JF, JK, JPDS,   JGDS, &
!                                  KF,  K, ksec1, ksec2, LB, FU, IRET)
				  
    CALL getgb2 (iunit,iuniti,JK,-1,jids,jpdtn,jpdt,jgdtn,jgdt, &
                                            unpack,kg,gfld,iret)   ! add by lipan
!      print*,'gfld%idsectlen=',gfld%idsectlen
!      print*,'gfld%idsect=', gfld%idsect
!      print*,'gfld%ifldnum=',gfld%ifldnum
!      print*,'gfld%griddef=',gfld%griddef
!      print*,'gfld%ngrdpts=',gfld%ngrdpts
!      print*,'gfld%igdtnum=',gfld%igdtnum
!      print*,'gfld%igdtmpl=',gfld%igdtmpl
!      print*,'gfld%igdtlen=',gfld%igdtlen
!      print*,'gfld%list_opt=',gfld%list_opt
!      print*,'gfld%num_opt=',gfld%num_opt
!      print*,'gfld%ipdtnum=',gfld%ipdtnum
!      print*,'gfld%ipdtlen=',gfld%ipdtlen
!      print*,'gfld%ipdtmpl=',gfld%ipdtmpl
!      print*,'gfld%num_coord=',gfld%num_coord
!      print*,'gfld%coord_list=',gfld%coord_list
!      print*,'gfld%idrtnum=',gfld%idrtnum
!      print*,'gfld%idrtmpl=',gfld%idrtmpl
      
!      print*,"iret,kg=",iret,kg
      				    				  
				  
!    IF ( IRET /= 0 ) THEN
     IF ( IRET .ne. 0 ) THEN    
       IF ( IRET == 96 ) go to 8003
       IF ( IRET == 97 ) go to 8004
       IF ( IRET == 98 ) go to 8005
       IF ( IRET == 99 ) THEN
        CALL getgb2 (iunit,iuniti,0,-1,jids,jpdtn,jpdt,jgdtn,jgdt, &
                                          unpack,kg,gfld,iret)   ! add by lipan       
          exit readhour        ! reach end of data with JPDS (6)
       ENDIF
       go to 8006
    ELSE
!       JK = K
       JK = kg    !add by lipan
    ENDIF

!    iparm = ksec1 ( 5 )    ! temperature as mid level indicater
!    layrs = ksec1 ( 7 )
    
    itabe = gfld%ipdtmpl(1)
    iparm = gfld%ipdtmpl(2)
    ltype = gfld%ipdtmpl(10)
    layrs = gfld%ipdtmpl(12)
!    print*,"itable,iparm,ltype,layrs=",itabe,iparm,ltype,layrs

    IF ( metvert == 1 .and. ltype == 105 ) THEN        ! hydrostatic sigma & avoid layrs equal 0

       layer = metlayh(layrs) * 1000000.    ! same value in fillsigma
!       print*,"layer,layrs,metlayh(layrs)=",layer,layrs,metlayh(layrs)

!       IF ( iparm == 11 ) THEN      ! GRIB ID for temperature
       IF ( itabe == 0 .and. iparm == 0 .and. ltype == 105 ) THEN      ! GRIB ID for temperature       

          !==========================================================
          ! assign sigma to layer and check for no duplicated data
          !==========================================================

          IF ( lyrct == 0 ) THEN
             lyrct = lyrct + 1
             IF ( lyrct > maxmetlays ) GOTO 8000
             inmetlays (lyrct) = layer
!	     print*,"lyrct,inmetlays(lyrct)=",lyrct,inmetlays(lyrct)
          ELSE
             DO n = 1, lyrct
                IF ( layer == inmetlays(n) ) newlyr = .FALSE.
             ENDDO
             IF ( newlyr ) THEN
                lyrct = lyrct + 1
                IF ( lyrct > maxmetlays ) GOTO 8000
                inmetlays (lyrct) = layer
!	     print*,"lyrct,inmetlays(lyrct)=",lyrct,inmetlays(lyrct)		
             ENDIF
          ENDIF

!       ELSEIF ( iparm == 7 ) THEN  ! GRIB ID for height
       ELSEIF ( itabe == 3 .and. iparm == 5 .and. ltype == 105 ) THEN  ! GRIB ID for height
       
          !==========================================================
          ! assign sigma to layer and check for no duplicated data
          !==========================================================

          IF ( lyrctht == 0 ) THEN
             lyrctht = lyrctht + 1
             IF ( lyrctht > maxmetlays ) GOTO 8000
             inmetlaysht (lyrctht) = layer
!	  print*,"lyrctht,inmetlaysht(lyrctht)=",lyrctht,inmetlaysht(lyrctht)
          ELSE
             DO n = 1, lyrctht
                IF ( layer == inmetlaysht(n) ) newlyr = .FALSE.
             ENDDO
             IF ( newlyr ) THEN
                lyrctht = lyrctht + 1
                IF ( lyrctht > maxmetlays ) GOTO 8000
                inmetlaysht (lyrctht) = layer
!	  print*,"lyrctht,inmetlaysht(lyrctht)=",lyrctht,inmetlaysht(lyrctht)		
             ENDIF
          ENDIF

       ENDIF

 !-----------   Caution  -------------------------------------------------
 !
 ! The following part is not completed.  At this moment, we still use
 ! "metvert == 1" to test non-hydrostatic
 !
 ! Further work need to be done in the running script to justify the "2"
 !
 !------------------------------------------------------------------------

    ELSEIF ( metvert == 2 .and. ltype == 105 ) THEN   ! non-hydrostatic hybrid sigma-pressure

       IF ( iparm == 11 ) THEN     ! GRIB ID for temperature

          !==========================================================
          ! assign sigma to layer and check for no duplicated data
          !==========================================================

          IF ( lyrct == 0 ) THEN
             lyrct = lyrct + 1
             IF ( lyrct > maxmetlays ) GOTO 8000
             inmetlays(lyrct) = layrs
          ELSE
             DO n = 1, lyrct
                IF ( layer == inmetlays(n) ) newlyr = .FALSE.
             ENDDO
             IF ( newlyr ) THEN
                lyrct = lyrct + 1
                IF ( lyrct > maxmetlays ) GOTO 8000
                inmetlays(lyrct) = layrs
             ENDIF
          ENDIF

       ELSEIF ( iparm == 7 ) THEN  ! GRIB ID for height

          !==========================================================
          ! assign sigma to layer and check for no duplicated data
          !==========================================================

          IF ( lyrctht == 0 ) THEN
             lyrctht = lyrctht + 1
             IF ( lyrctht > maxmetlays ) GOTO 8000
             inmetlaysht(lyrctht) = layrs
          ELSE
             DO n = 1, lyrctht
                IF ( layer == inmetlaysht(n) ) newlyr = .FALSE.
             ENDDO
             IF ( newlyr ) THEN
                lyrctht = lyrctht + 1
                IF ( lyrctht > maxmetlays ) GOTO 8000
                inmetlaysht(lyrctht) = layrs
             ENDIF
          ENDIF

       ENDIF

    ENDIF

  ENDDO readhour
!      print*,'gfld%igdtnum=',gfld%igdtnum
!      print*,'gfld%igdtmpl=',gfld%igdtmpl
!      print*,'gfld%igdtlen=',gfld%igdtlen  
!  stop

!-------------------------------------------------------------------------------
! Check whether data contains all the required layers
!-------------------------------------------------------------------------------

 ! mhlayer = SIZE (metlayh, 1)

 ! if ( lyrct   /= mhlayer ) go to 8007
 ! if ( lyrctht /= mhlayer ) go to 8007

!-------------------------------------------------------------------------------
! Store information about the grid on which the data is. 
! This stuff gets stored in the MAP variable, as defined in module GRIDINFO.
!-------------------------------------------------------------------------------

!  gribgrid = 138 !ksec1(3)          ! Grid identification
  IF ( gfld%igdtmpl(8) == 201 .and. gfld%igdtmpl(9) == 165 ) then
   gribgrid = 140 !ksec1(3)          ! Grid identification
  else if ( gfld%igdtmpl(8) == 468 .and. gfld%igdtmpl(9) == 288 ) then
   gribgrid = 138
  else if ( gfld%igdtmpl(8) == 82 .and. gfld%igdtmpl(9) == 54 ) then
   gribgrid = 139
  else 
   print*, "unknown gribgrid", gfld%igdtmpl(8), gfld%igdtmpl(9)
  endif
  
  print*, "gribgrid=", gribgrid

  !----------------------------
  ! map projection
  !----------------------------

  IF ( ksec2(1) == 205 ) THEN     ! Arakawa Non-E Staggered rotated Lat/Lon Grid
    map%igrid = 205
    map%nx    = ksec2( 2)
    map%ny    = ksec2( 3)
    map%lat1  = ksec2( 4) / 1000.0     ! lat of grid point (1,1)
    map%lon1  = ksec2( 5) / 1000.0     ! lon of grid point (1,1)
    map%cenlat= ksec2( 7) / 1000.0     ! center lat of grid
    map%cenlon= ksec2( 8) / 1000.0     ! center lon of grid
    map%dx    = ksec2( 9) / 1000.0     ! dlon
    map%dy    = ksec2(10) / 1000.0     ! dlat
    ! map%latn  = ksec2(12) / 1000.0     ! lat of grid point (nx,ny)
    ! map%lonn  = ksec2(13) / 1000.0     ! lon of grid point (nx,ny)
    map%truelat1 = 0.0
    map%truelat2 = 0.0
    map%lov      = 0.0

  ELSE IF ( ksec2(1) == 1 ) THEN  ! Mercator  projection grid
    map%igrid    = 1
    map%nx       = ksec2( 2)
    map%ny       = ksec2( 3)
    map%lat1     = ksec2( 4) / 1000.0
    map%lon1     = ksec2( 5) / 1000.0
    map%dx       = ksec2(12) / 1000.0   ! LONGITUDINAL DIR 
    map%dy       = ksec2(13) / 1000.0   ! LATITUDINAL DIR

    map%truelat1 = ksec2( 9) / 1000.0   ! ?????, not sure

!  ELSE IF ( ksec2(1) == 3 ) THEN  ! Lambert Conformal projection
  ELSE IF ( gfld%igdtnum == 30 ) THEN  ! Lambert Conformal projection  
    map%igrid    = 3
    map%nx       = gfld%igdtmpl(8) ! ksec2( 2)
    map%ny       = gfld%igdtmpl(9) ! ksec2( 3)
    map%lat1     = gfld%igdtmpl(10) / 1000000.0 ! ksec2( 4) / 1000.0
    map%lon1     = gfld%igdtmpl(11) / 1000000.0 ! ksec2( 5) / 1000.0
    map%lov      = gfld%igdtmpl(14) / 1000000.0 ! ksec2( 7) / 1000.0
    map%dx       = gfld%igdtmpl(15) / 1000000.0 ! ksec2( 8) / 1000.0
    map%dy       = gfld%igdtmpl(16) / 1000000.0 ! ksec2( 9) / 1000.0
    map%truelat1 = gfld%igdtmpl(19) / 1000000.0 ! ksec2(12) / 1000.0
    map%truelat2 = gfld%igdtmpl(20) / 1000000.0 ! ksec2(13) / 1000.0
         
  ELSE IF ( ksec2(1) == 4 ) THEN  ! Gaussian Lat/Lon Grid
    map%igrid = 0
    map%nx    = ksec2( 2)
    map%ny    = ksec2( 3)
    map%lat1  = ksec2( 4) / 1000.0
    map%lon1  = ksec2( 5) / 1000.0
    map%dx    = ksec2( 9) / 1000.0
    map%dy    = ksec2(10) / 1000.0

  ELSE IF ( ksec2(1) == 5 ) THEN  ! Polar-Stereographic projection Grid.
    map%igrid    = 5
    map%nx       = ksec2( 2)
    map%ny       = ksec2( 3)
    map%lat1     = ksec2( 4) / 1000.0
    map%lon1     = ksec2( 5) / 1000.0
    map%lov      = ksec2( 7) / 1000.0
    map%dx       = ksec2( 8) / 1000.0
    map%dy       = ksec2( 9) / 1000.0
    map%truelat1 = 60.0
    map%truelat2 = 91.0

  ELSE
    PRINT*, 'Unknown ksec2(4): ', ksec2(1)
  ENDIF

!-------------------------------------------------------------------------------
! Adjust grid geometry variables to eliminate real number imprecision issues.
!-------------------------------------------------------------------------------

  ihold = NINT(map%lov       * 1000.0)
  map%lov      = REAL(ihold) / 1000.0

  ihold = NINT(map%truelat1  * 1000.0)
  map%truelat1 = REAL(ihold) / 1000.0

  ihold = NINT(map%truelat2  * 1000.0)
  map%truelat2 = REAL(ihold) / 1000.0

  ihold = NINT(map%dx        * 1000.0)
  map%dx       = REAL(ihold) / 1000.0

  ihold = NINT(map%dy        * 1000.0)
  map%dy       = REAL(ihold) / 1000.0
  
  map%lon1=map%lon1-360.0
  map%lov=map%lov-360.0
  
  
!  print*,"lyrct=",lyrct
!  print*,"lyrctht=",lyrctht
!  print*,"inmetlays(lyrct)=",inmetlays(1:60)
!  print*,"inmetlaysht(lyrctht)=",inmetlaysht(1:60)
  
!  print*,"gribgrid=",gribgrid
!  print*,"map%igrid=",map%igrid
!  print*,"map%nx,map%ny=",map%nx,map%ny
!  print*,"map%lat1,map%lon1=",map%lat1,map%lon1
!  print*,"map%lov=",map%lov
!  print*,"map%dx,map%dy=",map%dx,map%dy
!  print*,"map%truelat1,map%truelat2=",map%truelat1,map%truelat2
!  stop

!-------------------------------------------------------------------------------

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000)
      GOTO 1001

 8003 WRITE (6,9003)
      GOTO 1001

 8004 WRITE (6,9004)
      GOTO 1001

 8005 WRITE (6,9005)
      GOTO 1001

 8006 WRITE (6,9006)
      GOTO 1001

 8007 WRITE (6,9007) mhlayer, lyrct, lyrctht
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   MAXMETLAYS HAS BEEN EXCEEDED',                 &
              /, 1x, 70('*'))

 9003 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   GETGB: ERROR READING INDEX FILE',              &
              /, 1x, 70('*'))

 9004 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   GETGB: ERROR READING GRIB FILE',               &
              /, 1x, 70('*'))

 9005 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   GETGB: NUMBER OF DATA POINTS GREATER THAN JF', &
              /, 1x, 70('*'))

 9006 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   GETGB: W3FI63 GRIB UNPACKER RETURN CODE',      &
              /, 1x, 70('*'))

 9007 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   DATA IS NOT MATCHED DEFINED MET LAYER NUMBER', &
              /, 1x, '***   DEFINED MET LAYER NUMBER = ', i4,              &
              /, 1x, '***   NUMBER OF MID LAYER IN DATA = ', i4,           &
              /, 1x, '***   NUMBER OF FULL LAYER IN DATA = ', i4,          &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdgribspecs
