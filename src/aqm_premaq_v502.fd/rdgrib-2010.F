SUBROUTINE rdgrib (iunit, gribflnm, iuniti, gribflnmi, ivar,  hdate)

!-------------------------------------------------------------------------------
! Name:     Read GRIB
! Purpose:  Reads one record from an input GRIB file and determines whether
!           or not to keep the data based on user input.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/grib.misc/rd_grib.
! Revised:  16 May 2002  Original version in REGRID.  (NCAR)
!           19 May 2003  Modified for PREMAQ.  (T. Otte)
!           03 Dec 2003  Added provisions for KTYPE=7 (trop ht).  (T. Otte)
!           05 Jan 2004  Added variable IPCP to keep track of when Eta
!                        accumulated precipitation buckets are dumped. (T. Otte)
!           30 Mar 2004  Removed INFIL and replaced with TBLNUM in calling
!                        list.  Retrieve TBL from PDS octet 4.  (T. Otte)
!           15 Dec 2004  Moved the allocation of SCR1D inside the nested IF block
!                        (David Wong)
!           03 Jan 2005  Reduced the frequency of allocating/deallocating
!                        variable scr1d. (David Wong)
!           20 Jan 2005  Extra criteria "tr" for extracting instant of average
!                        "0" => instaneous, "3" => average, "4" => accumulated
!                        check "filltable.F" for detail
!                        Change "lvl2=0" in ktype=1 to be consistent with new
!                        "filltable.F"                          (Hsin-mu Lin)
!           08 Feb 2005  1. Modified the varilables assignment to be consistent 
!                           with "filltable.F" and eliminate previously defined
!                           argument.
!                        2. Remove dummy tblnum from rdgrib and change it to be
!                           local varilable. 
!                        3. "odate" to replace the 2nd "hdate" in geth_newdate
!                                                                  (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE date_pack
! USE gridinfo
  USE table
  USE metvars, ONLY: scr2d
  USE grib_mod  !add by lipan  

  IMPLICIT NONE

  INTEGER                      :: day
  INTEGER                      :: fcst
  LOGICAL                      :: gotone
  CHARACTER*(*), INTENT(IN)    :: gribflnm
  CHARACTER*24,  INTENT(IN)    :: hdate
  INTEGER                      :: hour
  INTEGER                      :: i
  INTEGER                      :: icc
  INTEGER                      :: ierr
  INTEGER                      :: iparm
  INTEGER                      :: ipcp
  INTEGER,       INTENT(IN)    :: iunit
  INTEGER                      :: iyy
  INTEGER                      :: ksec1      ( 25 )
  INTEGER                      :: ksec2      ( 22 )
  INTEGER                      :: ktype
  INTEGER                      :: ztype  !add by lipan
  INTEGER                      :: itabe  !add by lipan  
  INTEGER                      :: lvl1
  INTEGER                      :: lvl2
  INTEGER                      :: minute
  INTEGER                      :: month
  INTEGER                      :: nunit
  CHARACTER*24                 :: odate, fcstdate
  REAL,          ALLOCATABLE   :: scr1d      ( : )
  INTEGER                      :: year

  CHARACTER*16,  PARAMETER     :: pname        = 'RDGRIB'

  CHARACTER*(*), INTENT(IN)    :: gribflnmi
  INTEGER,       INTENT(IN)    :: iuniti
  INTEGER,       INTENT(IN)    :: ivar
  INTEGER                      :: JPDS       ( 25 )
  INTEGER                      :: JGDS       ( 22 )
  INTEGER                      :: JK, KF, K, IRET

  INTEGER,       PARAMETER     :: JF  =  7500000
  INTEGER                      :: kpds7
  LOGICAL*1                    :: LB (JF)
  
  type(gribfield)              :: gfld      !add by lipan
  integer                      :: jids(200)  ! add by lipan
  integer                      :: jpdt(200)  ! add by lipan
  integer                      :: jgdt(200)  ! add by lipan
  
  integer                      :: jdisc      ! add by lipan
  integer                      :: jpdtn      ! add by lipan
  integer                      :: jgdtn      ! add by lipan
  integer                      :: kg      ! add by lipan  
  
  logical                      :: unpack     ! add by lipan  

!-------------------------------------------------------------------------------
! ON388-TABLE 3 & TABLE 3A
!   ** here are examples that are used in our routine. Check ON388 for more
! 
!  JPDS(6)
!
!    1. <= 99 .or. >= 204             JPDS(7)= 0
!       lvl1 = 0
!       lvl2 = 0
!
!    2. == 100, 103, 105, 107, 109    JPDS(7)= lvl1  ! exact related value
!       lvl1 = JPDS(7)
!       lvl2 = 0
!
!    ** note: 256 is Octets #, 2^8 ?
!
!    3. == 101, 104, 106, 108, 110    JPDS(7)= lvl1*256+lvl2
!       lvl1 = JPDS(7) / 256
!       lvl2 = MOD ( JPDS(7), 256 )
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Define the criteria of data that need to be extracted by using the variables
! defined in the variable request list "filltable.F".
!-------------------------------------------------------------------------------
 
  unpack = .true.  !add by lipan
  
  jids(:)=-9999
  jpdtn=-1
  jpdt(:)=-9999
  jgdtn=30
  jgdt(:)=-9999  
  

  JK = 0          ! skip no data record
  JPDS = -1
  JGDS = -1

!  i = ivar

!  JPDS( 6) = lcode(i)      ! type of level
!  JPDS( 5) = gcode(i)      ! parameter
!  JPDS(16) = tr   (i)      ! 3hr ave=> 3, instaneous=>0
!  JPDS(19) = tbl  (i)      ! GRIB parameter table
   
!   jdisc=-1
      
!-------------------------------------------------------------------------------
! Condition for KPDS(7) that related to l1(i) & l2(i) defined in "filltable"
!-------------------------------------------------------------------------------

!  ktype = lcode(i)

!  IF ( ( ktype <= 99 ) .or. ( ktype >= 204 ) ) THEN
!     kpds7 = 0
!  ELSEIF ( ( ktype == 100 )   .or.  &  ! isobaric level (mb, hPa)
!           ( ktype == 103 )   .or.  &  ! specific altitude above MSL (m)
!           ( ktype == 105 )   .or.  &  ! specific height level above ground (m)
!           ( ktype == 107 )   .or.  &  ! sigma level (value in 1/10000)
!           ( ktype == 109 ) ) THEN     ! Hybrid level (level #)
!     kpds7 = l1 (i)                    ! exact related value
!  ELSEIF ( ( ktype == 101 )   .or.  &  ! layer btw 2 isobaric levels (kPa)
!           ( ktype == 104 )   .or.  &  ! layer btw 2 specific alt abv MSL (hm)
!           ( ktype == 106 )   .or.  &  ! layer btw 2 specific hgt lvl abv gd (hm)
!           ( ktype == 108 )   .or.  &  ! layer btw 2 sigma lvl (value at 1/100)
!           ( ktype == 110 )   .or.  &  ! layer btw 2 hybrid levels (level #)
!           ( ktype == 112 )) THEN      ! layer btw 2 soil levels (level #)
!     kpds7 =l1 (i) * 256 + l2 (i)
!  ELSE
!     go to 8007
!  ENDIF

!-------------------------------------------------------------------------------
! when every layer is needed, the value for "l1" in "filltable" will be -88.
! The criteria of JPDS (7) will be omitted
!-------------------------------------------------------------------------------

!  IF ( kpds7 >= 0 ) THEN
!     JPDS ( 7) = kpds7           ! ON388, table 3 & table 3a
!  ENDIF

!-------------------------------------------------------------------------------
! Reshape &  Extract data that match JPDS(5), JPDS(6), JPDS(7), JPDS(16),
! JPDS(19) defined in variable request list "filltable.F".
!-------------------------------------------------------------------------------

  readfile1: DO

!    ALLOCATE ( scr1d (SIZE(scr2d)) )
!    scr1d = RESHAPE (scr2d, (/SIZE(scr1d)/))

    gotone = .FALSE.

!    CALL GETGB ( iunit, iuniti, JF, JK, JPDS, JGDS,    &
!                                KF,  K, ksec1, ksec2, LB, scr1d, IRET )
   
    CALL getgb2 (iunit,iuniti,JK,-1,jids,jpdtn,jpdt,jgdtn,jgdt, &
                                        unpack,kg,gfld,iret)   ! add by lipan 
									
				
    IF ( IRET /= 0 ) THEN
       IF ( IRET == 96 ) go to 8003
       IF ( IRET == 97 ) go to 8004
       IF ( IRET == 98 ) go to 8005
       EXIT readfile1         ! reach end of data
    ELSE
       gotone = .TRUE.
!       JK = K            ! data skip for the next extracting process, kpds7 < 0 
       JK = kg
    ENDIF

!-------------------------------------------------------------------------------
! date & time of data's initial cycle based on GRIB information
!-------------------------------------------------------------------------------

!    icc    = ksec1(21)   ! century of the data
!    iyy    = ksec1( 8)   ! two-digit year of the data
!    month  = ksec1( 9)   ! month of the data
!    day    = ksec1(10)   ! day of the data
!    hour   = ksec1(11)   ! hour of the data
!    minute = ksec1(12)   ! minute of the data
!  
! for grib2 add by lipan    
    icc    = gfld%idsect(6)/100+1   ! century of the data
    iyy    = mod(gfld%idsect(6),100)   ! two-digit year of the data
    month  = gfld%idsect(7)   ! month of the data
    day    = gfld%idsect(8)   ! day of the data
    hour   = gfld%idsect(9)   ! hour of the data
    minute = gfld%idsect(10)   ! minute of the data    
    
!    print*,"icc,iyy,month,day,hour,minute=",icc,iyy,month,day,hour,minute
!    stop

!-------------------------------------------------------------------------------
! forecast time (hr) information.
!-------------------------------------------------------------------------------

!    IF ( ksec1(16) == 3 ) THEN
!      fcst = ksec1(15)
!        !
!        !-- the following process doesn't fit the data format of Eta post
!        !   ex. data of 42hr, the criteria will be  "40-42" not "41-43"
!        !   this process can be active when the data is restructured (Hsin-mu Lin)
!        !
!        ! fcst = ( ksec1(14) + ksec1(15) ) / 2
!    ELSEIF ( ( ksec1(16) == 4 ) .OR. ( ksec1(16) == 5 ) ) THEN
!      fcst = ksec1(15)
!    ELSE
!      fcst = ksec1(14)
!    ENDIF

! for grib2 add by lipan

    IF ( gfld%ipdtnum == 0 ) THEN
      fcst = gfld%ipdtmpl(9)       !forecast
!      print*,"forecast=",fcst,gfld%ipdtmpl(9)
    ELSEIF ( gfld%ipdtnum == 8 ) THEN
      fcst = gfld%ipdtmpl(9) + gfld%ipdtmpl(27)  !average or accumulation
!      print*,"ave or acc=", fcst,gfld%ipdtmpl(9),gfld%ipdtmpl(27)
    ELSE
      GOTO 8006
    ENDIF
    
!    print*,"fcst,gfld%ipdtnum,gfld%ipdtmpl(9),gfld%ipdtmpl(27)=",  &
!     fcst,gfld%ipdtnum,gfld%ipdtmpl(9),gfld%ipdtmpl(27)

!-------------------------------------------------------------------------------
! Build the 24-character string for data's initial date & current forecast date
! *** checking the timing information for each record extracted from "GETGB" is
!     consistent to forecast
!-------------------------------------------------------------------------------

    IF ( iyy == 00 ) THEN
      year = icc * 100
    ELSE
      year = (icc-1) * 100 + iyy
    ENDIF

    WRITE (odate, '(i4, a, i2.2, a, i2.2, a, i2.2, a, i2.2, a)') &
      year, '-', month, '-', day, '-', hour, ':', minute, ':00.0000'

    CALL geth_newdate (fcstdate, odate, 3600*fcst)
!    print*,"fcstdate=",fcstdate
!    print*,"hdate=",hdate    

    IF ( fcstdate /= hdate ) THEN
       GOTO 8000
    ENDIF

!-------------------------------------------------------------------------------
! Determine whether Eta accumulated precipitation bucket has just been emptied.
! Look at Octets 21, 19, and 20 (KSEC1 16, 15, and 14, respectively).
! If KSEC1(16)=4, the GRIB variable represents an accumulated total from
! KSEC1(14) to KSEC1(15), in hours.  If there is only a one-hour difference
! between KSEC1(14) and KSEC1(15), the bucket has just been emptied.
! Since CMAQ needs an hourly total, we do not subtract the previous hour's
! "bucket" total just after the bucket has been emptied.
! (This adjustment takes place in fill_metarys.F.)
!-------------------------------------------------------------------------------

!    IF ( ( ksec1(16) == 4 ) .AND. ( ksec1(15) - ksec1(14) == 1 ) ) THEN
    IF ( gfld%ipdtnum == 8 ) THEN
      IF (( gfld%ipdtmpl(27) - gfld%ipdtmpl(9) == 1 ) ) THEN    
       ipcp = 1  ! accumulated precip is 1-h total...don't subtract prev hr
      ELSE
       ipcp = 0
     ENDIF
    ENDIF

!-------------------------------------------------------------------------------
! reshape data extract from "GETGB" and fill in the defined meteorlogy domain
!-------------------------------------------------------------------------------

!    scr2d = RESHAPE (scr1d, (/SIZE(scr2d,1),SIZE(scr2d,2)/))

!for grib2 add by lipan
    
    scr2d = RESHAPE (gfld%fld, (/SIZE(scr2d,1),SIZE(scr2d,2)/))    

!    iparm = ksec1 (5)
!    ktype = ksec1 (6)
!    lvl1  = ksec1 (7)

! for grib2 add by lipan 
    jdisc = gfld%discipline
    itabe = gfld%ipdtmpl(1)   
    iparm = gfld%ipdtmpl(2) 
    ktype = gfld%ipdtmpl(10) 
    lvl1  = gfld%ipdtmpl(12)
    ztype = gfld%ipdtmpl(13)     
    lvl2  = gfld%ipdtmpl(15) 
    
!    print*,"gfld%discipline=",gfld%discipline 
!    print*, "jdisc=",jdisc  

!    if (jdisc == 2) then
!     print*,itabe,iparm,ktype,lvl1,ztype,lvl2
!    endif
    
    
!     print*, kg,jdisc,itabe,iparm
!     print*, ktype,lvl1,ztype,lvl2  !add by lipan

!    CALL fill_metarys (iparm, ktype, lvl1, ipcp)

!for grib2 add by lipan
    CALL fill_metarys (jdisc,itabe,iparm,ktype,ztype,lvl1,lvl2,ipcp)    

!-------------------------------------------------------------------------------
! go another loop if need data from more layers, KPDS < 0
!-------------------------------------------------------------------------------

!    DEALLOCATE ( scr1d )
    call gf_free(gfld)

!    IF ( kpds7 >= 0 ) THEN
!       EXIT readfile1
!    ENDIF

  ENDDO readfile1
  
!  stop

  RETURN


!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) fcstdate, hdate, ksec1(5)
      GOTO 1001

 8003 WRITE (6,9003)
      GOTO 1001

 8004 WRITE (6,9004)
      GOTO 1001

 8005 WRITE (6,9005)
      GOTO 1001
      
 8006 WRITE (6,9006) gfld%ipdtnum
      GOTO 1001      

 8007 WRITE (6,9007) ktype
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '***   UNEXPECTED VALID DATE IN INPUT FIELD',         &
              /, 1x, '***   DATE IN FILE  = ', a,                          &
              /, 1x, '***   EXPECTED DATE = ', a,                          &
              /, 1x, '***   INPUT FIELD (gcode) kpds(5)= ', i4,            &
              /, 1x, 70('*'))

 9003 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '***   GETGB: ERROR READING INDEX FILE',              &
              /, 1x, 70('*'))

 9004 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '***   GETGB: ERROR READING GRIB FILE',               &
              /, 1x, 70('*'))

 9005 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '***   GETGB: NUMBER OF DATA POINTS GREATER THAN JF', &
              /, 1x, 70('*'))

 9006 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '*** GETGB: UNDEFINED PRODUCT DEFINITION TEMPLATE NUMBER', &
              /, 1x, 70('*'))
	      
 9007 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIB',                             &
              /, 1x, '***   UNDEFINED PDS6 ', i3,                          &
              /, 1x, '***   UNABLE TO DEFINE PDS7 ON CURRENT PDS5',        &
              /, 1x, '***   CHECK ON388 SECTION 1 & TABLE 3',               &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END subroutine rdgrib
