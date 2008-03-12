SUBROUTINE rdgrib (iunit, gribflnm, hdate, scr2d, ierr, iuarr, gotone,  &
                   iparm, ktype, lvl1, lvl2, ipcp)

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
  USE gridinfo
  USE table

  IMPLICIT NONE

  INTEGER                      :: day
  INTEGER                      :: fcst
  REAL                         :: ginfo      ( 40 )
  LOGICAL,       INTENT(INOUT) :: gotone
  CHARACTER*(*), INTENT(IN)    :: gribflnm
  CHARACTER*24,  INTENT(OUT)   :: hdate
  INTEGER                      :: hour
  INTEGER                      :: i
  INTEGER                      :: icc
  INTEGER                      :: ierr
  INTEGER                      :: igherr
  INTEGER                      :: infogrid   ( 40 )
  INTEGER,       INTENT(OUT)   :: iparm
  INTEGER,       INTENT(OUT)   :: ipcp
  INTEGER,       INTENT(INOUT) :: iuarr      ( 255 )
  INTEGER,       INTENT(INOUT) :: iunit              ! for C pointer
  INTEGER                      :: iyy
  INTEGER                      :: ksec1      ( 100 )
  INTEGER                      :: ksec2      ( 10 )
  INTEGER,       INTENT(OUT)   :: ktype
  INTEGER,       INTENT(OUT)   :: lvl1
  INTEGER,       INTENT(OUT)   :: lvl2
  INTEGER                      :: minute
  INTEGER                      :: month
  INTEGER                      :: nunit
  CHARACTER*24                 :: odate
  REAL,          ALLOCATABLE   :: scr1d      ( : )
  REAL,          INTENT(OUT)   :: scr2d      ( : , : )
  INTEGER                      :: tblnum
  INTEGER                      :: year

  INTERFACE

    SUBROUTINE gribdata (datarray)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: datarray   ( : )
    END SUBROUTINE gribdata

  END INTERFACE

!-------------------------------------------------------------------------------
! If the file GRIBFLNM has not been opened, then IUARR(IUNIT) should be Zero.
! In this case, open the file GRIBFLNM, and store the UNIX File descriptor
! in to IUARR(IUNIT).  This way, we'll know what UNIX File descriptor to use
! next time we call this subroutine.
!-------------------------------------------------------------------------------

  ierr = 0
  IF ( iuarr(iunit) == 0 ) THEN
    CALL copen (iunit, nunit, gribflnm, 1, ierr, 1)
    IF ( ierr /= 0 ) THEN
      CALL deallogrib
      ierr = 2
      RETURN
    ENDIF
    iuarr(iunit) = nunit
  ENDIF

!-------------------------------------------------------------------------------
! Read a single GRIB record, but do no unpacking now.
!-------------------------------------------------------------------------------

  CALL gribget (iuarr(iunit), ierr)

  IF ( ierr /= 0 ) THEN
    CALL deallogrib
    RETURN
  ENDIF

!-------------------------------------------------------------------------------
! Unpack the header information.
!-------------------------------------------------------------------------------

  CALL gribheader (igherr)
  IF ( igherr /= 0 ) THEN
    gotone = .FALSE.
    CALL deallogrib
    RETURN
  ENDIF

!-------------------------------------------------------------------------------
! Copy header information to arrays KSEC1, KSEC2, INFOGRID, and GRIDINFO.
!-------------------------------------------------------------------------------

  CALL get_sec1 (ksec1)
  CALL get_sec2 (ksec2)
  CALL get_gridinfo (infogrid, ginfo)

!-------------------------------------------------------------------------------
! date & time of initial data, based on GRIB header date and time information
!-------------------------------------------------------------------------------

  icc    = ksec1(22)   ! century of the data
  iyy    = ksec1(11)   ! two-digit year of the data
  month  = ksec1(12)   ! month of the data
  day    = ksec1(13)   ! day of the data
  hour   = ksec1(14)   ! hour of the data
  minute = ksec1(15)   ! minute of the data

!-------------------------------------------------------------------------------
! forecast time information.
!-------------------------------------------------------------------------------

  IF ( ksec1(19) == 3 ) THEN
    fcst = ksec1(18)
      !
      !-- the following process doesn't fit the data format of Eta post
      !   ex. data of 42hr, the criteria will be  "40-42" not "41-43"
      !   this process can be active when the data is restructured (Hsin-mu Lin)
      !
      ! fcst = ( ksec1(17)+ksec1(18) ) / 2
  ELSE IF ( ( ksec1(19) == 4 ) .OR. ( ksec1(19) == 5 ) ) THEN
    fcst = ksec1(18)
  ELSE
    fcst = ksec1(17)
  ENDIF

!-------------------------------------------------------------------------------
! Build the 24-character date string of initial data & current forecast data
! ***  This process "hdate & odate"  is for debuging purpose only.
!-------------------------------------------------------------------------------

  IF ( iyy == 00 ) THEN
    year = icc * 100
  ELSE
    year = (icc-1) * 100 + iyy
  ENDIF

  WRITE (odate, '(i4, a, i2.2, a, i2.2, a, i2.2, a, i2.2, a)') &
    year, '-', month, '-', day, '-', hour, ':', minute, ':00.0000'

  CALL geth_newdate (hdate, odate, 3600*fcst)

!-------------------------------------------------------------------------------
! Determine whether or not Eta accumulated precipitation bucket has just
! been emptied.  Look at Octets 21, 19, and 20 (KSEC1 19, 17, and 18,
! respectively).  If KSEC1(19)=4, the GRIB variable represents an
! accumulated total from KSEC1(17) to KSEC1(18), in hours.  If there is only
! a one-hour difference between KSEC1(17) and KSEC1(18), the bucket has just
! been emptied.  Since CMAQ needs an hourly total, we do not subtract the
! previous hour's "bucket" total just after the bucket has been emptied.
! (This adjustment takes place in fill_metarys.F.)
!-------------------------------------------------------------------------------

  IF ( ( ksec1(19) == 4 ) .AND. ( ksec1(18) - ksec1(17) == 1 ) ) THEN
    ipcp = 1  ! accumulated precip is 1-h total...don't subtract prev hr
  ELSE
    ipcp = 0
  ENDIF

!-------------------------------------------------------------------------------
! Extract GRIB field ID and level information.
!-------------------------------------------------------------------------------

  tblnum = ksec1(2)                   ! GRIB parameter table
  iparm  = ksec1(7)                   ! parameter
  ktype  = ksec1(8)                   ! type of level

  IF ( ktype == 100 ) THEN            ! isobaric level
    lvl1 = FLOAT(ksec1(9))  * 100.0
    lvl2 = ksec1(10)
  ELSE
    lvl1 = ksec1( 9)
    lvl2 = ksec1(10)
  ENDIF

!-------------------------------------------------------------------------------
! Extract and reshape data which match the combination of IPARM, KTYPE, LVL1,
! LVL2, and TBLNUM defined in the variable request list "filltable.F".
!-------------------------------------------------------------------------------

  ALLOCATE ( scr1d (SIZE(scr2d)) )
  varloop: DO i = 1, maxvar

    IF ( gcode(i) == iparm ) THEN
       IF ( lcode(i) == ktype ) THEN
          IF ( ( l1(i) == lvl1 ) .OR. ( l1(i) == splatcode ) ) THEN
             IF ( l2(i) == lvl2 ) THEN
                IF ( tr(i) == ksec1(19) ) THEN
                   IF ( tbl(i) == tblnum ) THEN           ! read in GRIB field
                      gotone = .TRUE.
!                     ALLOCATE ( scr1d (SIZE(scr2d)) )
                      scr1d = RESHAPE (scr2d, (/SIZE(scr1d)/))
                      CALL gribdata (scr1d)
                      scr2d = RESHAPE (scr1d, (/SIZE(scr2d,1),SIZE(scr2d,2)/))
!                     DEALLOCATE ( scr1d )
                      EXIT varloop
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ENDIF

  ENDDO varloop
  DEALLOCATE ( scr1d )

!-------------------------------------------------------------------------------
! Deallocate a couple of arrays that may have been allocated by the
! GRIB decoding routines.
!-------------------------------------------------------------------------------

  CALL deallogrib

END subroutine rdgrib
