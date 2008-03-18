subroutine gribheader(ierr)
!
! IERR non-zero means there was a problem unpacking the grib header.
!
  use module_grib
  implicit none
  integer :: ierr

  integer, parameter :: nsec1 = 24

  integer, dimension(nsec1) :: &
       iw1=(/3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2/)
  integer :: icount, iskip, ibts, nbm, nbz, i9skip, i17skip

  integer :: iman, ichar, isign, iscan

  integer,  allocatable, dimension(:) :: bm8

  real :: r
  integer :: isvns
  integer :: gsvns = 0
  integer :: bmsize

  if (gsvns.eq.0) then
     if (mwsize.eq.32) then
        gsvns = transfer('7777', gsvns)
     elseif(mwsize.eq.64) then
        call gbyte(char(0)//char(0)//char(0)//char(0)//'7777', gsvns, 0, mwsize)
     endif
  endif

! Section 0:
  sec0(2) = ied
  if (ied.eq.1) then
     call gbyte(grec, sec0(1), 32, 24)
     iskip = 64
  elseif (ied.eq.0) then
     sec0(1) = gribsize(grec,200)
     iskip = 32
  endif

! Section 1:
  i9skip = iskip + 80
  i17skip = iskip + 144
  do icount = 1, nsec1 - ((1-ied)*6)
     ibts = iw1(icount)*8
     call gbyte(grec, sec1(icount), iskip, ibts)
     iskip = iskip + ibts
  enddo
  if (ied.eq.0) sec1(22) = 20
  ! Sec1 indices 9 and 10 might actually be one value, not two.
  ! If this is the case, reread sec1(9), and set sec1(10) to zero:
  if ( (sec1(8) == 101) .or. (sec1(8) == 104) .or. (sec1(8) == 106) .or. &
       (sec1(8) == 108) .or. (sec1(8) == 110) .or. (sec1(8) == 112) .or. &
       (sec1(8) == 114) .or. (sec1(8) == 116) .or. (sec1(8) == 120) .or. &
       (sec1(8) == 121) .or. (sec1(8) == 128) .or. (sec1(8) == 141) ) then
     ! No action here.
  else
     call gbyte(grec, sec1(9), i9skip, 16)
     sec1(10) = 0.
  endif

  if (sec1(24).ge.32768) sec1(24) = 32768-sec1(24)

  ! If the TIME/RANGE INDICATOR (sec1(19)) indicates that the time P1
  ! is spread over two bytes, then recompute sec1(17) and set sec1(18)
  ! to zero.
  if (sec1(19) == 10) then
     call gbyte(grec, sec1(17), i17skip, 16)
     sec1(18) = 0
  endif

  ! Pull out single bits from sec1(6) for the GDS and BMS flags:
  sec1(25) = sec1(6)/128
  sec1(26) = mod(sec1(6)/64,2)

! Section 2:
  if (sec1(25) == 1) then

     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8
     endif
     call gbyte(grec, sec2(1), iskip, 24)
     iskip = iskip + 24
     call gbytes(grec, sec2(2), iskip, 8, 0, 3)
     iskip = iskip + 8*3

     if (sec2(4) == 0) then
        ! Lat/Lon Grid:
        call gbytes(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbyte(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbytes(grec, infogrid(8), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbyte(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbyte(grec, infogrid(11), iskip, 32)
        iskip = iskip + 32

        infogrid(8) = infogrid(8) * infogrid(21)
        infogrid(9) = infogrid(9) * infogrid(22)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        gridinfo(3) = float(infogrid(3))*0.001
        gridinfo(4) = infogrid(4) * 0.001
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        if (infogrid(7).ge.8388608) infogrid(7) = 8388608 - infogrid(7)
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(9) = infogrid(9) * 0.001
        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

        recognized_grid = .TRUE.
     elseif (sec2(4) == 1) then ! Mercator grid
        ! Number of points in X and Y
        call gbytes(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        ! Starting lat and lon
        call gbytes(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        ! "Resolution and component flags"
        call gbyte(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        ! Ending lat and lon
        call gbytes(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        ! Truelat, 3 bytes
        call gbyte(grec, infogrid(11), iskip, 24)
        iskip = iskip + 24
        ! "Reserved", i.e., skip a byte
        iskip = iskip + 8 
        ! Scanning mode flags, first three bits of the next byte
        ! and skip the last five bits.
        call gbyte(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        ! Grid increment in X and Y
        call gbytes(grec, infogrid(8), iskip, 24, 0, 2)        
        iskip = iskip + 48
        ! Done reading map specifications.
        ! Now do various conversions:

        gridinfo(1) = float(infogrid(1)) ! ok
        gridinfo(2) = float(infogrid(2)) ! ok

        if (infogrid(3) .ge.8388608) infogrid(3)  = 8388608 - infogrid(3)
        if (infogrid(4) .ge.8388608) infogrid(4)  = 8388608 - infogrid(4)
        if (infogrid(6) .ge.8388608) infogrid(6)  = 8388608 - infogrid(6)
        if (infogrid(7) .ge.8388608) infogrid(7)  = 8388608 - infogrid(7)
        if (infogrid(11).ge.8388608) infogrid(11) = 8388608 - infogrid(11)
        gridinfo(3)  = infogrid(3)  * 0.001
        gridinfo(4)  = infogrid(4)  * 0.001
        gridinfo(6)  = infogrid(6)  * 0.001
        gridinfo(7)  = infogrid(7)  * 0.001
        gridinfo(8)  = infogrid(8)  * 0.001
        gridinfo(9)  = infogrid(9)  * 0.001
        gridinfo(11) = infogrid(11) * 0.001

        gridinfo(21) = infogrid(21)
        gridinfo(22) = infogrid(22)

        gridinfo(20) = 6370.949
        grrth = gridinfo(20)
        gtrue1 = gridinfo(11)

     elseif (sec2(4) == 3) then
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
! Lambert Conformal:
        call gbytes(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        call gbyte(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes(grec, infogrid(6), iskip, 24, 0, 3)
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        iskip = iskip + 72
        call gbyte(grec, infogrid(9), iskip, 8)
        iskip = iskip + 8
        call gbyte(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbytes(grec, infogrid(11), iskip, 24, 0, 4)
        if (infogrid(11).ge.8388608) infogrid(11) = 8388608 - infogrid(11)
        if (infogrid(12).ge.8388608) infogrid(12) = 8388608 - infogrid(12)
        if (infogrid(13).ge.8388608) infogrid(13) = 8388608 - infogrid(13)
        if (infogrid(14).ge.8388608) infogrid(14) = 8388608 - infogrid(14)
        iskip = iskip + 96
        call gbyte(grec, infogrid(15), iskip, 16)
        iskip = iskip + 16

        infogrid(7) = infogrid(7) * infogrid(21)
        infogrid(8) = infogrid(8) * infogrid(22)


        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        gridinfo(3) = infogrid(3) * 0.001
        gridinfo(4) = infogrid(4) * 0.001
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(9) = infogrid(9) * 0.001
        gridinfo(11) = infogrid(11) * 0.001
        gridinfo(12) = infogrid(12) * 0.001
        gridinfo(13) = infogrid(13) * 0.001
        gridinfo(14) = infogrid(14) * 0.001

        gridinfo(20) = 6370.949
        ! a priori knowledge:
        if (sec1(5).eq.212) then
           gridinfo(3) = 12.190
           gridinfo(4) = -133.459
           gridinfo(7) = 40.63525
           gridinfo(8) = 40.63525
           gridinfo(20) = 6371.1
        endif

!=============================================================================!
! More a priori knowledge:                                                    !
! Correct some bad lat/lon numbers coded into some RUC headers.               !
!                                                                             !
        if (sec1(3) == 59) then  ! If FSL
           if (sec1(4) == 86) then  ! and RUC
              if (sec1(5) == 255) then 
           ! Check to correct bad lat/lon numbers.
                 if (infogrid(3) == 16909) then
                    infogrid(3) = 16281
                    gridinfo(3) = 16.281
                 endif
                 if (infogrid(4) == 236809) then
                    infogrid(4) = 2338622
                    gridinfo(4) = 233.8622
                 endif
              endif
           endif
        endif
!=============================================================================!

        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

        ! Map parameters
        glat1 = gridinfo(3)
        glon1 = gridinfo(4)
        if (gridinfo(6).gt.180.) gridinfo(6) = -(360.-gridinfo(6))
        if ((gridinfo(6)<0).and.(glon1>180)) glon1 = glon1-360.  
        gtrue1 = gridinfo(11)
        gtrue2 = gridinfo(12)
        grrth = gridinfo(20)
        call glccone(gtrue1, gtrue2, 1, gkappa)
        r = grrth/(gridinfo(7)*gkappa)*sind(90.-gtrue1) * &
             (tand(45.-glat1/2.)/tand(45.-gtrue1/2.)) ** gkappa
        gx1 =  r*sind(gkappa*(glon1-gridinfo(6)))
        gy1 = -r*cosd(gkappa*(glon1-gridinfo(6)))

        recognized_grid = .TRUE.
     elseif (sec2(4) == 4) then
        ! Gaussian Grid:
        call gbytes(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbyte(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbytes(grec, infogrid(8), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbyte(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbyte(grec, infogrid(11), iskip, 32)
        iskip = iskip + 32

        infogrid(8) = infogrid(8) * infogrid(21)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        gridinfo(3) = float(infogrid(3))*0.001
        gridinfo(4) = infogrid(4) * 0.001
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        if (infogrid(7).ge.8388608) infogrid(7) = 8388608 - infogrid(7)
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

        ! Compute an approximate delta-latitude and starting latitude.
        ! Replace the stored value of starting latitude with approximate one.
        gridinfo(18) = gridinfo(3)
        infogrid(18) = infogrid(3)
        gridinfo(17) = gridinfo(6)
        infogrid(17) = infogrid(6)
        call griblgg(infogrid(2), gridinfo(3), gridinfo(19))
        infogrid(19) = nint(gridinfo(19)*1000.)
        infogrid(3) = nint(gridinfo(3)*1000.)
        gridinfo(6) = -gridinfo(3)
        infogrid(6) = -infogrid(3)
        recognized_grid = .TRUE.

     elseif (sec2(4) == 5) then
! Polar Stereographic Grid
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
        call gbytes(grec, infogrid(1), iskip, 16, 0, 2)  ! NX and NY
        iskip = iskip + 32
        call gbytes(grec, infogrid(3), iskip, 24, 0, 2)  ! LAT1 and LON1
        iskip = iskip + 48
        call gbyte(grec, infogrid(5), iskip, 8) ! Resolution and Component
        iskip = iskip + 8
        call gbytes(grec, infogrid(6), iskip, 24, 0, 3) ! LOV, DX, and DY
        iskip = iskip + 72
        call gbyte(grec, infogrid(9), iskip, 8) ! Projection center flag
        iskip = iskip + 8
        call gbyte(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
!         call gbyte(grec, infogrid(11), iskip, 32) ! Set to 0 (reserved)
        iskip = iskip + 32

        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)


        infogrid(7) = infogrid(7) * infogrid(21)
        infogrid(8) = infogrid(8) * infogrid(22)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        gridinfo(3) = infogrid(3) * 0.001
        gridinfo(4) = infogrid(4) * 0.001
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001

        gridinfo(20) = 6370.949

        ! a priori knowledge:
        if (sec1(5).eq.240) then
           gridinfo(3) = 22.7736
           gridinfo(4) = -120.376
           gridinfo(7) = 4.7625
           gridinfo(8) = 4.7625
           gridinfo(20) = 6371.2
        endif

        ! Map parameters
        glat1 = gridinfo(3)
        glon1 = gridinfo(4)
        if (gridinfo(6).gt.180.) gridinfo(6) = -(360.-gridinfo(6))
        gtrue1 = 60.
        grrth = gridinfo(20)
        r = grrth/gridinfo(7) * tand((90.-glat1)/2.) * (1.+sind(gtrue1))
        gx1 = r * sind(glon1-gridinfo(6))
        gy1 = - r * cosd(glon1-gridinfo(6))

        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))
        recognized_grid = .TRUE.

     elseif (sec2(4) == 50) then
! Spherical harmonic coefficients
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
        call gbytes(grec, infogrid(1), iskip, 16, 0, 3)
        iskip = iskip + 48
        call gbytes(grec, infogrid(4), iskip, 8, 0, 2)
        iskip = iskip + 16

        iskip = iskip + 18*8
        recognized_grid = .TRUE.

     else
        call gribprint(0)
        call gribprint(1)
        call gribprint(2)
        call gribprint(3)
        call gribprint(4)
        write(*,'("Unrecognized grid: ", i8)') sec2(4)
        write(*,'("This grid is not currently supported.")')
        write(*,'("Write your own program to put the data to the intermediate format")')
        stop
     endif

     ! A priori knowledge of specific grids:
  else if (sec1(5) == 61) then
     gridinfo(1)= 91.
     gridinfo(2)= 46.
     infogrid(1)= 91
     infogrid(2)= 46
     gridinfo(3)= 0
     gridinfo(4)= 0
     infogrid(5)= 0
     gridinfo(6)= 90.000
     gridinfo(7)= 180.000
     gridinfo(8)= 2.000
     gridinfo(9)= 2.000
     infogrid(10)= 0
     infogrid(21)= 1
     gridinfo(21)= 1.
     infogrid(22)= 1
     gridinfo(22)= 1.
     recognized_grid = .TRUE.
  else if (sec1(5) == 62) then
     gridinfo(1)= 91.
     gridinfo(2)= 46.
     infogrid(1)= 91
     infogrid(2)= 46
     gridinfo(3)= 0.
     gridinfo(4)= -180.000
     infogrid(5)= 0
     gridinfo(6)= 90.000
     gridinfo(7)= 0
     gridinfo(8)= 2.000
     gridinfo(9)= 2.000
     infogrid(10)= 0
     infogrid(21)= 1
     gridinfo(21)= 1.
     infogrid(22)= 1
     gridinfo(22)= 1.
     recognized_grid = .TRUE.
  else if (sec1(5) == 63) then
     gridinfo(1)= 91.
     gridinfo(2)= 46.
     infogrid(1)= 91
     infogrid(2)= 46
     gridinfo(3)= -90.000
     gridinfo(4)= 0.
     infogrid(5)= 0
     gridinfo(6)= 0.
     gridinfo(7)= 180.000
     gridinfo(8)= 2.000
     gridinfo(9)= 2.000
     infogrid(10)= 0
     infogrid(21)= 1
     gridinfo(21)= 1.
     infogrid(22)= 1
     gridinfo(22)= 1.
     recognized_grid = .TRUE.
  else if (sec1(5) == 64) then
     gridinfo(1)= 91.
     gridinfo(2)= 46.
     infogrid(1)= 91
     infogrid(2)= 46
     gridinfo(3)= -90.000
     gridinfo(4)= -180.000
     infogrid(5)= 0
     gridinfo(6)= 0.
     gridinfo(7)= 0.
     gridinfo(8)= 2.000
     gridinfo(9)= 2.000
     infogrid(10)= 0
     infogrid(21)= 1
     gridinfo(21)= 1.
     infogrid(22)= 1
     gridinfo(22)= 1.
     recognized_grid = .TRUE.
  else if (sec1(5) == 105) then
     sec2(4) = 5                 ! Polar Stereographic grid
     infogrid(1) = 83            ! 83 points in X
     infogrid(2) = 83            ! 83 points in Y
     gridinfo(1) = 83.           ! 83 points in X
     gridinfo(2) = 83.           ! 83 points in Y
     gridinfo(3) = 17.52646      ! Starting Latitude
     gridinfo(4) = -129.2958     ! Starting Longitude
     infogrid(5) = 0             ! Resolution and Component Flags
     gridinfo(6) = -105.         ! Central Longitude
     gridinfo(7) = 90.75464      ! Grid distance in X (km)
     gridinfo(8) = 90.75464      ! Grid distance in Y (km)
     infogrid(9) = 0             ! Projection Center flag (North)
     infogrid(10) = 0            ! Scanning Mode flag (Fortran I,J)
     infogrid(21) = 1            ! Grid increment sign factor
     gridinfo(21) = 1.           ! Grid increment sign factor
     infogrid(22) = 1            ! Grid increment sign factor
     gridinfo(22) = 1.           ! Grid increment sign factor
     gtrue1 = 60.                ! True Latitude = 60.
     grrth = 6370.949            ! Radius of the earth
     glat1 = gridinfo(3)
     glon1 = gridinfo(4)
     r = grrth/gridinfo(7) * tand((90.-glat1)/2.) * (1.+sind(gtrue1))
     gx1 = r * sind(glon1-gridinfo(6))
     gy1 = - r * cosd(glon1-gridinfo(6))
     recognized_grid = .TRUE.
  else
     write(*,'("WARNING: Unrecognized grid: ", I8)') sec1(5)
     recognized_grid = .FALSE.
  endif

! Section 3
  if (sec1(26).eq.1) then
     if (ied.eq.0) then
        print '(//,"*** Despair ***"//)'
        stop
     endif

     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8 + sec2(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8 + sec2(1)*8
     endif
     call gbyte(grec, sec3(1), iskip, 24)
     iskip = iskip + 24
     call gbyte(grec, sec3(2), iskip, 8)
     iskip = iskip + 8
     call gbyte(grec, sec3(3), iskip, 16)
     iskip = iskip + 16

! Size of the bitmap is 6 bytes less than the size of the whole
! of section 3.  Grids 61,62,63,64 are a special case:
     if (sec1(5) >= 61 .and. sec1(5) <= 64) then
        bmsize = 4186
     else
        bmsize = (sec3(1)-6)*8
     endif

#if defined (CRAY)
#else
     allocate(bitmap(bmsize))
#endif
     allocate(bm8(bmsize))
     call gbytes(grec, bm8, iskip, 1, 0, (sec3(1)-6)*8)
     bitmap(1:(sec3(1)-6)*8) = bm8(1:(sec3(1)-6)*8)
     deallocate(bm8)
     iskip = iskip + sec3(1)-6

! Grids 61,62,63,64 are a special case:
     if (sec1(5) == 61 .or. sec1(5) == 62) then
        bitmap(4186) = bitmap(4096) ! The last point represents the pole.
        bitmap(4096:4185) = 0       ! Fill the rest of the pole values with 0
     elseif (sec1(5) == 63 .or. sec1(5) == 64) then
        bitmap(92:4186) = bitmap(2:4096)
        bitmap(2:91) = 0
     endif

  else
     sec3 = 0
  endif

! Section 4
  if ((sec1(25).eq.1) .or. (recognized_grid)) then
     ! Do this only if we recognize the grid, either from the GDS or from
     ! a priori knowledge.
     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8
     endif
     call gbyte(grec, sec4(1), iskip, 24)
     if (sec4(1) > (sec0(1) - sec1(1) - sec2(1) - sec3(1) - 4)) then
        write(*,'(/,"*** I have good reason to believe that this GRIB record is")')
        write(*,'("*** corrupted or miscoded.",/)')
        ierr = 1
        return
     endif
     iskip = iskip + 24
     call gbytes(grec, sec4(2), iskip, 1,0,4)
     iskip = iskip + 4
     call gbyte(grec, sec4(6), iskip, 4)
     iskip = iskip + 4
! Get the binary scale factor
     call gbyte(grec, isign, iskip, 1)
     iskip = iskip + 1
     call gbyte(grec, sec4(7), iskip, 15)
     iskip = iskip + 15
     sec4(7) = sec4(7) * (-2*isign+1)
! Get the reference value:
     call gbyte(grec, isign, iskip, 1)
     iskip = iskip + 1
     isign = -2*isign+1
     call gbyte(grec, ichar, iskip, 7)
     iskip = iskip + 7
     call gbyte(grec, iman, iskip, 24)
     iskip = iskip + 24
     xec4(1) = float(isign) * 2.**(-24) * float(iman) *  &
          float(16**(ichar-64))

     call gbyte(grec,sec4(8), iskip, 8)
! sec4(8) is the number of bits used per datum value.
! If sec4(8) = 255, assume they mean sec4(8) = 0
     if (sec4(8) == 255) sec4(8) = 0
     iskip = iskip + 8
  endif

! Section 5
  call gbyte(grec, isvns, ((sec0(1)-4)*8), 32)
  if (isvns.ne.gsvns) then
     write(*, '("End-of-record mark (7777) not found", 2I10)') isvns
     write(*, '("Sec0(1) = ", I8, i2)') sec0(1), sevencount
     sevencount = sevencount + 1
     if (sevencount > 10) then
        write(*,'(//," *** Found more than 10 consecutive bad GRIB records")')
        write(*,'(" *** Let''s just stop now.",//)')
        write(*,'(" Perhaps the analysis file should have been converted",/,&
             &" from COS-Blocked format?",//)')
        stop
     endif
  else
     sevencount = 0
  endif

  ierr = 0

end subroutine gribheader
