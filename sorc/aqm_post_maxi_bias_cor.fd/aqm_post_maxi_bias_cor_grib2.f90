!------------------------------------------------------------------------------
!  aqm_post_maxi_bias_cor_grib2 
!  Author:   Jianping Huang 03/19/2015
!            based on read__gridded_aqm.f90
!  Purposes: 1) convert Bias Correction files from netcdf format to grib2
!            2) calculate daily max and daily averaged PM2.5 
!
!------------------------------------------------------------------------------
program aqm_post_maxi_bias_cor_grib2 

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   use index_to_date_mod        
   use date__index
   use next__time

   implicit none

   						!   fail (stdlit)
! Local variables.

   character outfile1*200,outfile2*200
   integer nhours
   integer dims_in4(4), dims_in3(3)
!   logical fail1, fail2

! added by JP  
   character  infile1*200,infile2*200,infile3*200
   character  varname*10,ymd*8,ch_cyc*2,ch_chk*2
   integer    diag, imax,jmax
!   integer    icyc,iyear,imonth,iday,idate,ihour,base_year,nt,ichk
   integer    icyc,iyear,imonth,iday,ihour,base_year,ichk
!   integer    nowdate,nowdate1,nowtime
   integer    nowdate,nowtime
   integer    ierr,mday,ier
   integer    i, j 
! for grib2 by JP
   integer, parameter   :: max_bytes=20000000
   integer, parameter   :: nx=442,ny=265
!
   integer listsec0(2)
   integer listsec1(13)
   integer igds(5)
   integer igdstmpllen
   integer ipdstmpllen
   integer idrstmpllen
   integer idrsnum,ibmap,numcoord,ipdsnum,idefnum
   integer defnum

    integer,dimension(100) :: igdstmpl
    integer,dimension(100) :: ipdstmpl
    integer,dimension(100) :: idrstmpl
!
    integer ideflist(1)
    real(4) coordlist(1)
!
    character*1 cgrib1(max_bytes)
    character*1 cgrib2(max_bytes)
!
    logical*1 bmap(nx,ny)
!
    real(4),dimension(nx*ny) :: fld1
    real(4),dimension(nx*ny) :: fld2
    integer ifilw1,ifilw2,lengrib,lonstt,lonlst,latstt,latlst
!    integer yy,mm,dd,hh,mn,sc
    real(4) :: dxval

    character*50 gdss(400)
    integer GRID, kgdss(200), lengds,im,jm,jf
!-------------------------------------------------------------------

   integer status
   
   integer id_gribdomain

!   integer kpds(200),kgds(200)
!   integer kpds(25),kgds(25)
!   integer kpds(200),kgds(200),istime(1),ietime(1)  

   character(*), parameter :: calendar  = 'gregorian'

    logical  ave1hr

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

!   real(dp), allocatable :: vdata(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata1(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata2(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata3(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
!   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real, allocatable :: pm25data(:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: pm25(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: pm25_ave(:,:,:),pm25_max(:,:,:)
   real(dp), allocatable :: work(:,:)
   logical, allocatable :: LB1(:)

!   real tmp_t1, tmp_t2,tmpmax

! Get command arguments as strings.
   diag=5
   id_gribdomain=148
!   outfile1='aqm.t99z.pm25_24hr_ave.148.bc.grib2'
!   outfile2='aqm.t99z.pm25_24hr_max.148.bc.grib2'
   outfile1='aqm.t99z.24hpm25-ave.148.bc.grib2'
   outfile2='aqm.t99z.1hpm25-max.148.bc.grib2'

   ave1hr=.true.
   print*,"it is testing"
   call get_command_argument (1, varname) 
   call get_command_argument (2, ymd)
   call get_command_argument (3, ch_cyc)
   call get_command_argument (4, ch_chk)
   
!
   read(ymd(1:4),*)iyear
   read(ymd(5:6),*)imonth
   read(ymd(7:8),*)iday
   read(ch_cyc(1:2),*)icyc
   read(ch_chk(1:2),*)ichk
   ihour=icyc

   if ( icyc .le. 9 ) then
    write(outfile1(6:6),'(a1)')"0"
    write(outfile2(6:6),'(a1)')"0"
    write(outfile1(7:7),'(i1)')icyc
    write(outfile2(7:7),'(i1)')icyc
   else
    write(outfile1(6:7),'(i2)')icyc
    write(outfile2(6:7),'(i2)')icyc
   endif

   print*,"iyear=",iyear,"imonth=",imonth,"iday=",iday,"icyc=",icyc
   print*,"outfile1=",outfile1
   print*,"outfile2=",outfile2

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'aqm.post1_bias_correct: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

!   allocate(indata1(imax,jmax,1,48)) 
!   allocate(indata2(imax,jmax,1,48)) 
   infile1="a.nc"
   infile2="b.nc"
   infile3="c.nc"
 
   call read_netcdf_var (infile1, varname, diag, indata1, status)

   if ( icyc .eq. 6 ) then
    call read_netcdf_var (infile2, varname, diag, indata2, status)
   endif
   if ( icyc .eq. 12 ) then
    call read_netcdf_var (infile2, varname, diag, indata2, status)
    call read_netcdf_var (infile3, varname, diag, indata3, status)
   endif
    

! Read errors are soft errors.
! On read error, return with fail status from lower level.
   print*,"status=",status

!   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata1)			! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )		! omit vestigial LAY dimension

   imax       = dims_in4(1)
   jmax       = dims_in4(2)

! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours = size (indata1, 4)
   
   allocate (pm25data(imax,jmax))
   allocate (work(imax,jmax))
   allocate (pm25(imax,jmax,nhours))
   allocate (pm25_ave(imax,jmax,2))
   allocate (pm25_max(imax,jmax,2))
   allocate (LB1(imax*jmax))

!
!  combine previoud day 12z and today 06 or 12z to create a complete data for day1 and
!  day 2 (from current day 05z to 04z of the next day)
    if  ( icyc .eq. 6 ) then
      do i = 1, imax
        do j = 1, jmax
         if ( ichk .eq. 1 ) then
          pm25(i,j,1:2)  = indata2(i,j,1,5:6)  ! using today 00z file
         else
          pm25(i,j,1:2)  = indata2(i,j,1,17:18) ! using previous day 12z file
         endif
         pm25(i,j,3:nhours) = indata1(i,j,1,1:46)
       enddo  
      enddo 
    elseif ( icyc .eq. 12 ) then
     do i = 1, imax
      do j = 1, jmax
      if ( ichk .eq. 1 ) then
        pm25(i,j,1:2)  = indata2(i,j,1,5:6)      ! from 05z and 06z at 00z run
      else
        pm25(i,j,1:2)  = indata2(i,j,1,17:18)   ! using previous day 12z file
      endif
      pm25(i,j,3:8)  = indata3(i,j,1,1:6)      ! from 07-12z at 06z run
      pm25(i,j,9:nhours) = indata1(i,j,1,1:40) ! from 12z run
      enddo
     enddo
    endif
!
!  calculate daily max and 24_hr ave for day 1 and day 2
!-- ------------------------------------------------------
!-- set file unit

      ifilw1=51
      ifilw2=52
      call baopen(ifilw1,trim(outfile1),ierr)
      call baopen(ifilw2,trim(outfile2),ierr)
      if(ierr.ne.0) then
       print*,'can not open ',trim(outfile2)
       stop 2001
      endif

   do mday = 1, nhours/24 
       do i = 1, imax
        do j = 1, jmax
         pm25_ave(i,j,mday) = sum(pm25(i,j,(mday-1)*24+1:mday*24))/24. 
         pm25_max(i,j,mday) = maxval(pm25(i,j,(mday-1)*24+1:mday*24))
        enddo
       enddo

! Diagnostics, if requested.

    if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (pm25), ', ', &
         maxval (pm25)
    end if

    if (diag >= 3) print *, 'read_gridded_aqm: Return.'

!-----------------------------------------------------------------------
     base_year=iyear
     listsec1(6)=iyear    ! Reference time - Year (4 digits)
     listsec1(7)=imonth      ! Reference time - Month
     listsec1(8)=iday      ! Reference time - Day
     listsec1(9)=icyc      ! Reference time - Hour

     nowtime=(ihour+1)*10000
!      do nt=1,nhours

     GRID=148
    if(GRID.eq.148) then   !For HRRR grid
       im=442
       jm=265
         jf=im*jm
    else
       call makgds(GRID, kgdss, gdss, lengds, ier)
       im=kgdss(2)
       jm=kgdss(3)
       jf=kgdss(2)*kgdss(3)
    end if
!
    cgrib1=''
    cgrib2=''

!-----Grib2 file header information
!-- section 0:
     listsec0(1)=0       ! Discipline: table 0.0
     listsec0(2)=2       ! grib edition number
!
!-- section 1:
     listsec1(1)=7       ! Identification of orginating center (Table0)(7:ncep)
     listsec1(2)=0       ! Identification of orginating subcenter(ON388-TableC) (4:emc)
     listsec1(3)=2       ! GRIB master tables version number (Table 1.0)(11:May 2013 version)
     listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
     listsec1(5)=1       ! Significance of reference time (Table 1.2) (0:ana 1:fcst 2:vrfy)
! following need to be changed !
!    listsec1(6)=iyear    ! Reference time - Year (4 digits)
!    listsec1(7)=imonth      ! Reference time - Month
!    listsec1(8)=iday      ! Reference time - Day
!    listsec1(9)=icyc      ! Reference time - Hour
!    listsec1(9)=INT(nowtime/10000)      ! Reference time - Hour
     listsec1(10)=00     ! Reference time - Minute
     listsec1(11)=00     ! Reference time - Second
     listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
     listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

     call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)
     call gribcreate(cgrib2,max_bytes,listsec0,listsec1,ierr)
     print*,'gribcreate status=',ierr
!
!-- section 3: grid definition section
     igds(1)=0           ! Source of grid definition (Table 3.0) (0:specified in the code)
     igds(2)=nx*ny       ! Number of grid points in the defined grid
     igds(3)=0           ! Number of octets for optional list of numbers defining number of points
     igds(4)=0           ! Interpretation of list of numbers defining number of points
!-- example: Gaussian Lat/lon
     igds(5)=30          ! Grid definition template number (Table 3.1) (0:Lat/lon, 30:Lambert 40:Gaussian)
   if( igds(5)==30) then
     igdstmpllen=22
     igdstmpl=0
!
!-- set up grid definition template 3.30
     igdstmpl=0
     igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
     igdstmpl(8)=nx      ! Ni . number of points along a paralell
     igdstmpl(9)=ny      ! Nj . number of points along a meridian
     igdstmpl(10)=21821000      ! Basic angle of the initial production domain
     igdstmpl(11)=239372000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
     latstt=56
     lonstt=33000000
     latlst=-88541961
     lonlst=358125000
     dxval=1875000
     igdstmpl(12)=latstt ! La1 - latitude of first grid point
     igdstmpl(13)=lonstt ! Lo1 - longitude of first grid point
     igdstmpl(14)=263000000
     igdstmpl(15)=12000000
     igdstmpl(16)=12000000
     igdstmpl(17)=0
     igdstmpl(18)=64
     igdstmpl(19)=33000000
     igdstmpl(20)=45000000
     igdstmpl(21)=0
     igdstmpl(22)=0
!    igdstmpl(14)=48     ! Resolution and component flags (Table 3.3)
!    igdstmpl(15)=latlst ! La2 - latitude of last grid point
!    igdstmpl(16)=lonlst ! Lo2 - longitude of last grid point
!    igdstmpl(17)=dxval  ! Di - i direction increment
!    igdstmpl(18)=ny/2   ! N - number of paralells between a pole and the
!    equator
!    igdstmpl(19)=0      ! Scanning mode (Table 3.4) (0:Points in the first
!    row or column scan in the +i (+x) direction)
    endif
     defnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
     ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
     call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
     call addgrid(cgrib2,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
      print*,'addgrid status=',ierr,"cgrib1=",trim(cgrib1(1))
!
!-- section 4: product definition section
     ipdstmpl=0
     ipdsnum=8             ! Product Definition Template Number (Table 4.0) (0:Analysis or forecast at a horizontal level or in a horizontal layer at a point in time)
     ipdstmpllen=29        ! pdt template length
     ipdstmpl(1)=13        ! catogory
     ipdstmpl(2)=193       ! parameter
     ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana, 1:ic, 2:fcst)
     ipdstmpl(4)=0         ! Background generating process identifier
     ipdstmpl(5)=211        ! Analysis or forecast generating process identified (ON388TableA)
     ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
     ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
     ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!     ipdstmpl(9)=nt-1         ! Forecast time in units defined by ipdstmpl(8)
!     ipdstmpl(9)=-7         ! Forecast time in units defined by ipdstmpl(8)
     ipdstmpl(9)=(5-icyc)+(mday-1)*24         ! Forecast time in units defined by ipdstmpl(8)
     ipdstmpl(10)=104      ! Type of first fixed surface (see Code table 4.5)(100:isobaric leve)
     ipdstmpl(11)=4        ! Scale factor of first fixed surface
     ipdstmpl(12)=10000    ! Scaled value of first fixed surface
     ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
     ipdstmpl(14)=0        ! Scale factor of second fixed surface
     ipdstmpl(15)=0        ! Scaled value of second fixed surface
     ipdstmpl(16)=iyear    !  Year
     ipdstmpl(17)=imonth   !  Month
     ipdstmpl(18)=iday+mday     !  Date
!     ipdstmpl(19)=INT(nowtime/10000)        !  Forecast hour
!     ipdstmpl(19)=mday*23 + ipdstmpl(9)        !  Forecast hour
     ipdstmpl(19)=4        !  Forecast hour
     ipdstmpl(20)=0        !
     ipdstmpl(21)=0        !
     ipdstmpl(22)=1        !
     ipdstmpl(23)=0        !
     ipdstmpl(24)=0        !
     ipdstmpl(25)=2        !
     ipdstmpl(26)=1        !
!     ipdstmpl(27)=1        !
     ipdstmpl(27)=23        !
     ipdstmpl(28)=255      !
     ipdstmpl(29)=0        !

!
     numcoord=0            ! Number of coordinate values after template
     coordlist=0.          ! Optional list of coordinate values
!
!-- section 5: Data Representation Section
     idrstmpl=0
     idrsnum=3             ! Data representation section template number (Table5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
     idrstmpllen=18        ! Length of Data representation section
     idrstmpl(1)=1212562560  ! Binary scale factor
     idrstmpl(2)=0         ! Binary scale factor
     idrstmpl(3)=6         ! Decimal scale factor
     idrstmpl(4)=27        ! Decimal scale factor
     idrstmpl(5)=0         !
     idrstmpl(6)=1         !
     idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
     idrstmpl(8)=0         ! Primary missing value substitute
     idrstmpl(9)=0         ! Secondary missing value substitute
     idrstmpl(10)=8782     !
     idrstmpl(11)=0        !
     idrstmpl(12)=5        !
     idrstmpl(13)=1        !
     idrstmpl(14)=1        !
     idrstmpl(15)=11       !
     idrstmpl(16)=6        !
     idrstmpl(17)=2
     idrstmpl(18)=4
!
!-- section 6:
     ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
     pm25data(:,:)=pm25_ave(:,:,mday)

     do j=1,ny
      do i=1,nx
        fld1(i+(j-1)*nx)=pm25_ave(i,j,mday)
        fld2(i+(j-1)*nx)=pm25_max(i,j,mday)
      enddo
     enddo

     call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)

!     idrstmpl(3)=5         ! Decimal scale factor
     idrstmpl(4)=24        ! Decimal scale facto
     idrstmpl(10)=8908        ! Decimal scale facto
     idrstmpl(15)=12        ! Decimal scale facto
     idrstmpl(16)=6        ! Decimal scale facto



     ipdstmpl(1)=14       ! catogory
     ipdstmpl(2)=202       ! daily maxi 1hr pm2.5 parameter

     call addfield(cgrib2,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld2,nx*ny,ibmap,bmap,ierr)

     print*,'addfield status=',ierr

     nowdate=date_index(iyear, imonth, iday, base_year, calendar)

     print*,"hjp111a,nowdate:",nowdate,"nowtime=",nowtime
     call next_time(nowdate,nowtime,10000)
     print*,"hjp111b,nowdate:",nowdate,"nowtime=",nowtime
     call index_to_date(nowdate,iyear, imonth, iday, base_year, calendar)
     print*,"hjp111c,iyear,imn,iday=",iyear, imonth, iday

     call gribend(cgrib1,max_bytes,lengrib,ierr)
     call gribend(cgrib2,max_bytes,lengrib,ierr)

     print*,'gribend status=',ierr
     print*,'length of the final GRIB2 message in octets =',lengrib
!
      call wryte(ifilw1, lengrib, cgrib1)
      call wryte(ifilw2, lengrib, cgrib2)

    end do
         
end program aqm_post_maxi_bias_cor_grib2 


                  


     



























