       program cmaq_maxi2grib2 
!------convert CMAQ surface output to grib format and calculate daily maximum of 1-hr ozone
!            8 hr ozone,  1-hr PM2.5 and 24-hr PM2.5 starting from 04Z (to comparable with AIRNOW data)
!

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      real, allocatable :: work(:,:), o3(:,:,:), pm25(:,:,:), o3_8hr(:,:,:), &
        pm25_24hr(:,:,:)
      
      logical pmon
      logical, allocatable :: lb(:,:)   ! logical mask
      
!      integer, parameter :: naerospec=16, nname=4
      integer, parameter :: naerospec=1, nname=4
      character*16 aerospec(naerospec),varlist(nname),maxname(nname),currentfile
      character*200 outfile

! for grib2 by JP
!   integer, parameter   :: max_bytes=20000000
!jp       integer, parameter   :: nx=442,ny=265
!jp       integer, parameter   :: max_bytes=nx*ny*4

       integer listsec0(2)
       integer listsec1(13)
       integer igds(5)
       integer igdstmpllen
       integer ipdstmpllen
       integer idrstmpllen
       integer idrsnum,ibmap,numcoord,ipdsnum,idefnum
       integer defnum ,nowdate8,icyc,itimestep

       integer,dimension(100) :: igdstmpl
       integer,dimension(100) :: ipdstmpl
       integer,dimension(100) :: idrstmpl
!
       integer ideflist(1)
       real(4) coordlist(1)
!
!       character*1 cgrib1(max_bytes)
!       character*1 cgrib2(max_bytes)
        character*1,allocatable  ::cgrib1(:),cgrib2(:)
!
!jp       logical*1 bmap(nx,ny)
       logical*1 , allocatable ::bmap(:,:)
!
!jp       real(4),dimension(nx*ny) :: fld1
!jp       real(4),dimension(nx*ny) :: fld2
       real(4),allocatable :: fld1(:),fld2(:)
       integer ifilw1,ifilw2,lengrib,lonstt,lonlst,latstt,latlst
       integer yy,mm,dd,hh,mn,sc
       real(4) :: dxval

       character*50 gdss(400)
       integer GRID, kgdss(200), lengds,im,jm,jf
!-------------------------------------------------------------------
      integer kpds(200),kgds(200)
      
      data aerospec/'PM25_TOT'/
!      data aerospec/'ASO4I','ASO4J','ANH4I','ANH4J','ANO3I','ANO3J','AORGAI', &
!       'AORGAJ','AORGPAI','AORGPAJ','AORGBI','AORGBJ','AECI','AECJ','A25I','A25J'/
 
      
      data maxname/'o3_1hr','o3_8hr','pm25_1hr','pm25_24hr'/
      
      data varlist/nname*'   '/      
      
      integer nindex(nname),istime(3),ietime(3)    ! file start and ending in YYYYDDDHH
      
      namelist /control/markutc,outfile, varlist,id_gribdomain   !   (table A), 
                                                         ! markutc= 4 UTC, marking start hour for maximum calculation

      open(7,file='cmaq-maxi2grib.ini')      
      read(7,control)
      close(7)
      
      pmon=.false.
      do L=1,nname
       if(varlist(L).ne.'    ') then
         
	 if(index(varlist(L),'pm25').ge.1) pmon=.true.
	 
	 do L2=1,nname
	  if(varlist(L).eq.maxname(L2)) exit	  
	 enddo
	 if(L2.gt.nname) then
	  print*,'wrong varlist ', varlist(L)
	  stop
	 endif
	 
	 nindex(L)=L2
	 
       else
        exit
       endif	 	 
      enddo
      
      nspecies=L-1               ! species number for maximum calculation

      if(nspecies.lt.1) then
       print*,'no species provided'
       stop
      endif
      
!-----first nfile dimension      


      if(.not.OPEN3('CMAQFILE1',FSREAD3,'pathway')) then   ! CMAQFILE1 is the base file
       print*,'open input file error for CMAQFILE1'
       stop
      endif

      if (.not. DESC3('CMAQFILE1') ) then   ! get grid information from CMAQ output
       print*, 'Error getting info from CMAQFILE1' 
       stop
      endif

      if(id_gribdomain.eq.148.and.     &
       (ncols3d.ne.442.or.nrows3d.ne.265)) then
        print*,'5x domain dimension does not match'
	stop
      endif
      
      if(tstep3d.ne.10000) then
       print*,'need hourly input in CMAQFILEs'
       stop
      endif 
    
      imax=ncols3d
      jmax=nrows3d
      max_bytes=imax*jmax*4
      nx=imax
      ny=jmax
      
      nowsdate=sdate3d
      nowstime=stime3d
      maxrec1=mxrec3d

      allocate(cgrib1(max_bytes)) 
      allocate(cgrib2(max_bytes)) 
      allocate(bmap(imax,jmax))
      allocate(fld1(nx*ny))
      allocate(fld2(nx*ny))
      
      istime(1)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
     
      icyc=stime3d/10000

            
      ntmpdate=sdate3d
      ntmptime=stime3d      
      do n=1,mxrec3d-1
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      ietime(1)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
      
      time1_end=ntmptime/10000                 ! end time of  the first file

      allocate(lb(imax,jmax))
      lb(:,:)=.true.
      allocate(work(imax,jmax))
      
!---second file

      if(.not.OPEN3('CMAQFILE2',FSREAD3,'pathway')) stop
      if(.not. DESC3('CMAQFILE2') ) stop

      if(ncols3d.ne.imax.or.nrows3d.ne.jmax) then
       print*,'dimension wrong ',ncols3d,nrows3d,nlays3d
       stop
      endif
      
      if(tstep3d.ne.10000) then
       print*,'need hourly input in CMAQFILEs'
       stop
      endif 

      maxrec2=mxrec3d

      istime(2)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
            
      ntmpdate=sdate3d
      ntmptime=stime3d      
      do n=1,mxrec3d-1
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      ietime(2)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
      
!      if(istime(2).lt.istime(1)) then
!        print*,'reverse CMAQfile1 and CMAQfile2'
!	stop
!      endif	

!---third file

      if(.not.OPEN3('CMAQFILE3',FSREAD3,'pathway')) then
        nfile=2
	mintime=min(istime(1),istime(2))
	maxtime=max(ietime(1),ietime(2))
      else
        nfile=3 	
        if(.not. DESC3('CMAQFILE3') ) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax) then
        print*,'dimension wrong ',ncols3d,nrows3d,nlays3d
        stop
       endif
      
       if(tstep3d.ne.10000) then
        print*,'need hourly input in CMAQFILEs'
        stop
       endif 

      maxrec3=mxrec3d

      istime(3)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
            
      ntmpdate=sdate3d
      ntmptime=stime3d      
      do n=1,mxrec3d-1
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      ietime(3)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH

      mintime=min(istime(1),istime(2),istime(3))
      maxtime=max(ietime(1),ietime(2),ietime(3))

      endif	

      print*,'mintime,maxtime=',mintime,maxtime,nowsdate*100+markutc

!----calculate the total time steps  
      
      if(mintime.gt.(nowsdate*100+markutc)) then  
       nowdate=nowsdate+1
      else
       nowdate=nowsdate 
      endif
      
      msdate=nowdate
      
      metime=min(mod(maxtime,100),markutc)         ! endtime for maximum calculation
      imetime=maxtime/100*100+metime             ! endtime for maximum calculation in yyyydddhh
      
      ntmpdate=nowdate
      ntmptime=markutc*10000

      do ksteps=1,300
       if(ntmpdate*100+ntmptime/10000.ge.imetime) exit
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      if(ksteps.gt.300) then
       print*,'wrong time in CMAQ files ',sdate3d,stime3d,nowsdate,nowstime
       stop
      endif  

!-----read CMAQ files

      allocate(o3(imax,jmax,ksteps+7))
      allocate(pm25(imax,jmax,ksteps+7))
           
      nowtime=markutc*10000
      
      print*,'ksteps=',ksteps,pmon
      
      do nt=1,ksteps+7
        
	nowdatetime=nowdate*100+nowtime/10000
	
        if(nowdatetime.gt.maxtime) exit

	currentfile=''
	if(nowdatetime.ge.istime(1).and.nowdatetime.le.ietime(1)) & 
	 currentfile='CMAQFILE1'
	 
        if(nowdatetime.ge.istime(2).and.nowdatetime.le.ietime(2)) then
	 if(istime(1).le.istime(2).or.currentfile(1:4).ne.'CMAQ') currentfile='CMAQFILE2'    ! use the latest one
	endif
	
	if(nfile.ge.3.and.nowdatetime.ge.istime(3).and.nowdatetime.le.ietime(3)) then
	 if(max(istime(1),istime(2)).le.istime(3).or.currentfile(1:4).ne.'CMAQ') currentfile='CMAQFILE3'    ! use the latest one
	endif
	
	print*,'reading nt=',nt,nowdate,nowtime,trim(currentfile) !  nowdatetime,istime,ietime
	
        if(.not.read3(trim(currentfile),'O3',1,nowdate,nowtime,work)) then
         print*,'read file error ',nt
         stop
        endif      
       
       o3(1:imax,1:jmax,nt)=work(1:imax,1:jmax)*1000 ! ppmv to ppbv
       
       pm25(1:imax,1:jmax,nt)=0.
       if(pmon) then
        do L=1,naerospec
         if(.not.read3(trim(currentfile),aerospec(L),1,nowdate,nowtime,work)) then
 	  print*,'read file error ',aerospec(L)
	  stop
         endif	 
!        pm25(1:imax,1:jmax,nt)=pm25(1:imax,1:jmax,nt)+work(1:imax,1:jmax)
        pm25(1:imax,1:jmax,nt)=work(1:imax,1:jmax)
	enddo
       endif 	
       
       call nextime(nowdate,nowtime,tstep3d)
        	
      enddo
      
      ksteps8=nt-1
      
!------calculate 8-hr O3 mean     

      allocate(o3_8hr(imax,jmax,ksteps))
      
      do nt=1,ksteps8-7
        do i=1,imax
	 do j=1,jmax
	  o3_8hr(i,j,nt)=sum(o3(i,j,nt:nt+7))/8
	 enddo
	enddo
      enddo	    

!------calculate 24-hr PM25 mean     

      if(pmon) then
      allocate(pm25_24hr(imax,jmax,ksteps-23))
      
      do nt=1,ksteps-23
        do i=1,imax
	 do j=1,jmax
	  pm25_24hr(i,j,nt)=sum(pm25(i,j,nt:nt+23))/24
	 enddo
	enddo
      enddo
            
      endif

!-----Grib2 file header information
!-- section 0:
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table0)(7:ncep)
      listsec1(2)=0       ! Identification of orginating subcenter(ON388-TableC) (4:emc)
      listsec1(3)=2       ! GRIB master tables version number (Table 1.0) (11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2)(0:ana 1:fcst 2:vrfy)
! following need to be changed !
!      listsec1(6)=mod(istime(1)/100000,100) ! Initial yy of analysis or forecast
!      listsec1(7)=imonth      ! Reference time - Month
!      listsec1(8)=idate      ! Reference time - Day
!      listsec1(9)=mod(istime(1),100)      ! Reference time - Hour
      jjdate=int(istime(1)/100)
      call daymon(jjdate,imonth1,idate1)
      listsec1(6)=int(istime(1)/100000)  
      listsec1(7)=imonth1
      listsec1(8)=idate1
      listsec1(9)=mod(istime(1),100)-1 
      listsec1(10)=00     ! Reference time - Minute
      listsec1(11)=00     ! Reference time - Second
      listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

!
!-- section 3: grid definition section
      igds(1)=0           ! Source of grid definition (Table 3.0)(0:specified in the code)
      igds(2)=nx*ny       ! Number of grid points in the defined grid
      igds(3)=0           ! Number of octets for optional list of numbers defining number of points
      igds(4)=0           ! Interpretation of list of numbers defining number of points
!-- example: Gaussian Lat/lon
      igds(5)=30          ! Grid definition template number (Table 3.1)(0:Lat/lon, 30:Lambert 40:Gaussian)
      if( igds(5)==30) then
        igdstmpllen=22
        igdstmpl=0

!-- set up grid definition template 3.30
        igdstmpl=0
        igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
        igdstmpl(8)=nx      ! Ni . number of points along a paralell
        igdstmpl(9)=ny      ! Nj . number of points along a meridian
       if ( id_gribdomain .eq. 148 ) then
        igdstmpl(10)=21821000      ! Basic angle of the initial production domain
        igdstmpl(11)=239372000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
        latstt=8
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

      elseif ( id_gribdomain .eq. 140 ) then
        igdstmpl(10)=53020000      ! Basic angle of the initial production domain
        igdstmpl(11)=193523000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!        latstt=8
!        lonstt=33000000
!        latlst=-88541961
!        lonlst=358125000
!        dxval=1875000
        igdstmpl(12)=8
        igdstmpl(13)=57000000
        igdstmpl(14)=211400000
        igdstmpl(15)=12000000
        igdstmpl(16)=12000000
        igdstmpl(17)=0
        igdstmpl(18)=64
        igdstmpl(19)=57000000
        igdstmpl(20)=63000000
        igdstmpl(21)=0
        igdstmpl(22)=0
      elseif ( id_gribdomain .eq. 139 ) then
        igdstmpl(10)=17721000      ! Basic angle of the initial production domain
        igdstmpl(11)=198027000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!        latstt=8
!        lonstt=33000000
!        latlst=-88541961
!        lonlst=358125000
!        dxval=1875000
        igdstmpl(12)=8 ! La1 - latitude of first grid point
        igdstmpl(13)=19000000 ! Lo1 - longitude of first grid point
        igdstmpl(14)=202500000
        igdstmpl(15)=12000000
        igdstmpl(16)=12000000
        igdstmpl(17)=0
        igdstmpl(18)=64
        igdstmpl(19)=19000000
        igdstmpl(20)=21000000
        igdstmpl(21)=0
        igdstmpl(22)=0
      endif

         
      endif
        idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
        ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=8             ! Product Definition Template Number (Table 4.0)(0:Analysis or forecast at a horizontal level or in a horizontal layer at a pointin time)
      ipdstmpllen=29        ! pdt template length
!jp   ipdstmpl(1)=13        ! catogory
!jp   ipdstmpl(2)=193       ! parameter
      ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana,1:ic, 2:fcst)
      ipdstmpl(4)=0         ! Background generating process identifier
      ipdstmpl(5)=211       ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!      ipdstmpl(9)=nt-1    ! Forecast time in units defined by !ipdstmpl(8)
      ipdstmpl(10)=104      ! Type of first fixed surface (see Code table 4.5) (100:isobaric leve)
      ipdstmpl(11)=4        ! Scale factor of first fixed surface
      ipdstmpl(12)=10000    ! Scaled value of first fixed surface
      ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(14)=0        ! Scale factor of second fixed surface
      ipdstmpl(15)=0        ! Scaled value of second fixed surface
!jp      ipdstmpl(16)=iyear        !  Year
!jp      ipdstmpl(17)=imonth        !  Month
!jp      ipdstmpl(18)=iday        !  Date
!jp      ipdstmpl(19)=INT(nowtime/10000)        !  Forecast hour
      ipdstmpl(20)=0        !
      ipdstmpl(21)=0        !
      ipdstmpl(22)=1        !
      ipdstmpl(23)=0        !
      ipdstmpl(24)=0        !
      ipdstmpl(25)=2        !
      ipdstmpl(26)=1        !
      ipdstmpl(27)=1        !
      ipdstmpl(28)=255      !
      ipdstmpl(29)=0        !
!
      numcoord=0            ! Number of coordinate values after template
      coordlist=0.          ! Optional list of coordinate values


!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=0         ! Binary scale factor
      idrstmpl(3)=2         ! Decimal scale factor
      idrstmpl(4)=15        ! Decimal scale factor
      idrstmpl(5)=0         !
      idrstmpl(6)=1         !
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute
!     idrstmpl(10)=9047     !
      idrstmpl(11)=0        !
      idrstmpl(12)=5        !
      idrstmpl(13)=1        !
      idrstmpl(14)=1        !
!     idrstmpl(15)=12       !
      idrstmpl(16)=5        !
      idrstmpl(17)=2
      idrstmpl(18)=2
!
!-- section 6:
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
!
!==============================================================================
!-- set file unit

      ifilw1=51
      call baopen(ifilw1,trim(outfile),ierr)
      if(ierr.ne.0) then
       print*,'can not open ',trim(outfile)
       stop 2001
      endif

!----start time loop

      nowdate=msdate 
      nowtime=markutc*10000

      cgrib1=' '

      do mday=1,ksteps/24   

!      call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)

!      call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

!      do mday=1,ksteps/24   

       nowdate81=nowdate

       do L=1,nspecies

      call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)

      call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)


        nowdate8=nowdate81
        if ( varlist(L).eq.'o3_8hr') then 
          nowtime8=12-icyc+(mday-1)*24+1
        else 
          nowtime8=6-icyc+(mday-1)*24
        endif 

       ipdstmpl(9)=nowtime8


       call nextime(nowdate8,nowtime, 240000)


       nowdate=nowdate8
       call daymon(nowdate,imonth,idate)
!       nowdate=nowdate81

       ipdstmpl(16)=int(nowdate/1000)           !  Year
       ipdstmpl(17)=imonth                      !  Month
       ipdstmpl(18)=idate                       !  Date
       m_test=mday*24+markutc
       if  (varlist(L).eq.'o3_8hr' ) then 
!        ipdstmpl(19)=markutc+8-1+(mday-1)*24          !  Forecast hour
        if (m_test .gt. ksteps .and. icyc .le. 7 ) then 
         ipdstmpl(19)=11-5
!        print*,"h22=","mday=",mday,"ipdstmpl(19)=",ipdstmpl(19)
	else
          ipdstmpl(19)=11   ! used to be 12
        endif
       else
!        ipdstmpl(19)=markutc
        ipdstmpl(19)=markutc-1
       endif
 

!
!jp       if ( varlist(L).eq.'o3_8hr' .and. mday .eq. 2 ) then
!jp         ipdstmpl(19)=time1_end
!jp       endif

!       do L=1,nspecies

        if(varlist(L).eq.'o3_1hr') then

        do i=1,imax
	 do j=1,jmax
	   work(i,j)=maxval(o3(i,j,(mday-1)*24+1:mday*24))   ! 1hr daily O3 maximum
          enddo
	 enddo
	 
        ipdstmpl(1)=14        ! catogory
        ipdstmpl(2)=200       ! daily 1-hour average O3 
        ipdstmpl(24)=0        ! ave 
!        if (varlist(L).eq.'pm25_1hr') then
!        m_test=mday*24+markutc
         ipdstmpl(27)=23       ! 23 hrs 
!        endif 
	else if(varlist(L).eq.'pm25_1hr') then
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(pm25(i,j,(mday-1)*24+1:mday*24))   ! 1hr daily PM25 maximum
          enddo
	 enddo

        ipdstmpl(1)=14        ! catogory
        ipdstmpl(2)=202       ! daily 1-hour average PM2.5
        ipdstmpl(24)=0        ! ave 
        ipdstmpl(27)=23       ! 24 hr 
        ipdstmpl(9)=(6-icyc)+(mday-1)*24 
        ipdstmpl(19)=markutc-1  

	else if(varlist(L).eq.'o3_8hr') then  
           
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(o3_8hr(i,j,(mday-1)*24+1:min((mday-1)*24+24,ksteps)))   ! 8hr daily O3 maximum
          enddo
	 enddo

        ipdstmpl(1)=14        ! catogory
        ipdstmpl(2)=201       ! daily 8-hour average O3
        ipdstmpl(24)=0        ! ave 
        m_test1=mday*24+markutc-1
       if (m_test1 .ge. ksteps .and. icyc .le. 7  ) then
         ipdstmpl(27)=18       ! 24 hr
       else
         ipdstmpl(27)=23
       endif

       else if(varlist(L).eq.'pm25_24hr') then  
         do i=1,imax
	  do j=1,jmax
	     work(i,j)=pm25_24hr(i,j,1+(mday-1)*24)  ! the 1st hour denote 5 am 
          enddo
	 enddo
        ipdstmpl(1)=13        ! catogory
        ipdstmpl(2)=193       ! daily 8-hour average O3
        ipdstmpl(24)=0        ! average 
        ipdstmpl(27)=23       ! 24 hr
        ipdstmpl(9)=(markutc-icyc)+(mday-1)*24+1 
        ipdstmpl(19)=markutc-1  

        endif

       nowdate=nowdate81

      do j=1,ny
       do i=1,nx
          fld1(i+(j-1)*nx)=work(i,j)
       enddo
      enddo

       ibmap=255
       call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)

! -estimate forecast steps

       ntmpdate=istime(1)/100                  ! use file 1 as base
       ntmptime=mod(istime(1),100)*10000
       nowdatetime=nowdate*100+nowtime/10000
       
       if(ntmpdate*100+ntmptime/10000.gt.nowdatetime) then
         ntmpstep=-tstep3d
       else
         ntmpstep=tstep3d
       endif
       
!       print*,'nowdatetime,ntmpdate,ntmptime,ntmpstep=',nowdatetime,ntmpdate,ntmptime,ntmpstep
       do nsteps=1,300
        if(ntmpdate*100+ntmptime/10000.eq.nowdatetime) exit
        call nextime(ntmpdate,ntmptime,ntmpstep)
       enddo
       if(nsteps.gt.300) then
        print*,'wrong time in CMAQ files ',ntmpdate,ntmptime,nowdate,nowtime
        stop
       endif  

       call gribend(cgrib1,max_bytes,lengrib,ierr)
       call wryte(51, lengrib, cgrib1)

       enddo  ! L 
 
       nowdate=nowdate81
	
	call nextime(nowdate,nowtime, 240000)
	
      enddo

!       call gribend(cgrib1,max_bytes,lengrib,ierr)
!       call wryte(51, lengrib, cgrib1)



      end program cmaq_maxi2grib2
       
