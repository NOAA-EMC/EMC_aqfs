      program aqm_cmaq2grib2_test 

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      real, allocatable :: work(:,:), pm25(:,:), sigmas(:),workdot(:,:), &
          o3_8hr(:,:,:,:),ozonecat(:,:),ozonecat_8hr(:,:)

      logical, allocatable :: lb(:,:)   ! logical mask

      integer, parameter :: naerospec=16, ncmaq=50, nmet=11
      character*16 aerospec(naerospec),varlist(ncmaq),metlist(nmet), &
          cmaqspec(ncmaq),metspec(nmet)
      real conv_ratio(ncmaq)
! added by JP
      character  infile*80
      character  varname*10,ymd*8,ch_cyc*2
      integer    diag, imax,jmax
      integer    icyc,iyear,imonth,iday,ihour,base_year,nt
      integer    nowdate,nowtime
      integer    ierr,ier



!added by jphuang for grib2
      integer, parameter   :: max_bytes=20000000
      integer, parameter   :: nx=442,ny=265

      integer listsec0(2)
      integer listsec1(13)
      integer igds(5)
      integer igdstmpllen
      integer ipdstmpllen
      integer idrstmpllen
      integer idrsnum,ibmap,numcoord,ipdsnum,idefnum

      integer,dimension(100) :: igdstmpl
      integer,dimension(100) :: ipdstmpl
      integer,dimension(100) :: idrstmpl

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
      integer yy,mm,dd,hh,mn,sc
      real(4) :: dxval

      character*50 gdss(400)
      integer GRID, kgdss(200), lengds,im,jm,jf
!
      character*200 outfile,ozonecatfile
      character chtmp*2
      integer kpds(200),kgds(200),gribid(ncmaq),gribtb(ncmaq),  &
          mgribid(nmet),mgribtb(nmet)
      logical i3dvar(ncmaq),mi3dvar(nmet),iflag,ave1hr

      data aerospec/'ASO4I','ASO4J','ANH4I','ANH4J','ANO3I','ANO3J',&
           'AORGAI','AORGAJ','AORGPAI','AORGPAJ','AORGBI','AORGBJ', &
           'AECI','AECJ','A25I','A25J'/

      data varlist/ncmaq*'   '/
      data metlist/nmet*'   '/
      data conv_ratio/ncmaq*1./
      data ozonecatfile/'    '/

      integer indexcmaq(ncmaq),indexmet(nmet),istime(4),ietime(4)  !file start and ending in YYYYDDDHH

      namelist /control/varlist,metlist,outfile,nlayers,id_gribdomain,& !   (table A)
                         ave1hr,ozonecatfile

      open(7,file='cmaq2grib2.ini')
      read(7,control)
      close(7)

!--- cmaq species

      do L=1,ncmaq
       if(varlist(L).ne.'    ') then

         do L2=1,ncmaq
          if(varlist(L).eq.cmaqspec(L2)) exit
         enddo
         if(L2.gt.ncmaq) then
          print*,'wrong varlist ', varlist(L)
          stop
         endif
         indexcmaq(L)=L2

       else
        exit
       endif
      enddo

      nspcmaq=L-1

      if(nspcmaq.lt.1) then
       print*,'no CMAQ species provided'
!       stop
      endif

!-----met species

      do L=1,nmet
       if(metlist(L).ne.'    ') then
         do L2=1,nmet
          if(metlist(L).eq.metspec(L2)) exit
         enddo
         if(L2.gt.nmet) then
          print*,'wrong metlist ', metlist(L)
          stop
         endif
         indexmet(L)=L2

       else
        exit
       endif
      enddo

      nspcmet=L-1
      if(nspecmet.lt.1) print*,'no met species provided'


! open files

      if(.not.OPEN3('CHEM3D',FSREAD3,'pathway')) then
       print*,'open input file error for CHEM3D'
       stop
      endif

      if (.not. DESC3('CHEM3D') ) then   ! get grid information from CMAQ output
       print*, 'Error getting info from CHEM3D'
       stop
      endif

      do L=1,ncmaq
       do L2=1,nvars3d
        if(vname3d(L2).eq.cmaqspec(L).and.units3d(L2)(1:3).eq.'ppm') conv_ratio(L)=1000. ! convert to ppbv
       enddo
      enddo

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
      kmax=nlays3d

      if(nlayers.gt.kmax) then
       print*,'nlayers too high ', nlayers, kmax
       stop
      endif

      allocate(sigmas(kmax))
      do k=1,kmax
!       sigmas(k)=0.5*(vglvs3d(k)+vglvs3d(k+1))
        sigmas(k)=vglvs3d(k)
      enddo

      nowsdate=sdate3d
      nowstime=stime3d
      maxrec1=mxrec3d

      istime(1)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH

      ntmpdate=sdate3d
      ntmptime=stime3d
      do n=1,mxrec3d-1
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      ietime(1)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH

      allocate(lb(imax,jmax))
      lb(:,:)=.true.
      allocate(work(imax,jmax))
      allocate(pm25(imax,jmax))

      do L=1,nspcmaq
       if(varlist(L).eq.'O3_8hr') then
         allocate(o3_8hr(imax,jmax,nlayers,mxrec3d))
         exit
       endif
      enddo

!---met files
      if(nspcmet.ge.1) then
       if(.not.OPEN3('METCRO3D',FSREAD3,'cmaq2grib')) stop
       if(.not.DESC3('METCRO3D')) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax.or.nlays3d.lt.nlayers) then
         print*,'METCRO3D dimenison mismatch ',ncols3d,nrows3d,nlays3d
         stop
       endif

       istime(2)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
       ntmpdate=sdate3d
       ntmptime=stime3d
       do n=1,mxrec3d-1
        call nextime(ntmpdate,ntmptime,tstep3d)
       enddo
       ietime(2)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH

       if(istime(2).gt.istime(1).or.ietime(2).lt.ietime(1)) then
        print*,'METCRO3D time mismatch',istime(2),ietime(2),istime(1),ietime(1)
        stop
       endif

       if(.not.OPEN3('METCRO2D',FSREAD3,'cmaq2grib')) stop
       if(.not.DESC3('METCRO2D')) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax) then
         print*,'METCRO2D dimenison mismatch ',ncols3d,nrows3d
         stop
       endif

       istime(3)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
       ntmpdate=sdate3d
       ntmptime=stime3d
       do n=1,mxrec3d-1
        call nextime(ntmpdate,ntmptime,tstep3d)
       enddo
       ietime(3)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
       if(istime(3).gt.istime(1).or.ietime(3).lt.ietime(1)) then
        print*,'METCRO2D time mismatch',istime(3),ietime(3),istime(1),ietime(1)
        stop
       endif

       do L=1,nspcmet
        if(metlist(L).eq.'UWIND'.or.metlist(L).eq.'VWIND') exit
       enddo
       if(L.le.nspcmet) then
        if(.not.OPEN3('METDOT3D',FSREAD3,'cmaq2grib')) stop
        if(.not.DESC3('METDOT3D')) stop

        if(ncols3d.ne.imax+1.or.nrows3d.ne.jmax+1.or.nlays3d.lt.nlayers) then
         print*,'METDOT3D dimenison mismatch ',ncols3d,nrows3d,nlays3d
         stop
        endif
        istime(4)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
        ntmpdate=sdate3d
        ntmptime=stime3d
        do n=1,mxrec3d-1
         call nextime(ntmpdate,ntmptime,tstep3d)
        enddo
        ietime(4)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH

        if(istime(4).gt.istime(1).or.ietime(4).lt.ietime(1)) then
         print*,'METDOT3D time mismatch',istime(4),ietime(4),istime(1),ietime(1)
         stop
        endif

        allocate(workdot(imax+1,jmax+1))
       endif

      endif

!-----Grib file header information

      call daymon(istime(1)/100,imonth,idate)   ! istime is in YYYYDDDHH

       nowdate=nowsdate
       nowtime=nowstime

       cgrib1=''
       cgrib2=''

! add grib2 header information

!-- section 0:
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table0)(7:ncep)
      listsec1(2)=0       ! Identification of orginating subcenter (ON388-TableC) (4:emc)
      listsec1(3)=2       ! GRIB master tables version number (Table 1.0) (11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2)(0:ana 1:fcst 2:vrfy)
! following need to be changed !
      listsec1(6)=mod(istime(1)/100000,100) ! Initial yy of analysis or forecast
      listsec1(7)=imonth      ! Reference time - Month
      listsec1(8)=idate      ! Reference time - Day
      listsec1(9)=mod(istime(1),100)      ! Reference time - Hour
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
      endif
        idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
        ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=8             ! Product Definition Template Number (Table 4.0) (0:Analysis or forecast at a horizontal level or in a horizontal layer at a pointin time)
      ipdstmpllen=29        ! pdt template length
!jp   ipdstmpl(1)=13        ! catogory
!jp   ipdstmpl(2)=193       ! parameter
      ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana,1:ic, 2:fcst)
      ipdstmpl(4)=0         ! Background generating process identifier
      ipdstmpl(5)=211        ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!jp      ipdstmpl(9)=nt-1         ! Forecast time in units defined by
!ipdstmpl(8)
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
!

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
!      idrstmpl(10)=9047     !
      idrstmpl(11)=0        !
      idrstmpl(12)=5        !
      idrstmpl(13)=1        !
      idrstmpl(14)=1        !
!      idrstmpl(15)=12       !
      idrstmpl(16)=5        !
      idrstmpl(17)=2
      idrstmpl(18)=2
!
!-- section 6:
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
!
!==============================================================================
      if(trim(ozonecatfile).ne.'') then
        allocate(ozonecat(imax,jmax))
        allocate(ozonecat_8hr(imax,jmax))
      endif

!-----read CMAQ files

!jp      nowdate=nowsdate
!jp      nowtime=nowstime

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

      do nt=1,maxrec1
       print*,'nowdate,nowtime=',nowdate,nowtime


!grib1   kpds(14)=nt-1  ! time range. For the forecast, valid time is
!kpds(8-12)+kpds(14)
!grib2

       if(ave1hr) then
!grib1        kpds(15)=nt
!grib2
        write(chtmp,'(i2.2)')nt
       else
        write(chtmp,'(i2.2)')nt-1
       endif

       call baopen(51,trim(outfile)//chtmp,ierr)

       if(ierr.ne.0) then
        print*,'can not open ',trim(outfile)//chtmp
        stop 2001
       endif

       if(trim(ozonecatfile).ne.'') then
        call baopen(52,trim(ozonecatfile)//chtmp,ierr)

        if(ierr.ne.0) then
         print*,'can not open ',trim(ozonecatfile)//chtmp
         stop 2002
        endif
       endif

       cgrib1=''

       ipdstmpl(1)=14        ! catogory
       ipdstmpl(2)=193       ! parameter   ozone concentration (ppb) OZCON
       ipdstmpl(9)=(5-icyc)+(mday-1)*24         ! Forecast time in units defined by ipdstmpl(8)
       ipdstmpl(16)=iyear    !  Year
       ipdstmpl(17)=imonth   !  Month
       ipdstmpl(18)=iday+mday     !  Date
       ipdstmpl(19)=INT(nowtime/10000)        !  Forecast hour

       call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)
       print*,'gribcreate1 status=',ierr

       call gribcreate(cgrib2,max_bytes,listsec0,listsec1,ierr)
       print*,'gribcreate2 status=',ierr

       call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

       do K=1, nlayers

!grib1  kpds(7)=nint(sigmas(K)*10000)
!grib2
        do L=1,nspcmaq

         if((.not.i3dvar(indexcmaq(L))).and.K.gt.1) cycle

         if(varlist(L).ne.'PM2.5') then
          if(varlist(L).eq.'O3_8hr') then
           if(.not.read3('CHEM3D','O3',K,nowdate,nowtime,o3_8hr(1,1,K,nt))) stop
            o3_8hr(1:imax,1:jmax,K,nt)=o3_8hr(1:imax,1:jmax,K,nt)*conv_ratio(1) ! using O3 conversion ratio
           else
           if(.not.read3('CHEM3D',varlist(L),K,nowdate,nowtime,work)) stop

           work(1:imax,1:jmax)=work(1:imax,1:jmax)*conv_ratio(indexcmaq(L)) ! for gaseous species, ppm to ppb
        endif
         else             ! PM2.5
          work(1:imax,1:jmax)=0.

          do L2=1,naerospec
           if(.not.read3('CHEM3D',aerospec(L2),K,nowdate,nowtime,pm25)) stop
           work(1:imax,1:jmax)=pm25(1:imax,1:jmax)+work(1:imax,1:jmax)
          enddo
         endif

         if(varlist(L).eq.'O3_8hr'.and.nt.ge.8) then
          do i=1,imax
           do j=1,jmax
           work(i,j)=sum(o3_8hr(i,j,K,nt-7:nt))/8
           enddo
          enddo
          if(ave1hr) then
!grib1     kpds(14)=nt-8
!grib2
          else
!grib1     kpds(14)=nt-7
!grib2
          endif
!grib1    kpds(15)=nt
!grib2
         else
!grib1    kpds(14)=nt-1  ! time range. For the forecast, valid time is
!kpds(8-12)+kpds(14)

!grib1    if(ave1hr) kpds(15)=nt
!grib2
         endif

      do j=1,ny
       do i=1,nx
          fld1(i+(j-1)*nx)=work(i,j)
       enddo
      enddo

     call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)


!grib1         if(.not.(varlist(L).eq.'O3_8hr'.and.nt.lt.8)) call
!gribitb(lb,work,imax,jmax,51,kpds)

        enddo     ! CMAQ species loop

!---ozone category file

       if(trim(ozonecatfile).ne.'') then
        do i=1,imax
         do j=1,jmax
          if(o3_8hr(i,j,K,nt).lt.60.5) then
             ozonecat(i,j)=1.
          else if(o3_8hr(i,j,K,nt).ge.60.5.and.o3_8hr(i,j,K,nt).lt.84.5)  then
           ozonecat(i,j)=2.
          else if(o3_8hr(i,j,K,nt).ge.84.5.and.o3_8hr(i,j,K,nt).lt.99.5) then
           ozonecat(i,j)=3.
          else if(o3_8hr(i,j,K,nt).ge.99.5.and.o3_8hr(i,j,K,nt).lt.114.5) then
           ozonecat(i,j)=4.
          else if(o3_8hr(i,j,K,nt).ge.114.5.and.o3_8hr(i,j,K,nt).lt.124.5) then
           ozonecat(i,j)=5.
          else if(o3_8hr(i,j,K,nt).ge.124.5.and.o3_8hr(i,j,K,nt).lt.144.5) then
           ozonecat(i,j)=6.
          else if(o3_8hr(i,j,K,nt).ge.144.5.and.o3_8hr(i,j,K,nt).lt.164.5) then
           ozonecat(i,j)=7.
          else if(o3_8hr(i,j,K,nt).ge.164.5.and.o3_8hr(i,j,K,nt).lt.184.5) then
           ozonecat(i,j)=8.
          else if(o3_8hr(i,j,K,nt).ge.184.5.and.o3_8hr(i,j,K,nt).lt.204.5) then
           ozonecat(i,j)=9.
          else if(o3_8hr(i,j,K,nt).ge.204.5) then
           ozonecat(i,j)=10.
          endif

          if(nt.ge.8) then
            o3ave=sum(o3_8hr(i,j,K,nt-7:nt))/8
            if(o3ave.lt.50.5) then
             ozonecat_8hr(i,j)=1.
            else if(o3ave.ge.50.5.and.o3ave.lt.64.5) then
             ozonecat_8hr(i,j)=2.
            else if(o3ave.ge.64.5.and.o3ave.lt.74.5) then
             ozonecat_8hr(i,j)=3.
            else if(o3ave.ge.74.5.and.o3ave.lt.84.5) then
             ozonecat_8hr(i,j)=4.
            else if(o3ave.ge.84.5.and.o3ave.lt.94.5) then
             ozonecat_8hr(i,j)=5.
            else if(o3ave.ge.94.5.and.o3ave.lt.104.5) then
             ozonecat_8hr(i,j)=6.
            else if(o3ave.ge.104.5.and.o3ave.lt.114.5) then
             ozonecat_8hr(i,j)=7.
            else if(o3ave.ge.114.5.and.o3ave.lt.124.5) then
             ozonecat_8hr(i,j)=8.
            else if(o3ave.ge.124.5.and.o3ave.lt.134.5) then
             ozonecat_8hr(i,j)=9.
            else if(o3ave.ge.134.5) then
             ozonecat_8hr(i,j)=10.
           endif
           if(.not.(ozonecat_8hr(i,j).lt.11.and.ozonecat_8hr(i,j).ge.1)) then
            print*,'wrong ozonecat_8hr ', ozonecat_8hr(i,j),i,j,o3ave
            stop
           endif
          endif

         enddo
        enddo

! write ozone category
       ipdstmpl(1)=14        ! catogory
       ipdstmpl(2)=194       ! parameter   ozone category
       ipdstmpl(9)=(5-icyc)+(mday-1)*24         ! Forecast time in units defined by ipdstmpl(8)
       ipdstmpl(16)=iyear    !  Year
       ipdstmpl(17)=imonth   !  Month
       ipdstmpl(18)=iday+mday     !  Date
       ipdstmpl(19)=INT(nowtime/10000)        !  Forecast hour

       call addgrid(cgrib2,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

      do j=1,ny
       do i=1,nx
        if ( nt .lt. 8 ) then
          fld1(i+(j-1)*nx)=ozonecat(i,j)
        else
         fld1(i+(j-1)*nx)=ozonecat_8hr(i,j)
        endif
       enddo
      enddo

      call addfield(cgrib2,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)

      endif

! met species start
        do L=1,nspcmet
         if(mi3dvar(indexmet(L))) then
          if(metlist(L).eq.'UWIND'.or.metlist(L).eq.'VWIND') then

           if(.not.read3('METDOT3D',metlist(L),K,nowdate,nowtime,workdot)) stop
           if(metlist(L).eq.'UWIND') then
            do i=1,imax
             do j=1,jmax
              work(i,j)=0.5*(workdot(i,j)+workdot(i+1,j))   ! for CMAQ's C grid
             enddo
            enddo
           else
            do i=1,imax
             do j=1,jmax
              work(i,j)=0.5*(workdot(i,j)+workdot(i,j+1))
             enddo
            enddo
           endif

         else if(metlist(L).eq.'CWATER') then     ! total condensed water

           if(.not.read3('METCRO3D','QC',K,nowdate,nowtime,work)) stop ! cloud water
           if(.not.read3('METCRO3D','QR',K,nowdate,nowtime,pm25)) stop ! rain water
           work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
           if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop ! ice water
           work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
           if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop ! snow water
           work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)

          else
           if(.not.read3('METCRO3D',metlist(L),K,nowdate,nowtime,work)) stop
          endif
         else if((.not.mi3dvar(indexmet(L))).and.K.eq.1) then
          if(.not.read3('METCRO2D',metlist(L),K,nowdate,nowtime,work)) stop
         else
          cycle
         endif
        enddo  ! met species loop

       enddo      ! K loop

       call nextime(nowdate,nowtime,10000)

       call gribend(cgrib1,max_bytes,lengrib,ierr)
       call wryte(51, lengrib, cgrib1)


       call gribend(cgrib2,max_bytes,lengrib,ierr)
       call wryte(52, lengrib, cgrib2)

       enddo     ! time loop

       iflag=shut3()


       end program    aqm_cmaq2grib2_test 
