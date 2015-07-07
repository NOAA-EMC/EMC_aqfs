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
      integer    nowdate1,nowtime1,idate,idate1,imonth1
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

      data cmaqspec(1),gribid(1),gribtb(1),i3dvar(1)/'O3',193,14,.TRUE./
      data cmaqspec(2),gribid(2),gribtb(2),i3dvar(2)/'NO2',142,141,.TRUE./
      data cmaqspec(3),gribid(3),gribtb(3),i3dvar(3)/'NO',141,141,.TRUE./
      data cmaqspec(4),gribid(4),gribtb(4),i3dvar(4)/'NO3',145,141,.TRUE./
      data cmaqspec(5),gribid(5),gribtb(5),i3dvar(5)/'N2O5',143,141,.TRUE./
      data cmaqspec(6),gribid(6),gribtb(6),i3dvar(6)/'HNO3',144,141,.TRUE./
      data cmaqspec(7),gribid(7),gribtb(7),i3dvar(7)/'HONO',147,141,.TRUE./
      data cmaqspec(8),gribid(8),gribtb(8),i3dvar(8)/'PNA',146,141,.TRUE./
      data cmaqspec(9),gribid(9),gribtb(9),i3dvar(9)/'CO',148,141,.TRUE./
      data cmaqspec(10),gribid(10),gribtb(10),i3dvar(10)/'FORM',166,141,.TRUE./
      data cmaqspec(11),gribid(11),gribtb(11),i3dvar(11)/'ALD2',167,141,.TRUE./
      data cmaqspec(12),gribid(12),gribtb(12),i3dvar(12)/'PAN',172,141,.TRUE./
      data cmaqspec(13),gribid(13),gribtb(13),i3dvar(13)/'NTR',173,141,.TRUE./
      data cmaqspec(14),gribid(14),gribtb(14),i3dvar(14)/'ETH',160,141,.TRUE./
      data cmaqspec(15),gribid(15),gribtb(15),i3dvar(15)/'SO2',232,141,.TRUE./
      data cmaqspec(6),gribid(16),gribtb(16),i3dvar(16)/'ISOP',164,141,.TRUE./
      data cmaqspec(17),gribid(17),gribtb(17),i3dvar(17)/'NH3',149,141,.TRUE./
      data cmaqspec(18),gribid(18),gribtb(18),i3dvar(18)/'ASO4I',189,141,.TRUE./  ! my assignment
      data cmaqspec(19),gribid(19),gribtb(19),i3dvar(19)/'ASO4J',200,141,.TRUE./
      data cmaqspec(20),gribid(20),gribtb(20),i3dvar(20)/'ANH4I',190,141,.TRUE./
      data cmaqspec(21),gribid(21),gribtb(21),i3dvar(21)/'ANH4J',201,141,.TRUE./
      data cmaqspec(22),gribid(22),gribtb(22),i3dvar(22)/'ANO3I',191,141,.TRUE./
      data cmaqspec(23),gribid(23),gribtb(23),i3dvar(23)/'ANO3J',202,141,.TRUE./
      data cmaqspec(24),gribid(24),gribtb(24),i3dvar(24)/'AORGAI',192,141,.TRUE./
      data cmaqspec(25),gribid(25),gribtb(25),i3dvar(25)/'AORGAJ',203,141,.TRUE./
      data cmaqspec(26),gribid(26),gribtb(26),i3dvar(26)/'AORGPAI',193,141,.TRUE./
      data cmaqspec(27),gribid(27),gribtb(27),i3dvar(27)/'AORGPAJ',204,141,.TRUE./
      data cmaqspec(28),gribid(28),gribtb(28),i3dvar(28)/'AORGBI',194,141,.TRUE./
      data cmaqspec(29),gribid(29),gribtb(29),i3dvar(29)/'AORGBJ',205,141,.TRUE./
      data cmaqspec(30),gribid(30),gribtb(30),i3dvar(30)/'AECI',195,141,.TRUE./
      data cmaqspec(31),gribid(31),gribtb(31),i3dvar(31)/'AECJ',206,141,.TRUE./
      data cmaqspec(32),gribid(32),gribtb(32),i3dvar(32)/'A25I',196,141,.TRUE./
      data cmaqspec(33),gribid(33),gribtb(33),i3dvar(33)/'A25J',207,141,.TRUE./
      data cmaqspec(34),gribid(34),gribtb(34),i3dvar(34)/'ASOIL',220,141,.TRUE./
      data cmaqspec(35),gribid(35),gribtb(35),i3dvar(35)/'ACORS',221,141,.TRUE./
      data cmaqspec(36),gribid(36),gribtb(36),i3dvar(36)/'PM2.5',157,129,.TRUE./
      data cmaqspec(37),gribid(37),gribtb(37),i3dvar(37)/'XO2N',181,141,.TRUE./
      data cmaqspec(38),gribid(38),gribtb(38),i3dvar(38)/'NUMATKN',222,141,.TRUE./
      data cmaqspec(39),gribid(39),gribtb(39),i3dvar(39)/'NUMACC',223,141,.TRUE./
      data cmaqspec(40),gribid(40),gribtb(40),i3dvar(40)/'SRFATKN',228,141,.TRUE./
      data cmaqspec(41),gribid(41),gribtb(41),i3dvar(41)/'SRFACC',229,141,.TRUE./
      data cmaqspec(42),gribid(42),gribtb(42),i3dvar(42)/'AH2OJ',208,141,.TRUE./
      data cmaqspec(43),gribid(43),gribtb(43),i3dvar(43)/'AH2OI',211,141,.TRUE./
      data cmaqspec(44),gribid(44),gribtb(44),i3dvar(44)/'EXT_Mie',128,141,.TRUE./
      data cmaqspec(45),gribid(45),gribtb(45),i3dvar(45)/'PAR',159,141,.TRUE./
      data cmaqspec(48),gribid(48),gribtb(48),i3dvar(48)/'OLE',161,141,.TRUE./
      data cmaqspec(46),gribid(46),gribtb(46),i3dvar(46)/'TOL',162,141,.TRUE./
      data cmaqspec(47),gribid(47),gribtb(47),i3dvar(47)/'XYL',163,141,.TRUE./
      data cmaqspec(49),gribid(49),gribtb(49),i3dvar(49)/'H2O2',186,141,.TRUE./
      data cmaqspec(50),gribid(50),gribtb(50),i3dvar(1)/'O3_8hr',180,129,.TRUE./

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
      icyc=INT(ntmptime/10000)
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
       imonth1=imonth
       idate1=idate

!       iyear=istime(1)/10000

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
      ipdsnum=42             ! Product Definition Template Number (Table 4.42) (0:Analysis or forecast at a horizontal level or in a horizontal layer at a pointin time)
      ipdstmpllen=30        ! pdt template length
      ipdstmpl(1)=14        ! catogory
      ipdstmpl(2)=193         !  
      ipdstmpl(3)=0         ! aersol type 
      ipdstmpl(4)=2         ! Type of generating process (Table 4.3) (0:ana,1:ic, 2:fcst)
      ipdstmpl(5)=0         ! Background generating process identifier
      ipdstmpl(6)=211        ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(7)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(8)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(9)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!jp   ipdstmpl(10)=nt-1         ! Forecast time in units defined by !ipdstmpl(8)
      ipdstmpl(11)=104      ! Type of first fixed surface (see Code table 4.5) (100:isobaric leve)
      ipdstmpl(12)=4        ! Scale factor of first fixed surface
      ipdstmpl(13)=10000    ! Scaled value of first fixed surface
      ipdstmpl(14)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(15)=0        ! Scale factor of second fixed surface
      ipdstmpl(16)=0        ! Scaled value of second fixed surface
!jp   ipdstmpl(17)=iyear        !  Year
!jp   ipdstmpl(18)=imonth        !  Month
!jp   ipdstmpl(19)=iday        !  Date
!jp   ipdstmpl(20)=INT(nowtime/10000)        !  Forecast hour
      ipdstmpl(21)=0        !
      ipdstmpl(22)=0        !
      ipdstmpl(23)=1        !
      ipdstmpl(24)=0        !
      ipdstmpl(25)=0        !
      ipdstmpl(26)=2        !
      ipdstmpl(27)=1        !
!       ipdstmpl(28)=1        !
      ipdstmpl(29)=255      !
      ipdstmpl(30)=0        !
!
      numcoord=0            ! Number of coordinate values after template
      coordlist=0.          ! Optional list of coordinate values
!

!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=1         ! Binary scale factor
      idrstmpl(3)=3         ! Decimal scale factor
!      idrstmpl(3)=5         ! Decimal scale factor
!      idrstmpl(4)=15        ! Number of bits used  
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
      if(trim(ozonecatfile).ne.'') then
        allocate(ozonecat(imax,jmax))
        allocate(ozonecat_8hr(imax,jmax))
      endif

!-----read CMAQ files

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

!      do nt=1,maxrec1
      do nt=1,9

       nowdate1=nowdate
       nowtime1=nowtime

!       call nextime(nowdate1,nowtime1,10000)
       call nextime(nowdate1,nowtime1,10000)

       call daymon(nowdate1,imonth1,idate1)

       if(ave1hr) then
        write(chtmp,'(i2.2)')nt
       else
        write(chtmp,'(i2.2)')nt-1
       endif

       call baopen(51,trim(outfile)//chtmp//'.grb2',ierr)

       if(ierr.ne.0) then
        print*,'can not open ',trim(outfile)//chtmp//'.grb2'
        stop 2001
       endif

       if(trim(ozonecatfile).ne.'') then
        call baopen(52,trim(ozonecatfile)//chtmp//'.grb2',ierr)

        if(ierr.ne.0) then
         print*,'can not open ',trim(ozonecatfile)//chtmp//'.grb2'
         stop 2002
        endif
       endif

       cgrib1=''

!      ipdstmpl(1)=gribid(indexcmaq(L))
!      ipdstmpl(2)=gribtb(indexcmaq(L))
       
       ipdstmpl(17)=INT(nowdate1/1000)   ! YEAR (YYYY)
       ipdstmpl(18)=imonth1              !  Month
       ipdstmpl(19)=idate1               !  Date
       ipdstmpl(20)=INT(nowtime1/10000)  !  Forecast hour

       call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)

       call gribcreate(cgrib2,max_bytes,listsec0,listsec1,ierr)

!       call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

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
             print*,"hjp111,varlist(L)=",varlist(L),"nt=",nt,"work=",work(440:442,260:265)
     
          endif
         else             ! PM2.5
          work(1:imax,1:jmax)=0.

          do L2=1,naerospec
           if(.not.read3('CHEM3D',aerospec(L2),K,nowdate,nowtime1,pm25)) stop
           work(1:imax,1:jmax)=pm25(1:imax,1:jmax)+work(1:imax,1:jmax)
          enddo
         endif

       if(varlist(L).eq.'O3_8hr'.and.nt.ge.8) then
          do i=1,imax
           do j=1,jmax
            work(i,j)=sum(o3_8hr(i,j,K,nt-7:nt))/8
           enddo
          enddo
        endif

!      ipdstmpl(1)=gribid(indexcmaq(L))
!      ipdstmpl(2)=gribtb(indexcmaq(L))
       ipdstmpl(1)=14
       ipdstmpl(2)=193
       ipdstmpl(3)=0

       if (varlist(L).eq.'O3_8hr' .and. nt .ge. 8 ) then
        ipdstmpl(10)=nt-7
        ipdstmpl(28)=7        !
       else
        ipdstmpl(10)=nt-1    ! forecast time
        ipdstmpl(28)=1       ! averaging time period
       endif
             
       call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,&
               ideflist,idefnum,ierr)
      

        do i=1,imax
         do j=1,jmax
!jp            fld1(i+(j-1)*nx)=work(i,j)/1000000.   ! ppb--> kg/kg
            fld1(i+(j-1)*nx)=work(i,j)
         enddo
        enddo

       if( varlist(L).eq.'O3' ) then 

        call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)
       endif

 
       if  (varlist(L).eq.'O3_8hr'.and.nt.ge.8) then 
        call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)

       endif
       
!       if(.not.(varlist(L).eq.'O3_8hr'.and.nt.lt.8)) & 
!        call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
!                          coordlist,numcoord,idrsnum,idrstmpl, &
!                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)

!grib1         if(.not.(varlist(L).eq.'O3_8hr'.and.nt.lt.8)) call
!gribitb(lb,work,imax,jmax,51,kpds)

!       endif

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
       ipdstmpl(3)=0       ! type 
       ipdstmpl(10)=nt-1  ! Forecast time in units defined by ipdstmpl(8)
       ipdstmpl(28)=1        !

       ipdstmpl(17)=INT(nowdate1/1000)   ! YEAR (YYYY)
       ipdstmpl(18)=imonth1              !  Month
       ipdstmpl(19)=idate1               !  Date
       ipdstmpl(20)=INT(nowtime1/10000)  !  Forecast hour

      call addgrid(cgrib2,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

      do j=1,ny
       do i=1,nx
          fld1(i+(j-1)*nx)=ozonecat(i,j)
       enddo
      enddo

        call addfield(cgrib2,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)
      do j=1,ny
       do i=1,nx
        if ( nt .gt. 8 ) then
         fld1(i+(j-1)*nx)=ozonecat_8hr(i,j)
        endif
       enddo
      enddo


       if(nt.ge.8) then
        ipdstmpl(10)=nt-7  ! Forecast time in units defined by ipdstmpl(8)
        ipdstmpl(28)=7      !  the averaging time period
        call addfield(cgrib2,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)
        endif
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


!        ipdstmpl(1)=14        ! catogory
!        ipdstmpl(2)=194       ! parameter   ozone category



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
