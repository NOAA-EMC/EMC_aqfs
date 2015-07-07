!----------convert CMAQ output to grib format 
!
!  by      Youhua Tang, Jun 2009

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      real, allocatable :: work(:,:), pm25(:,:), sigmas(:), workdot(:,:), &
          o3_8hr(:,:,:,:),ozonecat(:,:),ozonecat_8hr(:,:)
      
      logical, allocatable :: lb(:,:)   ! logical mask
      
      integer, parameter :: naerospec=16, ncmaq=50, nmet=11
      character*16 aerospec(naerospec),varlist(ncmaq),metlist(nmet), &
          cmaqspec(ncmaq),metspec(nmet)
      real conv_ratio(ncmaq)
      	  
      character*200 outfile,ozonecatfile
      character chtmp*2
      integer kpds(200),kgds(200),gribid(ncmaq),gribtb(ncmaq),  &
          mgribid(nmet),mgribtb(nmet)
      logical i3dvar(ncmaq),mi3dvar(nmet),iflag,ave1hr
      
      data aerospec/'ASO4I','ASO4J','ANH4I','ANH4J','ANO3I','ANO3J','AORGAI', &
       'AORGAJ','AORGPAI','AORGPAJ','AORGBI','AORGBJ','AECI','AECJ','A25I','A25J'/
       
      data cmaqspec(1),gribid(1),gribtb(1),i3dvar(1)/'O3',180,129,.TRUE./
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
      data cmaqspec(16),gribid(16),gribtb(16),i3dvar(16)/'ISOP',164,141,.TRUE./
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
 
         
      data metspec(1),mgribid(1),mgribtb(1),mi3dvar(1)/'ZF',8,2,.TRUE./
      data metspec(2),mgribid(2),mgribtb(2),mi3dvar(2)/'PRES',1,2,.TRUE./
      data metspec(3),mgribid(3),mgribtb(3),mi3dvar(3)/'TA',11,2,.TRUE./
      data metspec(4),mgribid(4),mgribtb(4),mi3dvar(4)/'QV',51,2,.TRUE./
      data metspec(5),mgribid(5),mgribtb(5),mi3dvar(5)/'PBL',221,2,.FALSE./
      data metspec(6),mgribid(6),mgribtb(6),mi3dvar(6)/'PBL2',221,130,.FALSE./    ! ACM2 Richardson
      data metspec(7),mgribid(7),mgribtb(7),mi3dvar(7)/'PBLR',221,131,.FALSE./    ! NCEP Richardson based
      data metspec(8),mgribid(8),mgribtb(8),mi3dvar(8)/'MIXHT',67,2,.FALSE./
      data metspec(9),mgribid(9),mgribtb(9),mi3dvar(9)/'UWIND',33,2,.TRUE./
      data metspec(10),mgribid(10),mgribtb(10),mi3dvar(10)/'VWIND',34,2,.TRUE./
      data metspec(11),mgribid(11),mgribtb(11),mi3dvar(11)/'CWATER',153,2,.TRUE./
                   
      data varlist/ncmaq*'   '/ 
      data metlist/nmet*'   '/
      data conv_ratio/ncmaq*1./
      data ozonecatfile/'    '/

      integer indexcmaq(ncmaq),indexmet(nmet),istime(4),ietime(4)    ! file start and ending in YYYYDDDHH

      namelist /control/varlist,metlist,outfile,nlayers,id_gribdomain,&   !   (table A)
                         ave1hr,ozonecatfile
			 
      open(7,file='cmaq2grib.ini')      
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
        print*,'METCRO3D time mismatch ',istime(2),ietime(2),istime(1),ietime(1)
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
        print*,'METCRO2D time mismatch ',istime(3),ietime(3),istime(1),ietime(1)
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
         print*,'METDOT3D time mismatch ',istime(4),ietime(4),istime(1),ietime(1)
	 stop
        endif
   
        allocate(workdot(imax+1,jmax+1))
       endif
       	 
      endif


!-----Grib file header information      
            
      kpds(1)=07          ! ID OF CENTER, NCEP
      kpds(2)=211         ! Generating process ID number, table A
      kpds(3)=id_gribdomain      ! Grid Identification (255 = User defined grid, defined in kgds)
      kpds(4)=128        !  Flag indicating presence of grid description section (almost always
                        !  included in NCEP grib files) or bit map section (BMS) (not usually 
                        !  included, but does happen to be included for your sst files).  It's 
                        !  a binary value; 128 = GDS(yes), BMS(no); 192 = GDS(yes), BMS(yes).
!      if(nlayers.eq.1) then
!       kpds(6)=1
!      else 			
       kpds(6)=107     ! Type of level.  1 = surface (of the Earth, including sea surface).
                       ! Refer to Tables 3 & 3a in GG.  Other types of levels include 100,
                       ! which means standard pressure level. 109 Hybrid, 107 sigma
!      endif
      
      print*,'nlayers, kpds(6), kpds(7) =', nlayers, kpds(6), kpds(7)		       
!      kpds(7)=10000   ! Actual value of the height or pressure level.  0 = surface.
      
      call daymon(istime(1)/100,imonth,idate)   ! istime is in YYYYDDDHH

      kpds(8)=mod(istime(1)/100000,100)	! Initial yy of analysis or forecast 
      kpds(9)=imonth  	                ! Initial mm of analysis or forecast 
      kpds(10)=idate  	                ! Initial dd of analysis or forecast
      kpds(11)=mod(istime(1),100)       ! Initial hh of analysis or forecast
      kpds(12)=0		        ! Initial min of analysis or forecast


      kpds(13)=1      ! forecast time unit of kpds(14), table 4, 1= hour

      kpds(15)=0      ! However, if the data in this GRIB record contain, for 
                      ! example, an average of a value from one time to another, kpds(14) will 
                      ! hold the value of the beginning time of the average, and kpds(15) will 
                      ! hold the ending time.
      if(ave1hr) then
       kpds(16)=3
       kpds(17)=1
      else
       kpds(16)=0     ! time range indicator, table 5
      endif 
      kpds(18)=1     ! grib version
!      kpds(19)=129   ! Version number of Parameter Table (table 2)
      kpds(20)=0     ! number missing from average; meaningless for this data 
      kpds(21)=istime(1)/10000000+1                    ! Century of initial time of your data 
!      kpds(22)=6     ! Units decimal scale factor

!----kdgs start
      if(id_gribdomain.eq.148) then	   ! CMAQ 5x domain

! http://www.nco.ncep.noaa.gov/pmb/docs/libs/w3lib/putgb.html
	  
      kgds(1)=3 		! Data representation type (map projection).  0 = Lat/Lon grid. See table 6
      kgds(2)=imax		  ! Number of grid points in x-direction
      kgds(3)=jmax		  ! Number of grid points in y-direction
      kgds(4)=21821		 ! LA1 LAT OF ORIGIN (LOWER LEFT)
      kgds(5)=-120628		 ! LO1 LON OF ORIGIN (LOWER LEFT
      kgds(6)=136		  ! (1001000) Resolution Flag (see Table 7 in GG).  
      kgds(7)=-97000		 ! LOV - ORIENTATION OF GRID
      kgds(8)=12000		 ! DX - X-DIR INCREMENT
      kgds(9)=12000		 ! DY - Y-DIR INCREMENT      
      kgds(10)=0		 !  PROJECTION CENTER FLAG
				  !	 Bit 1 set to 0 if the North pole is on the projection plane.
				  !	 Bit 1 set to 1 if the South pole is on the projection plane.
      kgds(11)=64	    ! SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
      kgds(12)=33000	      ! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
      kgds(13)=45000	      ! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
      endif
      
      if(trim(ozonecatfile).ne.'') then
	allocate(ozonecat(imax,jmax))
	allocate(ozonecat_8hr(imax,jmax))
      endif
       			  
!-----read CMAQ files

      nowdate=nowsdate     
      nowtime=nowstime      
      
      do nt=1,maxrec1
       
       print*,'nowdate,nowtime=',nowdate,nowtime

       kpds(14)=nt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14)

       if(ave1hr) then
        kpds(15)=nt
        write(chtmp,'(i2.2)')nt
       else
        write(chtmp,'(i2.2)')nt-1
       endif
       
       call baopenw(51,trim(outfile)//chtmp,ierr)
       if(ierr.ne.0) then
        print*,'can not open ',trim(outfile)//chtmp
        stop 2001
       endif
       
       if(trim(ozonecatfile).ne.'') then
        call baopenw(52,trim(ozonecatfile)//chtmp,ierr)
	if(ierr.ne.0) then
	 print*,'can not open ',trim(ozonecatfile)//chtmp
         stop 2002
        endif
       endif
       
       do K=1, nlayers
        
	kpds(7)=nint(sigmas(K)*10000)

!	if(nlayers.eq.1) kpds(7)=0
	
	do L=1,nspcmaq
         
	 if((.not.i3dvar(indexcmaq(L))).and.K.gt.1) cycle

	 if(varlist(L).ne.'PM2.5') then
          if(varlist(L).eq.'O3_8hr') then
	   if(.not.read3('CHEM3D','O3',K,nowdate,nowtime,o3_8hr(1,1,K,nt))) stop
	   o3_8hr(1:imax,1:jmax,K,nt)=o3_8hr(1:imax,1:jmax,K,nt)*conv_ratio(1) ! using O3 conversion ratio
	  else  
           if(.not.read3('CHEM3D',varlist(L),K,nowdate,nowtime,work)) stop
      
           work(1:imax,1:jmax)=work(1:imax,1:jmax)*conv_ratio(indexcmaq(L))  ! for gaseous species, ppm to ppb
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
 	   kpds(14)=nt-8
	  else
	   kpds(14)=nt-7
	  endif 
	  kpds(15)=nt
	 else          
	  kpds(14)=nt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14) 
	  if(ave1hr) kpds(15)=nt
	 endif
	 
         kpds(5)=gribid(indexcmaq(L))        ! parameter ID
	 kpds(19)=gribtb(indexcmaq(L))       ! table version
	 kpds(22)=8-alog10(maxval(work))     ! Units decimal scale factor

         if(.not.(varlist(L).eq.'O3_8hr'.and.nt.lt.8)) call gribitb(lb,work,imax,jmax,51,kpds)
	  
        enddo     ! CMAQ species loop

!---ozone category file
      	
       if(trim(ozonecatfile).ne.'') then
        do i=1,imax
	 do j=1,jmax
	  if(o3_8hr(i,j,K,nt).lt.60.5) then
	     ozonecat(i,j)=1.
          else if(o3_8hr(i,j,K,nt).ge.60.5.and.o3_8hr(i,j,K,nt).lt.84.5) then
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

        kpds(5)=181        ! parameter ID
        kpds(19)=129       ! table version

	kpds(14)=nt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14) 
	if(ave1hr) kpds(15)=nt
	kpds(22)=8-alog10(maxval(ozonecat))     ! Units decimal scale factor
	call gribitb(lb,ozonecat,imax,jmax,52,kpds) 
	
	if(nt.ge.8) then	
          kpds(14)=nt-7
	  kpds(15)=nt	 
!  	  kpds(22)=8-alog10(maxval(ozonecat_8hr))
!	  print*,'ozonecat_8hr=',ozonecat_8hr
          call gribitb(lb,ozonecat_8hr,imax,jmax,52,kpds) 
	endif
	  
       endif
       
! met species start

	kpds(14)=nt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14) 
	if(ave1hr) kpds(15)=nt

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
 	   
	   if(.not.read3('METCRO3D','QC',K,nowdate,nowtime,work)) stop   ! cloud water
	   if(.not.read3('METCRO3D','QR',K,nowdate,nowtime,pm25)) stop   ! rain water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop   ! ice water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop   ! snow water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   
	  else	  
	   if(.not.read3('METCRO3D',metlist(L),K,nowdate,nowtime,work)) stop
	  endif 
	 else if((.not.mi3dvar(indexmet(L))).and.K.eq.1) then
	  if(.not.read3('METCRO2D',metlist(L),K,nowdate,nowtime,work)) stop
	 else
	  cycle
	 endif
       
         kpds(5)=mgribid(indexmet(L))        ! parameter ID
	 kpds(19)=mgribtb(indexmet(L))       ! table version
	 kpds(22)=8-alog10(maxval(work))    ! Units decimal scale factor	
	
         call gribitb(lb,work,imax,jmax,51,kpds)
	enddo  ! met species loop
	   	  
       enddo      ! K loop 

       call nextime(nowdate,nowtime,10000)
	
       call baclose(51,ierr)
       if(trim(ozonecatfile).ne.'') then
        call baclose(52,ierr)
       endif
       	
       enddo     ! time loop

       iflag=shut3()
      end
       

      subroutine gribitb(ln,ozout,im,jm,iunit,KPDSOUT)

      parameter (mxbit=16,lenpds=28,lengds=32)
      character*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)

      character*1  iflag*1, pds*28
      integer ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
      integer KPDSOUT(25),id(25),kgds(20)
      real ozout(im,jm)
!      save kbuf
!****************************************************************
!     PREPARE GRIB PDS
!     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
        
      id(1) =28 !NUMBER OF BYTES IN PRODUCT DEFINITION SECTION(PDS)
      id(2) =KPDSOUT(19)!PARAMETER TABLE VERSION NO (2 or 129 or 130)
      id(3) =KPDSOUT(1)  !IDENTIFICATION OF ORIGINATING CENTER
      id(4) =KPDSOUT(2)!MODEL IDENTIFICATION (BY ORIGINATING CENTER)
      id(5) =KPDSOUT(3)!GRID IDENTIFICATION
      id(6) =1  !IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
      id(7) =0  !IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
      id(8) =KPDSOUT(5)
      id(9) =  KPDSOUT(6)                !Had temporarily for 5x made into 105
      id(11) = KPDSOUT(7)
      id(10) = KPDSOUT(7)/256
      id(12)=KPDSOUT(8)          !  YEAR OF CENTURY
      id(13)=KPDSOUT(9)          !  MONTH OF YEAR
      id(14)=KPDSOUT(10)         !  DAY OF MONTH
      id(15)=KPDSOUT(11)         !  HOUR OF DAY
      id(16)=KPDSOUT(12)         !  MINUTE OF HOUR
      id(17)=KPDSOUT(13)         !  FCST TIME UNIT: 1 for h
      id(18)=KPDSOUT(14)         !  P1 PERIOD OF TIME
      id(19)=KPDSOUT(15)         !  P2 PERIOD OF TIME
      id(20)=KPDSOUT(16)         !  TIME RANGE INDICATOR
      id(21)=KPDSOUT(17)         !  NUMBER INCLUDED IN AVERAGE
      id(22)=0                   !  NUMBER MISSING FROM AVERAGES
      id(23)=KPDSOUT(21)         !  CENTURY 
      id(24)=0                   !  RESERVED - SET TO 0
      sgdg = 5.0        !  MAXIMUM SIGNIFICANT DIGITS TO KEEP
                        !  (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
                        !  OR BINARY PRECISION IF <0
                        !  (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
                        !             -3.0 "                    " 1/8
                        !  2**SGDS PRECISION)
      ibm=0
      ibitm=0
      ibitm = im*jm
      ibmap=1
      ibm =1    !as dictated by id(7)=0

      nout = im*jm
      call get_bits(ibm,sgdg,nout,ibmap,ozout,   &
                   ideci,ozout,gmin,gmax,nbit)

      id(25) =ideci     !   SCALING POWER OF 10

      itype=0
      ibitl = min(nbit,mxbit)
      ipflag=0
      igflag=0
      igrid=id(5)

      do 20 k = 1,18
         igds(k) = 0
 20   continue

      icomp=1
      ibflag=0
      iblen=nout
      do 30 k = 1,9
         ibdsfl(k) = 0
 30   continue

!      print*,'ibitl=',ibitl,id
      call w3fi72(itype,ozout,igrd,ibitl,      &
     &            ipflag,id,pds,               &
     &            igflag,igrid,igds,icomp,     &
     &            ibflag,ibmap,iblen,          &
     &            ibdsfl,                      &
     &            nout,kbuf,itot,ier)


      call wryte(iunit,itot,kbuf)

      return
      end

      SUBROUTINE GET_BITS(IBM,SGDS,LEN,MG,G,ISCALE,GROUND,    &
     &                    GMIN,GMAX,NBIT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    GET_BITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
!   PRGMMR: IREDELL          ORG: W/NP23     DATE: 92-10-31
!
! ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
!   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
!   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
!   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
!   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-04-14  BALDWIN - MODIFY FOLLOWING KEITH BRILL'S CODE
!			TO USE SIG DIGITS TO COMPUTE DEC SCALE
!
! USAGE:   CALL GET_BITS(IBM,ISGDS,LEN,MG,G,ISCALE,GROUND,GMIN,GMAX,NBIT)
!   INPUT ARGUMENT LIST:
!     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!     SGDS     - MAXIMUM SIGNIFICANT DIGITS TO KEEP
!		 (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
!		 OR BINARY PRECISION IF <0
!		 (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
!			    -3.0 "		      " 1/8
!			   2**SGDS PRECISION)
!      LEN	- INTEGER LENGTH OF THE FIELD AND BITMAP
!      MG	- INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!      G	- REAL (LEN) FIELD
!
!    OUTPUT ARGUMENT LIST:
!      ISCALE	- INTEGER DECIMAL SCALING
!      GROUND	- REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
!      GMIN	- REAL MINIMUM VALID ROUNDED FIELD VALUE
!      GMAX	- REAL MAXIMUM VALID ROUNDED FIELD VALUE
!      NBIT	- INTEGER NUMBER OF BITS TO PACK
!
!  SUBPROGRAMS CALLED:
!    ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
!  ATTRIBUTES:
!    LANGUAGE: FORTRAN


      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DETERMINE EXTREMES WHERE BITMAP IS ON
!
      IF(IBM.EQ.0) THEN
        GMAX=G(1)
        GMIN=G(1)
        DO I=2,LEN
          GMAX=MAX(GMAX,G(I))
          GMIN=MIN(GMIN,G(I))
        ENDDO
      ELSE
        I1=0
        DO I=1,LEN
          IF(MG(I).NE.0.AND.I1.EQ.0) I1=I
        ENDDO
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          GMAX=G(I1)
          GMIN=G(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
!
!
      CALL FNDBIT  ( GMIN, GMAX, SGDS, NBIT, ISCALE, RMIN, IRETT)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	RETURN
	END


	SUBROUTINE FNDBIT  ( rmin, rmax, rdb, nmbts, iscale, rmn, iret )
!************************************************************************
!* FNDBIT								*
!*									*
!* This subroutine computes the number of packing bits given the	*
!* maximum number (< 50) of significant digits to preserve or the	*
!* binary precision to store the data.  The binary precision is given	*
!* as zero, a negative integer, or as a postitive integer greater than  *
!* or equal to 50.  If the binary precision is given, ISCALE will	*
!* always be zero in this case. 					*
!*									*
!* The binary precision translates as follows:  			*
!*     53  =>  store data to nearest 8  				*
!*     52  =>  store data to nearest 4  				*
!*     51  =>  store data to nearest 2  				*
!*     50  =>  store data to nearest 1  				*
!*	0  =>  store data to nearest 1  				*
!*     -1  =>  store data to nearest 1/2				*
!*     -2  =>  store data to nearest 1/4				*
!*     -3  =>  store data to nearest 1/8				*
!*									*
!* Note that RDB - 50 give the nearest whole power of two for binary	*
!* precision.								*
!*									*
!* Note that a fractional number of significant digits is allowed.	*
!*									*
!* FNDBIT ( RMIN, RMAX, RDB, NBITS, ISCALE, RMN, IRET ) 		*
!*									*
!* Input parameters:							*
!*	RMIN		REAL		Minimum value			*
!*	RMAX		REAL		Maximum value			*
!*	RDB		REAL		Maximum # of significant digits *
!*					  OR binary precision if < 0	*
!*									*
! * Output parameters:							*
! *	NBITS		INTEGER 	Number of bits for packing	*
! *	ISCALE  	INTEGER 	Power of 10 scaling to use	*
! *	RMN		REAL		Rounded miniumum		*
! *	IRET		INTEGER 	Return code			*
! *					  0 = normal return		*
! **									*
! * Log: 								*
! * K. Brill/NMC 	06/92						*
! * K. Brill/EMC 	12/95	Added binary precision; added RMN	*
! * K. Brill/EMC 	 1/97	Add .5 in rr= & rng2= for better rnd off*
! * K. Brill/EMC 	 1/97	Use 10**iscale in rounding the min	*
! ************************************************************************

	DATA		rln2/0.69314718/
!-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
	rmn = rmin
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF

	IF ( rdb .gt. 0.0 .and. rdb .lt. 50. ) THEN
	    po = FLOAT ( INT ( ALOG10 ( range ) ) )
	    IF ( range .lt. 1.00 ) po = po - 1.
	    po = po - rdb + 1.
	    iscale = - INT ( po )
	    rr = range * 10. ** ( -po ) + .5
	    nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
	ELSE
	    ibin = NINT ( -rdb )
	    IF ( ibin .le. -50. ) ibin = ibin + 50
	    rng2 = range * 2. ** ibin + .5
	    nmbts = INT ( ALOG ( rng2 ) / rln2 ) + 1
	END IF
        IF(NMBTS.GT.0) then

!*	Compute RMN, the first packable value less than or equal to
!*	RMIN.

	tp = 10. ** iscale
	x = ( ALOG ( range * tp ) - ALOG ( 2 ** nmbts - 1. ) ) / rln2
	ixp = INT ( x )
	IF ( FLOAT ( ixp ) .ne. x .and. x .gt. 0. ) ixp = ixp + 1
	irmn = NINT ( ( rmin * tp ) / ( 2. ** ixp ) )
	rmn = FLOAT ( irmn ) * ( 2. ** ixp )
	IF ( rmn .gt. rmin * tp ) rmn = rmn - ( 2. ** ixp )
	rmn = rmn / tp

           rmn = rmn / 10. ** iscale
        ELSE
          nmbts=0
          rmn = rmin
          IF(ABS(rmin).GE.1.) THEN
           ISCALE=INT(ALOG10(ABS(rmin)))
          ELSE IF (ABS(rmin).LT.1..AND.ABS(rmin).GT.0.) then
           ISCALE=INT(ALOG10(ABS(rmin)))+1
          ELSE
           ISCALE=0
          ENDIF
        ENDIF

	RETURN
	END
