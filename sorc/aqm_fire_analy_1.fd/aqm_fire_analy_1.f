      program smoke2cmaq_35layer_1

c      implicit none

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API
      include 'CONST3.EXT'      ! i/o API
      
      parameter (imax=442,jmax=265,kmax=35,nvars=33)
      parameter (start=12,numrec=25)

      integer nfire,fhour,syear,smon,sday,jdate,jtime,jstep,
     1 total,n_heat,n_pm25,n_pm10,n_pm,n_co,n_co2,n_ch4,n_nmhc,emlays
      integer year(500),mon(500),day(500),hour(500),min(500),
     1 duration(500),intx(500),inty(500),intime(500,numrec),
     2 indomain(500),ftime(500),lstk(500,25),lpbl(500,25),ltop(500,25),
     3 header(6)
     
      real lat(500),lon(500),hgt(500),rate(500),area(500),heat(500),
     1 time(500),heatbtu(500),pm25(500),pm10(500),pm(500),co(500),
     2 co2(500),ch4(500),nmhc(500),x(500),y(500),latitude(500),
     3 longitude(500),acres(500),heatflux(500),tmpsum,pm25co(500)
      real hmix(500,25),psfc(500,25),tsfc(500,25),ddzf(500,25,35),
     1 qv(500,25,35),ta(500,25,35),zf(500,25,35),zh(500,25,35),
     2 zstk(500,25,35),pres(500,25,0:35),uw(500,25,35),vw(500,25,35),
     3 tstk(500,25),wstk(500,25),dthdz(500,25,35),wspd(500,25,35),
     4 zzf(500,25,0:35),ustmp(500,25),hfx(500,25),tmpbflx(500,25),
     5 ztop(500,25),zbot(500,25),zplm(500,25),tmpacre(500,25),
     6 sfract(500,25),tfrac(500,25,35)
      real xlon(imax,jmax),xlat(imax,jmax),xx(imax,jmax),yy(imax,jmax),
     1 vheight(kmax+1)
      real truelat1,truelat2,cenlat,cenlon,tlat,tlon,ddx,hts,ptop
      
      character*160 efile,ffile(500),grdcro2d,GRID,metcro3d,MCRO3,
     1 metdot3d,MDOT3,metcro2d,MCRO2,ofile(3),OUTPUT(3)
      character hline*300,aline(2)*120,bline(3)*120,cline(2)*120,
     1 dline(4)*120,eline(4)*120,dirname*120,emifix*16,firefix(3)*16,
     2 grdfix*32,metfix(3)*32,outfix(3)*32,chftmp*4,chytmp*4,chmtmp*2,
     3 chdtmp*2,firespec(8)*12 
     
      real, allocatable, dimension  (:,:,:,:) :: emis
      real, allocatable, dimension  (:,:,:,:) :: femis  
      real, allocatable, dimension  (:,:,:,:) :: lfrac  
      
c metcro2d variables      
      real, allocatable, dimension  (:,:,:) :: pbl    !mix height
      real, allocatable, dimension  (:,:,:) :: prsfc  !surface pres
      real, allocatable, dimension  (:,:,:) :: temp2  !surface temp
      real, allocatable, dimension  (:,:,:) :: ustar  !surface friction
      real, allocatable, dimension  (:,:,:) :: mhfx  !heat flux      
      
c metcro3d variables      
      real, allocatable, dimension  (:,:,:,:) :: mzh
      real, allocatable, dimension  (:,:,:,:) :: mzf
      real, allocatable, dimension  (:,:,:,:) :: mqv
      real, allocatable, dimension  (:,:,:,:) :: mta
      real, allocatable, dimension  (:,:,:,:) :: mpre   !pressure at middle  
      real, allocatable, dimension  (:,:,:,:) :: presf  !pressure at full
      real, allocatable, dimension  (:,:,:,:) :: dens
            
c metdot3d variables
      real, allocatable, dimension (:,:,:,:) :: uwind
      real, allocatable, dimension (:,:,:,:) :: vwind      
                                    
      allocate(emis(imax,jmax,kmax,7),STAT=ierr)
      if(ierr.ne.0) stop 2001      
      allocate(femis(imax,jmax,8,kmax),STAT=ierr)
      if(ierr.ne.0) stop 2002
      allocate(lfrac(imax,jmax,kmax,kmax),STAT=ierr)
      if(ierr.ne.0) stop 2003
      
c metcro2d variables      
      allocate(pbl(imax,jmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2004
      allocate(prsfc(imax,jmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2005
      allocate(temp2(imax,jmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2006
      allocate(ustar(imax,jmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2007
      allocate(mhfx(imax,jmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2008
            
c metcro3d variables
      allocate(mzh(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2009 
      allocate(mzf(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2010 
      allocate(mqv(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2011 
      allocate(mta(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2012 
      allocate(mpre(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2013         
      allocate(presf(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2014
      allocate(dens(imax,jmax,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2015
            
c metdot3d variables      
      allocate(uwind(imax+1,jmax+1,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2016   
      allocate(vwind(imax+1,jmax+1,kmax,25),STAT=ierr)
      if(ierr.ne.0) stop 2017
      
                                        
      data n_heat/1/,n_pm25/2/,n_pm10/3/,n_pm/4/,n_co/5/,n_co2/6/,
     1 n_ch4/7/,n_nmhc/8/
      data emlays/35/
      
      data hts/0.0/ !stack height can be set as 100.0
                  
c      data dirname
c     1 /'/data/data04/aqf/lip/OAR-GROUP/for_Daninel/dir_fire/0704/'/
cc      data dirname/'/aquest/lip/FIRE/07112011/'/
      data emifix/'EMITIMES'/
      data firefix/'NOAA','_','.OUT'/
      data grdfix/'aqm.t12z.grdcro2d.ncf'/
      data metfix/'aqm.t12z.metcro3d.ncf','aqm.t12z.metdot3d.ncf',
     1 'aqm.t12z.metcro2d.ncf'/
cc      data grdfix/'GRIDCRO2D_'/
cc      data metfix/'METCRO3D_','METDOT3D_','METCRO2D_'/      
      data outfix/'.play3d.t12z.fire.ncf',
     1 '.smokefire2d.ncf','.smokefire3d.ncf'/
      
cc      data syear/2011/
cc      data smon/7/
cc      data sday/11/
      
      data firespec/'HEAT','PM25','PM10','PM','CO','CO2','CH4','NMHC'/
      
      namelist/control/syear,smon,sday,dirname
      
      open(7,file='fire.ini')
      read(7,control)
      print*, syear,smon,sday,dirname
      
      aline(1)=dirname
      aline(2)=emifix
      
      efile=aline(1)(:len_trim(aline(1)))//
     1 aline(2)(:len_trim(aline(2)))
      print*,efile      

      open(10,file=efile,status='old')
      read(10,'(a300)')hline
      read(10,'(a300)')hline
c      read(10,'(a300)')hline
      read(10,*)header(1:6)
	    
c      nfire=0
c      do while(.true.)
c       nfire=nfire+1
c       read(10,*,end=96)year(nfire),mon(nfire),day(nfire),hour(nfire),
c     1  min(nfire),duration(nfire),lat(nfire),lon(nfire),hgt(nfire),
c     2  rate(nfire),area(nfire),heat(nfire)
c       heatflux(nfire)=heat(nfire)/area(nfire)
cc       print*, heatflux(nfire)
cc       print*,nfire,year(nfire),mon(nfire),day(nfire),hour(nfire),
cc     1  min(nfire),duration(nfire),lat(nfire),lon(nfire),hgt(nfire),
cc     2  rate(nfire),area(nfire),heat(nfire)
c      enddo
c 96   close(10)
c      nfire=nfire-1
c      print*,'the number of total detected fires is', nfire

      nfire=header(6)
      print*, 'the number of total detected fires is',nfire
      
      do n=1,nfire
       read(10,*)year(n),mon(n),day(n),hour(n),min(n),duration(n),
     1  lat(n),lon(n),hgt(n),rate(n),area(n),heat(n)
        heatflux(n)=heat(n)/area(n)
      enddo
      close(10)


      cline(1)=dirname
      cline(2)=grdfix
      
      write(chytmp,'(i4.4)')syear
      write(chmtmp,'(i2.2)')smon
      write(chdtmp,'(i2.2)')sday       
           
c      grdcro2d=cline(1)(:len_trim(cline(1)))//
c     1 cline(2)(:len_trim(cline(2)))//chytmp//chmtmp//chdtmp
      grdcro2d=cline(1)(:len_trim(cline(1)))//
     1 cline(2)(:len_trim(cline(2)))     
      print*, grdcro2d
      
      iflag=setenvvar('GRID',grdcro2d)
      
      if(.not.open3('GRID',FSREAD3,'aq_open')) then
       print*, 'failed to open GRID'
       stop
      endif


      if(.not. DESC3('GRID') ) then
       print*, 'Error getting info from GRID'
       stop
      endif
      
      if (imax.ne.ncols3d.and.jmax.ne.nrows3d) then
       print*, 'Error in domain setting',imax,ncols3d,jmax,nrows3d
       stop
      endif
      
c      print*, 'p_alp3d=',p_alp3d
c      print*, 'p_bet3d=',p_bet3d
c      print*, 'p_gam3d=',p_gam3d
c      print*, 'xcent3d=',xcent3d
c      print*, 'ycent3d=',ycent3d
c      print*, 'xcell3d=',xcell3d      

      if(.not.read3('GRID','LON',ALLAYS3,0,0,xlon)) then
       print*, 'reading error in LON'
       stop
      endif
      if(.not.read3('GRID','LAT',ALLAYS3,0,0,xlat)) then
       print*, 'reading error in LAT'
       stop
      endif
            
      truelat1=p_bet3d  ! Longitude of pole point
      truelat2=p_alp3d   ! Latitude of pole point
      ddx=xcell3d/1000.   ! grid size in kilo meter
      cenlat=ycent3d
      cenlon=xcent3d
      tlat=21.8212
      tlon=-120.628

      iflag=close3('GRID')
      
c      do i=1,imax
c       do j=1, jmax
c        call llij_lc(xlat(i,j),xlon(i,j),truelat1,truelat2,cenlat,
c     1   cenlon,tlat,tlon,xx(i,j),yy(i,j),ddx) 
c        print*, i,j,xlon(i,j),xlat(i,j),xx(i,j),yy(i,j)        	 
c       enddo
c      enddo

      dline(1)=dirname
      dline(2)=metfix(1)
      dline(3)=metfix(2)
      dline(4)=metfix(3)
      
c      metcro3d=dline(1)(:len_trim(dline(1)))//
c     1 dline(2)(:len_trim(dline(2)))//chytmp//chmtmp//chdtmp
      metcro3d=dline(1)(:len_trim(dline(1)))//
     1 dline(2)(:len_trim(dline(2)))    
      print*, metcro3d
      
      iflag=setenvvar('MCRO3',metcro3d)
      
      if(.not.open3('MCRO3',FSREAD3,'aq_open')) then
       print*, 'failed to open MCRO3'
       stop
      endif

      if(.not. DESC3('MCRO3') ) then
       print*, 'Error getting info from MCRO3'
       stop
      endif      

      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
      
      do k=1,kmax+1
       vheight(k)=vglvs3d(k)
c       print*, vheight(k)
      enddo
      
      ptop=vgtop3d
c      print*, "ptop=", ptop
      
      do m=1,25
       if(.not.READ3('MCRO3','ZF',ALLAYS3, jdate, jtime,
     1  mzf(1,1,1,m))) then
        print*, 'Error in reading ZF from MCRO3'
        stop
       endif 
       if(.not.READ3('MCRO3','ZH',ALLAYS3, jdate, jtime,
     1  mzh(1,1,1,m))) then
        print*, 'Error in reading ZH from MCRO3'
        stop
       endif 
       if(.not.READ3('MCRO3','QV',ALLAYS3, jdate, jtime,
     1  mqv(1,1,1,m))) then
        print*, 'Error in reading QV from MCRO3'
        stop
       endif 
       if(.not.READ3('MCRO3','TA',ALLAYS3, jdate, jtime,
     1  mta(1,1,1,m))) then
        print*, 'Error in reading TA from MCRO3'
        stop
       endif 
       if(.not.READ3('MCRO3','PRES',ALLAYS3, jdate, jtime,
     1  mpre(1,1,1,m))) then
        print*, 'Error in reading PRES from MCRO3'
        stop
       endif        
       if(.not.READ3('MCRO3','DENS',ALLAYS3, jdate, jtime,
     1  dens(1,1,1,m))) then
        print*, 'Error in reading DENS from MCRO3'
        stop
       endif
                                          
       call nextime(jdate,jtime,jstep)
      enddo      

      
      iflag=close3('MCRO3') 
      
      eline(1)=dirname
      eline(2)=outfix(1)
      write(chytmp,'(i4.4)')syear
      write(chmtmp,'(i2.2)')smon
      write(chdtmp,'(i2.2)')sday  
            
      ofile(1)=eline(1)(:len_trim(eline(1)))//
     1 chytmp//chmtmp//chdtmp//eline(2)(:len_trim(eline(2)))                       
     
      print*, ofile(1)
      
      iflag=setenvvar('OUTPUT(1)',ofile(1))       
            
      mxrec3d=25
      nvars3d=1     

      do L=1,nvars3d
       vname3d(L)='LFRAC'
       units3d(L)='fraction'
       vdesc3d(L)='Model species'
       vtype3d(L)=M3REAL
      enddo 
      print*,mxrec3d,nvars3d
            
      if(.not.open3('OUTPUT(1)',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT(1)'
       stop
      endif
      
c      print*,'mxrec3d=',mxrec3d
      
c      metdot3d=dline(1)(:len_trim(dline(1)))//
c     1 dline(3)(:len_trim(dline(3)))//chytmp//chmtmp//chdtmp
      metdot3d=dline(1)(:len_trim(dline(1)))//
     1 dline(3)(:len_trim(dline(3))) 
      print*, metdot3d
      
      iflag=setenvvar('MDOT3',metdot3d)
      
      if(.not.open3('MDOT3',FSREAD3,'aq_open')) then
       print*, 'failed to open MDOT3'
       stop
      endif

      if(.not. DESC3('MDOT3') ) then
       print*, 'Error getting info from MDOT3'
       stop
      endif   
         
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
      
      do m=1,25
       if(.not.READ3('MDOT3','UWIND',ALLAYS3, jdate, jtime,
     1  uwind(1,1,1,m))) then
        print*, 'Error in reading UWIND from MDOT3'
        stop
       endif 
       if(.not.READ3('MDOT3','VWIND',ALLAYS3, jdate, jtime,
     1  vwind(1,1,1,m))) then
        print*, 'Error in reading VWIND from MDOT3'
        stop
       endif 
       
       call nextime(jdate,jtime,jstep)      
      enddo

      
      iflag=close3('MDOT3') 
      
c      metcro2d=dline(1)(:len_trim(dline(1)))//
c     1 dline(4)(:len_trim(dline(4)))//chytmp//chmtmp//chdtmp
      metcro2d=dline(1)(:len_trim(dline(1)))//
     1 dline(4)(:len_trim(dline(4)))   
      print*, metcro2d
      
      iflag=setenvvar('MCRO2',metcro2d)
      
      if(.not.open3('MCRO2',FSREAD3,'aq_open')) then
       print*, 'failed to open MCRO2'
       stop
      endif

      if(.not. DESC3('MCRO2') ) then
       print*, 'Error getting info from MCRO2'
       stop
      endif      

      print*,'mxrec3d=',mxrec3d
      print*,'sdate3d=',sdate3d
      print*,'stime3d=',stime3d
      print*,'tstep3d=',tstep3d
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
      
      do m=1,25
       if(.not.READ3('MCRO2','PBL',ALLAYS3, jdate, jtime,
     1  pbl(1,1,m))) then
        print*, 'Error in reading pbl from MCRO2'
        stop
       endif 
       if(.not.READ3('MCRO2','PRSFC',ALLAYS3, jdate, jtime,
     1  prsfc(1,1,m))) then
        print*, 'Error in reading prsfc from MCRO2'
        stop
       endif 
       if(.not.READ3('MCRO2','TEMP2',ALLAYS3, jdate, jtime,
     1  temp2(1,1,m))) then
        print*, 'Error in reading temp2 from MCRO2'
        stop
       endif 
       if(.not.READ3('MCRO2','USTAR',ALLAYS3, jdate, jtime,
     1  ustar(1,1,m))) then
        print*, 'Error in reading USTAR from MCRO2'
        stop
       endif 
       if(.not.READ3('MCRO2','HFX',ALLAYS3, jdate, jtime,
     1  mhfx(1,1,m))) then
        print*, 'Error in reading HFX from MCRO2'
        stop
       endif 
       
! calculate full layer pressure based on sigmap
       do i=1,imax
        do j=1,jmax
	 do k=1,kmax
	  presf(i,j,k,m)=ptop+vheight(k+1)*(prsfc(i,j,m)-ptop)
	 enddo
	enddo
       enddo
                                   
       call nextime(jdate,jtime,jstep)
      enddo      
      
      iflag=close3('MCRO2') 
      
      
      eline(1)=dirname
      eline(3)=outfix(2)
      write(chytmp,'(i4.4)')syear
      write(chmtmp,'(i2.2)')smon
      write(chdtmp,'(i2.2)')sday  
                                 
      ofile(2)=eline(1)(:len_trim(eline(1)))//
     1 chytmp//chmtmp//chdtmp//eline(3)(:len_trim(eline(3)))
     
      print*, ofile(2)
      
      iflag=setenvvar('OUTPUT(2)',ofile(2))
            
      mxrec3d=25
      nvars3d=8
c      sdate3d=2012113

      do L=1,nvars3d
       vname3d(L)=firespec(L)
       if(vname3d(L).ne.'HEAT') then
        units3d(L)='kg/hour'
       else
        units3d(L)='BTU'
       endif
       vdesc3d(L)='Model species'
       vtype3d(L)=M3REAL
      enddo 
      print*,mxrec3d,nvars3d
               
      if(.not.open3('OUTPUT(2)',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT(2)'
       stop
      endif  
      
      if(.not. DESC3('OUTPUT(2)') ) then
       print*, 'Error getting info from OUTPUT(2)'
       stop
      endif  
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      total=mxrec3d
      
      print*, jdate,jtime,jstep,total  
      
c      emis(1:imax,1:jmax,1:kmax,1:nvars)=0.0
      femis(1:imax,1:jmax,1:8,1:kmax)=0.0 
      lfrac(1:imax,1:jmax,1:kmax,1:kmax)=0.0      
               
      do n=1,nfire       
       write(chftmp,'(i4.4)')n
c       write(chytmp,'(i4.4)')year(n)
c       write(chmtmp,'(i2.2)')mon(n)
c       write(chdtmp,'(i2.2)')day(n)
       write(chytmp,'(i4.4)')syear
       write(chmtmp,'(i2.2)')smon
       write(chdtmp,'(i2.2)')sday       
       
       bline(1)=firefix(1)
       bline(2)=firefix(2)
       bline(3)=firefix(3)
       
       
       ffile(n)=aline(1)(:len_trim(aline(1)))//
     1  bline(1)(:len_trim(bline(1)))//chftmp//
     2  bline(2)(:len_trim(bline(2)))//chytmp//chmtmp//chdtmp//
     3  bline(3)(:len_trim(bline(3)))
       print*,ffile(n)
       
       open(11,file=ffile(n),status='old')
       
        read(11,'(a300)')hline
        read(11,'(a17,f5.2)')hline,latitude(n)
	read(11,'(a17,f7.2)')hline,longitude(n)
	read(11,'(a300)')hline
	read(11,'(a17,f7.2)')hline,acres(n)
		
c	print*, latitude(n),longitude(n),acres(n)
	
	if((latitude(n)-lat(n)).ge.0.01.or.(longitude(n)-lon(n)).
     1       ge.0.01) then
         print*, 'latitude and longitude mismatch in nfire', n
	 stop
	endif
	
       
       do i=1,11
        read(11,'(a300)')hline
       enddo
       
       do while(.true.)
	
	read(11,*,end=97)time(n),heatbtu(n),pm25(n),pm10(n),pm(n),
     1   co(n),co2(n),ch4(n),nmhc(n)
        pm25co(n)=pm25(n)/co(n)
c	print*,pm25co(n)
c        print*, time(n),heatbtu(n),pm25(n),pm10(n),pm(n),co(n),co2(n),
c     1   ch4(n),nmhc(n)
       enddo
 97    close(11)
 
       call llij_lc(lat(n),lon(n),truelat1,truelat2,cenlat,cenlon,
     1  tlat,tlon,x(n),y(n),ddx)
               
c       print*, lon(n), lat(n), x(n), y(n)
       
       intx(n)=int(x(n))
       inty(n)=int(y(n))
       
c       print*, intx(n),inty(n)
       
       if (intx(n).ge.1.and.intx(n).le.imax.and.inty(n).ge.1.and.
     1      inty(n).le.jmax) then
         indomain(n)=1
       else
         indomain(n)=0
       endif
       
c       print*, indomain(n)
       
c       print*, mon(n),day(n),hour(n),min(n),duration(n),start

       if (indomain(n).eq.1) then
       
        if (mon(n).ne.smon.or.day(n).ne.sday) then
         hour(n)=hour(n)+24
        endif
       
        fhour=start*100     !hh to hhmm
        hour(n)=hour(n)*100+min(n)
        ftime(n)=hour(n)+duration(n)
c        print*, ftime(n)
	      
        do m=1,numrec
	
c	 print*, 'PBL at fire spot is',n,m,pbl(intx(n),inty(n),m)
	 
	 if (fhour.ge.hour(n).and.fhour.le.ftime(n)) then
	  intime(n,m)=1
	 else
	  intime(n,m)=0
	 endif
	 
	 if(intime(n,m).eq.1) then
C convert hysplit smoke fire emission rate to real rate
c hysplit smoke rate is 6 hours total
c hypslit smoke fire is assumed to be brush buring 
c convert to wildfire 0.121/0.083=1.46 for pm25 	 
	  femis(intx(n),inty(n),n_heat,m)=heat(n)/6.0
cc	  femis(intx(n),inty(n),n_pm25,m)=pm25(n)/6.0/1.46
	  femis(intx(n),inty(n),n_pm25,m)=pm25(n)/6.0
	  femis(intx(n),inty(n),n_pm10,m)=pm10(n)/6.0
	  femis(intx(n),inty(n),n_pm,m)=pm(n)/6.0
	  femis(intx(n),inty(n),n_co,m)=0.0*co(n)/6.0
	  femis(intx(n),inty(n),n_co2,m)=0.0*co2(n)/6.0
	  femis(intx(n),inty(n),n_ch4,m)=0.0*ch4(n)/6.0
	  femis(intx(n),inty(n),n_nmhc,m)=0.0*nmhc(n)/6.0
	  
	  hmix(n,m)=pbl(intx(n),inty(n),m)
	  psfc(n,m)=prsfc(intx(n),inty(n),m)*0.01 !convert pa to mb
	  pres(n,m,0)=psfc(n,m)
	  tsfc(n,m)=temp2(intx(n),inty(n),m)
	  ustmp(n,m)=max(ustar(intx(n),inty(n),m),0.1) !min value for ustar
C	  hfx(n,m)=heatflux(n)/(CP * dens(intx(n),inty(n),1,m))
	  hfx(n,m)=mhfx(intx(n),inty(n),m)/
     1     (CP * dens(intx(n),inty(n),1,m))	  
	  tmpbflx(n,m)=heatbtu(n)/6.0*0.00000258 !convert factor
c	  tmpbflx(n,m)=heatbtu(n)/10.0*0.00000258 !convert factor	  
c	  tmpbflx(n,m)=heatbtu(n)*4.0*0.00000258 !convert factor	  
C	  tmpbflx(n,m)=heatbtu(n)/time(n)*60.0*0.00000258 !convert factor
C	  tmpbflx(n,m)=heatbtu(n)*0.00000258 !convert factor
	  	  
c	  print*,'ustmp=',n,m,ustmp(n,m),ustar(intx(n),inty(n),m)
C	  print*, 'hfx=',n,m,hfx(n,m),heatflux(n),CP,
C     1     dens(intx(n),inty(n),1,m),mhfx(intx(n),inty(n),m)
c          print*,'tmpbflx=',n,m,tmpbflx(n,m),heatbtu(n),time(n)
	  
c	  print*, n,m,hmix(n,m)

          do k=1,emlays
	   if(k.eq.1) then
	    ddzf(n,m,k)=1.0/mzf(intx(n),inty(n),k,m)
	   else
	    ddzf(n,m,k)=1.0/(mzf(intx(n),inty(n),k,m)-mzf(intx(n),
     1       inty(n),1,m))
	   endif
	   
	    qv(n,m,k)=mqv(intx(n),inty(n),k,m)
	    ta(n,m,k)=mta(intx(n),inty(n),k,m)
	    zf(n,m,k)=mzf(intx(n),inty(n),k,m)
	    zh(n,m,k)=mzh(intx(n),inty(n),k,m)
	    zstk(n,m,k)=zf(n,m,k)-hts
	    pres(n,m,k)=presf(intx(n),inty(n),k,m)*0.01 !convert pa to mb
	    uw(n,m,k)=uwind(intx(n),inty(n),k,m) !u,v are on dot files 
	    vw(n,m,k)=vwind(intx(n),inty(n),k,m)
	  
	  enddo !emlays
c	  print*, n,m,hmix(n,m),psfc(n,m),tsfc(n,m),ddzf(n,m,1:35),
c     1     qv(n,m,1:35),ta(n,m,1:35),zf(n,m,1:35),zh(n,m,1:35),
c     2     zstk(n,m,1:35),pres(n,m,0:35),uw(n,m,1:35),vw(n,m,1:35)
	  
	  call fire_preplm(emlays,hmix(n,m),hts,psfc(n,m),tsfc(n,m),
     1     ddzf(n,m,1:35),qv(n,m,1:35),ta(n,m,1:35),uw(n,m,1:35),
     2     vw(n,m,1:35),zh(n,m,1:35),zf(n,m,1:35),zstk(n,m,1:35),     
     3     pres(n,m,0:35),lstk(n,m),lpbl(n,m),tstk(n,m),wstk(n,m),
     4     dthdz(n,m,1:35),wspd(n,m,1:35),zzf(n,m,0:35))

c          print*, lstk(n,m)
c	  print*, lpbl(n,m)
c	  print*, tstk(n,m)
c	  print*, wstk(n,m)
c	  print*, dthdz(n,m,1:35)
c	  print*, wspd(n,m,1:35)
c	  print*, zzf(n,m,0:35)

c         print*,n,m,emlays,lpbl(n,m),lstk(n,m),hfx(n,m),hmix(n,m),
c     1    tmpbflx(n,m),tstk(n,m),ustmp(n,m),wstk(n,m)
c         print*,dthdz(n,m,1:35),ta(n,m,1:35),wspd(n,m,1:35),
c     1    zzf(n,m,0:35),zh(n,m,1:35),zstk(n,m,1:35)
	 

          call fire_plmris(emlays,lpbl(n,m),lstk(n,m),hfx(n,m),
     1     hmix(n,m),tmpbflx(n,m),tstk(n,m),ustmp(n,m),dthdz(n,m,1:35),
     2     ta(n,m,1:35),wspd(n,m,1:35),zzf(n,m,0:35),zh(n,m,1:35),
     3     zstk(n,m,1:35),wstk(n,m),ztop(n,m),zbot(n,m),zplm(n,m))
     
c          print*, 'ztop=',ztop(n,m)
c	  print*, 'zbot=',zbot(n,m)
c	  print*, 'zplm=',zplm(n,m)
	  
	  tmpacre(n,m)=acres(n)

c          print*,n,m,emlays,zbot(n,m),ztop(n,m),pres(n,m,0:35),
c     1     zzf(n,m,0:35),ta(n,m,1:35),zh(n,m,1:35),tmpacre(n,m)
     	  
	  call fire_postplm(emlays,zbot(n,m),ztop(n,m),pres(n,m,0:35),
     1     zzf(n,m,0:35),ta(n,m,1:35),zh(n,m,1:35),tmpacre(n,m),
     2     sfract(n,m),ltop(n,m),tfrac(n,m,1:35))
     
          print*,'sfract=',sfract(n,m),ltop(n,m)
	  print*,'ltop=',ltop(n,m)
	  print*,'tfrac=',tfrac(n,m,1:35)


          tmpsum=0.0	  
	  do k=1,35
	   
	   if(tfrac(n,m,k).lt.0.0) then
	    print*,'tfrac negative value',n,m,k,tfrac(n,m,k)
	    stop
	   else
	    lfrac(intx(n),inty(n),k,m)=tfrac(n,m,k)
	   endif
	   
	   tmpsum=tmpsum+tfrac(n,m,k)	   
	    
	  enddo

	  if(tmpsum-1.0.ge.0.01) then
	   print*,'the sum of tfrac in not equal to 1.0',tmpsum
	   stop
	  endif	  
c	  print*,'tmpsum=',tmpsum
	 
	 
	  	  	  	  
	 endif ! withintime
c	 print*, n,m,hmix(n,m),psfc(n,m),tsfc(n,m),ddzf(n,m,1:35),
c     1    qv(n,m,1:35),ta(n,m,1:35),zf(n,m,1:35),zh(n,m,1:35),
c     2    zstk(n,m,1:35),pres(n,m,0:35),uw(n,m,1:35),vw(n,m,1:35)

c         print*, lstk(n,m)
c	 print*, lpbl(n,m)
c	 print*, tstk(n,m)
c	 print*, wstk(n,m)
c	 print*, dthdz(n,m,1:35)
c	 print*, wspd(n,m,1:35)
c	 print*, zzf(n,m,0:35)
	
c	 print*,m, intime(n,m),hour(n),fhour,ftime(n)
         fhour=fhour+100	 
	 	
        enddo !numrec
       endif !withindomain
	       
      enddo !nfire
      
      
      do n=1,25
       do L=1,nvars3d
        if(.not.WRITE3('OUTPUT(2)',vname3d(L),jdate,jtime,
     &  femis(1,1,L,n))) then
         print*,' write error in output(2)', vname3d(L)
         stop
        endif
       enddo       
       call nextime(jdate,jtime,jstep)
C       print*,jdate,jtime,jstep
      enddo         
      
      iflag=close3('OUTPUT(2)')

      if(.not. DESC3('OUTPUT(1)') ) then
       print*, 'Error getting info from OUTPUT(1)'
       stop
      endif 
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
            
      do n=1,25
       if(.not.WRITE3('OUTPUT(1)','LFRAC',jdate,jtime,
     & lfrac(1,1,1,n))) then
        print*,' write error in output(1)', LFRAC
        stop
       endif      
       call nextime(jdate,jtime,jstep)
C       print*,jdate,jtime,jstep
      enddo      

      iflag=close3('OUTPUT(1)')
      
      eline(1)=dirname
      eline(4)=outfix(3)
      write(chytmp,'(i4.4)')syear
      write(chmtmp,'(i2.2)')smon
      write(chdtmp,'(i2.2)')sday 
      
      ofile(3)=eline(1)(:len_trim(eline(1)))//
     1 chytmp//chmtmp//chdtmp//eline(4)(:len_trim(eline(4)))
     
      print*, ofile(3)
      
      iflag=setenvvar('OUTPUT(3)',ofile(3))
      
      nvars3d=7

      do L=1,nvars3d
       vname3d(L)=firespec(L+1)       
       units3d(L)='kg/hour'               
       vdesc3d(L)='Model species'
       vtype3d(L)=M3REAL
      enddo 
      print*,mxrec3d,nvars3d
               
      if(.not.open3('OUTPUT(3)',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT(3)'
       stop
      endif  
      
      if(.not. DESC3('OUTPUT(3)') ) then
       print*, 'Error getting info from OUTPUT(3)'
       stop
      endif  
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      do n=1,25
       emis(1:imax,1:jmax,1:kmax,1:7)=0.0
       do L=1,nvars3d
        do i=1,imax
	 do j=1,jmax
	  do k=1,kmax
	   emis(i,j,k,L)=femis(i,j,L+1,n)*lfrac(i,j,k,n)
	  enddo
	 enddo
	enddo
        if(.not.WRITE3('OUTPUT(3)',vname3d(L),jdate,jtime,
     &   emis(1,1,1,L))) then
         print*,' write error in output(3)', vname3d(L)
         stop
        endif 
       enddo     
       call nextime(jdate,jtime,jstep)
      enddo  
      
      iflag=close3('OUTPUT(3)')                
      
      end program smoke2cmaq_35layer_1      
      

      SUBROUTINE llij_lc( lat, lon, proj_truelat1, proj_truelat2,cenlat,
     1 cenlon,proj_lat1, proj_lon1, i, j, proj_dx)

  ! Subroutine to compute the geographical latitude and longitude values
  ! to the cartesian x/y on a Lambert Conformal projection.
    
       REAL, PARAMETER    :: pi = 3.1415927
       REAL, PARAMETER    :: deg_per_rad = 180./pi
       REAL, PARAMETER    :: rad_per_deg = pi / 180.


    ! Input Args
       REAL, INTENT(IN)              :: lat      ! Latitude (-90->90 deg N)
       REAL, INTENT(IN)              :: lon      ! Longitude (-180->180 E)
       REAL, INTENT(IN)              :: proj_dx,cenlon,cenlat 
       REAL, INTENT(IN)              :: proj_lat1, proj_lon1

    ! Output Args                 
       REAL, INTENT(OUT)             :: i        ! Cartesian X coordinate
       REAL, INTENT(OUT)             :: j        ! Cartesian Y coordinate

    ! Locals 
       REAL                          :: arg
       REAL                          :: deltalon
       REAL                          :: tl1r
       REAL                          :: rm
       REAL                          :: ctl1r
    

      if(proj_truelat1.gt.proj_truelat2) then
        temp=proj_truelat1
        proj_truelat1=proj_truelat2
        proj_truelat2=temp
      endif
      
      earth_radius_m=6371.2
    
      proj_rebydx = earth_radius_m / proj_dx

      if(proj_truelat1.lt.0) then
        proj_hemi = -1.
      else  
        proj_hemi = 1
      endif       
    
    ! First, see if this is a secant or tangent projection.  For tangent
    ! projections, truelat1 = truelat2 and the cone is tangent to the 
    ! Earth surface at this latitude.  For secant projections, the cone
    ! intersects the Earth surface at each of the distinctly different
    ! latitudes

      IF (ABS(proj_truelat1-proj_truelat2) .GT. 0.1) THEN
      ! Compute cone factor following:
       proj_cone=(ALOG(COS(proj_truelat1*rad_per_deg))-
     &  ALOG(COS(proj_truelat2*rad_per_deg)))/
     & (ALOG(TAN((90.-ABS(proj_truelat1))*rad_per_deg*0.5 ))- 
     &  ALOG(TAN((90.-ABS(proj_truelat2))*rad_per_deg*0.5 )) )
      ELSE
       proj_cone = SIN(ABS(proj_truelat1)*rad_per_deg )  
      ENDIF    
      
      deltalon1 = proj_lon1 - cenlon
      IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
      IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.
    ! Convert truelat1 to radian and compute COS for later use
      tl1r = proj_truelat1 * rad_per_deg
      ctl1r = COS(tl1r)

    ! Compute the radius to our known lower-left (SW) corner
      proj_rsw = proj_rebydx * ctl1r/proj_cone *
     & (TAN((90.*proj_hemi-proj_lat1)*rad_per_deg/2.) / 
     & TAN((90.*proj_hemi-proj_truelat1)*rad_per_deg/2.))**proj_cone

    ! Find pole point
            
      arg = proj_cone*(deltalon1*rad_per_deg)
      proj_polei = 1. - proj_hemi * proj_rsw * SIN(arg)
      proj_polej = 1. + proj_rsw * COS(arg) 

    !!!! BEGIN CODE    
    ! Compute longitude differences and ensure we stay out of the
    ! forbidden "cut zone"    
      deltalon = lon - cenlon
      IF (deltalon .GT. +180.) deltalon = deltalon - 360.
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.      

    ! Convert truelat1 to radian and compute COS for later use
      tl1r = proj_truelat1 * rad_per_deg
      ctl1r = COS(tl1r)
      
      
    ! Radius to desired point
      rm = proj_rebydx * ctl1r/proj_cone *
     & (TAN((90.*proj_hemi-lat)*rad_per_deg/2.) / 
     & TAN((90.*proj_hemi-proj_truelat1)*rad_per_deg/2.))**proj_cone
      
      arg = proj_cone*(deltalon*rad_per_deg)
      i = proj_polei + proj_hemi * rm * SIN(arg)
      j = proj_polej - rm * COS(arg)

    ! Finally, if we are in the southern hemisphere, flip the i/j
    ! values to a coordinate system where (1,1) is the SW corner
    ! (what we assume) which is different than the original NCEP
    ! algorithms which used the NE corner as the origin in the 
    ! southern hemisphere (left-hand vs. right-hand coordinate?)
      IF (proj_hemi .EQ. -1.) THEN
        i = 2. - i  
        j = 2. - j
      ENDIF
      RETURN
      END SUBROUTINE llij_lc


        SUBROUTINE FIRE_PREPLM( EMLAYS, HMIX, HTS, PSFC, TS, DDZF, 
     &                          QV, TA, UW, VW, ZH, ZF, ZSTK, PRES, 
     &                          LSTK, LPBL, TSTK, WSTK, DTHDZ, WSPD, 
     &                          ZZF )

C***********************************************************************
C  subroutine body starts at line
C
C  DESCRIPTION:
C    Computes the values needed for the PLMRIS subroutine from the 
C    meteorology data.
C
C  PRECONDITIONS REQUIRED:
C    Interpolated (to the location of a source) meteorology data as input,
C    vertical grid structure.
C
 
        IMPLICIT NONE
 
C...........   INCLUDES:
        INCLUDE 'PARMS3.EXT'    !  I/O API parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'FDESC3.EXT'    !  I/O API file description data structures.
        INCLUDE 'CONST3.EXT'    ! physical and mathematical constants

C...........   EXTERNAL FUNCTIONS and their descriptions:
        REAL          POLY

        EXTERNAL      POLY

C...........   SUBROUTINE ARGUMENTS (NOTE: All met parms are per-source)
        INTEGER, INTENT (IN) :: EMLAYS          ! no. emissions layers
        REAL   , INTENT (IN) :: HMIX            ! mixing height
        REAL   , INTENT (IN) :: HTS             ! stack height
        REAL   , INTENT (IN) :: PSFC            ! surface pressure
        REAL   , INTENT (IN) :: TS              ! surface temperature
        REAL   , INTENT (IN) :: DDZF ( EMLAYS ) ! 1/( zf(l) - zf(l-1) )
        REAL   , INTENT (IN) :: QV   ( EMLAYS ) ! mixing ratio
        REAL   , INTENT (IN) :: TA   ( EMLAYS ) ! absolute temperature
        REAL   , INTENT (IN) :: UW   ( EMLAYS ) ! x-direction winds
        REAL   , INTENT (IN) :: VW   ( EMLAYS ) ! y-direction winds
        REAL   , INTENT (IN) :: ZF   ( EMLAYS ) ! layer surface height (m)
        REAL   , INTENT (IN) :: ZH   ( EMLAYS ) ! layer center  height (m)
        REAL   , INTENT (IN) :: ZSTK ( EMLAYS ) ! zf( l,s ) - stkht(s) (m)
        REAL   , INTENT (IN) :: PRES ( 0:EMLAYS )! pressure at full-layer heights
        INTEGER, INTENT(OUT) :: LSTK            ! first L: ZF(L) > STKHT
        INTEGER, INTENT(OUT) :: LPBL            ! first L: ZF(L) > mixing layer
        REAL   , INTENT(OUT) :: TSTK            ! tmptr at top of stack (K)
        REAL   , INTENT(OUT) :: WSTK            ! wind speed @ top of stack(m/s)
        REAL   , INTENT(OUT) :: DTHDZ( EMLAYS ) ! potential temp. gradient
        REAL   , INTENT(OUT) :: WSPD ( EMLAYS ) ! wind speed (m/s)
        REAL   , INTENT(OUT) :: ZZF  ( 0:EMLAYS )! elevation at full-levels

C...........   Local variables

        INTEGER       L, M

        REAL          ES
        REAL          QSFC
        REAL          TVSFC
        REAL          THETG
        REAL          THV1
        REAL          THVK
        REAL          TV( EMLAYS )     ! virtual temperature
        REAL          TF( EMLAYS )     ! full-layer height temperatures
        REAL          P, Q
        REAL          DZZ
        REAL          DELZ

C***********************************************************************
C   begin body of subroutine PREPLM

C.........  Compute wind speed and virtual temperature
c        print*, 'in subroutine',EMLAYS,HMIX,HTS,PSFC,TS

        
c	print*, DDZF(1:EMLAYS),QV(1:EMLAYS),TA(1:EMLAYS),ZF(1:EMLAYS),
c     1   ZH(1:EMLAYS),ZSTK(1:EMLAYS),PRES(0:EMLAYS),UW(1:EMLAYS),
c     2   VW(1:EMLAYS)

        DO L = 1, EMLAYS
 
            P = UW( L )
            Q = VW( L )
            WSPD( L ) = SQRT( P * P  +  Q * Q )
            TV  ( L ) = TA( L ) * 
     &                  ( 1. + 0.622 * ( QV( L ) / ( 1. + QV( L ) ) ) )
 
        END DO

        ES    = 6.1078 * EXP( 5384.21 / CTOK - 5384.21 / TS )
        QSFC  = 0.622  * ES / ( PSFC - ES )
        TVSFC = TS   * ( 1.0 + 0.6077 * QSFC )
        THETG = TVSFC  * ( 1000.0 / PSFC )**0.286

        IF ( HMIX .LE. ZF( 1 ) ) THEN
            LPBL = 1
        END IF
        IF ( HTS .LE. ZF( 1 ) ) THEN
            LSTK = 1
        END IF

        ZZF( 0 ) = 0.0
        ZZF( 1 ) = ZF( 1 )
        
C.........  Compute temperatures at full-layer face heights
        DO L = 1, EMLAYS - 1
            DELZ = ZH( L+1 ) - ZH( L )
            TF( L ) = TV( L ) + ( TV( L+1 ) - TV( L ) ) * 
     &                ( ZF( L ) - ZH( L ) ) / DELZ
        END DO
        
        DELZ = ZH( EMLAYS ) - ZH( EMLAYS-1 )
        TF( EMLAYS ) = TV( EMLAYS ) - (TV( EMLAYS-1 ) - TV( EMLAYS )) *
     &                 ( ZF( EMLAYS ) - ZH( EMLAYS ) ) / DELZ
                       
        THV1 = TF( 1 ) * ( 1000. / PRES( 1 ) )**0.286
        DTHDZ( 1 ) = ( THV1 - THETG ) / ZF( 1 )

        DO L = 2, EMLAYS
 
            IF ( HMIX > ZF( L-1 ) )  LPBL = L
            IF ( HTS > ZF( L-1 ) )  LSTK = L
 
            THVK = TF( L ) * ( 1000. / PRES( L ) )**0.286
            DTHDZ( L ) = DDZF( L ) * ( THVK - THV1 )
            THV1 = THVK
 
            ZZF( L ) = ZF( L )
 
        END DO
        
C.........  Set the 1st level vertical THETV gradient to the 2nd layer value
C           This overrides the layer 1 gradient determined above
        DTHDZ( 1 ) = DTHDZ( 2 )

        M    = MAX( 1, LSTK - 2 )
        TSTK = TS
        WSTK = WSPD( 1 )
	
c	print*, 'in subroutine'
c        print*, LSTK
c	print*, LPBL
c	print*, TSTK
c	print*, WSTK
c	print*, DTHDZ(1:35)
c	print*, WSPD(1:35)
c	print*, ZZF(0:35)	

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I7, :, 1X ) )

        END SUBROUTINE FIRE_PREPLM


        SUBROUTINE FIRE_PLMRIS( EMLAYS, LPBL, LSTK, HFX, HMIX, BFLX,
     &                          TSTK, USTAR, DTHDZ, TA, WSPD, ZF, ZH,
     &                          ZSTK, WSTK, TOP, BOT, ZPLM )

C***********************************************************************
C  subroutine body starts at line 141
C
C  DESCRIPTION:  
C       computes plume top and bottom for fires
C
C  PRECONDITIONS REQUIRED:
C       meteorology and stack parameters
C
C  
C***********************************************************************

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'    ! I/O API constants
        INCLUDE 'FDESC3.EXT'    ! I/O API file description data structure
        INCLUDE 'IODECL3.EXT'   ! I/O API function declarations
        INCLUDE 'CONST3.EXT'    ! physical and mathematical constants

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT (IN) :: EMLAYS    ! no. of emission layers
        INTEGER, INTENT (IN) :: LPBL      ! lyr of height of PBL, = RADM's KMIX
        INTEGER, INTENT (IN) :: LSTK      ! lyr of top of stack, = RADM's KSTK
        REAL   , INTENT (IN) :: HFX            ! sensible heat flux (M K / S )
        REAL   , INTENT (IN) :: HMIX           ! mixing height (m)
        REAL   , INTENT (IN) :: BFLX           ! bouyancy flux (m^4/s^3)
        REAL   , INTENT (IN) :: TSTK           ! tmptr at top of stack (deg K)
        REAL   , INTENT (IN) :: USTAR          ! friction velocity (m/s)
        REAL   , INTENT (IN) :: DTHDZ( EMLAYS )! gradient of THETV
        REAL   , INTENT (IN) :: TA   ( EMLAYS )! temperature (deg K)
        REAL   , INTENT (IN) :: WSPD ( EMLAYS )! wind speed (m/s)
        REAL   , INTENT (IN) :: ZF ( 0:EMLAYS )! layer surface height (m)
        REAL   , INTENT (IN) :: ZH   ( EMLAYS )! layer center height (m) 
        REAL   , INTENT (IN) :: ZSTK ( EMLAYS )! zf( l )   - stkht   (m)
        REAL, INTENT(IN OUT) :: WSTK           ! wind speed @ top of stack (m/s)
        REAL   , INTENT(OUT) :: TOP            ! plume top elevation (m)
        REAL   , INTENT(OUT) :: BOT            ! plume bottom elevation (m)
        REAL   , INTENT(OUT) :: ZPLM           ! plume centerline height
                                               ! (can be greater than the height of
                                               !  the top of the EMLAYS layer)

C...........   PARAMETERS and their descriptions:

        REAL, PARAMETER :: GAMA    = -0.0098         ! ?? plume spread param
        REAL, PARAMETER :: HCRIT   =  1.0E-4 * 0.03  ! hfx min * tolerance
        REAL, PARAMETER :: CRDIST  =200.0            ! crit dstnce b/w HMIX & HS
        REAL, PARAMETER :: SMALL   =  3.0E-5         ! Criterion for stability
        REAL, PARAMETER :: D3      =  1.0 /  3.0
        REAL, PARAMETER :: D6      =  1.0 /  6.0
        REAL, PARAMETER :: D30     =  1.0 / 30.0
        REAL, PARAMETER :: D45     =  1.0 / 45.0
        REAL, PARAMETER :: D2664   =  1.0 /  2.664
        REAL, PARAMETER :: D59319  =  1.0 / 59.319
        REAL, PARAMETER :: TWOTHD  =  2.0 /  3.0
        REAL, PARAMETER :: FIVETHD =  5.0 /  3.0
        REAL, PARAMETER :: NODIV0  =  1.0            ! Prevent divide by zero
        REAL, PARAMETER :: ZERO    =  0.0

C...........   EXTERNAL FUNCTIONS and their descriptions:

        REAL        POLY
        EXTERNAL    POLY

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER IQ              !  stab class:  1-unstbl,2-neut,3-stbl
        INTEGER LPLM            !  first L: ZH(L) > Plume height ! same as RADM's KPR
        INTEGER NN              !  layer counter

        REAL    DH              !  plume rise increment to center of the plume
        REAL    DHM             !  plume rise from momentum
        REAL    DHSM            !  stable momentum plume rise
        REAL    DHN             !  plume rise for neutral case
        REAL    DHT             !  plume rise increment to the top of the plume
        REAL    HSTAR           !  convective scale at stack (m**2/s**3)
        REAL    P, R, S         !  scratch coefficients
        REAL    RBFLX           !  residual buoyancy flux (m**4/s**3)
        REAL    TPLM            !  temperature at top of plume (m/s)
        REAL    WPLM            !  wind speed  at top of plume (m/s)
        REAL    ZMIX            !  hmix - hs
        REAL    ZB              !  height of bottom of layer

C...........   STATEMENT FUNCTIONS:

        REAL    B, H, U, US     !  arguments

        REAL    NEUTRL          !  neutral-stability plume rise function
        REAL    STABLE          !  stable            plume rise function
        REAL    UNSTBL          !  unstable          plume rise function

        NEUTRL( H, B, U, US ) =
     &     1.2 * ((B/(U*US*US))**0.6 * (H+1.3*(B/(U*US*US)))**0.4)

        STABLE( B, U, S ) =  2.6 * ( B / ( U * S ) )**D3

        UNSTBL( B, U )    = 30.0 * ( B / U )**0.6

C***********************************************************************
C   begin body of subroutine  PLMRIS

c        print*,'in subroutine plmris'
c	print*, EMLAYS,LPBL,LSTK,HFX,HMIX,BFLX,TSTK,USTAR,WSTK
c	print*, DTHDZ(1:EMLAYS),TA(1:EMLAYS),WSPD(1:EMLAYS),ZF(0:EMLAYS),
c     1   ZH(1:EMLAYS),ZSTK(1:EMLAYS)

C.........  Compute convective scale
        HSTAR = GRAV * HFX / TA( 1 )   ! Using surface temperature is correct

C.........  Initialize layer of plume
        LPLM  = LSTK

C.........  Set minimum wind speed to 1 m/s
        WSTK  = MAX( WSTK, 1.0 )

C.........  Set momentum rise to zero
        DHM   = ZERO

C.........  Compute initial plume rise from stack top to next level surface:
        IF( HSTAR > HCRIT ) THEN             !  unstable case:

            ZMIX = HMIX

            IF ( ZMIX <= 0.0 ) THEN           !  Stack above mixing height:

                S = MAX( GRAV * DTHDZ( LPLM ) / TSTK, SMALL )

C.................  Reset the wind speed at stack to the wind speed at plume
C.................  when the layer of the plume is not equal to the layer of
C.................  the stack.  This is from Models-3, and we have asked
C.................  EPA 10/8/97 why this is done and haven't gotten a response.
                IF( LPLM .NE. LSTK ) THEN
                    WSTK = WSPD( LPLM )
                    IF( WSTK == 0. ) WSTK = NODIV0
                ENDIF

C.................  Set stable momentum rise
                IF( DTHDZ( LPLM ) > 0.001 ) THEN
                    DHSM = ZERO
                ELSE
                    DHSM = DHM
                END IF
                
                DHM = MIN( DHSM, DHM )

C.................  Compute neutral and stable plume rises
                DHN = NEUTRL( ZERO, BFLX, WSTK, USTAR )
                DH  = STABLE( BFLX, WSTK, S )

                IF( DHN < DH ) THEN  ! Take the minimum of neutral and stable
                    IQ = 2
                    DH = DHN
                ELSE 
                    IQ = 3 
                ENDIF

                IF( DHM > DH .AND. WSTK > 1. ) THEN
                    IQ = 4
                    DH = DHM
                ENDIF

                DHT = 1.5 * DH

            ELSE                        !  unstable case:

                DH  = UNSTBL( BFLX, WSTK )
                DHN = NEUTRL( ZERO, BFLX, WSTK, USTAR )

                IF ( DHN < DH ) THEN
                    DH = DHN
                    IQ = 2
                ELSE
                    IQ = 1
                END IF

                IF( DHM > DH .AND. WSTK > 1. ) THEN
                    DH = DHM
                    IQ = 4 
                ENDIF

                DHT = 1.5 * DH
     
            END IF

        ELSE IF( HSTAR < -HCRIT .OR. DTHDZ( LSTK ) > 0.001 ) THEN      !  stable case:

            S   = MAX( GRAV * DTHDZ( LSTK ) / TSTK, SMALL )
            
            DHT = 1.5 * STABLE( BFLX, WSTK, S )
            DHN = 1.5 * NEUTRL( ZERO, BFLX, WSTK, USTAR )

            IF ( DHN < DHT ) THEN
                DHT = DHN
                IQ = 2
            ELSE
                IQ = 3
            END IF

        ELSE                                   !  neutral case:

            DHT = 1.5 * NEUTRL( ZERO, BFLX, WSTK, USTAR )
            IQ  = 2
            
        END IF                 !  hstar ==> unstable, stable, or neutral
  
C.........  Compute further plume rise from between level surfaces:
        NN = 0
        RBFLX = BFLX
        ZPLM  = DHT

C.........  End calculations if the momentum rise was used in the calculation
        IF( IQ == 4 ) GO TO 199  ! to point past iterative bouyancy loop


        DO       !  loop computing further plume rise

            R = ZPLM - ZSTK( LPLM )
            IF( R <= 0.0 ) THEN
                EXIT  ! exit plume rise loop
            END IF
            
            IF( LPLM == EMLAYS ) THEN
                ZPLM = MIN( ZPLM, ZSTK( EMLAYS ) )
                EXIT  ! exit plume rise loop
            END IF

C.............  Re-set met data. NOTE- the original RADM code submitted the 
C.............  WSPD and TA to a interpolator, but then requested the a height of
C.............  interpolation identical to ZH( LPLM ).
            NN = NN + 1
            IF( NN > 1 ) THEN
                WPLM = WSPD( LPLM )
                TPLM = TA  ( LPLM )
            ELSE     ! 1st time, use stack values
                WPLM = WSTK
                TPLM = TSTK
            END IF

C.............  Compute residual bflx by stability case IQ:

            IF( IQ == 1 ) THEN
                R     = D45 * R     ! includes 1.5 factor for plume top
                RBFLX = WPLM * ( R**FIVETHD )
                
            ELSE IF ( IQ == 2 ) THEN
                P     = TWOTHD * ZPLM
                RBFLX = D2664 * ( R**FIVETHD ) * WPLM * ( USTAR**2. ) / 
     &                  P**TWOTHD
     
            ELSE        !  else iq = 3:
                RBFLX = D59319 * WPLM * S * R**3
     
            END IF      !  if stability flag iq is 1, 2, or 3

            IF( LPLM < EMLAYS ) LPLM = LPLM + 1
            WPLM = WSPD( LPLM )
            TPLM = TA  ( LPLM )

C.............  Prevent divide-by-zero by WPLM
            IF( WPLM == 0. ) WPLM = NODIV0

C.............  Process according to stability cases:
            S    = GRAV * DTHDZ( LPLM ) / TPLM
            
            IF( S > SMALL ) THEN               ! stable case:

                DHT = 1.5 * STABLE( RBFLX, WPLM, S )
                DHN = 1.5 * NEUTRL( ZERO, RBFLX, WPLM, USTAR )
                IF ( DHN < DHT ) THEN
                    DHT = DHN
                    IQ  = 2
                ELSE
                    IQ  = 3
                END IF
                DH = DHT / 1.5

            ELSE          ! if upper layer is not stable, use neutral formula

                DHN = NEUTRL( ZERO, RBFLX, WPLM, USTAR )
                DH  = UNSTBL( RBFLX, WPLM )
                IQ = 1
                IF ( DHN < DH ) THEN
                    DH = DHN
                    IQ  = 2
                END IF
                DHT = 1.5 * DH

            END IF
 
            ZPLM = ZSTK( LPLM-1 ) + DHT
            DH   = ZSTK( LPLM-1 ) + DH

        END DO    !  end loop computing further plume rise

199     CONTINUE

C.........  Compute plume top and bottom and determine actual height of
C           plume centerline after rise
        TOP  = ZPLM
        DH   = TWOTHD * ZPLM
        BOT  = 0.5 * DH
        ZPLM = DH
	
C	print*,'in subroutine'
C	print*,'TOP=',TOP
C	print*,'BOT=',BOT
C	print*,'ZPLM=',ZPLM
      
        RETURN

        END SUBROUTINE FIRE_PLMRIS



        SUBROUTINE FIRE_POSTPLM( EMLAYS, ZBOT, ZTOP, PRESF, ZZF, TA,
     &                           ZH, ACRES, SFRACT, LTOP, LFRAC )

C***********************************************************************
C  subroutine body starts at line 
C
C  DESCRIPTION:
C    Subroutine POSTPLM computes plume fractions given a top and bottom
C    height of the plume.  It assumes a uniform distribution in pressure
C    (mass concentration -- minor hydrostatic assumption) from bottom to top.
C
C  PRECONDITIONS REQUIRED:
C    Top and bottom of plume as input, vertical grid structure defined, 
C    vertical pressure distribution and temperature provided.
C
C
C***********************************************************************
 
        IMPLICIT NONE
 
C...........   INCLUDES:
c        INCLUDE 'EMCNST3.EXT'
        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'CONST3.EXT'

C...........   EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER(2)  CRLF

        EXTERNAL      CRLF

C...........   SUBROUTINE ARGUMENTS
        INTEGER, INTENT (IN) :: EMLAYS           ! no. emissions layers
c        INTEGER, INTENT (IN) :: S                ! source ID
        REAL   , INTENT (IN) :: ZBOT             ! plume bottom elevation (m)
        REAL   , INTENT (IN) :: ZTOP             ! plume top elevation (m)
        REAL   , INTENT (IN) :: PRESF( 0:EMLAYS )! pressure at full-levels (mb)
        REAL   , INTENT (IN) :: ZZF  ( 0:EMLAYS )! elevation at full-levels (m)
        REAL   , INTENT (IN) :: TA   ( 1:EMLAYS )! temperature at half-levels (K)
        REAL   , INTENT (IN) :: ZH   ( 1:EMLAYS )! layer center  height (m)
        REAL   , INTENT (IN) :: ACRES            ! number of acres burned
        REAL   , INTENT(OUT) :: SFRACT           ! smouldering fraction
        INTEGER, INTENT(OUT) :: LTOP             ! plume top layer
        REAL   , INTENT(OUT) :: LFRAC( EMLAYS )  ! layer fractions for source

C...........   Local variables

        INTEGER       L

        INTEGER       LBOT 

        DOUBLE PRECISION    DDP
        DOUBLE PRECISION    PDIFF
        
        REAL          PBOT, PTOP
        REAL          TEMP
        REAL          ZMAX, ZMIN
        REAL          AFRACT
        REAL          SUM, BESIZE

        CHARACTER(300) MESG

C***********************************************************************
C   begin body of subroutine POSTPLM

c        print*, 'in subroutine='
c	print*, emlays,zbot,ztop,presf(0:35),zzf(0:35),ta(1:35),
c     1   zh(1:35),acres

C...........   Check if area is zero or missing
        IF (ACRES .LE. 0.) THEN
            LFRAC = 0.0       ! array
            LFRAC( 1 ) = 1.0
            LTOP = 1
            
            RETURN
        END IF

C...........   Compute LBOT, LTOP so that
C...........   ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C...........   ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )
 
        DO L = 1, EMLAYS - 1
            IF ( ZBOT <= ZZF( L ) ) THEN
                LBOT = L
                GO TO  122   ! end loop and skip reset of LBOT
            ELSE
                LFRAC( L ) = 0.0      ! fractions below plume
            END IF
        END DO
        LBOT = EMLAYS           !  fallback

122     CONTINUE                !  loop exit:  bottom found at LBOT
 
        IF ( ZTOP <= ZZF( LBOT ) ) THEN  !  plume in this layer
 
            LFRAC( LBOT ) = 1.0
            LTOP = LBOT
 
            DO L = LBOT + 1, EMLAYS  ! fractions above plume
                LFRAC( L ) = 0.0
            END DO
 
C.........  Note- this check not in original algorithm, but w/o it,
C                         can end up with fractions > 1.0
        ELSE IF( LBOT == EMLAYS ) THEN    ! plume above top layer
 
            LFRAC( LBOT ) = 1.0
 
            DO L = 1, EMLAYS-1       ! fractions below plume
                LFRAC( L ) = 0.0
            END DO
 
        ELSE                               ! plume crosses layers
 
            DO L = LBOT + 1, EMLAYS
                IF ( ZTOP <= ZZF( L ) ) THEN
                    LTOP = L
                    GO TO 126  ! end loop and skip reset of LTOP
                END IF
            END DO
            LTOP = EMLAYS

126         CONTINUE
 
C...........   Compute corresponding PBOT,PTOP so that
C...........   PRESF( LBOT-1 ) <= PBOT < PRESF( LBOT ) and
C...........   PRESF( LTOP-1 ) <= PTOP < PRESF( LTOP )

C............. If above full layer and below half layer... 
            IF( ZBOT < ZH( LBOT ) .AND. ZBOT > ZZF( LBOT-1 ) ) THEN

C.................  Special case near ground
                IF( ZBOT < ZH( 1 ) ) THEN
                    TEMP = TA( 1 )
                ELSE
                    TEMP = ( TA( LBOT ) + TA( LBOT-1 ) ) / 2.
                END IF

C.............  Otherwise, above full layer and above half layer
            ELSE
                TEMP = ( TA( LBOT ) + TA( LBOT+1 ) ) / 2.
            END IF

C.............  Calculate bottom using hydrostatic assumption            
            PBOT = PRESF( LBOT ) * 
     &             EXP( GRAV / (RDGAS*TEMP) * (ZZF( LBOT ) - ZBOT ))
            
C.............  If above full layer and below half layer... 
            IF( ZTOP < ZH( LTOP ) .AND. ZTOP > ZZF( LTOP-1 ) ) THEN

C.................  Special case near ground
                IF( ZTOP < ZH( 1 ) ) THEN
                    TEMP = TA( 1 )
                ELSE
                    TEMP = ( TA( LTOP ) + TA( LTOP-1 ) ) / 2.
                END IF

C.............  Otherwise, above full layer and above half layer
            ELSE
                TEMP = ( TA( LTOP ) + TA( LTOP+1 ) ) / 2.
            END IF

C.............  Calculate top using hydrostatic assumption            
            PTOP = PRESF( LTOP-1 ) * 
     &             EXP( -GRAV / (RDGAS*TEMP) * (ZTOP - ZZF( LTOP-1 )) )
            
            PDIFF = DBLE( PBOT ) - DBLE( PTOP )
            
            IF( PDIFF > 0. ) THEN
            
                DDP = 1.0D0 / ( PDIFF )
                LFRAC( LBOT ) = DDP * 
     &                          (DBLE( PBOT ) - DBLE( PRESF( LBOT ) ))
                LFRAC( LTOP ) = DDP * 
     &                          (DBLE( PRESF( LTOP-1 ) ) - DBLE( PTOP ))
 
            ELSE
c                WRITE( MESG,94010 )
c     &           'Infinitely small plume created for source ', S,
c     &           CRLF() // BLANK5 // 
c     &           'because of inverted vertical pressure gradient!' //
c     &           CRLF() // BLANK5 // 
c     &           'All emissions put in first layer.'
c                CALL M3WARN( 'POSTPLM', 0,0, MESG )
                print*,'small plume,all emissions put in first layer'

                LBOT = 1
                LTOP = 1
                LFRAC( LBOT ) = 1.0
 
            ENDIF
 
            DO L = LBOT+1, LTOP-1 !  layers in plume
                LFRAC( L ) = DDP * 
     &                       (DBLE( PRESF( L-1 ) ) - DBLE( PRESF( L ) ))
            END DO
 
            DO L = LTOP+1, EMLAYS !  fractions above plume
                LFRAC( L ) = 0.0
            END DO
 
        END IF          !  if ztop in same layer as zbot, or not

C.........  For fire smouldering effects, include fractions below LBOT
        BESIZE = 0.0703 * LOG( ACRES ) + 0.3
        
        SFRACT = 1.0D0 - DBLE( BESIZE )
	
	SFRACT = MAX(SFRACT,0.1) ! avoid negative value
        
        PDIFF = DBLE( PRESF( 0 ) ) - DBLE( PBOT )
        DDP   = 1.0D0 / PDIFF
        
        DO L = 1, LBOT-1
            LFRAC( L ) = DDP * 
     &          ( DBLE( PRESF( L-1 ) ) - DBLE( PRESF( L ) ) ) * SFRACT
        END DO
      
        DO L = LBOT, LBOT
            LFRAC( L ) = LFRAC( L ) * ( 1 - SFRACT ) + DDP *
     &          ( DBLE( PRESF( LBOT-1 ) ) - DBLE( PBOT ) ) * SFRACT
        END DO
      
        DO L = LBOT+1, LTOP
            LFRAC( L ) = LFRAC( L ) * ( 1 - SFRACT )
        END DO
	
	print*,'in subroutine ACRES=',ACRES
	print*,'in subroutine besize=',besize
	print*,'in subroutine sfract=',sfract
	print*,'in subroutine presf=',presf(0:35)
	print*,'in subroutine ltop=',ltop
	print*, 'in subroutine lfrac=',lfrac(1:35)

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I7, :, 1X ) )

        END SUBROUTINE FIRE_POSTPLM
