      program smoke2cmaq_35layer_analy_1
c           
c     This program is created by Li.Pan at NOAA/OAR/ARL

c      implicit none

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API
      include 'CONST3.EXT'      ! i/o API
      
      parameter (imax=442,jmax=265,kmax=35) !domain
      parameter (numrec=25,maxnf=500)       !time and fire

      parameter (analy_pm_cnst=1., analy_gas_cnst=0.)
      parameter (heat_convert_cnst=0.00000258)

      integer nfire,fhour,syear,smon,sday,start,jdate,jtime,jstep,
     1 total,n_heat,n_pm25,n_pm10,n_pm,n_co,n_co2,n_ch4,n_nmhc,emlays
     
chc add diurnal profile
      real pm_frac(0:23), heat_frac(0:23)
chc   real analy_pm_cnst, analy_gas_cnst, heat_convert_cnst
      integer analy_local_hour
      integer utc
      real grdheatbtu(imax,jmax,numrec)
      real analy_pm_frac   !anl PM/GAS hourly fraction
      real analy_heat_frac !anl HEAT   hourly fraction
chc add diurnal profile
chc for diagnosis
      character*15 chr_name(41)
      character*80 chrdum
      real wrtout(41,numrec)
      logical flag_write_excel
      integer ii, jj, kk, ll, mm ,nn
chc for diagnosis

      integer year(maxnf),mon(maxnf),day(maxnf),hour(maxnf),min(maxnf),
     1 duration(maxnf),intx(maxnf),inty(maxnf),header(6),ftime(maxnf)
     
      integer intime(maxnf,numrec),indomain(maxnf),lstk(maxnf,numrec),
     1 lbot(maxnf,numrec),
     2 lpbl(maxnf,numrec),ltop(maxnf,numrec)
     
      real lat(maxnf),lon(maxnf),hgt(maxnf),rate(maxnf),area(maxnf),
     1 heat(maxnf),time(maxnf),heatbtu(maxnf),pm25(maxnf),pm10(maxnf),
     2 pm(maxnf),co(maxnf),co2(maxnf),ch4(maxnf),nmhc(maxnf),x(maxnf),
     3 y(maxnf),latitude(maxnf),longitude(maxnf),acres(maxnf),
     4 heatflux(maxnf),pm25co(maxnf)
     
      real hmix(maxnf,numrec),psfc(maxnf,numrec),tsfc(maxnf,numrec),
     1 tstk(maxnf,numrec),wstk(maxnf,numrec), ustmp(maxnf,numrec),
     2 hfx(maxnf,numrec),tmpbflx(maxnf,numrec),ztop(maxnf,numrec),
     3 zbot(maxnf,numrec),zplm(maxnf,numrec),tmpacre(maxnf,numrec),
     4 sfract(maxnf,numrec)
     
      real ddzf(maxnf,numrec,kmax),qv(maxnf,numrec,kmax),
     1 ta(maxnf,numrec,kmax),zf(maxnf,numrec,kmax),
     2 zh(maxnf,numrec,kmax),zstk(maxnf,numrec,kmax),
     3 pres(maxnf,numrec,0:kmax),uw(maxnf,numrec,kmax),
     4 vw(maxnf,numrec,kmax),wspd(maxnf,numrec,kmax),
     5 zzf(maxnf,numrec,0:kmax),tfrac(maxnf,numrec,kmax),
     6 dthdz(maxnf,numrec,kmax)
     
      real xlon(imax,jmax),xlat(imax,jmax),xx(imax,jmax),yy(imax,jmax),
     1 vheight(kmax+1)
     
      real truelat1,truelat2,cenlat,cenlon,tlat,tlon,ddx,hts,ptop,
     1 tmpsum
      
      character*160 efile,ffile(maxnf),GRID,MCRO3,MDOT3,MCRO2,
     1 OUTPUT1,OUTPUT2,OUTPUT3
     
      character hline*300,aline(2)*120,bline(3)*120,dirname*120,
     1 emifix*16,firefix(3)*16,chftmp*4,chytmp*4,chmtmp*2,
     2 chdtmp*2,firespec(8)*12 
     
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
      allocate(femis(imax,jmax,8,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2002
      allocate(lfrac(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2003
      
c metcro2d variables      
      allocate(pbl(imax,jmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2004
      allocate(prsfc(imax,jmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2005
      allocate(temp2(imax,jmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2006
      allocate(ustar(imax,jmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2007
      allocate(mhfx(imax,jmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2008
            
c metcro3d variables
      allocate(mzh(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2009 
      allocate(mzf(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2010 
      allocate(mqv(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2011 
      allocate(mta(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2012 
      allocate(mpre(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2013         
      allocate(presf(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2014
      allocate(dens(imax,jmax,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2015
            
c metdot3d variables      
      allocate(uwind(imax+1,jmax+1,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2016   
      allocate(vwind(imax+1,jmax+1,kmax,numrec),STAT=ierr)
      if(ierr.ne.0) stop 2017
      
      data n_heat/1/,n_pm25/2/,n_pm10/3/,n_pm/4/,n_co/5/,n_co2/6/,
     1 n_ch4/7/,n_nmhc/8/
      data emlays/35/
      
      data hts/0.0/ !stack height can be set as 100.0
                  
      data emifix/'EMITIMES'/
      data firefix/'NOAA','_','.OUT'/
      
      data firespec/'HEAT','PM25','PM10','PM','CO','CO2','CH4','NMHC'/

chc add diurnal profile
      data pm_frac/0.00570114, 0.00570114, 0.00570114, 0.00570114,
     1 0.00570114, 0.00570114, 0.00570114, 0.00570114, 0.00570114,
     2 0.00570114, 0.02000401, 0.04000801, 0.07001400, 0.10002000,
     3 0.13002600, 0.16003200, 0.17003400, 0.12002400, 0.07001401,
     4 0.04000801, 0.00570114, 0.00570114, 0.00570114, 0.00570114/

      data heat_frac/0.0020803256556821600, 0.0020705541795396600,
     1 0.0013733187228315500, 0.0010260869412932700, 0.0008185544662399030,
     2 0.0006807089512152810, 0.0005825625298669910, 0.0005091642217659640,
     3 0.0004511037968307230, 0.0004098816885552800, 0.0084852801695145100,
     4 0.0526339793024440000, 0.1143050168420500000, 0.1531754565788320000,
     5 0.1793493211634940000, 0.1974044722238220000, 0.1728953921098070000,
     6 0.0766986108811116000, 0.0262149334298009000, 0.0086042086899452900,
     7 0.0001345759113781090, 0.0000327073703766417, 0.0000322467417084864,
     8 0.0000315374318945850 /
chc add diurnal profile
      
      namelist/control/syear,smon,sday,start,dirname
chc for diagnosis
      flag_write_excel = .false.
      flag_write_excel = .true.
      if ( flag_write_excel ) then
         chr_name(1)=' '
         chr_name(2)='Layer top'
         chr_name(3)='Layer bottom'
         chr_name(4)='smold frac'
         chr_name(5)='total PM'
         chr_name(6)='total heat'
         do n = 1, 35
            write(chrdum,'(i2)') n
            chr_name(n+6)=chrdum(1:2)
         end do
         do n = 1, numrec
            wrtout(1,n) = float(n)
         end do
      end if
chc for diagnosis
      
      open(7,file='fire.ini')
      read(7,control)
      print*, syear,smon,sday,start,dirname
      
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
      
chc  heat(n) read heere is used to compute heatflux(n), femis(,,n_heat,)
chc  then both femis(,,n_heat,) and heatflux(n) are notused for plumerise
chc  area(n) in m2
chc  heat(n) in watts
      do n=1,nfire
       read(10,*)year(n),mon(n),day(n),hour(n),min(n),duration(n),
     1  lat(n),lon(n),hgt(n),rate(n),area(n),heat(n)
        heatflux(n)=heat(n)/area(n)
      enddo
      close(10)

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
      
      do m=1,numrec
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
            
      mxrec3d=numrec
      nvars3d=1     

      do L=1,nvars3d
       vname3d(L)='LFRAC'
       units3d(L)='fraction'
       vdesc3d(L)='Model species'
       vtype3d(L)=M3REAL
      enddo 
      print*,mxrec3d,nvars3d
            
      if(.not.open3('OUTPUT1',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT1'
       stop
      endif
      
c      print*,'mxrec3d=',mxrec3d      
      
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
      
      do m=1,numrec
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
      
      do m=1,numrec
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
       do k=1,kmax
        do j=1,jmax
         do i=1,imax
          presf(i,j,k,m)=ptop+vheight(k+1)*(prsfc(i,j,m)-ptop)
         enddo
        enddo
       enddo
                                   
       call nextime(jdate,jtime,jstep)
      enddo      
      
      iflag=close3('MCRO2')       
      
      mxrec3d=numrec
      nvars3d=8

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
               
      if(.not.open3('OUTPUT2',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT2'
       stop
      endif  
      
      if(.not. DESC3('OUTPUT2') ) then
       print*, 'Error getting info from OUTPUT2'
       stop
      endif  
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      total=mxrec3d
      
      print*, jdate,jtime,jstep,total  
      
c      emis(1:imax,1:jmax,1:kmax,1:nvars)=0.0
      femis(1:imax,1:jmax,1:8,1:numrec)=0.0 
      lfrac(1:imax,1:jmax,1:kmax,1:numrec)=0.0      
      grdheatbtu(1:imax,1:jmax,1:numrec)=0.0
               
      if ( flag_write_excel ) then
         write(chrdum,'(''fire_analy_gridindx_'',i4.4,
     1          i2.2,i2.2,''.txt'')')syear,smon,sday
         ii=len(chrdum)
         do jj=1,ii
            if ( chrdum(jj:jj) == ' ') then
               nn=jj-1
               exit
            end if
         end do
         open(88,file=chrdum(1:nn),form='formatted',status='unknown')
         write(88,'(''Fire ID, Grid I index,Grid J index'')')
      end if
      do n=1,nfire       
         if ( flag_write_excel ) then
c            write(chrdum,'(''fire_analy_6xmean_'',i3.3,''.txt'')')n
            write(chrdum,'(''fire_analy_profile_'',i3.3,''.txt'')')n
            ii=len(chrdum)
            do jj=1,ii
               if ( chrdum(jj:jj) == ' ') then
                  nn=jj-1
                  exit
               end if
            end do
            open(99,file=chrdum(1:nn),form='formatted',status='unknown')
            wrtout(2:21,1:numrec)=0.
         end if
       write(chftmp,'(i4.4)')n
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
                
c        print*, latitude(n),longitude(n),acres(n)
        
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
c        print*,pm25co(n)
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
       if ( flag_write_excel ) write(88,'(2(i3,'',''), i3)')n, intx(n), inty(n)
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

chc add diurnal profile
chc find time zone for utc to local hour, produce emis fraction for 0-24 anl hours
        timezone=int(longitude(n)/15.)
        analy_local_hour=start+timezone
        if ( analy_local_hour < 0 ) then
           analy_local_hour = analy_local_hour + 24
        else if ( analy_local_hour > 23 ) then
           analy_local_hour = analy_local_hour - 24
        end if
        analy_local_hour = analy_local_hour - 1
        utc = start - 1
        print *, ' cyc hour = ', start, '  time zone = ', timezone
        do m = 1, numrec
           utc = utc + 1
           if ( utc > 23 ) utc = utc - 24
           analy_local_hour = analy_local_hour + 1
           if(analy_local_hour>23)analy_local_hour=analy_local_hour-24
           write(*,'(''Processing ANALY hour '', i2.2, ''  UTC hour = '',
     1              i4.4, ''Z  Local hour = '', i2.2)') m, utc, analy_local_hour

c         print*, 'PBL at fire spot is',n,m,pbl(intx(n),inty(n),m)
         
         if (fhour.ge.hour(n).and.fhour.le.ftime(n)) then
          intime(n,m)=1
         else
          intime(n,m)=0
           write(*,'('' ==== Fire duration hours from '', i4.4, '' to '', i4.4, 
     1           '' SKIP to next ANALY hour'')') hour(n), ftime(n)
         endif
         
         if(intime(n,m).eq.1) then
          analy_pm_frac   = pm_frac(analy_local_hour)
          analy_heat_frac = heat_frac(analy_local_hour)
C convert hysplit smoke fire emission rate to real rate
c hysplit smoke rate is 6 hours total
c hypslit smoke fire is assumed to be brush buring 
c convert to wildfire 0.121/0.083=1.46 for pm25          
          if ( 1 .eq. 2 ) then
            femis(intx(n),inty(n),n_heat,m)=heat(n)/6.0
           grdheatbtu(intx(n),inty(n),m) = grdheatbtu(intx(n),inty(n),m) +
     1                heatbtu(n)*heat_convert_cnst/6.
            tmpbflx(n,m)=grdheatbtu(intx(n),inty(n),m)
cc            tmpbflx(n,m)=heatbtu(n)/6.0*0.00000258 !convert factor
cc           femis(intx(n),inty(n),n_pm25,m)=pm25(n)/6.0/1.46
           femis(intx(n),inty(n),n_pm25,m)=pm25(n)/6.0
           femis(intx(n),inty(n),n_pm10,m)=pm10(n)/6.0
           femis(intx(n),inty(n),n_pm,m)=pm(n)/6.0
           femis(intx(n),inty(n),n_co,m)=0.0*co(n)/6.0
           femis(intx(n),inty(n),n_co2,m)=0.0*co2(n)/6.0
           femis(intx(n),inty(n),n_ch4,m)=0.0*ch4(n)/6.0
           femis(intx(n),inty(n),n_nmhc,m)=0.0*nmhc(n)/6.0
          else
chc add diurnal profile
           femis(intx(n),inty(n),n_heat,m)=femis(intx(n),inty(n),n_heat,m) +
     1                analy_pm_cnst*analy_heat_frac*heat(n)
c need to store previous data for btu heat
           grdheatbtu(intx(n),inty(n),m) = grdheatbtu(intx(n),inty(n),m) +
     1                analy_pm_cnst*analy_heat_frac*heatbtu(n)*heat_convert_cnst
           tmpbflx(n,m)=grdheatbtu(intx(n),inty(n),m)
           femis(intx(n),inty(n),n_pm25,m)=femis(intx(n),inty(n),n_pm25,m) +
     1                analy_pm_cnst*analy_pm_frac*pm25(n)
           femis(intx(n),inty(n),n_pm10,m)=femis(intx(n),inty(n),n_pm10,m) +
     1                analy_pm_cnst*analy_pm_frac*pm10(n)
           femis(intx(n),inty(n),n_pm,m)=femis(intx(n),inty(n),n_pm,m) +
     1                analy_pm_cnst*analy_pm_frac*pm(n)
           femis(intx(n),inty(n),n_co,m)=femis(intx(n),inty(n),n_co,m) + 
     1                analy_gas_cnst*analy_pm_frac*co(n)
           femis(intx(n),inty(n),n_co2,m)=femis(intx(n),inty(n),n_co2,m) +
     1                analy_gas_cnst*analy_pm_frac*co2(n)
           femis(intx(n),inty(n),n_ch4,m)=femis(intx(n),inty(n),n_ch4,m) +
     1                analy_gas_cnst*analy_pm_frac*ch4(n)
           femis(intx(n),inty(n),n_nmhc,m)=femis(intx(n),inty(n),n_nmhc,m) +
     1                analy_gas_cnst*analy_pm_frac*nmhc(n)
chc add diurnal profile
          end if
          write(*,'(''total Heat = '',E12.5,TR2,'' Frac = '', F8.5,  
     1      TR2, ''hourly Heat = '', E12.5)') heat(n), analy_heat_frac,
     2      femis(intx(n),inty(n),n_heat,m)
          write(*,'(''T BTU Heat = '',E12.5,TR2,'' Frac = '', F8.5,  
     1      TR2, ''hourly BTU H= '', E12.5)') heatbtu(n), analy_heat_frac,
     2      tmpbflx(n,m)
          write(*,'(''total PM   = '',E12.5,TR2,'' Frac = '', F8.5,  
     1      TR2, ''hourly PM   = '', E12.5)') pm25(n), analy_pm_frac,
     2      femis(intx(n),inty(n),n_pm25,m)
           if ( flag_write_excel ) then
              wrtout(5,m)=femis(intx(n),inty(n),n_pm25,m)
              wrtout(6,m)=tmpbflx(n,m)
           end if
          
          hmix(n,m)=pbl(intx(n),inty(n),m)
          psfc(n,m)=prsfc(intx(n),inty(n),m)*0.01 !convert pa to mb
          pres(n,m,0)=psfc(n,m)
          tsfc(n,m)=temp2(intx(n),inty(n),m)
          ustmp(n,m)=max(ustar(intx(n),inty(n),m),0.1) !min value for ustar
C          hfx(n,m)=heatflux(n)/(CP * dens(intx(n),inty(n),1,m))
          hfx(n,m)=mhfx(intx(n),inty(n),m)/
     1     (CP * dens(intx(n),inty(n),1,m))          
c          tmpbflx(n,m)=heatbtu(n)/10.0*0.00000258 !convert factor          
c          tmpbflx(n,m)=heatbtu(n)*4.0*0.00000258 !convert factor          
C          tmpbflx(n,m)=heatbtu(n)/time(n)*60.0*0.00000258 !convert factor
C          tmpbflx(n,m)=heatbtu(n)*0.00000258 !convert factor
                    
c          print*,'ustmp=',n,m,ustmp(n,m),ustar(intx(n),inty(n),m)
C          print*, 'hfx=',n,m,hfx(n,m),heatflux(n),CP,
C     1     dens(intx(n),inty(n),1,m),mhfx(intx(n),inty(n),m)
c          print*,'tmpbflx=',n,m,tmpbflx(n,m),heatbtu(n),time(n)
          
c          print*, n,m,hmix(n,m)

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
c          print*, n,m,hmix(n,m),psfc(n,m),tsfc(n,m),ddzf(n,m,1:35),
c     1     qv(n,m,1:35),ta(n,m,1:35),zf(n,m,1:35),zh(n,m,1:35),
c     2     zstk(n,m,1:35),pres(n,m,0:35),uw(n,m,1:35),vw(n,m,1:35)
          
          call fire_preplm(emlays,hmix(n,m),hts,psfc(n,m),tsfc(n,m),
     1     ddzf(n,m,1:35),qv(n,m,1:35),ta(n,m,1:35),uw(n,m,1:35),
     2     vw(n,m,1:35),zh(n,m,1:35),zf(n,m,1:35),zstk(n,m,1:35),     
     3     pres(n,m,0:35),lstk(n,m),lpbl(n,m),tstk(n,m),wstk(n,m),
     4     dthdz(n,m,1:35),wspd(n,m,1:35),zzf(n,m,0:35))

c          print*, lstk(n,m)
c          print*, lpbl(n,m)
c          print*, tstk(n,m)
c          print*, wstk(n,m)
c          print*, dthdz(n,m,1:35)
c          print*, wspd(n,m,1:35)
c          print*, zzf(n,m,0:35)

c         print*,n,m,emlays,lpbl(n,m),lstk(n,m),hfx(n,m),hmix(n,m),
c     1    tmpbflx(n,m),tstk(n,m),ustmp(n,m),wstk(n,m)
c         print*,dthdz(n,m,1:35),ta(n,m,1:35),wspd(n,m,1:35),
c     1    zzf(n,m,0:35),zh(n,m,1:35),zstk(n,m,1:35)
         

          call fire_plmris(emlays,lpbl(n,m),lstk(n,m),hfx(n,m),
     1     hmix(n,m),tmpbflx(n,m),tstk(n,m),ustmp(n,m),dthdz(n,m,1:35),
     2     ta(n,m,1:35),wspd(n,m,1:35),zzf(n,m,0:35),zh(n,m,1:35),
     3     zstk(n,m,1:35),wstk(n,m),ztop(n,m),zbot(n,m),zplm(n,m))
     
c          print*, 'ztop=',ztop(n,m)
c          print*, 'zbot=',zbot(n,m)
c          print*, 'zplm=',zplm(n,m)
          
          tmpacre(n,m)=acres(n)

c          print*,n,m,emlays,zbot(n,m),ztop(n,m),pres(n,m,0:35),
c     1     zzf(n,m,0:35),ta(n,m,1:35),zh(n,m,1:35),tmpacre(n,m)
               
          call fire_postplm(emlays,zbot(n,m),ztop(n,m),pres(n,m,0:35),
     1     zzf(n,m,0:35),ta(n,m,1:35),zh(n,m,1:35),tmpacre(n,m),
     2     sfract(n,m),ltop(n,m),lbot(n,m),tfrac(n,m,1:35))
     
          print*,'sfract=',sfract(n,m),ltop(n,m)
          print*,'ltopt',ltop(n,m)
          print*,'tfrac=',tfrac(n,m,1:35)
          if ( flag_write_excel ) then
             wrtout(2,m)=float(ltop(n,m))
             wrtout(3,m)=float(lbot(n,m))
             wrtout(4,m)=sfract(n,m)
          end if


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
          if ( flag_write_excel ) then
             idum=6
             do k=1,35
                idum=idum+1
                if ( idum > 41 ) STOP 'idum>41'
                wrtout(idum,m)=femis(intx(n),inty(n),n_pm25,m) * lfrac(intx(n),inty(n),k,m)
             enddo
          end if

          if(tmpsum-1.0.ge.0.01) then
           print*,'the sum of tfrac in not equal to 1.0',tmpsum
           stop
          endif          
c          print*,'tmpsum=',tmpsum
         endif ! withintime
c         print*, n,m,hmix(n,m),psfc(n,m),tsfc(n,m),ddzf(n,m,1:35),
c     1    qv(n,m,1:35),ta(n,m,1:35),zf(n,m,1:35),zh(n,m,1:35),
c     2    zstk(n,m,1:35),pres(n,m,0:35),uw(n,m,1:35),vw(n,m,1:35)

c         print*, lstk(n,m)
c         print*, lpbl(n,m)
c         print*, tstk(n,m)
c         print*, wstk(n,m)
c         print*, dthdz(n,m,1:35)
c         print*, wspd(n,m,1:35)
c         print*, zzf(n,m,0:35)
        
c         print*,m, intime(n,m),hour(n),fhour,ftime(n)
         fhour=fhour+100         
                 
        enddo !numrec
       endif !withindomain
       if ( flag_write_excel ) then
          do m=1,41
             write(99,'(A12,25('','',e12.5))')chr_name(m),(wrtout(m,L), L=1,numrec)
          end do    
          close(99)
       end if
      enddo !nfire
      if ( flag_write_excel ) close(88)
      
      
      do n=1,numrec
       do L=1,nvars3d
        if(.not.WRITE3('OUTPUT2',vname3d(L),jdate,jtime,
     &  femis(1,1,L,n))) then
         print*,' write error in output2', vname3d(L)
         stop
        endif
       enddo       
       call nextime(jdate,jtime,jstep)
C       print*,jdate,jtime,jstep
      enddo         
      
      iflag=close3('OUTPUT2')

      if(.not. DESC3('OUTPUT1') ) then
       print*, 'Error getting info from OUTPUT1'
       stop
      endif 
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
            
      do n=1,numrec
       if(.not.WRITE3('OUTPUT1','LFRAC',jdate,jtime,
     & lfrac(1,1,1,n))) then
        print*,' write error in output1', LFRAC
        stop
       endif      
       call nextime(jdate,jtime,jstep)
C       print*,jdate,jtime,jstep
      enddo      

      iflag=close3('OUTPUT1')
      
      nvars3d=7

      do L=1,nvars3d
       vname3d(L)=firespec(L+1)       
       units3d(L)='kg/hour'               
       vdesc3d(L)='Model species'
       vtype3d(L)=M3REAL
      enddo 
      print*,mxrec3d,nvars3d
               
      if(.not.open3('OUTPUT3',FSCREA3,'aq_open')) then
       print*, 'failed to open OUTPUT3'
       stop
      endif  
      
      if(.not. DESC3('OUTPUT3') ) then
       print*, 'Error getting info from OUTPUT3'
       stop
      endif  
      
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
cc n is analy 0-24 hours
      do n=1,numrec
       emis(1:imax,1:jmax,1:kmax,1:7)=0.0
       do L=1,nvars3d
        do k=1,kmax
         do j=1,jmax
          do i=1,imax
           emis(i,j,k,L)=femis(i,j,L+1,n)*lfrac(i,j,k,n)
          enddo
         enddo
        enddo
        if(.not.WRITE3('OUTPUT3',vname3d(L),jdate,jtime,
     &   emis(1,1,1,L))) then
         print*,' write error in output3', vname3d(L)
         stop
        endif 
       enddo     
       call nextime(jdate,jtime,jstep)
      enddo  
      
      iflag=close3('OUTPUT3')                
      
      end program smoke2cmaq_35layer_analy_1
      

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

        
c        print*, DDZF(1:EMLAYS),QV(1:EMLAYS),TA(1:EMLAYS),ZF(1:EMLAYS),
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
        
c        print*, 'in subroutine'
c        print*, LSTK
c        print*, LPBL
c        print*, TSTK
c        print*, WSTK
c        print*, DTHDZ(1:35)
c        print*, WSPD(1:35)
c        print*, ZZF(0:35)        

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
c        print*, EMLAYS,LPBL,LSTK,HFX,HMIX,BFLX,TSTK,USTAR,WSTK
c        print*, DTHDZ(1:EMLAYS),TA(1:EMLAYS),WSPD(1:EMLAYS),ZF(0:EMLAYS),
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
        
C        print*,'in subroutine'
C        print*,'TOP=',TOP
C        print*,'BOT=',BOT
C        print*,'ZPLM=',ZPLM
      
        RETURN

        END SUBROUTINE FIRE_PLMRIS



        SUBROUTINE FIRE_POSTPLM( EMLAYS, ZBOT, ZTOP, PRESF, ZZF, TA,
     &                           ZH, ACRES, SFRACT, LTOP, LBOT, LFRAC )

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
        REAL   , INTENT(OUT) :: SFRACT           ! smoldering fraction
        INTEGER, INTENT(OUT) :: LTOP             ! plume top layer
        REAL   , INTENT(OUT) :: LFRAC( EMLAYS )  ! layer fractions for source

C...........   Local variables

        INTEGER       L

c        INTEGER       LBOT 
        INTEGER, INTENT(OUT) :: LBOT 

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
c        print*, emlays,zbot,ztop,presf(0:35),zzf(0:35),ta(1:35),
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

C.........  For fire smoldering effects, include fractions below LBOT
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
        
        print*,'== in FIRE_POSTPLM ACRES  = ',ACRES
        print*,'== in FIRE_POSTPLM besize = ',besize
        print*,'== in FIRE_POSTPLM sfract = ',sfract
        print*,'== in FIRE_POSTPLM lfrac  = ',lfrac(1:35)
        print*,'== in FIRE_POSTPLM presf  = ',presf(0:35)
        do L = 1, emlays
           sum = presf(L-1) - presf(L)
           write(*,'(i2, 2('','', e12.5))') L, sum, presf(L-1)
        end do
        write(*,'(i2, '',,'', e12.5)') L, presf(emlays)
           
        print*,'== in FIRE_POSTPLM ltop   = ',ltop
        print*,'== in FIRE_POSTPLM lbot   = ',lbot

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I7, :, 1X ) )

        END SUBROUTINE FIRE_POSTPLM
