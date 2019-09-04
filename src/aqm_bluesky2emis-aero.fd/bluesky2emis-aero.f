!---convert HMS-Blueksy V1 emission to CMAQ Point source
! Author: Youhua Tang   Oct,2015   
      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      integer, parameter :: nspecies=50,nhms=3
      integer startdate,secsdiff
      logical hgtin,em24hr_ext    ! whether extend the wildfire spots >=24 hr to longer period
      real tdiurnal(24),tfraction(72),dfrac(3)
      character aline*200,prefix*200,suffix*80,emtimefile*200,if_fcst*3
      character*16 emname(nspecies),hmsname(nhms),ctmp,echar(nspecies),
     1 gdnam
     
      data hmsname/'pm25','pmcoarse','co'/ !'ch4','nmhc'/
      data dfrac/1.,1.,1./  ! daily fraction
      
      integer, allocatable :: jday_fire(:),jtime_fire(:),irow(:),icol(:)    ! file starting date/time
     1 ,ifip(:),lmajor(:),lping(:),istack(:)
      real,allocatable :: sfact(:,:),val(:),fireemis(:,:,:),
     1 fireduration(:),firearea(:),hflux(:,:),firelat(:),firelon(:),
     2 xloca(:),yloca(:),badval(:)    
      
      namelist /control/prefix,suffix,nfiles,emtimefile,em24hr_ext,
     1 startdate,nduration,tdiurnal,dfrac,emi_factor_wf,emi_factor_rx,
     2 if_fcst,emname
      
      call aq_blank(16*nspecies,emname)
       
      open(7,file='bluesky2emis-aero.ini',status='old')
      read(7,control)
      call aq_find(nspecies,' ',emname,lpsec,iflag)
      if(iflag.eq.0) then
       nemis=lpsec-1
      else
       print*,'emname wrong ', lpsec
       stop
      endif
      
      jhour=mod(startdate,100)
      jdate=mod(startdate/100,100)
      jmonth=mod(startdate/10000,100)
      jyear=startdate/1000000

      jsdate=jyear*1000+julian(jyear,jmonth,jdate)  ! YYYYDDD
      jstime=jhour*10000
      print*,'jsdate,jstime=',jsdate,jstime,jyear
      
      allocate(sfact(nhms,nemis),val(nemis),jday_fire(nfiles),
     1 jtime_fire(nfiles),fireemis(nfiles,nemis,nduration),
     2 firearea(nfiles),hflux(nfiles,nduration),firelat(nfiles),
     3 firelon(nfiles),xloca(nfiles),yloca(nfiles),irow(nfiles),
     4 icol(nfiles),ifip(nfiles))

      fireemis(:,:,:)=0.
      hflux(:,:)=0.
      sfact(:,:)=0.
      call aq_locate(7,'Species Converting Factor',iflag)
      if(iflag.ne.0) then
       print*,'can not find Species Converting Factor'
       stop
      endif
      do while(.true.)
       call aq_readhd(7)
       read(7,*,end=98,err=99) ctmp,num
       call aq_find(nhms,ctmp,hmsname,lpsec1,iflag)
       if(iflag.eq.0) then
        read(7,*)(echar(i),val(i),i=1,num)
        do i=1,num
         call aq_find(nemis,echar(i),emname,lpsec2,iflag)
         if(iflag.eq.0) sfact(lpsec1,lpsec2)=val(i)
        end do
       endif
      print*,' Converting factor for ',hmsname(lpsec1),' is ',
     1 (sfact(lpsec1,lm),lm=1,nemis)
 99   continue
      enddo
 98   close(7)
      
      if(.not.open3('TOPO',FSREAD3,'pathway')) then   
       print*,' Error open TOPO file'
       stop
      endif
      if(.not.desc3('TOPO')) STOP
      imax=ncols3d
      jmax=nrows3d
      ddx=sngl(xcell3d)
      xorig=sngl(xorig3d)
      yorig=sngl(yorig3d)
      
c      if(index(gdnam3d,'CON_12').ge.1.or.
c     1   index(gdnam3d,'AQF_CONU').ge.1) then
c        gdnam='CON_12'
c      else if (index(gdnam3d,'COL_04').ge.1) then
c        gdnam='COL_04'
c      else
c       print*,'unknown grid ',gdnam3d
c       stop
c      endif
      call envstr('GRID_NAME','GRID name','CON_12',gdnam,ios)
      IF(.NOT.LAMBERT(GDNAM, P_ALP3d, P_BET3d, P_GAM3d,
     1       XCENT3D, YCENT3D )) stop      

!-- EMTIMES file
      open(7,file=trim(emtimefile),status='old')
      read(7,*)
      read(7,'(a200)')aline
      if(index(aline,'HGT(m)').ge.1) then
       hgtin=.true.
      else
       hgtin=.false.
      endif  
      read(7,*)iyear,imonth,idate,ihour,iduration,irecords
      if(irecords.ne.nfiles) then
       print*,'inconsistent file numbers ',nfiles,
     1  iyear,imonth,idate,ihour,iduration,irecords
       print*,'will process as is '
      endif
      
      mfire=1
      noaafirefile=1
      do n=1,min(nfiles,irecords)
       if(hgtin) then
       read(7,*)iyear,imonth,idate,ihour,iminutes,iduration,flat,
     1   flon,hgt,frate,farea,fheat
       else
       read(7,*)iyear,imonth,idate,ihour,iminutes,iduration,flat,
     1   flon,frate,farea,fheat
       endif
!       print*,'read emtime file ',n,nfiles,flat,flon,iduration
!   NOAA fire files
       
 11    write(ctmp,'(i4.4)')noaafirefile
       open(8,file=trim(prefix)//ctmp(1:4)//trim(suffix),status='old')
       read(8,*)
       read(8,'(a200)')aline
       read(aline(17:200),*)flat2
       read(8,'(a200)')aline
       read(aline(17:200),*)flon2
       if(abs(flat-flat2).ge.1e-5.or.abs(flon-flon2).ge.1e-5) then
        print*,' inconsistent lat/lon n1,n2,flat,flat2,flon,flon2=',
     1	 n, noaafilefile, flat,flat2,flon,flon2
        close(8)
	noaafirefile=noaafirefile+1
	goto 11
       endif	
       read(8,*)
       read(8,'(a200)')aline
       read(aline(17:200),*)farea2
       do i=1,9
        read(8,*)
       enddo
       read(8,'(a200)')aline
       if(index(aline,'time, heat, pm25, pm10, pm, co, co2, ch4, nmhc,')
     1  .lt.1.and.index(aline,
     2  'time_period, total_heat, total_pm25, total_pm10, total_pm,'//
     3  ' total_co, total_co2, total_ch4, total_nmhc,').lt.1) then
        print*,'file header',n,' error'
	print*,aline   
	stop
       endif
       read(8,*)
       read(8,*)fminute2,fheat2,fpm25,fpm10,fpm,fco,fco2,fch4,fnmhc
       close(8)
       noaafirefile=noaafirefile+1
       
       if(.not.LL2LAM(flon,flat,x,y)) stop
       xind=(x-xorig)/ddx+0.5
       yind=(y-yorig)/ddx+0.5
       
       print*,'process file ',n,nfiles,flat,flon,iduration
       
       if(xind.gt.imax.or.xind.lt.1.or.yind.gt.jmax.or.yind.lt.1) cycle ! out of domain
       if(if_fcst.eq.'YES'.and.iduration.lt.2400) cycle   ! short fire 
       if(iduration.lt.2400) then
         emi_factor=emi_factor_rx
       else
	 emi_factor=emi_factor_wf
       endif	 
       xloca(mfire)=x
       yloca(mfire)=y
       icol(mfire)=nint(xind)
       irow(mfire)=nint(yind)
       firearea(mfire)=farea2 ! acres/day
       firelon(mfire)=flon
       firelat(mfire)=flat
       ifip(mfire)=88880+13+int(flon/15.-0.5)
       
       lst=nint(ihour+iminutes/60.+flon/15.)              ! local time
       if(lst.lt.0) lst=lst+24
       if(lst.ge.24) lst=lst-24
       
       tfraction(1:72)=0.
       do mtime=1,iduration/100
        tfraction(mtime)=tdiurnal(lst+1)*dfrac(mtime/24+1)
	lst=lst+1
	if(lst.ge.24) lst=lst-24
       enddo
       mtime=mtime-1
       sumfraction=sum(tfraction)
       tfraction(1:mtime)=tfraction(1:mtime)/sumfraction  ! hourly fraction

       timediff=secsdiff(jsdate,jstime,iyear*1000+
     1  julian(iyear,imonth,idate),ihour*10000+iminutes*100)/3600.   ! time difference in fractional hour
       if(timediff.lt.0) then
        print*,'fire time and start time wrong ',jsdate,jstime,iyear,
     1	imonth,idate,ihour,timediff
        stop
       endif

       if(timediff.ge.1) then
         hflux(mfire,1:int(timediff))=0.
	 fireemis(mfire,1:nemis,1:int(timediff))=0.
       endif
       
       do ktime=1,mtime
        ktindex=ktime+int(timediff)
        hflux(mfire,ktindex)=fheat2*tfraction(ktime)/3600.  ! BTU/hr -> BTU/s
	do L=1,nemis
         fireemis(mfire,L,ktindex)=fireemis(mfire,L,ktindex)+    ! PM25 kg/hr -> g/s
     1	   fpm25*1000/3600*sfact(1,L)*tfraction(ktime)*emi_factor
         fireemis(mfire,L,ktindex)=fireemis(mfire,L,ktindex)+    ! PMcoarse kg/hr -> g/s
     1	   amax1(fpm10-fpm25,0.)*1000/3600*sfact(2,L)*tfraction(ktime)
     2     *emi_factor
         fireemis(mfire,L,ktindex)=fireemis(mfire,L,ktindex)+    ! CO kg/hr -> moles/s
     1	   fco*1000/3600*sfact(3,L)*tfraction(ktime)*emi_factor
!         fireemis(mfire,L,ktindex)=fireemis(mfire,L,ktindex)+    ! CH4 kg/hr -> moles/s
!     1	   fch4*1000/3600*sfact(4,L)*tfraction(ktime)*emi_factor
!         fireemis(mfire,L,ktindex)=fireemis(mfire,L,ktindex)+    ! NMHC kg/hr -> moles/s
!     1	   fnmhc*1000/3600*sfact(5,L)*tfraction(ktime)*emi_factor
        enddo
       enddo

       do ktime=ktindex+1,nduration
        if(mtime.ge.24.and.em24hr_ext) then   ! longer than 24 hours
         fireemis(mfire,1:nemis,ktime)=fireemis(mfire,1:nemis,ktindex)
	 hflux(mfire,ktime)=hflux(mfire,ktindex)
	else
	 fireemis(mfire,1:nemis,ktime)=0.
	 hflux(mfire,ktime)=0.	
	endif
       enddo	 

       mfire=mfire+1   ! actual fire inside of domain       
       
      enddo
      close(7)
      print*,'mfire=',mfire
!---stack file header
      allocate(badval(mfire),istack(mfire),lmajor(mfire),lping(mfire))
      lmajor(:)=1
      lping(:)=0
      badval(:)=-9.999E36
      do i=1,mfire
       istack(i)=i
      enddo
      
      ncols3d=1
      nrows3d=mfire  ! fire counts
      nlays3d=1
       
      nvars3d=17
      sdate3d=jsdate
      stime3d=jstime
      tstep3d=10000

      vname3d(1)='ISTACK'
      vtype3d(1)=M3INT
      vdesc3d(1)='Stack group number'
      units3d(1)='none'
      
      vname3d(2)='LATITUDE'
      vtype3d(2)=M3REAL
      vdesc3d(2)='LATITUDE'
      units3d(2)='degrees'
      
      vname3d(3)='LONGITUDE'
      vtype3d(3)=M3REAL
      vdesc3d(3)='LONGITUDE'
      units3d(3)='degrees'

      vname3d(4)='STKDM'
      vtype3d(4)=M3REAL
      vdesc3d(4)='Inside stack diameter'
      units3d(4)='m'
      
      vname3d(5)='STKHT'
      vtype3d(5)=M3REAL
      vdesc3d(5)='Stack height above ground surface'
      units3d(5)='m'
      
      vname3d(6)='STKTK'
      vtype3d(6)=M3REAL
      vdesc3d(6)='Stack exit temperature'
      units3d(6)='degrees K'
      
      vname3d(7)='STKVE'
      vtype3d(7)=M3REAL
      vdesc3d(7)='Stack exit velocity'
      units3d(7)='m/s'
            
      vname3d(8)='STKFLW'
      vtype3d(8)=M3REAL
      vdesc3d(8)='Stack exit flow rate'
      units3d(8)='m**3/s'
      
      vname3d(9)='STKCNT'
      vtype3d(9)=M3INT
      vdesc3d(9)='Number of stacks in group'
      units3d(9)='none'
      
      vname3d(10)='ROW'
      vtype3d(10)=M3INT
      vdesc3d(10)='Grid row number'
      units3d(10)='none'
      
      vname3d(11)='COL'
      vtype3d(11)=M3INT
      vdesc3d(11)='Grid column number'
      units3d(11)='none'

      vname3d(12)='XLOCA'
      vtype3d(12)=M3REAL
      vdesc3d(12)='Projection x coordinate'
      units3d(12)='m'
      
      vname3d(13)='YLOCA'
      vtype3d(13)=M3REAL
      vdesc3d(13)='Projection x coordinate'
      units3d(13)='m'

      vname3d(14)='IFIP'
      vtype3d(14)=M3INT
      vdesc3d(14)='FIPS CODE'
      units3d(14)='none'
      
      vname3d(15)='LMAJOR'
      vtype3d(15)=M3INT
      vdesc3d(15)='1= MAJOR SOURCE in domain, 0=otherwise'
      units3d(15)='none'

      vname3d(16)='LPING'
      vtype3d(16)=M3INT
      vdesc3d(16)='1=PING SOURCE in domain, 0=otherwise'
      units3d(16)='none'
      
      vname3d(17)='ACRESBURNED'
      vtype3d(17)=M3REAL
      vdesc3d(17)='number of acres burned for a fire in one day'
      units3d(17)='acres/day'
      
      if(.not.open3('STACK_GROUP',FSUNKN3,'Youhua')) stop
      if(.not.write3('STACK_GROUP','ISTACK',jsdate,jstime,istack)) stop
      if(.not.write3('STACK_GROUP','LATITUDE',jsdate,jstime,firelat)) 
     1  stop
      if(.not.write3('STACK_GROUP','LONGITUDE',jsdate,jstime,firelon)) 
     1  stop
      if(.not.write3('STACK_GROUP','STKDM',jsdate,jstime,badval)) stop
      if(.not.write3('STACK_GROUP','STKHT',jsdate,jstime,badval)) stop
      if(.not.write3('STACK_GROUP','STKTK',jsdate,jstime,badval)) stop 
      if(.not.write3('STACK_GROUP','STKVE',jsdate,jstime,badval)) stop      
      if(.not.write3('STACK_GROUP','STKFLW',jsdate,jstime,badval)) stop
      if(.not.write3('STACK_GROUP','STKCNT',jsdate,jstime,lmajor)) stop
      if(.not.write3('STACK_GROUP','ROW',jsdate,jstime,irow)) stop
      if(.not.write3('STACK_GROUP','COL',jsdate,jstime,icol)) stop
      if(.not.write3('STACK_GROUP','XLOCA',jsdate,jstime,xloca)) stop
      if(.not.write3('STACK_GROUP','YLOCA',jsdate,jstime,yloca)) stop
      if(.not.write3('STACK_GROUP','IFIP',jsdate,jstime,ifip)) stop
      if(.not.write3('STACK_GROUP','LMAJOR',jsdate,jstime,lmajor)) stop
      if(.not.write3('STACK_GROUP','LPING',jsdate,jstime,lping)) stop
      if(.not.write3('STACK_GROUP','ACRESBURNED',jsdate,jstime,
     1  firearea)) stop
      iflag=close3('STACK_GROUP')
      
!----hourly emission files
      nvars3d=nemis+1
      vname3d(1:nemis)=emname(1:nemis)
      vdesc3d(1:nemis)=emname(1:nemis)
      vtype3d(1:nvars3d)=M3REAL

      vname3d(nvars3d)='HFLUX'
      vdesc3d(nvars3d)='HFLUX'

      do L=1,nvars3d
       if(vname3d(L)(1:1).eq.'P'.and.vname3d(L).ne.'PAR') then ! aerosol
        units3d(L)='g/s'
       else
        units3d(L)='moles/s'
       endif
      enddo
      if(.not.open3('PTFIRE',FSUNKN3,'Youhua')) stop 
      
      do n=1,nduration
       do L=1,nemis
        if(.not.write3('PTFIRE',vname3d(L),jsdate,jstime,
     1   fireemis(1,L,n))) stop
       enddo 
       if(.not.write3('PTFIRE','HFLUX',jsdate,jstime,hflux(1,n))) stop
       call nextime(jsdate,jstime,tstep3d)
      enddo
      iflag=shut3()
      end 	
      subroutine aq_blank(ntot,y)
c***********************************************************************
      character*1 y(ntot)
      do i=1,ntot
      y(i)=' '
      enddo
      return
      end

      subroutine aq_locate(iunit,char,iflag)
c***********************************************************************
      character*(*) char
      character*80 dum1
      nchar=len(char)
      iflag=0
      do iter=1,10000
      read(iunit,'(a)',end=98) dum1(1:nchar)
c      print*,'dum1= ',dum1(1:nchar)
      if(dum1(1:nchar).eq.char) return
      enddo
98    iflag=1
c      print*,'dum1= ',dum1(1:nchar)
      return
      end

c**********************************************************************
      subroutine aq_find(num,cdum1,sname,lpsec,iflag)
c***********************************************************************
      dimension sname(1)
      character*(*) sname,cdum1
      iflag=0
      do l=1,num
       if(cdum1.ne.sname(l)) cycle
       lpsec=l
       return
      enddo 
      iflag=1
      return
      end

      subroutine aq_readhd(iunit)
c***********************************************************************
      character*1 char(80)
       do iter=1,1000
	read(iunit,100,end=8)  (char(i),i=1,80)
	if(char(1).eq.'$') then
	  write(6,200) iunit,(char(i),i=1,80)
        else if(char(1).eq.'#') then
	else
	  backspace iunit
	  return
        endif
       end do
 8    continue
100   format(80a1)
200   format(2x,'iunit=',i3,2x,80a1)
      end
