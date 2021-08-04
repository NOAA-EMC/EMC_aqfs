!---convert GBBEPx emission to CMAQ Point source
! Author: Youhua Tang   Apr, 2019
      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API
      include 'netcdf.inc'
      
      integer, parameter :: nspecies=50,nhms=7
      integer startdate,secsdiff
      logical hgtin,em24hr_ext    ! whether extend the wildfire spots >=24 hr to longer period
      real tdiurnal(24),tfraction(72),dfrac(3)
      character aline*200,prefix*200,suffix*80,efilein*200
      character*16 emname(nspecies),hmsname(nhms),ctmp,echar(nspecies),
     1 gdnam
      integer istart3d(3),icount3d(3)
      data istart3d/1,1,1/
     
      data hmsname/'CO','NOx','SO2','NH3','OC','BC','PM2.5'/ 
      data dfrac/1.,1.,1./  ! daily fraction
      
      double precision, allocatable :: emhold(:,:)
      integer, allocatable :: jday_fire(:),jtime_fire(:),irow(:),icol(:)    ! file starting date/time
     1 ,ifip(:),lmajor(:),lping(:),istack(:)
      real,allocatable ::sfact(:,:),val(:),fireemis(:,:,:),lwmask(:,:),
     1 fireduration(:),firearea(:),hflux(:,:),firelat(:),firelon(:),
     2 xloca(:),yloca(:),badval(:),lufrac(:,:,:),gbbepx(:,:,:),frp(:,:)
     3 ,xlat(:,:),xlon(:,:),forestfrac(:,:),tlon(:),tlat(:),frpout(:,:)
      namelist /control/efilein,markutc,burnarea_ratio,frpfactor,  ! frp factor to hflux
     1 startdate,nduration,tdiurnal,dfrac,emname

      data r_earth/6370997./

      piconv=atan(1.)*4/180.  ! degree to radian
      
      call aq_blank(16*nspecies,emname)
       
      open(7,file='gbbepx2pts.ini',status='old')
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
      
      allocate(sfact(nhms,nemis),val(nemis))
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
      
      call envstr('GRID_NAME','GRID name','CON_12',gdnam,ios)
      IF(.NOT.LAMBERT(GDNAM, P_ALP3d, P_BET3d, P_GAM3d,
     1       XCENT3D, YCENT3D )) stop
      allocate(xlat(imax,jmax),xlon(imax,jmax),lufrac(imax,jmax,24),  ! USGS 24 category landuse fractions
     1  forestfrac(imax,jmax),lwmask(imax,jmax)) 
      if(.not.read3('TOPO','LAT',1,sdate3d,stime3d,xlat)) stop
      if(.not.read3('TOPO','LON',1,sdate3d,stime3d,xlon)) stop
      xlatmin=minval(xlat)
      xlatmax=maxval(xlat)
      xlonmin=minval(xlon)
      xlonmax=maxval(xlon)
      if(.not.read3('TOPO','LWMASK',1,sdate3d,stime3d,lwmask)) stop
      do i=1,24
       write(ctmp,"('LUFRAC_',i2.2)")i
       if(.not.read3('TOPO',trim(ctmp),1,sdate3d,stime3d,
     1  lufrac(1,1,i))) stop
      enddo 
      do i=1,imax
       do j=1,jmax
       forestfrac(i,j)=lufrac(i,j,11)   ! Deciduous Broadleaf Forest
     1    +  lufrac(i,j,12)              ! Deciduous Needleleaf Forest
     2    +  lufrac(i,j,13)              ! Evergreen Broadleaf Forest
     3    +  lufrac(i,j,14)              ! Evergreen Needleleaf Forest
     4    +  lufrac(i,j,15)              ! Mixed Forest
!     5    +  lufrac(i,j,8)               ! shrubland
       enddo
      enddo
       
!   gbbepx file input
      print*,'read gbbepx file'
      iflag=nf_open(trim(efilein),NF_NOWRITE,id_file) ! open file
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      print*,'id_file=',id_file
      iflag=nf_inq_dimid(id_file,'Latitude',id_nlat)        ! dimension ID
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      iflag=nf_inq_dimlen(id_file,id_nlat,nlat)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      
      iflag=nf_inq_dimid(id_file,'Longitude',id_nlon)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      iflag=nf_inq_dimlen(id_file,id_nlon,nlon)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      
      allocate(tlon(nlon),tlat(nlat))
      
c      iflag=nf_inq_varid(id_file,'Time',id_time)
c      iflag=nf_get_var_real(id_file,id_time,atime)
c      if(iflag.ne.NF_NOERR) stop  
          
      iflag=nf_inq_varid(id_file,'Latitude',id_lat)
      iflag=nf_get_var_real(id_file,id_lat,tlat)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      tlatint=(tlat(nlat)-tlat(1))/(nlat-1)
      
      iflag=nf_inq_varid(id_file,'Longitude',id_lon)
      iflag=nf_get_var_real(id_file,id_lon,tlon)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      tlonint=(tlon(nlon)-tlon(1))/(nlon-1)
      
      icount3d(1)=nlon
      icount3d(2)=nlat
      icount3d(3)=1
      
      allocate(emhold(nlon,nlat),gbbepx(nlon,nlat,nhms),frp(nlon,nlat))
      print*,'nlon,nlat=',nlon,nlat
      do i=1,nhms
       print*,'read ',hmsname(i)
       iflag=nf_inq_varid(id_file,trim(hmsname(i)),id_tmp)
       if(iflag.ne.NF_NOERR) call handle_err(iflag)
      iflag=nf_get_vara_double(id_file,id_tmp,istart3d,icount3d,emhold)
       if(iflag.ne.NF_NOERR) call handle_err(iflag)
       gbbepx(1:nlon,1:nlat,i)=sngl(emhold(1:nlon,1:nlat))
      enddo
      iflag=nf_inq_varid(id_file,'MeanFRP',id_tmp)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      iflag=nf_get_vara_double(id_file,id_tmp,istart3d,icount3d,emhold)
      if(iflag.ne.NF_NOERR) call handle_err(iflag)
      frp(1:nlon,1:nlat)=sngl(emhold(1:nlon,1:nlat))
      iflag=nf_close(id_file)
      
!! start processing

      nfiles=imax*jmax*2  ! maximum of locations for 0.1x0.1 inside the 5x domain
      
      allocate( jday_fire(nfiles),
     1 jtime_fire(nfiles),fireemis(nfiles,nemis,nduration),
     2 firearea(nfiles),hflux(nfiles,nduration),firelat(nfiles),
     3 firelon(nfiles),xloca(nfiles),yloca(nfiles),irow(nfiles),
     4 icol(nfiles),ifip(nfiles),frpout(nfiles,nduration))
      
      firearea(:)=0
      fireemis(:,:,:)=0.
      hflux(:,:)=0.
      frpout(:,:)=0.
      print*,'start processing'
      mfire=1
      do i=1,nlon
       jloop: do j=1,nlat
       
       if(tlon(i).ge.xlonmin.and.tlon(i).le.xlonmax.and.
     1    tlat(j).ge.xlatmin.and.tlat(j).le.xlatmax.and.
     2     maxval(gbbepx(i,j,:)).gt.0) then

       if(.not.LL2LAM(tlon(i),tlat(j),x,y)) stop
       xind=(x-xorig)/ddx+0.5
       yind=(y-yorig)/ddx+0.5
c       print*,'mfire,i,j,tlon(i),tlat(j),xind,yind=',
c     1    mfire,i,j,tlon(i),tlat(j),xind,yind,gbbepx(i,j,:)
       if(xind.gt.imax.or.xind.lt.1.or.yind.gt.jmax.or.yind.lt.1) cycle jloop  ! out of domain
       if( frp(i,j).le.0.or.lwmask(xind,yind).lt.0.5 ) cycle jloop  ! no FRP or over water

       xloca(mfire)=x
       yloca(mfire)=y
       icol(mfire)=nint(xind)
       irow(mfire)=nint(yind)
       
       tlon1=tlon(i)-0.5*tlonint
       tlon2=tlon(i)+0.5*tlonint
       tlat1=tlat(j)-0.5*tlatint
       tlat2=tlat(j)+0.5*tlatint
       tarea=r_earth**2*abs(sin(tlat1*piconv)-
     1 sin(tlat2*piconv))*abs(tlon1-tlon2)*piconv    ! area in m2

       firearea(mfire)=tarea/4046.86*burnarea_ratio                            ! acres/day
       
       firelon(mfire)=tlon(i)
       firelat(mfire)=tlat(j)
       ifip(mfire)=88880+13+int(tlon(i)/15.-0.5)
       
       lst=jhour+nint(tlon(i)/15)
       if(lst.lt.0) lst=lst+24
       if(lst.ge.24) lst=lst-24
       
       marklst=markutc+nint(tlon(i)/15)  ! gbbepx marking local time 
       if(marklst.lt.0) marklst=marklst+24
       if(marklst.ge.24) marklst=marklst-24
             
       mtime: do m=1,nduration
c       tratio=tdiurnal(lst+1)/tdiurnal(marklst)*dfrac(min(m/24+1,3))
        tratio=tdiurnal(lst+1)/maxval(tdiurnal)*dfrac(min(m/24+1,3))   ! assume the  VIIRS detection is the maximum during the day
        if(tratio.ge.40) then
	  print*,'tratio too big ',tratio,lst,jhour,markutc,marklst,
     1	    tdiurnal(lst+1),tdiurnal(marklst),dfrac(min(m/24+1,3))
          stop
	else if (tratio.lt.1e-6) then  
	 print*,'tratio is too small ',tratio,lst,jhour,markutc,
     1	 marklst,tdiurnal(lst+1),tdiurnal(marklst),dfrac(min(m/24+1,3))
         stop
	endif
c       print*,'m,tratio=',m,tratio,tdiurnal(lst+1),dfrac(min(m/24+1,3)),
c    1	 forestfrac(nint(xind),nint(yind)) 
        if(frp(i,j).gt.0) then
	 hflux(mfire,m)=frp(i,j)*947.82*frpfactor*tratio    ! MW -> BTU/s
	 frpout(mfire,m)=frp(i,j)*tratio
        endif
       
       if((tlon(i).ge.-100.or.forestfrac(nint(xind),nint(yind)).le.0.4)
     1   .and.m.gt.10)  exit mtime ! skip non-forest fire 
	
         do L1=1,nhms
	  do L2=1,nemis
          fireemis(mfire,L2,m)=fireemis(mfire,L2,m)+gbbepx(i,j,L1)*
     1	    sfact(L1,L2)*tarea*tratio
	  enddo
	 enddo 
	 lst=lst+1
	 if(lst.ge.24) lst=lst-24
       enddo mtime
c        print*,'mfire,i,j,tlon(i),tlat(j),xind,yind,frp(i,j)=',
c     1  mfire,i,j,tlon(i),tlat(j),xind,yind,frp(i,j),hflux(mfire,1),
c     2  tratio
       mfire=mfire+1   ! actual fire inside of domain
       endif
       enddo jloop
      enddo
      mfire=mfire-1
      print*,'total fire points',mfire

      if(mfire.eq.0) then
        print*,'fill empty fire with zero emission'
	mfire=1
	firelat(1)=xlatmin
	firelon(1)=xlonmin
	if(.not.LL2LAM(xlonmin,xlatmin,x,y)) stop
        irow(1)=(x-xorig)/ddx+0.5
        icol(1)=(y-yorig)/ddx+0.5
       endif 
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
      nvars3d=nemis+2
      vname3d(1:nemis)=emname(1:nemis)
      vdesc3d(1:nemis)=emname(1:nemis)
      vtype3d(1:nvars3d)=M3REAL

      vname3d(nemis+1)='HFLUX'
      vdesc3d(nemis+1)='HFLUX'
      units3d(nemis+1)='BTU/s'
      
      vname3d(nvars3d)='FRP'
      vdesc3d(nvars3d)='Fire Radiactive Power'
      units3d(nvars3d)='MW'

      do L=1,nvars3d-2
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
       if(.not.write3('PTFIRE','FRP',jsdate,jstime,frpout(1,n))) stop
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

      subroutine handle_err(iflag)
      character*80 NF_STRERROR
      print*,'error occurred in netcdf ',iflag, NF_STRERROR(iflag)
      stop
      end
