      program gocart_bnd
!-------------------------------------------------------------------------------------      
!   interpolate GOCART Concentration to be lateral boundary condition for regional 
!   air quality model,  also output a layer result for checking purpose
!
!   for GOCART output in NEMS-IO format 
!
!   Author: Youhua Tang
!   Revisions
!-------------------------------------------------------------------------------------
      
      use nemsio_module

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      parameter(maxfile=300,nspecies=100,ngocart=10)

      real sfact(ngocart,nspecies),val(nspecies),  &
         checkfact(ngocart,nspecies)

      real,allocatable  :: glon(:,:), glat(:,:),tmpa(:)
      real,allocatable  :: pgocart(:,:,:),vgocart(:,:,:), press(:,:,:),  &
       worka(:),workb(:),workc(:), work(:), work2(:), work3(:), xlat(:,:), xlon(:,:), bnd(:,:,:),    &
       tmpbnd(:,:,:),bndcoord(:,:,:), checkcoord(:,:,:),checksp(:,:,:), &
       airgocart(:,:,:),tgocart(:,:,:)
      
      character bndname(nspecies)*16,gocartname(ngocart)*8,ctmp*16,  &
       echar(nspecies)*16,mofile(2)*200,checkname(nspecies)*16,     &
       aline*200,gdatatype*4,modelname*4,gtype*16
     
      integer netindex(ngocart),checklayer,modate(maxfile),         &
       mosecs(maxfile),julian,ismotime(maxfile),iemotime(maxfile),  &
       idate(7),tlmeta,iret
      logical ingocart,lflag,extrameta,indexfind(nspecies)
           
      data gocartname/'o3mr','dust1','dust2','dust3','dust4','dust5','bc1','bc2','oc1','oc2'/
       
      data indexfind/nspecies*.false./
      
      data iprint/0/
      
      type(nemsio_gfile) :: gfile

      integer  begyear,begdate,begtime,dtstep,numts      
      namelist /control/begyear,begdate,        &
       begtime,bndname,dtstep,numts,mofile,	&	  !  input file preffix and suffix
       checkname,checklayer
      
      
      call aq_blank(16*nspecies,bndname)
      call aq_blank(16*nspecies,checkname)

      sfact(1:ngocart,1:nspecies)=0.
      checkfact(1:ngocart,1:nspecies)=0.
! read converting information

      open(7,file='ngac-bnd-nemsio.ini')
      read(7,control)
      nowdate=begyear*1000+begdate   ! YYYYDDD
      nowtime=begtime*10000          ! HHMMSS      
      call aq_find(nspecies,' ',bndname,lpsec,iflag)   ! BND species
      noutbnd=lpsec-1
      call aq_find(nspecies,' ',checkname,lpsec,iflag)   ! BND species
      ncheck=lpsec-1

      call aq_locate(7,'Species converting Factor',iflag)      

      do while(.true.)
       call aq_readhd(7)
       read(7,*,end=98,err=99) ctmp,num
       call aq_find(ngocart,ctmp,gocartname,lpsec1,iflag)
       if(iflag.eq.0) then
        read(7,*)(echar(i),val(i),i=1,num)
	do i=1,num
	 call aq_find(noutbnd,echar(i),bndname,lpsec2,iflag)
	 if(iflag.eq.0) then
	  sfact(lpsec1,lpsec2)=val(i)
	  indexfind(lpsec2)=.true.
	 endif 
	 call aq_find(ncheck,echar(i),checkname,lpsec2,iflag)
	 if(iflag.eq.0) checkfact(lpsec1,lpsec2)=val(i)   
	end do
       endif 
      print*,' Converting factor for ',gocartname(lpsec1),' is ', &
      (sfact(lpsec1,lm),lm=1,noutbnd) 
 99   continue
      enddo 
 98   close(7)

      
      if(.not.open3('TOPO',FSREAD3,'epscreat')) then
       print*, 'failed to open TOPO'
       stop
      endif
      if(.not. DESC3('TOPO')) stop
      imax=ncols3d
      jmax=nrows3d
      lenbnd=imax*2+jmax*2+4
      
      allocate(xlat(imax,jmax))
      allocate(xlon(imax,jmax))
      allocate(checksp(imax,jmax,nspecies))
      allocate(checkcoord(imax,jmax,3))
      
      if(.not.READ3('TOPO','LAT',ALLAYS3, 0, 0, xlat)) then
        print*, 'Error in reading Latitude'
	stop
      endif
      if(.not.READ3('TOPO','LON',ALLAYS3, 0, 0, xlon)) then
        print*, 'Error in reading Latitude'
	stop
      endif
      do i=1,imax
       do j=1,jmax
       if(xlon(i,j).lt.0) xlon(i,j)=xlon(i,j)+360
       enddo
      enddo 
      					      
      if(.not.open3('METEO3D',FSREAD3,'pathway')) then   
       print*,' Error open METEO3D file'
       stop
      endif
!      if(.not.open3('METEO3D2',FSREAD3,'pathway')) then
!       print*,' Error open METEO3D2 file'
!       stop
!      endif
      if(.not. DESC3('METEO3D')) stop
      if(imax.ne.ncols3d.or.jmax.ne.nrows3d) then
       print*,'meteo3d dimension does not match'
       stop
      endif

      kmax=nlays3d
      allocate(press(imax,jmax,kmax))
      allocate(bndcoord(lenbnd,kmax,3))
      allocate(bnd(lenbnd,kmax,nspecies))
      allocate(tmpbnd(lenbnd,kmax,nspecies))      
      
      metstime=sdate3d*100+stime3d/10000                ! met file start time in yyyymmddhh      
      if(.not.read3('METEO3D','PRES',ALLAYS3,sdate3d,   &     ! read in pressure once and use it forever
       stime3d,press)) stop

      call nextime(sdate3d,stime3d,tstep3d*(mxrec3d-1))
      metetime=sdate3d*100+stime3d/10000               ! met file end time in yyyymmddhh
      
      if(.not.open3('BND1',FSREAD3,'init-convert')) then ! tracer boundary
       print*, 'failed to open INIT_IN'
       stop
      endif
      if (.not. DESC3('BND1') ) then   ! get grid information 
       print*, 'Error getting info from BND1' 
       stop
      endif
      if(nlays3d.ne.kmax.or.imax.ne.ncols3d.or.jmax.ne.nrows3d) then
        print*,'dimension does not match'
        print*,'imax,jmax,kmax=',imax,jmax,kmax
	print*,'ncols3d,nrows3d,nlays3d,nvars3d=',ncols3d,nrows3d,nlays3d,nvars3d
        stop
      endif 
      ietime=((mxrec3d-1)*tstep3d+stime3d)/10000
      ietime=(sdate3d+ietime/24)*100+mod(ietime,24) ! end time of the file in YYYYDDDHH

!-----build output file header     

      if(.not.open3('BND2',FSRDWR3,'pathway')) then  ! if not exist, generate it       

       nvars3d=noutbnd
       do n=1,noutbnd
        vname3d(n)=bndname(n)
        vtype3d(n)=M3REAL
        vdesc3d(n)='boundary condition of'//bndname(n)
        units3d(n)='ppmV or ug/m3'
       enddo
                 
       sdate3d=nowdate
       stime3d=nowtime
       tstep3d=dtstep*10000
       if(.not.OPEN3('BND2',FSUNKN3,'pathway')) then	! FSCREA3 FSUNKN3
        print*, 'Error opening output file BND2'
        stop	
       endif 

      else         ! check the consistence
       if(.not.desc3('BND2')) stop
       
       if(imax.ne.ncols3d.or.jmax.ne.nrows3d.or.kmax.ne.nlays3d.or.  &
        nvars3d.ne.noutbnd) then
        print*,'dimension does not much, STOP'
	stop
       endif
      endif 	

!----2D sample file
      if(.not.open3('CHECK2D',FSRDWR3,'pathway')) then  ! if not exist, generate it 
       ftype3d=GRDDED3
       nvars3d=ncheck
       nlays3d=1
       do n=1,ncheck
        vname3d(n)=checkname(n)
        vtype3d(n)=M3REAL
        vdesc3d(n)='2D Value of '//checkname(n)
        units3d(n)='ppmV or ug/m3'
       enddo 
       sdate3d=nowdate
       stime3d=nowtime
       tstep3d=dtstep*10000
       if(.not.OPEN3('CHECK2D',FSUNKN3,'pathway')) then	! FSCREA3 FSUNKN3
        print*, 'Error opening output file BND2'
        stop	
       endif 

      else         ! check the consistence
       if(.not.desc3('CHECK2D')) stop
       
       if(imax.ne.ncols3d.or.jmax.ne.nrows3d) then
        print*,'dimension does not much, STOP'
	stop
       endif
      endif 	
     
      
      call nemsio_init(iret=iret)

!****inquire dimension information

      jfhour=0                    ! forecasting hours
      jfiletime=0
      inowtime=nowdate*100+nowtime/10000
      
      do mtime=1,numts            

       if(mtime.gt.1) then
        call nextime(nowdate,nowtime,dtstep*10000)
	inowtime=nowdate*100+nowtime/10000        ! time in YYYYDDDHH
       endif  
       
       bnd(1:lenbnd,1:kmax,1:noutbnd)=0.
       checksp(1:imax,1:jmax,1:ncheck)=0.
       
       print*,'Now time is ', nowdate,              &
        nowtime,' in GMT.',inowtime,ietime
      
       do while(inowtime.ne.jfiletime)
        call daymon(nowdate,mmonth,mday)
	
       write(aline,'(a,i3.3,a)')trim(mofile(1)),jfhour,trim(mofile(2))

       if(mtime.gt.1) call nemsio_close(gfile)
       print*, aline
       call nemsio_open(gfile,trim(aline),'READ',iret=iret,gdatatype="bin4")
       if(iret.ne.0) then
         print*,'failed to open ',trim(aline)
	 stop
       endif	 
      call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,	  &
         dimy=jm,dimz=lm,idate=idate,gdatatype=gdatatype,gtype=gtype,	  &
        modelname=modelname,nfhour=nfhour,nfminute=nfminute,		  &
        nfsecondn=nfsecondn,nfsecondd=nfsecondd,nframe=nframe,  	  &
        ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta,	  &
        tlmeta=tlmeta)
       
        jdate=julian(idate(1),idate(2),idate(3))
	
	jfiledate=idate(1)*1000+jdate                      ! date in YYYYDDD
	jfiletime=idate(4)*10000+idate(5)*100+idate(6)     ! time in HHMMSS 
	call nextime(jfiledate,jfiletime,                    &
      	     nfhour*10000+nfminute*100+nfsecondn)
        jfiletime=jfiledate*100+jfiletime/10000            ! time in YYYYDDDHH
	
	print *,trim(aline),' iret=',iret,'nrec=',nrec,'im=',im,       &
        'jm=',jm,'lm=',lm,'idate=',idate,'gdatatype=',gdatatype,       &
        'gtype=',trim(gtype),'nfhour=',nfhour,'nfminute=',nfminute,    &
        'nfsecondn=',nfsecondn,'nfsecondd=',nfsecondd,'modelname=',    &
        modelname,'extrameta=',extrameta,'nframe=',nframe,'nmeta=',    &
        nmeta,'nsoil=',nsoil,'extrameta=',extrameta,'ntrac=',	       &
        ntrac,'tlmeta=',tlmeta,'filetime=',jfiletime
        
	jfhour=jfhour+dtstep      
       enddo
		
       if(mtime.eq.1) then
         igocart=im+2*nframe
	 jgocart=jm+2*nframe
	 kgocart=lm
	 
	 allocate(glon(igocart,jgocart))
	 allocate(glat(igocart,jgocart))
	 allocate(worka(igocart*jgocart))
         allocate(workb(igocart*jgocart))
         allocate(workc(igocart*jgocart))
         allocate(work(igocart*jgocart))
	 allocate(work2(igocart*jgocart))
	 allocate(work3(igocart*jgocart))
	 allocate(tmpa(kgocart),STAT=ierr)
         allocate(pgocart(igocart,jgocart,kgocart),STAT=ierr)
	 allocate(tgocart(igocart,jgocart,kgocart),STAT=ierr)
         allocate(airgocart(igocart,jgocart,kgocart),STAT=ierr)
	 allocate(vgocart(igocart,jgocart,kgocart),STAT=ierr)
	 
	else
	 if(igocart.ne.im+2*nframe.or.jgocart.ne.jm+2*nframe.or.  &
      	   kgocart.ne.lm) then
          print*,'inconsistent GOCART coordinate ',trim(aline)
	  stop
	 endif
	endif
           
      if(mtime.eq.1) then
       call nemsio_getfilehead(gfile,iret=iret,lat=work,lon=work2)
       do i=1,igocart
        do j=1,jgocart
	 glat(i,j)=work(i+(j-1)*igocart)
	 glon(i,j)=work2(i+(j-1)*igocart)
	enddo
       enddo 
       if(iret.ne.0) stop
       glatint=glat(1,2)-glat(1,1)
       glonint=glon(2,1)-glon(1,1)

       print*,' Gocart Latitude, Longtitude interval:',glatint,glonint

       if(iprint.eq.1) open(27,file='dust2.bin',form='unformatted',&
         access='direct',recl=igocart*jgocart*4)
	 
!---calculating lateral boundary horizontal index in GOCART coordinate

       do i=1,lenbnd 
        if(i.le.imax+1) then
         ix=i
	 jy=1
	if(ix.gt.imax) ix=imax
        else if(i.gt.imax+1.and.i.le.imax+jmax+2) then
         ix=imax
	 jy=i-imax-1
	 if(jy.gt.jmax) jy=jmax
        else if(i.gt.imax+jmax+2.and.i.le.2*imax+jmax+3) then
         ix=i-imax-jmax-2
       	 jy=jmax
	 if(ix.gt.imax) ix=imax
        else
         ix=1
	 jy=i-2*imax-jmax-3
	 if(jy.gt.jmax) jy=jmax
        endif		 

        do i2=1,igocart
         if(xlon(ix,jy).ge.glon(i2,1).and.xlon(ix,jy).le.glon(i2+1,1)) then
	  bndcoord(i,1:kmax,1)=i2+(xlon(ix,jy)-glon(i2,1))/     &   ! i in gocart coordiate
      	    (glon(i2+1,1)-glon(i2,1))  
	  exit
	 endif
	enddo

        do j2=1,jgocart
	 if( (glatint.gt.0.and.xlat(ix,jy).ge.glat(i2,j2).and.    &
           xlat(ix,jy).le.glat(i2,j2+1)).OR.(glatint.lt.0.and.    &
            xlat(ix,jy).le.glat(i2,j2).and.xlat(ix,jy)  	  &
             .ge.glat(i2,j2+1))) then
	  bndcoord(i,1:kmax,2)=j2+(xlat(ix,jy)-glat(i2,j2))/     &   ! j, in gocart coordiate
     	    (glat(i2,j2+1)-glat(i2,j2))  
	  exit
	 endif
	enddo
	  
       enddo
       
!---check layer horizontal index in GOCART coordinate

       do i=1,imax
        do j=1,jmax

	 do i2=1,igocart
          if(xlon(i,j).ge.glon(i2,1).and.xlon(i,j).le.glon(i2+1,1)) then
	   checkcoord(i,j,1)=i2+(xlon(i,j)-glon(i2,1))/      &  ! i in gocart coordiate
     	     (glon(i2+1,1)-glon(i2,1))  
	   exit
	  endif
	 enddo
         
	 do j2=1,jgocart
	 if( (glatint.gt.0.and.xlat(i,j).ge.glat(i2,j2).and.  &
           xlat(i,j).le.glat(i2,j2+1)).OR.(glatint.lt.0.and.  &
            xlat(i,j).le.glat(i2,j2).and.xlat(i,j)  	&
             .ge.glat(i2,j2+1))) then
     	  checkcoord(i,j,2)=j2+(xlat(i,j)-glat(i2,j2))/      &  ! j, in gocart coordiate
      	    (glat(i2,j2+1)-glat(i2,j2))  
	  exit
	 endif
	enddo

	enddo
       enddo	
      endif   ! end of mtime.eq.1
     
      if(inowtime.ge.metstime.and.inowtime.le.metetime) then
       if(check3('METEO3D','PRES',nowdate,nowtime)) then      ! if has met input, read it. otherwise use existing one
        if(.not.read3('METEO3D','PRES',ALLAYS3,nowdate,nowtime,press)) stop 
       endif
!        print*,'error read Pressure'
!	if(.not.read3('METEO3D2','PRES',ALLAYS3,nowyear*1000+nowdate,
!     1  nowtime*10000,press)) stop
!       endif
       endif
       call nemsio_readrecv(gfile,'pres','sfc',1, worka, &   ! surface pressure in Pa
          iret=iret)
        if(iret.ne.0) then
         print*,iret,'read gocart pressfc failed '
         stop
        endif

       workc=worka*0.0   !initialize the summed pressure to zero at first level
       do k=1,kgocart
        call nemsio_readrecv(gfile,'dpres','mid layer',k,workb, &   ! delta pressure in Pa
     	  iret=iret)
        if(iret.ne.0) then
         print*,'read gocart dpres failed ',k
	 stop
        endif
        workc=workc+workb   !sum delta pressure
	call nemsio_readrecv(gfile,'tmp','mid layer',k,work2, &  ! temperature in K
     	  iret=iret)
        if(iret.ne.0) then
	 print*,'error reading gocart temperature ',k
	 stop
        endif	
	call nemsio_readrecv(gfile,'spfh','mid layer',k,work3, &  ! specific humidity (kg/kg)
     	  iret=iret)
        if(iret.ne.0) then
	 print*,'error reading gocart Q ',k
	 stop
        endif
	
	do i=1,igocart
	 do j=1,jgocart
          pgocart(i,j,k)=worka(i+(j-1)*igocart) - workc(i+(j-1)*igocart)  
	  tgocart(i,j,k)=work2(i+(j-1)*igocart)
	  tv=work2(i+(j-1)*igocart)*(1+0.608*amax1(work3(i+(j-1)*igocart),1.e-15))  ! virtual temperature
	  airgocart(i,j,k)=pgocart(i,j,k)/tv/287.04*1000 ! air density in g/m3  R= 287.04 m3 Pa /kg/K
	 enddo
	enddo  
       enddo
       do i=1,lenbnd         ! find vertical index
        if(i.le.imax+1) then
         ix=i
	 jy=1
	if(ix.gt.imax) ix=imax	
        else if(i.gt.imax+1.and.i.le.imax+jmax+2) then
         ix=imax
	 jy=i-imax-1
	 if(jy.gt.jmax) jy=jmax
        else if(i.gt.imax+jmax+2.and.i.le.2*imax+jmax+3) then
         ix=i-imax-jmax-2
       	 jy=jmax
	 if(ix.gt.imax) ix=imax	
        else
         ix=1
	 jy=i-2*imax-jmax-3
	 if(jy.gt.jmax) jy=jmax
        endif		 
	x=bndcoord(i,1,1)
	y=bndcoord(i,1,2)
	xratio=x-int(x)
	yratio=y-int(y)

	do kp=1,kgocart
      	 tmpa(kp)=(1-yratio)*(pgocart(int(x),int(y),kp)*      &    ! horizontally interpolate pressure
     	  (1-xratio)+pgocart(int(x)+1,int(y),kp)*xratio)+     &
     	  yratio*(pgocart(int(x),int(y)+1,kp)*(1-xratio)+     &
     	  pgocart(int(x)+1,int(y)+1,kp)*xratio)
        enddo
	   
        do k=1,kmax
	 if(press(ix,jy,k).ge.tmpa(1)) then
	   bndcoord(i,k,3)=1.
	 else  
 	  do kp=2,kgocart

 	  if(press(ix,jy,k).ge.tmpa(kp).and.press(ix,jy,k).le.tmpa(kp-1)) then
          bndcoord(i,k,3)=kp-1+(press(ix,jy,k)-tmpa(kp-1))/  &
     	      (tmpa(kp)-tmpa(kp-1))
          goto 31
 	  endif  
	 enddo
        endif
  31     if(press(ix,jy,k).le.tmpa(kgocart)) bndcoord(i,k,3)=real(kgocart)     
        enddo	
       enddo

       do i=1,imax               ! find vertical index for checking species
        do j=1,jmax
	 x=checkcoord(i,j,1)
	 y=checkcoord(i,j,2)
 	 xratio=x-int(x)
	 yratio=y-int(y)
 	 do kp=1,kgocart
	  tmpa(kp)=(1-yratio)*(pgocart(int(x),int(y),kp)*      &    ! horizontally interpolate pressure
           (1-xratio)+pgocart(int(x)+1,int(y),kp)*xratio)+     &
           yratio*(pgocart(int(x),int(y)+1,kp)*(1-xratio)+     &
           pgocart(int(x)+1,int(y)+1,kp)*xratio)
         enddo
     
         if(press(i,j,checklayer).ge.tmpa(1)) then
	  checkcoord(i,j,3)=1.
	 else 
 	  do kp=2,kgocart

 	  if(press(i,j,checklayer).ge.tmpa(kp).and.   &
     	  press(i,j,checklayer).le.tmpa(kp-1)) then           ! find vertical index

           checkcoord(i,j,3)=kp-1+(press(i,j,checklayer)-   &
     	    tmpa(kp-1))/(tmpa(kp)-tmpa(kp-1))
          goto 41
	 endif
	 enddo
   41   continue     
        if(press(i,j,checklayer).le.tmpa(kgocart))  checkcoord(i,j,3)=real(kgocart)
	endif
        enddo
       enddo 
       	
             ! begin species interpolation                
       do L1=1,ngocart

        do k=1,kgocart
         call nemsio_readrecv(gfile,gocartname(L1),'mid layer',k,  &
     	  work,iret=iret)
          if(iret.ne.0) then
           print*,'read gocart failed ',k,gocartname(L1)
	   stop
          endif

        if(gocartname(L1).eq.'dust2'.and.iprint.eq.1.and.k.eq.1) then
	 write(27,rec=(mtime-1)*2+1)work
	 write(27,rec=mtime*2) airgocart(:,:,1)
	endif 
            
 	 do i=1,igocart
	  do j=1,jgocart
!	   vgocart(i,j,k)=work(i+(j-1)*igocart)*airgocart(i,j,k)*1e6    ! Operational NGAC unit conversion:  g/g -> ug/m3
           vgocart(i,j,k)=work(i+(j-1)*igocart)   ! FV3-Chem aerosol units already in -> ug/m3  
	   if(vgocart(i,j,k).gt.1e18) vgocart(i,j,k)=0. ! for undefine bug
	   
           if(gocartname(L1).eq.'so2') vgocart(i,j,k)=work(i+(j-1)*igocart)*1e6/64*28.97   ! kg/kg -> ppmV
	   if(gocartname(L1).eq.'o3mr') vgocart(i,j,k)=work(i+(j-1)*igocart)*1e6/48*28.97 
	  enddo
	 enddo
	enddo 

!        if(gocartname(L1).eq.'dust2'.and.iprint.eq.1) &
!	 write(27,rec=mtime)vgocart(:,:,1)
	
	  	   
	do i=1,lenbnd        ! for boundary conditions
	x=bndcoord(i,1,1)
	y=bndcoord(i,1,2)
	xratio=x-int(x)
	yratio=y-int(y)
	tmpa(1:kgocart)=(1-yratio)*(vgocart(int(x),int(y),     & ! horizontally interpolate values
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y),       &
     	 1:kgocart)*xratio)+yratio*(vgocart(int(x),int(y)+1,  &
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y)+1,     &
     	 1:kgocart)*xratio)
	
	 do k=1,kmax
	  z=bndcoord(i,k,3)
	  zratio=z-int(z)
	  tmpvalue=(1-zratio)*tmpa(int(z))+zratio*tmpa(int(z)+1)     ! vertically interpolate values
	  do L2=1,noutbnd
	   bnd(i,k,L2)=bnd(i,k,L2)+amax1(tmpvalue,0.)*sfact(L1,L2)
	  enddo
	 enddo  
	enddo
	
       do i=1,imax        ! for checking species
	do j=1,jmax
	 x=checkcoord(i,j,1)
	 y=checkcoord(i,j,2)
 	 xratio=x-int(x)
	 yratio=y-int(y)
 	 tmpa(1:kgocart)=(1-yratio)*(vgocart(int(x),int(y),    &  ! horizontally interpolate values
      	  1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y),       &
     	  1:kgocart)*xratio)+yratio*(vgocart(int(x),int(y)+1,  &
     	  1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y)+1,     &
     	  1:kgocart)*xratio)

	  z=checkcoord(i,j,3)
	  zratio=z-int(z)
	  tmpvalue=(1-zratio)*tmpa(int(z))+zratio*tmpa(int(z)+1)     ! vertically interpolate values
	  do L2=1,ncheck
	   checksp(i,j,L2)=checksp(i,j,L2)+amax1(tmpvalue,0.)*checkfact(L1,L2)
	  enddo
	 enddo 
        enddo  
	
       enddo	 ! end of Mozart species loop
       

       do L=1,noutbnd       ! check if gocart supplies all species, otherwise pickup from BND1 or FIXINIT
!        if(sum(bnd(1:lenbnd,1:kmax,L)).le.1e-20.and.(.not.indexfind(L))) then

!	 if(.not.interp3('BND1',bndname(L),'meteo',nowdate,  &
!          nowtime,lenbnd*kmax*1,tmpbnd(1,1,L))) then
!          print*,bndname(L),'is also not availabe in BND1 (fixed BND)', &
!     	    inowtime,trim(aline)
!         stop

!-----use fix bnd instead
!	  i0n = imax + jmax + 2
!          do k = 1,kmax
!           do i = 1,imax
!            bnd(i,k,L) = fixinit(i,1,k,L)
!            bnd(i0n+i,k,L) = fixinit(i,jmax,k,L)
!           enddo
!	   bnd(imax+1,k,L) = fixinit(imax,1,k,L)
!	   bnd(i0n+imax+1,k,L) = fixinit(imax,jmax,k,L)
!          enddo
    
!          j0e = imax + 1
!          j0w = 2*imax + jmax + 3
!          do k = 1,kmax
!           do j = 1,jmax
!            bnd(j0e+j,k,L) = fixinit(imax,j,k,L)
!            bnd(j0w+j,k,L) = fixinit(1,j,k,L)
!           enddo
!  	   bnd(j0e+jmax+1,k,L) = fixinit(imax,jmax,k,L)
!	   bnd(j0w+jmax+1,k,L) = fixinit(1,jmax,k,L)
!          enddo
!	 else
!	  print*,'use BND1 for ',bndname(L)
!	 endif
!        endif

        if(sum(bnd(1:lenbnd,1:kmax,L)).le.1e-20.and.(.not.indexfind(L))) then
	 print*, 'use BND1 for ', bndname(L)
	 if(.not.interp3('BND1',bndname(L),'meteo',nowdate,  &
          nowtime,lenbnd*kmax*1,bnd(1,1,L))) then
          print*,bndname(L),'is also not availabe in BND1 (fixed BND)', &
     	    inowtime,trim(aline)
	 endif	 
	else if	(indexfind(L).and.bndname(L).eq.'NUMACC') then
	 print*, "add numacc together"
	 if(.not.interp3('BND1',bndname(L),'meteo',nowdate,  &
          nowtime,lenbnd*kmax*1,tmpbnd(1,1,L))) then
          print*,bndname(L),'is also not availabe in BND1 (fixed BND)', &
     	    inowtime,trim(aline)
	 endif	 	 
	 do i=1,lenbnd
	  do k=1,kmax
	   bnd(i,k,L)=bnd(i,k,L)+tmpbnd(i,k,L)	  
	  enddo
	 enddo
	else if (indexfind(L).and.bndname(L).eq.'NUMCOR') then
	 print*, "add numcor together"
	 if(.not.interp3('BND1',bndname(L),'meteo',nowdate,  &
          nowtime,lenbnd*kmax*1,tmpbnd(1,1,L))) then
          print*,bndname(L),'is also not availabe in BND1 (fixed BND)', &
     	    inowtime,trim(aline)
	 endif	 		 
	 do i=1,lenbnd
	  do k=1,kmax
	   bnd(i,k,L)=bnd(i,k,L)+tmpbnd(i,k,L)	  
	  enddo
	 enddo
	else
	 print*,'use NGAC bnd for ', bndname(L)	
	endif


       enddo

       if(.not.write3('BND2',ALLVAR3,nowdate, &
         nowtime,bnd))	then
         print*,'writting error'
	 stop
       endif	 

       if(.not.write3('CHECK2D',ALLVAR3,nowdate,  &
         nowtime,checksp))  then
        print*,'writting error'
	stop
       endif	 

      enddo 
      lflag=shut3()
      
      end


      subroutine aq_blank(ntot,y)

      character*1 y(ntot)
      do i=1,ntot
      y(i)=' '
      enddo
      return
      end

      subroutine aq_locate(iunit,char,iflag)
!***********************************************************************
      character*(*) char
      character*80 dum1
      nchar=len(char)
      iflag=0
      do iter=1,10000
      read(iunit,'(a)',end=98) dum1(1:nchar)
!      print*,'dum1= ',dum1(1:nchar)
      if(dum1(1:nchar).eq.char) return
      enddo
98    iflag=1
!      print*,'dum1= ',dum1(1:nchar)
      return
      end

!**********************************************************************
      subroutine aq_find(num,cdum1,sname,lpsec,iflag)
!***********************************************************************
      dimension sname(1)
      character*(*) sname,cdum1
      iflag=0
      do 15 l=1,num
      if(cdum1.ne.sname(l)) go to 15
      lpsec=l
      return
15    continue
      iflag=1
      return
      end

      subroutine aq_readhd(iunit)
!***********************************************************************
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
