      program smoke2cmaq_35layer_2
      
c
c  This program is created by Li.Pan at NOAA/OAR/ARL      
      
c      implicit none

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API
      include 'CONST3.EXT'      ! i/o API 
      
      parameter (imax=442,jmax=265,kmax=35,kemis=20)
C      parameter (nspe_emis=33,nspe_fire=7)
c new emission 35 sepcies, POA NASN
c      parameter (nspe_emis=35,nspe_fire=7)  
c new emission 54 species
c      parameter (nspe_emis=54,nspe_fire=7) 
c 502 emission 53 species
      parameter (nspe_emis=53,nspe_fire=7)          
      
      integer syear,smon,sday,jdate,jtime,jstep
      integer crosindex(nspe_emis)
      
      real vheight(kmax+1),crosratio(nspe_emis),conunit(nspe_fire)
      
      character*160 FIRE3D,OEMIS,NEMIS
      character dirname*120,fire_spe(nspe_fire)*16,
     1 emis_spe(nspe_emis)*16     
     
     
c PM25 PM10 PM CO CO2 CH4 NMHC
c convert PM and NMHC from kg/hour to g/s
c convert CO CO2 CH4 from kg/hour to moles/s
      data conunit/0.27778,0.27778,0.27778,0.00992,0.00631,0.01736,
     1 0.27778/

c ALD2,ALDX,BENZENE,CH4,CO,ETH,ETHA,ETOH,FORM,HONO,IOLE,ISOP,MEOH,NASN 
c NH3,NO,NO2,NR,NVOL,OLE,PAR,PEC,PMC,PMFINE,PNO3,POA,POC,PSO4,SO2,SULF
c TERP,TOL,UNK,UNR,XYL    
c      data crosindex/7, 7, 0, 6, 4, 7, 7, 0, 7, 0, 7, 0, 0, 0,
c     1 4, 4, 4, 7, 0, 7 ,7, 1, 2, 1, 1, 0, 1, 1, 4, 0,
c     2 7, 7, 0, 7, 7/  
     
c ALD2,ALD2_PRIMARY,ALDX,BENZENE,CH4,CL2,CO,ETH,ETHA,ETOH,FORM,FORM_PRIMARY
c HCL,HONO,IOLE,ISOP,MEOH,NH3,NH3_FERT,NO,NO2,NR,NVOL,OLE,PAL,PAR,PCA,PCL
c PEC,PFE,PH2O,PK,PMC,PMFINE,PMG,PMN,PMOTHR,PNA,PNCOM,PNH4,PNO3,POA,POC,PSI
c PSO4,PTI,SO2,SULF,TERP,TOL,UNK,UNR,VOC_INV,XYL
c      data crosindex/7, 0, 7, 0, 6, 0, 4, 7, 7, 0, 7, 0,
c     1 0, 0, 7, 0, 0, 4, 0, 4, 4, 7, 0, 7, 0, 7, 0, 0, 
c     2 1, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
c     3 1, 0, 4, 0, 7, 7, 0, 7, 0, 7/ 

c for CMAQ502 changing PMFINE to PMOTHR     
      data crosindex/7, 0, 7, 0, 6, 0, 4, 7, 7, 0, 7, 0,
     1 0, 0, 7, 0, 0, 4, 0, 4, 4, 7, 0, 7, 0, 7, 0, 0, 
     2 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0,
     3 1, 0, 4, 0, 7, 7, 0, 7, 0, 7/      

c ALD2,ALDX,BENZENE,CH4,CO,ETH,ETHA 
c ETOH,FORM,HONO,IOLE,ISOP,MEOH,NASN,NH3,NO,
c NO2,NR,NVOL,OLE,PAR,PEC,PMC,PMFINE,PNO3,POA,
c POC,PSO4,SO2,SULF,TERP,TOL,UNK,UNR,
c XYL
c      data crosratio/0.0001279,0.0004563,0.0,1.0,1.0,0.008516,0.004352,
c     1 0.0,0.000002975,0.0,0.00011577,0.0,0.0,0.0,0.00659,0.0115,
c     2 0.00271,0.01872,0.0,0.002167,0.028,0.0949,1.0,0.3348,0.0013,0.0,
c     3 0.5564,0.0126,0.002625,0.0,0.0001591,0.0003973,0.0,0.008133,
c     4 0.0003012/

c ALD2,ALD2_PRIMARY,ALDX,BENZENE,CH4,CL2,CO,
c ETH,ETHA,ETOH,FORM,FORM_PRIMARY,HCL,HONO,IOLE,
c ISOP,MEOH,NH3,NH3_FERT,NO,NO2,NR,NVOL,OLE,
c PAL,PAR,PCA,PCL,PEC,PFE,PH2O,PK,PMC,PMFINE,PMG,
c PMN,PMOTHR,PNA,PNCOM,PNH4,PNO3,POA,POC,PSI,PSO4,
c PTI,SO2,SULF,TERP,TOL,UNK,UNR,VOC_INV,
c XYL
     
c      data crosratio/0.0001279, 0.0, 0.0004563, 0.0, 1.0, 0.0, 1.0,
c     1 0.008516, 0.004352, 0.0, 0.000002975, 0.0, 0.0, 0.0, 0.00011577,
c     2 0.0, 0.0, 0.00659, 0.0, 0.0115, 0.00271, 0.01872, 0.0, 0.002167,
c     3 0.0, 0.028, 0.0, 0.0, 0.0949, 0.0, 0.0, 0.0, 1.0, 0.3348, 0.0, 
c     4 0.0, 0.0, 0.0, 0.0, 0.0, 0.0013, 0.0, 0.5564, 0.0, 0.0126, 
c     5 0.0, 0.002625, 0.0, 0.0001591, 0.0003973, 0.0, 0.008133, 0.0, 
c     7 0.0003012/ 
     
c for CMAQ502 changing PMFINE to PMOTHR
      data crosratio/0.0001279, 0.0, 0.0004563, 0.0, 1.0, 0.0, 1.0,
     1 0.008516, 0.004352, 0.0, 0.000002975, 0.0, 0.0, 0.0, 0.00011577,
     2 0.0, 0.0, 0.00659, 0.0, 0.0115, 0.00271, 0.01872, 0.0, 0.002167,
     3 0.0, 0.028, 0.0, 0.0, 0.0949, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 
     4 0.0, 0.3348, 0.0, 0.0, 0.0, 0.0013, 0.5564, 0.0, 0.0126,
     5 0.0, 0.002625, 0.0, 0.0001591, 0.0003973, 0.0, 0.008133, 0.0, 
     7 0.0003012/      
      
      real, allocatable, dimension  (:,:,:,:) :: emis1      
      real, allocatable, dimension  (:,:,:,:) :: femis  
      real, allocatable, dimension  (:,:,:,:) :: emis2      
      
      allocate(emis1(imax,jmax,kemis,nspe_emis),STAT=ierr)
      if(ierr.ne.0) stop 2001      
      allocate(femis(imax,jmax,kmax,nspe_fire),STAT=ierr)
      if(ierr.ne.0) stop 2002
      allocate(emis2(imax,jmax,kmax,nspe_emis),STAT=ierr)
      if(ierr.ne.0) stop 2003
      
      namelist/control/syear,smon,sday,numrec,dirname
      
      open(7,file='cmaq.ini')
      read(7,control)
      print*, syear,smon,sday,numrec,dirname                   
      
      if(.not.open3('FIRE3D',FSREAD3,'aq_open')) then
       print*, 'failed to open FIRE3D'
       stop
      endif


      if(.not. DESC3('FIRE3D') ) then
       print*, 'Error getting info from FIRE3D'
       stop
      endif 
      
      do L=1,nvars3d
       fire_spe(L)=vname3d(L)
c       print*,fire_spe(L)
      enddo
      
      do k=1,kmax+1
       vheight(k)=vglvs3d(k)
c       print*,vheight(k)
      enddo
       
      
      if(.not.open3('OEMIS',FSREAD3,'aq_open')) then
       print*, 'failed to open OEMIS'
       stop
      endif


      if(.not. DESC3('OEMIS') ) then
       print*, 'Error getting info from OEMIS'
       stop
      endif 
      
      nlays3d=kmax
      
      do k=1,nlays3d+1
       vglvs3d(k)=vheight(k)
      enddo
      
      
      if(.not.open3('NEMIS',FSCREA3,'aq_open')) then
       print*, 'failed to open NEMIS'
       stop
      endif
      
      do L=1,nvars3d
       emis_spe(L)=vname3d(L)
c       print*,emis_spe(L)
      enddo
                  
      jdate=sdate3d
      jtime=stime3d
      jstep=tstep3d
      
      print*, jdate,jtime,jstep
                 
      do n=1,numrec
c      do n=1,mxrec3d      
c      do n=1,1      
       femis(1:imax,1:jmax,1:kmax,1:nspe_fire)=0.0
       emis1(1:imax,1:jmax,1:kemis,1:nspe_emis)=0.0
       emis2(1:imax,1:jmax,1:kmax,1:nspe_emis)=0.0

       do L1=1,nspe_fire
c        print*,fire_spe(L1)
        if(.not.READ3('FIRE3D',fire_spe(L1),ALLAYS3, jdate, jtime,
     1   femis(1,1,1,L1))) then
         print*, 'Error in reading fire_spe(L1) from FIRE3D'
         stop
        endif
       enddo !L1
	       
       do L=1,nvars3d
        if(.not.READ3('OEMIS',vname3d(L),ALLAYS3, jdate, jtime,
     1   emis1(1,1,1,L))) then
         print*, 'Error in reading vname3d(L) from OEMIS'
         stop
        endif	

       L2=crosindex(L)
c       if (L2.eq.0) then
c        print*,'there is no cross index match'
c        print*,L2,fire_spe(5),vname3d(L),crosratio(L),conunit(L2)
c       else
c        print*,L2,fire_spe(L2),vname3d(L),crosratio(L),conunit(L2)
c       endif
       
       do i=1,imax
        do j=1,jmax
	 do k=1,kmax
	  if(k.le.kemis) then	  
	   emis2(i,j,k,L)=emis1(i,j,k,L)+femis(i,j,k,L2)*crosratio(L)*
     1      conunit(L2)
c           if(femis(i,j,k,L2).gt.0.0.and.k.eq.1) then
c	    print*,'before',i,j,k,vname3d(L),emis1(i,j,k,L)
c            print*,'after',i,j,k,vname3d(L),emis2(i,j,k,L)
c	   endif
	  else	  
	   emis2(i,j,k,L)=0.0+femis(i,j,k,L2)*crosratio(L)*conunit(L2)
c           if(femis(i,j,k,L2).gt.0.0) then
c            print*,'after',i,j,k,vname3d(L),emis2(i,j,k,L)
c	   endif	   
	  endif
	 	 
	 enddo !k
	enddo !j
       enddo !i
       
       if(.not.WRITE3('NEMIS',vname3d(L),jdate,jtime,
     &  emis2(1,1,1,L))) then
        print*,' write error in NEMIS', vname3d(L)
        stop
       endif 

       enddo !L
             
       call nextime(jdate,jtime,jstep)
c       print*,jdate,jtime,jstep
       
      enddo         
      
      iflag=close3('FIRE3D')
      iflag=close3('OEMIS')
      iflag=close3('NEMIS')
      
      
      end program smoke2cmaq_35layer_2
