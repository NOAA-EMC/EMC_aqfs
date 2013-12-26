!----------convert CMAQ surface output to grib format and calculate daily maximum of 1-hr ozone
!            8 hr ozone,  1-hr PM2.5 and 24-hr PM2.5 starting from 04Z (to comparable with AIRNOW data)
!
!  by      Youhua Tang, April 2009

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      real, allocatable :: work(:,:), o3(:,:,:), pm25(:,:,:), o3_8hr(:,:,:), &
        pm25_24hr(:,:,:)
      
      logical pmon
      logical, allocatable :: lb(:,:)   ! logical mask
      
      integer, parameter :: naerospec=16, nname=4
      character*16 aerospec(naerospec),varlist(nname),maxname(nname),currentfile
      character*200 outfile
      integer kpds(200),kgds(200)
      
      data aerospec/'ASO4I','ASO4J','ANH4I','ANH4J','ANO3I','ANO3J','AORGAI', &
       'AORGAJ','AORGPAI','AORGPAJ','AORGBI','AORGBJ','AECI','AECJ','A25I','A25J'/
      
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
	enddo
        pm25(1:imax,1:jmax,nt)=pm25(1:imax,1:jmax,nt)+work(1:imax,1:jmax)
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

!-----Grib file header information
      
      call baopenw(51,trim(outfile),ierr)
      if(ierr.ne.0) then
       print*,'can not open ',trim(outfile)
       stop 2001
      endif
            
      kpds(1)=07          ! ID OF CENTER, NCEP
      kpds(2)=211         ! Generating process ID number, table A
      kpds(3)=id_gribdomain      ! Grid Identification (255 = User defined grid, defined in kgds)
      kpds(4)=128        !  Flag indicating presence of grid description section (almost always
                        !  included in NCEP grib files) or bit map section (BMS) (not usually 
                        !  included, but does happen to be included for your sst files).  It's 
                        !  a binary value; 128 = GDS(yes), BMS(no); 192 = GDS(yes), BMS(yes).
      kpds(6)=107     ! Type of level.  1 = surface (of the Earth, including sea surface).
                      ! Refer to Tables 3 & 3a in GG.  Other types of levels include 100,
                      ! which means standard pressure level. 109 Hybrid, 107 sigma
      kpds(7)=10000   ! Actual value of the height or pressure level.  0 = surface.
      
      call daymon(istime(1)/100,imonth,idate)   ! istime is in YYYYDDDHH, CMAQFILE1 is the base file

      kpds(8)=mod(istime(1)/100000,100)	! Initial yy of analysis or forecast 
      kpds(9)=imonth  	                ! Initial mm of analysis or forecast 
      kpds(10)=idate  	                ! Initial dd of analysis or forecast
      kpds(11)=mod(istime(1),100)       ! Initial hh of analysis or forecast
      kpds(12)=0		        ! Initial min of analysis or forecast


      kpds(13)=1      ! forecast time unit of kpds(14), table 4, 1= hour

      kpds(18)=1     ! grib version
      kpds(19)=129   ! Version number of Parameter Table (table 2)
      kpds(20)=0     ! number missing from average; meaningless for this data 
      kpds(21)=istime(1)/10000000+1    ! Century of initial time of your data 
      kpds(22)=6     ! Units decimal scale factor

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
      

!----start time loop

      nowdate=msdate 
      nowtime=markutc*10000

!      open(9,file='o3-maxi.grads',form='unformatted',access='direct',recl=imax*jmax*4)
      
      do mday=1,ksteps/24   

       print*,'mday=',mday      
       do L=1,nspecies

        if(varlist(L).eq.'o3_1hr') then
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(o3(i,j,(mday-1)*24+1:mday*24))   ! 1hr daily O3 maximum
          enddo
	 enddo
	 
!        write(9,rec=mday)work
	 
	 kpds(5)=231                                    ! 1hr daily O3 maximum
         kpds(17)=1                                     ! Number included in average
	 
	else if(varlist(L).eq.'pm25_1hr') then
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(pm25(i,j,(mday-1)*24+1:mday*24))   ! 1hr daily PM25 maximum
          enddo
	 enddo
	 
	 kpds(5)=233                                    ! 1hr daily PM25 maximum
         kpds(17)=1                                     ! Number included in average
	 		 
	else if(varlist(L).eq.'o3_8hr') then  
	  
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(o3_8hr(i,j,(mday-1)*24+1:min((mday-1)*24+24,ksteps)))   ! 8hr daily O3 maximum
          enddo
	 enddo
	 
	 kpds(5)=232                                    ! 8hr daily o3 maximum
         kpds(17)=8                                     ! Number included in average 
        else if(varlist(L).eq.'pm25_24hr') then  
	
	if((ksteps-23).lt.mday*24) cycle
	
         do i=1,imax
	  do j=1,jmax
	   work(i,j)=maxval(pm25_24hr(i,j,(mday-1)*24+1:mday*24))   ! 24hr daily PM25 maximum
          enddo
	 enddo
	 
	 kpds(5)=234                                    ! 24hr daily PM25 maximum
         kpds(17)=24                                    ! Number included in average	 
        endif

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

       kpds(14)=nsteps    ! time range (0,6,12,18), or starting time for average, 0 f from markutc
       if(ntmpstep.lt.0) kpds(14)=1-nsteps   ! initial time is 1st time step, not 0th step
       
       if(varlist(L).eq.'pm25_24hr') then
         kpds(14)=kpds(14)+24   ! for 24hr maximum, we pick up second day
       else if(varlist(L).eq.'o3_8hr') then
         kpds(14)=kpds(14)+7   ! for 8hr maximum
       endif

       if(istime(1).eq.istime(2)) then    ! same file
        kpds(15)=min(kpds(14)+23,maxrec2)
       else	      
       kpds(15)=min(kpds(14)+23,ksteps)    ! However, if the data in this GRIB record contain, for 
                               ! example, an average of a value from one time to another, kpds(14) will 
                               ! hold the value of the beginning time of the average, and kpds(15) will 
                               ! hold the ending time.
       endif
       
       if(kpds(14).lt.0) then
        kpds(16)=7              ! time range indicator, table 5, P1
	kpds(14)=-kpds(14)
       else
        kpds(16)=3     ! time range indicator, table 5	
       endif 	

          
	tmpmax=maxval(work)
	  
	kpds(22)=8-alog10(tmpmax)     ! Units decimal scale factor	
	
	print*,'write ',varlist(L),nowdatetime,ntmpstep, kpds(14), ntmpdate,ntmptime
	
!	call putgb(51,imax*jmax,kpds,kgds,lb,work,iret)
!	if(iret.ne.0) then
!	  print*,'writting grib error ',iret, L,k
!	  call errexit(iret)
!	  stop
!	endif

        call gribitb(lb,work,imax,jmax,51,kpds)
	  
        enddo 
	
	call nextime(nowdate,nowtime, 240000)
	
      enddo

      call baclose(51,ierr)

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
      id(9) = KPDSOUT(6)         !Had temporarily for 5x made into 105
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

      call w3fi72(itype,ozout,igrd,ibitl,      &
     &            ipflag,id,pds,               &
     &            igflag,igrid,igds,icomp,     &
     &            ibflag,ibmap,iblen,          &
     &            ibdsfl,                      &
     &            npts,kbuf,itot,ier)


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
