!------------------------------------------------------------------------------
!  aqm.post1_maxi_bias_cor
!  Author:   Jianping Huang 03/17/2015
!            based on read__gridded_aqm.f90
!  Purposes: 1) convert Bias Correction files from netcdf format to grib1
!            2) calculate daily max and daily averaged PM2.5 
!
!------------------------------------------------------------------------------
program aqm_post_maxi_bias_cor

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   use index_to_date_mod        
   use date__index
   use next__time

   implicit none

   						!   fail (stdlit)
! Local variables.

   character outfile1*200,outfile2*200
   integer nhours
   integer dims_in4(4), dims_in3(3)
!   logical fail1, fail2

! added by JP  
   character  infile1*200,infile2*200,infile3*200
   character  varname*10,ymd*8,ch_cyc*2,ch_chk*2
   integer    diag, imax,jmax
   integer    icyc,iyear,imonth,iday,idate,ihour,base_year,nt,ichk
   integer    nowdate,nowdate1,nowtime
   integer    ierr,lb,mday
   integer    i, j, IRET 

   integer status
   
   integer id_gribdomain
   integer ibin_scl,idec_scl,inumbits

!   integer kpds(200),kgds(200)
   integer kpds(25),kgds(25)
!   integer kpds(25),kgds(20)
!   integer kpds(200),kgds(200),istime(1),ietime(1)  

   character(*), parameter :: calendar  = 'gregorian'

    logical  ave1hr

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

!   real(dp), allocatable :: vdata(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata1(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata2(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata3(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real, allocatable :: pm25data(:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: pm25(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: pm25_ave(:,:,:),pm25_max(:,:,:)
   real(dp), allocatable :: work(:,:)
   logical, allocatable :: LB1(:)

   real tmp_t1, tmp_t2,tmpmax

! Get command arguments as strings.
   diag=5
   id_gribdomain=148
!   outfile1='aqm.t99z.pm25_24hr_ave.148.bc.grib'
!   outfile2='aqm.t99z.pm25_24hr_max.148.bc.grib'
   outfile1='aqm.t99z.24hpm25-ave.148.bc.grib'
   outfile2='aqm.t99z.1hpm25-max.148.bc.grib'

   ave1hr=.true.
   print*,"it is testing"
   call get_command_argument (1, varname) 
   call get_command_argument (2, ymd)
   call get_command_argument (3, ch_cyc)
   call get_command_argument (4, ch_chk)
   
!
   read(ymd(1:4),*)iyear
   read(ymd(5:6),*)imonth
   read(ymd(7:8),*)iday
   read(ch_cyc(1:2),*)icyc
   read(ch_chk(1:2),*)ichk
   ihour=icyc

   if ( icyc .le. 9 ) then
    write(outfile1(6:6),'(a1)')"0"
    write(outfile2(6:6),'(a1)')"0"
    write(outfile1(7:7),'(i1)')icyc
    write(outfile2(7:7),'(i1)')icyc
   else
    write(outfile1(6:7),'(i2)')icyc
    write(outfile2(6:7),'(i2)')icyc
   endif

   print*,"iyear=",iyear,"imonth=",imonth,"iday=",iday,"icyc=",icyc
   print*,"outfile1=",outfile1
   print*,"outfile2=",outfile2

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'aqm.post1_bias_correct: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

!   allocate(indata1(imax,jmax,1,48)) 
!   allocate(indata2(imax,jmax,1,48)) 
   infile1="a.nc"
   infile2="b.nc"
   infile3="c.nc"
 
   call read_netcdf_var (infile1, varname, diag, indata1, status)

   if ( icyc .eq. 6 ) then
    call read_netcdf_var (infile2, varname, diag, indata2, status)
   endif
   if ( icyc .eq. 12 ) then
    call read_netcdf_var (infile2, varname, diag, indata2, status)
    call read_netcdf_var (infile3, varname, diag, indata3, status)
   endif
    

! Read errors are soft errors.
! On read error, return with fail status from lower level.
   print*,"status=",status

!   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata1)			! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )		! omit vestigial LAY dimension

   imax       = dims_in4(1)
   jmax       = dims_in4(2)

! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours = size (indata1, 4)
   
   allocate (pm25data(imax,jmax))
   allocate (work(imax,jmax))
   allocate (pm25(imax,jmax,nhours))
   allocate (pm25_ave(imax,jmax,2))
   allocate (pm25_max(imax,jmax,2))
   allocate (LB1(imax*jmax))

!
!  combine previoud day 12z and today 06 or 12z to create a complete data for day1 and
!  day 2 (from current day 05z to 04z of the next day)
    if  ( icyc .eq. 6 ) then
      do i = 1, imax
        do j = 1, jmax
         if ( ichk .eq. 1 ) then
          pm25(i,j,1:2)  = indata2(i,j,1,5:6)  ! using today 00z file
         else
          pm25(i,j,1:2)  = indata2(i,j,1,17:18) ! using previous day 12z file
         endif
         pm25(i,j,3:nhours) = indata1(i,j,1,1:46)
       enddo  
      enddo 
    elseif ( icyc .eq. 12 ) then
     do i = 1, imax
      do j = 1, jmax
      if ( ichk .eq. 1 ) then
        pm25(i,j,1:2)  = indata2(i,j,1,5:6)      ! from 05z and 06z at 00z run
      else
        pm25(i,j,1:2)  = indata2(i,j,1,17:18)   ! using previous day 12z file
      endif
      pm25(i,j,3:8)  = indata3(i,j,1,1:6)      ! from 07-12z at 06z run
      pm25(i,j,9:nhours) = indata1(i,j,1,1:40) ! from 12z run
      enddo
     enddo
    endif
!
!  calculate daily max and 24_hr ave for day 1 and day 2
!-- ------------------------------------------------------
!
     call baopenw(51,trim(outfile1),ierr)
     call baopenw(52,trim(outfile2),ierr)

     if(ierr.ne.0) then
       print*,'can not open ',trim(outfile1)
        stop 2001
     endif

    kpds(14) = icyc

    do mday = 1, nhours/24 
     do i = 1, imax
      do j = 1, jmax
        pm25_ave(i,j,mday) = sum(pm25(i,j,(mday-1)*24+1:mday*24))/24. 
        pm25_max(i,j,mday) = maxval(pm25(i,j,(mday-1)*24+1:mday*24))
      enddo
     enddo
!   enddo


! Diagnostics, if requested.

   if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (pm25), ', ', &
         maxval (pm25)
   end if

   if (diag >= 3) print *, 'read_gridded_aqm: Return.'

!---------------------------------------------------------------------------------
!-----Grib file header information

      kpds(1)=07          ! ID OF CENTER, NCEP
      kpds(2)=211         ! Generating process ID number, table A
      kpds(3)=id_gribdomain   ! Grid Identification (255 = User defined grid,defined in kgds)
      kpds(4)=128        !  Flag indicating presence of grid description section (almost always
                        !  included in NCEP grib files) or bit map section (BMS)
                        !  (not usually
                        !  included, but does happen to be included for your sst
                        !  files).  It's
                        !  a binary value; 128 = GDS(yes), BMS(no); 192 =
                        !  GDS(yes), BMS(yes).
!      if(nlayers.eq.1) then
!       kpds(6)=1
!      else
       kpds(6)=107     ! Type of level.  1 = surface (of the Earth, including sea surface).
                       ! Refer to Tables 3 & 3a in GG.  Other types of levels
                       ! include 100,
                       ! which means standard pressure level. 109 Hybrid, 107
                       ! sigma
!      endif

!      print*,'nlayers, kpds(6), kpds(7) =', nlayers, kpds(6), kpds(7)
      kpds(7)=10000   ! Actual value of the height or pressure level.  0 =
!      surface.
!   where can I get istime ? where is daymon located?
!==========================================
      kpds(8)=mod(iyear,100)              ! Initial yy of analysis or forecast
      kpds(9)=imonth                       ! Initial mm of analysis or forecast
      kpds(10)=iday                      ! Initial dd of analysis or forecast
      kpds(11)=ihour                      ! Initial hh of analysis or forecast

      kpds(12)=0                        ! Initial min of analysis or forecast


      kpds(13)=1      ! forecast time unit of kpds(14), table 4, 1= hour

      kpds(15)=0      ! However, if the data in this GRIB record contain, for
                      ! example, an average of a value from one time to another,
                      ! kpds(14) will
                      ! hold the value of the beginning time of the average, and
                      ! kpds(15) will
                      ! hold the ending time.
      if(ave1hr) then
       kpds(16)=3
       kpds(17)=1
      else
       kpds(16)=0     ! time range indicator, table 5
      endif
      kpds(18)=1     ! grib version
      kpds(19)=129   ! Version number of Parameter Table (table 2)
      kpds(20)=0     ! number missing from average; meaningless for this data
      kpds(21)=iyear/100+1   ! Century of initial time of your data
!      kpds(22)=6     ! Units decimal scale factor


!----kdgs start
      if(id_gribdomain.eq.148) then        ! CMAQ 5x domain

! http://www.nco.ncep.noaa.gov/pmb/docs/libs/w3lib/putgb.html

      kgds(1)=3                 ! Data representation type (map projection).  0= Lat/Lon grid. See table 6
      kgds(2)=imax                ! Number of grid points in x-direction
      kgds(3)=jmax                ! Number of grid points in y-direction
      kgds(4)=21821              ! LA1 LAT OF ORIGIN (LOWER LEFT)
      kgds(5)=-120628            ! LO1 LON OF ORIGIN (LOWER LEFT
      kgds(6)=136                 ! (1001000) Resolution Flag (see Table 7 in GG).
      kgds(7)=-97000             ! LOV - ORIENTATION OF GRID
      kgds(8)=12000              ! DX - X-DIR INCREMENT
      kgds(9)=12000              ! DY - Y-DIR INCREMENT
      kgds(10)=0                 !  PROJECTION CENTER FLAG
                                  !      Bit 1 set to 0 if the North pole is on
                                  !      the projection plane.
                                  !      Bit 1 set to 1 if the South pole is on
                                  !      the projection plane.
      kgds(11)=64           ! SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
      kgds(12)=33000          ! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
      kgds(13)=45000          ! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
      endif


      base_year=iyear
      nowtime=ihour*10000

      kpds(11)=INT(nowtime/10000)

!
!  write out pm25_ave 
! ----------------------------------------

       kpds(5)=157
       kpds(17)=24

       if ( mday .eq. 1 ) then
         kpds(14)=5-icyc
         tmp_t1=kpds(14)
       endif
       if(kpds(14).lt.0) then
         kpds(16)=7              ! time range indicator, table 5, P1
         kpds(14)=-kpds(14)
         kpds(15)=-kpds(14)+23
       else
         kpds(14)=tmp_t1+24
         kpds(16)=3     ! time range indicator, table 5
         kpds(15)=kpds(14)+23
       endif
  
        pm25data(1:imax,1:jmax)=pm25_ave(1:imax,1:jmax,mday)   ! 24hr_pm25 average

        tmpmax = maxval(pm25data)
        kpds(22)=8-alog10(tmpmax)
      
!          call gribitb(lb,pm25data,imax,jmax,51,kpds)
        call putgb(51,imax*jmax,kpds,kgds,LB1,pm25data,IRET)

!
! write out daily max pm2.5
!--------------------------------------
!
       kpds(5)=233            ! 1hr daily PM25 maximum 
       kpds(17)=1 
       pm25data(1:imax,1:jmax)=pm25_max(1:imax,1:jmax,mday)   ! 24hr_ave pm2.5
       tmpmax = maxval(pm25data)
       kpds(22)=8-alog10(tmpmax)

       call putgb(52,imax*jmax,kpds,kgds,LB1,pm25data,IRET)

       end do

       call baclose(51,ierr)
       call baclose(52,ierr)
       
         
end program aqm_post_maxi_bias_cor 


                  


     



























