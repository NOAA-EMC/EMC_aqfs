!------------------------------------------------------------------------------
!  aqm.post1_bias_correct  
!  Author:   Jianping Huang 12/19/2014
!            based on read__gridded_aqm.f90
!  Purposes: 1) convert Bias Correction files from netcdf format to grib1
!
!------------------------------------------------------------------------------
program aqm_post1_bias_correct 

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   use index_to_date_mod        
   use date__index
   use next__time

   implicit none

   						!   fail (stdlit)
! Local variables.

   character outfile*200
   integer nhours
   integer dims_in4(4), dims_in3(3)
!   logical fail1, fail2

! added by JP  
   character  infile*80
   character  varname*10,ymd*8,ch_cyc*2
   integer    diag, imax,jmax
   integer    icyc,iyear,imonth,iday,idate,ihour,base_year,nt
   integer    nowdate,nowdate1,nowtime
   integer    ierr,lb

   integer status
   
   integer id_gribdomain

!   integer kpds(200),kgds(200)
   integer kpds(25),kgds(25)
!   integer kpds(25),kgds(20)
!   integer kpds(200),kgds(200),istime(1),ietime(1)  

   character(*), parameter :: calendar  = 'gregorian'

    logical  ave1hr

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

!   real(dp), allocatable :: vdata(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real, allocatable :: pm25data(:,:)	! (COL, ROW, LAY, TSTEP)

! Get command arguments as strings.
   diag=5
   id_gribdomain=148
   outfile='aqm.t99z.25pm99.bc'
   ave1hr=.true.
   call get_command_argument (1, infile)
   call get_command_argument (2, varname) 
   call get_command_argument (3, ymd)
   call get_command_argument (4, ch_cyc) 
   
!
   read(ymd(1:4),*)iyear
   read(ymd(5:6),*)imonth
   read(ymd(7:8),*)iday
   read(ch_cyc(1:2),*)icyc
   ihour=icyc

   if ( icyc .le. 9 ) then
    write(outfile(6:6),'(a1)')"0"
    write(outfile(7:7),'(i1)')icyc
   else
    write(outfile(6:7),'(i2)')icyc
   endif

   print*,"iyear=",iyear,"imonth=",imonth,"iday=",iday,"icyc=",icyc

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'aqm.post1_bias_correct: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

   call read_netcdf_var (infile, varname, diag, indata, status)

! Read errors are soft errors.
! On read error, return with fail status from lower level.
   print*,"status=",status

!   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata)			! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )		! omit vestigial LAY dimension

   imax       = dims_in4(1)
   jmax       = dims_in4(2)
!   print*,"dims_in4=",dims_in4
!   print*,"dims_in3=",dims_in3
!   expect2    = shape (indata)			! expected dimensions
!   print*,"expected dimenstio=",expect2
!   expect2(3) = expect2(3) + 1			! with 1 extra hour (MET)

!   fail1 = (any (dims_in3 /= shape (vdata)))
!   fail2 = (any (dims_in3 /= expect2))

!jp0   if (fail1 .and. fail2) then
!      print *, '*** read_gridded_aqm: Incorrect var dimensions in file.'
!      print *, '*** File = ' // trim (infile)
!      print *, '*** Var name = ' // trim (varname)
!      fmt1 = '(a,3(1x,i0),a,3(1x,i0),a)'
!      print fmt1, ' *** Expected 3-D dimensions     = (', shape (vdata), &
!         ') or (', expect2, ')'
!      print fmt1, ' *** Current file var dimensions = (', dims_in3(:), ')'
!      print *, '*** Fundamental error.  Abort.'
!      call exit (1)
!jp9   end if

! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours = size (indata, 4)
   allocate (pm25data(imax,jmax))

! Diagnostics, if requested.

   if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (indata), ', ', &
         maxval (indata)
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
!jp      kpds(21)=istime(1)/10000000+1   ! Century of initial time of your data
       kpds(21)=iyear/100+1   ! Century of initial time of your data
!      kpds(22)=6     ! Units decimal scale factor

       kpds(5)=157    ! for PM2.5

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

      do nt=1,nhours

!       kpds(8)=mod(iyear,100)              ! Initial yy of analysis or forecast
!       kpds(9)=imonth                       ! Initial mm of analysis or forecast
!       kpds(10)=iday                      ! Initial dd of analysis or forecast
!       kpds(11)=INT(nowtime/10000)
      
       print*,"nt=",nt,"kpds(11)=",kpds(11)
 
       kpds(14)=nt-1

       kpds(15)=nt

      if (nt .le. 9 ) then
       write(outfile(14:14),'(a1)')"0"
       write(outfile(15:15),'(i1)')nt
      else
       write(outfile(14:15),'(i2)')nt
      endif

       call baopenw(51,trim(outfile),ierr)
       if(ierr.ne.0) then
        print*,'can not open ',trim(outfile)
        stop 2001
       endif

       pm25data(:,:)=indata(:,:,1,nt)

       kpds(22)=8-alog10(maxval(pm25data))

       call gribitb(lb,pm25data,imax,jmax,51,kpds)

!       call date_index(iyear, imonth, iday, ibase_year, calendar)
       nowdate=date_index(iyear, imonth, iday, base_year, calendar)

!       print*,"hjp111a,nowdate:",nowdate,"nowtime=",nowtime
       call next_time(nowdate,nowtime,10000)
!       print*,"hjp111b,nowdate:",nowdate,"nowtime=",nowtime
       call index_to_date(nowdate,iyear, imonth, iday, base_year, calendar)
!       print*,"hjp111c,iyear,imn,iday=",iyear, imonth, iday       

       call baclose(51,ierr)

       end do
         
end program aqm_post1_bias_correct 


      subroutine gribitb(ln,ozout,im,jm,iunit,KPDSOUT)
!      subroutine gribitb(ozout,im,jm,iunit,KPDSOUT)
     
      integer  mxbit, lenpds, lengds,im,jm,iunit
      parameter (mxbit=16,lenpds=28,lengds=32)
      character*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)

!      character*1  iflag*1, pds*28
      character*1   pds*28
      integer ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
      integer KPDSOUT(25),id(25),kgds(20)
      real ozout(im,jm)
      real  sgdg,gmin,gmax
      integer ln
      integer ibm, ibitm,nout,ideci,nbit,itype,ibitl,ipflag
      integer igrid,k,icomp,ibflag,iblen,itot,ier,igflag
      
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
!                       TO USE SIG DIGITS TO COMPUTE DEC SCALE
!
! USAGE:   CALL GET_BITS(IBM,ISGDS,LEN,MG,G,ISCALE,GROUND,GMIN,GMAX,NBIT)
!   INPUT ARGUMENT LIST:
!     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!     SGDS     - MAXIMUM SIGNIFICANT DIGITS TO KEEP
!                (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
!                OR BINARY PRECISION IF <0
!                (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
!                           -3.0 "                    " 1/8
!                          2**SGDS PRECISION)
!      LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
!      MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!      G        - REAL (LEN) FIELD
!
!    OUTPUT ARGUMENT LIST:
!      ISCALE   - INTEGER DECIMAL SCALING
!      GROUND   - REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
!      GMIN     - REAL MINIMUM VALID ROUNDED FIELD VALUE
!      GMAX     - REAL MAXIMUM VALID ROUNDED FIELD VALUE
!      NBIT     - INTEGER NUMBER OF BITS TO PACK
!
!  SUBPROGRAMS CALLED:
!    ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
!  ATTRIBUTES:
!    LANGUAGE: FORTRAN

      REAL  GMAX,GMIN,RMIN
      INTEGER LEN,IBM,i,i1
!jp      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
      REAL SGDS
      INTEGER MG(LEN)
      REAL G(LEN),GROUND(LEN)
      integer nbit,iscale,irett
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
!* FNDBIT                                                               *
!*                                                                      *
!* This subroutine computes the number of packing bits given the        *
!* maximum number (< 50) of significant digits to preserve or the       *
!* binary precision to store the data.  The binary precision is given   *
!* as zero, a negative integer, or as a postitive integer greater than  *
!* or equal to 50.  If the binary precision is given, ISCALE will       *
!* always be zero in this case.                                         *
!*                                                                      *
!* The binary precision translates as follows:                          *
!*     53  =>  store data to nearest 8                                  *
!*     52  =>  store data to nearest 4                                  *
!*     51  =>  store data to nearest 2                                  *
!*     50  =>  store data to nearest 1                                  *
!*      0  =>  store data to nearest 1                                  *
!*     -1  =>  store data to nearest 1/2                                *
!*     -2  =>  store data to nearest 1/4                                *
!*     -3  =>  store data to nearest 1/8                                *
!*                                                                      *
!* Note that RDB - 50 give the nearest whole power of two for binary    *
!* precision.                                                           *
!*                                                                      *
!* Note that a fractional number of significant digits is allowed.      *
!*                                                                      *
!* FNDBIT ( RMIN, RMAX, RDB, NBITS, ISCALE, RMN, IRET )                 *
!*                                                                      *
!* Input parameters:                                                    *
!*      RMIN            REAL            Minimum value                   *
!*      RMAX            REAL            Maximum value                   *
!*      RDB             REAL            Maximum # of significant digits *
!*                                        OR binary precision if < 0    *
!*                                                                      *
! * Output parameters:                                                  *
! *     NBITS           INTEGER         Number of bits for packing      *
! *     ISCALE          INTEGER         Power of 10 scaling to use      *
! *     RMN             REAL            Rounded miniumum                *
! *     IRET            INTEGER         Return code                     *
! *                                       0 = normal return             *
! **                                                                    *
! * Log:                                                                *
! * K. Brill/NMC        06/92               
! * K. Brill/EMC        12/95   Added binary precision; added RMN       *
! * K. Brill/EMC         1/97   Add .5 in rr= & rng2= for better rnd off*
! * K. Brill/EMC         1/97   Use 10**iscale in rounding the min      *
! ************************************************************************
       integer iret, icnt,iscale,nmbts,ibin,ixp,irmn
       real  rln2,rmn,rmin,range,rmax,rdb,po,rr,rng2,tp,x
    
        DATA            rln2/0.69314718/
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

!*      Compute RMN, the first packable value less than or equal to
!*      RMIN.

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
                  


     



























