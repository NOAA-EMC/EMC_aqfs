      program rdgrbwt_aot_CHA_g2 
!c
!c This program reads in AQF etapost generated grib files and compute
!c
!c    modified by jphuang to read in cmaq5.0.2 output
!c                  and write out AOD in grib2 on 4/20
!
!     June 10 2020   Ho-Chun Huang   Modified for CB6 with direct-read-in
!                                    of AOT from v5.3.1 FCST output 
!                                    aqm.t12z.rj_1.ncf, "AOD_W550_ANGST".
      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      integer, parameter :: izmax=35

      integer :: imon,idy,ihr,iyr,icyc,itime
      integer :: iseek,llgrib,llskip,kf,mm,dd,yyyy,cyc,fhr,istatus

      real, allocatable ::  aod550(:,:)

!!      character*200 outfile,ozonecatfile
      character*200 outfile
      character chtmp*2,grib_id*3
!!      integer :: nout, ibm, ibitm
      integer, dimension(200)   :: kpds,   kgds
!!      integer, dimension(ncmaq) :: gribid,  gribtb
!!      integer, dimension(nmet)  :: mgribid, mgribtb
      logical  :: iflag

      integer :: imax,jmax,kmax ,ii, jj, kk
      integer :: iyear,imonth,iday,ihour,base_year,nt
      integer :: lengrib
      integer :: nowdate,nowtime
      integer :: nowdate1,nowtime1,idate,idate1
      integer :: ierr,ier

      integer :: istime  !file start and ending in YYYYDDD0H

! grib2
      integer :: listsec0(2)
      integer :: listsec1(13)
      integer :: igds(5)
      integer :: igdstmpllen
      integer :: ipdstmpllen
      integer :: idrstmpllen
      integer :: idrsnum,ibmap,numcoord,ipdsnum,idefnum
      integer :: ibin_scl,idec_scl,inumbits

      integer, dimension(100) :: igdstmpl
      integer, dimension(100) :: ipdstmpl
      integer, dimension(100) :: idrstmpl

      real*4, parameter :: SPVAL=9.9e10
      character*1, allocatable :: cgrib1(:),cgrib2(:)
      logical*1  , allocatable :: bmap(:,:)
      logical*1  , allocatable :: bmap1(:)
      real*4     , allocatable :: fld1(:),fld2(:)
      real*4                   :: dxval, fldscl
      integer ibmap1

      character*14 fout1*15, date*10, fname*19, Dn2*3, Dcyc*3
      character*3  ch_im,ch_jm
      INTEGER, EXTERNAL :: ENVINT

      namelist /control/outfile,nlayers,id_gribdomain !   (table A)

      open(7,file='cmaq2grib2_aot.ini')
      read(7,control)
      close(7)


      imon= ENVINT('mm',' ',mm,istatus)
      idy = ENVINT('dd',' ',dd,istatus)
      iyr = ENVINT('yyyy',' ',yyyy,istatus)
      icyc= ENVINT('cyc',' ',cyc,istatus)
      ihr = icyc
      icyc= 100+ icyc
      write(Dcyc,'(i3)') icyc
      itime = ENVINT('fhr',' ',fhr,istatus)

      print*, iyr,imon,idy,ihr,"icyc=",icyc

      call getarg(1,ch_im)
      read(ch_im,'(i3)')im
      call getarg(2,ch_jm)
      read(ch_jm,'(i3)')jm
      print*,"im=",im,"jm=",jm
    
      nD2 = 100 + itime
      write(Dn2,'(i3)') nD2
      iunit=10
!==============================================================================
! open files
!==============================================================================

      if(.not.OPEN3('CHEM2D',FSREAD3,'pathway')) then
         print*,'open input file error for CHEM2D'
         stop
      endif

      if (.not. DESC3('CHEM2D') ) then   ! get grid information from CMAQ output
         print*, 'Error getting info from CHEM2D'
         stop
      endif

      if(tstep3d.ne.10000) then
         print*,'need hourly input in CMAQFILEs'
         stop
      endif

      write(*,'(''READ IN ncols3d '', i3.3, '' nrows3d = '', i3.3, 
     &          ''nlays3d = '', i3.3)') ncols3d, nrows3d, nlays3d
      imax=ncols3d
      jmax=nrows3d
      kmax=nlays3d

      max_bytes=imax*jmax*4
      nx=imax
      ny=jmax

      allocate(aod550(imax,jmax))

      allocate(cgrib1(max_bytes))
      allocate(cgrib2(max_bytes))
      allocate(bmap(imax,jmax))
      allocate(bmap1(nx*ny))
      allocate(fld1(nx*ny))
      allocate(fld2(nx*ny))
     
      if(nlayers.gt.kmax) then
         print*,'nlayers too high ', nlayers, kmax
         stop
      endif

      nowsdate=sdate3d
      nowstime=stime3d
      maxrec1=mxrec3d

      print*,"nowsdate=",nowsdate,"nowstime=",nowstime
      print*,"maxrec1=",maxrec1

      istime=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH

      ntmpdate=sdate3d
      ntmptime=stime3d
   
      icyc=INT(ntmptime/10000)

!==========================================================================
! add grib2 header information
!==========================================================================

!-- section 0:
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table0)(7:ncep)
      listsec1(2)=0       ! Identification of orginating subcenter(ON388-TableC) (4:emc)
      listsec1(3)=2       ! GRIB master tables version number (Table 1.0) (11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2)(0:ana 1:fcst 2:vrfy)
! following need to be changed !
      jjdate=int(istime/100)
      call daymon(jjdate,imonth1,idate1)
      listsec1(6)=int(istime/100000)
      listsec1(7)=imonth1
      listsec1(8)=idate1
      listsec1(9)=mod(istime,100)-1
      listsec1(10)=00     ! Reference time - Minute
      listsec1(11)=00     ! Reference time - Second
      listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

!
!-- section 3: grid definition section
      igds(1)=0           ! Source of grid definition (Table 3.0)(0:specified in the code)
      igds(2)=nx*ny       ! Number of grid points in the defined grid
      igds(3)=0           ! Number of octets for optional list of numbers defining number of points
      igds(4)=0           ! Interpretation of list of numbers defining number of points
!-- example: Gaussian Lat/lon
      igds(5)=30          ! Grid definition template number (Table 3.1)(0:Lat/lon, 30:Lambert 40:Gaussian)
      if( igds(5)==30) then
         igdstmpllen=22
         igdstmpl=0

!-- set up grid definition template 3.30
         igdstmpl=0
         igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
         igdstmpl(8)=nx      ! Ni . number of points along a paralell
         igdstmpl(9)=ny      ! Nj . number of points along a meridian
         if ( id_gribdomain .eq. 148 ) then
            igdstmpl(10)=21821000      ! Basic angle of the initial production domain
            igdstmpl(11)=239372000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
            latstt=8
            lonstt=33000000
            latlst=-88541961
            lonlst=358125000
            dxval=1875000
            igdstmpl(12)=latstt ! La1 - latitude of first grid point
            igdstmpl(13)=lonstt ! Lo1 - longitude of first grid point
            igdstmpl(14)=263000000
            igdstmpl(15)=12000000
            igdstmpl(16)=12000000
            igdstmpl(17)=0
            igdstmpl(18)=64
            igdstmpl(19)=33000000
            igdstmpl(20)=45000000
            igdstmpl(21)=0
            igdstmpl(22)=0
         elseif ( id_gribdomain .eq. 140 ) then
            igdstmpl(10)=53020000      ! Basic angle of the initial production domain
            igdstmpl(11)=193523000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!           latstt=8
!           lonstt=33000000
!           latlst=-88541961
!           lonlst=358125000
!           dxval=1875000
            igdstmpl(12)=8
            igdstmpl(13)=57000000
            igdstmpl(14)=211400000
            igdstmpl(15)=12000000
            igdstmpl(16)=12000000
            igdstmpl(17)=0
            igdstmpl(18)=64
            igdstmpl(19)=57000000
            igdstmpl(20)=63000000
            igdstmpl(21)=0
            igdstmpl(22)=0
         elseif ( id_gribdomain .eq. 139 ) then
            igdstmpl(10)=17721000      ! Basic angle of the initial production domain
            igdstmpl(11)=198027000      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!           latstt=8
!           lonstt=33000000
!           latlst=-88541961
!           lonlst=358125000
!           dxval=1875000
            igdstmpl(12)=8 ! La1 - latitude of first grid point
            igdstmpl(13)=19000000 ! Lo1 - longitude of first grid point
            igdstmpl(14)=202500000
            igdstmpl(15)=12000000
            igdstmpl(16)=12000000
            igdstmpl(17)=0
            igdstmpl(18)=64
            igdstmpl(19)=19000000
            igdstmpl(20)=21000000
            igdstmpl(21)=0
            igdstmpl(22)=0
         endif

      endif
      idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=46            ! Template 4.46
      ipdstmpllen=35        ! pdt template length
      ipdstmpl(1)=20        ! catogory  : Mass Mixing Ratio (Mass Fraction in Air)
      ipdstmpl(2)=102       ! Aerosol Optical Thickness 
!     2: Mass
!                            ! mixing ratio (mass fraction in air)-
!                            ! kg.kg^-1
!     ipdstmpl(3)=          ! aerosol typ (see code table 4.233)
!     ipdstmpl(4)=          ! Type of interval for first and second size
!                           !  (see Code Table 4.91)
!     ipdstmpl(5)=          ! Scale factor of first size
!     ipdstmpl(6)=          ! Scale value of first size in meters
!     ipdstmpl(7)=          ! Scale factor of second size
!     ipdstmpl(8)=          ! Scale value of second size in meters
      ipdstmpl(9)=2         ! Type of generating process (Table 4.3) (0:ana,1:ic, 2:fcst)
      ipdstmpl(10)=0        ! Background generating process identifier
      ipdstmpl(11)=211      ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(12)=0        ! Hours of observational data cutoff after reference time
      ipdstmpl(13)=0        ! Minutes of observational data cutoff after reference time
      ipdstmpl(14)=1        ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!     ipdstmpl(15)=nt-1     ! Forecast time in units defined by
      ipdstmpl(16)=104      ! Type of first fixed surface (see Code table 4.5) (100:isobaric leve)
      ipdstmpl(17)=4        ! Scale factor of first fixed surface
      ipdstmpl(18)=10000    ! Scaled value of first fixed surface
      ipdstmpl(19)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(20)=0        ! Scale factor of second fixed surface
      ipdstmpl(21)=0        ! Scaled value of second fixed surface
!jp   ipdstmpl(22)=iyear    !  Year
!jp   ipdstmpl(23)=imonth   !  Month
!jp   ipdstmpl(24)=iday     !  Date
!jp   ipdstmpl(25)=INT(nowtime/10000)     !  Forecast hour
      ipdstmpl(26)=0        !
      ipdstmpl(27)=0        !
!     ipdstmpl(28)=1       ! number of time range specifications
      ipdstmpl(29)=0        !
      ipdstmpl(30)=0        !
      ipdstmpl(31)=2        !
      ipdstmpl(32)=1        !
      ipdstmpl(33)=1        !
      ipdstmpl(34)=255      !
      ipdstmpl(35)=0        !
!
      numcoord=0            ! Number of coordinate values after template
      coordlist=0.          ! Optional list of coordinate values
!

!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=0         ! Binary scale factor
      idrstmpl(3)=15        ! Decimal scale factor
      idrstmpl(4)=21        ! Decimal scale factor
      idrstmpl(5)=0         !
      idrstmpl(6)=1         !
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute
!     idrstmpl(10)=9047     !
      idrstmpl(11)=0        !
      idrstmpl(12)=5        !
      idrstmpl(13)=1        !
      idrstmpl(14)=1        !
!     idrstmpl(15)=12       !
      idrstmpl(16)=5        !
      idrstmpl(17)=2
      idrstmpl(18)=2
!
!-- section 6:
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
!
!==============================================================================

!----read CMAQ files

      do nt=1,maxrec1

         nowdate1=nowsdate
         nowtime1=nowstime

!        print*,"hjp551,nowdate1=",nowdate1,"nowtime1=",
!     +     nowtime1
         call daymon(nowdate1,imonth1,idate1)

         ipdstmpl(15)=nt-1     ! Forecast time in units defined by

         write(chtmp,'(i2.2)')nt
         ipdstmpl(28)=1

         write(grib_id,'(i3.3)')id_gribdomain
         call baopen(51,trim(outfile)//'.f'//chtmp//'.'//grib_id//
     +                     '.grib2',ierr)

         if(ierr.ne.0) then
             print*,'can not open ',
     +             trim(outfile)//'.f'//chtmp//'.'//grib_id//'.grib2'
             stop 2001
         endif

         cgrib1=''

         ipdstmpl(22)=INT(nowdate1/1000)   ! YEAR (YYYY)
         ipdstmpl(23)=imonth1                !  Month
         ipdstmpl(24)=idate1                 !  Date
         ipdstmpl(25)=INT(nowtime1/10000)  !  Forecast hour

         cgrib2=''
         cgrib1=''

         call  gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)

         call  addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,
     +                 ideflist,idefnum,ierr)

!!
!! AOD is in 2D file, set layer=1
!!
         if(.not.read3('CHEM2D','AOD_W550_ANGST',1,
     &                 nowsdate,nowstime,aod550)) stop

         iseek=0

         do j=1,ny
            do i=1,nx
               fld1(i+(j-1)*nx)=aod550(i,j)
            enddo
         enddo

         if (maxval(fld1)==minval(fld1))then
            idrsnum=0
            print*,' changing to simple packing for constant fields'
         end if
         ibmap1=255
         bmap1=.true.
         if(any(fld1>=SPVAL))then
            ibmap1=0
            where(abs(fld1)>=SPVAL)bmap1=.false.
         endif

         fldscl=5.0
!         fldscl=8-alog10(maxval(work))     ! Units decimal scale factor

         call g2getbits(ibmap1,fldscl,size(fld1),bmap1,fld1,ibin_scl,
     +                  idec_scl,inumbits)
!         print
!         *,'idec_scl=',idec_scl,'ibin_scl=',ibin_scl,'number_bits=',inumbits

         idrstmpl(2)=ibin_scl   !
         idrstmpl(3)=idec_scl   ! SCALING POWER OF 10
         idrstmpl(4)=inumbits   ! numbe of bits used for eack packed value

         call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, 
     +                        coordlist,numcoord,idrsnum,idrstmpl, 
     +                        idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)
         call gribend(cgrib1,max_bytes,lengrib,ierr)
         call wryte(51, lengrib, cgrib1)

         call nextime(nowsdate,nowstime,10000)
      enddo     ! end do nt

      iflag=shut3()

      deallocate (aod550, cgrib1, cgrib2, bmap, bmap1, fld1, fld2)
      stop

      end
C************************************************
      SUBROUTINE RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
C
C  READ GRIB FILE
C  INPUT
C    LUGB - LOGICAL UNIT TO READ
C    LGRIB - LENGTH OF GRIB RECORD
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C  OUTPUT
C    KPDS(22) - UNPACKED PRODUCT DEFINITION SECTION
C    KGDS(20) - UNPACKED GRID DEFINITION SECTION
C    NDATA    - NUMBER OF DATA POINTS
C    LBMS(NDATA) - LOGICAL BIT MAP
C    DATA(NDATA) - DATA UNPACKED
C
      CHARACTER GRIB(LGRIB)*1
      INTEGER KPDS(25),KGDS(22),KPTR(20)
      LOGICAL LBMS(*)
      REAL DATA(*)
      NDATA=0
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
      IF(LREAD.LT.LGRIB) RETURN
      CALL W3FI63(GRIB,KPDS,KGDS,LBMS,DATA,KPTR,IRET)
      IF(IRET.NE.0) RETURN
      NDATA=KPTR(10)
c     print*,'ndata= ',ndata
      RETURN
      END
C********************************************
C********************************************
      SUBROUTINE SKGB(LUGB,ISEEK,LGRIB,LSKIP)
C
C  SEEK FOR NEXT GRIB1 RECORD WITHIN THE NEXT LSEEK=4096 BYTES
C  INPUT
C    LUGB  - LOGICAL UNIT TO READ
C    ISEEK - BYTES TO SKIP BEFORE SEARCH (SET TO 0 AT START)
C  OUTPUT
C    ISEEK - NUMBER OF BYTES READ SO FAR
C    LGRIB - LENGTH OF GRIB RECORD (0 IF NOT FOUND)
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C
      PARAMETER(LSEEK=4096)
      CHARACTER C*(LSEEK)
      CALL BAREAD(LUGB,ISEEK,LSEEK,LREAD,C)
      DO I=0,LREAD-8
        IF(C(I+1:I+4).EQ.'GRIB'.AND.MOVA2I(C(I+8:I+8)).EQ.1) THEN
          LGRIB=MOVA2I(C(I+5:I+5))*65536
     &         +MOVA2I(C(I+6:I+6))*256
     &         +MOVA2I(C(I+7:I+7))
          LSKIP=ISEEK+I
          ISEEK=LSKIP+LGRIB
          RETURN
        ENDIF
      ENDDO
      LGRIB=0
      LSKIP=0
      ISEEK=ISEEK+LSEEK
      RETURN
      END

! 
      subroutine g2getbits(ibm,scl,len,bmap,g,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total
!   number of bits,
!   The argument list is modified to have
!   ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep,
!   .false. skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
       ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF

!     write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in
!     g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range

!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
!        I1=1
!        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
!          I1=I1+1
!        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
!     write(0,*)'in
!     g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits

