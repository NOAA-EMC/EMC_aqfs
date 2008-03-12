      program rdgrbwt_60hyb_hrly

c
c This program reads in WRF-post 60 hybrid levels in pressure and
c interpolate gfs_o3 spatially and temporally. 
c
       parameter(im=468, jm=288, jf=im*jm)

       integer imon,idy,ihr,iyr,icyc,itime,
     &         iseek,llgrib,llskip,kf,ifd,
     &         mm,dd,yyyy,cyc,fhr,status
       real var(jf),O31(jf),O32(jf),O33(jf),
     &      O3b(jf,60),O3e(jf,60)

       dimension jpds(25),jgds(25),kpds(25),kgds(25),
     &           ifsig(47)

       character date*10, fname*19, Dn2*3, Dcyc*3
       character fout1*16, fout2*16, fout3*16
       logical*1 lb(jf)
       INTEGER, EXTERNAL :: ENVINT

       imon= ENVINT('mm',' ',mm,status)
       idy = ENVINT('dd',' ',dd,status)
       iyr = ENVINT('yyyy',' ',yyyy,status)
       icyc= ENVINT('cyc',' ',cyc,status)
       write(Dcyc,'(i3)') 100+icyc
       ihr = ENVINT('cyc',' ',cyc,status)
       itime = ENVINT('fhr',' ',fhr,status)

       print*, iyr,imon,idy,ihr,"icyc=",icyc

        nD2 = 100 + itime
        write(Dn2,'(i3)') nD2

        iunit=10
        
        fname='aqm.t'//Dcyc(2:3)//'z.O3h'//Dn2(2:3)
        call baopenr(10,fname,ierr)
        print*,"After open file ",fname
     
        do k=1,60
        do j=1,jf
          O3b(j,k)=0.0 
        enddo
        enddo

        iseek=0
        call skgb(iunit,iseek,llgrib,llskip)
        print*,"iseek=",iseek,"llgrib=",llgrib,"llskip=",llskip
        do while(llgrib.gt.0)
          call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
          call skgb(iunit,iseek,llgrib,llskip)
          do kl=1,60
           if(kpds(5).eq.154.and.kpds(6).eq.109.and.kpds(7).
     &     eq.kl) then
C --- TEST max and min                                  !^^^^
           xmax=1e-15
           xmin=1e15                                     !VVVV
           do igrid=1,jf
            O3b(igrid,kl)=var(igrid)
C --- TEST max and min                                  !^^^^
            if(var(igrid).gt.xmax) xmax=var(igrid)
            if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
           enddo
C --- TEST max and min                                        !^^^^
            print*,"level=",kl,"maxb=",xmax,"minb=",xmin !VVVV
           endif
          enddo
        enddo
        call baclose(10,ierr)

        if(itime.ne.84) nD2=100+itime+3
        write(Dn2,'(i3)') nD2
        iunit=11
        fname='aqm.t'//Dcyc(2:3)//'z.O3h'//Dn2(2:3)
        call baopenr(11,fname,ierr)
        print*,"After open file ",fname

        do k=1,60
        do j=1,jf
          O3e(j,k)=0.0 
        enddo
        enddo

        iseek=0
        call skgb(iunit,iseek,llgrib,llskip)
        print*,"iseek=",iseek,"llgrib=",llgrib,"llskip=",llskip
        do while(llgrib.gt.0)
          call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
          call skgb(iunit,iseek,llgrib,llskip)
          do kl=1,60
           if(kpds(5).eq.154.and.kpds(6).eq.109.and.kpds(7).
     &     eq.kl) then
C --- TEST max and min                                  !^^^^
           xmax=1e-15
           xmin=1e15                                     !VVVV
           do igrid=1,jf
            O3e(igrid,kl)=var(igrid)
C --- TEST max and min                                  !^^^^
            if(var(igrid).gt.xmax) xmax=var(igrid)
            if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
           enddo
C --- TEST max and min                                        !^^^^
            print*,"level=",kl,"maxe=",xmax,"mine=",xmin !VVVV
           endif
          enddo
        enddo
        call baclose(11,ierr)

c---- temporal interpolation into hourly data
        t1d=0.333333
        t2d=0.666667
        nD2 = 100 + itime
        write(Dn2,'(i3)') nD2
        fout1='aqm.t'//Dcyc(2:3)//'z.O3hb'//Dn2(2:3)
        call baopen(51,fout1,ierr)
        nD2 = 100 + itime+1
        write(Dn2,'(i3)') nD2
        fout2='aqm.t'//Dcyc(2:3)//'z.O3hb'//Dn2(2:3)
        call baopen(52,fout2,ierr)
        nD2 = 100 + itime+2
        write(Dn2,'(i3)') nD2
        fout3='aqm.t'//Dcyc(2:3)//'z.O3hb'//Dn2(2:3)
        call baopen(53,fout3,ierr)
        do kl=1,60
         do j=1,jf
          O31(j)=O3b(j,kl)
          O32(j)=O3b(j,kl)*t2d+O3e(j,kl)*t1d
          O33(j)=O3b(j,kl)*t1d+O3e(j,kl)*t2d
          if(kl.eq.1.and.j.eq.1) print*,"O3123=",O31(j),O32(j),O33(j)
         enddo   
c --- write out O3h in grib format
         kpds(13)=1
         kpds(14)=itime+6
         kpds(5)=154
         kpds(6)=109
         kpds(7)=kl
         kpds(19)=2
         kpds(22)=9
         if(kl.eq.45) print*,itime,"just before dump b123e",
     &  O3b(1,45),O31(1),O32(1),O33(1),O3e(1,45)
         call putgb(51,jf,kpds,kgds,lb,O31,iret)
CCCJUNE5         kpds(11)=ihr
         kpds(12)=0
         kpds(13)=1
         kpds(14)=itime+7
         kpds(5)=154
         kpds(6)=109
         kpds(7)=kl
         kpds(19)=2
         kpds(22)=9
         call putgb(52,jf,kpds,kgds,lb,O32,iret)
CCCJUNE5         kpds(11)=ihr
         kpds(12)=0
         kpds(13)=1
         kpds(14)=itime+8
         kpds(5)=154
         kpds(6)=109
         kpds(7)=kl
         kpds(19)=2
         kpds(22)=9
         call putgb(53,jf,kpds,kgds,lb,O33,iret)
        enddo   
        call baclose(51,ierr)
        call baclose(52,ierr)
        call baclose(53,ierr)
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

