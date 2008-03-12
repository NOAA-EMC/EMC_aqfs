      program rdgrbwt_60hyb_gfs

c
c This program reads in WRF-post 60 hybrid levels in pressure and
c interpolate gfs_o3 spatially and temporally. 
c
       parameter(im=468, jm=288, jf=im*jm)

       integer imon,idy,ihr,iyr,icyc,itime,
     &         iseek,llgrib,llskip,kf,ifd,
     &         mm,dd,yyyy,cyc,fhr,status
       real var(jf),prss(jf,0:60),O3h(jf),O3i(jf,47)

       dimension jpds(25),jgds(25),kpds(25),kgds(25),
     &           ifsig(47)

       character*14 fout1*16,date*10,fname*19,Dn2*3,Dcyc*3
       logical*1 lb(jf)
       INTEGER, EXTERNAL :: ENVINT

       data ifsig/1000, 975, 950, 925, 900, 875, 850, 825,
     & 800, 775, 750, 725, 700, 675, 650, 625, 600,
     & 575, 550, 525, 500, 475, 450, 425, 400, 375,
     & 350, 325, 300, 275, 250, 225, 200, 175, 150,
     & 125, 100,  70,  50,  30,  20,  10,   7,   5,
     &   3,   2,   1/

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

        iunit=11
        fname='aqm.t'//Dcyc(2:3)//'z_O3_'//Dn2(2:3)
        call baopenr(11,fname,ierr)
        print*,"After open file ",fname
        do j=1,jf
        do ki=1,47
          O3i(j,ki)=0.0 
        enddo
        enddo
        iseek=0
        call skgb(iunit,iseek,llgrib,llskip)
C----Succeeded readgrib?
        print*,"iseek=",iseek,"llgrib=",llgrib,"llskip=",llskip
        do while(llgrib.gt.0)
          call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
          call skgb(iunit,iseek,llgrib,llskip)
          do kl=1,47
           if(kpds(5).eq.154.and.kpds(6).eq.100.and.kpds(7).
     &     eq.ifsig(kl)) then
C --- TEST max and min                                  !^^^^
           xmax=1e-15
           xmin=1e15                                     !VVVV
           do igrid=1,jf
            O3i(igrid,kl)=var(igrid)
C --- TEST max and min                                  !^^^^
            if(var(igrid).gt.xmax) xmax=var(igrid)
            if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
           enddo
C --- TEST max and min                                        !^^^^
            print*,"level=",ifsig(kl),"maxo=",xmax,"mino=",xmin !VVVV
           endif
          enddo
          if(kpds(5).eq.1) then
           xmax=1e-15
           xmin=1e15                                     !VVVV
           do igrid=1,jf
            prss(igrid,0)=var(igrid)
C --- TEST max and min                                  !^^^^
            if(var(igrid).gt.xmax) xmax=var(igrid)
            if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
           enddo
C --- TEST max and min                                        !^^^^
            print*,"level=",ifsig(kl),"maxs=",xmax,"mins=",xmin !VVVV
          endif
        enddo
        call baclose(11,ierr)

c --- write out O3h in grib format
       fout1='aqm.t'//Dcyc(2:3)//'z.O3h'//Dn2(2:3)
       call baopen(51,fout1,ierr)

        iunit=10
        fname='aqm.t'//Dcyc(2:3)//'z_O3_'//Dn2(2:3) 
        call baopenr(10,fname,ierr)
        print*,"After open file ",fname
        do lk=0,60
        do j=1,jf
          prss(j,lk)=0.0
         enddo
        enddo

        do j=1,jf
          O3h(j)=0.0 
        enddo
c Read grib file depending on the value of jpds(5) & jpds(7)
c                                          offload them first
        iseek=0
        call skgb(iunit,iseek,llgrib,llskip)
        print*,"iseek=",iseek,"llgrib=",llgrib,"llskip=",llskip
C --- TEST check all in argument list
C       print *,"iunit=",iunit,"iseek=",iseek,"ll=",llgrib,llskip
        do while(llgrib.gt.0)
          call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
          call skgb(iunit,iseek,llgrib,llskip)
c --- upload geopotential heights in pascals
          do kl=60,1,-1
          if(kpds(5).eq.1.and.kpds(6).eq.109.and.kpds(7).eq.kl) then
C --- TEST max and min                                  !^^^^
            xmax=1e-15
            xmin=1e15                                     !VVVV
            do igrid=1,jf
               prss(igrid,kl)=var(igrid)
C --- TEST max and min                                  !^^^^
              if(var(igrid).gt.xmax) xmax=var(igrid)
              if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
            enddo
C --- TEST max and min                                        !^^^^
            print*,"level=",kl,"maxp=",xmax,"minp=",xmin !VVVV
          endif
          enddo
         enddo
c---- search for prss just below ifsig:
          do kl=60,1,-1
           do igd=1,jf
             ifd=0
             do ih=1,46
               if(ifd.eq.0.and.prss(igd,kl).le.ifsig(ih)*100
     &     .and.prss(igd,kl).ge.ifsig(ih+1)*100) then
                 h=0.01/(ifsig(ih)-ifsig(ih+1))
                 ha=prss(igd,kl)-ifsig(ih+1)*100
                 hb=ifsig(ih)*100-prss(igd,kl)
                 O3h(igd)=h*(hb*O3i(igd,ih+1)+ha*O3i(igd,ih))
                 ifd=1
C--
C      if(igd.eq.1) print*,"kl=",kl,"prss=",prss(igd,kl),
C     & "ifsig*100=",ifsig(ih)*100,ifsig(ih+1)*100,"h=",h,
C     & "ha,hb'",ha,hb,"(ha+hb)*h=",(ha+hb)*h,
C     & "O3i=",O3i(igd,ih+1),O3i(igd,ih),"O3h=",O3h(igd)
               endif
             enddo
             if(ifd.eq.0) O3h(igd)=O3i(igd,1)
           enddo
c --- write out O3h in grib format
         kpds(5)=154
         kpds(6)=109
         kpds(7)=kl
         kpds(19)=2
         kpds(22)=9
C         if(kl.eq.1) print*,"just before dump",O3h(1)
         call putgb(51,jf,kpds,kgds,lb,O3h,iret)
        enddo
        call baclose(10,ierr)
        call baclose(51,ierr)

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

