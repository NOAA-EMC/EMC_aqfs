       program rdgrbwt_aot
c
c This program reads in AQF etapost generated grib files and compute
c
       parameter(izmax=22)

       integer imon,idy,ihr,iyr,icyc,itime,
     &         iseek,llgrib,llskip,kf,mm,dd,yyyy,cyc,fhr,status
       integer, allocatable :: lpbl(:)
         real,allocatable ::  var(:),vnt(:),Uwind(:,:),
     &                      Vwind(:,:),
     &      gpht(:,:),pblh(:),Usum(:),Vsum(:),wdspeed(:),
     &      ext_m(:,:),aot(:)

       dimension jpds(25),jgds(25),kpds(25),kgds(25),
     &           ifsig(izmax)

       character*14 fout1*15, date*10, fname*19, Dn2*3, Dcyc*3
       character*3 ch_im,ch_jm
        logical*1,allocatable :: lb(:) 
       INTEGER, EXTERNAL :: ENVINT

c  modifed by jphuang 02/01/2010
c       data ifsig/9980,9919,9837,9752,9662,9568,
c     +             9465,9352,9222,9054,8841,8521,
C     +             8090,7612,7089,6521,5921,5309,
C     +             4551,3678,2788,1768/
C        data ifsig/9976,9929,9881,9832,9757,9654,
c     +             9546,9429,9302,9160,8926,8531,
C     +             7957,7194,6294,5333,4362,3294,
C     +             2472,1965,1482,1084/ 
        data ifsig/10000,9953,9905,9857,9808,9707,9602,
     +             9490,9369,9236,9084,8768,8293,
     +             7621,6766,5821,4844,3881,2707,
     +             2237,1693,1271/


       imon= ENVINT('mm',' ',mm,status)
       idy = ENVINT('dd',' ',dd,status)
       iyr = ENVINT('yyyy',' ',yyyy,status)
       icyc= ENVINT('cyc',' ',cyc,status)
       ihr = icyc
       icyc= 100+ icyc
       write(Dcyc,'(i3)') icyc
       itime = ENVINT('fhr',' ',fhr,status)

        print*, iyr,imon,idy,ihr,"icyc=",icyc

        call getarg(1,ch_im)
        read(ch_im,'(i3)')im
        call getarg(2,ch_jm)
        read(ch_jm,'(i3)')jm
        jf = im * jm
        print*,"im=",im,"jm=",jm,"jf=",jf
    
        allocate(lpbl(jf))
        allocate(var(jf),vnt(jf),Uwind(jf,izmax),Vwind(jf,izmax) )
        allocate(gpht(jf,izmax),pblh(jf),Usum(jf),Vsum(jf),wdspeed(jf))
        allocate (ext_m(jf,izmax),aot(jf))
        allocate (lb(jf))
        nD2 = 100 + itime
        write(Dn2,'(i3)') nD2
        iunit=10
        fname='aqm.t'//Dcyc(2:3)//'z.mie'//Dn2(2:3)
        call baopenr(10,fname,ierr)
        print*,"After open file ",fname
         do j=1,jf
          vnt(j)=0.0
          lpbl(j)=0
         enddo

c Read grib file depending on the value of jpds(5) & jpds(7)
c                                          offload them first
        iseek=0
        call skgb(iunit,iseek,llgrib,llskip)
C --- TEST check all in argument list
C       print *,"iunit=",iunit,"iseek=",iseek,"ll=",llgrib,llskip
        do while(llgrib.gt.0)
        call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
        call skgb(iunit,iseek,llgrib,llskip)
c --- upload physical height above ground 
        do kl=1,22
         if(kpds(5).eq.8 .and.kpds(6).eq.107.and.kpds(7).
     &    eq.ifsig(kl)) then
C --- TEST max and min                                  !^^^^
          xmax=1e-5
          xmin=1e15                                     !VVVV
          do igrid=1,jf
            gpht(igrid,kl)=var(igrid)
C --- TEST max and min                                  !^^^^
            if(var(igrid).gt.xmax) xmax=var(igrid)
            if(var(igrid).lt.xmin) xmin=var(igrid)      !VVVV
          enddo
C --- TEST max and min                                        !^^^^
C           print*,"level=",ifsig(kl),"max=",xmax,"min=",xmin !VVVV
         endif
       enddo


c --- upload Mie Extinction coefficient
        do kl=1,22
         if(kpds(5).eq.128.and.kpds(6).eq.107.and.kpds(7).
     &    eq.ifsig(kl)) then
           do igrid=1,jf
              ext_m(igrid,kl)=var(igrid)
              ext_m(igrid,kl)=ext_m(igrid,kl)-0.01 !! remove Rayleigh SSOBHA 2005 
              ext_m(igrid,kl)=max(ext_m(igrid,kl),0.0) !! remove R
           enddo
          endif
        enddo

c --- determine Aerosol Optical Thickness:
      do igrid=1,jf
         aot(igrid)=0.0
         do lh=1,22                  ! modified by jphuang 04/01/2010
          if ( lh .eq. 1 )then
            aot(igrid) = aot(igrid) + 1e-3*
     &      gpht(igrid,lh)*ext_m(igrid,lh)
          else
            aot(igrid) = aot(igrid) + 1e-3*
     &      (gpht(igrid,lh)-gpht(igrid,lh-1))*ext_m(igrid,lh)
          endif
         enddo
      enddo

       enddo ! llgrib loop
        call baclose(10,ierr)

c --- write out vnt in grib format

       fout1='aqm.t'//Dcyc(2:3)//'z.aot'//Dn2(2:3)
       call baopen(51,fout1,ierr)
       kpds(8)=iyr-(iyr/100)*100
       kpds(21)=21
       kpds(9)=imon
       kpds(10)=idy
       kpds(11)=ihr
       kpds(12)=0

       kpds(5)=129
       kpds(6)=200
       kpds(7)=0
       kpds(19)=141
       kpds(22)=3.7
       print*,'prob_aot= ',(aot(i),i=1,10)
       call putgb(51,jf,kpds,kgds,lb,aot,iret)
       print*,'Finished ',itime,' with enswrite'


        deallocate(lpbl)
        deallocate(var,vnt,Uwind,Vwind )
        deallocate(gpht,pblh,Usum,Vsum,wdspeed)
        deallocate (ext_m,aot)
        deallocate (lb)


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

