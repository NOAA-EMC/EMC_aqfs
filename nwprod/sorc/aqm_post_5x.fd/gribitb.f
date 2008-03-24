      subroutine gribitb(ln,ozout,im,jm,iunit,KPDSOUT)

      parameter (mxbit=16,lenpds=28,lengds=32)
      character*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)

      character*1  iflag*1, pds*28
      integer ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
      integer KPDSOUT(25),id(25),kgds(20)
      real ozout(im,jm)
CC      save kbuf
C****************************************************************
C     PREPARE GRIB PDS
C     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
        
      id(1) =28 !NUMBER OF BYTES IN PRODUCT DEFINITION SECTION(PDS)
      id(2) =KPDSOUT(19)!PARAMETER TABLE VERSION NO (2 or 129 or 130)
      id(3) =KPDSOUT(1)  !IDENTIFICATION OF ORIGINATING CENTER
      id(4) =KPDSOUT(2)!MODEL IDENTIFICATION (BY ORIGINATING CENTER)
      id(5) =KPDSOUT(3)!GRID IDENTIFICATION
      id(6) =1  !IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
      id(7) =0  !IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
      id(8) =KPDSOUT(5)
      id(9) = 107                !Had temporarily for 5x made into 105
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
      id(23)=21                  !  CENTURY 
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
      call get_bits(ibm,sgdg,nout,ibmap,ozout,
     &              ideci,ozout,gmin,gmax,nbit)

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

      call w3fi72(itype,ozout,igrd,ibitl,
     &            ipflag,id,pds,
     &            igflag,igrid,igds,icomp,
     &            ibflag,ibmap,iblen,
     &            ibdsfl,
     &            npts,kbuf,itot,ier)


      call wryte(iunit,itot,kbuf)

      return
      end

