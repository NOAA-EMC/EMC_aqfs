      PROGRAM RAWINS2IOAPI

C  This program reads obs upper air data and writes them to a 
C  NetCDF file.
C
C  Written by MCNC, copyright 2002.

      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'    !  I/O API parameters
      INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
      INCLUDE 'FDESC3.EXT'    !  I/O API file description data structures.

C........  Enternal functions

      LOGICAL         DSCGRID
      INTEGER         LDEV      !  UNIT NUMBER FOR LOG FILE
      INTEGER         PROMPTFFILE
      CHARACTER*5     STRING
      INTEGER         JULIAN, JSTEP3
      CHARACTER*16    PROMPTMFILE
      CHARACTER*16    PROGNAME
      DATA            PROGNAME /'RAWINS2IOAPI'/
      REAL            GETREAL
      LOGICAL         GETYN

      EXTERNAL        JULIAN, DSCGRID, PROMPTFFILE,
     &                PROMPTMFILE, GETREAL, GETYN

      CHARACTER*16, PARAMETER::    RAWINS_NCF_OBS = 'RAWINS_NCF_OBS'
      CHARACTER*256   MESG      !  message buffer

      integer  inmxlvl,inmxstn,stnum,yymmdd,hhmm,yy,mm,dd,hh,mi,icnt,
     &         maxlev,outmxstn,icur,it,i,j,k,yr,itime,ilev,icnt00,
     &         icnt12,maxsites,n,imiss,jdate,jtime,nprof
      parameter (inmxlvl=200,inmxstn=400,outmxstn=200)
      real     inlat(inmxstn),inlon(inmxstn),inelev(inmxstn),lat,
     &         lon,elev,amis,td,tf,ws,wd,pr,ht
      real     inpres(inmxstn,inmxlvl),intmpf(inmxstn,inmxlvl),
     &         indptf(inmxstn,inmxlvl),inwdir(inmxstn,inmxlvl),
     &         inwspd(inmxstn,inmxlvl),inhght(inmxstn,inmxlvl)
      integer  instnum(inmxstn),inyymmdd(inmxstn),inhhmm(inmxstn),
     &         innumlev(inmxstn),injdate(inmxstn),injtime(inmxstn)
      character*4 inid(inmxstn),id,instim(inmxstn),aflag1,aflag2,
     &            aflag3,aflag4,aflag5,aflag6
      character*55 rawdat(inmxlvl)
      character infile*80, junk*1

      integer   outid(outmxstn),outlevs(outmxstn)
      real      outtd(inmxlvl,outmxstn),outws(inmxlvl,outmxstn),
     &          outtf(inmxlvl,outmxstn),outpr(inmxlvl,outmxstn),
     &          outht(inmxlvl,outmxstn),outwd(inmxlvl,outmxstn)
      REAL      outlat(outmxstn),outlon(outmxstn),outelev(outmxstn)

      COMMON /BAR/ NPROF,OUTID,OUTLEVS,OUTLON,OUTLAT,OUTELEV,
     &             OUTHT,OUTPR,OUTTF,OUTTD,OUTWD,OUTWS 

      amis = -9998.00

      write(*,*) ' Enter name of input file'
      read(*,'(a80)') infile
      write(*,*) ' Reading from file: ', infile

      open (10,file=infile,form='formatted',status='old')

C  Scroll through obs file to get starting point

      read (10,50) junk
50    format(a)
      read (10,50) junk
      read (10,50) junk

C  Now, start processing the rawins obs

      icnt = 0
      maxlev = 0
100   read (10,101,end=888,err=999) aflag1,id,aflag2,stnum,aflag3,
     &                      yy,mm,dd,hh,mi
101   format(1x,a4,3x,a4,10x,a4,6x,i5,3x,a4,3x,3i2.2,1x,2i2.2)
      if (aflag1.ne.'STID' .or. aflag2.ne.'STNM' 
     &    .or. aflag3.ne.'TIME') then
        write(*,*) ' Bad flags encountered'
        write(*,*) ' Expected STID STNM TIME'
        write(*,*) ' Found ', aflag1, ' ', aflag2, ' ', aflag3
        write(*,*) ' id: ', id
        stop
      end if
      icnt = icnt + 1
      inid(icnt) = id
      instnum(icnt) = stnum
      inyymmdd(icnt) = yy*10000 + mm*100 + dd
      inhhmm(icnt) = hh*100 + mi
      yr = 2000 + yy
      injdate(icnt) = 1000*yr + JULIAN (yr, mm, dd)
      injtime(icnt) = hh*10000 + mi*100 
   
      read (10,102,end=888) aflag1,lat,aflag2,lon,aflag3,elev
102   format(1x,a4,3x,f6.2,5x,a4,4x,f7.2,3x,a4,3x,f7.1)
      if (aflag1.ne.'SLAT' .or. aflag2.ne.'SLON' 
     &    .or. aflag3.ne.'SELV') then
        write(*,*) ' Bad flags encountered'
        write(*,*) ' Expected SLAT SLON SELV'
        write(*,*) ' Found ', aflag1, ' ', aflag2, ' ', aflag3
        write(*,*) ' id: ', id
        stop
      end if
      inlat(icnt) = lat
      inlon(icnt) = lon
      inelev(icnt) = elev
      read (10,103) aflag1, itime
103   format (1x,a4,3x,i5)
      if (aflag1.ne.'STIM') then
        write(*,*) ' Bad flag encountered'
        write(*,*) ' Expected STIM'
        write(*,*) ' Found ', aflag1
        write(*,*) ' id: ', id
        stop
      end if

      read(10,50) junk
      read (10,104) aflag1,aflag2,aflag3,aflag4,aflag5,aflag6
104   format (1x,6(5x,a4))
      if (aflag1.ne.'PRES' .or. aflag2.ne.'TMPF' .or. 
     &    aflag3.ne.'DWPF' .or. aflag4.ne.'DRCT' .or. 
     &    aflag5.ne.'SKNT' .or. aflag6.ne.'HGHT') then
        write(*,*) ' Bad flags encountered'
        write(*,*) ' Expected PRES TMPF DWPF DRCT SKNT HGHT'
        write(*,*) ' Found ', aflag1, ' ', aflag2, ' ', aflag3,
     &                   ' ', aflag4, ' ', aflag5, ' ', aflag6
        write(*,*) ' id: ', id
      end if

C  We're now at the start of the actual sounding. Read and store.

      ilev = 1
110   read (10,120,end=888) rawdat(ilev)
120   format(a55)
      read(rawdat(ilev),'(5x,a5)') string
      if (string.eq.'     ') then           ! End of sounding reached
        ilev = ilev - 1                     ! We've overcounted
        do i = 1, ilev
          read(rawdat(i),'(1x,6f9.2)') inpres(icnt,i), intmpf(icnt,i),
     &    indptf(icnt,i),inwdir(icnt,i),inwspd(icnt,i),inhght(icnt,i)
        end do
        if (ilev.gt.maxlev) maxlev = ilev
        innumlev(icnt) = ilev
      else
        ilev = ilev + 1
        go to 110
      end if

C  This sounding has been completely read and stored. Go read the next one. 

      go to 100

888   continue

      print *, ' Number of soundings encountered: ', icnt
      print *, ' Max levels encountered: ', maxlev

C  With all the sounding info in memory, let's process them

      icnt00 = 0
      icnt12 = 0
      do i = 1, icnt
        if (inhhmm(i).eq.0000) icnt00 = icnt00 + 1
        if (inhhmm(i).eq.1200) icnt12 = icnt12 + 1
      end do

      if (icnt00.gt.icnt12) then
        maxsites = icnt00
      else
        maxsites = icnt12
      end if

C  Let's put the appropriate data in the output arrays, starting with the
C  00Z data. If the levels are below ground, rearrange them so that they
C  end up in decreasing pressure / increasing height order.

      LDEV = INIT3 ( )        !  Start up I/O API
     
      jdate = injdate(1)
      jtime = injtime(1)

      P_ALP3D = 0.0D0
      P_BET3D = 0.0D0
      P_GAM3D = 0.0D0
      XCENT3D = 0.0D0
      YCENT3D = 0.0D0
      XORIG3D = 0.0D0
      YORIG3D = 0.0D0
      XCELL3D = 0.0D0
      YCELL3D = 0.0D0
      FTYPE3D = PROFIL3
      SDATE3D = JDATE
      STIME3D = JTIME
      TSTEP3D = 120000
      NCOLS3D = inmxlvl
      NROWS3D = outmxstn
      NLAYS3D = 1
      NTHIK3D = 1
      GDTYP3D = LATGRD3
      VGTYP3D = IMISS3
      VGTOP3D = BADVAL3

      N = 1
      VNAME3D(N) = 'PRES'
      UNITS3D(N) = 'mb'
      VDESC3D(N) = 'Pressure level'
      VTYPE3D(N) = M3REAL

      N = N + 1
      VNAME3D(N) = 'TF'
      UNITS3D(N) = 'deg F'
      VDESC3D(N) = 'Observed temperature at sounding level'
      VTYPE3D(N) = M3REAL

      N = N + 1
      VNAME3D(N) = 'TDF'
      UNITS3D(N) = 'deg F'
      VDESC3D(N) = 'Observed dew point at sounding level'
      VTYPE3D(N) = M3REAL

      N = N + 1
      VNAME3D(N) = 'WDIR'
      UNITS3D(N) = 'deg'
      VDESC3D(N) = 'Observed wind direction at sounding level'
      VTYPE3D(N) = M3REAL

      N = N + 1
      VNAME3D(N) = 'WSPD'
      UNITS3D(N) = 'knots'
      VDESC3D(N) = 'Observed wind speed at sounding level'
      VTYPE3D(N) = M3REAL

      NVARS3D = N
      GDNAM3D = 'RAWINSONDE'

      FDESC3D( 1 ) = 'Heights are above MSL'

      IF (.NOT.OPEN3('RAWINS_NCF_OBS',FSUNKN3,PROGNAME))THEN
          CALL M3EXIT( PROGNAME, JDATE, JTIME,
     &                 'Could not open file "RAWINS_NCF_OBS"', 2 )
      END IF

      do it = 1, 2
       icur = 0
       do i = 1, icnt
        if (injdate(i).eq.jdate .and. injtime(i).eq.jtime) then
          icur = icur + 1
          outid(icur) = instnum(i)
          outlat(icur) = inlat(i)
          outlon(icur) = inlon(i)
          outelev(icur) = inelev(i)
          ilev = innumlev(i)
          outlevs(icur) = ilev
          imiss = 0
          do k = 2, ilev
            pr = inpres(i,k)
            ht = inhght(i,k)
            tf = intmpf(i,k)
            td = indptf(i,k)
            wd = inwdir(i,k)
            ws = inwspd(i,k)
            if (pr.gt.0.0 .and. ht.gt.amis .and. tf.lt.amis .and.
     &          td.lt.amis .and. wd.lt.amis .and. ws.lt.amis)
     &                                                        then
              imiss = imiss + 1
            else
              go to 900
            end if
          end do
900       continue
          if (imiss.eq.0) then
            do k = 1, ilev
              pr = inpres(i,k) 
              ht = inhght(i,k)
              tf = intmpf(i,k)
              td = indptf(i,k)
              wd = inwdir(i,k)
              ws = inwspd(i,k)
              outpr(k,icur) = pr
              outht(k,icur) = ht
              outtf(k,icur) = tf
              outtd(k,icur) = td
              outws(k,icur) = ws
              outwd(k,icur) = wd
            end do
          else
            do k = 2, imiss+1
              pr = inpres(i,k)
              ht = inhght(i,k)
              tf = intmpf(i,k)
              td = indptf(i,k)
              wd = inwdir(i,k)
              ws = inwspd(i,k)
              outpr(k-1,icur) = pr
              outht(k-1,icur) = ht
              outtf(k-1,icur) = tf
              outtd(k-1,icur) = td
              outws(k-1,icur) = ws
              outwd(k-1,icur) = wd
            end do
            k = imiss
            pr = inpres(i,1)
            ht = inhght(i,1)
            tf = intmpf(i,1)
            td = indptf(i,1)
            wd = inwdir(i,1)
            ws = inwspd(i,1)
            outpr(k+1,icur) = pr
            outht(k+1,icur) = ht
            outtf(k+1,icur) = tf
            outtd(k+1,icur) = td
            outws(k+1,icur) = ws
            outwd(k+1,icur) = wd
            do k = imiss+2, ilev
              pr = inpres(i,k)
              ht = inhght(i,k)
              tf = intmpf(i,k)
              td = indptf(i,k)
              wd = inwdir(i,k)
              ws = inwspd(i,k)
              outpr(k,icur) = pr
              outht(k,icur) = ht
              outtf(k,icur) = tf
              outtd(k,icur) = td
              outws(k,icur) = ws
              outwd(k,icur) = wd
            end do
            do k = 1, ilev
              if (outpr(k,icur) .lt. amis) outpr(k,icur) = BADVAL3
              if (outht(k,icur) .lt. amis) outht(k,icur) = BADVAL3
              if (outtf(k,icur) .lt. amis) outtf(k,icur) = BADVAL3
              if (outtd(k,icur) .lt. amis) outtd(k,icur) = BADVAL3
              if (outws(k,icur) .lt. amis) outws(k,icur) = BADVAL3
              if (outwd(k,icur) .lt. amis) outwd(k,icur) = BADVAL3
            end do
            do k = ilev+1,maxlev
              outpr(k,icur) = BADVAL3
              outht(k,icur) = BADVAL3
              outtf(k,icur) = BADVAL3
              outtd(k,icur) = BADVAL3
              outws(k,icur) = BADVAL3
              outwd(k,icur) = BADVAL3
            end do
          end if
        end if
       end do

C  Write out the data

       NPROF = icur 

       print *, ' Max sites (outmxstn): ', outmxstn
       print *, ' Max levels (inmxlvl): ', inmxlvl
       print *, ' nprof: ', nprof
       print *, ' outid(1): ', outid(1)
       print *, ' outid(6): ', outid(6)
       print *, ' outlevs(1): ', outlevs(1) 
       print *, ' outlevs(6): ', outlevs(6)
       print *, ' outlon(1): ', outlon(1)
       print *, ' outlon(6): ', outlon(6)
       print *, ' outlat(1): ', outlat(1)
       print *, ' outlat(6): ', outlat(6)
       print *, ' outelev(1): ', outelev(1)
       print *, ' outelev(6): ', outelev(6)
       do i = 1, outlevs(1)
         write (*,222) outht(i,1), outpr(i,1), outtf(i,1)
222      format ( ' Ht, pr, tf for stn 1: ', 3f9.2)
       end do
       do i = 1, outlevs(6)
         write (*,223) outht(i,6), outpr(i,6), outtf(i,6)
223      format ( ' Ht, pr, tf for stn 6: ', 3f9.2)
       end do

       print *, ' jdate: ', jdate
       print *, ' jtime: ', jtime
       IF ( .NOT.WRITE3( RAWINS_NCF_OBS, 'ALL', JDATE,
     &                   JTIME, NPROF ) ) THEN
            MESG = 'Error writing variable sounding data'
            CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
        END IF

       CALL NEXTIME(JDATE,JTIME,120000)

      end do

      CALL M3EXIT(PROGNAME,0,0,'Normal completion',0)

      stop

999   print *, ' Error reading id line'

      stop

      end

