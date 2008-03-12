
       character*25 chbuf 
       real freq,dist,theta 
       integer IYEAR,MONTH,IDAY,JLDAYN,IDAYYR
       call getarg(1,chbuf) 
       read(chbuf,*) IYEAR
       call getarg(2,chbuf) 
       read(chbuf,*) MONTH
       call getarg(3,chbuf) 
       read(chbuf,*) IDAY

       JLDAYN  =    
     &       IDAY - 32075
     &       + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
     &       + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
     &       - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4

       IDAYYR = JLDAYN -
     &  (-31739 +1461 * (IYEAR+4799) / 4 - 3 * ((IYEAR+4899)/100)/4)

c       print*, IYEAR,MONTH,IDAY,JLDAYN,IDAYYR
        print *, IDAYYR
        end

