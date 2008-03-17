      subroutine cmaq_inhouse(N,M,NFLDS_INT,KPDSIN,KPDSOUT,K5PDS,
     &   MDLID,KGRIDA,MAXG,MAXF,INHOUSE,EXT_MIE,fnamepre)

      integer   KPDSIN(25),KPDSOUT(25),K5PDS(7,MAXF),
     &          IOUTUN(MAXG,MAXF),MDLID(MAXG,MAXF),
     &          KGRIDA(MAXG,MAXF),N,NFLDS_INT
      character Dn2*3, fnamepre(MAXG)*80, ozonefile*80
      integer   ii,jj,ncols,nrows,nlays,nspcs,nrec,sdate,stime,
     &          iyear,month,iday,nout,kozone
      real      asigo(22)
      logical   matched,INHOUSE,EXT_MIE
      integer   ispec(180:226), nsp_len
      character chform(180:226)*8,spname*8,spcname(200)*16
      INCLUDE   "SPECIES.comm"
      data asigo/0.9975,0.9915,0.9835,0.9745,0.965,0.949,0.926,
     &     0.9015,0.8755,0.848,0.819,0.789,0.7585,0.7185,0.669,
     &     0.618,0.547,0.455,0.3595,0.2605,0.158,0.053/
      data ispec /180,      181,      182,      183,
     &184,      185,      186,      187,      188,      189,
     &190,      191,      192,      193,      194,      195,
     &196,      197,      198,      199,      200,      201,
     &202,      203,      204,      205,      206,      207,
     &208,      209,      210,      211,      212,      213,
     &214,      215,      216,      217,      218,      219,
     &220,      221,      222,      223,      224,      225,
     &226/
      data chform/'O3     ','       ','       ','       ',
     &'NO2    ','NO     ','NO3    ','N2O5   ','HNO3   ','HONO   ',
     &'PNA    ','CO     ','FORM   ','ALD2   ','PAN    ','NTR    ',
     &'XO2N   ','ISOP   ','ISPD   ','SO2    ','NH3    ','PM25   ',
     &'PM100  ','ASO4I  ','ASO4J  ','ANH4I  ','ANH4J  ','ANO3I  ',
     &'ANO3J  ','AORGAI ','AORGAJ ','AORGPAI','AORGPAJ','AORGBI ',
     &'AORGBJ ','AECI   ','AECJ   ','A25I   ','A25J   ','NUMATKN',
     &'NUMACC ','SRFATKN','SRFACC ','AH2OI  ','AH2OJ  ','EXT_Mie',
     &'TEMP1P5'/
             call notcdf_in(ncols,nrows,nlays,nspcs,nrec,
     &                      N,sdate,stime,spcname)
             iyear=int(sdate/1000)
             call daymon(sdate,month,iday)

       if(NFLDS_INT.gt.nspcs*nlays) then
        write(*,*) 
     & "Error: Ctl Specified more species*nlays than available "
        stop 99
       endif
             print*,"crlspcs=",ncols,nrows,nlays,nspcs,
     &       "sdate=",sdate,"month=",month,"iday=",iday,
     &       "nrec=",nrec

            nout = ncols*nrows
            KPDSOUT=KPDSIN
            KPDSOUT(1)=7
            KPDSOUT(8) =iyear-2000  !  YEAR OF CENTURY
            KPDSOUT(9) =month       !  MONTH OF YEAR
            KPDSOUT(10)=iday        !  DAY OF MONTH
            KPDSOUT(11)=stime*1e-4  !  HOUR OF DAY
            KPDSOUT(13)=1           !  INDICATOR OF FCST TIME UNIT
            do 330 ln = 1,NFLDS_INT
               ioutun(M,ln) = 63
               if(ln.eq.1) then
                  nD2 = 100+N
                  write(Dn2,'(i3)') nD2
                  ozonefile=fnamepre(ln)
                  kozone=index(ozonefile,' ')-1
                  ozonefile= ozonefile(1:kozone)//Dn2(2:3)
                  call baopenw(ioutun(M,ln),ozonefile,ier)
               endif
               KPDSOUT(2)=MDLID(M,ln)
               KPDSOUT(3)=KGRIDA(M,ln)
               KPDSOUT(5)=K5PDS(1,ln)    !INDICATOR OF PARAMETER
               KPDSOUT(6)=K5PDS(2,ln)
               KPDSOUT(7)=K5PDS(3,ln)*256+K5PDS(4,ln)
              kk=1
              do ik=1,22
C##                 if(abs(KPDSOUT(7)*1e-4-asigo(ik)).le.1e-7) kk=ik 
               enddo
               KPDSOUT(14)=N-1
               KPDSOUT(15)=N
               KPDSOUT(16)=3
               KPDSOUT(17)=3
               if(EXT_MIE) then     ! instantaneous quantity file
                  KPDSOUT(11)=(stime-100)*1e-4
                  KPDSOUT(14)=N
                  KPDSOUT(15)=0
                  KPDSOUT(16)=0
                  KPDSOUT(17)=0
               endif
               KPDSOUT(19)=K5PDS(6,ln)
               lsp=1
               matched=.false.
               do l_sp=180,226
                 if(.not.matched.and.KPDSOUT(5).eq.l_sp) then
                   nsp_len=index(chform(l_sp),' ')-1
                   spname=chform(l_sp)(1:nsp_len)
                   fctr=1
                   do m_sp=1,nspcs
                     if(.not.matched.and.spname.eq.spcname(m_sp)) then
                        matched=.true.
                        lsp=m_sp
                        if(l_sp.le.200) fctr=1e3
                     endif
                   enddo
                 endif
               enddo
              do jj=1,nrows
                 do ii=1,ncols
                    ozout(ii,jj)=species(ii,jj,kk,lsp)*fctr
                 enddo
              enddo
              call gribitb(ln,ozout,ncols,nrows,
     &                     ioutun(M,ln),KPDSOUT)

  330      continue
       return
       end
