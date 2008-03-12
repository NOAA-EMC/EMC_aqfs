      subroutine gfs_derive(iloop,KPDSIN,K5PDS,JPDS5,FOUT,MAXG,
     &           fnamepre,MAXF,JMAXOT,NOUT,LOUT,MDLID,KGRIDA,
     &           N,NFLDS)

      integer KPDSIN(25),JPDS5(MAXF),K5PDS(7,MAXF),KPDSOUT(25),
     &        IOUTUN(MAXG,MAXF),NOUT,MDLID(MAXG,MAXF),
     &        KGRIDA(MAXG,MAXF),N,NFLDS,iloop,lvel(47)
      real FOUT(JMAXOT)
      real O3_bgn(JMAXOT,47),O3_end(JMAXOT,47),
     &     SP_bgn(JMAXOT),   SP_end(JMAXOT),
     &     O3_sig_bgn(JMAXOT,60),O3_sig_end(JMAXOT,60)
      LOGICAL   LOUT(JMAXOT)
      LOGICAL   continue_search
      character Dn2*3, fnamepre(MAXG)*80, ozonefile*80
      save   i_st_time
      real asigo(60),hgt(46),P_sig(JMAXOT,60),uppres,blpres
      data lvel/1000, 975, 950, 925, 900, 875, 850, 825,
     & 800, 775, 750, 725, 700, 675, 650, 625, 600,
     & 575, 550, 525, 500, 475, 450, 425, 400, 375,
     & 350, 325, 300, 275, 250, 225, 200, 175, 150,
     & 125, 100,  70,  50,  30,  20,  10,   7,   5,
     &   3,   2,   1/
      data asigo/0.0124,0.0372,0.0620,0.0868,0.1115,0.1360,
     & 0.1594,0.1803,0.1990,0.2171,0.2357,0.2557,0.2771,0.2999,
     & 0.3236,0.3477,0.3721,0.3966,0.4212,0.4456,0.4698,0.4940, 
     & 0.5180,0.5420,0.5657,0.5892,0.6123,0.6348,0.6567,0.67800,
     & 0.6985,0.7185,0.7379,0.7567,0.7750,0.7926,0.8097,0.8261,
     & 0.8418,0.8565,0.8704,0.8831,0.8945,0.9043,0.9127,0.9203,
     & 0.9273,0.9340,0.9403,0.9464,0.9522,0.9578,0.9632,0.9684,
     & 0.9736,0.9786,0.9836,0.9884,0.9932,0.9978/

C****************************************************************

      do ik=2,47
         hgt(ik-1)=(lvel(ik-1)-lvel(ik))*100.
      enddo

          if(iloop.eq.1) then
             if(JPDS5(N).eq.1) then
                do ii=1,NOUT
                   SP_bgn(ii)=FOUT(ii)
                enddo
             endif
             if(JPDS5(N).eq.154) then
                do jj=1,47
                  if(lvel(jj).eq.KPDSIN(7)) then
                     do ii=1,NOUT
                       O3_bgn(ii,jj)=FOUT(ii)
                     enddo
                  endif
                enddo
             endif
             if(N.eq.NFLDS) then
              do ii=1,NOUT
                do ik=1,60
                  P_sig(ii,ik)=asigo(ik)*(SP_bgn(ii)-10000)+10000
                  continue_search=.true.
                  do kk=47,2,-1
                     uppres=lvel(kk)*100.
                     blpres=lvel(kk-1)*100.
                     if(continue_search.and.
     &                 (P_sig(ii,ik).le.blpres.and.
     &                 P_sig(ii,ik).ge.uppres)) then
                       top_wt=(blpres-P_sig(ii,ik))/hgt(kk-1)
                       bot_wt=(P_sig(ii,ik)-uppres)/hgt(kk-1)
                       O3_sig_bgn(ii,ik)=top_wt*O3_bgn(ii,kk)+
     &                                   bot_wt*O3_bgn(ii,kk-1)

                       continue_search=.false.
                     endif
                     if(continue_search.and.kk.eq.2) then
                       O3_sig_bgn(ii,ik)=O3_bgn(ii,1)
                     endif
                   enddo
                enddo
              enddo
      write(6,*) "iloop1 N=",N,"KPDSIN(14)=",KPDSIN(14),i_st_time
      write(6,2332) "iloop=1 P_sig ii =100",
     &  (P_sig(100,kj),kj=1,22)
      write(6,2332) "O3_sig_bgn=",
     &  (O3_sig_bgn(100,kj),kj=1,22)
 2332 format(a,/,(22(10(e12.5,1x)),/))
             endif
          endif
          if(iloop.eq.2) then
             if(JPDS5(N).eq.1) then
                do ii=1,NOUT
                   SP_end(ii)=FOUT(ii)
                enddo
             endif
             if(JPDS5(N).eq.154) then
                do jj=1,47
                  if(lvel(jj).eq.KPDSIN(7)) then
                     do ii=1,NOUT
                       O3_end(ii,jj)=FOUT(ii)
                     enddo
                  endif
                enddo
             endif
             if(N.eq.NFLDS) then
              do ii=1,NOUT
                do ik=1,60
                  P_sig(ii,ik)=asigo(ik)*(SP_end(ii)-10000)+10000
                  continue_search=.true.
                  do kk=47,2,-1
                     uppres=lvel(kk)*100.
                     blpres=lvel(kk-1)*100.

                     if(continue_search.and.
     &                 (P_sig(ii,ik).le.blpres.and.
     &                 P_sig(ii,ik).ge.uppres)) then
                       top_wt=(blpres-P_sig(ii,ik))/hgt(kk-1)
                       bot_wt=(P_sig(ii,ik)-uppres)/hgt(kk-1)
                       O3_sig_end(ii,ik)=top_wt*O3_end(ii,kk)+
     &                                   bot_wt*O3_end(ii,kk-1)

                       continue_search=.false.
                     endif
                     if(continue_search.and.kk.eq.2) then
                       O3_sig_end(ii,ik)=O3_end(ii,1)
                     endif
                   enddo
                enddo
              enddo
CCPCL
      write(6,2332) "iloop=2 P_sig ii =100",
     &  (P_sig(100,kj),kj=1,60)
      write(6,2332) "O3_sig_end=",
     &  (O3_sig_end(100,kj),kj=1,60)
          i_st_time=KPDSIN(14)-3
      write(6,*) "iloop2 N=",N,"KPDSIN(14)=",KPDSIN(14),i_st_time
          KPDSOUT=KPDSIN
          KPDSOUT(5)=K5PDS(1,2)
          KPDSOUT(19)=K5PDS(6,2)
          KPDSOUT(2)=MDLID(1,2)
          KPDSOUT(3)=KGRIDA(1,2)
          third1=0.333333
          third2=0.666667
          do im=1,3
             KPDSOUT(14)=i_st_time+(im-1)
             fmax=-9.e40
             fmin=9.e40
             do ik=1,60
                KPDSOUT(7)=asigo(ik)*10000
                if(im.eq.1) IOUTUN(im,ik)=61
                if(im.eq.2) IOUTUN(im,ik)=62
                if(im.eq.3) IOUTUN(im,ik)=63
                if(ik.eq.1) then
                   nD2 = 100+i_st_time+(im-1)   -6  !setback 6 fcst h
                   write(Dn2,'(i3)') nD2            !in file name
                   ozonefile=fnamepre(ik)
                   kozone=index(ozonefile,' ')-3
                   ozonefile= ozonefile(1:kozone)//'_'//Dn2(2:3)
                   call baopenw(IOUTUN(im,ik),ozonefile,ier)
                endif

                do k=1,nout
                   if(im.eq.1) fout(k)=O3_sig_bgn(k,ik)
                   if(im.eq.2) fout(k)=O3_sig_bgn(k,ik)*third2+
     &                         O3_sig_end(k,ik)*third1
                   if(im.eq.3) fout(k)=O3_sig_bgn(k,ik)*third1+
     &                         O3_sig_end(k,ik)*third2
                   if(lout(k).and.fout(k).gt.fmax) fmax=fout(k)
                   if(lout(k).and.fout(k).lt.fmin) fmin=fout(k)
                 enddo
C<<>>PLEE
C       write(6,*) "fill fout KPDSOUT=",KPDSOUT(1:25)
       write(6,1111) "im,ik=",im,ik,"out=",nout,fmax,fmin
C      stop
 1111 format(a,2(i3,1x),a,i6,1x,2(e12.5,1x))
                 call gribitb(ik,fout,nout,1,
     &                        IOUTUN(im,ik),KPDSOUT,notcdf)
             enddo
            enddo
           endif
          endif

      return
      end

