      subroutine cmaq_output(N,M,NFLDS_INT,KPDSIN,KPDSOUT,K5PDS,
     &           MDLID,KGRIDA,MAXG,MAXF,fnamepre)

      integer KPDSIN(25),KPDSOUT(25),K5PDS(7,MAXF),
     &        IOUTUN(MAXG,MAXF),MDLID(MAXG,MAXF),
     &        KGRIDA(MAXG,MAXF),N,NFLDS_INT
      character Dn2*3,fnamepre(MAXG)*80,ozonefile*80,spcname(200)*16
      integer   ii,jj,ncols,nrows,nlays,nspcs,nrec,sdate,stime,
     &          iyear,month,iday,nout,kozone
      INCLUDE "SPECIES.comm"

             call notcdf_in(ncols,nrows,nlays,nspcs,nrec,
     &                      N,sdate,stime,spcname)

             iyear=int(sdate/1000)
             call daymon(sdate,month,iday)

             print*,"crlspcs=",ncols,nrows,nlays,nspcs,
     &       "sdate=",sdate,"month=",month,"iday=",iday,
     &       "nrec=",nrec

            nout = ncols*nrows

            KPDSOUT=KPDSIN
            KPDSOUT(1)=7
            KPDSOUT(7)=10000        !  Treating first sigma as surface
            KPDSOUT(8) =iyear-2000  !  YEAR OF CENTURY
            KPDSOUT(9) =month       !  MONTH OF YEAR
            KPDSOUT(10)=iday        !  DAY OF MONTH
            KPDSOUT(11)=stime*1e-4  !  HOUR OF DAY
            KPDSOUT(13)=1           !  INDICATOR OF FCST TIME UNIT
            do 330 ln = 1,NFLDS_INT
               KPDSOUT(2)=MDLID(M,ln)
               KPDSOUT(3)=KGRIDA(M,ln)
               KPDSOUT(5)=K5PDS(1,ln)    !INDICATOR OF PARAMETER
               KPDSOUT(6)=K5PDS(2,ln)
               KPDSOUT(19)=K5PDS(6,ln)
               if(mod(ln,2).eq.1) ioutun(M,ln) = 61
               if(mod(ln,2).eq.0) ioutun(M,ln) = 62
               if(ln.le.2) then
                  nD2 = 100+N
                  write(Dn2,'(i3)') nD2
                  ozonefile=fnamepre(ln)
                  kozone=index(ozonefile,' ')-1
                  ozonefile= ozonefile(1:kozone)//Dn2(2:3)
                  call baopenw(ioutun(M,ln),ozonefile,ier)
                endif
               do ii=1,ncols
                  do jj=1,nrows
                     ozout(ii,jj)=20.0
                  enddo
               enddo
               if(ln.le.2) then
                  KPDSOUT(14)=N-1
                  KPDSOUT(15)=N
                  KPDSOUT(16)=3
                  KPDSOUT(17)=3
                  do jj=1,nrows
                     do ii=1,ncols
                        ozout(ii,jj)=species(ii,jj,1,1)*1e3
                        ozone(ii,jj,N)=species(ii,jj,1,1)*1e3
                        if(ln.eq.2) then
                           if(ozout(ii,jj).lt.60.5) ozout(ii,jj)=1.
                           if(ozout(ii,jj).ge.60.5.and.
     &                        ozout(ii,jj).lt.84.5) ozout(ii,jj)=2.
                           if(ozout(ii,jj).ge.84.5.and.
     &                        ozout(ii,jj).lt.99.5) ozout(ii,jj)=3.
                           if(ozout(ii,jj).ge.99.5.and.
     &                        ozout(ii,jj).lt.114.5)ozout(ii,jj)=4.
                           if(ozout(ii,jj).ge.114.5.and.
     &                        ozout(ii,jj).lt.124.5)ozout(ii,jj)=5.
                           if(ozout(ii,jj).ge.124.5.and.
     &                        ozout(ii,jj).lt.144.5)ozout(ii,jj)=6.
                           if(ozout(ii,jj).ge.144.5.and.
     &                        ozout(ii,jj).lt.164.5)ozout(ii,jj)=7.
                           if(ozout(ii,jj).ge.164.5.and.
     &                        ozout(ii,jj).lt.184.5)ozout(ii,jj)=8.
                           if(ozout(ii,jj).ge.184.5.and.
     &                        ozout(ii,jj).lt.204.5)ozout(ii,jj)=9.
                           if(ozout(ii,jj).ge.204.5)ozout(ii,jj)=10.
                        endif
                     enddo
                  enddo
               else
                if(N.ge.8)then
                  if(N.ge.8) then
                       KPDSOUT(14)=N-8
                       KPDSOUT(15)=N
                       KPDSOUT(16)=3
                       KPDSOUT(17)=3
                     else
                       KPDSOUT(14)=N
                       KPDSOUT(15)=0
                       KPDSOUT(16)=0
                       KPDSOUT(17)=0
                  endif
                  do jj=1,nrows
                     do ii=1,ncols
                        wrkoz(ii,jj)=0.
                        do isum=N-7,N
                           wrkoz(ii,jj)=
     &                     wrkoz(ii,jj)+ozone(ii,jj,isum)
                        enddo
                        ozout(ii,jj)=wrkoz(ii,jj)/8
                        if(ln.eq.4) then
                           if(ozout(ii,jj).lt.50.5) ozout(ii,jj)=1.
                           if(ozout(ii,jj).ge.50.5.and.
     &                        ozout(ii,jj).lt.64.5) ozout(ii,jj)=2.
                           if(ozout(ii,jj).ge.64.5.and.
     &                        ozout(ii,jj).lt.74.5) ozout(ii,jj)=3.
                           if(ozout(ii,jj).ge.74.5.and.
     &                        ozout(ii,jj).lt.84.5) ozout(ii,jj)=4.
                           if(ozout(ii,jj).ge.84.5.and.
     &                        ozout(ii,jj).lt.94.5) ozout(ii,jj)=5.
                           if(ozout(ii,jj).ge.94.5.and.
     &                        ozout(ii,jj).lt.104.5)ozout(ii,jj)=6.
                           if(ozout(ii,jj).ge.104.5.and.
     &                        ozout(ii,jj).lt.114.5)ozout(ii,jj)=7.
                           if(ozout(ii,jj).ge.114.5.and.
     &                        ozout(ii,jj).lt.124.5)ozout(ii,jj)=8.
                           if(ozout(ii,jj).ge.124.5.and.
     &                        ozout(ii,jj).lt.134.5)ozout(ii,jj)=9.
                           if(ozout(ii,jj).ge.134.5)ozout(ii,jj)=10.
                        endif
                     enddo
                  enddo
                endif
               endif

               call gribitb(ln,ozout,ncols,nrows,
     &              ioutun(M,ln),KPDSOUT)

  330      continue
       return
       end
