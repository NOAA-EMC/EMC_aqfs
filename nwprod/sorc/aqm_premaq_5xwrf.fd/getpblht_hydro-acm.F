SUBROUTINE acmpblht (c, r, wspd, tstv)

!----------------------------------------------------------------------
! Name:     PBL Height
! Purpose:  Calculates PBL height from bulk Richardson number.
! Notes:    -- Same method as used in ACM2 in WRF (Pleim 2007)
! Revised:  Apr 2007  (JEP)
! Revised:  Apr 19 2007 for PREMAQ (Hsin-mu Lin)
!----------------------------------------------------------------------

  USE premaqparm
  USE const
  USE xvars
  USE const_pbl


!....... Name of subroutine

  CHARACTER*16,  PARAMETER  :: pname       = 'acmpblht'

!.......Arguments

  INTEGER,     INTENT(IN)   :: c, r
  REAL,        INTENT(IN)   :: tstv
  REAL,        INTENT(IN)   :: wspd       ( : )

!.......Local scalars

  REAL                      :: dtmp, dth1, dthv, dzl, dwspd, dzf, dxden
  REAL                      :: dricr, dribk
  REAL                      :: fint
  REAL                      :: fintt
  INTEGER                   :: k, kmix
  INTEGER                   :: kpblht, kpblh
  REAL                      :: pbl,pl, ql
  REAL                      :: mol,ust,wst
  REAL                      :: wss,tmpfx,tconv,th1
  REAL                      :: zmix,umix,tog,wssq

!.......Local arrays

  REAL,  SAVE, ALLOCATABLE  :: ribk       ( : )
  REAL,  SAVE, ALLOCATABLE  :: thetav     ( : )
  REAL,  SAVE, ALLOCATABLE  :: zl         ( : )
  REAL,  SAVE, ALLOCATABLE  :: zf         ( : )

!----------------------------------------------------------------------
! Allocate necessary arrays.
!----------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( ribk   ) ) ALLOCATE ( ribk    (   metlay ) )
  IF ( .NOT. ALLOCATED ( thetav ) ) ALLOCATE ( thetav  (   metlay ) )
  IF ( .NOT. ALLOCATED ( zl     ) ) ALLOCATE ( zl      (   metlay ) )
  IF ( .NOT. ALLOCATED ( zf     ) ) ALLOCATE ( zf      ( 0:metlay ) )

!----------------------------------------------------------------------

  zf(0) = 0.0

  do k = 1, metlay
     zl(k) = x3htm  (c,r,k)
     zf(k) = x3htf  (c,r,k)
     dxden = xdensam(c,r,k) - xdenswm(c,r,k)
     ql    = xdenswm(c,r,k) / dxden
     pl    = xpresm (c,r,k)
     thetav(k) = xtempm(c,r,k) * ( 100000.0 / pl )**(2.0/7.0)  &
                               * ( 1.0 + 0.6077 * ql )
  enddo
  
  mol = xmol2   (c,r)
  ust = xustar2 (c,r)
  wst = xwstar2 (c,r)

!----------------------------------------------------------------------
! Calculate bulk Richardson number.  
!----------------------------------------------------------------------

  if (mol < 0.0) then
     wss   = (ust ** 3 + 0.6 * wst ** 3) ** 0.33333
     tmpfx = -ust * tstv
     tconv = 8.5 * tmpfx / wss
     th1   = thetav(1) + tconv
  else
     th1   = thetav(1)
  endif

  kmix = 1
  do k = 1, metlay
     dtmp = thetav(k) - th1
     if (dtmp < 0.0) kmix = k
  enddo

  if (kmix > 1) then
     dth1  =    th1         - thetav(kmix)
     dthv  = thetav(kmix+1) - thetav(kmix)
     dzl   =     zl(kmix+1) -     zl(kmix)
     dwspd =   wspd(kmix+1) -   wspd(kmix)
     
     fintt = dth1 / dthv
     zmix  = fintt * dzl   +   zl(kmix)
     umix  = fintt * dwspd + wspd(kmix)
  else
     zmix = zl(1)
     umix = wspd(1)
  endiF

  ribloop: do k = kmix,metlay

     if (kmix == 1) then
        wust = 100.*ust*ust
        wssq = (wspd(k)-umix)**2 + wust
     else
        wssq = (wspd(k)-umix)**2
     endif

     wssq = MAX( wssq, 0.1 )

     dtmp = thetav(k) - th1
     tog  = 0.5 * (thetav(K) + th1) / grav

     ribk(k) = ABS( zl(k)-zmix ) * dtmp / (tog * wssq)

     if (ribk(k) >= ricr) then
        kpblh = k
        EXIT ribloop
     endif
  enddo ribloop

  if (k == metlay) write (*,*) '######## ACM PBL K=  ', k, '  C= ', c, '  R= ', r

  if (k == metlay+1) then
     WRITE (6,9000) 
     GO TO 1001
  endif

!-------------------------------------------------------------------------------
! Calculate PBL height.
!-------------------------------------------------------------------------------

  if (kpblh /= 1) then

      !---- INTERPOLATE BETWEEN LEVELS -- jp 7/93

     dricr = ricr        - ribk(kpblh-1)
     dribk = ribk(kpblh) - ribk(kpblh-1)

     fint  = dricr / dribk

     if (fint > 0.5) then
        kpblht  = kpblh
        fint = fint - 0.5
     else
        kpblht  = kpblh - 1
        fint = fint + 0.5
     endif

     dzf = zf(kpblht) - zf(kpblht-1)
     pbl = fint * dzf + zf(kpblht-1)
  else
     pbl = zf(1)                                                  
  endif

  xpbl2(c,r) = MAX( pbl , zf(1) )

  if (xpbl2(c,r) >= 8000.0) then
     write (*,*) 
     write (*,*) '######## ACM PBL= ', xpbl2(c,r) 
     write (*,*) '######## Kpblht = ', kpblh ,'  C= ', c, '  R= ', r
     write (*,*) '######## ribk   = ', ribk(kpblh), '  ribk-1= ', ribk(kpblh-1)
     write (*,*) '######## fint   = ', fint
     write (*,*) '######## zfkpbl = ', zf(kpblht), '  zfkpbl-1= ', zf(kpblht-1)
     write (*,*) '######## TSTV   = ', tstv
     write (*,*) '######## MOL    = ', mol
     write (*,*) '######## UST    = ', ust
     write (*,*) '######## WST    = ', wst
     write (*,*) '######## TCONV  = ', tconv
     write (*,*) '----------------------------------------------------------------'
  endif


  RETURN


!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000  FORMAT (/, 1x, 70('*'),                                                 &
               /, 1x, '*** SUBROUTINE: acmpblht',                              &
               /, 1x, '***    RIB never exceeds RICR, RIB(kte)= ', ribk(k-1),  &
               /, 1x, '***    THETAV(1) = ', thetav(1),                        &
               /, 1x, '***    MOL   = ', mol,                                  &
               /, 1x, '***    TCONV = ', TCONV,                                &
               /, 1x, '***    WST   = ', WST,                                  &
               /, 1x, '***    KMIX  = ', kmix,                                 &
               /, 1x, '***    UST   = ', UST,                                  &
               /, 1x, '***    TSTv  = ', TSTv,                                 &
               /, 1x, '***    wspd  = ', wspd(1),                              &
               /, 1x, '***    c =', c, '  r=', r,                              &
               /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)

  RETURN


END SUBROUTINE acmpblht
