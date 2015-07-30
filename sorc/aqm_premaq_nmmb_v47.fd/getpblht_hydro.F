SUBROUTINE getpblht (c, r, wspd)

!-------------------------------------------------------------------------------
! Name:     Get PBL Height
! Purpose:  Calculates PBL height from bulk Richardson number.
! Notes:    -- This routine is used when PBL from input meteorology is 0.0.
!              Bulk Richardson number calculation taken from Byun's pblpkg.F
!              subroutine.  PBL height algorithm from Alapaty's VMM for MM5.
!           -- Liberally adapted from EPA's MCIP2.
! Revised:  08 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE const
  USE xvars
  USE const_pbl

!.......Arguments

  INTEGER,       INTENT(IN)    :: c, r
  REAL,          INTENT(IN)    :: wspd       ( : )

!.......Parameter

  REAL,          PARAMETER     :: dtempmax   =  5.0 ! max sfc_T - air_T
  REAL,          PARAMETER     :: dtempmin   = -3.0 ! min sfc_t - air_T

  REAL,          PARAMETER     :: fineparm   =  1.0e-10  ! min parm

!.......Local scalars

  INTEGER                      :: k, ktop

  REAL                         :: apbl
  REAL                         :: densd, densw, dtmpv
  REAL                         :: f_ptemp, f_vtemp, fract1, fract2
  REAL                         :: press, psurf
  REAL                         :: qmix, qratio
  REAL                         :: rhumid
  REAL                         :: temp, tempc, tsurf
  REAL                         :: vpress, vptemp, vptsurf, vsat
  REAL                         :: zpbl

!.......Local arrays

  REAL,    SAVE, ALLOCATABLE   :: ribk       ( : )
  REAL,    SAVE, ALLOCATABLE   :: vptempk    ( : )


!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  !-- Saturation vapor pressure [Pa]

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )


  !-- Virtual temperature [K]

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )


  !-- Potential temperature [K]

  f_ptemp(temp,press) = temp * ( 100000.0 / press )**(2.0/7.0)

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( ribk    ) ) ALLOCATE ( ribk    ( metlay ) )
  IF ( .NOT. ALLOCATED ( vptempk ) ) ALLOCATE ( vptempk ( metlay ) )

!-------------------------------------------------------------------------------
! Compute difference of virtual temperature.  Algorithm taken from Byun's
! pblpkg.F.
!-------------------------------------------------------------------------------

  !---------------------------------------------------------------------
  !-- Saturation vapor pressure over water [mb]
  !
  ! Estimating water vapor at surface with TSURF = 0.5*(TEMPM(1)+TEMPG)
  ! assuming it has the same relative humidity as the layer 1 air,
  ! but at the reference temperature
  !---------------------------------------------------------------------

  vsat    = e_aerk( xtempm(c,r,1) - stdtemp )
  vpress  = xdenswm(c,r,1) * rwvap * xtempm(c,r,1)

  rhumid  = vpress / vsat

  psurf   = xpresm(c,r,1) + grav*xdensam(c,r,1)*x3htm(c,r,1)  ! [Pa]
  tsurf   = 0.5 * ( xtempm(c,r,1) + xtempg(c,r) )
  vsat    = e_aerk( tsurf - stdtemp )                         ! [Pa]

  vpress  = vsat * rhumid
  densw   = vpress / ( rwvap * tsurf )
  densd   = xdensaf(c,r,0)

  qsurf   = densw / densd

  !---------------------------------------------------------------------
  !-- Virtual temperature of ground and air above ground
  !---------------------------------------------------------------------

  vptsurf = f_vtemp( tsurf, qsurf )
  vptsurf = f_ptemp( vptsurf, psurf )

  qratio  = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )

  vptemp  = f_vtemp( xtempm(c,r,1), qratio )
  vptemp  = f_ptemp( vptemp, xpresm(c,r,1) )

  dtmpv   = vptsurf - vptemp

  if ( dtmpv == 0.0 ) then
     dtmpv = fineparm
  elseif ( dtmpv < dtempmin ) then
     dtmpv = dtempmin
  elseif ( dtmpv > dtempmax ) then
     dtmpv = dtempmax
  endif

!-------------------------------------------------------------------------------
! Calculate bulk Richardson number.  Algorithm taken from Byun's pblpkg.F.
!-------------------------------------------------------------------------------

  ribloop: DO k = 1, metlay

    vptempk(k) = f_vtemp( xtempm(c,r,k), qratio )
    vptempk(k) = f_ptemp( vptempk(k), xpresm(c,r,k) )

    dtemp = - ( dtmpv + vptempk(1) - vptempk(k) )

    if ( dtemp == 0.0 ) then
       dtemp = fineparm
    endif

    ribk(k) = grav * x3htm(c,r,k) * dtemp / ( tsurf * wspd(k)**2 )

    if ( ribk(k) == 0.0 ) then
       ribk(k) = -1.0 * fineparm
    endif

    IF ( ribk(k) >= ricr ) THEN
      ktop = k
      EXIT ribloop
    ENDIF

  ENDDO ribloop

!-------------------------------------------------------------------------------
! Calculate PBL height.  Algorithm adapted from Alapaty's VMM for Blackadar
! in MM5.
!-------------------------------------------------------------------------------

  IF ( ktop /= 1 ) THEN
    fract1 = ( ricr - ribk(ktop-1) ) / ( ribk(ktop) - ribk(ktop-1) )
    fract2 = 1.0 - fract1
    apbl   = x3htm(c,r,ktop) * fract1
    zpbl   = apbl + ( x3htm(c,r,ktop) * fract2 )
  ENDIF

  IF ( zpbl < x3htm(c,r,1) ) THEN
    xpbl(c,r) = x3htm(c,r,1)
  ELSE
    xpbl(c,r) = zpbl
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( ribk    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vptempk )  ! commented out to avoid memory fragmentation

END SUBROUTINE getpblht
