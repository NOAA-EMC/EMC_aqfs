SUBROUTINE getprs

!-------------------------------------------------------------------------------
! Name:     Get Pressure
! Purpose:  Calculates 3-D pressure field from hydrostatic sigma and
!           surface pressure.
! Revised:  01 Apr 2003  Original version.  (T. Otte)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE metvars
  USE xvars
  USE premaqparm

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: jx
  INTEGER                      :: k, kp
  INTEGER                      :: kx

  REAL                         :: aeta2, z0, z1
  real, allocatable            :: zk(:)
  real, allocatable            :: tmp9(:),geohgt9(:),u_grda9(:),v_grda9(:)
  real, allocatable            :: v_vel9(:),spc_h9(:),c_ice9(:),t_cdc9(:),o3mr9(:)
  real, allocatable            :: tke9(:),kth9(:),clwmr9(:),rwmr9(:),snmr9(:)
  
  EXTERNAL       ENVYN

  LOGICAL        ENVYN, VERT_INTERP
  INTEGER        ISTAT, izk, izk1

  CHARACTER*16,  PARAMETER     :: pname     = 'GETPRS'

!-------------------------------------------------------------------------------
! Extract domain dimensions.
!-------------------------------------------------------------------------------

  IF ( SIZE(pres,1) /= SIZE(psfc,1) ) THEN
    WRITE (6,9000) SIZE(pres,1), SIZE(psfc,1)
    GOTO 1001
  ENDIF

  IF ( SIZE(pres,2) /= SIZE(psfc,2) ) THEN
    WRITE (6,9100) SIZE(pres,2), SIZE(psfc,2)
    GOTO 1001
  ENDIF

  IF ( SIZE(pres,3) /= SIZE(sigmah) ) THEN
    WRITE (6,9200) SIZE(pres,3), SIZE(sigmah)
    GOTO 1001
  ENDIF

  ix = SIZE(pres,1)
  jx = SIZE(pres,2)
  kx = SIZE(pres,3)

!-------------------------------------------------------------------------------
! Calculate 3-D mid-level pressure from the meteorological input data by using
! hydrostatic sigma and surface pressure.
!
!              sigma = (p-ptop)/(psfc-ptop)
!              p = sigma*(psfc-ptop) + ptop
!
!-------------------------------------------------------------------------------

!$omp parallel do

  DO k = 1, kx
     aeta2 = sigmah(k)
     DO j = 1, jx
        DO i = 1, ix
           pres(i,j,k) = aeta2*pdbot(i,j) + x3top
        ENDDO
     ENDDO
  ENDDO

!$omp end parallel do


! Youhua Tang add the following Dec 2010

  VERT_INTERP = ENVYN ('VERT_INTERP', 'USE VERT_INTERP ?', .TRUE. , ISTAT)

  IF (VERT_INTERP) THEN
     write(*,*) 'perform vertical interpolation'
     if ( .not. allocated(zk) ) allocate(zk(kx))
     if ( .not. allocated(tmp9) ) allocate(tmp9(kx))
     if ( .not. allocated(geohgt9) ) allocate(geohgt9(kx))
     if ( .not. allocated(u_grda9) ) allocate(u_grda9(kx))
     if ( .not. allocated(v_grda9) ) allocate(v_grda9(kx))
     if ( .not. allocated(v_vel9) ) allocate(v_vel9(kx))
     if ( .not. allocated(spc_h9) ) allocate(spc_h9(kx))
     if ( .not. allocated(c_ice9) ) allocate(c_ice9(kx))
     if ( .not. allocated(t_cdc9) ) allocate(t_cdc9(kx))
     if ( .not. allocated(o3mr9) ) allocate(o3mr9(kx))
     if ( .not. allocated(tke9) ) allocate(tke9(kx))
     if ( .not. allocated(kth9) ) allocate(kth9(kx))
     if ( .not. allocated(clwmr9) ) allocate(clwmr9(kx))
     if ( .not. allocated(rwmr9) ) allocate(rwmr9(kx))
     if ( .not. allocated(snmr9) ) allocate(snmr9(kx))
       
!$omp parallel do

  DO j = 1, jx
     DO i = 1, ix
        DO k = 1, kx

           ! --- find index

           DO kp=2,kx
              if ( pres(i,j,k) >= pres3d(i,j,kp  ) .and.            &
                   pres(i,j,k) <= pres3d(i,j,kp-1) ) then 
	       zk(k) = kp - 1 + ( pres(i,j,k)-pres3d(i,j,kp-1) )  &
                                 /( pres3d(i,j,kp)-pres3d(i,j,kp-1) )
	       exit
	    endif
           ENDDO

           if ( pres(i,j,k) >= pres3d(i,j,1 ) ) zk(k)=1
           if ( pres(i,j,k) <= pres3d(i,j,kx) ) zk(k)=kx

           if ( zk(k) < 1. .or. zk(k) > kx ) then
              write(*,*) 'zk is out of range ',i,j,k,kp,zk
              write(*,*) 'pres=',pres(i,j,1:kx)
              write(*,*) 'pres3d=',pres3d(i,j,1:kx)
              write(*,*) 'sigmah=',sigmah
              write(*,*) 'pdtop, pdbot, x3top=',pdtop(i,j),pdbot(i,j),x3top
              GOTO 1001
           endif
        ENDDO

        ! --- vertical interpolation

        DO k=2, kx-1 
           izk =int(zk(k))
           izk1=izk+1
           z1=zk(k)-izk
           z0=1-z1

           tmp9(k)=z0*tmp(i,j,izk)+z1*tmp(i,j,izk1)           ! temperature
           geohgt9(k)=z0*geohgt(i,j,izk)+z1*geohgt(i,j,izk1)  ! geopotential height
           u_grda9(k)=z0*u_grda(i,j,izk)+z1*u_grda(i,j,izk1)  ! u wind
           v_grda9(k)=z0*v_grda(i,j,izk)+z1*v_grda(i,j,izk1)  ! v wind
           v_vel9(k)=z0*v_vel(i,j,izk)+z1*v_vel(i,j,izk1)     ! vertical wind
           spc_h9(k)=z0*spc_h(i,j,izk)+z1*spc_h(i,j,izk1)     ! specific humidity
           c_ice9(k)=z0*c_ice(i,j,izk)+z1*c_ice(i,j,izk1)     ! cloud ice
           t_cdc9(k)=z0*t_cdc(i,j,izk)+z1*t_cdc(i,j,izk1)     ! cloud cover
           o3mr9(k)=z0*o3mr(i,j,izk)+z1*o3mr(i,j,izk1)        ! ozone
           tke9(k)=z0*tke(i,j,izk)+z1*tke(i,j,izk1)           ! TKE
           kth9(k)=z0*kth(i,j,izk)+z1*kth(i,j,izk1)           ! KH
           clwmr9(k)=z0*clwmr(i,j,izk)+z1*clwmr(i,j,izk1)     ! cloud water mixing ratio
           rwmr9(k)=z0*rwmr(i,j,izk)+z1*rwmr(i,j,izk1)        ! rain water mixing ratio
           snmr9(k)=z0*snmr(i,j,izk)+z1*snmr(i,j,izk1)        ! snow mixing ratio
        ENDDO
         tmp(i,j,2:kx-1)=tmp9(2:kx-1)
         geohgt(i,j,2:kx-1)=geohgt9(2:kx-1)
         u_grda(i,j,2:kx-1)=u_grda9(2:kx-1)
         v_grda(i,j,2:kx-1)=v_grda9(2:kx-1)
         v_vel(i,j,2:kx-1)=v_vel9(2:kx-1)
         spc_h(i,j,2:kx-1)=spc_h9(2:kx-1)
         c_ice(i,j,2:kx-1)=c_ice9(2:kx-1)
         t_cdc(i,j,2:kx-1)=t_cdc9(2:kx-1)
         o3mr(i,j,2:kx-1)=o3mr9(2:kx-1)
         tke(i,j,2:kx-1)=tke9(2:kx-1)
         kth(i,j,2:kx-1)=kth9(2:kx-1)
         clwmr(i,j,2:kx-1)=clwmr9(2:kx-1)
         rwmr(i,j,2:kx-1)=rwmr9(2:kx-1)
         snmr(i,j,2:kx-1)=snmr9(2:kx-1)
     ENDDO
  ENDDO

!$omp end parallel do

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETPRS',                             &
              /, 1x, '***   ARRAY SIZE MISMATCH IN FIRST DIMENSION',       &
              /, 1x, '***   1ST DIM OF PRES, PSFC = ', 2(2x, i4),          &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETPRS',                             &
              /, 1x, '***   ARRAY SIZE MISMATCH IN SECOND DIMENSION',      &
              /, 1x, '***   2ND DIM OF PRES, PSFC = ', 2(2x, i4),          &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETPRS',                             &
              /, 1x, '***   ARRAY SIZE MISMATCH IN THIRD DIMENSION',       &
              /, 1x, '***   3RD DIM OF PRES, SIGMAH = ', 2(2x, i4),        &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getprs
