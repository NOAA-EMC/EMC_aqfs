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
  USE premaqparm, ONLY: neta2

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: jx
  INTEGER                      :: k
  INTEGER                      :: kx

  REAL                         :: aeta1, aeta2

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
! *** Because the sigma-p of NMM, this relation need to be modified from 
!     eta1 & eta2, where p, eta1, eta2 are in the "interface" or "full" level
!
!              p =  eta1*pdtop +  eta2*pd +ptop    => INTERFACE
!              p = aeta1*pdtop + aeta2*pd +ptop    => MID-LEVEL 
!
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, kx
     if ( k <= neta2 ) then
        aeta1 = 1.
        aeta2 = sigmah(k)
     else
        aeta1 = sigmah(k)
        aeta2 = 0.
     endif

     DO j = 1, jx
        DO i = 1, ix
           pres(i,j,k) = aeta1*pdtop(i,j) + aeta2*pdbot(i,j) + x3top
        ENDDO
      ENDDO
  ENDDO
!$omp end parallel do

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
