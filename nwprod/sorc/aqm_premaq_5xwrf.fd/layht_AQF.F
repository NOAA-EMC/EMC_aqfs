SUBROUTINE layht (xx3face, xx3midl, xx3jcbf, xx3jcbm, xx3htf, xx3htm)

!-------------------------------------------------------------------------------
! Name:     Layer Heights
! Purpose:  Compute height of vertical coordinates for given Jacobian.
!              XX3HT =  XX3JACOB * ( X3 (K+1) - X3 (K) )
!
! Input:   IMAX,JMAX:  total domain size in x (E-W) & y (N-S) direction
!          KMAX     :  number of layers (actually used)
!          XX3FACE  :  vertical coordinate values of layer interface
!          XX3MIDL  :  vertical coordinate values of layer middle
!          XX3JCBF  :  vertical Jacobian at layer face in meter
!          XX3JCBM  :  vertical Jacobian at layer middle in meter
!
! Output:  XX3HTF   :  height of layer interface in meter
!          XX3HTM   :  height of layer middle in meter
!
! Notes:    Liberally adapted from EPA's MCIP2.
!     
! Revised:  09 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           24 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!-------------------------------------------------------------------------------

  USE const
  USE xvars,   ONLY: xgeohgt, xtopo

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: imax
  INTEGER                      :: j
  INTEGER                      :: jmax
  INTEGER                      :: k
  INTEGER                      :: lbndf
  INTEGER                      :: lbndm
  CHARACTER*16,  PARAMETER     :: pname      = 'LAYHT'
  INTEGER                      :: ubndf
  INTEGER                      :: ubndm

!-------------------------------------------------------------------------------
! **** "xx3face" invokes the previous k dimention "0:metlay" and start from "1"
!      Because it is defined by "INTENT(IN)", local only and won't effect rest
!      of the subroutines
!-------------------------------------------------------------------------------

  REAL,          INTENT(IN)    :: xx3face    ( : )
  REAL,          INTENT(OUT)   :: xx3htf     ( : , : , : )
  REAL,          INTENT(OUT)   :: xx3htm     ( : , : , : )
  REAL,          INTENT(IN)    :: xx3jcbf    ( : , : , : )
  REAL,          INTENT(IN)    :: xx3jcbm    ( : , : , : )
  REAL,          INTENT(IN)    :: xx3midl    ( : )

!-------------------------------------------------------------------------------
! Since input arrays are assumed-shape, do simple QC on array sizes and
! extract dimension information.
!-------------------------------------------------------------------------------

  ! Check IMAX.

  imax = SIZE(xx3htf,1)
  IF ( ( SIZE(xx3htm,1)  /= imax ) .OR. ( SIZE(xx3jcbf,1) /= imax ) .OR.  &
       ( SIZE(xx3jcbm,1) /= imax ) ) THEN
    WRITE (6,9000) imax, SIZE(xx3htm,1), SIZE(xx3jcbf,1), SIZE(xx3jcbm,1)
    GOTO 1001
  ENDIF

  ! Check JMAX.

  jmax = SIZE(xx3htf,2)
  IF ( ( SIZE(xx3htm,2)  /= jmax ) .OR. ( SIZE(xx3jcbf,2) /= jmax ) .OR.  &
       ( SIZE(xx3jcbm,2) /= jmax ) ) THEN
    WRITE (6,9100) jmax, SIZE(xx3htm,2), SIZE(xx3jcbf,2), SIZE(xx3jcbm,2)
    GOTO 1001
  ENDIF

  ! Check vertical dimensions.

  IF ( ( SIZE(xx3midl,1) /= SIZE(xx3htm,3)  ) .OR.  &
       ( SIZE(xx3midl,1) /= SIZE(xx3jcbm,3) ) ) THEN
    WRITE (6,9200) SIZE(xx3midl,1), SIZE(xx3htm,3), SIZE(xx3jcbm,3)
    GOTO 1001
  ELSE
    lbndm = LBOUND(xx3midl,1)     ! 1
    ubndm = UBOUND(xx3midl,1)     ! metlay
  ENDIF

  IF ( ( SIZE(xx3face,1) /= SIZE(xx3htf,3)  ) .OR.  &
       ( SIZE(xx3face,1) /= SIZE(xx3jcbf,3) ) ) THEN
    WRITE (6,9300) SIZE(xx3face,1), SIZE(xx3htf,3), SIZE(xx3jcbf,3)
    GOTO 1001
  ELSE
    lbndf = LBOUND(xx3face,1)     ! 1
    ubndf = UBOUND(xx3face,1)     ! metlay+1
  ENDIF

!-------------------------------------------------------------------------------
! Build layer height fields using Jacobian.
!-------------------------------------------------------------------------------

 !-- Full levels height --------------

  xx3htf(1:imax,1:jmax,lbndf) = 0.0       ! 1st layer (surface)

  DO k = lbndf+1, ubndf                   ! layer 2 to top
     DO j = 1, jmax
        DO i = 1, imax
           xx3htf(i,j,k) = xx3htf(i,j,k-1) + ( xx3face(k) - xx3face(k-1) ) *  &
                                               xx3jcbm(i,j,lbndm+k-2)
        ENDDO                 
     ENDDO                 
  ENDDO                 

 !-- Half levels height --------------

  !-- 1st layer mid level height

   !----------------------------------------------------------------
   ! -- original code : mix up the full and mis level Jacobian, Bug?
   !----------------------------------------------------------------

   !
   ! xx3htm(1:imax,1:jmax,lbndm) = ( xx3midl(lbndm) - xx3face(lbndf) )     &
   !                              * 0.5 * ( xx3jcbf(1:imax,1:jmax,lbndf)   &
   !                                      + xx3jcbm(1:imax,1:jmax,lbndm) )

   !----------------------------------------------------------------
   ! -- Modification : straight from half the diff of full level
   !----------------------------------------------------------------

  xx3htm(1:imax,1:jmax,lbndm) = 0.5 * ( xx3htf(1:imax,1:jmax,lbndf+1)   &
                                       - xx3htf(1:imax,1:jmax,lbndf) )

  !-- layer 2 to top

  DO k = lbndm+1, ubndm
     DO j = 1, jmax
        DO i = 1, imax
           xx3htm(i,j,k) = xx3htm(i,j,k-1) + ( xx3midl(k) - xx3midl(k-1) ) *  &
                                               xx3jcbf(i,j,lbndf+k-1)
        ENDDO
     ENDDO
  ENDDO


 RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH IMAX',                            &
              /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4),        &
              /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH JMAX',                            &
              /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4),        &
              /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH MIDDLE LEVEL DIMENSION',          &
              /, 1x, '***   SIZE OF XX3MIDL = ', 2x, i4,                   &
              /, 1x, '***   SIZES OF XX3HTM, XX3JCBM = ', 2(2x, i4),       &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH FACE LEVEL DIMENSION',            &
              /, 1x, '***   SIZE OF XX3FACE = ', 2x, i4,                   &
              /, 1x, '***   SIZES OF XX3HTF, XX3JCBF = ', 2(2x, i4),       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE layht
