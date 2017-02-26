SUBROUTINE crs2dot (varcrs, vardot)

!-------------------------------------------------------------------------------
! Name:     Cross to Dot
! Purpose:  Interpolates in horizontal from dot to cross points.
! Notes:    *** This subroutine is functionally dot2crs for most applications.
! Revised:  29 Jan 2003  Original version.  (T. Otte)
!           22 Mar 2004  Rewritten as cross-to-dot for inverting dot-point
!                        and cross-point grids.  (T. Otte)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: ie
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: je
  INTEGER                      :: jx
  CHARACTER*16,  PARAMETER     :: pname     = 'CRS2DOT'
  REAL,          INTENT(OUT)   :: vardot    ( : , : )
  REAL,          INTENT(IN)    :: varcrs    ( : , : )

!-------------------------------------------------------------------------------
! Extract domain dimensions.
!-------------------------------------------------------------------------------

  IF ( SIZE(varcrs,1) /= SIZE(vardot,1) ) THEN
    WRITE (6,9000) SIZE(varcrs,1), SIZE(vardot,1)
    GOTO 1001
  ENDIF

  IF ( SIZE(varcrs,2) /= SIZE(vardot,2) ) THEN
    WRITE (6,9100) SIZE(varcrs,2), SIZE(vardot,2)
    GOTO 1001
  ENDIF

  ix = SIZE(varcrs,1)
  jx = SIZE(varcrs,2)

  ie = ix - 1
  je = jx - 1

!-------------------------------------------------------------------------------
! For interior of grid, interpolate cross point values to dot points
! using four-point interpolation.
!-------------------------------------------------------------------------------

!$omp parallel do

  DO j = 1, je
    DO i = 1, ie
      vardot(i,j) = 0.25 * ( varcrs(i,j)   + varcrs(i+1,j)  &
                           + varcrs(i,j+1) + varcrs(i+1,j+1) )
    ENDDO
  ENDDO

!$omp end parallel do

!-------------------------------------------------------------------------------
! Define outermost row and column by persisting from IE and JE.
! This is non-physical space, so the values do not matter. 
!-------------------------------------------------------------------------------

  vardot(:,jx) = vardot(:,je)
  vardot(ix,:) = vardot(ie,:)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CRS2DOT',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN FIRST DIMENSION',       &
              /, 1x, '***   1ST DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CRS2DOT',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN SECOND DIMENSION',      &
              /, 1x, '***   2ND DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE crs2dot
