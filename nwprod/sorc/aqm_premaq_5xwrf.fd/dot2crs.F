SUBROUTINE dot2crs (vardot, varcrs)

!-------------------------------------------------------------------------------
! Name:     Dot to Cross
! Purpose:  Interpolates in horizontal from dot to cross points.
! Revised:  29 Jan 2003  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: ie
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: je
  INTEGER                      :: jx
  CHARACTER*16,  PARAMETER     :: pname     = 'DOT2CRS'
  REAL,          INTENT(OUT)   :: varcrs    ( : , : )
  REAL,          INTENT(IN)    :: vardot    ( : , : )

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
! For interior of grid, interpolate dot point values to cross points
! using four-point interpolation.
!-------------------------------------------------------------------------------

  DO j = 1, je
    DO i = 1, ie
      varcrs(i,j) = 0.25 * ( vardot(i,j)   + vardot(i+1,j)  &
                           + vardot(i,j+1) + vardot(i+1,j+1) )
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Define outermost row and column by persisting from IE and JE.
! This is non-physical space, so the values do not matter. 
!-------------------------------------------------------------------------------

  varcrs(:,jx) = varcrs(:,je)
  varcrs(ix,:) = varcrs(ie,:)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: DOT2CRS',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN FIRST DIMENSION',       &
              /, 1x, '***   1ST DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: DOT2CRS',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN SECOND DIMENSION',      &
              /, 1x, '***   2ND DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE dot2crs
