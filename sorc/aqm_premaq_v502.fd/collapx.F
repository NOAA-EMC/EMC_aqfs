SUBROUTINE collapx (aa, vertin, vertout)

!-------------------------------------------------------------------------------
! Name:     Collapse X Arrays
! Purpose:  Collapses a meteorological 3D array AA with layers defined in
!           VERTIN down to layers defined in VERTOUT.  Result is in AA.
! Notes:    - Array AA is used for both input/output to save memory of the
!             calling routines (GRIDOUT.F, METCRO.F, METDOT.F)   
!           - Liberally adapted from EPA's MCIP2.
! Revised:  09 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           22 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!           01 Aug 2006  -- Corrected the range of array worb when it assigns
!                           to array aa
!                        -- Added a logic to check whether the size of arrays 
!                           ilays, wgt, and workb has changed. If it does, 
!                           deallocate and allocate them. (David Wong)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(INOUT) :: aa         ( : , : , : )
  INTEGER                      :: i
  INTEGER, SAVE, ALLOCATABLE   :: ilays      ( : )
  INTEGER                      :: imax
  INTEGER                      :: j
  INTEGER                      :: jmax
  INTEGER                      :: k
  INTEGER                      :: kin
  INTEGER                      :: kout
  INTEGER                      :: lay
  INTEGER                      :: lbnd
  INTEGER                      :: ll
  INTEGER                      :: nlend
  CHARACTER*16,  PARAMETER     :: pname      = 'COLLAPX'
  REAL,          INTENT(IN)    :: vertin     ( : )
  REAL,          INTENT(IN)    :: vertout    ( : )
  REAL,    SAVE, ALLOCATABLE   :: wgt        ( : )
  REAL,    SAVE, ALLOCATABLE   :: workb      ( : )
  REAL                         :: y
  REAL                         :: dy

  INTEGER :: STAT

  INTERFACE

    SUBROUTINE locate (xx, x, j)
      IMPLICIT NONE
      INTEGER,       INTENT(OUT)   :: j
      REAL,          INTENT(IN)    :: x
      REAL,          INTENT(IN)    :: xx         ( : )
    END SUBROUTINE locate

  END INTERFACE

!-------------------------------------------------------------------------------
! Get array dimensions to use as loop bounds.
!-------------------------------------------------------------------------------

  IF ( ( SIZE(aa,3) /= SIZE(vertin,1)  ) .AND.  &
       ( SIZE(aa,3) /= SIZE(vertout,1) ) ) THEN
    WRITE (6,9000)
    GOTO 1001
  ENDIF

  imax  = SIZE(aa,1)
  jmax  = SIZE(aa,2)
  kin   = SIZE(vertin,1)
  kout  = SIZE(vertout,1)

  lbnd  = LBOUND(aa,3)
  nlend = lbnd + kout - 1

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

! IF ( .NOT. ALLOCATED ( ilays ) ) ALLOCATE ( ilays ( kout ) )
! IF ( .NOT. ALLOCATED ( wgt   ) ) ALLOCATE ( wgt   ( kout ) )
! IF ( .NOT. ALLOCATED ( workb ) ) ALLOCATE ( workb ( kout ) )

  IF ( .NOT. ALLOCATED ( ilays   ) ) THEN
     ALLOCATE ( ilays   ( kout ), STAT = STAT )
     IF (STAT .NE. 0) THEN
        PRINT *, ' Error: allocating ilays '
        CALL graceful_stop (pname)
     END IF
     ALLOCATE ( wgt   ( kout ), STAT = STAT )
     IF (STAT .NE. 0) THEN
        PRINT *, ' Error: allocating wgt '
        CALL graceful_stop (pname)
     END IF
     ALLOCATE ( workb   ( kout ), STAT = STAT )
     IF (STAT .NE. 0) THEN
        PRINT *, ' Error: allocating workb '
        CALL graceful_stop (pname)
     END IF
  ELSE
     IF (SIZE(ilays) .LT. kout) THEN
       DEALLOCATE ( ilays )
       DEALLOCATE ( wgt )
       DEALLOCATE ( workb )
       ALLOCATE ( ilays ( kout ), STAT = STAT )
       IF (STAT .NE. 0) THEN
          PRINT *, ' Error: allocating ilays '
          CALL graceful_stop (pname)
       END IF
       ALLOCATE ( wgt ( kout ), STAT = STAT )
       IF (STAT .NE. 0) THEN
          PRINT *, ' Error: allocating wgt '
          CALL graceful_stop (pname)
       END IF
       ALLOCATE ( workb ( kout ), STAT = STAT )
       IF (STAT .NE. 0) THEN
          PRINT *, ' Error: allocating workb '
          CALL graceful_stop (pname)
       END IF
     END IF
  END IF

!-------------------------------------------------------------------------------
! Compute output layer coordinates.
!-------------------------------------------------------------------------------

!$omp parallel do                      &
!$omp firstprivate(kin,kout)           &
!$omp private(ll)                      &
!$omp shared(wgt,ilays,vertout,vertin) 

  DO lay = 1, kout

    CALL locate (vertin, vertout(lay), ll)
    IF ( ll >= kin ) THEN
      wgt(lay)   = 1.0
      ilays(lay) = kin - 1
    ELSE
      wgt(lay)   = ( vertout(lay) - vertin(ll) ) /  &
                   ( vertin(ll+1) - vertin(ll) )
      ilays(lay) = ll
    ENDIF

  ENDDO

!$omp end parallel do

!-------------------------------------------------------------------------------
! Linear interpolation.
!-------------------------------------------------------------------------------

!$omp parallel do                             &
!$omp shared(aa,wgt,ilays)                    &
!$omp private(ll,lay,i,workb)                 &
!$omp firstprivate(kout,imax,jmax,lbnd,nlend)
  DO j = 1, jmax
    DO i = 1, imax

      DO lay = 1, kout
        ll = ilays(lay)
!       workb(lay) = (1.0-wgt(lay)) * aa(i,j,lbnd+ll-1)   +  &
!                           wgt(lay)  * aa(i,j,lbnd+ll)
        workb(lay) = wgt(lay) * (aa(i,j,lbnd+ll) - aa(i,j,lbnd+ll-1)) &
                     + aa(i,j,lbnd+ll-1)

      ENDDO

!     aa(i,j,lbnd:nlend) = workb
      aa(i,j,lbnd:nlend) = workb(1:kout)
      aa(i,j,nlend+1:)   = 0.0

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( ilays )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( wgt   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( workb )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: COLLAPX',                            &
              /, 1x, '***   INPUT ARRAY SIZES DO NOT MATCH',               &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE collapx
