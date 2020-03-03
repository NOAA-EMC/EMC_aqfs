SUBROUTINE edyintb

!-------------------------------------------------------------------------------
! Name:     Eddy Integration
! Purpose:  Calculates vertical eddy diffusivity using integration method
!           Estimates max. admissible diffusion time step for
!           Crank-Nicholson method (with some safety factor)
!           Integrated Version of Heinz Formula - by Daewon Byun
!
! Function: Calculates vertical eddy diffusivity using integration method
!           Estimates max. admissible diffusion time step for
!           Crank-Nicholson method (with some safety factor)
!           Integrated Version of Heinz Formula - by Daewon Byun
!             ZL, ZU; low and upper limit of integration
!             ZSL   ; surface layer thickness
!             PBL   ; boundary layer thickness
!             Summary of Cases
!             Case 1 : 0 <= ZL < ZU  < ZSL < PBL (A,B,C)
!             Case 2 : 0 <= ZL < ZSL < ZU  < PBL (A,B,C) + (D,E)
!             Case 3 : 0 <= ZL < ZSL < PBL < ZU  (A,B,C) + (D,E) + (F)
!             Case 4 : 0 < ZSL < ZL  < ZU  < PBL (D,E)
!             Case 5 : 0 < ZSL < ZL  < PBL < ZU  (D,E)   + (F)
!             Case 6 : 0 < ZSL < PBL < ZL  < ZU  (F)
!             Where (A): stable surface layer formula
!                   (B): neutral surface layer formula
!                   (C): unstable surface layer formula
!                   (D): stable pbl formula
!                   (E): unstable mixed layer formula
!                   (F): formula for free atmosphere
!
! Notes:    Liberally adapted from EPA's CMAQ edyintb.F.
! Revised:  21 Nov 2000  Original version in CMAQ.  (EPA/ORD/NERL/AMD)
!           16 Apr 2003  Converted to free-form F90 and modified for PREMAQ.
!                        (T. Otte)
!           29 Apr 2004  Corrected definition of FIFTH.  Changed local array
!                        allocation to occur only on initial call to
!                        subroutine to avoid memory fragmentation.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const
  USE premaqparm
  USE xvars

  IMPLICIT NONE
 
  REAL                         :: alpha
  REAL                         :: arg1
  REAL                         :: as
  REAL                         :: as2
  REAL                         :: as3
  REAL                         :: beta
  REAL                         :: beta1
  INTEGER                      :: c
  REAL,    SAVE, ALLOCATABLE   :: dzint     ( : , : )
  REAL                         :: dzl
  REAL,          PARAMETER     :: eta       = 0.5
  REAL,          PARAMETER     :: fifth     = 1.0 / 5.0
  REAL                         :: fka
  REAL                         :: fkc
  REAL                         :: fkd
  REAL                         :: fke
  REAL                         :: free
  REAL                         :: head
  INTEGER, SAVE, ALLOCATABLE   :: icase     ( : , : , : )
  REAL,          PARAMETER     :: karm      = 0.4         ! von Karman constant
  REAL,          PARAMETER     :: karmc     = karm / 0.74
  INTEGER, SAVE, ALLOCATABLE   :: kftb      ( : , : )
  REAL,          PARAMETER     :: kz0ut     = 1.0         ! [m^2/s] upper trop
  INTEGER                      :: l
  REAL,          PARAMETER     :: minmoli   = 1.0e-4      ! min for inv M-O len
  REAL,    SAVE, ALLOCATABLE   :: moli      ( : , : )
  REAL,          PARAMETER     :: p0        = 100000.0    ! ref prs [Pa]
  CHARACTER*16,  PARAMETER     :: pname     = 'EDYINTB'
  INTEGER                      :: r
  REAL                         :: rib
  REAL                         :: ric
  REAL                         :: rl
  REAL                         :: ru
  REAL,    SAVE, ALLOCATABLE   :: thetav    ( : , : , : )
  REAL,          PARAMETER     :: third     = 1.0 / 3.0
  REAL                         :: tv
  REAL                         :: ws2
  REAL                         :: ww2
  REAL                         :: x1
  REAL                         :: x2
  REAL,          PARAMETER     :: xlamb     = 1600.0      ! asymptotic mix len
  REAL                         :: z1
  REAL                         :: z2
  REAL,    SAVE, ALLOCATABLE   :: zint      ( : , : , : )
  REAL                         :: zk
  REAL                         :: zl
  REAL                         :: zpbl
  REAL                         :: zsl
  REAL                         :: zu
  REAL                         :: zwst

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  fka (z1, z2, beta1) = ( z2 - z1 - LOG( (beta1 * z2 + 1.0) /  &
                                         (beta1 * z1 + 1.0) ) / beta1 ) / beta1

  fkc (z1, z2, alpha) =                                                       &
                  ( (3.0 * alpha * z2 - 2.0) * SQRT(1.0 + alpha * z2)**3 -    &
                    (3.0 * alpha * z1 - 2.0) * SQRT(1.0 + alpha * z1)**3 ) /  &
                  ( 7.5 * alpha * alpha )

  fkd (x1, x2, as2, as3) = as3 * ( as2 * fifth * (x1**5 - x2**5) +            &
                                   (as2 - 1.0) * ( third * (x1**3 - x2**3) +  &
                                   x1 - x2 - 0.5 *                            &
                                   ( LOG( (1.0 + x1) / (1.0 - x1) ) -         &
                                     LOG( (1.0 + x2) / (1.0 - x2) ) ) ) )

  fke (z1, z2, zk, zwst, zpbl) =                                              &
                          zk * zwst * ( z2*z2 * (0.5 - third * z2 / zpbl) -   &
                                        z1*z1 * (0.5 - third * z1 / zpbl) )

  free (z1, z2, zk) = zk * (z2 - z1)

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( dzint  ) )  &
    ALLOCATE ( dzint  ( ncols, nrows )          )
  IF ( .NOT. ALLOCATED ( icase  ) )  &
    ALLOCATE ( icase  ( ncols, nrows, 0:nlays ) ) 
  IF ( .NOT. ALLOCATED ( kftb   ) )  &
    ALLOCATE ( kftb   ( ncols, nrows )          )
  IF ( .NOT. ALLOCATED ( moli   ) )  &
    ALLOCATE ( moli   ( ncols, nrows )          )
  IF ( .NOT. ALLOCATED ( thetav ) )  &
    ALLOCATE ( thetav ( ncols, nrows,   nlays ) )
  IF ( .NOT. ALLOCATED ( zint   ) )  &
    ALLOCATE ( zint   ( ncols, nrows,   nlays ) )

!-------------------------------------------------------------------------------
! Define inverse Monin-Obukhov length, virtual temperature, and layer height.
!-------------------------------------------------------------------------------

  DO r = 1, nrows
    DO c = 1, ncols

      ! Bound MOLI away from (-e-04, e-04).

      moli(c,r) = 1.0 / xmol(c,r)
      moli(c,r) = SIGN ( MAX ( ABS(moli(c,r)), minmoli ), moli(c,r) )

      tv            = xtempm(c,r,1) * ( 1.0 + 0.6 * xwvapor(c,r,1) )
      thetav(c,r,1) = tv * ( p0 / xpresm(c,r,1) )**0.286

      dzint(c,r)    = eta * x3htf(c,r,1)
      zint(c,r,1)   = dzint(c,r)

      kftb(c,r)     = nlays - 1

    ENDDO
  ENDDO

  DO l = 2, nlays
    DO r = 1, nrows
      DO c = 1, ncols

        tv            = xtempm(c,r,l) * ( 1.0 + 0.6 * xwvapor(c,r,l) )
        thetav(c,r,l) = tv * ( p0 / xpresm(c,r,l) )**0.286

        zint(c,r,l)   = x3htf(c,r,l-1) + dzint(c,r)
        dzint(c,r)    = x3htf(c,r,l  ) - zint(c,r,l)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Determine layer indices, RADM layer cases, KFTB.
!-------------------------------------------------------------------------------

  DO l = 1, nlays-1
    DO r = 1, nrows
      DO c = 1, ncols

        zsl = 0.1 * xpbl(c,r)
        zsl = MIN (50.0, zsl)  !  ...but at most 50 meters

        IF ( zint(c,r,l) < zsl ) THEN

          IF ( zint(c,r,l+1) < zsl ) THEN
            icase(c,r,l) = 1
          ELSE IF ( zint(c,r,l+1) < xpbl(c,r) ) THEN
            icase(c,r,l) = 2
          ELSE
            icase(c,r,l) = 3
          ENDIF

        ELSE IF ( zint(c,r,l) < xpbl(c,r) ) THEN

          IF ( zint(c,r,l+1) < xpbl(c,r) ) THEN
            icase(c,r,l) = 4
          ELSE
            icase(c,r,l) = 5
          ENDIF

        ELSE

          icase(c,r,l) = 6
          kftb(c,r)    = MIN (kftb(c,r), l)

        ENDIF

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Calculate eddy diffusivity.
!-------------------------------------------------------------------------------

  layers: DO l = 1, nlays-1
    rows: DO r = 1, nrows
      columns: DO c = 1, ncols
 
        arg1 = karmc * xustar(c,r)
        zsl  = 0.1   * xpbl(c,r)
        zsl  = MIN (50.0, zsl)      !  ...but at most 50 meters

        IF ( l < kftb(c,r) ) THEN

          IF ( moli(c,r) > 0.0 ) THEN  ! stable

            beta1 = 6.345 * moli(c,r)
            beta  = xpbl(c,r) * beta1   ! 4.7*pbl/(0.74*l)
            head  = 2.0 * arg1 * xpbl(c,r) * xpbl(c,r) / beta
            as2   = ( 1.0 + beta ) / beta
            as    = SQRT( as2 )
            as3   = as * as2

            zl    = zint(c,r,l)
            zu    = zint(c,r,l+1)

            SELECT CASE ( icase(c,r,l) )
 
              CASE ( 1 )  ! stable surface layer

                xeddyv(c,r,l) = arg1 * fka(zl,zu,beta1) / (zu - zl)
                xeddyv(c,r,l) = MAX( kz0ut, xeddyv(c,r,l) ) 

              CASE ( 2 )  ! neutral surface layer

                rl = SQRT( MAX( 1.0e-5, 1.0 - zsl / xpbl(c,r) ) ) / as
                ru = SQRT( 1.0 - zu / xpbl(c,r) ) / as
                xeddyv(c,r,l) = arg1 * fka(zl,zsl,beta1) +  &
                                head * fkd(rl,ru,as2,as3 )
                xeddyv(c,r,l) = xeddyv(c,r,l) / (zu - zl)
                xeddyv(c,r,l) = MAX( kz0ut, xeddyv(c,r,l) )

              CASE ( 3 )  ! unstable surface layer

                rl = SQRT( MAX( 1.0e-5, 1.0 - zsl / xpbl(c,r) ) ) / as
                ru = 0.0
                xeddyv(c,r,l) = arg1 * fka(zl,zsl,beta1)  +  &
                                head * fkd(rl,ru,as2,as3) +  &
                                free(xpbl(c,r),zu,kz0ut)
                xeddyv(c,r,l) = xeddyv(c,r,l) / ( zu - zl )
                xeddyv(c,r,l) = MAX( kz0ut, xeddyv(c,r,l) )

              CASE ( 4 )  ! stable PBL

                rl = SQRT( 1.0 - zl / xpbl(c,r) ) / as
                ru = SQRT( 1.0 - zu / xpbl(c,r) ) / as
                xeddyv(c,r,l) = head * fkd(rl,ru,as2,as3)
                xeddyv(c,r,l) = xeddyv(c,r,l) / ( zu - zl )
                xeddyv(c,r,l) = MAX( kz0ut, xeddyv(c,r,l) )

              CASE ( 5 )  ! unstable mixed layer

                rl = SQRT( 1.0 - zl / xpbl(c,r) ) / as
                ru = 0.0
                xeddyv(c,r,l) = head * fkd(rl,ru,as2,as3) +  &
                                free(xpbl(c,r),zu,kz0ut)
                xeddyv(c,r,l) = xeddyv(c,r,l) / ( zu - zl )
                xeddyv(c,r,l) = MAX( kz0ut, xeddyv(c,r,l) )

              CASE ( 6 )  ! free atmosphere

                xeddyv(c,r,l) = kz0ut

            END SELECT

            IF ( xeddyv(c,r,l) <= 0.0 ) GOTO 8000
 
          ELSE  ! unstable case, MOL < 0

            alpha = -9.0 * moli(c,r)

            zl = zint(c,r,l)
            zu = zint(c,r,l+1)

            SELECT CASE ( icase(c,r,l) )
 
              CASE ( 1 )  ! stable surface layer

                xeddyv(c,r,l) = arg1 * fkc(zl,zu,alpha) / (zu - zl)

              CASE ( 2 )  ! neutral surface layer

                xeddyv(c,r,l) = arg1 * fkc(zl,zsl,alpha) +  &
                                fke(zsl,zu,karm,xwstar(c,r),xpbl(c,r))
                xeddyv(c,r,l) = xeddyv(c,r,l) / (zu - zl)

              CASE ( 3 )  ! unstable surface layer

                xeddyv(c,r,l) = arg1 * fkc(zl,zsl,alpha) +                     &
                                fke(zsl,xpbl(c,r),karm,xwstar(c,r),xpbl(c,r))  &
                                + free(xpbl(c,r),zu,kz0ut)
                xeddyv(c,r,l) = xeddyv(c,r,l) / (zu - zl)

              CASE ( 4 )  ! stable PBL

                xeddyv(c,r,l) = fke(zl,zu,karm,xwstar(c,r),xpbl(c,r)) 
                xeddyv(c,r,l) = xeddyv(c,r,l) / (zu - zl)

              CASE ( 5 )  ! unstable mixed layer

                xeddyv(c,r,l) = fke(zl,xpbl(c,r),karm,xwstar(c,r),xpbl(c,r))  &
                                + free(xpbl(c,r),zu,kz0ut)
                xeddyv(c,r,l) = xeddyv(c,r,l) / (zu - zl)

              CASE ( 6 )  ! free atmosphere

                xeddyv(c,r,l) = kz0ut

            END SELECT

          ENDIF  ! if stable case, or else unstable case

        ELSE  ! l >= kftb...upper layers

          dzl = x3htm(c,r,l+1) - x3htm(c,r,l)
          ric = 0.257 * dzl**0.175

          ! Calculate component-wise wind shear.

          ww2 = 0.25 * xmapc(c,r) * xmapc(c,r) *             &
                ( (xuu(c+1,r,  l+1) - xuu(c+1,r,  l) +       &
                   xuu(c,  r,  l+1) - xuu(c,  r,  l) )**2 +  &
                  (xvv(c,  r+1,l+1) - xvv(c,  r+1,l) +       &
                   xvv(c,  r,  l+1) - xvv(c,  r,  l) )**2 )

          ws2 = ww2 / (dzl * dzl) + 1.0e-9
 
          rib = 2.0 * grav * (thetav(c,r,l+1) - thetav(c,r,l)) /  &
               ( dzl * ws2 * (thetav(c,r,l+1) + thetav(c,r,l)) )
          rib = MAX(rib, 0.0)

          IF ( (rib - ric) >= 0.0 ) THEN
            xeddyv(c,r,l) = kz0ut
          ELSE
            xeddyv(c,r,l) = kz0ut + xlamb * SQRT(ws2) * ( 1.0 - rib / ric )
          ENDIF

        ENDIF

      ENDDO columns
    ENDDO rows
  ENDDO layers

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dzint  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( icase  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( kftb   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( moli   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( thetav )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( zint   )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) c, r, l, icase(c,r,l), xeddyv(c,r,l),    &
                     zl, zu, xpbl(c,r), xmol(c,r), as, head,  &
                     rl, ru, fkd(rl,ru,as2,as3),              &
                     as * fifth * (rl**5 - ru**5),            &
                     third * (rl**3 - ru**3),                 &
                     rl - ru,                                 &
                     (1.0+rl)/(1.0-rl), (1.0+ru)/(1.0-ru),    &
                     LOG((1.0+rl)/(1.0-rl)) - LOG((1.0+ru)/(1.0-ru))
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                                &
              /, 1x, '*** SUBROUTINE: EDYINTB',                              &
              /, 1x, '***   EDDY DIFFUSIVITY IS LESS THAN OR EQUAL TO 0',    &
              /, 1x, '***   C, R, L, ICASE, XEDDYV = ', 4(2x, i3), 2x, f9.4, &
              /, 1x, '***   ZL, ZU, XPBL, XMOL, AS, HEAD = ', 6(2x, f9.4),   &
              /, 1x, '***   RL, RU, FKD = ', 3(2x, f9.4),                    &
              /, 1x, '***   FKD term 1 = ', f9.4,                            &
              /, 1x, '***   FKD subterm 2 = ', f9.4,                         &
              /, 1x, '***   FKD subterm 3 = ', f9.4,                         &
              /, 1x, '***   FKD ln ARGS = ', 2(2x, f9.4),                    &
              /, 1x, '***   SUM FKD ln TERMS = ', f9.4                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE edyintb
