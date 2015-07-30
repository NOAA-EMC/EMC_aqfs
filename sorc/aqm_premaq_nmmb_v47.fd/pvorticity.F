!23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE PVS

! **********************************************************************

      USE const,      ONLY: GRAV
      USE coord,      ONLY: DX=>xcell_gd, DY=>ycell_gd
      USE premaqparm, ONLY: IX=>ncols_x, JX=>nrows_x, KX=>metlay
 
      USE xvars, ONLY: XMSF=>xmapc, UMSF=>xmap_s, VMSF=>xmap_t,        &
                       PB=>xpresm, TB=>xtempm, UC=>xu_s, VB=>xv_t,     &
                       CORL=>xfc, XX3=>xx3midl, UD=>xuu, VD=>xvv,      &
                       xmapd , xx3face, xpdtop, xpdbot, XPVC

      REAL,  SAVE,  ALLOCATABLE  ::  DUDS ( : , : ), DVDS ( : , : )
      REAL,  SAVE,  ALLOCATABLE  ::  DTDX ( : , : ), DTDY ( : , : )
      REAL,  SAVE,  ALLOCATABLE  ::  DTDS ( : , : ), THEH ( : , : )

      REAL,  SAVE,  ALLOCATABLE  ::  SIGMA ( : )

      INTEGER IM, JM

!-----------------------------------------------------------------------
! 1/3/06 SAM PSB is PI(star) - reference PI(surface) minus PI(top) MUB
!                              variable in WRF output
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
! Begin Potential Vorticity calculation subroutine:
!
! Formalism based on Ebel et al.,
! Simulation of ozone intrusion caused by tropopause fold and cut-off low
! Atmos. Environ., Part A, 25, 2131-2144.
!
! Output Variable:
!  XPVC - Ertl's potential vorticity
!
! Input Variables:
!  TB    (Half level grid center Temperature - degrees K)
!  PB    (Half level grid center pressure - Pa)
!  UC    (Half level C-grid west to east wind vector - m/s)
!  VB    (Half level C-grid south to north wind vector - m/s)
!  SIGMA (Half level model coordinate sigma values)       ! 1-X3 instead
!  XMSF  (grid center map scale factor)
!  UMSF  (C-grid, west to east wind coordinate map scale factor)
!  VMSF  (C-grid, south to north wind coordinate map scale factor)
!  DX    (Horizontal grid resolution - m)
!  GRAV  (gravitational constant = 9.8 m/s(2))
!  CORL  (grid center Coriolis parameter - 1/s)
!  PSB   (grid center PI(star) = reference PI(surface) - PI(top)
!                                (MUB var in WRF output) - Pa)
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Allocate necessary variables.
!-----------------------------------------------------------------------

      IF ( .NOT. ALLOCATED (DUDS)  )  ALLOCATE ( DUDS(IX-1,JX-1) )
      IF ( .NOT. ALLOCATED (DVDS)  )  ALLOCATE ( DVDS(IX-1,JX-1) )
      IF ( .NOT. ALLOCATED (DTDX)  )  ALLOCATE ( DTDX(IX  ,JX  ) )
      IF ( .NOT. ALLOCATED (DTDY)  )  ALLOCATE ( DTDY(IX  ,JX  ) )
      IF ( .NOT. ALLOCATED (DTDS)  )  ALLOCATE ( DTDS(IX  ,JX  ) )
      IF ( .NOT. ALLOCATED (THEH)  )  ALLOCATE ( THEH(IX  ,JX  ) )

      IF ( .NOT. ALLOCATED (SIGMA) )  ALLOCATE ( SIGMA(KX) )

!----------------------------

      DATA XE /0.286/

!
! converts Potential Vorticity into standard PV units (1.E-6*m(2)K/kg/s)
! - see Ebel et al. reference.
!
      SCALE = -1.E6

!
!  Define variables for interation
!
      IM=IX-1
      JM=JX-1

!
! Transfer monotonically increased X3 to monotonically decreased SIGMA
!
      DO K=1,KX
         SIGMA(K) = 1.0 - XX3(K)
      ENDDO

!
! ... EVERYTHING IS BY SLABS ( K slab, Z )
!
! 6/3/99, Vertical gradients calculated by 2nd order polynomials at all levels.
! Also corrects original code so gradients now obtained at model sigma levels,
! not at sigma=.5*(sigma(K+1)+sigma(K-1))
!
      DO 3000 K=1,KX

         ! write (*,*) "K=",K, "######################"

         IF (K .EQ. 1) THEN
            K0=K
            K1=K+1
            K2=K+2
            F0=-1./(SIGMA(K1)-SIGMA(K0)) - 1./(SIGMA(K2)-SIGMA(K0))
            F1= 1./(SIGMA(K1)-SIGMA(K0)) + 1./(SIGMA(K2)-SIGMA(K1))
            F2=-1.*( (SIGMA(K1)-SIGMA(K0))  &
                    /( (SIGMA(K2)-SIGMA(K0))*(SIGMA(K2)-SIGMA(K1)) ) )
         ELSEIF (K .EQ. KX) THEN
            K0=K-2
            K1=K-1
            K2=K
            F0=    (SIGMA(K2)-SIGMA(K1))    &
                  /( (SIGMA(K2)-SIGMA(K0))*(SIGMA(K1)-SIGMA(K0)) )
            F1=-1./(SIGMA(K1)-SIGMA(K0)) - 1./(SIGMA(K2)-SIGMA(K1))
            F2= 1./(SIGMA(K2)-SIGMA(K0)) + 1./(SIGMA(K2)-SIGMA(K1))
         ELSE
            K0=K-1
            K1=K
            K2=K+1
            F0=-1.*(SIGMA(K2)-SIGMA(K1))    &
                  /( (SIGMA(K1)-SIGMA(K0))*(SIGMA(K2)-SIGMA(K0)) )
            F1= 1./(SIGMA(K1)-SIGMA(K0)) - 1./(SIGMA(K2)-SIGMA(K1))
            F2=    (SIGMA(K1)-SIGMA(K0))    &
                  /( (SIGMA(K2)-SIGMA(K1))*(SIGMA(K2)-SIGMA(K0)) )
         ENDIF

         ! write (*,*) "K0=",K0, " K1=",K1, " K2=",K2

!
!     ... COMPUTE VERTICAL DERIVATIVES: dU/ds, dV/ds, dTHETA/ds
!
         DO J=1,JX-2
            DO I=1,IX-2
               DUDS(I+1,J+1) = .5*( F0*(UC(I+1,J  ,K0)+UC(I,J,K0))   &
                                   +F1*(UC(I+1,J  ,K1)+UC(I,J,K1))   &
                                   +F2*(UC(I+1,J  ,K2)+UC(I,J,K2)) )
               DVDS(I+1,J+1) = .5*( F0*(VB(I  ,J+1,K0)+VB(I,J,K0))   &
                                   +F1*(VB(I  ,J+1,K1)+VB(I,J,K1))   &
                                   +F2*(VB(I  ,J+1,K2)+VB(I,J,K2)) )
            ENDDO
         ENDDO

         DO J=1,JX
            DO I=1,IX

              !-------- Calculate Potential Temperature ------------

               T00 = TB(I,J,K0) * (1.E5/PB(I,J,K0))**XE
               T1  = TB(I,J,K1) * (1.E5/PB(I,J,K1))**XE
               T2  = TB(I,J,K2) * (1.E5/PB(I,J,K2))**XE

              !------------ Finish Calculation ---------------------

               DTDS(I,J) = F0*T00 + F1*T1 + F2*T2

               THEH(I,J) = TB(I,J,K) * (1.E5/PB(I,J,K))**XE

              ! IF (J==100 .AND. I==100)     &
              ! write (*,*) "DTDS(100,100)=",DTDS(I,J), " THEH(100,100)=",THEH(I,J)

            ENDDO
         ENDDO

!
! ... COMPUTE HORIZONTAL DERIVATIVES: dTHETA/dx, dTHETA/dy
!
! 6/8/99, Horizontal gradients also calculated by 2nd order polynomials
! at all levels. In particular, this modifies orig code so PV at side
! boundaries are not biased low.
!
! ** mfac(col,row) = xmapc(col,row)**2 / dx from verthyd_hydro.F
!
         DSX=2.*DX
         DSY=2.*DY

         DO J=1,JX
            DO I=2,IM
               T1 = THEH(I-1,J) / XMSF(I-1,J)
               T2 = THEH(I+1,J) / XMSF(I+1,J)
               DTDX(I,J) = (XMSF(I,J)**2) * (T2-T1) / DSX

               ! IF (J==100 .AND. I==100) write (*,*) "XMSF(100,100)=",XMSF(I,J)

            ENDDO

            T1 = THEH(1,J) / XMSF(1,J)
            T2 = THEH(2,J) / XMSF(2,J)
            T3 = THEH(3,J) / XMSF(3,J)
            DTDX(1,J) = (XMSF(1,J)**2) * (-1.5*T1+2.*T2-.5*T3) / DX

            T00 = THEH(IM-1,J) / XMSF(IM-1,J)
            T1  = THEH(IM  ,J) / XMSF(IM  ,J)
            T2  = THEH(IX  ,J) / XMSF(IX  ,J)
            DTDX(IX,J) = (XMSF(IX,J)**2) * (.5*T00-2.*T1+1.5*T2) / DX
         ENDDO

         DO I=1,IX
            DO J=2,JM
               T1 = THEH(I,J-1) / XMSF(I,J-1)
               T2 = THEH(I,J+1) / XMSF(I,J+1)
               DTDY(I,J) = (XMSF(I,J)**2) * (T2-T1) / DSY

               ! IF (J==100 .AND. I==100) write (*,*) "XMSF(100,100)=",XMSF(I,J)

            ENDDO

            T1 = THEH(I,1) / XMSF(I,1)
            T2 = THEH(I,2) / XMSF(I,2)
            T3 = THEH(I,3) / XMSF(I,3)
            DTDY(I,1 ) = (XMSF(I,1 )**2) * (-1.5*T1+2.*T2-.5*T3) / DX

            T00 = THEH(I,JM-1) / XMSF(I,JM-1)
            T1  = THEH(I,JM  ) / XMSF(I,JM  )
            T2  = THEH(I,JX  ) / XMSF(I,JX  )
            DTDY(I,JX) = (XMSF(I,JX)**2) * (.5*T00-2.*T1+1.5*T2) / DX
         ENDDO

!
!     ... COMPUTE SLAB ABSOLUTE VORTICITY
!     ... Store Potential Vorticity in XPVC
!
!     ######################### NOTE ###################################
!
!     1. Because we use "X3" instead of "sigma" in equation,
!        "GRAV/PSB" will be replaced by "1/xrhojm" (rho * Jacobian)
!
!     2. To trim the process, "1/xrhojm" is not added in "XPVC" here,
!        but will be added in subroutine "metcro"
!
!     ##################################################################
!
         DO J=1,JX-2
            J1 = J+1
            DO I=1,IX-2
               I1=I+1
               VOR=(XMSF(I1,J1)**2)   &
                  *((VD(I1,J ,K)+VD(I1,J1,K)-VD(I,J,K)-VD(I ,J1,K))/DSX  &
                   -(UD(I ,J1,K)+UD(I1,J1,K)-UD(I,J,K)-UD(I1,J ,K))/DSY) &
                  +CORL(I1,J1)

               XPVC(I1,J1,K) = SCALE * ( VOR        *DTDS(I1,J1)        &
                                        -DVDS(I1,J1)*DTDX(I1,J1)        &
                                        +DUDS(I1,J1)*DTDY(I1,J1) )

             !  IF (J==99 .AND. I==99) write (*,*)  &
             !  "CORL(100,100)=",CORL(I,J), " PSB=",PSB, " XPVC(100,100,K)=",XPVC(I,J,K)

             !  WRITE(*,37) I, J, XMSF(I,J), VOR,       &
             !              UD(I ,J ,K), VD(I ,J ,K),   &
             !              UD(I1,J ,K), VD(I1,J ,K),   &
             !              UD(I1,J1,K), VD(I1,J1,K),   &
             !              UD(I ,J1,K), VD(I ,J1,K),   &
             !              XPVC(I1,J1,K)

            ENDDO
         ENDDO

!    ######################### Special treatment ###################################
!
!    1. Because PV at the boundary, when i=1 or J=1, need wind at "x=0" or "y=0", at
!       current "X domain" that did not have this option.  Though we can set extra
!       row and column, but there are no extra data at current AQF CONUS when row y=0
!       or column x=ncols_x. ==> The current CONUS AQF X-domain & meteorological 
!       domain setup do not allow extra raw and column to exist at bottom and right.
!
!    2. Limited Solution ==> use the inner pv at x=1 or y=1 as the boundary at x=0
!                            or y=o
!
!    3. For the future, the X-domain setup, "(X0,Y0) & NCOLS & NROWS", in premaq
!       running script should leave at least 1 free column and row at each boundary
!       for PV calculation.
!
!    *** the lines before "3000 CONTINUE" should be deleted if "3." is done
!
!    ###############################################################################

         DO J=1,JX-2
            J1 = J+1
            XPVC( 1, J1, K) = XPVC(   2, J1, K)    ! BC, Left
            XPVC(IX, J1, K) = XPVC(IX-1, J1, K)    ! BC, Right
         ENDDO

         DO I=1,IX-2
            I1=I+1
            XPVC(I1,  1, K) = XPVC(I1,    2, K)    ! BC, Bottom
            XPVC(I1, JX, K) = XPVC(I1, JX-1, K)    ! BC, Top
         ENDDO

         XPVC( 1,  1, K) = XPVC(   2,    2, K)     ! BC, Lower Left corner
         XPVC(IX,  1, K) = XPVC(IX-1,    2, K)     ! BC, Lower Right corner
         XPVC( 1, JX, K) = XPVC(2,    JX-1, K)     ! BC, Upper Left corner
         XPVC(IX, JX, K) = XPVC(IX-1, JX-1, K)     ! BC, Upper Right corner
        
 3000 CONTINUE

 37   FORMAT(2I4,1P,11E10.3)


      RETURN
      END


