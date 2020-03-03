
        SUBROUTINE  PLMRIS( EMLAYS, LPBL, LSTK, HFX, HMIX, STKDM,  
     &                      STKHT, STKTK, STKVE, TSTK, USTAR, DTHDZ, TA,  
     &                      WSPD, ZF, ZH, ZSTK, WSTK, ZPLM )

C***********************************************************************
C  subroutine body starts at line 141
C
C  DESCRIPTION:  
C       computes final effective plume centerline height.
C
C  PRECONDITIONS REQUIRED:
C	meteorology and stack parameters
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C
C  REVISION  HISTORY:
C       Prototype 12/95 by CJC, based on Briggs algorithm adapted from
C         RADM 2.6 subroutine PLUMER() (but with completely different 
C         data structuring).
C       Copied from plmris.F 4.4 by M Houyoux 3/99 
C
C***********************************************************************
C  
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: plmris.f,v 1.3 2002/03/18 15:40:14 mhouyoux Exp $
C  
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C  
C See file COPYRIGHT for conditions of use.
C  
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C  
C smoke@emc.mcnc.org
C  
C Pathname: $Source: /env/proj/archive/cvs/smoke/smoke/src/point/plmris.f,v $
C Last updated: $Date: 2002/03/18 15:40:14 $ 
C  
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'    ! I/O API constants
        INCLUDE 'FDESC3.EXT'    ! I/O API file description data structure
        INCLUDE 'IODECL3.EXT'   ! I/O API function declarations
        INCLUDE 'CONST3.EXT'    ! physical and mathematical constants

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT (IN) :: EMLAYS    ! no. of emission layers
        INTEGER, INTENT (IN) :: LPBL      ! lyr of height of PBL, = RADM's KMIX
        INTEGER, INTENT (IN) :: LSTK      ! lyr of top of stack, = RADM's KSTK
        REAL   , INTENT (IN) :: HFX            ! sensible heat flux (M K / S )
        REAL   , INTENT (IN) :: HMIX           ! mixing height (m)
        REAL   , INTENT (IN) :: STKDM          ! stack diameter (m)
        REAL   , INTENT (IN) :: STKHT          ! stack height (m)
        REAL   , INTENT (IN) :: STKTK          ! exhaust temperature (deg K)
        REAL   , INTENT (IN) :: STKVE          ! exhaust velocity (m/s)
        REAL   , INTENT (IN) :: TSTK           ! tmptr at top of stack (deg K)
        REAL   , INTENT (IN) :: USTAR          ! friction velocity (m/s)
        REAL   , INTENT (IN) :: DTHDZ( EMLAYS )! gradient of THETV
        REAL   , INTENT (IN) :: TA   ( EMLAYS )! temperature (deg K)
        REAL   , INTENT (IN) :: WSPD ( EMLAYS )! wind speed (m/s)
        REAL   , INTENT (IN) :: ZF ( 0:EMLAYS )! layer surface height (m)
        REAL   , INTENT (IN) :: ZH   ( EMLAYS )! layer center height (m) 
        REAL   , INTENT (IN) :: ZSTK ( EMLAYS )! zf( l )   - stkht   (m)
        REAL, INTENT(IN OUT) :: WSTK           ! wind speed @ top of stack (m/s)
        REAL  , INTENT(OUT) ::  ZPLM           !  temporarily plume top height
	                                       !  above stack, finally
					       ! plume centerline height
                                !    (can be greater than the height of the 
                                !     top of the EMLAYS layer)

C...........   PARAMETERS and their descriptions:

        REAL, PARAMETER :: GAMA    = -0.0098         ! ?? plume spread param
        REAL, PARAMETER :: HCRIT   =  1.0E-4 * 0.03  ! hfx min * tolerance
        REAL, PARAMETER :: CRDIST  =200.0            ! crit dstnce b/w HMIX & HS
c        REAL, PARAMETER :: SMALL   =  1.0E-5         ! Criterion for stability
        REAL, PARAMETER :: SMALL   =  3.0E-5         ! Criterion for stability
        REAL, PARAMETER :: D3      =  1.0 /  3.0     ! 1/ 3
        REAL, PARAMETER :: D30     =  1.0 / 30.0     ! 1/30
        REAL, PARAMETER :: D45     =  1.0 / 45.0     ! 1/45       (1.5*30.)
        REAL, PARAMETER :: D2664   =  1.0 /  2.664   ! 1/ 2.664
        REAL, PARAMETER :: D59319  =  1.0 / 59.319   ! 1/59.319
c        REAL, PARAMETER :: D1355   =  1.0 /  1.355   ! 1/ 1.355  (1.5cubed out)
c        REAL, PARAMETER :: D17576  =  1.0 / 17.576   ! 1/17.576  (1.5cubed out)
        REAL, PARAMETER :: TWOTHD  =  2.0 /  3.0     ! 2/ 3
        REAL, PARAMETER :: FIVETHD =  5.0 /  3.0     ! 5/ 3
        REAL, PARAMETER :: D6      =  1.0 / 6.0      ! 1 / 6
        REAL, PARAMETER :: NODIV0  =  1.0            ! Prevent divide by zero

C...........   EXTERNAL FUNCTIONS and their descriptions:

        REAL        POLY
        EXTERNAL    POLY

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER IQ              !  stab class:  1-unstbl,2-neut,3-stbl
        INTEGER LPLM            !  first L: ZH(L) > Plume height ! same as RADM's KPR
        INTEGER NN              !  Counter for interations through layers
        REAL    BFLX            !  buoyancy flux (m**4/s**3)
        REAL    DH		!  plume rise increment to center of the plume
        REAL    DHM             !  plume rise from momentum
	REAL    DHSM            !  stable momentum plume rise
        REAL    DHN             !  plume rise for neutral case
        REAL    DHT             !  plume rise increment to the top of the plume
        REAL    HSTAR           !  convective scale at stack (m**2/s**3)
        REAL    P, R, S         !  scratch coefficients
        REAL    RBFLX           !  residual buoyancy flux (m**4/s**3)
        REAL    TPLM            !  temperature at top of plume (m/s)
        REAL    WPLM            !  wind speed  at top of plume (m/s)
        REAL    ZMIX            !  hmix - hs
        REAL ZB                 !  height of bottom of a layer

C...........   STATEMENT FUNCTIONS:

        REAL    B, H, U, US     !  arguments
       
        REAL    NEUTRL		!  neutral-stability plume rise function
        REAL    STABLE		!  stable            plume rise function
        REAL    UNSTBL		!  unstable          plume rise function

        NEUTRL( H, B, U, US ) =
     &     MIN( 10.0 * H, 
     &          1.2 * ( (              B/( U*US*US )   )**0.6 *    ! pwr 3 * 0.2
     &                  (  H + 1.3 * ( B/( U*US*US ) ) )**0.4   )) ! pwr 2 * 0.2
C  07/02  REVISION : SIMPLIFY BY LETTING U* = U/12  US = WSPD below
C        NEUTRL( H, B, U, US ) =
C     &     MIN( 10.0 * H, 
C     &        1.2 * ( ( 144.*B/( U*US*US )  )**0.6 *    ! pwr 3 * 0.2
C     &    (  H + 1.3 * (144.*B/( U*US*US ) ) )**0.4   )) ! pwr 2 * 0.2

        STABLE( B, U, S ) =  2.6 * ( B / ( U * S ) )**D3

        UNSTBL( B, U )    = 30.0 * ( B / U )**0.6

C***********************************************************************
C   begin body of subroutine  PLMRIS

C.......   Compute buoyancy flux, convective scale.

        HSTAR = GRAV * HFX / TA( 1 )   ! Using surface temperature is correct
        BFLX  = 0.25*GRAV * ( STKTK-TSTK ) * STKVE * STKDM**2 / STKTK

C.......   Initialize layer of plume
        LPLM  = LSTK

C.......   Compute momentum rise ( set min wind speed to 1 m/s)
        WSTK = MAX(WSTK, 1.0 )
        DHM   = 3.0 * STKDM * STKVE / WSTK

C.......   When BFLX <= zero, use momentum rise only
C.......   NOTE: This part of algorithm added based on Models-3 plume rise

        IF( BFLX .LE. 0.0 ) THEN
C (06/02)  SET THE ZPLM PLUME RISE HEIGHT TO THE MOMENTUM VALUE DHM ABOVE
            ZPLM = STKHT + MAX( DHM, +2. )

            RETURN

        ENDIF

C.......   Compute initial plume rise from stack top to next level surface:

        IF( HSTAR .GT. HCRIT ) THEN		!  unstable case:
            ZMIX = HMIX - STKHT

            IF ( ZMIX .LE. 0.0 ) THEN           !  Stack above mixing height:
C                LPLM = MIN( EMLAYS-1, LPBL+1 )   ! Use LPLM = LSTK from above ...
                S    = MAX( GRAV * DTHDZ( LPLM ) / TSTK, SMALL )

C.................  Reset the wind speed at stack to the wind speed at plume
C.................  when the layer of the plume is not equal to the layer of
C.................  the stack.  This is from Models-3, and we have asked
C.................  EPA 10/8/97 why this is done and haven't gotten a response.
                IF( LPLM .NE. LSTK ) THEN
                    WSTK = WSPD( LPLM )
                    IF( WSTK .EQ. 0. ) WSTK = NODIV0
                ENDIF
		IF (DTHDZ(LPLM) .GT. 0.001 ) THEN
C  COMPUTE THE STABLE MOMENTUM RISE, FOR LAYER OF THE STACK.
		   DHSM = 0.646 * (STKVE*STKVE*STKDM*STKDM /
     &                (STKTK*WSTK))**D3 * SQRT(TSTK) / (DTHDZ(LPLM)**D6)
c                   WRITE(*,*) 'DHSM ', DHSM
		ELSE
		  DHSM = DHM         ! SET IT TO DHM , IF THGRAD TOO SMALL
		ENDIF
		
		DHM = MIN( DHSM, DHM )
		
C.  COMPUTE THE NEUTRAL AND STABLE PLUME RISES.		
                DHN = NEUTRL( STKHT, BFLX, WSTK, USTAR )
                DH  = STABLE( BFLX, WSTK, S )

                IF( DHN .LT. DH ) THEN  ! Take the minimum of neutral and stable
                    IQ = 2
                    DH = DHN
                ELSE 
                    IQ = 3 
                ENDIF

                IF( DHM .GT. DH .AND. WSTK .GT. 1. ) THEN
                    IQ = 4
                    DH = DHM
                ENDIF

                DHT = 1.5 * DH

            ELSE				!  unstable case:
                DH  = UNSTBL( BFLX, WSTK )
                DHN = NEUTRL( STKHT, BFLX, WSTK, USTAR )

                IF ( DHN .LT. DH ) THEN
                    DH = DHN
                    IQ = 2
                ELSE
                    IQ = 1
                END IF

                IF( DHM .GT. DH .AND. WSTK .GT. 1. ) THEN
                    DH = DHM
                    IQ = 4 
                ENDIF

                DHT = 1.5 * DH
	       
            END IF

         ELSE IF( HSTAR .LT. -HCRIT .OR. DTHDZ(LSTK) .GT. 0.001 ) THEN   ! stable case:

            S   = MAX( GRAV * DTHDZ( LSTK ) / TSTK, SMALL )

            DHT = 1.5 * STABLE( BFLX, WSTK, S )
            DHN = 1.5 * NEUTRL( STKHT, BFLX, WSTK, USTAR )

            IF ( DHN .LT. DHT ) THEN
                DHT = DHN
                IQ = 2
            ELSE
                IQ = 3
            END IF

        ELSE					!  neutral case:

            DHT = 1.5 * NEUTRL( STKHT, BFLX, WSTK, USTAR )

            IQ  = 2
        END IF			!  hstar ==> unstable, stable, or neutral
  
C.......   Compute further plume rise from between level surfaces:
        NN = 0
        RBFLX = BFLX
        ZPLM  = DHT
!         IF(LPLM .GT. 1) ZB = ZSTK(LPLM-1)
!         IF(LPLM .EQ. 1) ZB = -1.*STKHT
!         ZPLM = DHT + ZB
!	 DH = DH + ZB
!      if ( IQ .ne. 4) then
!      write(*,*) '1st: ',IQ,ZPLM,ZB,BFLX,S,WSTK,DTHDZ(LSTK),LPLM,DHT
!      end if

C.......   End calculations if the momentum rise was used in the calculation
C  Let interation proceed.
        IF( IQ .EQ. 4 ) GO TO 199  ! to point past iterative bouyancy loop

C.......   NOTE- LPLM has been initialized at line 145, and may have been
C                reset at line 169
        DO       !  loop computing further plume rise
         
            R = ZPLM - ZSTK( LPLM )
            IF( R .LE. 0.0 ) THEN
C                EXIT  ! exit plume rise loop
            GO TO 199
            ENDIF

C            ELSE IF ( LPLM .LT. EMLAYS ) THEN
C 4/14/04       LPLM = LPLM + 1    ! Increment layer after RBFLX calc below

           IF( LPLM .EQ. EMLAYS) THEN
                 ZPLM = MIN( ZPLM, ZSTK( EMLAYS ) )
C                EXIT  ! exit plume rise loop
                 write(*,*) 'PLUME RISE REACHED EMLAYS:', EMLAYS, ZPLM
                 stop
                 GO TO 199
           END IF

C...........   Re-set met data. NOTE- the original RADM code submitted the 
C...........   WSPD and TA to a interpolator, but then requested the a height of
C...........   interpolation identical to ZH( LPLM ).
            NN = NN + 1
            IF( NN .GT. 1 ) THEN
              WPLM = WSPD( LPLM )
              TPLM = TA  ( LPLM )
            ELSE                  ! 1st time, use stack values ...
              WPLM = WSTK
              TPLM = TSTK
           ENDIF
 
C...........   Compute residual bflx by stability case IQ:

            IF( IQ .EQ. 1 ) THEN
C                R     = D30 * R
		R     = D45 * R      ! Includes the 1.5 factor for plume top
                RBFLX = WPLM * (R**FIVETHD)
            ELSE IF ( IQ .EQ. 2 ) THEN
                 P = STKHT + TWOTHD * ZPLM         
C                 RBFLX = D2664 * R * WPLM * USTAR**2 * ( R / P )**TWOTHD
                 RBFLX = D2664*(R**FIVETHD)*WPLM*(USTAR**2.) / P**TWOTHD
            ELSE	!  else iq = 3:
                RBFLX = D59319 * WPLM * S * R**3
            END IF	!  if stability flag iq is 1, 2, or 3
c         write(*,*) 'After RBFLX : ', IQ,ZPLM,ZSTK(LPLM),WPLM,USTAR,S,RBFLX 
C  Increment the layer number below.
           IF( LPLM .LT. EMLAYS) LPLM = LPLM + 1
           WPLM = WSPD(LPLM)
           TPLM = TA (LPLM)
C...........   Prevent divide-by-zero by WPLM

            IF( WPLM .EQ. 0. ) WPLM = NODIV0

C...........   Process according to stability cases:

            S    = GRAV * DTHDZ( LPLM ) / TPLM
            IF( S .GT. SMALL ) THEN               ! stable case:
C  Use the theta gradient to determine pr eqn.
C             IF (DTHDZ(LPLM) .GT. 0.001 ) THEN
                DHT = 1.5 * STABLE( RBFLX, WPLM, S )
                DHN = 1.5 * NEUTRL( STKHT, RBFLX, WPLM, USTAR )
               IF ( DHN .LT. DHT ) THEN
                    DHT = DHN
                    IQ  = 2
                ELSE
                    IQ  = 3
                END IF
                DH = DHT / 1.5
            ELSE          ! if upper layer is not stable, use neutral formula
                            
                DHN =  NEUTRL( STKHT, RBFLX, WPLM, USTAR )

C  07/02  REVISION TO ALSO COMPUTE THE UNSTABLE RISE
                DH  = UNSTBL( RBFLX, WPLM )
                IQ = 1
                IF ( DHN .LT. DH ) THEN
                    DH = DHN
                    IQ  = 2
		ENDIF
		DHT = 1.5*DH
C  END OF REVISION		
            END IF
C 
            ZPLM = ZSTK( LPLM-1 ) + DHT
	    DH = ZSTK( LPLM-1 )   + DH 
	    
!       WRITE(*,*) 'PLMRIS :', NN,IQ, DTHDZ(LPLM), S, RBFLX, DH, ZPLM, 
!     &         ZSTK(LPLM-1),ZSTK( LPLM), LPLM
        END DO

199     CONTINUE   !  end loop computing further plume rise

C  (06/02) Adjustment for layer 1 combustion pt. source stacks with
C  plume rise limited to layer 1; put plume height in middle of layer 2.
      IF ((TWOTHD*ZPLM+STKHT) .LE. ZF(1) .AND. STKTK .GT. TA(1)) THEN
            ZPLM = ZH(2)
      ENDIF

C....... Compute plume rise amount (DH) and actual final plume 
C....... centerline height (ZPLM):
!	write (*,*) ZPLM,STKHT
	
	DH  = TWOTHD*ZPLM
        ZPLM = TWOTHD*ZPLM + STKHT             

        RETURN

        END SUBROUTINE PLMRIS
