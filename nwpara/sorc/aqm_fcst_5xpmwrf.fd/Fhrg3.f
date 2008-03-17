
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrg3.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%



       SUBROUTINE HRG3( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of C2O3 and PAN
C   
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2002
C                    
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE 

C..INCLUDES:  NONE

C..ARGUMENTS:      
      REAL DTC              ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER*16 PNAME      ! Program name
      SAVE PNAME

      
C..SCRATCH LOCAL VARIABLES:
      REAL A, B, C, Q   ! Quadratic equation terms
      REAL CMN          ! Temp scalar
      REAL LC2O3        ! Loss of C2O3
      REAL LPAN         ! Loss of PAN
      REAL PC2O3        ! Production of C2O3

      REAL R88DT        ! Kc2o3+c2o3 x delta t
      REAL R89DT        ! Kpan-->c2o3 x delta t
      REAL R98DT        ! Kc2o3+no2-->pan

C**********************************************************************
      DATA PNAME  / 'HRG3'/

c..Production rate of C2O3 (except from PAN )

      PC2O3 =          RXRAT( 43 ) +         RXRAT( 44 ) +         
     &                 RXRAT( 45 ) +         RXRAT( 71 ) +
     *                 RXRAT( 72 ) + 0.620 * RXRAT( 73 ) +
     &                 RXRAT( 74 ) +         RXRAT( 75 ) +
     &         0.250 * RXRAT( 76 ) + 0.200 * RXRAT( 78 ) +
     &         0.498 * RXRAT( 89 ) + 0.114 * RXRAT( 90 ) +
     &         0.075 * RXRAT( 91 ) + 0.967 * RXRAT( 92 )          


 
c..Loss frequency of C2O3 ( not including C2O3 + C2O3 )
      LC2O3 =  RKI( 47 ) * YC( NO ) + RKI( 48 ) * YC( NO2 ) +
     &         RKI( 51 ) * YC( HO2 )


c..Loss frequency of PAN
      LPAN  =  RKI( 49 )

c..Solution of quadratic equation to get C2O3 & PAN
c....R88DT = K for C2O3+C2O3 times delta t
c....R89DT = K for PAN times delta t
c....R98DT = K for C2O3+NO2=PAN times delta t times [NO2]

      R88DT = RKI( 50 ) * DTC
      R89DT = RKI( 49 ) * DTC
      R98DT = RKI( 48 ) * YC( NO2 ) * DTC

      CMN = 1.0 + LPAN * DTC
      A = 2.0 * R88DT * CMN
      B = CMN * ( 1.0 + LC2O3 * DTC ) - R89DT * R98DT
      C = CMN * ( YC0( C2O3 ) + PC2O3 * DTC ) + R89DT * YC0( PAN )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP( C2O3 ) = MAX( Q / A , -C / Q  )

      YCP( PAN ) = ( YC0( PAN ) + R98DT * YCP( C2O3 ) ) / CMN

      RETURN

      END 
