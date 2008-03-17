
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrg1.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%



       SUBROUTINE HRG1( DTC )

C**********************************************************************
C
C  FUNCTION: To solve for the concentration of NO2, NO, O3, and O3P
C            algebraically.  
C
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2003
C                    
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE 


C..INCLUDES: None


C..ARGUMENTS:
      REAL DTC                      ! Time step


C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER*16 PNAME              ! Program name
      SAVE PNAME 

      
C..SCRATCH LOCAL VARIABLES:
      REAL O1D_DNM             ! Denominator term for O1D conc
      REAL O3P_S               ! stoich coeff for O3P from O1D

      REAL R1_2                ! Production term for NO from NO2
      REAL R2_1                ! Production term for NO2 from NO
      REAL P1, P2, P3, P12     ! Production terms for NO, NO2, O3, & O3P
      REAL L1, L2, L3, L12     ! Loss terms for NO, NO2, O3, O3P
      REAL L1_INV, L2_INV, 
     &       L3_INV, L12_INV     ! Inverse of loss terms

      REAL T1, T2, T3, T4, T5  ! Intermerdiate terms
      REAL F1, F2, F3          ! Intermerdiate terms
      REAL A, B, C             ! Coefficients for quadratic equation
      REAL Q, XX, S1, S2       ! Intermerdiate terms

      REAL RK1, RK2, RK3

      REAL PO3

C**********************************************************************
      DATA PNAME  / 'HRG1'/

c..Sum of O1D loss
      O1D_DNM = RKI(  10 ) + RKI(  11 ) + RKI(  12 )

c..O3P fractional yield from O1D
      O3P_S = ( RKI(  10 ) + RKI(  11 ) ) / O1D_DNM

 
c..Production of NO from NO2
      R1_2 =           RKI(   1 )                  
     &       +         RKI(   4 ) * YC( O )        
     &       + 0.200 * RKI(  93 ) * YC( ISOP ) 
      R1_2 = R1_2 * DTC   

c..Remaining NO production terms
      P1  =    0.110 * RXRAT(  15 )     +         RXRAT(  17 )
     &       +         RXRAT(  24 )     +         RXRAT(  26 )
      P1 = YC0(   NO ) + P1 * DTC

c..Loss terms for NO
      L1  =            RKI(  22 ) * YC(  NO2 )
     &       +         RKI(  23 ) * YC(   OH )
     &       + 0.100 * RKI(  65 ) * YC(  TO2 )
     &       +         RKI(  82 ) * YC( XO2N )
      L1 = 1.0 + L1 * DTC

c..Production of NO2 form NO except NO+O3=NO2+O
      R2_1 =           RKI(   6 ) * YC(    O )
     &       +         RKI(  16 ) * YC(  NO3 )
     &       + 2.000 * RKI(  21 ) * YC(   NO )
     &       +         RKI(  29 ) * YC(  HO2 )
     &       +         RKI(  47 ) * YC( C2O3 )
     &       + 0.900 * RKI(  65 ) * YC(  TO2 )
     &       +         RKI(  80 ) * YC(  XO2 )
      R2_1 = R2_1 * DTC

c..Remaining NO2 production terms
      P2  =   0.890 * RXRAT(  15 ) +         RXRAT(  16 )
     &      +         RXRAT(  20 ) +         RXRAT(  25 )
     &      +         RXRAT(  26 ) +         RXRAT(  31 )
     &      +         RXRAT(  32 ) +         RXRAT(  49 )
     &      +         RXRAT(  60 ) + 0.200 * RXRAT(  79 )
      P2 = YC0(   NO2 ) + P2 * DTC


c..Loss terms for NO2
      L2  =           RKI(   5 ) * YC(    O )
     &      +         RKI(   7 ) * YC(   O3 )
     &      +         RKI(  18 ) * YC(  NO3 )
     &      +         RKI(  22 ) * YC(   NO )
     &      +         RKI(  27 ) * YC(   OH )
     &      +         RKI(  30 ) * YC(  HO2 )
     &      +         RKI(  48 ) * YC( C2O3 )
     &      +         RKI(  56 ) * YC(  ROR )
     &      +         RKI(  69 ) * YC(  CRO )
     &      + 0.800 * RKI(  93 ) * YC( ISOP )
      L2 = 1.0 + L2 * DTC

c..Production terms for O3P except NO2+hv=O3P...
      P12 =   O3P_S * RXRAT(   9 ) + 0.890 * RXRAT(  15 )
     &      +         RXRAT(   8 )
      P12 = YC0( O ) + P12 * DTC 

c..Loss terms for O3P
      L12 =           RKI(   2 )   
     &      +         RKI(   4 ) * YC(  NO2 )
     &      +         RKI(   5 ) * YC(  NO2 )
     &      +         RKI(   6 ) * YC(   NO )
     &      +         RKI(  41 ) * YC( FORM )
     &      +         RKI(  43 ) * YC( ALD2 )
     &      +         RKI(  57 ) * YC(  OLE )
     &      +         RKI(  61 ) * YC(  ETH )
     &      +         RKI(  76 ) * YC( ISOP )
      L12 = 1.0 + L12 * DTC

c..Production terms for O3 except O+O2=O3
      P3 = YC0(  O3 )

c..Loss terms for O3 except NO+O3=NO2
      L3 =            RKI(   7 ) * YC(  NO2 )
     &      +         RKI(   8 )
     &      +         RKI(   9 )
     &      +         RKI(  13 ) * YC(   OH )
     &      +         RKI(  14 ) * YC(  HO2 )
     &      +         RKI(  59 ) * YC(  OLE )
     &      +         RKI(  63 ) * YC(  ETH )
     &      +         RKI(  73 ) * YC( OPEN )
     &      +         RKI(  78 ) * YC( ISOP )
     &      +         RKI(  90 ) * YC( ISPD )
      L3 = 1.0 + L3 * DTC


c..Compute reciprocal of loss terms
      L1_INV  = 1.0 / L1
      L2_INV  = 1.0 / L2
      L3_INV  = 1.0 / L3
      L12_INV = 1.0 / L12

c..compute k*delta t terms
      RK1 = RKI(   1 ) * DTC
      RK2 = RKI(   2 ) * DTC
      RK3 = RKI(   3 ) * DTC

c..Compute terms that are used to calulate a,b & c
      T1 = RK1  * L2_INV                ! J1   / ( 1.0 + Lno2 * dt )
      T2 = R1_2 * L2_INV                ! r1,2 / ( 1.0 + Lno2 * dt)
      T3 = R2_1 * L1_INV                ! r2,1 / ( 1.0 + Lno  * dt)
      T4 = RK2  * L12_INV               ! J2   / ( 1.0 + Lo3p * dt )
      T5 = T3   * P1 - T2 * P2          ! T3 * Pno - T2 * Pno2

      F1 = 1.0 + T2 + T3                ! factor in calculating a & b
      F2 = T1 * T4                      ! factor in calculating a & b
      F3 = L3 * L1 + RK3 * P1           ! (1 + Lo3 * dt) (1 + lno * dt )
                                        ! + rk3 * dt * Pno

      PO3 = P3 + P12 * T4 

      A = RK3 * ( F1  - F2 )

      B = F1 * F3 +  RK3 * ( F2 * ( P2 - P1 ) + PO3 +  T5 )

      C = RK3 * P1 * ( PO3 + P2 * F2 ) + F3 * T5

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B - 4.0 * A * C ) )

      XX = MAX( Q / A , C / Q  )

      YCP( NO  ) = MAX( 0.0, ( P1 + XX ) * L1_INV )

      YCP( NO2 ) = MAX( 0.0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NO2 )

      S2 = T4 * S1

      YCP(  O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP(   O ) = S1 * L12_INV

      YCP( O1D ) = RKI( 9 ) * YCP( O3 ) / O1D_DNM

      RETURN

      END
     















      
