
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrg2.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%




      SUBROUTINE HRG2( DTC )
C**********************************************************************
C
C  FUNCTION: To solve for the concentration of HO, HO2, HONO, and
C            PNA alebraically.    
C
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2003
C                    Modified 11/03 by J. Gipson to add prod of 1.0 HO2
C                      from OH+OPEN to P5 (remaining HO2 production terms) 
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
      REAL    OH_S                  ! stoich coeff for OH from O1D+H2O=OH
      REAL    R4_19                 ! production of OH from HONO
      REAL    R19_4                 ! production of HONO from OH
      REAL    R4_5                  ! production of OH from HO2
      REAL    R5_4                  ! production of HO2 from OH
      REAL    R5_21                 ! production of HO2 from PNA
      REAL    R21_5                 ! production of PNA from HO2
      REAL    P4, P5, P19, P21      ! species production form other paths 
      REAL    L4, L5, L19, L21      ! species loss terms

      REAL    A, B, C               ! coeef of quadartic eq. for HO2
      REAL    Q                     ! intermediate term

      REAL    T1, T2, T3            ! intermediate terms

      REAL    L21_INV               ! reciprocal of PNA loss term

C**********************************************************************
      DATA PNAME  / 'HRG2'/


c..stoichiometric coeeficient for production of OH from O3 via O1D
      OH_S = 2.0 * RKI(  12 ) / ( RKI(  10 ) + RKI(  11 ) + RKI(  12 ) )


c..Production of OH from HONO (r4,19 terms )
      R4_19 =            RKI(  24 )

      R4_19 = R4_19 * DTC

c..Production of HONO from OH (r19,4 terms )
      R19_4 =            RKI(  23 ) * YC(   NO )

      R19_4 = R19_4 * DTC


c..Production of OH from HO2 (r4,5 terms )
      R4_5   =          RKI(  14 ) * YC(   O3 )
     &        +         RKI(  29 ) * YC(   NO )
     &        + 0.210 * RKI(  51 ) * YC( C2O3 )

      R4_5  = R4_5 * DTC


c..Production of PNA from HO2 (r21,5 term )
      R21_5 =   RKI(  30 ) * YC(   NO2 ) * DTC


c..Production of HO2 from PNA (r5,21 term )
      R5_21 =  RKI(  31 )  * DTC


c..Remaining OH production
      P4 =  OH_S  * RXRAT(   9 )  +  2.000 * RXRAT(  35 )
     &    +         RXRAT(  41 )  +          RXRAT(  43 )
     &    + 0.580 * RXRAT(  51 )  +  0.200 * RXRAT(  57 )
     &    + 0.100 * RXRAT(  59 )  +  0.300 * RXRAT(  61 )
     &    + 0.080 * RXRAT(  73 )  +  0.266 * RXRAT(  78 )
     &    + 0.268 * RXRAT(  90 )

      P4 = YC0(   OH ) + P4 * DTC

c..Production of HO2 from OH ( r5,4 terms )
      R5_4 =            RKI(  13 ) * YC(    O3 )
     &        +         RKI(  36 ) * YC(  H2O2 )
     &        +         RKI(  37 ) * YC(    CO )
     &        +         RKI(  38 ) * YC(  FORM )
     &        +         RKI(  52 )
     &        + 0.110 * RKI(  53 ) * YC(   PAR )
     &        +         RKI(  58 ) * YC(   OLE )
     &        +         RKI(  62 ) * YC(   ETH )
     &        + 0.440 * RKI(  64 ) * YC(   TOL )
     &        + 0.600 * RKI(  67 ) * YC(  CRES )
     &        + 0.700 * RKI(  70 ) * YC(   XYL )
     &        +         RKI(  71 ) * YC(  OPEN )
     &        + 0.912 * RKI(  77 ) * YC(  ISOP )
     &        +         RKI(  83 ) * YC(   SO2 )
     &        + 0.503 * RKI(  89 ) * YC(  ISPD )

      R5_4  = R5_4 * DTC


c..Remaining HO2 production terms
      P5   =    2.000 * RXRAT(  39 )  +          RXRAT(  41 )
     &       +          RXRAT(  42 )  +  2.000 * RXRAT(  46 )
     &       +          RXRAT(  47 )  +  2.000 * RXRAT(  50 )
     &       +  0.940 * RXRAT(  54 )  +          RXRAT(  55 )
     &       +  0.380 * RXRAT(  57 )  +  0.440 * RXRAT(  59 )
     &       +  1.700 * RXRAT(  61 )  +  0.120 * RXRAT(  63 )
     &       +  0.900 * RXRAT(  65 )  +          RXRAT(  66 )
     &       +          RXRAT(  72 )  +  0.760 * RXRAT(  73 )
     &       +          RXRAT(  75 )  +  0.250 * RXRAT(  76 )
     &       +  0.066 * RXRAT(  78 )  +  0.800 * RXRAT(  79 )
     &       +  0.154 * RXRAT(  90 )  +  0.925 * RXRAT(  91 )
     &       +  1.033 * RXRAT(  92 )  +  0.800 * RXRAT(  93 )
     &       +          RXRAT(  71 )

      P5 = YC0(  HO2 ) + P5 * DTC


c..OH Loss terms not yet accounted for
      L4   =  +         RKI(  25 ) * YC(  HONO )         
     &        +         RKI(  27 ) * YC(   NO2 )
     &        +         RKI(  28 ) * YC(  HNO3 )
     &        +         RKI(  32 ) * YC(   PNA )
     &        +         RKI(  44 ) * YC(  ALD2 )
     &        + 0.890 * RKI(  53 ) * YC(   PAR )
     &        + 0.560 * RKI(  64 ) * YC(   TOL )
     &        + 0.400 * RKI(  67 ) * YC(  CRES )
     &        + 0.300 * RKI(  70 ) * YC(   XYL )
     &        +         RKI(  74 ) * YC(  MGLY )
     &        + 0.088 * RKI(  77 ) * YC(  ISOP )
     &        + 0.497 * RKI(  89 ) * YC(  ISPD )

      L4 = 1.0 + L4 * DTC + R5_4 + R19_4

c..HO2 Loss terms not yet accounted for (except for HO2+HO2 )
      L5   =  +         RKI(  85 ) * YC(   XO2 )         
     &        +         RKI(  86 ) * YC(  XO2N )

      L5 = 1.0 + L5 * DTC + R4_5 + R21_5

c..Remaining HONO loss terms
      L19  =  +         RKI(  25 ) * YC(   OH )         
     &        + 2.000 * RKI(  26 ) * YC(  HONO )

      L19 = 1.0 + L19 * DTC + R4_19

c..Remaining HONO production terms
      P19  =  YC0( HONO ) +  2.0 * RXRAT(  22  ) * DTC 


c..Remaining PNA loss terms
      L21  =    RKI(  32 ) * YC(   OH )   

      L21 = 1.0 + L21 * DTC + R5_21

      L21_INV = 1.0 / L21

c..Remaining PNA production terms
      P21  =  YC0(  PNA ) 
      
c..compute terms used to calculate a,b & c
      T1 = 1.0 / ( L4 * L19 - R4_19 * R19_4 )
      T2 = R5_4 * T1
      T3 = R5_21 * L21_INV

c..solve quadratic equation for HO2
      A = 2.0 * ( RKI(  33 ) + RKI(  34 ) ) * DTC

      B = L5 - T3 * R21_5 - T2 * R4_5 * L19

      C = P5 + T3 * P21 + T2 * ( P4 * L19 + P19 * R4_19 )

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B + 4.0 * A * C ) )

      YCP(  HO2 ) = MAX( Q / A , -C / Q  )

c..compute remaining species concentrations
      YCP(   OH ) = ( ( P4 + R4_5 * YCP(   HO2 ) ) * L19 + R4_19 * P19 ) * T1

      YCP(  PNA ) = ( P21 + R21_5 * YCP(  HO2 ) ) * L21_INV

      YCP( HONO ) = ( P19 + R19_4 * YCP(   OH ) ) / L19

      RETURN

      END


