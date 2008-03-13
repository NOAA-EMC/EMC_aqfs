
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrprodloss.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%




      SUBROUTINE HRPRODLOSS

C***********************************************************************
C
C  FUNCTION: To compute the production and loss rates for EBI species 
C
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2002
C
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE


C.....INCLUDES: NONE


C.....ARGUMENTS: NONE 


C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS: NONE


C.....SAVED VARIABLES: NONE


C.....LOCAL VARIABLES: 
      CHARACTER*16  PNAME    ! Program name

      INTEGER ESP

C***********************************************************************
      DATA PNAME / 'HRPRODLOSS' / 


      PROD( HNO3    ) =    2.000 * RXRAT(  19 )
     &                  +          RXRAT(  27 )
     &                  +          RXRAT(  42 )
     &                  +          RXRAT(  45 )
     &                  +          RXRAT(  68 )
     &                  +  0.075 * RXRAT(  91 )
      LOSS( HNO3    ) =            RXRAT(  28 )

      PROD( H2O2    ) =            RXRAT(  33 )
     &                  +          RXRAT(  34 )
      LOSS( H2O2    ) =            RXRAT(  35 )
     &                  +          RXRAT(  36 )

      PROD( CO      ) =            RXRAT(  38 )
     &                  +          RXRAT(  39 )
     &                  +          RXRAT(  40 )
     &                  +          RXRAT(  41 )
     &                  +          RXRAT(  42 )
     &                  +          RXRAT(  46 )
     &                  +  0.300 * RXRAT(  57 )
     &                  +  0.330 * RXRAT(  59 )
     &                  +          RXRAT(  61 )
     &                  +  0.420 * RXRAT(  63 )
     &                  +  2.000 * RXRAT(  71 )
     &                  +          RXRAT(  72 )
     &                  +  0.690 * RXRAT(  73 )
     &                  +          RXRAT(  75 )
     &                  +  0.066 * RXRAT(  78 )
     &                  +  0.334 * RXRAT(  89 )
     &                  +  0.225 * RXRAT(  90 )
     &                  +  0.643 * RXRAT(  91 )
     &                  +  0.333 * RXRAT(  92 )
      LOSS( CO      ) =            RXRAT(  37 )

      PROD( FORM    ) =            RXRAT(  46 )
     &                  +          RXRAT(  47 )
     &                  +  2.000 * RXRAT(  50 )
     &                  +  0.790 * RXRAT(  51 )
     &                  +          RXRAT(  52 )
     &                  +  0.200 * RXRAT(  57 )
     &                  +          RXRAT(  58 )
     &                  +  0.740 * RXRAT(  59 )
     &                  +          RXRAT(  60 )
     &                  +          RXRAT(  61 )
     &                  +  1.560 * RXRAT(  62 )
     &                  +          RXRAT(  63 )
     &                  +          RXRAT(  71 )
     &                  +  0.700 * RXRAT(  73 )
     &                  +  0.500 * RXRAT(  76 )
     &                  +  0.629 * RXRAT(  77 )
     &                  +  0.600 * RXRAT(  78 )
     &                  +  0.167 * RXRAT(  89 )
     &                  +  0.150 * RXRAT(  90 )
     &                  +  0.282 * RXRAT(  91 )
     &                  +  0.900 * RXRAT(  92 )
      LOSS( FORM    ) =            RXRAT(  38 )
     &                  +          RXRAT(  39 )
     &                  +          RXRAT(  40 )
     &                  +          RXRAT(  41 )
     &                  +          RXRAT(  42 )


      PROD( ALD2    ) =    0.110 * RXRAT(  53 )
     &                  +  1.100 * RXRAT(  54 )
     &                  +  0.630 * RXRAT(  57 )
     &                  +          RXRAT(  58 )
     &                  +  0.500 * RXRAT(  59 )
     &                  +          RXRAT(  60 )
     &                  +  0.220 * RXRAT(  62 )
     &                  +  0.030 * RXRAT(  73 )
     &                  +  0.150 * RXRAT(  78 )
     &                  +  0.800 * RXRAT(  79 )
     &                  +  0.273 * RXRAT(  89 )
     &                  +  0.020 * RXRAT(  90 )
     &                  +  0.357 * RXRAT(  91 )
     &                  +  0.067 * RXRAT(  92 )
     &                  +  0.800 * RXRAT(  93 )
      LOSS( ALD2    ) =            RXRAT(  43 )
     &                  +          RXRAT(  44 )
     &                  +          RXRAT(  45 )
     &                  +          RXRAT(  46 )

      PROD( XO2     ) =            RXRAT(  46 )
     &                  +          RXRAT(  47 )
     &                  +  2.000 * RXRAT(  50 )
     &                  +  0.790 * RXRAT(  51 )
     &                  +          RXRAT(  52 )
     &                  +  0.870 * RXRAT(  53 )
     &                  +  0.960 * RXRAT(  54 )
     &                  +  0.280 * RXRAT(  57 )
     &                  +          RXRAT(  58 )
     &                  +  0.220 * RXRAT(  59 )
     &                  +  0.910 * RXRAT(  60 )
     &                  +  0.700 * RXRAT(  61 )
     &                  +          RXRAT(  62 )
     &                  +  0.080 * RXRAT(  64 )
     &                  +  0.600 * RXRAT(  67 )
     &                  +  0.500 * RXRAT(  70 )
     &                  +          RXRAT(  71 )
     &                  +  0.030 * RXRAT(  73 )
     &                  +          RXRAT(  74 )
     &                  +  0.250 * RXRAT(  76 )
     &                  +  0.991 * RXRAT(  77 )
     &                  +  0.200 * RXRAT(  78 )
     &                  +          RXRAT(  79 )
     &                  +  0.713 * RXRAT(  89 )
     &                  +  0.064 * RXRAT(  90 )
     &                  +  0.075 * RXRAT(  91 )
     &                  +  0.700 * RXRAT(  92 )
     &                  +          RXRAT(  93 )
      LOSS( XO2     ) =            RXRAT(  80 )
     &                  +  2.000 * RXRAT(  81 )
     &                  +          RXRAT(  85 )
     &                  +          RXRAT(  88 )

      PROD( PAR     ) =    0.220 * RXRAT(  57 )
     &                  +  1.100 * RXRAT(  70 )
     &                  +  0.250 * RXRAT(  76 )
     &                  +  0.350 * RXRAT(  78 )
     &                  +  2.400 * RXRAT(  79 )
     &                  +  1.565 * RXRAT(  89 )
     &                  +  0.360 * RXRAT(  90 )
     &                  +  1.282 * RXRAT(  91 )
     &                  +  0.832 * RXRAT(  92 )
     &                  +  2.400 * RXRAT(  93 )
      LOSS( PAR     ) =    1.110 * RXRAT(  53 )
     &                  +  2.100 * RXRAT(  54 )
     &                  +          RXRAT(  58 )
     &                  +          RXRAT(  59 )
     &                  +          RXRAT(  60 )

      PROD( XO2N    ) =    0.130 * RXRAT(  53 )
     &                  +  0.040 * RXRAT(  54 )
     &                  +  0.020 * RXRAT(  57 )
     &                  +  0.090 * RXRAT(  60 )
     &                  +  0.088 * RXRAT(  77 )
      LOSS( XO2N    ) =            RXRAT(  82 )
     &                  +          RXRAT(  86 )
     &                  +  2.000 * RXRAT(  87 )
     &                  +          RXRAT(  88 )

      PROD( ROR     ) =    0.760 * RXRAT(  53 )
      LOSS( ROR     ) =    0.980 * RXRAT(  54 )
     &                  +          RXRAT(  55 )
     &                  +          RXRAT(  56 )

      PROD( NTR     ) =            RXRAT(  56 )
     &                  +  0.100 * RXRAT(  65 )
     &                  +          RXRAT(  69 )
     &                  +  0.800 * RXRAT(  79 )
     &                  +          RXRAT(  82 )
     &                  +  0.850 * RXRAT(  91 )
     &                  +  0.800 * RXRAT(  93 )
      LOSS( NTR     ) =    0.0

      PROD( OLE     ) =    0.0
      LOSS( OLE     ) =            RXRAT(  57 )
     &                  +          RXRAT(  58 )
     &                  +          RXRAT(  59 )
     &                  +          RXRAT(  60 )

      PROD( ETH     ) =    0.0
      LOSS( ETH     ) =            RXRAT(  61 )
     &                  +          RXRAT(  62 )
     &                  +          RXRAT(  63 )

      PROD( TOL     ) =    0.0
      LOSS( TOL     ) =            RXRAT(  64 )

      PROD( CRES    ) =    0.360 * RXRAT(  64 )
     &                  +          RXRAT(  66 )
     &                  +  0.200 * RXRAT(  70 )
      LOSS( CRES    ) =            RXRAT(  67 )
     &                  +          RXRAT(  68 )

      PROD( TO2     ) =    0.560 * RXRAT(  64 )
     &                  +  0.300 * RXRAT(  70 )
      LOSS( TO2     ) =            RXRAT(  65 )
     &                  +          RXRAT(  66 )

      PROD( OPEN    ) =    0.900 * RXRAT(  65 )
     &                  +  0.300 * RXRAT(  67 )
      LOSS( OPEN    ) =            RXRAT(  71 )
     &                  +          RXRAT(  72 )
     &                  +          RXRAT(  73 )

      PROD( CRO     ) =    0.400 * RXRAT(  67 )
     &                  +          RXRAT(  68 )
      LOSS( CRO     ) =            RXRAT(  69 )

      PROD( XYL     ) =    0.0
      LOSS( XYL     ) =            RXRAT(  70 )

      PROD( MGLY    ) =    0.800 * RXRAT(  70 )
     &                  +  0.200 * RXRAT(  73 )
     &                  +  0.168 * RXRAT(  89 )
     &                  +  0.850 * RXRAT(  90 )
      LOSS( MGLY    ) =            RXRAT(  74 )
     &                  +          RXRAT(  75 )

      PROD( ISOP    ) =    0.0
      LOSS( ISOP    ) =            RXRAT(  76 )
     &                  +          RXRAT(  77 )
     &                  +          RXRAT(  78 )
     &                  +          RXRAT(  79 )
     &                  +          RXRAT(  93 )

      PROD( ISPD    ) =    0.750 * RXRAT(  76 )
     &                  +  0.912 * RXRAT(  77 )
     &                  +  0.650 * RXRAT(  78 )
     &                  +  0.200 * RXRAT(  79 )
     &                  +  0.200 * RXRAT(  93 )
      LOSS( ISPD    ) =            RXRAT(  89 )
     &                  +          RXRAT(  90 )
     &                  +          RXRAT(  91 )
     &                  +          RXRAT(  92 )

      PROD( SO2     ) =    0.0
      LOSS( SO2     ) =            RXRAT(  83 )
     &                  +          RXRAT(  84 )

      PROD( SULF    ) =            RXRAT(  83 )
     &                  +          RXRAT(  84 )
      LOSS( SULF    ) =    0.0


      IF( L_AE_VRSN ) THEN

         PROD( TOLAER  ) =            RXRAT(  64 )
         LOSS( TOLAER  ) =    0.0
   
         PROD( CSLAER  ) =            RXRAT(  67 )
     &                     +          RXRAT(  68 )
         LOSS( CSLAER  ) =    0.0

         PROD( XYLAER  ) =            RXRAT(  70 )
         LOSS( XYLAER  ) =    0.0

         PROD( SULAER  ) =            RXRAT(  83 )
     &                     +          RXRAT(  84 )
         LOSS( SULAER  ) =    0.0


         PROD( TERP    ) =    0.0
         LOSS( TERP    ) =            RXRAT(  94 )
     &                     +          RXRAT(  95 )
     &                     +          RXRAT(  96 )

         PROD( TERPAER ) =            RXRAT(  94 )
     &                     +          RXRAT(  95 )
     &                     +          RXRAT(  96 )
         LOSS( TERPAER ) =    0.0

      ENDIF


      IF( L_AQ_VRSN ) THEN

         PROD( PACD    ) =    0.210 * RXRAT(  51 )
         LOSS( PACD    ) =    0.0

         PROD( FACD    ) =    0.200 * RXRAT(  59 )
     &                     +  0.400 * RXRAT(  63 )
         LOSS( FACD    ) =    0.0

         PROD( AACD    ) =    0.200 * RXRAT(  59 )
         LOSS( AACD    ) =    0.0


         PROD( UMHP    ) =            RXRAT(  85 )
         LOSS( UMHP    ) =    0.0

      ENDIF


      PNEG( PAR ) =     2.100 * RXRAT(  54 )
     &               +          RXRAT(  58 )
     &               +          RXRAT(  59 )
     &               +          RXRAT(  60 )


      RETURN

      END
