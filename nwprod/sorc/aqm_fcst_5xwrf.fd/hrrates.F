
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrrates.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%




      SUBROUTINE HRRATES

C***********************************************************************
C
C  FUNCTION: To calculate rates of reactions 
C
C  PRECONDITIONS: For CB4 family of mechanisms
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

C***********************************************************************
      DATA PNAME / 'HRRATES' / 


C..NOTE: RXRAT commented out are not referenced by any routine

C..Photolytic reactions
!      RXRAT(   1 ) = RKI(   1 )  *  YC( NO2 )
      RXRAT(   8 ) = RKI(   8 )  *  YC( O3 )
      RXRAT(   9 ) = RKI(   9 )  *  YC( O3 )
      RXRAT(  15 ) = RKI(  15 )  *  YC( NO3 )
      RXRAT(  24 ) = RKI(  24 )  *  YC( HONO )
      RXRAT(  35 ) = RKI(  35 )  *  YC( H2O2 )
      RXRAT(  39 ) = RKI(  39 )  *  YC( FORM )
      RXRAT(  40 ) = RKI(  40 )  *  YC( FORM )
      RXRAT(  46 ) = RKI(  46 )  *  YC( ALD2 )
      RXRAT(  72 ) = RKI(  72 )  *  YC( OPEN )
      RXRAT(  75 ) = RKI(  75 )  *  YC( MGLY )
      RXRAT(  92 ) = RKI(  92 )  *  YC( ISPD )
 
 
C..Thermal reactions
!      RXRAT(   2 ) = RKI(   2 ) * YC( O )
!      RXRAT(   3 ) = RKI(   3 ) * YC( O3 )      * YC( NO )
!      RXRAT(   4 ) = RKI(   4 ) * YC( O )       * YC( NO2 )
      RXRAT(   5 ) = RKI(   5 ) * YC( O )       * YC( NO2 )
!      RXRAT(   6 ) = RKI(   6 ) * YC( O )       * YC( NO )
      RXRAT(   7 ) = RKI(   7 ) * YC( O3 )      * YC( NO2 )
!      RXRAT(  10 ) = RKI(  10 ) * YC( O1D )
!      RXRAT(  11 ) = RKI(  11 ) * YC( O1D )
!      RXRAT(  12 ) = RKI(  12 ) * YC( O1D )
!      RXRAT(  13 ) = RKI(  13 ) * YC( O3 )      * YC( OH )
!      RXRAT(  14 ) = RKI(  14 ) * YC( O3 )      * YC( HO2 )
      RXRAT(  16 ) = RKI(  16 ) * YC( NO3 )     * YC( NO )
      RXRAT(  17 ) = RKI(  17 ) * YC( NO3 )     * YC( NO2 )
!      RXRAT(  18 ) = RKI(  18 ) * YC( NO3 )     * YC( NO2 )
      RXRAT(  19 ) = RKI(  19 ) * YC( N2O5 )
      RXRAT(  20 ) = RKI(  20 ) * YC( N2O5 )
      RXRAT(  21 ) = RKI(  21 ) * YC( NO )      * YC( NO )
      RXRAT(  22 ) = RKI(  22 ) * YC( NO )      * YC( NO2 )
!      RXRAT(  23 ) = RKI(  23 ) * YC( OH )      * YC( NO )
      RXRAT(  25 ) = RKI(  25 ) * YC( HONO )    * YC( OH )
      RXRAT(  26 ) = RKI(  26 ) * YC( HONO )    * YC( HONO )
      RXRAT(  27 ) = RKI(  27 ) * YC( OH )      * YC( NO2 )
      RXRAT(  28 ) = RKI(  28 ) * YC( OH )      * YC( HNO3 )
!      RXRAT(  29 ) = RKI(  29 ) * YC( HO2 )     * YC( NO )
!      RXRAT(  30 ) = RKI(  30 ) * YC( HO2 )     * YC( NO2 )
      RXRAT(  31 ) = RKI(  31 ) * YC( PNA )
      RXRAT(  32 ) = RKI(  32 ) * YC( PNA )     * YC( OH )
      RXRAT(  33 ) = RKI(  33 ) * YC( HO2 )     * YC( HO2 )
      RXRAT(  34 ) = RKI(  34 ) * YC( HO2 )     * YC( HO2 )
      RXRAT(  36 ) = RKI(  36 ) * YC( H2O2 )    * YC( OH )
      RXRAT(  37 ) = RKI(  37 ) * YC( CO )      * YC( OH )
      RXRAT(  38 ) = RKI(  38 ) * YC( FORM )    * YC( OH )
      RXRAT(  41 ) = RKI(  41 ) * YC( FORM )    * YC( O )
      RXRAT(  42 ) = RKI(  42 ) * YC( FORM )    * YC( NO3 )
      RXRAT(  43 ) = RKI(  43 ) * YC( ALD2 )    * YC( O )
      RXRAT(  44 ) = RKI(  44 ) * YC( ALD2 )    * YC( OH )
      RXRAT(  45 ) = RKI(  45 ) * YC( ALD2 )    * YC( NO3 )
      RXRAT(  47 ) = RKI(  47 ) * YC( C2O3 )    * YC( NO )
      RXRAT(  48 ) = RKI(  48 ) * YC( C2O3 )    * YC( NO2 )
      RXRAT(  49 ) = RKI(  49 ) * YC( PAN )
      RXRAT(  50 ) = RKI(  50 ) * YC( C2O3 )    * YC( C2O3 )
      RXRAT(  51 ) = RKI(  51 ) * YC( C2O3 )    * YC( HO2 )
      RXRAT(  52 ) = RKI(  52 ) * YC( OH )
      RXRAT(  53 ) = RKI(  53 ) * YC( PAR )     * YC( OH )
      RXRAT(  54 ) = RKI(  54 ) * YC( ROR )
      RXRAT(  55 ) = RKI(  55 ) * YC( ROR )
      RXRAT(  56 ) = RKI(  56 ) * YC( ROR )     * YC( NO2 )
      RXRAT(  57 ) = RKI(  57 ) * YC( OLE )     * YC( O )
      RXRAT(  58 ) = RKI(  58 ) * YC( OLE )     * YC( OH )
      RXRAT(  59 ) = RKI(  59 ) * YC( OLE )     * YC( O3 )
      RXRAT(  60 ) = RKI(  60 ) * YC( OLE )     * YC( NO3 )
      RXRAT(  61 ) = RKI(  61 ) * YC( ETH )     * YC( O )
      RXRAT(  62 ) = RKI(  62 ) * YC( ETH )     * YC( OH )
      RXRAT(  63 ) = RKI(  63 ) * YC( ETH )     * YC( O3 )
      RXRAT(  64 ) = RKI(  64 ) * YC( TOL )     * YC( OH )
      RXRAT(  65 ) = RKI(  65 ) * YC( TO2 )     * YC( NO )
      RXRAT(  66 ) = RKI(  66 ) * YC( TO2 )
      RXRAT(  67 ) = RKI(  67 ) * YC( CRES )    * YC( OH )
      RXRAT(  68 ) = RKI(  68 ) * YC( CRES )    * YC( NO3 )
      RXRAT(  69 ) = RKI(  69 ) * YC( CRO )     * YC( NO2 )
      RXRAT(  70 ) = RKI(  70 ) * YC( XYL )     * YC( OH )
      RXRAT(  71 ) = RKI(  71 ) * YC( OPEN )    * YC( OH )
      RXRAT(  73 ) = RKI(  73 ) * YC( OPEN )    * YC( O3 )
      RXRAT(  74 ) = RKI(  74 ) * YC( MGLY )    * YC( OH )
      RXRAT(  76 ) = RKI(  76 ) * YC( ISOP )    * YC( O )
      RXRAT(  77 ) = RKI(  77 ) * YC( ISOP )    * YC( OH )
      RXRAT(  78 ) = RKI(  78 ) * YC( ISOP )    * YC( O3 )
      RXRAT(  79 ) = RKI(  79 ) * YC( ISOP )    * YC( NO3 )
      RXRAT(  80 ) = RKI(  80 ) * YC( XO2 )     * YC( NO )
      RXRAT(  81 ) = RKI(  81 ) * YC( XO2 )     * YC( XO2 )
      RXRAT(  82 ) = RKI(  82 ) * YC( XO2N )    * YC( NO )
      RXRAT(  83 ) = RKI(  83 ) * YC( SO2 )     * YC( OH )
      RXRAT(  84 ) = RKI(  84 ) * YC( SO2 )
      RXRAT(  85 ) = RKI(  85 ) * YC( XO2 )     * YC( HO2 )
      RXRAT(  86 ) = RKI(  86 ) * YC( XO2N )    * YC( HO2 )
      RXRAT(  87 ) = RKI(  87 ) * YC( XO2N )    * YC( XO2N )
      RXRAT(  88 ) = RKI(  88 ) * YC( XO2N )    * YC( XO2 )
      RXRAT(  89 ) = RKI(  89 ) * YC( ISPD )    * YC( OH )
      RXRAT(  90 ) = RKI(  90 ) * YC( ISPD )    * YC( O3 )
      RXRAT(  91 ) = RKI(  91 ) * YC( ISPD )    * YC( NO3 )
      RXRAT(  93 ) = RKI(  93 ) * YC( ISOP )    * YC( NO2 )


c..Reactions for aerosols versions

      IF( L_AE_VRSN ) THEN 

         RXRAT(  94 ) = RKI(  94 ) * YC( TERP )    * YC( OH )
         RXRAT(  95 ) = RKI(  95 ) * YC( TERP )    * YC( NO3 )
         RXRAT(  96 ) = RKI(  96 ) * YC( TERP )    * YC( O3 )

      ENDIF


      RETURN

      END
