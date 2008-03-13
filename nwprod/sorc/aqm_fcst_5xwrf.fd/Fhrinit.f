
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrinit.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%



      SUBROUTINE HRINIT
C***********************************************************************
C
C  FUNCTION: To initialize species tolerances, arrays, and indices 
C
C  PRECONDITIONS: For CB4 mechanisms only
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

C.....INCLUDES:
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_SPC.EXT"    ! Gas chem species names and MWs
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_EMIS.EXT"   ! Gas chem emissions name and mapping tables
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/RXCM.EXT"    ! Mechanism reaction common block

C.....ARGUMENTS: NONE
 
C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS:
      INTEGER  FINDEX         ! Finds location of a number in a list

C.....SAVED VARIABLES: NONE

 
C.....LOCAL VARIABLES: 
      CHARACTER*16  PNAME     ! Program name
      CHARACTER*132 MSG       ! Log message

      INTEGER IND             ! Species index
      INTEGER N               ! Loop index

C***********************************************************************
      DATA PNAME / 'HRINIT' / 


      N_SPEC = N_GC_SPC
      N_RXNS = NRXNS

      ALLOCATE( RKI( NRXNS ) )
      ALLOCATE( RXRAT( NRXNS ) )
      ALLOCATE( RTOL( N_SPEC) )
      ALLOCATE( YC(   N_SPEC) )
      ALLOCATE( YC0(  N_SPEC) )
      ALLOCATE( YCP(  N_SPEC) )
      ALLOCATE( PROD( N_SPEC) )
      ALLOCATE( LOSS( N_SPEC) )
      ALLOCATE( PNEG( N_SPEC) )

      NING1 = 4
      NING2 = 4


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices and pointers for gas-phase only version
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( .NOT. L_AQ_VRSN .AND. .NOT. L_AE_VRSN ) THEN

         NO2      =   1
         NO       =   2
         O        =   3
         O3       =   4
         NO3      =   5
         O1D      =   6
         OH       =   7
         HO2      =   8
         N2O5     =   9
         HNO3     =  10
         HONO     =  11
         PNA      =  12
         H2O2     =  13
         CO       =  14
         FORM     =  15
         ALD2     =  16
         C2O3     =  17
         XO2      =  18
         PAN      =  19
         PAR      =  20
         XO2N     =  21
         ROR      =  22
         NTR      =  23
         OLE      =  24
         ETH      =  25
         TOL      =  26
         CRES     =  27
         TO2      =  28
         OPEN     =  29
         CRO      =  30
         XYL      =  31
         MGLY     =  32
         ISOP     =  33
         ISPD     =  34
         SO2      =  35
         SULF     =  36

         N_EBISP  =  23
         ALLOCATE( EBISP( N_EBISP ) )

         EBISP(  1 ) = HNO3            
         EBISP(  2 ) = H2O2            
         EBISP(  3 ) = CO            
         EBISP(  4 ) = FORM              
         EBISP(  5 ) = ALD2             
         EBISP(  6 ) = XO2            
         EBISP(  7 ) = PAR             
         EBISP(  8 ) = XO2N             
         EBISP(  9 ) = ROR            
         EBISP( 10 ) = NTR            
         EBISP( 11 ) = OLE             
         EBISP( 12 ) = ETH            
         EBISP( 13 ) = TOL             
         EBISP( 14 ) = CRES            
         EBISP( 15 ) = TO2            
         EBISP( 16 ) = OPEN             
         EBISP( 17 ) = CRO            
         EBISP( 18 ) = XYL             
         EBISP( 19 ) = MGLY             
         EBISP( 20 ) = ISOP            
         EBISP( 21 ) = ISPD            
         EBISP( 22 ) = SO2            
         EBISP( 23 ) = SULF             

      ENDIF
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices for gas-phase plus aerosol only versions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( L_AE_VRSN .AND. .NOT. L_AQ_VRSN ) THEN

         NO2      =   1
         NO       =   2
         O        =   3
         O3       =   4
         NO3      =   5
         O1D      =   6
         OH       =   7
         HO2      =   8
         N2O5     =   9
         HNO3     =  10
         HONO     =  11
         PNA      =  12
         H2O2     =  13
         CO       =  14
         FORM     =  15
         ALD2     =  16
         C2O3     =  17
         XO2      =  18
         PAN      =  19
         PAR      =  20
         XO2N     =  21
         ROR      =  22
         NTR      =  23
         OLE      =  24
         ETH      =  25
         TOL      =  26
         CRES     =  27
         TO2      =  28
         TOLAER   =  29
         OPEN     =  30
         CRO      =  31
         CSLAER   =  32
         XYL      =  33
         MGLY     =  34
         XYLAER   =  35
         ISOP     =  36
         ISPD     =  37
         SO2      =  38
         SULF     =  39
         SULAER   =  40
         TERP     =  41
         TERPAER  =  42

         N_EBISP  =  29
         ALLOCATE( EBISP( N_EBISP ) )

         EBISP(  1 ) = HNO3            
         EBISP(  2 ) = H2O2            
         EBISP(  3 ) = CO            
         EBISP(  4 ) = FORM              
         EBISP(  5 ) = ALD2             
         EBISP(  6 ) = XO2            
         EBISP(  7 ) = PAR             
         EBISP(  8 ) = XO2N             
         EBISP(  9 ) = ROR            
         EBISP( 10 ) = NTR            
         EBISP( 11 ) = OLE             
         EBISP( 12 ) = ETH            
         EBISP( 13 ) = TOL             
         EBISP( 14 ) = CRES            
         EBISP( 15 ) = TO2            
         EBISP( 16 ) = OPEN             
         EBISP( 17 ) = CRO            
         EBISP( 18 ) = XYL             
         EBISP( 19 ) = MGLY             
         EBISP( 20 ) = ISOP            
         EBISP( 21 ) = ISPD            
         EBISP( 22 ) = SO2            
         EBISP( 23 ) = SULF             
         EBISP( 24 ) = TOLAER             
         EBISP( 25 ) = CSLAER           
         EBISP( 26 ) = XYLAER           
         EBISP( 27 ) = SULAER           
         EBISP( 28 ) = TERP           
         EBISP( 29 ) = TERPAER           

      ENDIF 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices for gas-phase plus AQ chemistry only versions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( .NOT. L_AE_VRSN .AND. L_AQ_VRSN ) THEN

         NO2      =   1
         NO       =   2
         O        =   3
         O3       =   4
         NO3      =   5
         O1D      =   6
         OH       =   7
         HO2      =   8
         N2O5     =   9
         HNO3     =  10
         HONO     =  11
         PNA      =  12
         H2O2     =  13
         CO       =  14
         FORM     =  15
         ALD2     =  16
         C2O3     =  17
         XO2      =  18
         PAN      =  19
         PACD     =  20
         PAR      =  21
         XO2N     =  22
         ROR      =  23
         NTR      =  24
         OLE      =  25
         FACD     =  26
         AACD     =  27
         ETH      =  28
         TOL      =  29
         CRES     =  30
         TO2      =  31
         OPEN     =  32
         CRO      =  33
         XYL      =  34
         MGLY     =  35
         ISOP     =  36
         ISPD     =  37
         SO2      =  38
         SULF     =  39
         UMHP     =  40

         N_EBISP  =  27
         ALLOCATE( EBISP( N_EBISP ) )

         EBISP(  1 ) = HNO3            
         EBISP(  2 ) = H2O2            
         EBISP(  3 ) = CO            
         EBISP(  4 ) = FORM              
         EBISP(  5 ) = ALD2             
         EBISP(  6 ) = XO2            
         EBISP(  7 ) = PAR             
         EBISP(  8 ) = XO2N             
         EBISP(  9 ) = ROR            
         EBISP( 10 ) = NTR            
         EBISP( 11 ) = OLE             
         EBISP( 12 ) = ETH            
         EBISP( 13 ) = TOL             
         EBISP( 14 ) = CRES            
         EBISP( 15 ) = TO2            
         EBISP( 16 ) = OPEN             
         EBISP( 17 ) = CRO            
         EBISP( 18 ) = XYL             
         EBISP( 19 ) = MGLY             
         EBISP( 20 ) = ISOP            
         EBISP( 21 ) = ISPD            
         EBISP( 22 ) = SO2            
         EBISP( 23 ) = SULF             
         EBISP( 24 ) = PACD             
         EBISP( 25 ) = FACD           
         EBISP( 26 ) = AACD           
         EBISP( 27 ) = UMHP           

      ENDIF 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices for gas-phase plus aerosol plus AQ chem versions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( L_AE_VRSN .AND. L_AQ_VRSN ) THEN

         NO2      =   1
         NO       =   2
         O        =   3
         O3       =   4
         NO3      =   5
         O1D      =   6
         OH       =   7
         HO2      =   8
         N2O5     =   9
         HNO3     =  10
         HONO     =  11
         PNA      =  12
         H2O2     =  13
         CO       =  14
         FORM     =  15
         ALD2     =  16
         C2O3     =  17
         XO2      =  18
         PAN      =  19
         PACD     =  20
         PAR      =  21
         XO2N     =  22
         ROR      =  23
         NTR      =  24
         OLE      =  25
         FACD     =  26
         AACD     =  27
         ETH      =  28
         TOL      =  29
         CRES     =  30
         TO2      =  31
         TOLAER   =  32
         OPEN     =  33
         CRO      =  34
         CSLAER   =  35
         XYL      =  36
         MGLY     =  37
         XYLAER   =  38
         ISOP     =  39
         ISPD     =  40
         SO2      =  41
         SULF     =  42
         SULAER   =  43
         UMHP     =  44
         TERP     =  45
         TERPAER  =  46

         N_EBISP  =  33
         ALLOCATE( EBISP( N_EBISP ) )

         EBISP(  1 ) = HNO3            
         EBISP(  2 ) = H2O2            
         EBISP(  3 ) = CO            
         EBISP(  4 ) = FORM              
         EBISP(  5 ) = ALD2             
         EBISP(  6 ) = XO2            
         EBISP(  7 ) = PAR             
         EBISP(  8 ) = XO2N             
         EBISP(  9 ) = ROR            
         EBISP( 10 ) = NTR            
         EBISP( 11 ) = OLE             
         EBISP( 12 ) = ETH            
         EBISP( 13 ) = TOL             
         EBISP( 14 ) = CRES            
         EBISP( 15 ) = TO2            
         EBISP( 16 ) = OPEN             
         EBISP( 17 ) = CRO            
         EBISP( 18 ) = XYL             
         EBISP( 19 ) = MGLY             
         EBISP( 20 ) = ISOP            
         EBISP( 21 ) = ISPD            
         EBISP( 22 ) = SO2            
         EBISP( 23 ) = SULF             
         EBISP( 24 ) = TOLAER             
         EBISP( 25 ) = CSLAER           
         EBISP( 26 ) = XYLAER           
         EBISP( 27 ) = SULAER           
         EBISP( 28 ) = TERP           
         EBISP( 29 ) = TERPAER           
         EBISP( 30 ) = PACD           
         EBISP( 31 ) = FACD           
         EBISP( 32 ) = AACD           
         EBISP( 33 ) = UMHP           

      ENDIF 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      RTOL( NO2      ) = 1.0D-03
      RTOL( NO       ) = 1.0D-03
      RTOL( O        ) = 1.0D+00
      RTOL( O3       ) = 1.0D-03
      RTOL( NO3      ) = 1.0D-03
      RTOL( O1D      ) = 1.0D+00
      RTOL( OH       ) = 1.0D-03
      RTOL( HO2      ) = 1.0D-03
      RTOL( N2O5     ) = 1.0D-03
      RTOL( HNO3     ) = 1.0D-03
      RTOL( HONO     ) = 1.0D-03
      RTOL( PNA      ) = 1.0D-03
      RTOL( H2O2     ) = 1.0D-03
      RTOL( CO       ) = 1.0D-03
      RTOL( FORM     ) = 1.0D-03
      RTOL( ALD2     ) = 1.0D-03
      RTOL( C2O3     ) = 1.0D-03
      RTOL( XO2      ) = 1.0D-03
      RTOL( PAN      ) = 1.0D-03
      RTOL( PAR      ) = 1.0D-03
      RTOL( XO2N     ) = 1.0D-03
      RTOL( ROR      ) = 1.0D-03
      RTOL( NTR      ) = 1.0D+00
      RTOL( OLE      ) = 1.0D-03
      RTOL( ETH      ) = 1.0D-03
      RTOL( TOL      ) = 1.0D-03
      RTOL( CRES     ) = 1.0D-03
      RTOL( TO2      ) = 1.0D-03
      RTOL( OPEN     ) = 1.0D-03
      RTOL( CRO      ) = 1.0D-03
      RTOL( XYL      ) = 1.0D-03
      RTOL( MGLY     ) = 1.0D-03
      RTOL( ISOP     ) = 1.0D-03
      RTOL( ISPD     ) = 1.0D-03
      RTOL( SO2      ) = 1.0D-03
      RTOL( SULF     ) = 1.0D-03

      IF( L_AE_VRSN ) THEN

         RTOL( SULAER   ) = 1.0D+00
         RTOL( TOLAER   ) = 1.0D+00
         RTOL( XYLAER   ) = 1.0D+00
         RTOL( CSLAER   ) = 1.0D+00
         RTOL( TERP     ) = 1.0D+00
         RTOL( TERPAER  ) = 1.0D+00

      ENDIF

      IF( L_AQ_VRSN ) THEN

         RTOL( PACD     ) = 1.0D+00
         RTOL( FACD     ) = 1.0D+00
         RTOL( AACD     ) = 1.0D+00
         RTOL( UMHP     ) = 1.0D+00

      ENDIF


      RETURN

      END
