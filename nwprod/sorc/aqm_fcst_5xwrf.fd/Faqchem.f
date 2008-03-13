
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/cloud/cloud_acm/aqchem.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE AQCHEM ( JDATE, JTIME, TEMP, PRES_PA, TAUCLD, PRCRATE,
     &                    WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &                    AEROSOL, GASWDEP, AERWDEP, HPWDEP )

C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Compute concentration changes in cloud due to aqueous chemistry,
C    scavenging and wet deposition amounts.
C
C  Revision History:
C      No   Date   Who	What
C      -- -------- ---  -----------------------------------------
C      0  / /86    CW   BEGIN PROGRAM - Walceks's Original Code
C      1  / /86    RB   INCORPORATE INTO RADM
C      2  03/23/87 DH   REFORMAT
C      3  04/11/88 SJR  STREAMLINED CODE - ADDED COMMENTS
C      4  08/27/88 SJR  COMMENTS, MODIFIED FOR RPM
C      4a 03/15/96 FSB  Scanned hard copy to develop Models3
C                       Version.
C      5  04/24/96 FSB  Made into Models3 Format
C      6  02/18/97 SJR  Revisions to link with Models3
C      7  08/12/97 SJR  Revised for new concentration units (moles/mole)
C                       and new treatment of nitrate and nitric acid
C      8  01/15/98 sjr  revised to add new aitken mode scavenging
C                       and aerosol number scavenging
C      9  12/15/98 David Wong at LM:
C             -- change division of XL, TEMP to multiplication of XL, TEMP
C                reciprocal, respectively
C             -- change / TOTOX / TSIV to / ( TOTOX * TSIV )
C     10  03/18/99 David Wong at LM:
C             -- removed "* 1.0" redundant calculation at TEMP1 calculation
C     11  04/27/00 sjr  Added aerosol surface area as modeled species
C     12  12/02    sjr  changed calls to HLCONST and updated the dissociation
C                       constants
C     13  06/26/03 sjr  revised calculations of DTW based on CMAS website
C                       discussions
C     14  08/05/03 sjr  revision made to the coarse aerosol number washout
C     15  04/20/05  us  revisions to add sea salt species in the fine and
C                       coarse aerosol modes, and HCl dissolution/dissociation
C     16  10/29/05 yoj  f90
C
C  Reference:
C     Walcek & Taylor, 1986, A theoretical Method for computing
C      vertical distributions of acidity and sulfate within cumulus
C      clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
C
C  Called by:  AQMAP
C
C  Calls the following subroutines:  none
C
C  Calls the following functions:  HLCONST
C
C  ARGUMENTS     TYPE      I/O       DESCRIPTION
C  ---------     ----  ------------  --------------------------------
C  GAS(ngas)     real  input&output  Concentration for species i=1,12
C  GASWDEP(ngas) real     output     wet deposition for species
C                                    (1) = SO2   conc (mol/mol of S02)
C                                    (2) = HNO3  conc (mol/mol of HNO3)
C                                    (3) = N2O5  conc (mol/mol of N2O5)
C                                    (4) = CO2   conc (mol/mol of CO2)
C                                    (5) = NH3   conc (mol/mol of NH3)
C                                    (6) = H2O2  conc (mol/mol of H2O2)
C                                    (7) = O3    conc (mol/mol of O3)
C                                    (8) = FOA   conc (mol/mol of FOA)
C                                    (9) = MHP   conc (mol/mol of MHP)
C                                    (10)= PAA   conc (mol/mol of PAA)
C                                    (11)= H2SO4 conc (mol/mol of H2SO4)
C                                    (12)= HCL   conc (mol/mol of HCL)
C
C  AEROSOL(naer) real input&output   Concentration for species i=1,29
C  AERWDEP(naer) real     output     wet deposition for species
C                                    (1) = SO4AKN conc (mol/mol)
C                                    (2) = SO4ACC conc (mol/mol)
C                                    (3) = SO4COR conc (mol/mol)
C                                    (4) = NH4AKN conc (mol/mol)
C                                    (5) = NH4ACC conc (mol/mol)
C                                    (6) = NO3AKN conc (mol/mol)
C                                    (7) = NO3ACC conc (mol/mol)
C                                    (8) = NO3COR conc (mol/mol)
C                                    (9) = ORGAKN conc (mol/mol)
C                                    (10)= ORGACC conc (mol/mol)
C                                    (11)= PRIAKN conc (mol/mol)
C                                    (12)= PRIACC conc (mol/mol)
C                                    (13)= PRICOR conc (mol/mol)
C                                    (14)= CACO3  conc (mol/mol)
C                                    (15)= MGCO3  conc (mol/mol)
C                                    (16)= NAAKN  conc (mol/mol)
C                                    (17)= NAACC  conc (mol/mol)
C                                    (18)= NACOR  conc (mol/mol)
C                                    (19)= CLAKN  conc (mol/mol)
C                                    (20)= CLACC  conc (mol/mol)
C                                    (21)= CLCOR  conc (mol/mol)
C                                    (22)= A3FE   conc (mol/mol)
C                                    (23)= B2MN   conc (mol/mol)
C                                    (24)= K      conc (mol/mol)
C                                    (25)= NUMAKN conc (#/mol)
C                                    (26)= NUMACC conc (#/mol)
C                                    (27)= NUMCOR conc (#/mol)
C                                    (28)= SRFAKN conc (m2/mol)
C                                    (29)= SRFACC conc (m2/mol)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/CONST.EXT"          ! constants
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/PARMS3.EXT"        ! I/O parameters definitions
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/RXCM.EXT"         ! Mechanism reaction common block
      INCLUDE 'AQ_PARAMS.EXT'      ! aqueous chemistry shared parameters

      CHARACTER( 120 ) :: XMSG = ' '  ! Exit status message

C...........PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NUMOX =  5 ! number of oxidizing reactions
      INTEGER, PARAMETER :: NLIQS = 38 ! number of liquid phase species

      REAL, PARAMETER :: H2ODENS = 1000.0 ! density of water at 20 C and 1 ATM (kg/m3)
      REAL, PARAMETER :: ONETHIRD = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0
      REAL, PARAMETER :: CONCMIN = 1.0E-30 ! minimum concentration
      REAL, PARAMETER :: SEC2HR = 1.0 / 3600.0 ! convert seconds to hours

C...........ARGUMENTS and their descriptions

      INTEGER JDATE           ! current model date, coded YYYYDDD
      INTEGER JTIME           ! current model time, coded HHMMSS

      REAL    AIRM            ! total air mass in cloudy layers (mol/m2)
      REAL    ALFA0           ! scav coef for aitken aerosol number
      REAL    ALFA2           ! scav coef for aitken aerosol sfc area
      REAL    ALFA3           ! scav coef for aitken aerosol mass
      REAL    HPWDEP          ! hydrogen wet deposition (mm mol/liter)
      REAL    PRCRATE         ! precip rate (mm/hr)
      REAL    PRES_PA         ! pressure (Pa)
      REAL    TAUCLD          ! timestep for cloud (s)
      REAL    TEMP            ! temperature (K)
      REAL    WCAVG           ! liquid water content (kg/m3)
      REAL    WTAVG           ! total water content (kg/m3)
      REAL    GAS    ( NGAS ) ! gas phase concentrations (mol/molV)
      REAL    AEROSOL( NAER ) ! aerosol concentrations (mol/molV)
      REAL    GASWDEP( NGAS ) ! gas phase wet deposition array (mm mol/liter)
      REAL    AERWDEP( NAER ) ! aerosol wet deposition array (mm mol/liter)

C...........LOCAL VARIABLES (scalars) and their descriptions:

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru
      LOGICAL, SAVE :: L_AE3_VRSN        ! flag for AE3 mechanism version

      CHARACTER( 16 ) :: PNAME = 'AQCHEM' ! program name

      INTEGER       I20C      ! loop counter for do loop 20
      INTEGER       I30C      ! loop counter for do loop 30
      INTEGER       ITERAT    ! # iterations of aqueaous chemistry solver
      INTEGER       I7777C    ! aqueous chem iteration counter
      INTEGER       ICNTAQ    ! aqueous chem iteration counter
      INTEGER       LIQ       ! loop counter for liquid species
      INTEGER       IOX       ! index over oxidation reactions

      REAL          DEPSUM
      REAL          BETASO4
      REAL          A         ! iron's anion concentration
      REAL          AC        ! H+ concentration in cloudwater (mol/liter)
      REAL          ACT1      ! activity corretion factor!single ions
      REAL          ACT2      ! activity factor correction!double ions
      REAL          ACTB      !
      REAL          AE        ! guess for H+ conc in cloudwater (mol/liter)
      REAL          B         ! manganese's anion concentration
      REAL          PRES_ATM  ! pressure (Atm)
      REAL          BB        ! lower limit guess of cloudwater pH
      REAL          CA        ! Calcium conc in cloudwater (mol/liter)
      REAL          CAA       ! inital Calcium in cloudwater (mol/liter)
      REAL          CL        ! total Cl-  conc in cloudwater (mol/liter)
      REAL          CLACC     ! fine Cl- in cloudwater (mol/liter)
      REAL          CLACCA    ! initial fine Cl in cloudwater (mol/liter)
      REAL          CLAKNA    ! initial interstitial aero Cl (mol/liter)
      REAL          CLCOR     ! coarse Cl-  conc in cloudwater (mol/liter)
      REAL          CLCORA    ! init coarse Cl-  in cloudwater (mol/liter)
      REAL          CO2H      ! Henry's Law constant for CO2
      REAL          CO21      ! First dissociation constant for CO2
      REAL          CO22      ! Second dissociation constant for CO2
      REAL          CO212     ! CO21*CO22
      REAL          CO212H    ! CO2H*CO21*CO22
      REAL          CO21H     ! CO2H*CO21
      REAL          CO2L      ! CO2 conc in cloudwater (mol/liter)
      REAL          CO3       ! CO3= conc in cloudwater (mol/liter)
      REAL          CO3A      ! initial CO3 in cloudwater (mol/liter)
      REAL          CTHK1     ! cloud thickness (m)
      REAL          DTRMV     !
      REAL          DTS6      !
      REAL          EBETASO4T ! EXP( -BETASO4 * TAUCLD )
      REAL          EALFA0T   ! EXP( -ALFA0 * TAUCLD )
      REAL          EALFA2T   ! EXP( -ALFA2 * TAUCLD )
      REAL          EALFA3T   ! EXP( -ALFA3 * TAUCLD )
      REAL          FA        ! functional value ??
      REAL          FB        ! functional value ??
      REAL          FE        ! Fe+++ conc in cloudwater (mol/liter)
      REAL          FEA       ! initial Fe in cloudwater (mol/liter)
      REAL          FNH3      ! frac weight of NH3 to total ammonia
      REAL          FNH4ACC   ! frac weight of NH4 acc to total ammonia
      REAL          FHNO3     ! frac weight of HNO3 to total NO3
      REAL          FNO3ACC   ! frac weight of NO3 acc to total NO3
      REAL          FRACLIQ   ! fraction of water in liquid form
      REAL          FOA1      ! First dissociation constant for FOA
      REAL          FOAH      ! Henry's Law constant for FOA
      REAL          FOA1H     ! FOAH*FOA1
      REAL          FOAL      ! FOA conc in cloudwater (mol/liter)
      REAL          FTST      !
      REAL          GM        !
      REAL          GM1       !
      REAL          GM1LOG    !
      REAL          GM2       ! activity correction factor
      REAL          GM2LOG    !
      REAL          HA        !
      REAL          HB        !
      REAL          H2OW      !
      REAL          H2O2H     ! Henry's Law Constant for H2O2
      REAL          H2O2L     ! H2O2 conc in cloudwater (mol/liter)
      REAL          HCLH      ! Henry's Law Constant for HCL
      REAL          HCL1      ! First dissociation constant for HCL
      REAL          HCL1H     ! HCL1*HCLH
      REAL          HCLL      ! HCl  conc in  cloudwater (mol/liter)
      REAL          HCO2      ! HCO2 conc in cloudwater (mol/liter)
      REAL          HCO3      ! HCO3 conc in cloudwater (mol/liter)
      REAL          HNO3H     ! Henry's Law Constant for HNO3
      REAL          HNO31     ! First dissociation constant for HNO3
      REAL          HNO31H    !
      REAL          HNO3L     ! HNO3 conc in cloudwater (mol/liter)
      REAL          HSO3      ! HSO3 conc in cloudwater (mol/liter)
      REAL          HSO4      ! HSO4 conc in cloudwater (mol/liter)
      REAL          HSO4ACC   ! accumulation mode HSO4 conc in cloudwater (mol/liter)
      REAL          HSO4COR   ! coarse HSO4 conc in cloudwater (mol/liter)
      REAL          HTST      !
      REAL          K         ! K conc in cloudwater (mol/liter)
      REAL          KA        ! initial K in cloudwater (mol/liter)
      REAL          LGTEMP    ! log of TEMP
      REAL          M3NEW     ! accumulation mode mass at time t
      REAL          M3OLD     ! accumulation mode mass at time 0
      REAL          MG        !
      REAL          MGA       ! inital Mg in cloudwater (mol/liter)
      REAL          MHPH      ! Henry's Law Constant for MHP
      REAL          MHPL      ! MHP conc in cloudwater (mol/liter)
      REAL          MN        ! Mn++ conc in cloudwater (mol/liter)
      REAL          MNA       ! initial Mn in cloudwater (mol/liter)
      REAL          NA        ! Na conc in cloudwater (mol/liter)
      REAL          NAACC     ! Na in cloudwater (mol/liter)
      REAL          NAACCA    ! initial Na in cloudwater (mol/liter)
      REAL          NAAKNA    ! init Aitken mode aer conc (mol/liter)
      REAL          NACOR     ! coarse Na in cloudwater (mol/liter)
      REAL          NACORA    ! init Coarse Na in cloudwater (mol/liter)
      REAL          NH31      ! First dissociation constant for NH3
      REAL          NH3H      ! Henry's Law Constant for NH3
      REAL          NH3DH20   !
      REAL          NH31HDH   !
      REAL          NH3L      ! NH3 conc in cloudwater (mol/liter)
      REAL          NH4       ! NH4+ conc in cloudwater (mol/liter)
      REAL          NH4AKNA   ! init NH4 akn conc in cloudwater (mol/liter)
      REAL          NH4ACCA   ! init NH4 acc conc in cloudwater (mol/liter)
      REAL          NITAER    ! total aerosol nitrate
      REAL          NO3       ! NO3 conc in cloudwater (mol/liter)
      REAL          NO3ACC    ! NO3 acc conc in cloudwater (mol/liter)
      REAL          NO3ACCA   ! init NO3 acc conc in cloudwater (mol/liter)
      REAL          NO3AKNA   ! init NO3 akn conc in cloudwater (mol/liter)
      REAL          NO3CORA   ! init NO3 coa conc in cloudwater (mol/liter)
      REAL          NO3COR    ! NO3 coarse conc in cloudwater (mol/liter)
      REAL          NUMCOR    ! coarse aerosol number in cloudwater (mol/liter)
      REAL          NUMCORA   ! initial coarse aerosol number in cloudwater (mol/liter)
      REAL          O3H       ! Henry's Law Constant for O3
      REAL          O3L       ! O3 conc in cloudwater (mol/liter)
      REAL          OH        ! OH conc in cloudwater (mol/liter)
      REAL          ORGN      ! ORGANIC aerosol in cloudwater (mol/liter)
      REAL          ORGACCA   ! init ORG ACC aerosol in cloudwater (mol/liter)
      REAL          ORGAKNA   ! init ORG AKN aerosol in cloudwater (mol/liter)
      REAL          PAAH      ! Henry's Law Constant for PAA
      REAL          PAAL      ! PAA conc in cloudwater (mol/liter)
      REAL          PCO20     ! total CO2 partial pressure (atm)
      REAL          PCO2F     ! gas only CO2 partial pressure (atm)
      REAL          PFOA0     ! total ORGANIC acid partial pressure (atm)
      REAL          PFOAF     ! gas only ORGANIC ACID partial press (atm)
      REAL          PH2O20    ! total H2O2 partial pressure (atm)
      REAL          PH2O2F    ! gas only H2O2 partial pressure (atm)
      REAL          PHCL0     ! total HCL partial pressure (atm)
      REAL          PHCLF     ! gas only HCL partial pressure (atm)
      REAL          PHNO30    ! total HNO3 partial pressure (atm)
      REAL          PHNO3F    ! gas only HNO3 partial pressure (atm)
      REAL          PMHP0     ! total MHP partial pressure (atm)
      REAL          PMHPF     ! gas only MHP partial pressure (atm)
      REAL          PNH30     ! total NH3 partial pressure (atm)
      REAL          PNH3F     ! gas only NH3 partial pressure (atm)
      REAL          PO30      ! total O3 partial pressure (atm)
      REAL          PO3F      ! gas only O3 partial pressure (atm)
      REAL          PPAA0     ! total PAA partial pressure (atm)
      REAL          PPAAF     ! gas only PAA partial pressure (atm)
      REAL          PRIM      ! PRIMARY acc+akn aerosol in cloudwater (mol/liter)
      REAL          PRIMCOR   ! PRIMARY coarse aerosol in cloudwater (mol/liter)
      REAL          PRIACCA   ! init PRI ACC aerosol in cloudwater (mol/liter)
      REAL          PRIAKNA   ! init PRI AKN aerosol in cloudwater (mol/liter)
      REAL          PRICORA   ! init PRI COR aerosol in cloudwater (mol/liter)
      REAL          PSO20     ! total SO2 partial pressure (atm)
      REAL          PSO2F     ! gas only SO2 partial pressure (atm)
      REAL          RATE      !
      REAL          RECIPA1   !
      REAL          RECIPA2   !
      REAL          RECIPAP1  ! one over pressure (/atm)
      REAL          RH2O2     !
      REAL          RMHP      !
      REAL          RPAA      !
      REAL          RT        ! gas const * temperature (liter atm/mol)
      REAL, SAVE :: SCVEFF = 100.0 ! Scavenging efficiency (%) currently set to 100%
      REAL    SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL         SK6        !
      REAL         SK6TS6     !
      REAL         SO21      ! First dissociation constant for SO2
      REAL         SO22      ! Second dissociation constant for SO2
      REAL         SO2H      ! Henry's Law Constant for SO2
      REAL         SO212     ! SO21*SO22
      REAL         SO212H    ! SO21*SO22*SO2H
      REAL         SO21H     ! SO21*SO2H
      REAL         SO2L      ! SO2 conc in cloudwater (mol/liter)
      REAL         SO3       ! SO3= conc in cloudwater (mol/liter)
      REAL         SO4       ! SO4= conc in cloudwater (mol/liter)
      REAL         SO4ACC    ! accumulation mode SO4= conc in cloudwater (mol/liter)
      REAL         SO4COR    ! coarse SO4= conc in cloudwater (mol/liter)
      REAL         STION     ! ionic strength
      REAL         TAC       !
      REAL         TEMP1     !
      REAL         TIMEW     ! cloud chemistry clock (sec)
      REAL         TOTOX     !
      REAL         TOTAMM    ! total ammonium
      REAL         TOTNIT    ! total nitrate (excluding coarse mode)
      REAL         TS6       ! SO4 conc in cloudwater (mol/liter)
      REAL         TS6AKNA   ! init SO4 akn conc in cloudwater (mol/liter)
      REAL         TS6ACC    ! SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6ACCA   ! init SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6COR    ! coarse SO4 conc in cloudwater   (mol/liter)
      REAL         TS6CORA   ! init SO4 coa conc in cloudwater (mol/liter)
      REAL         TSIV      !
      REAL         TST       !
      REAL         WETFAC    ! converts mol/l to mm-mol/l based on precip
      REAL         XC1       ! (/mm)
      REAL         XC2       ! (liter-atm/mol/mm)
      REAL         XL        ! conversion factor (liter-atm/mol)
      REAL         ONE_OVER_XL ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL ! PRES_ATM / XL
      REAL         XLCO2     !
      REAL         XLH2O2    !
      REAL         XLHCL     ! const in calc of HCL final partial pres
      REAL         XLHNO3    !
      REAL         XLMHP     !
      REAL         XLNH3     !
      REAL         XLO3      !
      REAL         XLPAA     !
      REAL         XLSO2     !

C...........LOCAL ARRAYS

      REAL         LIQUID( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL         WETDEP( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL         DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL         DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL         DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL         ONE_OVER_TEMP     ! 1.0 / TEMP

C...........EXTERNAL FUNCTIONS

      REAL, EXTERNAL :: HLCONST

C*********************************************************************
C     begin body of subroutine AQCHEM

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

         IF ( INDEX( MECHNAME, 'AE3' ) .NE. 0 ) THEN
           L_AE3_VRSN = .TRUE.
         ELSE
           L_AE3_VRSN = .FALSE.
         ENDIF

      END IF    ! FIRSTIME

      ONE_OVER_TEMP = 1.0 / TEMP

C...check for bad temperature, cloud air mass, or pressure

      IF ( TEMP .LE. 0.0 ) THEN
        IF ( AIRM .LE. 0.0 ) THEN
          IF ( PRES_PA .LE. 0.0 ) THEN
            XMSG = 'MET DATA ERROR'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
          END IF
        END IF
      END IF

C...compute several conversion factors

      ICNTAQ = 0
      ITERAT = 0
      RT = ( MOLVOL / STDTEMP ) * TEMP             ! R * T (liter atm / mol)
      PRES_ATM = PRES_PA /  STDATMPA               ! pressure (atm)
      CTHK1 = AIRM * RT / ( PRES_ATM * 1000.0 )    ! cloud thickness (m)
      XL   = WCAVG * RT / H2ODENS     ! conversion factor (l-atm/mol)
      ONE_OVER_XL = 1.0 / XL
      PRES_ATM_OVER_XL = PRES_ATM / XL
      TST  = 0.999
      GM   = SCVEFF / 100.0
      ACT1 = 1.0
      ACT2 = 1.0
      GM2  = 1.0
      TIMEW = 0.0
      RECIPAP1 = 1.0 / PRES_ATM
      XC1  = 1.0 / ( WCAVG * CTHK1 )
      XC2  = RT / ( 1000.0 * CTHK1 )
      FRACLIQ = WCAVG / WTAVG

C...set equilibrium constants as a function of temperature
C...   Henry's law constants

      SO2H  = HLCONST( 'SO2             ', TEMP, .FALSE., 0.0 )
      CO2H  = HLCONST( 'CO2             ', TEMP, .FALSE., 0.0 )
      NH3H  = HLCONST( 'NH3             ', TEMP, .FALSE., 0.0 )
      H2O2H = HLCONST( 'H2O2            ', TEMP, .FALSE., 0.0 )
      O3H   = HLCONST( 'O3              ', TEMP, .FALSE., 0.0 )
      HCLH  = HLCONST( 'HCL             ', TEMP, .FALSE., 0.0 )
      HNO3H = HLCONST( 'HNO3            ', TEMP, .FALSE., 0.0 )
      MHPH  = HLCONST( 'METHYLHYDROPEROX', TEMP, .FALSE., 0.0 )
      PAAH  = HLCONST( 'PEROXYACETIC_ACI', TEMP, .FALSE., 0.0 )
      FOAH  = HLCONST( 'FORMIC_ACID     ', TEMP, .FALSE., 0.0 )

      TEMP1 = ONE_OVER_TEMP - 1.0 / 298.0

C...dissociation constants

      FOA1  = 1.80E-04 * EXP( -2.00E+01 * TEMP1 )  ! Martell and Smith (1977)
      SK6   = 1.02E-02 * EXP(  2.72E+03 * TEMP1 )  ! Smith and Martell (1976)
      SO21  = 1.30E-02 * EXP(  1.96E+03 * TEMP1 )  ! Smith and Martell (1976)
      SO22  = 6.60E-08 * EXP(  1.50E+03 * TEMP1 )  ! Smith and Martell (1976)
      CO21  = 4.30E-07 * EXP( -1.00E+03 * TEMP1 )  ! Smith and Martell (1976)
      CO22  = 4.68E-11 * EXP( -1.76E+03 * TEMP1 )  ! Smith and Martell (1976)
      H2OW  = 1.00E-14 * EXP( -6.71E+03 * TEMP1 )  ! Smith and Martell (1976)
      NH31  = 1.70E-05 * EXP( -4.50E+02 * TEMP1 )  ! Smith and Martell (1976)
      HCL1  = 1.74E+06 * EXP(  6.90E+03 * TEMP1 )  ! Marsh and McElroy (1985)
      HNO31 = 1.54E+01 * EXP(  8.70E+03 * TEMP1 )  ! Schwartz (1984)

C...Kinetic oxidation rates
C...   From Chamedies (1982)

      RH2O2 = 8.0E+04 * EXP( -3650.0 * TEMP1 )

C...From Kok

      RMHP = 1.75E+07 * EXP( -3801.0 * TEMP1 )
      RPAA = 3.64E+07 * EXP( -3994.0 * TEMP1 )

C...make initializations

      DO LIQ = 1, NLIQS
        WETDEP( LIQ ) = 0.0
      END DO

      DO IOX = 0, NUMOX
        DSIVDT( IOX ) = 0.0
        DTW   ( IOX ) = 0.0
        DS4   ( IOX ) = 0.0
      END DO

C...compute the initial accumulation aerosol 3rd moment

      M3OLD = ( AEROSOL( LSO4ACC ) * SGRAERMW( LSO4ACC ) / 1.8e6
     &      +   AEROSOL( LNH4ACC ) * SGRAERMW( LNH4ACC ) / 1.8e6
     &      +   AEROSOL( LNO3ACC ) * SGRAERMW( LNO3ACC ) / 1.8e6
     &      +   AEROSOL( LORGACC ) * SGRAERMW( LORGACC ) / 2.0e6
     &      +   AEROSOL( LPRIACC ) * SGRAERMW( LPRIACC ) / 2.2e6
     &      +   AEROSOL( LNAACC  ) * SGRAERMW( LNAACC  ) / 2.2e6
     &      +   AEROSOL( LCLACC  ) * SGRAERMW( LCLACC  ) / 2.2e6 )
!    &      * 6.0 / PI    ! cancels out in division at end of subroutine

C...compute fractional weights for several species

      TOTNIT = GAS( LHNO3 ) + AEROSOL( LNO3ACC )
      IF ( TOTNIT .GT. 0.0 ) THEN
        FHNO3   = GAS( LHNO3 ) / TOTNIT
        FNO3ACC = AEROSOL( LNO3ACC ) / TOTNIT
      ELSE
        FHNO3   = 1.0
        FNO3ACC = 0.0
      END IF

      TOTAMM = GAS( LNH3 ) + AEROSOL( LNH4ACC )
      IF ( TOTAMM .GT. 0.0 ) THEN
        FNH3    = GAS( LNH3 ) / TOTAMM
        FNH4ACC = AEROSOL( LNH4ACC ) / TOTAMM
      ELSE
        FNH3    = 1.0
        FNH4ACC = 0.0
      END IF

C...initial concentration from accumulation-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the accumulation-mode
C...  aerosol mass in incorporated into the cloud droplets

      TS6ACCA = ( AEROSOL( LSO4ACC )
     &        +   GAS    ( LH2SO4  ) ) * PRES_ATM_OVER_XL
      NO3ACCA =   AEROSOL( LNO3ACC )   * PRES_ATM_OVER_XL
      NH4ACCA =   AEROSOL( LNH4ACC )   * PRES_ATM_OVER_XL
      ORGACCA =   AEROSOL( LORGACC )   * PRES_ATM_OVER_XL
      PRIACCA =   AEROSOL( LPRIACC )   * PRES_ATM_OVER_XL
      NAACCA  =   AEROSOL( LNAACC  )   * PRES_ATM_OVER_XL
      CLACCA  =   AEROSOL( LCLACC  )   * PRES_ATM_OVER_XL

C...initial concentration from coarse-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the coarse-mode
C...  aerosol mass in incorporated into the cloud droplets

      TS6CORA =   AEROSOL( LSO4COR )   * PRES_ATM_OVER_XL
      NO3CORA =   AEROSOL( LNO3COR )   * PRES_ATM_OVER_XL

      IF ( L_AE3_VRSN ) THEN
        CLCORA  = AEROSOL( LNACL   )   * PRES_ATM_OVER_XL
        NACORA  = AEROSOL( LNACL   )   * PRES_ATM_OVER_XL
      ELSE
        CLCORA  = AEROSOL( LCLCOR  )   * PRES_ATM_OVER_XL
        NACORA  = AEROSOL( LNACOR  )   * PRES_ATM_OVER_XL
      END IF

      KA      =   AEROSOL( LK      )   * PRES_ATM_OVER_XL
      CAA     =   AEROSOL( LCACO3  )   * PRES_ATM_OVER_XL
      MGA     =   AEROSOL( LMGCO3  )   * PRES_ATM_OVER_XL
      FEA     =   AEROSOL( LA3FE   )   * PRES_ATM_OVER_XL
      MNA     =   AEROSOL( LB2MN   )   * PRES_ATM_OVER_XL
      CO3A    = ( AEROSOL( LCACO3  )
     &        +   AEROSOL( LMGCO3  ) ) * PRES_ATM_OVER_XL
      PRICORA =   AEROSOL( LPRICOR )   * PRES_ATM_OVER_XL
      NUMCORA =   AEROSOL( LNUMCOR )   * PRES_ATM_OVER_XL

C...set constant factors that will be used in later multiplications (moles/atm)

      XLH2O2  = H2O2H * XL
      XLO3    = O3H   * XL
      XLMHP   = MHPH  * XL
      XLPAA   = PAAH  * XL
      XLSO2   = SO2H  * XL
      XLNH3   = NH3H  * XL
      XLHCL   = HCLH  * XL
      XLHNO3  = HNO3H * XL
      XLCO2   = CO2H  * XL

      SO212   = SO21  * SO22
      SO21H   = SO21  * SO2H
      SO212H  = SO212 * SO2H
      CO212   = CO21  * CO22
      CO21H   = CO21  * CO2H
      CO212H  = CO22  * CO21H
      NH3DH20 = NH31  / H2OW
      NH31HDH = NH3H  * NH3DH20
      FOA1H   = FOA1  * FOAH
      HCL1H   = HCL1  * HCLH
      HNO31H  = HNO31 * HNO3H

C...If kinetic calculations are made, return to this point

      I20C = 0
20    CONTINUE

      I20C = I20C + 1
      IF ( I20C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I20C'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

C...set aitken-mode aerosol loading (mol/liter)

      NO3AKNA = AEROSOL( LNO3AKN ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      NH4AKNA = AEROSOL( LNH4AKN ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      TS6AKNA = AEROSOL( LSO4AKN ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ORGAKNA = AEROSOL( LORGAKN ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      PRIAKNA = AEROSOL( LPRIAKN ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      NAAKNA  = AEROSOL( LNAAKN  ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      CLAKNA  = AEROSOL( LCLAKN  ) * PRES_ATM_OVER_XL
     &        * ( 1.0 - EXP( -ALFA3 * TIMEW ) )

C...Initial gas phase partial pressures (atm)
C...   = initial partial pressure - amount deposited partial pressure

      PSO20  = GAS( LSO2  ) * PRES_ATM
     &       + DS4( 0 ) * XL
     &       - ( WETDEP(  8 ) + WETDEP(  9 ) + WETDEP( 10 ) ) * XC2
      PNH30  = GAS( LNH3  ) * PRES_ATM
     &       + ( NH4ACCA + NH4AKNA ) * XL
     &       - ( WETDEP(  2 ) + WETDEP( 15 ) ) * XC2
      PHNO30 = ( GAS( LHNO3 ) + 2.0 * GAS( LN2O5 ) ) * PRES_ATM
     &       + ( NO3ACCA + NO3CORA + NO3AKNA ) * XL
     &       - ( WETDEP( 14 ) + WETDEP( 32 ) + WETDEP( 38 ) ) * XC2
      PHCL0  = GAS(  LHCL ) * PRES_ATM
     &       + ( CLACCA  + CLCORA  + CLAKNA  ) * XL ! new for sea salt
     &       - ( WETDEP( 16 ) + WETDEP( 26 ) + WETDEP( 37 ) ) * XC2
      PH2O20 = GAS( LH2O2 ) * PRES_ATM - WETDEP( 17 ) * XC2
      PO30   = GAS( LO3   ) * PRES_ATM - WETDEP( 18 ) * XC2
      PFOA0  = GAS( LFOA  ) * PRES_ATM
     &       - ( WETDEP( 22 ) + WETDEP( 23 ) ) * XC2
      PMHP0  = GAS( LMHP  ) * PRES_ATM - WETDEP( 24 ) * XC2
      PPAA0  = GAS( LPAA  ) * PRES_ATM - WETDEP( 25 ) * XC2
      PCO20  = GAS( LCO2  ) * PRES_ATM
     &       + CO3A * XL
     &       - ( WETDEP( 11 ) + WETDEP( 12 ) + WETDEP( 13 ) ) * XC2

C...don't allow gas concentrations to go below zero

      PSO20  = MAX( PSO20,  0.0 )
      PNH30  = MAX( PNH30,  0.0 )
      PH2O20 = MAX( PH2O20, 0.0 )
      PO30   = MAX( PO30,   0.0 )
      PFOA0  = MAX( PFOA0,  0.0 )
      PMHP0  = MAX( PMHP0,  0.0 )
      PPAA0  = MAX( PPAA0,  0.0 )
      PCO20  = MAX( PCO20,  0.0 )
      PHCL0  = MAX( PHCL0,  0.0 )
      PHNO30 = MAX( PHNO30, 0.0 )

C...Molar concentrations of soluble aerosols
C...   = Initial amount - amount deposited  (mol/liter)

      TS6COR  = MAX( TS6CORA - WETDEP( 35 ) * XC1, 0.0 )
      NO3COR  = MAX( NO3CORA - WETDEP( 38 ) * XC1, 0.0 )
      NACOR   = MAX( NACORA  - WETDEP( 36 ) * XC1, 0.0 )
      CLCOR   = MAX( CLCORA  - WETDEP( 37 ) * XC1, 0.0 )

      TS6     = TS6ACCA  + TS6AKNA + TS6COR
     &        - ( WETDEP(  6 ) + WETDEP(  7 ) ) * XC1
     &        - DS4( 0 )
      NA      = NAACCA   + NAAKNA + NACOR
     &        - WETDEP(  4 ) * XC1
      CA      = CAA      -   WETDEP(  3 )  * XC1
      MG      = MGA      -   WETDEP( 29 )  * XC1
      K       = KA       -   WETDEP( 30 )  * XC1
      FE      = FEA      -   WETDEP( 19 )  * XC1
      MN      = MNA      -   WETDEP( 20 )  * XC1
      ORGN    = ORGACCA + ORGAKNA - WETDEP( 27 )  * XC1
      PRIM    = PRIACCA + PRIAKNA - WETDEP( 28 )  * XC1
      PRIMCOR = PRICORA  -   WETDEP( 33 )  * XC1
      NUMCOR  = NUMCORA  -   WETDEP( 34 )  * XC1
      A       = 3.0 * FE
      B       = 2.0 * MN

C...don't allow aerosol concentrations to go below zero

      TS6     = MAX( TS6,     0.0 )
      NA      = MAX( NA,      0.0 )
      CA      = MAX( CA,      0.0 )
      MG      = MAX( MG,      0.0 )
      K       = MAX( K,       0.0 )
      FE      = MAX( FE,      0.0 )
      MN      = MAX( MN,      0.0 )
      ORGN    = MAX( ORGN,    0.0 )
      PRIM    = MAX( PRIM,    0.0 )
      PRIMCOR = MAX( PRIMCOR, 0.0 )
      NUMCOR  = MAX( NUMCOR,  0.0 )
      A       = MAX( A,       0.0 )
      B       = MAX( B,       0.0 )

      SK6TS6 = SK6 * TS6

C...find solution of the equation using a method of reiterative
C...  bisections Make initial guesses for pH:   between .01  to  10.

      HA =  0.01
      HB = 10.0

      I7777C = 0
7777  CONTINUE

      I7777C = I7777C + 1
      IF ( I7777C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I7777C'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      HA = MAX( HA - 0.8, 0.1 )
      HB = MIN( HB + 0.8, 9.9 )
      AE = 10.0**( -HA )

      RECIPA1 = 1.0 / ( AE * ACT1 )
      RECIPA2 = 1.0 / ( AE * AE * ACT2 )

C...calculate final gas phase partial pressure of SO2, NH3, HNO3
C...  HCOOH, and CO2 (atm)

      PSO2F = PSO20 / ( 1.0 + XLSO2 * ( 1.0 + SO21 * RECIPA1
     &      + SO212 * RECIPA2 ) )

      PNH3F = PNH30 / ( 1.0 + XLNH3 * ( 1.0 + NH3DH20 * AE ) )

      PHCLF   = PHCL0  / ( 1.0 + XLHCL *  ( 1.0 + HCL1 * RECIPA1 ) )

      PFOAF = PFOA0 / ( 1.0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

      PHNO3F = PHNO30 / ( 1.0 + XLHNO3 * ( 1.0 + HNO31 * RECIPA1 ) )

      PCO2F = PCO20 / ( 1.0 + XLCO2 * ( 1.0 + CO21 * RECIPA1
     &      + CO212 * RECIPA2 ) )

C...calculate liquid phase concentrations (moles/liter)

      SO4  = SK6TS6 / ( AE * GM2 + SK6 )
      HSO4 = TS6 - SO4
      SO3  = SO212H  * PSO2F  * RECIPA2
      HSO3 = SO21H   * PSO2F  * RECIPA1
      CO3  = CO212H  * PCO2F  * RECIPA2
      HCO3 = CO21H   * PCO2F  * RECIPA1
      OH   = H2OW    * RECIPA1
      NH4  = NH31HDH * PNH3F  * AE
      HCO2 = FOA1H   * PFOAF  * RECIPA1
      NO3  = HNO31H  * PHNO3F * RECIPA1
      CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

C...compute functional value

      FA = AE + NH4 + NA + 2.0 * ( CA + MG - CO3 - SO3 - SO4 )
     &   - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

C...Start iteration and bisection ****************<<<<<<<

      I30C = 0
30    CONTINUE

      I30C = I30C + 1
      IF ( I30C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I30C'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      BB = ( HA + HB ) / 2.0
      AE = 10.0**( -BB )

      ICNTAQ = ICNTAQ + 1
      IF ( ICNTAQ .GE. 3000 ) THEN
        XMSG = 'Maximum AQCHEM total iterations exceeded'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      RECIPA1 = 1.0 / ( AE * ACT1 )
      RECIPA2 = 1.0 / ( AE * AE * ACT2 )

C...calculate final gas phase partial pressure of SO2, NH3, HCL, HNO3
C...  HCOOH, and CO2 (atm)

      PSO2F = PSO20 / ( 1.0 + XLSO2
     &	    * ( 1.0 + SO21 * RECIPA1 + SO212 * RECIPA2 ) )

      PNH3F = PNH30 / ( 1.0 + XLNH3 * ( 1.0 + NH3DH20 * AE ) )

      PHCLF = PHCL0  / ( 1.0 + XLHCL *  ( 1.0 + HCL1 * RECIPA1 ) )

      PHNO3F = PHNO30 / ( 1.0 + XLHNO3 * ( 1.0 + HNO31 * RECIPA1 ) )

      PFOAF = PFOA0 / ( 1.0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

      PCO2F = PCO20 / ( 1.0 + XLCO2 * ( 1.0 + CO21 * RECIPA1
     &      + CO212 * RECIPA2 ) )

C...calculate liquid phase concentrations (moles/liter)

      SO4  = SK6TS6 / ( AE * GM2 + SK6 )
      HSO4 = TS6 - SO4
      SO3  = SO212H  * PSO2F  * RECIPA2
      HSO3 = SO21H   * PSO2F  * RECIPA1
      CO3  = CO212H  * PCO2F  * RECIPA2
      HCO3 = CO21H   * PCO2F  * RECIPA1
      OH   = H2OW    * RECIPA1
      NH4  = NH31HDH * PNH3F  * AE
      HCO2 = FOA1H   * PFOAF  * RECIPA1
      NO3  = HNO31H  * PHNO3F * RECIPA1
      CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

C...compute functional value

      FB = AE + NH4 + NA + 2.0 * ( CA + MG - CO3 - SO3 - SO4 )
     &     - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

C...Calculate and check the sign of the product of the two functional values

      FTST = FA * FB
      IF ( FTST .LE. 0.0 ) THEN
        HB = BB
      ELSE
        HA = BB
        FA = FB
      END IF

C...Check convergence of solutions

      HTST = HA / HB
      IF ( HTST .LE. TST ) GO TO 30

C...end of zero-finding routine ****************<<<<<<<<<<<<

C...compute Ionic strength and activity coefficient by the Davies equation

      STION = 0.5 * (AE + NH4 + OH + HCO3 + HSO3
     &      + 4.0 * (SO4 + CO3 + SO3 + CA + MG + MN)
     &      + NO3 + HSO4 + 9.0 * FE + NA + K + CL + A + B + HCO2)
      GM1LOG = -0.509 * ( SQRT( STION )
     &       / ( 1.0 + SQRT( STION ) ) - 0.2 * STION )
      GM2LOG = GM1LOG * 4.0
      GM1  = 10.0**GM1LOG
      GM2  = MAX( 10.0**GM2LOG, 1.0E-30 )
      ACTB = ACT1
      ACT1 = MAX( GM1 * GM1, 1.0E-30 )
      ACT2 = MAX( GM1 * GM1 * GM2, 1.0E-30 )

C...check for convergence and possibly go to 7777, to recompute
C...  Gas and liquid phase concentrations

      TAC = ABS( ACTB - ACT1 ) / ACTB
      IF ( TAC .GE. 1.0E-2 ) GO TO 7777

C...return an error if the pH is not in range

!     IF ( ( HA .LT. 0.02 ) .OR. ( HA .GT. 9.49 ) ) THEN
      IF ( ( HA .LT. 0.1 ) .OR. ( HA .GT. 9.9 ) ) THEN
        print *, ha
        XMSG = 'PH VALUE OUT OF RANGE'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

C...Make those concentration calculations which can be made outside
C...  of the function.

      SO2L = SO2H * PSO2F
      AC = 10.0**( -BB )
      SIV = SO3 + HSO3 + SO2L

C...Calculate final gas phase concentrations of oxidants (atm)

      PH2O2F = ( PH2O20 + XL * DS4( 1 ) ) / ( 1.0 + XLH2O2 )
      PO3F   = ( PO30   + XL * DS4( 2 ) ) / ( 1.0 + XLO3   )
      PMHPF  = ( PMHP0  + XL * DS4( 4 ) ) / ( 1.0 + XLMHP  )
      PPAAF  = ( PPAA0  + XL * DS4( 5 ) ) / ( 1.0 + XLPAA  )

      PH2O2F = MAX( PH2O2F, 0.0 )
      PO3F   = MAX( PO3F,   0.0 )
      PMHPF  = MAX( PMHPF,  0.0 )
      PPAAF  = MAX( PPAAF,  0.0 )

C...Calculate liquid phase concentrations of oxidants (moles/liter)

      H2O2L = PH2O2F * H2O2H
      O3L   = PO3F   * O3H
      MHPL  = PMHPF  * MHPH
      PAAL  = PPAAF  * PAAH
      FOAL  = PFOAF  * FOAH
      NH3L  = PNH3F  * NH3H
      CO2L  = PCO2F  * CO2H
      HCLL  = PHCLF  * HCLH
      HNO3L = PHNO3F * HNO3H

C...compute modal concentrations

      SO4COR  = SK6 * TS6COR / ( AE * GM2 + SK6 )
      HSO4COR = MAX( TS6COR - SO4COR, 0.0 )

      TS6ACC  = MAX( TS6  - TS6COR,   0.0 )
      SO4ACC  = MAX( SO4  - SO4COR,   0.0 )
      HSO4ACC = MAX( HSO4 - HSO4COR,  0.0 )
      NO3ACC  = MAX( NO3  - NO3COR,   0.0 )
      NAACC   = MAX( NA   - NACOR,    0.0 )
      CLACC   = MAX( CL   - CLCOR,    0.0 )

C...load the liquid concentration array with current values

      LIQUID(  1 ) = AC
      LIQUID(  2 ) = NH4
      LIQUID(  3 ) = CA
      LIQUID(  4 ) = NAACC
      LIQUID(  5 ) = OH
      LIQUID(  6 ) = SO4ACC
      LIQUID(  7 ) = HSO4ACC
      LIQUID(  8 ) = SO3
      LIQUID(  9 ) = HSO3
      LIQUID( 10 ) = SO2L
      LIQUID( 11 ) = CO3
      LIQUID( 12 ) = HCO3
      LIQUID( 13 ) = CO2L
      LIQUID( 14 ) = NO3ACC
      LIQUID( 15 ) = NH3L
      LIQUID( 16 ) = CLACC
      LIQUID( 17 ) = H2O2L
      LIQUID( 18 ) = O3L
      LIQUID( 19 ) = FE
      LIQUID( 20 ) = MN
      LIQUID( 21 ) = A
      LIQUID( 22 ) = FOAL
      LIQUID( 23 ) = HCO2
      LIQUID( 24 ) = MHPL
      LIQUID( 25 ) = PAAL
      LIQUID( 26 ) = HCLL
      LIQUID( 27 ) = ORGN
      LIQUID( 28 ) = PRIM
      LIQUID( 29 ) = MG
      LIQUID( 30 ) = K
      LIQUID( 31 ) = B
      LIQUID( 32 ) = HNO3L
      LIQUID( 33 ) = PRIMCOR
      LIQUID( 34 ) = NUMCOR
      LIQUID( 35 ) = TS6COR
      LIQUID( 36 ) = NACOR
      LIQUID( 37 ) = CLCOR
      LIQUID( 38 ) = NO3COR

C...if the maximum cloud lifetime has not been reached, the compute
C...  the next timestep.

      IF ( TIMEW .LT. TAUCLD ) THEN

C...make kinetics calculations
C...  note: DS4(i) and DSIV(I) are negative numbers!

        DTRMV = 300.0
        IF ( ( CTHK1 .GT. 1.0E-10 ) .AND. ( PRCRATE .GT. 1.0E-10 ) )
     &     DTRMV = 3.6 * WTAVG * 1000.0 * CTHK1 / PRCRATE  ! <<<uma found bug, was .36
        DTRMV = MIN( DTRMV, 300.0 )
        ITERAT = ITERAT + 1

C...Define the total S(iv) available for oxidation

        TSIV = PSO20 * ONE_OVER_XL

C...Calculate sulfur iv oxidation rate due to H2O2

        DSIVDT( 1 ) = -RH2O2 * H2O2L * SO2L / ( 0.1 + AC )
        TOTOX = PH2O20 * ONE_OVER_XL
        IF ( ( DSIVDT( 1 ) .EQ. 0.0 ) .OR.
     &       ( TSIV  .LE. CONCMIN ) .OR.
     &       ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 1 ) = DTRMV
        ELSE
          DTW( 1 ) = -0.05 * MIN( TOTOX, TSIV ) / DSIVDT( 1 )
        END IF

C...Calculate sulfur iv oxidation rate due to O3

        IF ( BB .GE. 2.7 ) THEN
          DSIVDT( 2 ) = -4.19E5 * ( 1.0 + 2.39E-4 / AC ) * O3L * SIV
        ELSE
          DSIVDT( 2 ) = -1.9E4 * SIV * O3L / SQRT( AC )
        END IF
        TOTOX = PO30 * ONE_OVER_XL
        IF ( ( DSIVDT( 2 ) .EQ. 0.0 ) .OR.
     &       ( TSIV  .LE. CONCMIN ) .OR.
     &       ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 2 ) = DTRMV
        ELSE
          DTW( 2 ) = -0.01 * MIN( TOTOX, TSIV ) / DSIVDT( 2 )
        END IF

C...Calculate sulfur iv oxidation rate due to 02 catalyzed by Mn++
C...  and Fe+++  See Table IV Walcek & Taylor ( 1986)

        IF ( BB .GE. 4.0 )  THEN  ! 4.0  < pH

          IF ( SIV .LE. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -5000.0 * MN * HSO3
          ELSE IF ( SIV .GT. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC
     &                  + 1.0E7 * FE * SIV * SIV )
          END IF  ! end of first pass through SIV conc.

        ELSE          ! pH , + 4.0

	  IF ( SIV .LE. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -3.0 * ( 5000.0 * MN * HSO3
     &                  + 0.82 * FE * SIV / AC )
          ELSE
            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC
     &                  + ( 0.82 * FE * SIV / AC )
     &                  * ( 1.0 + 1.7E3 * MN**1.5 / ( 6.3E-6 + FE ) ) )
          END IF ! end of second pass through SIV conc.

        END IF  ! end of pass through pH

        IF ( ( DSIVDT( 3 ) .EQ. 0.0 ) .OR. ( TSIV .LE. CONCMIN ) ) THEN
          DTW( 3 ) = DTRMV
        ELSE
          DTW( 3 ) = -0.1 * TSIV / DSIVDT( 3 )
        END IF

C...Calculate sulfur oxidation rate due to MHP

        DSIVDT( 4 ) = -RMHP * AC * MHPL * HSO3
        TOTOX = PMHP0 * ONE_OVER_XL
        IF ( ( DSIVDT( 4 ) .EQ. 0.0 ) .OR.
     &       ( TSIV  .LE. CONCMIN ) .OR.
     &       ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 4 ) = DTRMV
        ELSE
          DTW( 4 ) = -0.1 * MIN( TOTOX, TSIV ) / DSIVDT( 4 )
        END IF

C...Calculate sulfur oxidation due to PAA

        DSIVDT( 5 ) = -RPAA * HSO3 * PAAL * ( AC + 1.65E-5 )
        TOTOX = PPAA0 * ONE_OVER_XL
        IF ( ( DSIVDT( 5 ) .EQ. 0.0 ) .OR.
     &       ( TSIV  .LE. CONCMIN ) .OR.
     &       ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 5 ) = DTRMV
        ELSE
          DTW( 5 ) = -0.1 * MIN( TOTOX, TSIV ) / DSIVDT( 5 )
        END IF

C...Calculate total sulfur iv oxidation rate

        DSIVDT( 0 ) = 0.0
        DO IOX = 1, NUMOX
          DSIVDT( 0 ) = DSIVDT( 0 ) + DSIVDT( IOX )
        END DO

C...Calculate a minimum time step required

        DTW( 0 ) = MIN( DTW( 1 ), DTW( 2 ), DTW( 3 ),
     &                  DTW( 4 ), DTW( 5 ) )

C...check for large time step

        IF ( DTW( 0 ) .GT. 8.0E+37 ) THEN
          WRITE(6,1001) PRCRATE, DSIVDT(0), TS6, DTW(0), CTHK1, WTAVG
        ELSE

C...calculate the change in sulfur iv for this time step

60        CONTINUE
          DTS6 = ABS( DTW( 0 ) * ( -DSIVDT( 0 ) - TS6 * PRCRATE
     &         / ( 3600.0 * CTHK1 * WTAVG ) ) )

C...If DSIV(0), sulfur iv oxidized during this time step would be
C... less than 5% of sulfur oxidized since time 0, then double DT

          IF ( DTW( 0 ) .LE. TAUCLD ) THEN
            IF ( DTS6 .LT. 0.05 * TS6 ) THEN
              DTW( 0 ) = DTW( 0 ) * 2.0
	      GO TO 60
            END IF
          END IF
        END IF
        DTW( 0 ) = MIN( DTW( 0 ), DTRMV )

C...If the total time after this time increment will be greater than
C...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        IF ( TIMEW + DTW( 0 ) .GT. TAUCLD ) DTW( 0 ) = TAUCLD - TIMEW
        IF ( TS6 .LT. 1.0E-11 ) DTW( 0 ) = TAUCLD - TIMEW
        IF ( ITERAT .GT. 100 ) DTW( 0 ) = TAUCLD - TIMEW

C...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each
C... individual oxidizing agent, as well as the total.

        DO IOX = 0, NUMOX
          DS4( IOX ) = DS4( IOX ) + DTW( 0 ) * DSIVDT( IOX )
        END DO

C...Compute depositions and concentrations for each species

        WETFAC = PRCRATE * FRACLIQ * DTW( 0 ) * SEC2HR
        DO LIQ = 1, NLIQS
          WETDEP( LIQ ) = WETDEP( LIQ ) + LIQUID( LIQ ) * WETFAC
        END DO

        TIMEW = TIMEW + DTW( 0 )

C...Return to make additional calculations

        GO TO 20
      END IF

C...At this point, TIMEW=TAUCLD
C...  compute the scavenging coefficient for SO4 which will be used for
C...  scavenging aerosol number in the accumulation mode

      DEPSUM = ( WETDEP( 6 ) + WETDEP( 7 ) ) * XC1

      IF ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) .NE. 0.0 ) THEN
        BETASO4 = DEPSUM / ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) * TAUCLD )
      ELSE
        BETASO4 = 0.0
      END IF

      EBETASO4T = EXP( -BETASO4 * TAUCLD )
      EALFA0T   = EXP( -ALFA0 * TAUCLD )
      EALFA2T   = EXP( -ALFA2 * TAUCLD )
      EALFA3T   = EXP( -ALFA3 * TAUCLD )

C...Compute the output concentrations and wet deposition amounts

      TOTAMM = ( PNH3F  + ( NH4 + NH3L  ) * XL ) * RECIPAP1
      TOTNIT = ( PHNO3F + ( NO3ACC + HNO3L ) * XL ) * RECIPAP1

C...gas-phase species wet deposition (mm mol/lit)

      GASWDEP( LSO2   ) = WETDEP(  8 ) + WETDEP(  9 ) + WETDEP( 10 )
      GASWDEP( LNH3   ) = WETDEP( 15 )
      GASWDEP( LH2O2  ) = WETDEP( 17 )
      GASWDEP( LO3    ) = WETDEP( 18 )
      GASWDEP( LCO2   ) = WETDEP( 11 ) + WETDEP( 12 ) + WETDEP( 13 )
      GASWDEP( LFOA   ) = WETDEP( 22 ) + WETDEP( 23 )
      GASWDEP( LMHP   ) = WETDEP( 24 )
      GASWDEP( LPAA   ) = WETDEP( 25 )
      GASWDEP( LHCL   ) = WETDEP( 26 )
      GASWDEP( LHNO3  ) = WETDEP( 32 )
      GASWDEP( LN2O5  ) = 0.0
      GASWDEP( LH2SO4 ) = 0.0

C...gas concentrations (mol/molV)

      GAS( LSO2   ) = ( PSO2F   + XL *  SIV )   * RECIPAP1
      GAS( LH2O2  ) = ( PH2O2F  + XL *  H2O2L ) * RECIPAP1
      GAS( LO3    ) = ( PO3F    + XL *  O3L )   * RECIPAP1
      GAS( LCO2   ) = ( PCO2F   + XL *  CO2L )  * RECIPAP1
      GAS( LFOA   ) = ( PFOAF   + XL * ( FOAL
     &              +  HCO2 ) ) * RECIPAP1
      GAS( LMHP   ) = ( PMHPF   + XL *  MHPL )  * RECIPAP1
      GAS( LPAA   ) = ( PPAAF   + XL *  PAAL )  * RECIPAP1
      GAS( LHCL   ) = ( PHCLF   + XL *  HCLL )  * RECIPAP1

      GAS( LNH3   ) = FNH3  * TOTAMM
      GAS( LHNO3  ) = FHNO3 * TOTNIT
      GAS( LN2O5  ) = 0.0 ! assume all into aerosol
      GAS( LH2SO4 ) = 0.0 ! assume all into aerosol

C...aerosol species wet deposition (mm mol/lit)
C...  there is no wet deposition of aitken particles, they attached
C...  to the accumulation mode particles

      AERWDEP( LSO4AKN ) = 0.0
      AERWDEP( LNH4AKN ) = 0.0
      AERWDEP( LNO3AKN ) = 0.0
      AERWDEP( LORGAKN ) = 0.0
      AERWDEP( LPRIAKN ) = 0.0
      AERWDEP( LNAAKN  ) = 0.0
      AERWDEP( LCLAKN  ) = 0.0

      AERWDEP( LSO4ACC ) = WETDEP(  6 ) + WETDEP(  7 )
      AERWDEP( LNH4ACC ) = WETDEP(  2 )
      AERWDEP( LNO3ACC ) = WETDEP( 14 )
      AERWDEP( LORGACC ) = WETDEP( 27 )
      AERWDEP( LPRIACC ) = WETDEP( 28 )
      AERWDEP( LNAACC  ) = WETDEP(  4 )
      AERWDEP( LCLACC  ) = WETDEP( 16 )

      AERWDEP( LSO4COR ) = WETDEP( 35 )
      AERWDEP( LNO3COR ) = WETDEP( 38 )
      AERWDEP( LPRICOR ) = WETDEP( 33 )

      IF ( L_AE3_VRSN ) THEN
        AERWDEP( LNACL   ) = WETDEP( 36 )
      ELSE
        AERWDEP( LNACOR  ) = WETDEP( 36 )
        AERWDEP( LCLCOR  ) = WETDEP( 37 )
      END IF

      AERWDEP( LK      ) = WETDEP( 30 )
      AERWDEP( LA3FE   ) = WETDEP( 19 )
      AERWDEP( LB2MN   ) = WETDEP( 20 )
      AERWDEP( LCACO3  ) = WETDEP(  3 )
      AERWDEP( LMGCO3  ) = WETDEP( 29 )

      AERWDEP( LNUMAKN ) = 0.0
      AERWDEP( LNUMACC ) = 0.0
      AERWDEP( LNUMCOR ) = 0.0
      AERWDEP( LSRFAKN ) = 0.0
      AERWDEP( LSRFACC ) = 0.0

C...aerosol concentrations (mol/molV)

      AEROSOL( LSO4AKN ) = AEROSOL( LSO4AKN ) * EALFA3T
      AEROSOL( LNH4AKN ) = AEROSOL( LNH4AKN ) * EALFA3T
      AEROSOL( LNO3AKN ) = AEROSOL( LNO3AKN ) * EALFA3T
      AEROSOL( LORGAKN ) = AEROSOL( LORGAKN ) * EALFA3T
      AEROSOL( LPRIAKN ) = AEROSOL( LPRIAKN ) * EALFA3T
      AEROSOL( LNAAKN  ) = AEROSOL( LNAAKN  ) * EALFA3T
      AEROSOL( LCLAKN  ) = AEROSOL( LCLAKN  ) * EALFA3T

      AEROSOL( LSO4ACC ) = TS6ACC * XL * RECIPAP1
      AEROSOL( LORGACC ) = ORGN   * XL * RECIPAP1
      AEROSOL( LPRIACC ) = PRIM   * XL * RECIPAP1
      AEROSOL( LNAACC  ) = NAACC  * XL * RECIPAP1
      AEROSOL( LCLACC  ) = CLACC  * XL * RECIPAP1

      AEROSOL( LNH4ACC ) = FNH4ACC * TOTAMM
      AEROSOL( LNO3ACC ) = FNO3ACC * TOTNIT

      AEROSOL( LSO4COR ) = TS6COR * XL * RECIPAP1
      AEROSOL( LNO3COR ) = NO3COR * XL * RECIPAP1
      AEROSOL( LPRICOR ) = PRIMCOR* XL * RECIPAP1
      AEROSOL( LK      ) = K      * XL * RECIPAP1
      AEROSOL( LA3FE   ) = FE     * XL * RECIPAP1
      AEROSOL( LB2MN   ) = MN     * XL * RECIPAP1
      AEROSOL( LCACO3  ) = CA     * XL * RECIPAP1
      AEROSOL( LMGCO3  ) = MG     * XL * RECIPAP1

      IF ( L_AE3_VRSN ) THEN
        AEROSOL( LNACL   ) = NACOR  * XL * RECIPAP1
      ELSE
        AEROSOL( LNACOR  ) = NACOR  * XL * RECIPAP1
        AEROSOL( LCLCOR  ) = CLCOR  * XL * RECIPAP1
      END IF

      AEROSOL( LNUMAKN ) = AEROSOL( LNUMAKN ) * EALFA0T
      AEROSOL( LNUMACC ) = AEROSOL( LNUMACC ) * EBETASO4T
      AEROSOL( LNUMCOR ) = NUMCOR * XL * RECIPAP1

C...compute the final accumulation aerosol 3rd moment

      M3NEW = ( AEROSOL( LSO4ACC ) * SGRAERMW( LSO4ACC ) / 1.8e6
     &      +   AEROSOL( LNH4ACC ) * SGRAERMW( LNH4ACC ) / 1.8e6
     &      +   AEROSOL( LNO3ACC ) * SGRAERMW( LNO3ACC ) / 1.8e6
     &      +   AEROSOL( LORGACC ) * SGRAERMW( LORGACC ) / 2.0e6
     &      +   AEROSOL( LPRIACC ) * SGRAERMW( LPRIACC ) / 2.2e6
     &      +   AEROSOL( LNAACC  ) * SGRAERMW( LNAACC  ) / 2.2e6
     &      +   AEROSOL( LCLACC  ) * SGRAERMW( LCLACC  ) / 2.2e6 )
!    &      * 6.0 / PI      ! cancels out in division below

      AEROSOL( LSRFAKN ) = AEROSOL( LSRFAKN ) * EALFA2T
      AEROSOL( LSRFACC ) = AEROSOL( LSRFACC )
     &                   * ( EXP( -BETASO4 * TAUCLD * ONETHIRD ) )
     &                   * ( M3NEW / MAX( M3OLD, CONCMIN) ) ** TWOTHIRDS

C...store the amount of hydrogen deposition

      HPWDEP = WETDEP( 1 )

      RETURN

C...formats

1001  FORMAT( 1X,'STORM RATE=', F6.3, 'DSIVDT(0) =', F10.5,
     &       'TS6=', F10.5, 'DTW(0)=', F10.5, 'CTHK1=', F10.5,
     &       'WTAVG=', F10.5 )

      END
