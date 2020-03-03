
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/hetchem.f,v 1.7 2012/01/19 13:13:59 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HETCHEM( GAMMA, DT )

c Calculates the heterogeneous conversion of N2O5 to HNO3 by following
c the Model Description section by Pleim et al. (1995).

c Key Subroutines Called: none
c
c Key Functions Called: N2O5PROB
c
c Revision History:
c    First version was coded in November 2007 by Dr. Prakash Bhave
c    using excerpts of the EQL3 subroutine in CMAQv4.6.  Entries
c    from the EQL3 revision history which are specific to the
c    heterogeneous chemistry calculations have been copied here, to
c    preserve that history in the event that the EQL3 subroutine is
c    later removed from the aerosol code.
c
c FSB 10/23/01 Revised to include the heterogeneous reaction of N2O5
c              with aerosol to produce HNO3, based on Pleim et al
c              (1995).  AIRPRES & DT added to call vector. These
c              modifications assume that GETPAR has been called prior
c              to calling EQL3.  It is also assumed that AH2OI and AH2OJ
c              have been added to the transport routines.
c
c GLG 08/15/02 Revised to use radius instead of diameter in calculation
c              of N2O5->HNO3 rate constant
c
c GLG 03/10/03 Revised to use composition-dependent gamma from Riemer
c              et al. (2003)
c
c SJR 03/14/03 Revised to use the effective diameter in the calculation
c              of N2O5->HNO3 rate constant instead of the geometric
c              mean diameter
c
c SJR 04/15/03 Corrected units in the HNO3 yield from the heterogeneous
c              N2O5 Rxn of Riemer et al. (2003)
c
c GS  04/04/06 Revised to use T,RH-dependent gamma from Evans & Jacob
c              (2005).  Retained ratio of GAMMA1/GAMMA2 from Riemer et
c              al. (2003).
c
c PVB 04/06/06 Added GAMMA to the call vector, so it can be written
c              to the aerosol diagnostic file.
c
c JOY 04/04/07 Optimized GAMMA calculation; initialized GAMMA in case
c              RH < 1%; Note: some compilers recognize GAMMA as an
c              intrinsic GAMMA function.
c
c PVB 11/06/07 Copied excerpts of the EQL3 subroutine to create this
c              new subroutine.  Moved all calculations of GAMMA to a
c              function subprogram, N2O5PROB.  Revised to use GAMMA
c              parameterization of Davis et al (2008), by default.
c
c GS  03/31/08 Added a heterogeneous reaction producing HONO
c
c JTK 04/17/08 Moved molar mass and conversion factors to AERO_INFO.f
c
c SH  12/08/09 Use new Fortran modules (aero_data, precursor_data,
c              met_data) in lieu of CBLK array and AERO_INFO module
c
c SH  03/10/11 Renamed met_data to aeromet_data
c
c  REFERENCES:
c   1. Pleim, J.E., F.S. Binkowski, J.K.S. Ching, R.L. Dennis, and N.V.
c      Gallani, An improved representation of the reaction of N2O5 on
c      aerosols for mesoscale air quality models.  In "Regional
c      Photochemical Measurement and Modeling Studies, Vol 2 - Results
c      and Status of Modeling," Eds A.J. Ranzieri and P.A. Solomon, pp
c      904-913, 1995.
c
c   2. Davis, J.M., P.V. Bhave, and K.M. Foley, Parameterization of N2O5
c      reaction probabilities on the surface of particles containing
c      ammonium, sulfate, and nitrate.  Atmos. Chem. Phys., 2008, in
c      press.
c
c   3. Vogel, B., H. Vogel, J. Kleffman, and R. Kurtenbach, Measured and
c      simulated vertical profiles of nitrous acid - Part II. Model
c      simulations and indications for a photolytic source, Atmospheric
c      Environment, 37, 2957-2966, 2003.
c
c   4. Sarwar, G., S.J. Roselle, R. Mathur, W. Appel, R.L. Dennis, and
c      B. Vogel, A comparison of CMAQ HONO predictions with observations
c      from the Northeast Oxidant and Particle Study, Atmospheric
c      Environment, 2008, in press.
C-----------------------------------------------------------------------

      USE AERO_DATA
      USE PRECURSOR_DATA
      USE AEROMET_DATA   ! Includes CONST.EXT

      IMPLICIT NONE

C *** Arguments
      REAL,    INTENT( OUT ) :: GAMMA     ! N2O5->NO3 rxn probability
      REAL,    INTENT( IN )  :: DT        ! Synchronization time step

C *** Parameters
      REAL, PARAMETER :: STD_DIFF_N2O5 = 0.1E-4  ! molecular diffusivity
                                                 ! of N2O5 at 101325 Pa
                                                 ! and 273.15 K [m2/sec]
      REAL, PARAMETER :: GPKG = 1.0E+03          ! g/kg unit conversion

C *** Local Variables

C *** chemical species concentrations
      REAL      GHNO3    ! gas-phase nitric acid [ug/m3]
      REAL      GN2O5    ! gas-phase dinitrogen pentoxide [ug/m3]
      REAL      GNO2     ! gas-phase NO2 [ug/m3]
      REAL      GHONO    ! gas-phase HONO [ug/m3]

C *** 2nd and 3rd moments before equilibration (without H2O)
      REAL      OLD_M3_I, OLD_M3_J
      REAL      OLD_M2_I, OLD_M2_J

C *** variables for N2O5 + H2O -> 2 HNO3 conversion
      REAL      WET_M3_I, WET_M3_J   ! M3 before equilibration w.H2O
      REAL      WET_M2_I, WET_M2_J   ! M2 before equilibration w.H2O
      REAL      DG_AT_WET, DG_AC_WET ! Initial median diameter w.H2O
      REAL      DE_AT_WET, DE_AC_WET ! Initial effective diameter w.H2O
      REAL      XXF_AT, XXF_AC       ! modal factors to calculate KN2O5
      REAL      CBAR       ! molecular velocity (m/s)
      REAL      DIFF_N2O5  ! ambient molecular diffusivity [m2/s]
      REAL      N2O5PROB   ! function to compute GAMMA
      REAL      KN2O5      ! pseudo-first order rate constant
      REAL      EXPDT_N2O5 ! fraction of N2O5 left after chemical rxn

C *** saved variables for conversion
      REAL, SAVE ::  FAERNO2    ! converts ug -> mol
      REAL, SAVE ::  FAERN2O5   ! converts ug -> mol
      REAL, SAVE ::  DFHNO3     ! converts mol -> ug
      REAL, SAVE ::  DFHONO     ! converts mol -> ug

C *** variables for 2 NO2 + H2O -> HONO + HNO3 conversion
      REAL      KNO2       ! pseudo-first order rate constant
      REAL      EXPDT_NO2  ! fraction of NO2 left after chemical rxn
      REAL      TOTSURFA   ! aerosol surface area (m**2/m**3)

C *** first time switch
      LOGICAL, SAVE ::  firstime = .true.

C-----------------------------------------------------------------------

C *** compute only on first pass
      If ( firstime ) Then
        firstime = .false.
        FAERNO2 = 1.0E-6 / precursor_mw( NO2_IDX )
        FAERN2O5 = 1.0E-6 / precursor_mw( N2O5_IDX )
        DFHONO = precursor_mw( HONO_IDX ) / 1.0E-6
        DFHNO3 = precursor_mw( HNO3_IDX ) / 1.0E-6
      End If   ! first time condition

C *** fetch vapor-phase concentrations [ug/m3]
      GHNO3 = precursor_conc( HNO3_IDX )
      GN2O5 = precursor_conc( N2O5_IDX )
      GNO2  = precursor_conc( NO2_IDX )
      GHONO = precursor_conc( HONO_IDX )

C *** set up variables needed for calculating KN2O5

C *** capture values of "dry" 2nd and 3rd moments before equilibration
C     the folowing code assumes that GETPAR has been called with
C     M3_WET_FLAG set to .FALSE. and that the 2nd and 3rd moments have
C     been adjusted for the new SOA.
      OLD_M3_I = moment3_conc( 1 )
      OLD_M3_J = moment3_conc( 2 )
      OLD_M2_I = moment2_conc( 1 )
      OLD_M2_J = moment2_conc( 2 )

C *** compute GAMMA as function of TEMP, RH, & particle composition
C     Note: the last argument to this function can be changed to use
C     a different parameterization of GAMMA.
      GAMMA = N2O5PROB( AIRTEMP, AIRRH, 0 )

C *** calculate molecular speed (m/s) using Eq 4 of Pleim et al (1995)
      CBAR = SQRT( 8.0 * RGASUNIV * AIRTEMP * GPKG
     &             / ( PI * precursor_mw( N2O5_IDX ) ) )

C *** correct molecular diffusivity for ambient conditions
      DIFF_N2O5 = STD_DIFF_N2O5
     &          * ( ( AIRTEMP / STDTEMP ) ** 1.75 )
     &          * ( STDATMPA / AIRPRES )

C *** estimate the "wet third moments" by adding aerosol water
C      Note: this is the H2O concentration from previous time step
      WET_M3_I = OLD_M3_I + H2OFAC * aerospc_conc( AH2O_IDX,1 )
      WET_M3_J = OLD_M3_J + H2OFAC * aerospc_conc( AH2O_IDX,2 )

C *** calculate "wet second moment" assuming that H2O does not
C     affect the geometric standard deviation
      WET_M2_I = OLD_M2_I * ( WET_M3_I / OLD_M3_I ) ** ( 2.0 / 3.0 )
      WET_M2_J = OLD_M2_J * ( WET_M3_J / OLD_M3_J ) ** ( 2.0 / 3.0 )

C *** calculate "wet" geometric mean (same as median) diameters
      DG_AT_WET = aeromode_diam( 1 ) * SQRT( WET_M2_I / OLD_M2_I )
      DG_AC_WET = aeromode_diam( 2 ) * SQRT( WET_M2_J / OLD_M2_J )

C *** calculate effective diameters using Eq 3 of Pleim et al (1995)
      DE_AT_WET = DG_AT_WET * EXP( 1.5
     &          * ( LOG( EXP( aeromode_sdev( 1 ) ) ) ** 2.0 ) )
      DE_AC_WET = DG_AC_WET * EXP( 1.5
     &          * ( LOG( EXP( aeromode_sdev( 2 ) ) ) ** 2.0 ) )

C *** calculate pseudo-first order rate constant using Eq 2 of
C     Pleim et al (1995)
      XXF_AT = WET_M2_I /
     &         ( 4.0 + 0.5 * DE_AT_WET * GAMMA * CBAR / DIFF_N2O5 )
      XXF_AC = WET_M2_J /
     &         ( 4.0 + 0.5 * DE_AC_WET * GAMMA * CBAR / DIFF_N2O5 )
      KN2O5 =   GAMMA * CBAR * PI * ( XXF_AT + XXF_AC )

C *** calculate fraction of N2O5 remaining after chemical reaction
      EXPDT_N2O5 = EXP( - KN2O5 * DT )

C *** set up variables needed for calculating KNO2

C *** calculate aerosol surface area
      TOTSURFA = ( WET_M2_I + WET_M2_J ) * PI

C *** calculate pseudo-first order rate constant using Eq 1 of Vogel
C     et al. (2003). Units of KNO2 is in 1/min in the paper; divide it
C     by 60 to convert it into 1/sec
      KNO2 = MAX ( 0.0, 5.0E-5 * TOTSURFA )

C *** calculate fraction of NO2 remaining after chemical reaction
      EXPDT_NO2 = EXP( -2.0 * KNO2 * DT )

C *** compute new gas-phase concs after heterogeneous reactions occur

C *** adjust nitrous acid for contribution from NO2
      GHONO = GHONO
     &      + ( 0.5 * GNO2  * FAERNO2  * DFHONO ) * ( 1.0 - EXPDT_NO2 )

C *** adjust nitric acid for contributions from N2O5 and NO2
      GHNO3 = GHNO3
     &      + ( 2.0 * GN2O5 * FAERN2O5 * DFHNO3 ) * ( 1.0 - EXPDT_N2O5 )
     &      + ( 0.5 * GNO2  * FAERNO2  * DFHNO3 ) * ( 1.0 - EXPDT_NO2 )

C *** adjust N2O5 for heterogeneous loss
      GN2O5 = GN2O5 * EXPDT_N2O5

C *** adjust NO2 for heterogeneous loss
      GNO2  = GNO2  * EXPDT_NO2

C *** UPDATE GAS and PM CONCENTRATIONS
C     HNO3, N2O5, NO2, and HONO concs are changed in this subroutine.
C     Ensure that all species remain above the minimum concentration.
      precursor_conc( HNO3_IDX ) = MAX( GHNO3, CONMIN )
      precursor_conc( N2O5_IDX ) = MAX( GN2O5, CONMIN )
      precursor_conc( NO2_IDX )  = MAX( GNO2, CONMIN )
      precursor_conc( HONO_IDX ) = MAX( GHONO, CONMIN )

      RETURN

      END SUBROUTINE HETCHEM

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      REAL FUNCTION N2O5PROB( TEMP, RH, GPARAM )

C  Calculates the N2O5 heterogeneous reaction probability, which is the
C  fraction of collisions between a gaseous N2O5 molecule and a particle
C  surface that leads to nitrate production.  In the literature, this
C  probability is commonly referred to with the Greek letter, GAMMA.  To
C  avoid conflicts with the intrinsic GAMMA function on some compilers,
C  we refer to the reaction probability as N2O5PROB in this function.

C  A variety of parameterizations of N2O5PROB are available in this
C  function.  Users may select among the different parameterizations
C  by changing the input argument, GPARAM.  This argument may take on
C  the following values (see code for further details):
C     1. Constant value of 0.1 based on Dentener & Crutzen (1993)
C     2. Function of particle SO4 and NO3, based on Riemer et al. (2003)
C     3. Function of RH, Temp, and particle composition, based on a
C        combination of parameterizations by Evans & Jacob (2005) and
C        Riemer et al. (2003)
C  If GPARAM matches none of the above values, the default calculation
C  of N2O5PROB is a function of RH, T, particle composition, and phase
C  state, based on the parameterization by Davis et al. (2008).

C  Key Subroutines Called: none

C  Key Functions Called: CRHB, IRHX

C  Revision History:
C    First version was coded in November 2007 by Dr. Prakash Bhave
C    using excerpts of the HETCHEM subroutine, which contained only
C    one option for computing N2O5PROB (i.e., GPARAM = 3).
C
C  PVB 11/03/07 Removed code that sets N2O5PROB to zero when RH < 1%.
C
C  PVB 11/05/07 Corrected GPARAM = 3 option to fix the typographical
C               error in the paper by Evans & Jacob (2005), which was
C               found by Dr. Jerry Davis.
C
C  PVB 04/11/08 Updated formulas for LAM1 & LAM2 based on revised paper
C               by Davis et al. (2008).  Added APNDX flag so users may
C               switch between base parameterization and the alternative
C               discussed in Appendix A by Davis et al.  Set default
C               parameterization to match equations in Appendix A.
C               Reduced all regression coefficients by one decimal place
C               for consistency with revised paper.
C
C  JTK 04/17/08 Moved molar mass to AERO_INFO.f
C
C  SH  12/08/09 Use new Fortran modules (aero_data, met_data) in lieu of
C               CBLK array and AERO_INFO module
C
C  SH  03/10/11 Renamed met_data to aeromet_data
 
C References:
C   1. Dentener, F.J. and P.J. Crutzen, Reaction of N2O5 on tropospheric
C      aerosols: Impact of global distributions of NOx, O3, and OH.
C      J. Geophys. Res., Vol 98, 7149-7163, 1993.
C
C   2. Riemer, N., H. Vogel, B. Vogel, B. Schell, I. Ackermann, C.
C      Kessler, and H. Hass, Impact of the heterogeneous hydrolysis
C      of N2O5 on chemistry of nitrate aerosol formation in the lower
C      troposphere under photosmog conditions.  J. Geophys. Res., Vol
C      108, No D4, 4144, doi:10.1029/2002JD002436, 2003.
C
C   3. Evans, M.J. and D.J. Jacob, Impact of new laboratory studies of
C      N2O5 hydrolysis on global model budgets of tropospheric nitrogen
C      oxides, ozone, and OH.  Geophys. Res. Lett., 32, L09813,
C      doi:10.1029/2005GL022469, 2005.
C
C   4. Davis, J.M., P.V. Bhave, and K.M. Foley, Parameterization of N2O5
C      reaction probabilities on the surface of particles containing
C      ammonium, sulfate, and nitrate.  Atmos. Chem. Phys., 2008, in
C      press.
C
C   5. Mentel, T.F., M. Sohn, and A. Wahner, Nitrate effect in the
C      heterogeneous hydrolysis of dinitrogen pentoxide on aqueous
C      aerosols.  Phys. Chem. Chem. Phys., 1, 5451-5457, 1999.
C-----------------------------------------------------------------------

      USE AERO_DATA
      USE AEROMET_DATA   ! Includes CONST.EXT

      IMPLICIT NONE

C *** Arguments
      REAL,    INTENT( IN ) :: TEMP     ! Air temperature [ K ]
      REAL,    INTENT( IN ) :: RH       ! Fractional relative humidity
      INTEGER, INTENT( IN ) :: GPARAM   ! switch to select among
                                        !  parameterizations

C *** Parameters

C *** switch for alternative parameterization of LAM1 & LAM2
C     when APNDX = .TRUE. (default), Eqs A1-A2 are used for reaction
C     probability on aqueous sulfate particles.  Alternatively, set
C     APNDX = .FALSE. to use Eqs 4-5.
      LOGICAL, PARAMETER :: APNDX = .TRUE.

C *** Local Variables

C *** chemical species concentrations [ug/m3]
      REAL      ANH4      ! i+j mode ammonium
      REAL      ANO3      ! i+j mode nitrate
      REAL      ASO4      ! i+j mode sulfate

C *** variables for computing N2O5PROB when GPARAM = 2 or 3
      REAL      FRACSO4   ! aerosol mass ratio of SO4/(SO4+NO3)
      REAL      GAMMA1    ! upper limit of rxn prob
      REAL      GAMMA2    ! lower limit of rxn prob
      REAL      ALPHA     ! RH-dependent parameter to compute GAMMA1
      REAL      BETA      ! TEMP-dependent parameter to compute GAMMA1

C *** variables for default parameterization of N2O5PROB
      LOGICAL   CRHB      ! function to determine if RH is below CRH
      LOGICAL   CRYSTAL   ! true if ambient RH < CRH, false otherwise
      LOGICAL   IRHX      ! function to determine whether RH exceeds IRH
      LOGICAL   FROZEN    ! true if ambient RH > IRH, false otherwise
      REAL      NNO3      ! particle-phase nitrate [micromoles/m3]
      REAL      NSO4      ! particle-phase sulfate [micromoles/m3]
      REAL      NNH4      ! particle-phase ammonium [micromoles/m3]
      REAL      NANI      ! particle-phase anions [micromoles/m3]
      REAL      X1        ! mole fraction of ammonium bisulfate
      REAL      X2        ! mole fraction of ammonium sulfate
      REAL      X3        ! mole fraction of ammonium nitrate
      REAL      LAM1      ! logit transformation of N2O5PROB on
      REAL      LAM2      !   aqueous NH4HSO4 [LAM1], aqueous (NH4)2SO4
      REAL      LAM3      !   [LAM2], aqueous NH4NO3 [LAM3], and dry
      REAL      LAMD      !   sulfate-containing particles [LAMD]
      REAL      GAM1      ! reaction probability on aqueous NH4HSO4
      REAL      GAM2      !    "          "      "     "    (NH4)2SO4
      REAL      GAM3      !    "          "      "     "    NH4NO3
      REAL      GAMD      !    "          "      " dry sulfate particles
      REAL      T293,T291 ! temperature threshold variables
      REAL      RH46      ! RH threshold variable

C *** statement function for inverting the logit transformation given
C     in Eq 7 by Davis et al (2008)
      REAL      LOGITINV  ! statement function
      REAL      XX        ! dummy argument for LOGITINV
      LOGITINV( XX ) = 1.0 / ( 1.0 + EXP( -XX ) )

C-----------------------------------------------------------------------

C *** retrieve particle-phase ammonium, nitrate, and sulfate [ug/m3]
      ANH4 = aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANH4_IDX,2 )
      ANO3 = aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 )
      ASO4 = aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ASO4_IDX,2 )

C *** User Option: GPARAM = 1
C     Dentener and Crutzen (1993) recommended a constant value of
C     N2O5PROB = 0.1, which was used in CMAQ prior to ver4.3.  In more
C     recent literature, this value has been recognized as an upper
C     estimate of N2O5PROB so it should not be used for routine
C     simulations.  It is included here only to facilitate sensitivity
C     studies by CMAQ model users.
      IF ( GPARAM .EQ. 1 ) THEN
         N2O5PROB = 0.1
         RETURN
      END IF

C *** User Options: GPARAM = 2 and 3
C     These options both employ Eqs 2 and 3 by Riemer et al (2003), in
C     which N2O5PROB varies according to the particle-phase sulfate and
C     nitrate concentrations.  In both options, the NO3 effect (i.e.,
C     GAMMA1/GAMMA2) is assumed to be a factor of 10 based on Mentel et
C     al (1999) and Riemer et al (2003).
C      - When GPARAM = 2, upper limit of N2O5PROB is fixed at 0.02.
C        This was the default setting in CMAQ ver4.3 through ver4.5.1.
C      - When GPARAM = 3, upper limit of N2O5PROB is a function of
C        ambient TEMP & RH based on the "Sulfate" equation in Table 1
C        by Evans & Jacob (2005).  This was the default setting in CMAQ
C        ver4.6.  After that release, a typographical error was found
C        in the published equation of Evans & Jacob (2005) so this code
C        has been corrected accordingly.
      IF ( GPARAM .EQ. 2 ) THEN

         GAMMA1 = 0.02

      ELSE IF ( GPARAM .EQ. 3 ) THEN

C        In this function, RH is in fractional units whereas the
C        published equation by Evans&Jacob refers to RH as a percentage.
         ALPHA = 2.79E-4 + RH * ( 1.3E-2 + RH * ( -3.43E-2 + 7.52E-2 * RH ) )

C        To fix the typographical error by Evans & Jacob (2005), the
C        sign of BETA has been switched in this code.
         IF ( TEMP .LT. 282.0 ) THEN
            GAMMA1 = 3.0199517 * ALPHA   ! (10.0 ** 0.48) * ALPHA
         ELSE
            BETA  = 0.04 * ( 294.0 - TEMP )
            GAMMA1 = ALPHA * ( 10.0 ** BETA )
         END IF

      END IF

      IF ( ( GPARAM .EQ. 2 ) .OR. ( GPARAM .EQ. 3 ) ) THEN

         IF ( ANO3 .GT. 0.0 ) THEN
            FRACSO4 = ASO4 / ( ASO4 + ANO3 )
         ELSE
            FRACSO4 = 1.0
         END IF

         GAMMA2 = 0.1 * GAMMA1
         N2O5PROB = GAMMA2 + FRACSO4 * ( GAMMA1 - GAMMA2 )
         RETURN

      END IF

C *** Default setting in current version of CMAQ:
C     This code implements the paramaterization given in Eq 15 by Davis
C     et al (2008), in which N2O5PROB is a function of RH, TEMP,
C     particle composition, and phase state.  Note: In this function, RH
C     is in fractional units whereas the published equations refer to RH
C     as a percentage.

C *** Check whether the ambient RH is below the crystallization RH for
C     the given inorganic particle composition.
      CRYSTAL = CRHB( RH, .TRUE. )

C *** Check whether the ambient RH exceeds the RH of ice formation.
      FROZEN = IRHX( TEMP, RH )

C *** Set N2O5PROB to constant value if particles contain ice, based on
C     Eq 14 by Davis et al (2008).
      IF ( FROZEN ) THEN
         N2O5PROB = 0.02                               ! Eq 14

C *** Compute mole-fractional-composition of particles based on Eq 11 by
C     Davis et al (2008).
      ELSE
         NNO3 = ANO3 / aerospc_mw( ANO3_IDX )
         NSO4 = ASO4 / aerospc_mw( ASO4_IDX )
         NNH4 = ANH4 / aerospc_mw( ANH4_IDX )
         NANI = NNO3 + NSO4

         X3 = NNO3 / NANI
         X2 = MAX( 0.0, MIN( 1.0 - X3, NNH4/NANI - 1.0 ) )
         X1 = 1.0 - ( X2 + X3 )

C *** Compute N2O5PROB on pure NH4NO3 particles using Eqs 6 and 8 by
C     Davis et al (2008).
         LAM3 = -8.10774 + 4.902 * RH                  ! Eq 6
         GAM3 = MIN( LOGITINV( LAM3 ), 0.0154 )        ! Eq 8

C *** Compute N2O5PROB on dry particles using Eqs 9, 10, and 13 by
C     Davis et al (2008).
         IF ( CRYSTAL ) THEN
            T293     = MAX( 0.0, TEMP - 293.0 )
            LAMD     = -6.13376 + 3.592 * RH           ! Eq 9
     &                - 0.19688 * T293
            GAMD     = MIN( LOGITINV( LAMD ), 0.0124 ) ! Eq 10
            N2O5PROB = ( X1 + X2 ) * GAMD              ! Eq 13
     &                + X3 * MIN( GAMD, GAM3 )

C *** Compute N2O5PROB on aqeuous particles using Eqs A1, A2, 8, and 12
C     by Davis et al (2008).  When APNDX = .TRUE. (default), Eqs A1-A2
C     are used for reaction probability on aqueous sulfate particles.
C     Switch to .FALSE. if Eqs 4-5 are desired.  See Appendix A by
C     Davis et al. (2008) for a discussion of these options.
         ELSE
            T291 = MAX( 0.0, TEMP - 291.0 )
            IF ( APNDX ) THEN
               RH46 = MIN( 0.0, RH - 0.46 )
               LAM2  = -3.64849 + 9.553 * RH46         ! Eq A2
               LAM1  = LAM2 + 0.97579                  ! Eqs A1 & A2
     &                - 0.20427 * T291
            ELSE
               LAM1  = -4.10612 + 2.386 * RH           ! Eq 4
     &                - 0.23771 * T291
               LAM2  = LAM1 - 0.80570                  ! Eqs 4 & 5
     &                + 0.10225 * T291
            END IF
            GAM1     = MIN( LOGITINV( LAM1 ), 0.08585 )! Eq 8
            GAM2     = MIN( LOGITINV( LAM2 ), 0.053 )  ! Eq 8
            N2O5PROB = ( X1 * GAM1 )                   ! Eq 12
     &               + ( X2 * GAM2 )
     &               + ( X3 * GAM3 )

         END IF

      END IF

      RETURN

      END FUNCTION N2O5PROB

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LOGICAL FUNCTION CRHB( RH, COMPLETE )

C  Determines whether the ambient RH is below the crystallization relative
C  humidity (CRH).  The output of this logical function is .TRUE. when the
C  ambient RH is below the CRH and .FALSE. otherwise.  The empirical
C  equations developed by Martin et al (2003) are applied to determine the
C  CRH for a given mixture of sulfate, nitrate, and ammonium.  Though those
C  equations are validated only at 293K, they are applied at all ambient
C  temperatures because insufficient data exist to estimate the temperature
C  dependence of the CRH of mixed sulfate-nitrate-ammonium particles.
C  Users can opt to compute either the RH of initial crystal formation
C (i.e., COMPLETE .EQ. .FALSE.) or the RH of compete crystallization (i.e.,
C  COMPLETE .EQ. .TRUE.).

C  References:
C   1. Martin, S.T., J.C. Schlenker, A. Malinowski, H.-M. Hung, and
C      Y. Rudich, Crystallization of atmospheric sulfate-nitrate-
C      ammonium particles.  Geophys. Res. Lett., 30(21), 2102,
C      doi:10.1029/2003GL017930, 2003.

C  Revision History:
C    PVB 11/05/07 Coded the first version.
C    JTK 04/17/08 Moved molar mass to AERO_INFO.f
C    SH  12/08/09 Use aero_data module in lieu of CBLK array

C-----------------------------------------------------------------------

      USE aero_data

      IMPLICIT NONE

C *** Arguments
      REAL, INTENT( IN )    :: RH        ! fractional relative humidity
      LOGICAL, INTENT( IN ) :: COMPLETE  ! flag deciding which CRH
                                         !  equation to use

C *** Local Variables

C *** chemical species concentrations [micromoles/m3]
      REAL      NSO4     ! i+j mode sulfate
      REAL      NNO3     ! i+j mode nitrate
      REAL      NNH4     ! i+j mode ammonium
      REAL      NCAT     ! i+j mode cations
      REAL      NANI     ! i+j mode anions

C *** cation and anion mole fractions used in CRH equations
      REAL X         ! ammonium/cation mole fraction: NH4/(NH4+H)
      REAL Y         ! sulfate/anion mole fraction:  SO4/(SO4+NO3)

C *** intermediate variables used in CRH equations
      REAL X2, XY, Y2, X2Y, XY2, RDEN

      REAL CRH       ! crystallization RH (fractional units)

C-----------------------------------------------------------------------

C *** Experimental measurements of CRH are lacking below 1% relative
C     humidity.  Under those very dry conditions, we assume that
C     particles will crystallize.  Equations by Martin et al (2003) for
C     internally-mixed sulfate-nitrate-ammonium particles yield maximum
C     CRH values of 35.03% and 34.50% for initial crystal formation and
C     complete crystallization, respectively.  Therefore, the full CRH
C     calculation can be avoided when RH > 35.1%.
      IF ( RH .LE. 0.01 ) THEN
         CRHB = .TRUE.    ! ambient particles are dry
         RETURN
      ELSE IF ( RH .GT. 0.351 ) THEN
         CRHB = .FALSE.   ! ambient RH exceeds CRH
         RETURN
      END IF

C *** calculate total particle-phase composition [micromoles/m3]
      NNO3 = ( aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 ) )
     &     / aerospc_mw( ANO3_IDX )
      NSO4 = ( aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ASO4_IDX,2 ) )
     &     / aerospc_mw( ASO4_IDX )
      NNH4 = ( aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANH4_IDX,2 ) )
     &     / aerospc_mw( ANH4_IDX )

C *** calculate total anion and cation concentrations
      NCAT = MAX( NNH4, 2.0 * NSO4 + NNO3 )
      NANI = NNO3 + NSO4

C *** calculate ammonium and sulfate mole fractions
      X = NNH4 / NCAT
      Y = NSO4 / NANI

C *** Experimental data of Martin et al. (2003) show no crystal
C     formation when X < 0.50 or Y < 0.22.  For these particle
C     compositions, the full CRH calculation can be avoided.
C
C     Note: Martin's equation for initial crystal formation returns
C     very large CRH values when X and Y approach zero.  However,
C     those values were verified to be erroneous by personal
C     communication with Dr. Scot Martin on Aug. 30, 2007.
      IF ( ( X .LT. 0.50 ) .OR. ( Y .LT. 0.22 ) ) THEN
         CRHB = .FALSE.   ! ambient RH exceeds CRH
         RETURN
      END IF

C *** store some terms needed to evaluate the CRH equations
      X2   = X * X
      XY   = X * Y
      X2Y  = X2 * Y
      Y2   = Y * Y
      XY2  = X * Y2
      RDEN = 1.0 / ( 25.0 + ( X - 0.7 ) * ( Y - 0.5 ) )

C *** calculate CRH using empirical equations of Martin et al (2003)
      IF ( COMPLETE ) THEN

         CRH = 3143.44 + (63.07 * X) + (0.114 * X2) + (87.97 * Y)
     &       - (125.73 * XY) + (0.586 * X2Y) + (0.95 * Y2)
     &       - (1.384 * XY2) - (79692.5 * RDEN)

      ELSE

         CRH = -697.908 - (15.351 * X) + (0.43 * X2) - (22.11 * Y)
     &       + (33.882 * XY) - (1.818 * X2Y) + (0.772 * Y2)
     &       - (1.636 * XY2) + (17707.6 * RDEN)

      END IF

C *** set value of the output variable, CRHB
      IF ( RH .LE. CRH ) THEN
         CRHB = .TRUE.    ! ambient particles are dry
      ELSE
         CRHB = .FALSE.   ! ambient RH exceeds CRH
      END IF

      RETURN

      END FUNCTION CRHB

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LOGICAL FUNCTION IRHX( TEMP, RH )

C  Determines whether the ambient RH has exceeded the RH of ice formation,
C  based on the Goff-Gratch equations as given by List (1984).

C  References:
C   1. Goff, J.A. and S. Gratch, Low-pressure properties of water from
C      -160 to 212 F, in Transactions of the American Society of Heating
C      and Ventilating Engineers, pp 95-122, New York, 1946.
C   2. List, R.J. (editor), Smithsonian Meteorological Tables, 5th ed.
C      pp. 350, 1984.

C  Revision History:
C   PVB 11/06/07 Coded the first version.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C *** Arguments
      REAL, INTENT( IN ) ::  TEMP        ! Air temperature [ K ]
      REAL, INTENT( IN ) ::  RH          ! Fractional relative humidity

C *** Parameters

C *** The following values are taken from List (1984).  Note that
C     these differ slightly from the original equations published by
C     Goff & Gratch (1946).  We also note that T0 and PST differ
C     slightly from STDTEMP and STDATMPA in the AERO_INFO module.
C     Here, we use 273.16 K and 1013.246 hPa to be consistent with
C     the Goff-Gratch equations as given by List (1984).
      REAL, PARAMETER :: TST = 373.16    ! steam-point temperature, K
      REAL, PARAMETER :: T0  = 273.16    ! ice-point temperature, K
      REAL, PARAMETER :: PST = 1013.246  ! sat vapor pres at TST, hPa
      REAL, PARAMETER :: P0  = 6.1071    ! sat vapor pres at T0, hPa

      REAL, PARAMETER :: LOGPST = 3.0057149     ! LOG10(PST)
      REAL, PARAMETER :: LOGP0  = 0.78583503    ! LOG10(P0)

C *** Local Variables

C *** estimates of IRH using a polynomial approximation
      REAL      EIRH   ! IRH approximated using 2nd order polynomial
      REAL      LIRH   ! lower-bound of IRH
      REAL      UIRH   ! upper-bound of IRH

C *** variables used to compute RH of ice formation
      REAL      TSDT, TDTS, T0DT, TDT0  ! intermediate variables
      REAL      LOGPW  ! log10 of saturation vapor pressure over H2O
      REAL      LOGPI  ! log10 of saturation vapor pressure over ice

      REAL      IRH    ! fractional RH at which ice forms

C-----------------------------------------------------------------------

      IF ( TEMP .LT. T0 ) THEN

C *** To mitigate the computational expense associated with Goff-Gratch
C     equations, use a 2nd order polynomial function to approximate IRH.
C     This approximation, EIRH, matches IRH from the full Goff-Gratch
C     equations within 0.004 over the entire low-temperature range of
C     interest (200 to 275K) and is used for screening purposes.
         EIRH = 1.61299 + TEMP * ( 4.4117437E-5 * TEMP - 1.4293888E-2 )
         LIRH = EIRH - 0.005
         UIRH = EIRH + 0.005

         IF ( RH .GT. UIRH ) THEN
            IRHX = .TRUE.
         ELSE IF ( RH .LT. LIRH ) THEN
            IRHX = .FALSE.
         ELSE

C *** Compute IRH using Goff-Gratch equations as given by List (1984)
            TSDT  = TST / TEMP
            TDTS  = TEMP / TST
            T0DT  = T0 / TEMP
            TDT0  = TEMP / T0
            LOGPW = -7.90298 * ( TSDT - 1.0 )
     &            + 5.02808 * LOG10( TSDT )
     &            - 1.3816E-7 * ( 10.0 ** ( 11.344 *  (1.0 - TDTS) ) -1.0 )
     &            + 8.1328E-3 * ( 10.0 ** ( -3.49149* (TSDT - 1.0) ) -1.0 )
     &            + LOGPST
            LOGPI = -9.09718 * (T0DT - 1.0)
     &            - 3.56654 * LOG10( T0DT )
     &            + .876793 * (1.0 - TDT0)
     &            + LOGP0
            IRH   = 10.0 ** ( LOGPI - LOGPW )

            IF ( RH .GT. IRH ) THEN
               IRHX = .TRUE.
            ELSE
               IRHX = .FALSE.
            END IF

         END IF
      ELSE
         IRHX = .FALSE.
      END IF

      RETURN

      END FUNCTION IRHX

