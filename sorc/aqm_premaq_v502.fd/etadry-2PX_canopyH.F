SUBROUTINE etadry

!-------------------------------------------------------------------------------
! Name:     Eta Dry Deposition
! Purpose:  Computes dry deposition velocities using Rst and Ra, and
!           elements of ADOM DD model.  Uses output parameters supplied by
!           NCEP's Eta model.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  26 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           03 Apr 2003  Added logic for modeling snow covered surfaces.
!                        Changed the reactivities for SO2, HNO3 and NH3.
!                        Changed pH values to have an east-west variation.
!                        Using the Henry's law constant function from CMAQ in
!                        place of local code.  Also changed the code for
!                        deposition to water to use a pH of 8.1 and the
!                        temperature of water in calculating the Henry's law
!                        constant.  Adjusted values of RSNOW0 = 1000 and
!                        A(NH3) = 20.  Added new dry deposition species: N2O5,
!                        NO3, Generic_aldehyde.  Corrected diffusivities of
!                        chemicals and water and viscosity of air to all be at
!                        the same temperature (273.15K).  Temperature and
!                        pressure adjustments to the values are not needed
!                        because the diffusivities and viscosity are always used
!                        as ratios, so the temperature-pressure dependence was
!                        removed.  (D. Schwede and J. Pleim)
!           17 Apr 2003  Modified for PREMAQ and tailored for Eta model output.
!                        (T. Otte and J. Pleim)
!           25 Nov 2003  Changed water-specific algorithms to look for dynamic
!                        water category in land use.  (T. Otte)
!           09 Dec 2003  Removed scaling of surface exchange coefficient by
!                        density, per e-mail from NCEP suggesting that CH
!                        is not actually coupled with density.  (T. Otte)
!           29 Apr 2004  Changed XTEMP1P5 to XTEMP2.  (T. Otte)
!           20 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           01 FEB 2005  Changed water-specific algorithms to look for dynamic
!                        water category in land use to be consistent with the 
!                        Eta input                              (Hsin-mu Lin)
!           08 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!           07 July 2007 Add mesophyll resistance for the calculation of
!                        deposition velocity                    (Hsin-mu Lin)
!           13 Mar 2008  Modify the "xradyn" calculation according to Pleim's
!                        suggestion for "compute Ra like in pxsfclay in WRF"
!                                                               (Hsin-mu Lin)
!           15 June 2010 According to Daewon's suggestion, the M-O equation 
!                        including vegetation height (about 10 meter) over the 
!                        forested region is implemented using a subroutine from 
!                        MCIP version 3.0 (resistcalc.F)
!                                                               (Yunsoo Choi)
!           29 Oct 2010 According to GRL paper, the new vegetation height 
!                        (satellite product) is implemented over the forest region
!                        See the detail in the presentation by Choi et al. from
!                        2010 CMAS conference 
!                                                               (Yunsoo Choi)
!           
!            5 Nov 2010 Change to read satellite canopy once from IOAPI file  ( Youhua Tang)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE const
  USE const_pbl, ONLY: betah => betah_PX , gamah, pro, vkar
  USE gridinfo, ONLY: map

  USE PARMS3
  USE FDESC3
  USE IODECL3

  IMPLICIT NONE

  REAL                         :: a          ( ltotg )
  REAL,          PARAMETER     :: a0         = 8.0     ! [dim'less]
! REAL                         :: alphs      ( ltotg )
  INTEGER                      :: c
  REAL                         :: ch
  REAL,          PARAMETER     :: chmin      = 0.00001 ! [m/s]
  REAL,          PARAMETER     :: d3         = 4.8e-4  ! [dim'less]
  REAL                         :: delta
  REAL                         :: dif0       ( ltotg )
  REAL                         :: diffu
! REAL                         :: dkhor      ( ltotg ) ! [K] - enthalpy (activation energy)
  REAL,          PARAMETER     :: dwat       = 0.2178  ! [cm^2 / s] at 273.15K
  LOGICAL                      :: effective            ! T=calc effective Henry's Law const
  REAL                         :: es
! REAL                         :: h                    ! [dim'less]
  REAL                         :: hcan
  REAL                         :: heff
  REAL,          EXTERNAL      :: hlconst              ! [M / atm]
  REAL                         :: hplus
  REAL                         :: hplus_const
  INTEGER                      :: ifsnow               ! 1=snow
! REAL                         :: kh         ( ltotg ) ! [M / atm]
  REAL,          PARAMETER     :: kvis       = 0.132   ! [cm^2 / s] at 273.15K
  INTEGER                      :: l
  REAL                         :: meso       ( ltotg )
  REAL,          PARAMETER     :: pr         = 0.709   ! [dim'less]
  REAL                         :: qss
  INTEGER                      :: r
  REAL                         :: rac
  REAL,          PARAMETER     :: ramin      = 5.0     ! [s/m]
  REAL                         :: rbc
  REAL                         :: rbh
  REAL                         :: rbsulf
  REAL                         :: rbw
  REAL                         :: rci
! REAL                         :: rci_old
  REAL                         :: rcut
  REAL,          PARAMETER     :: rcut0      = 3000.0  ! [s/m]
  REAL,          PARAMETER     :: rcw0       = 125000.0 ! acc'd'g to Padro and
                                                       ! adapted from Slinn 78
  REAL,          PARAMETER     :: rg0        = 1000.0  ! [s/m]
  REAL                         :: rgnd
  REAL                         :: rgndc
  REAL                         :: rgw                  ! resist for water-covered sfc
! REAL,          PARAMETER     :: rgwet0     = 25000.0
  REAL                         :: rh                   ! relative humidity
  REAL                         :: rinc
  REAL                         :: rnph                 ! pH of rain water
  REAL,          PARAMETER     :: rnph0      = 4.3     ! default pH of rain water
  REAL,          PARAMETER     :: rsndiff    = 10.0    ! snow diffusivity fac
  REAL                         :: rsnow
  REAL,          PARAMETER     :: rsnow0     = 1000.0
  REAL                         :: rstom
  REAL                         :: rsurf
! REAL                         :: rsurf_old
  REAL                         :: rwet                 ! wet sfc resist (cuticle or grd)
  REAL                         :: rwetsfc
  REAL                         :: scc
  REAL                         :: scw
  REAL                         :: scw_pr_const
  CHARACTER*16                 :: subname    ( ltotg ) ! chemical name
  REAL,          PARAMETER     :: svp2       = 17.67   ! from MM5
  REAL,          PARAMETER     :: svp3       = 29.65   ! from MM5
  REAL,          PARAMETER     :: wrmax      = 0.5     ! [kg/m2 = mm] from Eta
  REAL                         :: xm                   ! liquid water mass frac

  REAL  :: gz1oz0, psih, yl1, ynt, zntol, zol, z0_10, z0, z1, psih0, tmp1, tmp2

  EXTERNAL       ENVYN, PROMPTMFILE

  LOGICAL        ENVYN, PX_RA
  LOGICAL, SAVE  :: first

  INTEGER        ISTAT, TRIMLEN
  INTEGER, SAVE            :: icanofile

  REAL, SAVE, ALLOCATABLE  :: canoh2(:,:)

  CHARACTER*16 :: PROGNAME = 'ETADRY'          ! program name
  CHARACTER*16    PROMPTMFILE, CANOPY_IN
  CHARACTER*300   MESG                         ! message buffer for M3EXIT()

  data first /.true./

!-------------------------------------------------------------------------------
!-- Chemical-Dependent Parameters (Original Source: Modified ADOM - Padro)
!
!                                                          at 298.15 K
!     Species   Dif0(cm2/s) Alphastar   Reactivity     -DKHOR [K]  KH0 [M/atm]
!     _______   ___________ _________   __________     _________  __________
!  1   SO2      0.1089 ~    1000.       8.00              3100.0 ~     1.2e00 ~
!  2   SULF      --         --           --                --           --
!  3   NO2      0.1361 ~    1.00        8.00 2.00*        2500.0 ~     1.2e-2 ~
!  4   NO       0.1802 ~    1           2.0+              1500.0 ~     1.9e-3 ~
!  5   O3       0.1444 ~    10.00       15.0  8@          2700.0 ~     1.2e-2 ~
!  6   HNO3     0.1628 ~    1E9         18.0 800* 8000**  8700.0 ~     2.6e+6 ~
!  7   H2O2     0.2402 ~    1.00        12.0 30*          6600.0 ~     9.7e+4 ~
!  8   ACET ALD 0.1525 ~    1           10.+              5700.0 ~     1.3e+1 ~
!  9   HCHO     0.1877 ~    1.00        10.+              6400.0 ~     7.0e+3 ~
! 10   OP       0.1525 ~    1           10.0+             5200.0 ~     3.1e+2 ~
! 11   PAA      0.1220 ~    1           20+               5300.0 ~     8.4e+2 ~
! 12   ORA      0.1525 ~    1           20+               5700.0 ~     3.7e+3 ~
! 13   NH3      0.1978 ~    1E5         10.0              4100.0 ~     5.8e+1 ~
! 14   PAN      0.0938 ~    1           4.0               5900.0 ~     2.9e00 ~
! 15   HONO     0.1525 ~    1           20+               4800.0 ~     4.9e+1 ~
! 16   CO       0.1807 ~    1           5.0               1600.0 ~     9.5e-4 ~
! 17   METHANOL 0.1363 ~    1.0 ~       2.0 ~             5200.0 ~     2.2e+2 ~
! --   CO2      0.1381 ~                                  2400.0 ~     3.4e-2 ~
! 18   N2O5     0.0808 ^^               5000.0**
! 19   NO3      0.1153 ^^               5000.0**
! 20   GEN ALD  0.1525 ##   1.0         10.0##
!
!---------Notes
!  * Updates based on literature review 7/96 JEP
!  # Diff and H based on Wesely (1988) same as RADM
!  + Estimated by JEP 2/97 
!  ^ Estimated by WTH 10/99
!  @ Updated by JEP 9/01
!  ~ Added by YW 1/02.  Dif0 based on Massman (1998).  Henry's Law constant
!    is defined here as: h=cg/ca, where cg is the concentration of a species
!    in gas-phase, and ca is its aqueous-phase concentration.  The smaller h,
!    the larger solubility.  Henry's Law constant in another definition (KH):
!    KH = ca/pg [M/atm], KH = KH0 * exp(-DKH/R(1/T-1/T0)), where KH0 and -DKH
!    values are from Rolf Sander (1999).  h=1/(KH*R*T).
! ** Update by DBS based on estimates by JEP 1/03
! ^^ From Bill Massman, personal communication 4/03
! ## Diffusivity calculated by SPARC, reactivity = other aldehydes
! ++ Dif0 in Massman is diffusivity at temperature 0C and 1 atm (101.325kPa), so
!    chemicals that were not in Massman's paper need to be adjusted.  We assume
!    JEP's original values were for 25C and 1 atm.
!
!    Suggestion from Jon Pleim   (fix the bugs)
!    1. add "meso" vars & modify "rstom" accordingly
!    2. change "dif0" & "a" of "SULFATE" from 0, 0 to 0.0001, 0 
!                                                    (Hsin-mu Lin, 2007/06/13)
!-------------------------------------------------------------------------------

  DATA subname( 1), dif0( 1), a( 1), meso( 1) / 'SO2             ', 0.1089,   10.0,     0.0/
! DATA subname( 2), dif0( 2), a( 2), meso( 2) / 'SULFATE         ', 0.0000,    0.0,     0.0/
  DATA subname( 2), dif0( 2), a( 2), meso( 2) / 'SULFATE         ', 0.0001,    0.0,     0.0/
  DATA subname( 3), dif0( 3), a( 3), meso( 3) / 'NO2             ', 0.1361,    2.0,   500.0/
  DATA subname( 4), dif0( 4), a( 4), meso( 4) / 'NO              ', 0.1802,    2.0,  9400.0/
  DATA subname( 5), dif0( 5), a( 5), meso( 5) / 'O3              ', 0.1444,    8.0,     0.0/
  DATA subname( 6), dif0( 6), a( 6), meso( 6) / 'HNO3            ', 0.1067, 8000.0,     0.0/
  DATA subname( 7), dif0( 7), a( 7), meso( 7) / 'H2O2            ', 0.1300,   30.0,     0.0/
  DATA subname( 8), dif0( 8), a( 8), meso( 8) / 'ACETALDEHYDE    ', 0.1111,   10.0,     0.0/
  DATA subname( 9), dif0( 9), a( 9), meso( 9) / 'FORMALDEHYDE    ', 0.1554,   10.0,     0.0/
  DATA subname(10), dif0(10), a(10), meso(10) / 'METHYLHYDROPEROX', 0.1179,   10.0,     0.0/
  DATA subname(11), dif0(11), a(11), meso(11) / 'PEROXYACETIC_ACI', 0.0868,   20.0,     0.0/
  DATA subname(12), dif0(12), a(12), meso(12) / 'ACETIC_ACID     ', 0.0944,   20.0,     0.0/
  DATA subname(13), dif0(13), a(13), meso(13) / 'NH3             ', 0.1978,   20.0,     0.0/
  DATA subname(14), dif0(14), a(14), meso(14) / 'PAN             ', 0.0687,    4.0,     0.0/
  DATA subname(15), dif0(15), a(15), meso(15) / 'HNO2            ', 0.1349,   20.0,     0.0/
  DATA subname(16), dif0(16), a(16), meso(16) / 'CO              ', 0.1807,    5.0,100000.0/
  DATA subname(17), dif0(17), a(17), meso(17) / 'METHANOL        ', 0.1329,    2.0,     0.0/
  DATA subname(18), dif0(18), a(18), meso(18) / 'N2O5            ', 0.0808, 5000.0,     0.0/
  DATA subname(19), dif0(19), a(19), meso(19) / 'NO3             ', 0.1153, 5000.0,     0.0/
  DATA subname(20), dif0(20), a(20), meso(20) / 'GENERIC_ALDEHYDE', 0.0916,   10.0,     0.0/

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  One sign is that the XUSTAR array
! will contain all 0.0 values.  In that case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  PX_RA = ENVYN ('PX_RA', 'USE PX_Ra ?', .FALSE. , ISTAT)

  effective = .TRUE.

  xvd(:,:,:) = 0.0

  IF ( MAXVAL(xustar) == 0.0 ) THEN  ! assume initialization period

    xradyn(:,:) = badval3   ! inverse taken in metcro.F
    xrbndy(:,:) = badval3   ! inverse taken in metcro.F
    xrstom(:,:) = badval3   ! inverse taken in metcro.F

  ELSE

    hplus_const = 10.0**(-8.1)
    scw = kvis / dwat
    scw_pr_const = (scw/pr)**0.66667

!-------------------------------------------------------------------------------
! Loop over grid cells and calculate dry deposition.
!-------------------------------------------------------------------------------

!$omp parallel do                                                      &
!$omp private(c,ch,delta,diffu,es,hcan,heff,                           &
!$omp hplus,ifsnow,l,qss,r,rac,rbc,rbh,rbsulf,rbw,rci,rcut,            &
!$omp rgnd,rgndc,rgw,rh,rinc,rnph,rsnow,rstom,rsurf,rwet,rwetsfc,scc)  &
!$omp firstprivate(hplus_const,scw,scw_pr_const)

    DO r = 1, nrows_x
       DO c = 1, ncols_x

        !-------------------------------------------
        ! Calculate the aerodynamic resistance.
        !-------------------------------------------

        rbh = 5.0 / xustar(c,r)

        if ( .not. PX_RA ) then

           ch  = MAX(chmin, xch(c,r))             ! minimum ch (prevent zeroes)

           ! Though the veg type is USGS, water=>16, input Eta data reserve the 16
           ! and still use "0" as water surface.
           ! in order to compensate the change and in caution, set "0" & "16" valid

           IF ( NINT(xdluse(c,r))==lwater .or. NINT(xdluse(c,r))==lwater2 ) ch = chmin
           xradyn(c,r) = 1.0 / ch - rbh
           xradyn(c,r) = MAX(ramin, xradyn(c,r))  ! minimum Ra (prevent neg vals)

        else

           !-------------------- BEGIN --------------------------------------------
           !------- compute Ra like in pxsfclay in WRF (03/13/2008) ---------------

           !-------------------------------------------------------------------------
           ! Calculate Ra according to M-O sfc layer theory like in PX LSM

           ! zol   = x3htm(c,r,1) / xmol(c,r)
           ! zntol = xzruf(c,r  ) / xmol(c,r)

           ! IF ( xmol(c,r) < 0.0 ) THEN
           !    ynt = ( 1.0 - gamah * zntol )**0.5
           !    yl1 = ( 1.0 - gamah * zol   )**0.5
           !    psih = 2. * ALOG( (yl1+1.0) / (ynt+1.0) )
           ! ELSE
           !    IF ( (zol-zntol) <= 1.0 ) THEN
           !       psih = -betah * (zol-zntol)
           !    ELSE
           !       psih = 1. - betah - (zol-zntol)
           !    ENDIF
           ! ENDIF

           ! gz1oz0 = ALOG( x3htm(c,r,1) / xzruf(c,r) )
           ! xradyn(c,r) = pro * (gz1oz0 - psih) / (vkar * xustar(c,r))

           !------- compute Ra like in pxsfclay in WRF (03/13/2008) ----------------
           !---------------------- END ---------------------------------------------

         !=======================================================================
         !--- The followings are modification based on previous block
         !=======================================================================

           !---------------------------------------------------------------------
           ! Read the canopy height from satellite file
           !---------------------------------------------------------------------
  
           if (first) then
              write (*,*) 'use supplied canopy height'

              CANOPY_IN = PROMPTMFILE('Enter name for CANOPY file',             &
                                       FSREAD3, 'CANOPY_IN', PROGNAME )
              IF ( .NOT. DESC3( CANOPY_IN ) ) THEN
                 MESG = 'Could not get description of file "' //                &
                                   CANOPY_IN( 1 : TRIMLEN( CANOPY_IN ) ) // '"'
                 CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
              ENDIF

              if (ncols3d /= ncols_x  .or.  nrows3d /= nrows_x) then
                 MESG = 'inconsistent dimension of file "' //                   &
                                   CANOPY_IN( 1 : TRIMLEN( CANOPY_IN ) ) // '"'
                 write (*,*) 'ncols3d=', ncols3d, '    nrows3d=', nrows3d
                 write (*,*) 'ncols_x=', ncols_x, '    nrows_x=', nrows_x
                 CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
              endif

              IF (.NOT. ALLOCATED (canoh2) ) ALLOCATE( canoh2(ncols_x,nrows_x) )

              IF (.NOT. READ3(CANOPY_IN,'CANOPY_HEIGHT',1, 0, 0, canoh2)) THEN
                 CALL M3EXIT( PROGNAME, 0, 0, 'Error reading CANOPY file', 2 )
              ENDIF

              IF (.NOT. CLOSE3(CANOPY_IN)) THEN
                 CALL M3EXIT( PROGNAME, 0, 0, 'Error closing CANOPY file', 2 )
              ENDIF

              icanofile = 0
              first=.false.
           endif

           !------------------------------------------------------------------------
           ! Consider vegetation height over the forest regions 6/15/2010
           !------------------------------------------------------------------------

           if (xzruf(c,r) .eq. 0.800 .or.    &
               xzruf(c,r) .eq. 0.850 .or.    &
               xzruf(c,r) .eq. 2.650 .or.    &
               xzruf(c,r) .eq. 1.090) then
              z0_10 = xzruf(c,r) + 10
           else
              z0_10 = xzruf(c,r)
           endif

           if ( icanofile==0 ) then
              tmp1 = MAX( 0. , canoh2(c,r) )
              z0 = MAX( z0_10 , xzruf(c,r) + tmp1 )
           else
	    z0 = z0_10
	 endif  

           ! z1 = x3htm(c,r,1)
           z1 = MAX ( x3htm(c,r,1) , z0+0.1 )           
           
           zol   = z1 / xmol(c,r)
           zntol = z0 / xmol(c,r)

           if (zol >= 0.0) then
              if (zol > 1.0) then
                 psih0 = 1.0 - betah - zol
              else
                 psih0 = -betah * zol
              endif

              if (zntol > 1.0) then
                 psih = psih0 - (1.0 - betah - zntol)
              else
                 psih = psih0 + betah * zntol
              endif
           else
              tmp1 = 1.0 + sqrt(1.0 - gamah*zol  )
              tmp2 = 1.0 + sqrt(1.0 - gamah*zntol)
              psih = 2.0 * alog( tmp1 / tmp2 ) 
           endif

           gz1oz0 = ALOG( z1 / z0 )
           xradyn(c,r) = pro * (gz1oz0 - psih) / (vkar * xustar(c,r))

         !=======================================================================
         !--- End of modification
         !=======================================================================

        endif

        ! Calculate the relative humidity.

        IF ( xtemp2(c,r) <= stdtemp ) THEN
          es = vp0 * EXP( 22.514 - (6.15e3 / xtemp2(c,r)) )
        ELSE        
          es = vp0 * EXP( svp2 * (xtemp2(c,r) - stdtemp) /  &
                          (xtemp2(c,r) - svp3) )
        ENDIF

        qss = es * 0.622 / (xprsfc(c,r) - es)
        rh  = 100.0 * xwvapor(c,r,1) / qss

        ddloop: DO l = 1, ltotg

          IF ( l == lso4 ) THEN  ! Sulfate (SULF)

            ! Sulfate calculation follows Wesely (1985), Eqn. 11.

            rbsulf = 1.0 / (0.002*(xustar(c,r)**2 + 0.24*xwstar(c,r)**2) /   &
                     xustar(c,r))
            xvd(c,r,l) = 1.0 / (xradyn(c,r) + rbsulf)

          ELSE

            ! Assign a pH for rain water based on longitude if US simulation.
            ! Otherwise use default pH.

            IF ( ( map%cenlat >=   30.0 ) .AND. ( map%cenlat <=  45.0 ) .AND.  &
                 ( map%cenlon >= -120.0 ) .AND. ( map%cenlon <= -70.0 ) ) THEN
              IF ( xlonc(c,r) >  -100.0 ) THEN
                rnph = 4.5
              ELSE
                rnph = 5.5
              ENDIF
            ELSE
              rnph = rnph0
            ENDIF

            ! Assign diffusivity from table.

            diffu = dif0(l)

            ! Though the veg type is USGS, water=>16, the input Eta data reserve the 16
            ! and still use "0" as water surface.
            ! in order to compensate the change and in caution, set "0" and "16" valid

            IF ( NINT(xdluse(c,r))==lwater .or. NINT(xdluse(c,r))==lwater2 ) THEN  ! water

              ! Use CMAQ function for calculating the effective Henry's Law
              ! constant.  Note that original M3DRY was inverse, non-dimensional
              ! Henry's Law (caq/cg).   Water pH is different than rain, and
              ! we need to use the water temperature.

!             hplus = 10.0**(-8.1)
!             heff  = hlconst(subname(l), xtempg(c,r), effective, hplus)
              heff  = hlconst(subname(l), xtempg(c,r), effective, hplus_const)

              ! Make Henry's Law constant non-dimensional.

              heff  = heff * 0.08205 * xtempg(c,r)

              rgw   = 1.0 / (heff * d3 * xustar(c,r))
              rsurf = rgw

            ELSE

              ! Use CMAQ function for calculating the effective Henry's Law
              ! constant.  Note that original M3DRY was inverse, non-dimensional
              ! Henry's Law (caq/cg).

              hplus = 10.0**(-rnph)
              heff  = hlconst(subname(l), xtempm(c,r,1), effective, hplus)

              ! Make Henry's Law constant non-dimensional.

              heff  = heff * 0.08205 * xtempm(c,r,1)

              ! Wet surface resistance.  (Note DELTA = CWC in ADOM lingo.)
              ! This now applies to cuticle and ground.

              IF ( xwr(c,r) <= 0.0 ) THEN
                delta = 0.0
              ELSE
                delta = SQRT(xwr(c,r) / wrmax)  ! refer to Eta
                delta = MIN( 1.0, delta )
              ENDIF

              rwet = rcw0 / heff


              ! Dry snow resistance.

              rsnow = rsnow0 * a0 / a(l)


              ! If the surface is cold and wet use dry snow.

              IF ( xtemp2(c,r) < stdtemp ) THEN
                rwetsfc = rsnow
              ELSE
                rwetsfc = rwet
              ENDIF


              ! Dry cuticle resistance.

              IF ( l == lnh3 ) THEN
                rcut = 4000.0 * EXP(-0.054*rh)
              ELSE
                rcut = rcut0 * a0 / a(l)
              ENDIF

              ! Dry ground resistance.  (revised according to Erisman)

              hcan  = xzruf(c,r) * 10.0
              rinc  = 14.0 * xlai(c,r) * hcan / xustar(c,r)
              rgnd  = rg0 * a0 / a(l)
              rgndc = rgnd + rinc       ! Add in-canopy part


              ! Determine the snow liquid water mass fraction (0.0 to 0.5).

              xm = 0.02 * (xtemp2(c,r) - (stdtemp - 1.0))**2
              xm = MIN (xm, 0.5)
              xm = MAX (xm, 0.0)
              IF ( xtemp2(c,r) < (stdtemp - 1.0) ) xm = 0.0

             ! ifsnow = NINT(xsnocov(c,r))
	      ifsnow = NINT(xsnowc(c,r))   !add by lipan
              ifsnow = MAX(0, ifsnow)


              ! Bulk stomatal resistance.

              rstom = xrstom(c,r) * dwat / diffu + meso( l ) / xlai( c,r )
            ! rstom = xrstom(c,r) * dwat / diffu   ! original code

              ! Bulk surface resistance.

              rci = xveg(c,r) * (1.0/rstom + (1.0-delta) * xlai(c,r) / rcut +  &
                    ( delta * xlai(c,r) / rwetsfc ) + (1.0/rgndc)) +           &
                    (1-ifsnow) * ( (1.0 - xveg(c,r)) * ( (1.0-delta) /         &
                    rgnd + delta / rwetsfc ) ) +                               &
                    ifsnow * ( (1.0 - xm) / rsnow + xm / (rsndiff + rwet) )

              rsurf = 1.0 / rci

            ENDIF

            ! Aerodynamic and deposition layer resistances.
            ! The RA read in from MM5PX includes ra + rb for water vapor.
            ! Therefore, RA for chemical species is RAC = RA-RBW+RBC
            ! where RBW is rb for water and RBC is rb for the chemical.

         !  scw = kvis / dwat
            scc = kvis / diffu

         !  rbw = 5.0 / xustar(c,r) * (scw/pr)**0.66667
            rbw = 5.0 / xustar(c,r) * scw_pr_const

            rbc = 5.0 / xustar(c,r) * (scc/pr)**0.66667
            rac = xradyn(c,r) + rbc

            ! Store boundary resistance for water for file output.

            xrbndy(c,r) = rbw

            ! Dry deposition velocity.

            xvd(c,r,l) = 1.0 / (rsurf + rac)

          ENDIF

        ENDDO ddloop

      ENDDO
    ENDDO
!$omp end parallel do

  ENDIF

END SUBROUTINE etadry
