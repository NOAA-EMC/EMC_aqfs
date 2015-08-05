SUBROUTINE metvars2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables to CTM (X) Grid
! Purpose:  Puts time-variant meteorology arrays on CTM (X) grid.
! Notes:    -- Vegetation information does not include seasonal changes.
!           -- Liberally adapted from EPA's MCIP2.
! Revised:  23 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           17 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           26 Nov 2003  Modified for Kh.  Modified for 24-category
!                        USGS land use in Eta.  (T. Otte)
!           04 Dec 2003  Modified for GFS Ozone and tropopause height.
!                        Changed comments associated with surface exchange
!                        coefficient to note that it is not coupled with
!                        density.  (T. Otte)
!           26 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           30 Apr 2004  Added definitions for molecular weight of ozone
!                        that were part of the now-defunct CHEMMECH_MOD.
!                        Combined XTEMP1P5 and XTEMP10 as XTEMP2.  Added
!                        XWSPD10 and XWDIR10.  (T. Otte)
!           01 Jan 2005  Optimized the code by using F90 array range indexing
!                        rather than nested loop
!           28 JAN 2005  Added XALBEDO.  (Hsin-mu Lin)
!           02 FEB 2005  Added 27 USGS-WRF VEGETATION/SURFACE TYPE AND MODIFIED
!                        THE DECISION FOR USING WHETHER 13 OR 27 (24) VEGE TYPE
!                                                                 (Hsin-mu Lin)
!           17 Apr 2006  -- Corrected the range of the first two dimensions of
!                           kth when it assigns to xeddyv.
!                        -- Reduced the value of ec and er by 1 so the correct
!                           range of u_grd, and v_grd are assigned to xuu and
!                           xvv, respectively. (David Wong)
!           22 Mar 2008  TKE based PBL is  no longer used by NCEP
!                        Assign mixed layer depth to replace the TKE based 
!                        PBL for "xpbl"                        (Hsin-mu Lin)
!-------------------------------------------------------------------------------
!
! CLASS USGS-WRF VEGETATION/SURFACE TYPE
!
!   1   Urban and Built-Up Land
!   2   Dryland Cropland and Pasture
!   3   Irrigated Cropland and Pasture
!   4   Mixed Dryland/Irrigated Cropland and Pasture
!   5   Cropland/Grassland Mosaic
!   6   Cropland/Woodland Mosaic
!   7   Grassland
!   8   Shrubland
!   9   Mixed Shrubland/Grassland
!  10   Savanna
!  11   Deciduous Broadleaf Forest
!  12   Deciduous Needleleaf Forest
!  13   Evergreen Broadleaf Forest
!  14   Evergreen Needleleaf Forest
!  15   Mixed Forest
!  16   Water Bodies
!  17   Herbaceous Wetland
!  18   Wooded Wetland
!  19   Barren or Sparsely Vegetated
!  20   Herbaceous Tundra
!  21   Wooded Tundra
!  22   Mixed Tundra
!  23   Bare Ground Tundra
!  24   Snow or Ice
!  25   Playa
!  26   Lava
!  27   White Sand
!
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE metvars
  USE metinfo
  USE const
  USE const_pbl
  USE parms3, ONLY: badval3
  USE gridinfo

  IMPLICIT NONE

  INTEGER                      :: maxvgt
  INTEGER                      :: col
  INTEGER                      :: i
  INTEGER                      :: ii
  INTEGER                      :: iil
  INTEGER                      :: ilu
  INTEGER                      :: jj
  INTEGER                      :: jjl
  INTEGER                      :: k
  INTEGER                      :: kp1
  REAL                         :: lai13     ( 13 )
  REAL                         :: lai24     ( 24 )
  REAL                         :: lai27     ( 27 )
  INTEGER                      :: lbndmet
  REAL,          PARAMETER     :: mwo3      = 48.0  ! (oxygen = 16.0) * 3.0
  REAL,          PARAMETER     :: mwaovo3   = mwair / mwo3
  REAL                         :: presf
  INTEGER                      :: row
  INTEGER                      :: ubndx
  REAL                         :: z0_13     ( 13 )
  REAL                         :: z0_24     ( 24 )
  REAL                         :: z0_27     ( 27 )
  REAL                         :: p1, p2

  INTEGER                      :: igbpv (21), idx

  integer :: sc, ec, sr, er, r, c

  EXTERNAL       ENVYN

  LOGICAL        ENVYN, MIXHT_4_PBL, IGBP_4_USGS
  INTEGER        ISTAT


  ! Leaf-area index and roughness length based on Eta model values in SFLX.F.

  DATA (lai13(i),i=1,13)   /   4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0   /

  DATA (z0_13(i),i=1,13)   /   2.653,  0.826,  0.563,  1.089,  0.854,  0.856, &
                               0.035,  0.238,  0.065,  0.076,  0.011,  0.035, &
                               0.011 /

  ! Leaf-area index and roughness length based on NOAH model values in REDPRM.f.
  ! Note:  LAI24(16) changed to 0.0 (water).  TLO 25 Nov 2003
  ! Note:  z0_24(16)=0.001        ! correct the previous 0.01  (HML 01 Feb 2005)

  DATA (lai24(i),i=1,24)   /   4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    0.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    4.0,    4.0,    4.0    /

  DATA (z0_24(i),i=1,24)   /   1.000,  0.070,  0.070,  0.070,  0.070,  0.150, &
                               0.035,  0.030,  0.050,  0.860,  0.800,  0.850, &
                               2.650,  1.090,  0.800,  0.001,  0.040,  0.500, &
                             ! 2.650,  1.090,  0.800,  0.010,  0.040,  0.500, &
                               0.010,  0.040,  0.060,  0.050,  0.030,  0.010  /

  DATA (lai27(i),i=1,27)   /   4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    0.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0,    4.0,    4.0,    4.0,   &
                               4.0,    4.0,    4.0     /

  DATA (z0_27(i),i=1,27)   /   1.00,   0.07,   0.07,   0.07,   0.07,   0.15,  &
                               0.08,   0.03,   0.05,   0.86,   0.80,   0.85,  &
                               2.65,   1.09,   0.80,   0.001,  0.04,   0.05,  &
                               0.01,   0.04,   0.06,   0.05,   0.03,   0.001, &
                               0.01,   0.15,   0.01    /

!-------------------------------------------------------------------------------

  ! Transfer vegetation in IGBP to USGS
  ! the order is from igbp=0 and the related # is USGS value.
  !  *** begin from igbp=0 that maps to USGS=16          (11/17/2011, Formosian)

  DATA (igbpv(i),i=1,21)   /   16,                                            &
                               14,  13,  12,  11,  15,  8,  8,  10,  10,  7,  &
                               17,   2,   1 ,  5,  24, 19, 16,  21,  22, 23   /

!-------------------------------------------------------------------------------
! Put time-variant cross-point arrays on PREMAQ grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtempg (:,:) = skint(sc:ec,sr:er)
  xtemp2 (:,:) = temp_2m(sc:ec,sr:er)
  xrainc (:,:) = acpcp(sc:ec,sr:er)  ! rain is already incremental here
  xrainn (:,:) = ncpcp(sc:ec,sr:er)  ! rain is already incremental here

 !===============================================
 ! mixed layer depth instead of TKE based PBL?
 !===============================================

  MIXHT_4_PBL = ENVYN ('MIXHT_4_PBL', 'MIXHT 4 PBL ?', .FALSE. , ISTAT)

  IF ( MIXHT_4_PBL ) THEN
     xpbl  (:,:) = hmixht(sc:ec,sr:er)
  ELSE
     xpbl  (:,:) = hpbl (sc:ec,sr:er)
  ENDIF


  xpblr  (:,:) = hpblr (sc:ec,sr:er)
  xmixht (:,:) = hmixht(sc:ec,sr:er)

 !===============================================
 ! According to the sign change in NCEP, 03/2006
 !===============================================

  xhfx   (:,:) = shtfl (sc:ec,sr:er)
  xqfx   (:,:) = lhtfl (sc:ec,sr:er)
  ! xhfx   (:,:) = shtfl (sc:ec,sr:er) * (-1.0)  ! upward to downward
  ! xqfx   (:,:) = lhtfl (sc:ec,sr:er) * (-1.0)  ! upward to downward

 !===============================================

  xustar (:,:) = max ( fricv (sc:ec,sr:er), 0.001)
  xustar2(:,:) = max ( fricv (sc:ec,sr:er), 0.01 )
  xglw   (:,:) = ulwrf (sc:ec,sr:er)
  xgsw   (:,:) = dswrf (sc:ec,sr:er) - uswrf (sc:ec,sr:er)
  xrgrnd (:,:) = dswrf (sc:ec,sr:er)

  xalbedo(:,:) = albdo (sc:ec,sr:er) * 0.01  ! convert percent to decimal
  xcsusf (:,:) = csusf (sc:ec,sr:er)
  xcsdsf (:,:) = csdsf (sc:ec,sr:er)

  xpsccb (:,:) = psccb (sc:ec,sr:er)
  xpscct (:,:) = pscct (sc:ec,sr:er)
  xpdccb (:,:) = pdccb (sc:ec,sr:er)
  xpdcct (:,:) = pdcct (sc:ec,sr:er)
  xptccb (:,:) = ptccb (sc:ec,sr:er)
  xptcct (:,:) = ptcct (sc:ec,sr:er)

  xpdtop (:,:) = pdtop (sc:ec,sr:er)
  xpdbot (:,:) = pdbot (sc:ec,sr:er)

  xveg   (:,:) = veg   (sc:ec,sr:er) * 0.01  ! convert percent to decimal
  xch    (:,:) = sfexc (sc:ec,sr:er)
  xwr    (:,:) = cnwat (sc:ec,sr:er)

  xsotyp (:,:) = sotyp (sc:ec,sr:er)
  xsoilw (:,:) = soilw (sc:ec,sr:er)

  xsnowc (:,:) = snowc (sc:ec,sr:er) * 0.01  ! convert percent to decimal

  xrstom (:,:) = 1.0 / ccond (sc:ec,sr:er)   ! conductance to resistance
  xtrop  (:,:) = trop  (sc:ec,sr:er)

  xprsfc (:,:) = psfc(sc:ec,sr:er)

  xwwind (:,:,0) = 0.0
! xeddyv (:,:,0) = kth(:,:,1)
  xeddyv (:,:,0) = kth(sc:ec,sr:er,1)

  xwvapor(:,:,:) = spc_h(sc:ec,sr:er,:)
  xcldwtr(:,:,:) = clwmr(sc:ec,sr:er,:)
  xranwtr(:,:,:) = rwmr (sc:ec,sr:er,:)
  xqice  (:,:,:) = c_ice(sc:ec,sr:er,:)
  xqsnow (:,:,:) = snmr (sc:ec,sr:er,:)

  xpres3d(:,:,:) = pres3d (sc:ec,sr:er,:)    ! hybrid level pressure
  xgeohgt(:,:,:) = geohgt (sc:ec,sr:er,:)

  xo3    (:,:,:) = o3mr (sc:ec,sr:er,:) * 1.0e6 * mwaovo3  ! kg/kg -> ppmV

  xtempm (:,:,:) = tmp   (sc:ec,sr:er,1:metlay)

!-------------------------------------------------------------------------------
! mapping the IGBP vegetation to USGS type
!  *** the defaults in IGBP                  (11/17/2011, Hsin-mu Lin)
!-------------------------------------------------------------------------------

  IGBP_4_USGS = ENVYN ('IGBP_4_USGS', 'IGBP TO USGS ?', .TRUE. , ISTAT)

  IF ( IGBP_4_USGS ) THEN
     do c = 1, nrows_x
        do r = 1, nrows_x
           idx = vgtyp (c,r)
           vgtyp (c,r) = igbpv (idx+1)     ! igbp begin from "0"
        enddo
     enddo
  ENDIF

!-------------------------------------------------------------------------------
!      when (hybrid lvl P)=(mid lvl P), use "pres3d" instead
!
!  *** must be consistent with "getmet..F"
!-------------------------------------------------------------------------------

  ! xpresm (:,:,:) = pres3d (sc:ec,sr:er,1:metlay)    ! hybrid level pressure
  xpresm (:,:,:) = pres (sc:ec,sr:er,1:metlay)     ! original

  ! xdensam (:,:,:) = xpresm (:,:,:) / ( rdgas * xtempm(:,:,:) )      ! in met3dsup

  ! Define pressure and density at surface.  Recall k=1 at surface here.

  ! xdensaf (:,:,0) = xprsfc (:,:) / ( rdgas * tmp(sc:ec,sr:er,1) )   ! in met3dsup

  maxvgt = MAXVAL(vgtyp)                     ! # of VEG type

  c = x0 - 1
  r = y0 - 1

  DO row = 1, nrows_x

    jj = r + row

    DO col = 1, ncols_x

      ii = c + col

      ! Define 2D arrays.


      CALL wind (u10m(ii,jj), v10m(ii,jj), xwspd10(col,row), xwdir10(col,row), &
                 xlonc(col,row), map%lov, map%xn)


      ilu = vgtyp(ii,jj)

     ! IF ( lwater == 0 ) THEN  ! original Eta vegetation type

      IF ( maxvgt <=13 ) THEN  ! original Eta vegetation typea

         IF ( ilu > 0 ) THEN  ! land
            xlai  (col,row) = lai13(ilu)
            xzruf (col,row) = z0_13(ilu)
         ELSE  ! water
            xlai  (col,row) = 0.0
            xzruf (col,row) = 0.001  ! from Eta's INIT.F value for Z0SEA
         ENDIF

     ! ELSE  ! USGS 24-category land use
     !    IF ( ilu > 0 ) THEN    ! "16" is water
     !       xlai  (col,row) = lai24(ilu)
     !       xzruf (col,row) = z0_24(ilu)
     !    ELSE                   ! water, inputs reserve "16" and use "0" instead
     !       xlai  (col,row) = lai24(16)
     !       xzruf (col,row) = z0_24(16)
     !    ENDIF

      ELSE  ! USGS-WRF 27-category land use
         IF ( ilu > 0 ) THEN    ! "16" is water
            xlai  (col,row) = lai27(ilu)
            xzruf (col,row) = z0_27(ilu)
         ELSE                   ! water, inputs reserve "16" and use "0" instead
            xlai  (col,row) = lai27(16)
            xzruf (col,row) = z0_27(16)
         ENDIF
 
      ENDIF

      ! Define 3D arrays.

      kloop: DO k = 1, metlay

        kp1 = MIN (k+1, metlay)

        xwwind(col,row,k) = (-0.5) * (v_vel(ii,jj,k) + v_vel(ii,jj,kp1))

 !       xtempm(col,row,k) = tmp  (ii,jj,k)
 !       xpresm(col,row,k) = pres (ii,jj,k)

 !       if ( k < neta2 ) then
 !          p1 = xpdtop(col,row)
 !          p2 = xpdbot(col,row)
 !       else
 !          p1 = 0.
 !          p2 = xpdtop(col,row)
 !       endif

 !       presf = p1 + p2 * ( 1.0 - xx3face(k) ) + x3top

 !       xdensam(col,row,k) = xpresm(col,row,k) / ( rdgas * xtempm(col,row,k) )   ! in met3dsup
 !       xdensaf(col,row,k) = 2.0 * presf /   &                                   ! in met3dsup
 !                            (rdgas * (xtempm(col,row,k) + xtempm(col,row,kp1)))

        xeddyv (col,row,k) = MAX( kth(ii,jj,k+1), kthmin )

      ENDDO kloop

    ENDDO

  ENDDO

 !  xdenss(:,:) = xdensaf(:,:,0)      ! in met3dsup

!-------------------------------------------------------------------------------
! Put time-variant dot-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  ec = ec - 1
  er = er - 1

  xuu(:,:,:) = u_grd(sc:ec,sr:er,:)
  xvv(:,:,:) = v_grd(sc:ec,sr:er,:)

END SUBROUTINE metvars2ctm
