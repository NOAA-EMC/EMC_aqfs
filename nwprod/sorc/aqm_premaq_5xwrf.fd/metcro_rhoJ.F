SUBROUTINE metcro (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Meteorology on Cross Points
! Purpose:  Compute and output time-dependent, cross-point parameters.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           27 Mar 2003  Corrected calculation of WGHT_TOP and WGHT_BOT that
!                        is used to get WHAT_JD_C and WHAT_JD_B on full levels.
!                        Removed extraneous calculation of X3MIDL.  Changed
!                        calls to COLLAPX to be on Jacobian coupled with
!                        density instead of just Jacobian for better mass
!                        conservation reasons.  Changed subsequent calculations
!                        of output variables based on Jacobian.  Removed
!                        JDRATE.  (J. Pleim and T. Otte)
!           14 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           22 Oct 2003  Removed fields associated with METBDY2D.  
!                        Modified for XEDDYV on full levels.  (T. Otte)
!           25 Nov 2003  Added JACOBF and JACOBS.  (T. Otte)
!           04 Dec 2003  Added TROP.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine.  Combined TEM1P5 and TEMP10
!                        as TEMP2.  Added WSPD10 and WDIR10 to output. (T. Otte)
!           14 JAN 2005  Added ATTEN, ATTENX, CSRAD, CSUSF, CSDSF AND CSWTOA
!                                                                  (Hsin-mu Lin)
!           10 Feb 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!           25 Apr 2006  Changed calculations of XRHOJM and XRHOJF to use
!                        Jacobian formula for WRF-NMM.  (T. Otte)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE coord
  USE fdesc3
  USE file
  USE iodecl3
  USE mcoutcom
  USE vgrd
  USE parms3
  USE const

  IMPLICIT NONE

  LOGICAL,       SAVE          :: first       = .TRUE.

  CHARACTER*2                  :: str1, str2
  CHARACTER*16                 :: spcname
  CHARACTER*63                 :: ifmt1
  CHARACTER*16,  PARAMETER     :: pname       = 'METCRO'

  INTEGER                      :: c, col, i, j, k, k1, k2, l, n, r
  INTEGER                      :: ilg
  INTEGER                      :: index, t_index, len1, len2
  INTEGER                      :: jdate
  INTEGER                      :: lvl
  INTEGER                      :: row
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime

  REAL,    SAVE, ALLOCATABLE   :: dumaray0    ( : , : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  REAL,          PARAMETER     :: epsilon     = 1.0e-30
  REAL,          PARAMETER     :: twooverg    = 2.0 / grav
  REAL,    SAVE, ALLOCATABLE   :: wght_bot    ( : )
  REAL,    SAVE, ALLOCATABLE   :: wght_top    ( : )
  REAL                         :: x3jfmin
  REAL                         :: x3jmmin
  REAL                         :: xdnamin
  REAL                         :: xdnjmin
  REAL                         :: xmapmin
  REAL,    SAVE, ALLOCATABLE   :: xrhojf      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: xrhojm      ( : , : , : )
  REAL                         :: onerhojm

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,          INTENT(INOUT) :: aa         ( : , : , : )
      REAL,          INTENT(IN)    :: vertin     ( : )
      REAL,          INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( xrhojf ) )  &
    ALLOCATE ( xrhojf ( ncols_x, nrows_x, 0:metlay ) )
  IF ( .NOT. ALLOCATED ( xrhojm ) )  &
    ALLOCATE ( xrhojm ( ncols_x, nrows_x,   metlay ) )

!-------------------------------------------------------------------------------
! If first time, build headers for files.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( wght_top (nlays) )
    ALLOCATE ( wght_bot (nlays) )

    ! Build common header.

    CALL comheader (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO2D file.
    !---------------------------------------------------------------------------

    DO index = 1, mc2index
      vtype3d(index) = m3real
      vname3d(index) = mc2vname(index)
      units3d(index) = mc2units(index)
      vdesc3d(index) = mc2vdesc(index)
    ENDDO

    ! Set up header for deposition velocity arrays.

    DO ilg = 1, ltotg
      index = mc2index + ilg
      vtype3d(index) = m3real
      spcname        = TRIM(xdepspc(ilg))
      vname3d(index) = 'VD_' // spcname
      units3d(index) = 'M/S'
      vdesc3d(index) = 'deposition velocity for species ' // spcname
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = index
    nlays3d = 1
    ncols3d = ncols
    nrows3d = nrows
    tstep3d = grstep

    IF ( .NOT. open3 (metcro2d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metcro2d_g1)
      GOTO 1001
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO3D file.
    !---------------------------------------------------------------------------

    DO index = 1, mc3index
      vtype3d(index) = m3real
      vname3d(index) = mc3vname(index)
      units3d(index) = mc3units(index)
      vdesc3d(index) = mc3vdesc(index)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows

    ftype3d = grdded3
    nvars3d = mc3index
    nlays3d = nlays

    IF ( .NOT. open3 (metcro3d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metcro3d_g1)
      GOTO 1001
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METBDY3D file.
    ! Header information is the same as METCRO3D except for file type.
    !---------------------------------------------------------------------------

    ftype3d = bndary3
    nvars3d = mc3index
    nlays3d = nlays

    IF ( .NOT. open3 (metbdy3d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metbdy3d_g1)
      GOTO 1001
    ENDIF

    ! Compute linear interpolation coefficients based on layer thickness.

    DO lvl = 1, nlays-1
      wght_top(lvl) = ( x3face_gd(lvl)   - x3midl(lvl) )  &
                      / ( x3midl (lvl+1) - x3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_2D (time dependent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)
  x3jfmin = MINVAL(x3jacobf)

!$omp parallel do
  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik

      prsfc_c(col,row) = xprsfc(c,r)

      IF ( ( x3jfmin < amiss3 ) .OR.  &
           ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
        jacobs_c (col,row) = badval3
      ELSE
        jacobs_c (col,row) = x3jacobf(c,r,0) / xmapc(c,r)**2  ! sfc total Jacob
      ENDIF

      ustar_c(col,row) = xustar(c,r)
      wstar_c(col,row) = xwstar(c,r)

      pbl_c  (col,row) = xpbl  (c,r)
      pbl2_c (col,row) = xpbl2 (c,r)
      pblr_c (col,row) = xpblr (c,r)
      mixht_c(col,row) = xmixht(c,r)

      zzero_c(col,row) = xzruf (c,r)

      IF ( xmol(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        moli_c(col,row) = 0.0
      ELSE
        moli_c(col,row) = 1.0 / xmol(c,r)
      ENDIF

      hfx_c(col,row) = xhfx(c,r)
      qfx_c(col,row) = xqfx(c,r)

      IF ( xradyn(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        radyni_c(col,row) = 0.0
      ELSE
        radyni_c(col,row) = 1.0 / xradyn(c,r)
      ENDIF

      IF ( xrbndy(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        rbndyi_c(col,row) = 0.0
      ELSE
        rbndyi_c(col,row) = 1.0 / xrbndy(c,r)
      ENDIF

      IF ( xrstom(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        rstomi_c(col,row) = 0.0
      ELSE
        rstomi_c(col,row) = 1.0 / xrstom(c,r)
      ENDIF

      tempg_c  (col,row) = xtempg  (c,r)
      temp2_c  (col,row) = xtemp2  (c,r)
      wspd10_c (col,row) = xwspd10 (c,r)
      wdir10_c (col,row) = xwdir10 (c,r)
      glw_c    (col,row) = xglw    (c,r)
      gsw_c    (col,row) = xgsw    (c,r)
      rgrnd_c  (col,row) = xrgrnd  (c,r)

      rainn_c  (col,row) = xrainn  (c,r)
      rainc_c  (col,row) = xrainc  (c,r)
      cfract_c (col,row) = xcfract (c,r)
      cldtop_c (col,row) = xcldtop (c,r) 
      cldbot_c (col,row) = xcldbot (c,r) 
      wbar_c   (col,row) = xwbar   (c,r) 
      trop_c   (col,row) = xtrop   (c,r)

      attenX_c (col,row) = xattenX (c,r)
      atten_c  (col,row) = xatten  (c,r)
      csrad_c  (col,row) = xcsrad  (c,r)
      cswtoa_c (col,row) = xcstoa  (c,r)
      csusf_c  (col,row) = xcsusf  (c,r)
      csdsf_c  (col,row) = xcsdsf  (c,r)

      psccb_c  (col,row) = xpsccb  (c,r)
      pscct_c  (col,row) = xpscct  (c,r)
      pdccb_c  (col,row) = xpdccb  (c,r)
      pdcct_c  (col,row) = xpdcct  (c,r)
      ptccb_c  (col,row) = xptccb  (c,r)
      ptcct_c  (col,row) = xptcct  (c,r)


      ! Put in deposition velocities

      DO ilg = 1, ltotg
        vd_c(col,row,ilg) = xvd(c,r,ilg)
      ENDDO

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Calculate density*Jacobian on mid-layers and full levels.  Use this for
! collapsing rather than Jacobian alone.  Here, use the Jacobian formula
! rather than simply rho*J to ensure consistency through the column.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, neta2
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojm(c,r,k) = twooverg * xpdbot(c,r)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!$omp parallel do
  DO k = 0, neta2-1
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojf(c,r,k) = twooverg * xpdbot(c,r)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!$omp parallel do
  DO r = 1, nrows_x
    DO c = 1, ncols_x

        ! At sigma-P interface, J is complicated...just use rho*J
        ! based on J calculated in met3dsup.F.
        xrhojf(c,r,neta2) = xdensaf(c,r,neta2) * x3jacobf(c,r,neta2)

    ENDDO
  ENDDO
!$omp end parallel do

!$omp parallel do
  DO k = neta2+1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojm(c,r,k) = twooverg * xpdtop(c,r)
        xrhojf(c,r,k) = twooverg * xpdtop(c,r)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Add factor to potential vorticity. Check subroutine "pvorticity" for detail
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        onerhojm = 1.0 / xrhojm(c,r,k)
        xpvc(c,r,k) = onerhojm * xpvc(c,r,k)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays.  (Original values are replaced.)
! For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray0 ) )  &
      ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay,  7 ) )
    IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
      ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 11 ) )

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray1(c,r,k, 1) = xrhojm  (c,r,k)
          dumaray1(c,r,k, 2) = xdensam (c,r,k)
          dumaray1(c,r,k, 3) = xpresm  (c,r,k)
          dumaray1(c,r,k, 4) = xcldwtr (c,r,k)
          dumaray1(c,r,k, 5) = xranwtr (c,r,k)
          dumaray1(c,r,k, 6) = xqice   (c,r,k)
          dumaray1(c,r,k, 7) = xqsnow  (c,r,k)
          dumaray1(c,r,k, 8) = xtempm  (c,r,k)
          dumaray1(c,r,k, 9) = xwvapor (c,r,k)
          dumaray1(c,r,k,10) = x3htm   (c,r,k)
          dumaray1(c,r,k,11) = xpvc    (c,r,k)

        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

!$omp parallel do
    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray0(c,r,k,1) = xrhojf  (c,r,k)
          dumaray0(c,r,k,2) = xwhat   (c,r,k)
          dumaray0(c,r,k,3) = xwwind  (c,r,k)
          dumaray0(c,r,k,4) = x3htf   (c,r,k)
          dumaray0(c,r,k,5) = xeddyv  (c,r,k)
          dumaray0(c,r,k,6) = xdensaf (c,r,k)
          dumaray0(c,r,k,7) = xpresf  (c,r,k)

        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

    CALL collapx (xrhojm,  xx3midl, x3midl)
    CALL collapx (xdensam, xx3midl, x3midl)
    CALL collapx (xpresm,  xx3midl, x3midl)
    CALL collapx (xcldwtr, xx3midl, x3midl)  
    CALL collapx (xranwtr, xx3midl, x3midl)
    CALL collapx (xqice,   xx3midl, x3midl)
    CALL collapx (xqsnow,  xx3midl, x3midl)
    CALL collapx (xtempm,  xx3midl, x3midl)
    CALL collapx (xwvapor, xx3midl, x3midl)
    CALL collapx (x3htm,   xx3midl, x3midl)
    CALL collapx (xpvc,    xx3midl, x3midl)

    CALL collapx (xrhojf,  xx3face, x3face)
    CALL collapx (xwhat,   xx3face, x3face)  
    CALL collapx (xwwind,  xx3face, x3face)
    CALL collapx (x3htf,   xx3face, x3face)
    CALL collapx (xeddyv,  xx3face, x3face)
    CALL collapx (xdensaf, xx3face, x3face)
    CALL collapx (xpresf,  xx3face, x3face)
 
  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_3D (time dependent).
!-------------------------------------------------------------------------------

  x3jmmin = MINVAL(x3jacobm)
  xdnamin = MINVAL(xdensam)

!$omp parallel do
  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_c (col,row,lvl) = badval3
        ELSE
          jacobf_c (col,row,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /   &
                                   xmapc(c,r)**2
        ENDIF


        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_c (col,row,lvl) = badval3
        ELSE
          jacobm_c (col,row,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /   &
                                   xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_c(col,row,lvl) = badval3
        ELSE
          densa_j_c(col,row,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Used for cloud and AQCHEM

        cldwtr_c (col,row,lvl) = xcldwtr (c,r,lvl)
        ranwtr_c (col,row,lvl) = xranwtr (c,r,lvl)
        qice_c   (col,row,lvl) = xqice   (c,r,lvl)
        qsnow_c  (col,row,lvl) = xqsnow  (c,r,lvl)

        ! Meteorological variables.
          !! ---- mid-level, 1st lavel "lvl=1" ----

        tempa_c  (col,row,lvl) = xtempm  (c,r,lvl)
        wvapor_c (col,row,lvl) = xwvapor (c,r,lvl)
        press_c  (col,row,lvl) = xpresm  (c,r,lvl)  ! mid-level P
        densa_c  (col,row,lvl) = xdensam (c,r,lvl)
        wwind_c  (col,row,lvl) = xwwind  (c,r,lvl)
        x3htm_c  (col,row,lvl) = x3htm   (c,r,lvl)

          !! ---- full-level, 1st lavel "lvl=0" ----

        x3htf_c  (col,row,lvl) = x3htf   (c,r,lvl)  ! "0" at lvl=0
        presf_c  (col,row,lvl) = xpresf  (c,r,lvl)  ! P from lvl=1

        ! Vertical eddy diffusivity field.

        eddyv_c  (col,row,lvl) = xeddyv  (c,r,lvl)

        ! Vertical Potential vorticity

        pvc_c    (col,row,lvl) = xpvc    (c,r,lvl)

      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

  ! Added for mass consistency

  xdnjmin = MINVAL(densa_j_c)

  IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

    what_jd_c(:,:,:) = badval3

  ELSE

!$omp parallel do
    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nlays-1

          what_jd_c(col,row,lvl) = xwhat(c,r,lvl) *                           &
                                   ( wght_bot(lvl) * densa_j_c(col,row,lvl)   & 
                                   + wght_top(lvl) * densa_j_c(col,row,lvl+1) )
        ENDDO

        what_jd_c(col,row,nlays) = 0.0

      ENDDO
    ENDDO
!$omp end parallel do

  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in MET_BDY_3D (time dependent).
!-------------------------------------------------------------------------------

  index = 0
  len1 = ncols_x - nthik

!$omp parallel do
  DO r = 1, nthikd
    DO c = 1 + nthik, ncols_x
!     index = index + 1
      index = (r - 1) * len1 + c - nthik
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (index,lvl) = badval3
        ELSE
          jacobf_b (index,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (index,lvl) = badval3
        ELSE
          jacobm_b (index,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(index,lvl) = badval3
        ELSE
          densa_j_b(index,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Used for cloud and AQCHEM

        cldwtr_b (index,lvl) = xcldwtr (c,r,lvl)
        ranwtr_b (index,lvl) = xranwtr (c,r,lvl)
        qice_b   (index,lvl) = xqice   (c,r,lvl)
        qsnow_b  (index,lvl) = xqsnow  (c,r,lvl)

        ! Meteorological variables.

        tempa_b  (index,lvl) = xtempm  (c,r,lvl)
        wvapor_b (index,lvl) = xwvapor (c,r,lvl)
        press_b  (index,lvl) = xpresm  (c,r,lvl)
        densa_b  (index,lvl) = xdensam (c,r,lvl)
        wwind_b  (index,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (index,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (index,lvl) = x3htm   (c,r,lvl)
        pvc_b    (index,lvl) = xpvc    (c,r,lvl)
        presf_b  (index,lvl) = xpresf  (c,r,lvl)

        ! Vertical eddy diffusivity field.

        eddyv_b  (index,lvl) = xeddyv  (c,r,lvl)

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(index,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(index,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(index,lvl)   &
                                 + wght_top(lvl) * densa_j_b(index,lvl+1) )
        ENDDO

        what_jd_b(index,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
!$omp end parallel do

  t_index = len1 * nthikd

  ! Side I: add one more index at the end
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1
  IF ( nthik == 0 ) t_index = t_index + 1

  len1 = nthikd
  len2 = 1 + nthik

!$omp parallel do
  DO r = 1+nthik, nrows_x
    DO l = 1, nthikd
      c = ncols_x - nthikd + l
!     index = index + 1
      index = (r - len2) * len1 + l + t_index
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (index,lvl) = badval3
        ELSE
          jacobf_b (index,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (index,lvl) = badval3
        ELSE
          jacobm_b (index,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(index,lvl) = badval3
        ELSE
          densa_j_b(index,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Used for cloud and AQCHEM

        cldwtr_b (index,lvl) = xcldwtr (c,r,lvl)
        ranwtr_b (index,lvl) = xranwtr (c,r,lvl)
        qice_b   (index,lvl) = xqice   (c,r,lvl)
        qsnow_b  (index,lvl) = xqsnow  (c,r,lvl)

        ! Meteorological variables.

        tempa_b  (index,lvl) = xtempm  (c,r,lvl)
        wvapor_b (index,lvl) = xwvapor (c,r,lvl)
        press_b  (index,lvl) = xpresm  (c,r,lvl)
        densa_b  (index,lvl) = xdensam (c,r,lvl)
        wwind_b  (index,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (index,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (index,lvl) = x3htm   (c,r,lvl)
        pvc_b    (index,lvl) = xpvc    (c,r,lvl)
        presf_b  (index,lvl) = xpresf  (c,r,lvl)

        ! Vertical eddy diffusivity field.

        eddyv_b  (index,lvl) = xeddyv  (c,r,lvl)

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(index,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(index,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(index,lvl)   &
                                 + wght_top(lvl) * densa_j_b(index,lvl+1) )
        ENDDO

        what_jd_b(index,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
!$omp end parallel do

  t_index = len1 * (nrows_x - nthik) + t_index

  ! Side II: add one more index at the end
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1
  IF ( nthik == 0 ) t_index = t_index + 1
      
  ! side III add one more index before
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1
  IF ( nthik == 0 ) t_index = t_index + 1

  len1 = ncols_x - nthik

!$omp parallel do
  DO l = 1, nthikd
    r = nrows_x - nthikd + l
    DO c = 1, ncols_x - nthik

!     index = index + 1
      index = len1 * (l - 1) + c + t_index

      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (index,lvl) = badval3
        ELSE
          jacobf_b (index,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (index,lvl) = badval3
        ELSE
          jacobm_b (index,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(index,lvl) = badval3
        ELSE
          densa_j_b(index,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Used for cloud and AQCHEM

        cldwtr_b (index,lvl) = xcldwtr (c,r,lvl)
        ranwtr_b (index,lvl) = xranwtr (c,r,lvl)
        qice_b   (index,lvl) = xqice   (c,r,lvl)
        qsnow_b  (index,lvl) = xqsnow  (c,r,lvl)

        ! Meteorological variables.

        tempa_b  (index,lvl) = xtempm  (c,r,lvl)
        wvapor_b (index,lvl) = xwvapor (c,r,lvl)
        press_b  (index,lvl) = xpresm  (c,r,lvl)
        densa_b  (index,lvl) = xdensam (c,r,lvl)
        wwind_b  (index,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (index,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (index,lvl) = x3htm   (c,r,lvl)
        pvc_b    (index,lvl) = xpvc    (c,r,lvl)
        presf_b  (index,lvl) = xpresf  (c,r,lvl)

        ! Vertical eddy diffusivity field.

        eddyv_b  (index,lvl) = xeddyv  (c,r,lvl)

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(index,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(index,lvl) = xwhat(c,r,lvl) *               &
                        ( wght_bot(lvl) * densa_j_b(index,lvl)  & 
                        + wght_top(lvl) * densa_j_b(index,lvl+1) )
        ENDDO

        what_jd_b(index,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
!$omp end parallel do

  t_index = len1 * nthikd + t_index
       
  ! Side IV: add one more index before
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1
  IF ( nthik == 0 ) t_index = t_index + 1

  len1 = nthikd

!$omp parallel do
  DO r = 1, nrows_x - nthik
    DO c = 1, nthikd
!     index = index + 1
      index = len1 * (r - 1) + c + t_index
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (index,lvl) = badval3
        ELSE
          jacobf_b (index,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (index,lvl) = badval3
        ELSE
          jacobm_b (index,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(index,lvl) = badval3
        ELSE
          densa_j_b(index,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Used for cloud and AQCHEM

        cldwtr_b (index,lvl) = xcldwtr (c,r,lvl)
        ranwtr_b (index,lvl) = xranwtr (c,r,lvl)
        qice_b   (index,lvl) = xqice   (c,r,lvl)
        qsnow_b  (index,lvl) = xqsnow  (c,r,lvl)

        ! Meteorological variables.

        tempa_b  (index,lvl) = xtempm  (c,r,lvl)
        wvapor_b (index,lvl) = xwvapor (c,r,lvl)
        press_b  (index,lvl) = xpresm  (c,r,lvl)
        densa_b  (index,lvl) = xdensam (c,r,lvl)
        wwind_b  (index,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (index,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (index,lvl) = x3htm   (c,r,lvl)
        pvc_b    (index,lvl) = xpvc    (c,r,lvl)
        presf_b  (index,lvl) = xpresf  (c,r,lvl)

        ! Vertical eddy diffusivity field.

        eddyv_b  (index,lvl) = xeddyv  (c,r,lvl)

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(index,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(index,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(index,lvl)   &
                                 + wght_top(lvl) * densa_j_b(index,lvl+1) )
        ENDDO

        what_jd_b(index,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Write MET_CRO_2D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro2d_g1) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro2d_g1 // ' file', .TRUE.)
  ENDIF

  DO l = 1, mc2index
    IF ( .NOT. write3 (metcro2d_g1, vname3d(l), sdate, stime,  &
                       mc2(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro2d_g1)
      GOTO 1001
    ENDIF
  ENDDO

  DO l = mc2index+1, mc2index + ltotg
    IF (.NOT. write3 (metcro2d_g1, vname3d(l), sdate, stime,  &
                      mc2(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro2d_g1)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_CRO_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro3d_g1) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro3d_g1 // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mc3) < epsilon ) mc3 = 0.0

  IF (.NOT. write3 (metcro3d_g1, allvar3, sdate, stime, mc3) ) THEN
    WRITE (6,9100) TRIM(metcro3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Write MET_BDY_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metbdy3d_g1) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metbdy3d_g1 // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mb3) < epsilon ) mb3 = 0.0

  IF ( .NOT. write3 (metbdy3d_g1, allvar3, sdate, stime, mb3) ) THEN
    WRITE (6,9100) TRIM(metbdy3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojm  (c,r,k) = dumaray1(c,r,k, 1)
          xdensam (c,r,k) = dumaray1(c,r,k, 2)
          xpresm  (c,r,k) = dumaray1(c,r,k, 3)
          xcldwtr (c,r,k) = dumaray1(c,r,k, 4)
          xranwtr (c,r,k) = dumaray1(c,r,k, 5)
          xqice   (c,r,k) = dumaray1(c,r,k, 6)
          xqsnow  (c,r,k) = dumaray1(c,r,k, 7)
          xtempm  (c,r,k) = dumaray1(c,r,k, 8)
          xwvapor (c,r,k) = dumaray1(c,r,k, 9)
          x3htm   (c,r,k) = dumaray1(c,r,k,10)
          xpvc    (c,r,k) = dumaray1(c,r,k,11)
        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

!$omp parallel do
    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojf  (c,r,k) = dumaray0(c,r,k,1)
          xwhat   (c,r,k) = dumaray0(c,r,k,2) 
          xwwind  (c,r,k) = dumaray0(c,r,k,3) 
          x3htf   (c,r,k) = dumaray0(c,r,k,4) 
          xeddyv  (c,r,k) = dumaray0(c,r,k,5)
          xdensaf (c,r,k) = dumaray0(c,r,k,6)
          xpresf  (c,r,k) = dumaray0(c,r,k,7)
        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

!   DEALLOCATE ( dumaray0 )  ! commented out to avoid memory fragementation
!   DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragementation

  ENDIF

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nlays / 5
  k2 = MOD(nlays, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x,"   &
         &    // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4),/,10x," // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4))"
    ENDIF
  ENDIF

  WRITE (*,'(/,a,/)') '- METCRO: Printing sample cells in output grid'

  DO n = 1, mc2index
    WRITE (*,6000) TRIM(mc2vname(n)), mc2(lprt_col,lprt_row,n), mc2units(n)
  ENDDO

  DO n = 1, ltotg
    WRITE (*,6000) 'VD_' // TRIM(xdepspc(n)),  &
                   mc2(lprt_col,lprt_row,n+mc2index), 'm/s'
  ENDDO

  DO n = 1, mc3index
    WRITE (*,ifmt1) TRIM(mc3vname(n)), mc3(lprt_col,lprt_row,:,n)
  ENDDO

!-------------------------------------------------------------------------------
! Print extra sample output to log file for checking. This block can be deleted
!-------------------------------------------------------------------------------

 ! i = 150
 ! j = 150

 ! write (*,*) " "
 ! write (*,*) "====== ORG HEIGHT CHECK ", "i=", i, " j=", j, "======" 
 ! write (*,*) " "

 ! DO k = 1,nmlays
 !    write (*,*) "k=", k, " x3htf=", x3htf(i,j,k),  " x3htm=", x3htm(i,j,k) &
 !              , " xtopo=", xtopo(i,j), " xgeohgt=", xgeohgt(i,j,k)
 ! ENDDO

 ! write (*,*) " "

  !------

 ! write (*,*) "====== ORG PRES CHECK ", "i=", i, " j=", j, "======"
 ! write (*,*) " "

 ! DO k = 1,nmlays
 !    write (*,*) "k=", k, " P_wrf=", xpres3d(i,j,k), " P_cal=", xpresm(i,j,k)
 ! ENDDO

!-------------------------------------------------------------------------------
! Print extra sample output to log file for checking. This block can be deleted
!-------------------------------------------------------------------------------

 ! i = i-nthik            ! "c" array is inset of "x" array, c = col + nthik
 ! j = j-nthik            ! "c" array is inset of "x" array, r = row + nthik

 ! write (*,*) " "
 ! write (*,*) "====== CLP HEIGHT & PRES ", "i=", i+nthik, " j=", j+nthik, "======"
 ! write (*,*) " "

 ! DO k = 1,nlays
 !    write (*,*) "k=", k, " x3htf=", x3htf_c(i,j,k),  " x3htm=", x3htm_c(i,j,k) &
 !              , " P_cal=", press_c(i,j,k)
 ! ENDDO

 ! write (*,*) " "

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( xrhojf )  ! commented out to avoid memory fragementation
! DEALLOCATE ( xrhojm )  ! commented out to avoid memory fragementation

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (1x, a9, 2x, f12.4, 2x, a)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METCRO',                             &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METCRO',                             &
              /, 1x, '***   ERROR WRITING TO FILE ', a,                    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE metcro
