SUBROUTINE alloc_ctm

!-------------------------------------------------------------------------------
! Name:     Allocate Arrays for CTM Dimensions
! Purpose:  Allocate arrays based on CTM grid.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed JDRATE.  (T. Otte)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           21 Oct 2003  Removed fields associated with GRIDBDY2D, GRIDBDY3D,
!                        and METBDY2D.  (T. Otte)
!           25 Nov 2003  Added JACOBF and JACOBS.  (T. Otte)
!           01 Dec 2003  Added O3.  (T. Otte)
!           04 Dec 2003  Added TROP.  (T. Otte)
!           09 Apr 2004  Added PURB.  (T. Otte)
!           29 Apr 2004  Moved allocation of arrays for chemical boundary
!                        conditions to READCHEMBC so that CB3INDEX can be
!                        set dynamically.  Combined TEM1P5 and TEMP10 as
!                        TEMP2.  Added WSPD10 and WDIR10 to output.  (T. Otte)
!           15 Jun 2004  Added PURB.  (T. Otte)
!           14 JAN 2005  Added ATTEN, CSRAD, AND CSWTOA         (Hsin-mu Lin)
!           24 Jun 2005  Added array to capture reference 03 profile.  (T. Otte)
!           25 Apr 2006  Changed tracers in TB3 from one per lateral boundary
!                        for ozone to tracers that nominally represent ozone in
!                        the boundary layer, free troposphere, and top layer.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE premaqparm
  USE groutcom
  USE mcoutcom
  USE mdoutcom
  USE choutcom
  USE file

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Allocate grid arrays for COORD.
!-------------------------------------------------------------------------------

  ALLOCATE ( vglvs_gd  ( nlays+1 ) )
  ALLOCATE ( x3face_gd ( 0:nlays ) )

!-------------------------------------------------------------------------------
! Allocate grid arrays for VGRD.
!-------------------------------------------------------------------------------

  ALLOCATE ( x3face ( 0:nlays ) )
  ALLOCATE ( x3midl (   nlays ) )

!-------------------------------------------------------------------------------
! Allocate grid arrays for GROUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( gd2 ( ncols+1, nrows+1, gd2index ) )
    glat_d     => gd2(:,:,1)
    glon_d     => gd2(:,:,2)
    gmsfsq_d   => gd2(:,:,3)

  ALLOCATE ( gc2 ( ncols, nrows, gc2index ) )
    glat_c     => gc2(:,:,1)
    glon_c     => gc2(:,:,2)
    gmsfsq_c   => gc2(:,:,3)
    gtopo_c    => gc2(:,:,4)
    gdluse_c   => gc2(:,:,5)
    gpurb_c    => gc2(:,:,6)
    glwmask_c  => gc2(:,:,7)    ! add by lipan
    glufrac01_c  => gc2(:,:,8)    ! add by lipan     
    glufrac02_c  => gc2(:,:,9)    ! add by lipan 
    glufrac03_c  => gc2(:,:,10)    ! add by lipan     
    glufrac04_c  => gc2(:,:,11)    ! add by lipan    
    glufrac05_c  => gc2(:,:,12)    ! add by lipan     
    glufrac06_c  => gc2(:,:,13)    ! add by lipan 
    glufrac07_c  => gc2(:,:,14)    ! add by lipan     
    glufrac08_c  => gc2(:,:,15)    ! add by lipan     
    glufrac09_c  => gc2(:,:,16)    ! add by lipan     
    glufrac10_c  => gc2(:,:,17)    ! add by lipan 
    glufrac11_c  => gc2(:,:,18)    ! add by lipan     
    glufrac12_c  => gc2(:,:,19)    ! add by lipan    
    glufrac13_c  => gc2(:,:,20)    ! add by lipan     
    glufrac14_c  => gc2(:,:,21)    ! add by lipan 
    glufrac15_c  => gc2(:,:,22)    ! add by lipan     
    glufrac16_c  => gc2(:,:,23)    ! add by lipan    
    glufrac17_c  => gc2(:,:,24)    ! add by lipan     
    glufrac18_c  => gc2(:,:,25)    ! add by lipan 
    glufrac19_c  => gc2(:,:,26)    ! add by lipan     
    glufrac20_c  => gc2(:,:,27)    ! add by lipan    
    glufrac21_c  => gc2(:,:,28)    ! add by lipan     
    glufrac22_c  => gc2(:,:,29)    ! add by lipan 
    glufrac23_c  => gc2(:,:,30)    ! add by lipan     
    glufrac24_c  => gc2(:,:,31)    ! add by lipan    
    
    
  ALLOCATE ( gc3 ( ncols, nrows, nlays, gc3index ) )
    gx3htf_c   => gc3(:,:,:,1)
    gx3htm_c   => gc3(:,:,:,2)

!-------------------------------------------------------------------------------
! Allocate grid arrays for MCOUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( mc2 ( ncols, nrows, mc2index+ltotg ) )
    prsfc_c    => mc2(:,:, 1)
    jacobs_c   => mc2(:,:, 2)
    ustar_c    => mc2(:,:, 3)
    wstar_c    => mc2(:,:, 4)
    pbl_c      => mc2(:,:, 5)
    zzero_c    => mc2(:,:, 6)
    moli_c     => mc2(:,:, 7)
    hfx_c      => mc2(:,:, 8)
    qfx_c      => mc2(:,:, 9)
    radyni_c   => mc2(:,:,10)
    rbndyi_c   => mc2(:,:,11)
    rstomi_c   => mc2(:,:,12)
    tempg_c    => mc2(:,:,13)
    temp2_c    => mc2(:,:,14)
    wspd10_c   => mc2(:,:,15)
    wdir10_c   => mc2(:,:,16)
    glw_c      => mc2(:,:,17)
    gsw_c      => mc2(:,:,18)
    rgrnd_c    => mc2(:,:,19)
    rainn_c    => mc2(:,:,20)
    rainc_c    => mc2(:,:,21)
    cfract_c   => mc2(:,:,22)
    cldtop_c   => mc2(:,:,23)
    cldbot_c   => mc2(:,:,24)
    wbar_c     => mc2(:,:,25)
    trop_c     => mc2(:,:,26)

    attenX_c   => mc2(:,:,27)
    atten_c    => mc2(:,:,28)
    csrad_c    => mc2(:,:,29)
    cswtoa_c   => mc2(:,:,30)
    csusf_c    => mc2(:,:,31)
    csdsf_c    => mc2(:,:,32)

    psccb_c    => mc2(:,:,33)
    pscct_c    => mc2(:,:,34)
    pdccb_c    => mc2(:,:,35)
    pdcct_c    => mc2(:,:,36)
    ptccb_c    => mc2(:,:,37)
    ptcct_c    => mc2(:,:,38)

    pbl2_c     => mc2(:,:,39)
    pblr_c     => mc2(:,:,40)
    mixht_c    => mc2(:,:,41)

    sotyp_c    => mc2(:,:,42)
    soilw_c    => mc2(:,:,43)

    lai_c      => mc2(:,:,44)
    snowc_c    => mc2(:,:,45)
    snocov_c   => mc2(:,:,46)  !add by lipan
    veg_c      => mc2(:,:,47)  !add by lipan
    q2m_c      => mc2(:,:,48)  !add by lipan
    wr_c       => mc2(:,:,49)  !add by lipan
    soim1_c    => mc2(:,:,50)  !add by lipan
    soim2_c    => mc2(:,:,51)  !add by lipan 
    soit1_c    => mc2(:,:,52)  !add by lipan
    soit2_c    => mc2(:,:,53)  !add by lipan
    sltyp_c    => mc2(:,:,54)  !add by lipan
    seaice_c   => mc2(:,:,55)  !add by lipan         

    vd_c       => mc2(:,:,mc2index+1:mc2index+ltotg)

  ALLOCATE ( mc3 ( ncols, nrows, nlays, mc3index ) )
    jacobf_c   => mc3(:,:,:, 1)
    jacobm_c   => mc3(:,:,:, 2)
    densa_j_c  => mc3(:,:,:, 3)
    what_jd_c  => mc3(:,:,:, 4)
    cldwtr_c   => mc3(:,:,:, 5)
    ranwtr_c   => mc3(:,:,:, 6)
    qice_c     => mc3(:,:,:, 7)
    qsnow_c    => mc3(:,:,:, 8)
    tempa_c    => mc3(:,:,:, 9)
    wvapor_c   => mc3(:,:,:,10)
    press_c    => mc3(:,:,:,11)
    densa_c    => mc3(:,:,:,12)
    wwind_c    => mc3(:,:,:,13)
    x3htm_c    => mc3(:,:,:,14)
    x3htf_c    => mc3(:,:,:,15)
    eddyv_c    => mc3(:,:,:,16)
    pvc_c      => mc3(:,:,:,17)
    presf_c    => mc3(:,:,:,18)
    cfrac_3d_c => mc3(:,:,:,19)  !add by lipan

  ALLOCATE ( mb3 ( nbndy, nlays, mb3index ) )
    jacobf_b   => mb3(:,:, 1)
    jacobm_b   => mb3(:,:, 2)
    densa_j_b  => mb3(:,:, 3)
    what_jd_b  => mb3(:,:, 4)
    cldwtr_b   => mb3(:,:, 5)
    ranwtr_b   => mb3(:,:, 6)
    qice_b     => mb3(:,:, 7)
    qsnow_b    => mb3(:,:, 8)
    tempa_b    => mb3(:,:, 9)
    wvapor_b   => mb3(:,:,10)
    press_b    => mb3(:,:,11)
    densa_b    => mb3(:,:,12)
    wwind_b    => mb3(:,:,13)
    x3htm_b    => mb3(:,:,14)
    x3htf_b    => mb3(:,:,15)
    eddyv_b    => mb3(:,:,16)
    pvc_b      => mb3(:,:,17)
    presf_b    => mb3(:,:,18)
    cfrac_3d_b => mb3(:,:,19)  !add by lipan

!-------------------------------------------------------------------------------
! Allocate grid arrays for MDOUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( md3 ( ncols+1, nrows+1, nlays, md3index ) )
    uu_d       => md3(:,:,:,1)
    vv_d       => md3(:,:,:,2)
    uhat_s     => md3(:,:,:,3)
    vhat_t     => md3(:,:,:,4)
    uu_c       => md3(:,:,:,5)  !add by lipan
    vv_c       => md3(:,:,:,6)  !add by lipan
    
!-------------------------------------------------------------------------------
! Allocate arrays for CHOUTCOM.
!-------------------------------------------------------------------------------

  IF ( makebc ) THEN

    ALLOCATE ( o3_ref_b ( nbndy, nlays ) )

    ALLOCATE ( tb3 ( nbndy, nlays, tb3index ) )
      o3bl_b     => tb3(:,:, 1)
      o3ft_b     => tb3(:,:, 2)
      o3st_b     => tb3(:,:, 3)
      o3stadv_b  => tb3(:,:, 4)
      o3sttrn_b  => tb3(:,:, 5)

  ENDIF


END SUBROUTINE alloc_ctm
