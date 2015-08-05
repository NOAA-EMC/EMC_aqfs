SUBROUTINE metgrid2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Grid to CTM Grid
! Purpose:  Puts time-invariant meteorology arrays on CTM grid.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  22 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           25 Nov 2003  Added dynamic definition for LWATER based on
!                        land use classification.  (T. Otte)
!           26 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           24 Jan 2005  Used F90 array index assignment syntax (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE metvars
  USE metinfo

  IMPLICIT NONE

  INTEGER                      :: col
  INTEGER                      :: ii
  INTEGER                      :: iil
  INTEGER                      :: jw
  INTEGER                      :: jj
  INTEGER                      :: jjl
  INTEGER                      :: kk
  INTEGER                      :: lbndface
  INTEGER                      :: lbndsigf
  INTEGER                      :: maxface
  INTEGER                      :: maxlu
  INTEGER                      :: row
  INTEGER                      :: ubndsigf
  REAL                         :: eta1, eta2, test1

  REAL,          ALLOCATABLE   :: x3workf ( : ), x3etaf ( : )

  integer :: sc, ec, sr, er, r, c

!-------------------------------------------------------------------------------
! Set up vertical coordinate,
!        where XX3FACE : interface
!              XX3MIDL : mid level
!
! XX3FACE and XX3MIDL are dimensioned in MCIPCOM and increase monotonically with
! height.
!
! x3workf : delt(sigma) to the bottom one
!           => restruct the sigma  value of each interafce =>  "bottom" is 0
!-------------------------------------------------------------------------------

  lbndsigf = LBOUND(sigmaf,1)
  lbndface = LBOUND(xx3face,1)
  ubndsigf = UBOUND(sigmaf,1)
  maxface  = lbndface + SIZE(sigmaf) - 1

  ALLOCATE ( x3workf ( lbndsigf:ubndsigf ) )
  ALLOCATE ( x3etaf  ( lbndsigf:ubndsigf ) )

!-------------------------------------------------------------------------------
! ** xx3face here are referred to the value from meteorological input, not the CTM
!-------------------------------------------------------------------------------

  DO kk = lbndsigf, ubndsigf
     eta2 = sigmaf(kk)
     x3etaf(kk) = 1.0 - eta2
  ENDDO

  IF ( x3etaf(lbndsigf) > x3etaf(ubndsigf) ) THEN    ! monotonic decrease
    DO jw = lbndsigf, ubndsigf
       x3workf(jw) = x3etaf(lbndsigf) - x3etaf(jw)
    ENDDO
  ELSE
    DO jw = lbndsigf, ubndsigf
      x3workf(jw) = x3etaf(jw) - x3etaf(lbndsigf)
    ENDDO
  ENDIF

  xx3face(lbndface:maxface) = x3workf(lbndsigf:ubndsigf)  ! sigma interface, bottom is "0"

!---------------------------------------------------------------
! sigma value of the mid level, for the bottom interface is "0"
!---------------------------------------------------------------

  DO jw = 1, SIZE(sigmah)
     xx3midl(jw) = 0.5 * ( xx3face(jw) + xx3face(jw-1) )
  ENDDO

  DEALLOCATE ( x3workf )
  DEALLOCATE ( x3etaf  )

!-------------------------------------------------------------------------------
! Determine which land use category represents water (LWATER).  Prior to
! Spring/Summer 2004 implementation for Eta, the Eta model land use contains
! 12 categories in "vegetation type" (numbered 1-12) plus water (numbered 0).
! The Spring/Summer 2004 implementation will contain the USGS 24-category
! classification, where water is 16.  A quick-and-dirty method of determining
! which land use classification is used in Eta is to query for the maximum
! value contained in the VGTYP array.  If it is greater than 13, assume it is
! USGS; otherwise assume it is the original classfication.
!
! Though the 24-category USGS land use set "16" to be over water, the input data
! is "0".  In order to compensate the "0" & "16", set the lwater to be "0", and 
! put extra criteria in subroutines
!    "etadry_RAD", "metvars2ctm_RAD2", "  premaqparm_mod_RAD" (Feb 02 2005, HML)
!-------------------------------------------------------------------------------

 ! maxlu = MAXVAL(vgtyp)

 ! IF ( maxlu > 13 ) THEN  ! assume 24-category USGS land use
 !   lwater = 16
 ! ELSE                    ! assume original 12-category vegetation type
    lwater =  0
 ! ENDIF

!-------------------------------------------------------------------------------
! Put time-invariant cross-point arrays on PREMAQ grid.  Also process
! initial roughness and surface pressure for the static output file.  Process
! initial 3D temperature and pressure for the hydrostatic data set.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtopo(:,:)   =          hgt(sc:ec,sr:er)
  xmapc(:,:)   =        smapc(sc:ec,sr:er)
  xlatc(:,:)   =         latc(sc:ec,sr:er)
  xlonc(:,:)   =         lonc(sc:ec,sr:er)
  xfc  (:,:)   =         f   (sc:ec,sr:er)

  xdluse(:,:)   =  FLOAT(vgtyp(sc:ec,sr:er))

 ! -- The followings are defined in "metvars2ctm" ------------

 ! xprsfc(:,:)   =         psfc(sc:ec,sr:er)

 ! IF ( met_inhyd == 0 ) THEN  ! hydrostatic
 !    DO kk = 1, nz
 !      xtempm(:,:,kk) =  tmp(sc:ec,sr:er,kk)
 !      xpresm(:,:,kk) = pres(sc:ec,sr:er,kk)
 !    ENDDO
 ! ENDIF

!-------------------------------------------------------------------------------
! Put time-invariant dot-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  ec = ec - 1
  er = er - 1

  xmapd(:,:)   =  smapd(sc:ec,sr:er)
  xlatd(:,:)   =   latd(sc:ec,sr:er)
  xlond(:,:)   =   lond(sc:ec,sr:er)

END SUBROUTINE metgrid2ctm
