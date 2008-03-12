SUBROUTINE setgriddefs (lyrct, lyrctht)

!-------------------------------------------------------------------------------
! Name:     Set Grid Definitions
! Purpose:  Gets information from user input and input meteorology, and
!           sets up grid definitions.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           06 Mar 2003  Modified definitions of XORIG_GD and Y0RIG_GD to
!                        minimize real number round-off issues which can
!                        cause header mismatches in CMAQ.  Added double
!                        precision attributes to variables assigned to
!                        I/O API header.  (T. Otte)
!           27 Mar 2003  Modified for PREMAQ.  (T. Otte)
!           30 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  Added error checking to compare XCENT_GD
!                        to MAP%LOV to ensure that the input domain is
!                        centered at the center of the projection.  (T. Otte)
!           30 Apr 2004  Added cone factor (MAP%XN) to argument list for
!                        GETCENTPOINT.  (T. Otte)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE metinfo
  USE coord
  USE parms3
  USE gridinfo

  IMPLICIT NONE

  INTEGER                      :: hh
  INTEGER,       INTENT(IN)    :: lyrct
  INTEGER,       INTENT(IN)    :: lyrctht
  INTEGER                      :: nrow1
  INTEGER                      :: ncol1
  CHARACTER*60                 :: option
  CHARACTER*16,  PARAMETER     :: pname     = 'SETGRIDDEFS'
  REAL                         :: rndx
  REAL                         :: rnthik
  REAL                         :: xorig_ctm
  REAL                         :: xorig_m
  REAL                         :: xorig_w
  REAL                         :: xorig_x
  REAL                         :: yorig_ctm
  REAL                         :: yorig_m
  REAL                         :: yorig_w
  REAL                         :: yorig_x

!-------------------------------------------------------------------------------
! From input meteorology vertical structure indicator, define whether this is
! a hydrostatic or a non-hydrostatic run.
!-------------------------------------------------------------------------------

  IF ( metvert == 1 ) THEN       ! hydrostatic sigma
    met_inhyd = 0
  ELSE IF ( metvert == 2 ) THEN  ! non-hydrostatic hybrid sigma-pressure
    met_inhyd = 1
  ENDIF

!-------------------------------------------------------------------------------
! Use meteorology grid input to define grid dimensions.
!-------------------------------------------------------------------------------

  nx = map%nx
  ny = map%ny

  IF ( lyrct > 0 ) THEN
    nz = lyrct
  ELSE
    GOTO 8020
  ENDIF

  IF ( lyrctht > 0 ) THEN
    nh = lyrctht
  ELSE
    GOTO 8040
  ENDIF

!-------------------------------------------------------------------------------
! Define PREMAQ grid coordinate information from meteorology grid input.
!-------------------------------------------------------------------------------

  metcol = nx
  metrow = ny
  metlay = nz

  IF ( nbdrytrim >= 0 ) THEN  ! not windowing...need to define NCOLS, NROWS
    ncols = ( nx - (2 * nbdrytrim) - (2 * nthik) ) * ndx
    nrows = ( ny - (2 * nbdrytrim) - (2 * nthik) ) * ndx
  ENDIF

  nrows_x = nrows + 2 * nthik
  ncols_x = ncols + 2 * nthik

  nbndy   = 2 * nthik  * (ncols + nrows + 2*nthik)
  nbndyd  = 2 * nthikd * (ncols + nrows + 2*nthikd)

!-------------------------------------------------------------------------------
! Check dimensions of domain.
!-------------------------------------------------------------------------------

  IF ( ( x0 < 1      ) .OR. ( y0 < 1      ) .OR.  &
       ( x0 > nx - 1 ) .OR. ( y0 > ny - 1 ) ) THEN
    WRITE (6,9000) x0, y0, nx, ny
    GOTO 1001
  ENDIF

  IF ( ( ncols < 1 ) .OR. ( nrows < 1 ) ) THEN
    WRITE (6,9050) ncols, nrows
    GOTO 1001
  ENDIF

  IF ( ( nx < ncols_x/ndx ) .OR.  &
       ( ny < nrows_x/ndx ) ) THEN
    WRITE (6,9100) nx, ny, ncols_x/ndx, nrows_x/ndx
    GOTO 1001
  ENDIF

  ncol1 = ncols
  nrow1 = nrows

  IF ( ndx > 1 ) THEN
    ncol1 = ncol1 + 2
    nrow1 = nrow1 + 2
  ENDIF

  IF ( ( ncols > nx-2*nthik ) .OR.  &
       ( nrows > ny-2*nthik ) ) THEN
    WRITE (6,9200) ncols, nrows, nx, ny, nx-2*nthik, ny-2*nthik
    GOTO 1001
  ENDIF

  IF ( ( x0+2*nthik+ncol1/ndx-1 > nx ) .OR.  &
       ( y0+2*nthik+nrow1/ndx-1 > ny ) ) THEN
    WRITE (6,9250) nx, ny, x0+2*nthik+ncol1/ndx-1, y0+2*nthik+nrow1/ndx-1
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Calculate window domain size in terms of MET grid.
!-------------------------------------------------------------------------------

  rndx  = FLOAT(ndx)

  ncg_x = INT( ( ncols + 2 * nthik - 1 ) / rndx )
  ncg_y = INT( ( nrows + 2 * nthik - 1 ) / rndx )

!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection type:
!   1: LATGRD for lat-lon coordinates (unused)
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!-------------------------------------------------------------------------------

  IF ( map%igrid == 3 ) THEN       ! Lambert conformal
    gdtyp_gd = lamgrd3
  ELSE IF ( map%igrid == 5 ) THEN  ! polar stereographic
    gdtyp_gd = stegrd3
  ELSE IF ( map%igrid == 1 ) THEN  ! Mercator
    gdtyp_gd = mergrd3
  ELSE IF ( map%igrid == 6 ) THEN  ! universal transverse Mercator
    gdtyp_gd = utmgrd3
  ELSE
    WRITE (6,9275) map%igrid
    GOTO 1001
  ENDIF

  IF ( gdtyp_gd /= lamgrd3 ) THEN
    WRITE (6,9290) gdtyp_gd
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! The definitions of the map projection specification parameters:
!   P_ALP_GD  (PROJ_ALPHA),
!   P_BET_GD  (PROJ_BETA),
!   P_GAM_GD  (PROJ_GAMMA),
! depend upon the projection type, as follows:
! (Note: if P_ALP_GD < AMISS (=-9E36, from PARMS3.EXT), then the grid
!  description is missing or invalid.)
!
! Lambert        P_ALP_GD <= P_BET_GD are the two latitudes that
!                determine the projection cone; P_GAM_GD is the
!                central meridian.
!
! Mercator       P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the coordinate origin (within the tangent circle);
!                P_GAM_GD is the angle between the cylinder axis
!                and the North polar axis.
!
! Stereographic  P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the point of tangency; P_GAM_GD is the angle from
!                true North to the Y-axis.
!   
! UTM:           P_ALP_GD is the UTM zone.
!                P_BET_GD and P_GAM_GD are unused.
!   
! lat-lon:       Currently not used.  Coordinate units are degrees, with
!                -180.0 < X <= 180.0,  -90.0 <= Y <= 90.0
!                Western longitudes and southern latitudes are negative.
!-------------------------------------------------------------------------------

  p_alp_gd   =  DBLE(map%truelat1)  ! [degrees]
  p_bet_gd   =  DBLE(map%truelat2)  ! [degrees]
  p_gam_gd   =  DBLE(map%lov)       ! [degrees]

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For Lambert, Mercator, and Stereographic, these are the
!     longitude, -180 < X <= 180, and the latitude, -90 <= Y <= 90,
!     for the center of the grid's respective Cartesian coordinate system.
! For UTM:  ?
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------

  CALL getcentpoint (nx, ny, map%truelat1, map%truelat2, map%dx, map%dy,  &
                     map%lat1, map%lon1, ycent_gd, xcent_gd, map%xn)

  IF ( REAL(xcent_gd) /= map%lov ) THEN
    WRITE (6,9293) xcent_gd, map%lov
    GOTO 1001
  ENDIF

  map%cenlon = REAL(xcent_gd)
  map%cenlat = REAL(ycent_gd)

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------

  xcell_gd   =  DBLE(map%dx * 1000.0)  ! [m]
  ycell_gd   =  DBLE(map%dy * 1000.0)  ! [m]

  IF ( xcell_gd == ycell_gd ) THEN
    x_resol  =  xcell_gd
  ELSE
    WRITE (6,9295) map%dx, map%dy
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! VGTYP_GD:
! The vertical grid type:
!   1: VGSIGP for sigma-P coordinates
!   2: VGSGP0 for sigma-P0 coordinates
!   3: VGSIGZ for sigma-Z coordinates
!   4: VGETAP for eta-P coordinates
!   5: VGPRES for pressure coordinates
!   6: VGZVAL for Z (meters above ground)
!   7: VHZVAL for H (meters above mean sea level)
!   8: IMISS  for vertical coordinates not stored in VGLVSD
!             (e.g., temporally or spatially changing vertical coordinates)
!-------------------------------------------------------------------------------

  IF ( metvert == 1 ) THEN       ! hydrostatic sigma
    vgtyp_gd = vgsgph3
  ELSE IF ( metvert == 2 ) THEN  ! non-hydrostatic hybrid sigma-pressure
    vgtyp_gd = vgsgpn3  ! <--- may need new category  tlo 24 Feb 03
  ENDIF

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  vgtpun_gd  = 'Pa'

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  vgtop_gd   = 10000.0  ! [Pa]
  x3top      = 10000.0  ! [Pa]

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  vglvun_gd  = 'none'

!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  coordnam_gd  = coordnam

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  gdname_gd  = grdnam

!-------------------------------------------------------------------------------
! Check origins of output PREMAQ domain and met from offsets.  Take into account
! resolution of MET, PREMAQ, and NTHIK.
!   (X0, Y0) = (COL_OFFSET, ROW_OFFSET)
!-------------------------------------------------------------------------------

  xorig_m = ( ( - 0.5 * FLOAT(nx) ) * rndx ) * map%dx * 1000.0
  yorig_m = ( ( - 0.5 * FLOAT(ny) ) * rndx ) * map%dy * 1000.0

  xorig_w = xorig_m + FLOAT(x0-nthik) * xcell_gd
  yorig_w = yorig_m + FLOAT(y0-nthik) * ycell_gd

  xorig_x = xorig_w + 0.5 * (rndx - 1.0) * xcell_gd
  yorig_x = yorig_w + 0.5 * (rndx - 1.0) * ycell_gd

  rnthik = FLOAT(nthik)

  xorig_ctm = xorig_x + rnthik * xcell_gd
  yorig_ctm = yorig_x + rnthik * ycell_gd

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
! *** Note: This function assumes that the origin of the grid can be measured
!           in whole kilometers.
!-------------------------------------------------------------------------------

  xorig_gd  = DBLE(NINT(xorig_ctm))  ! X-origin [m]
  yorig_gd  = DBLE(NINT(yorig_ctm))  ! Y-origin [m]

!-------------------------------------------------------------------------------
! Check user-defined MCIP output time info against input meteorology.
!-------------------------------------------------------------------------------

  IF ( aqf_start < met_startdate ) THEN
    WRITE (6,9400) aqf_start, met_startdate
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Set up coordinates for diagnostic print on all domains.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col > ncols ) .OR. ( lprt_row > nrows ) .OR.  &
       ( lprt_col < 0     ) .OR. ( lprt_row < 0     ) ) THEN
    WRITE (6,9600) lprt_col, lprt_row, ncols, nrows
    GOTO 1001
  ENDIF

  IF ( lprt_col == 0 ) lprt_col = ncols / 2
  IF ( lprt_row == 0 ) lprt_row = nrows / 2

  lprt_xcol = lprt_col + nthik
  lprt_xrow = lprt_row + nthik

  lprt_metx = (lprt_col+ndx-1)/ndx + x0
  lprt_mety = (lprt_row+ndx-1)/ndx + y0

!-------------------------------------------------------------------------------
! Echo user options and grid definitions to log file.
!-------------------------------------------------------------------------------

  WRITE (*, "(/, 1x, 78('-'), /)")
  WRITE (*, "(24x, a, /)") 'USER OPTIONS AND GRID DEFINITIONS'

  WRITE (*,6000) aqf_start, aqf_end, intvl

  WRITE (*,6200) 'Met   ', metcol,  metrow,  metlay
  WRITE (*,6200) 'X-fer ', ncols_x, nrows_x, metlay
  WRITE (*,6200) 'Output', ncols,   nrows,   nlays
  WRITE (*,'(/)')

  WRITE (*,*) 'Output grid resolution: ', xcell_gd / 1000.0,  ' km'
  WRITE (*,*) 'Window domain origin on met domain (col,row):     ',  &
              x0, ', ', y0
  WRITE (*,*) 'Window domain far corner on met domain (col,row): ',  &
              x0 + ncg_x, ', ', y0 + ncg_y

  WRITE (*,'(/)')
  WRITE (*,"(' Cells and points for diagnostic prints')")
  WRITE (*,6300) 'LPRT_COL ', 'LPRT_ROW ', lprt_col,  lprt_row
  WRITE (*,6300) 'LPRT_XCOL', 'LPRT_XROW', lprt_xcol, lprt_xrow
  WRITE (*,6300) 'LPRT_METX', 'LPRT_METY', lprt_metx, lprt_mety

  WRITE (*,'(/)')
  WRITE (*,"(' IOAPI header variables:')")

  WRITE (*,6400) 'GDTYP3D', gdtyp_gd
  WRITE (*,6500) 'GDNAM3D', gdname_gd
  WRITE (*,6600) 'P_ALP3D', p_alp_gd
  WRITE (*,6600) 'P_BET3D', p_bet_gd
  WRITE (*,6600) 'P_GAM3D', p_gam_gd
  WRITE (*,6600) 'XCENT3D', xcent_gd
  WRITE (*,6600) 'YCENT3D', ycent_gd
  WRITE (*,6600) 'XORIG3D', xorig_gd
  WRITE (*,6600) 'YORIG3D', yorig_gd
  WRITE (*,6600) 'XCELL3D', xcell_gd
  WRITE (*,6600) 'YCELL3D', ycell_gd
  WRITE (*,6400) 'VGTYP3D', vgtyp_gd
  WRITE (*,6600) 'VGTOP3D', vgtop_gd

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (/, 1x, 'Output start date = ', a,  &
              /, 1x, 'Output end date   = ', a,  &
              /, 1x, 'Output interval   = ', i3, ' minutes', / )
 6100 FORMAT (1x, a, ' = ', i3, ':  ', a)
 6200 FORMAT (1x, a, ' domain dimensions (col, row, lay):', 3(2x, i3))
 6300 FORMAT (4x, a, 1x, a, 3x, i4, 2x, i4)
 6400 FORMAT (4x, a, 2x, i10)
 6500 FORMAT (4x, a, 2x, a)
 6600 FORMAT (4x, a, 2x, f10.1)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8020 WRITE (6,9020) lyrct
      GOTO 1001

 8040 WRITE (6,9040) lyrctht
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   PREMAQ lower-left corner is not in met domain',   &
              /, 1x, '***   X0, Y0 = ', 2(2x, i4),                            &
              /, 1x, '***   NX, NY = ', 2(2x, i4),                            &
              /, 1x, 70('*'))

 9020 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   LYRCT must be greater than 0',                    &
              /, 1x, '***   LYRCT = ', i4,                                    &
              /, 1x, 70('*'))

 9040 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   LYRCTHT must be greater than 0',                  &
              /, 1x, '***   LYRCTHT = ', i4,                                  &
              /, 1x, 70('*'))

 9050 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   MCIP domain must have 1 or more cells per side',  &
              /, 1x, '***   NCOLS, NROWS = ', 2(2x, i4),                      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested PREMAQ X domain exceeds met domain',    &
              /, 1x, '***   METCOL, METROW = ', i4, 2x, i4,                   &
              /, 1x, '***   PREMAQ domain (col, row) = ', i4, 2x, i4,         &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested PREMAQ dim. exceed input Met. dim.',    &
              /, 1x, '***   Requested PREMAQ dim.: ', i4, ' x ', i4,          &
              /, 1x, '***   Met. dim.: ', i4, ' x ', i4,                      &
              /, 1x, '***   Max. allowable dim.: ', i4, ' x ', i4,            &
              /, 1x, 70('*'))

 9250 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested PREMAQ dim. exceed input Met. dim.',    &
              /, 1x, '***   Input meteorology dimensions: ', 2(2x, i4),       &
              /, 1x, '***   PREMAQ domain in terms of met: ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9275 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   INVALID INPUT METEOROLOGY MAP PROJECTION ', i4,   &
              /, 1x, 70('*'))

 9290 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   ONLY SET UP TO HANDLE LAMBERT CONFORMAL FOR NOW', &
              /, 1x, '***   INPUT GDTYP_GD ', i4,                             &
              /, 1x, 70('*'))

 9293 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   INPUT DOMAIN MUST BE CENTERED ON CENT LONG',      &
              /, 1x, '***   CALCULATED CENT LONG = ', f13.6,                  &
              /, 1x, '***   INPUT CENT LONG = ', f13.6,                       &
              /, 1x, 70('*'))

 9295 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   ONLY SET UP TO HANDLE DX = DY',                   &
              /, 1x, '***   DX, DY = ', f7.2, 2x, f7.2,                       &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested AQF start date is earlier than input ', &
              /, 1x, '***     meteorology start time',                        &
              /, 1x, '***   User-defined AQF start date  = ', a,              &
              /, 1x, '***   Input meteorology start date = ', a,              &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Diagnostic print cell is outside domain',         &
              /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4,    &
              /, 1x, '***   Output domain NCOLS and NROWS are ', i4, 2x, i4,  &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setgriddefs
