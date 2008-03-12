MODULE coord

!-------------------------------------------------------------------------------
! Name:     Coordinate and Domain Descriptions
! Purpose:  Contains coordinate and domain descriptions.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  03 Oct 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! The definitions and declarations in this include file follow those
! given in the I/O-API include file FDESC3C.EXT and replace similar
! definitions.  This include file is compatible with FDESC3C.EXT.
!
! All variable names end in string "_GD", which is a grid identifier for
! multiple grid applications. "_GD" should be "_G1" for the first grid,
! "_G2" for the second grid, etc.
!
! The horizontal grid definition information is REAL*8 in order 
! to achieve the required precision in geographic-to/from-grid
! coordinate conversions.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection type:
!   1: LATGRD for lat-lon coordinates (unused)
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!-------------------------------------------------------------------------------

  INTEGER                      :: gdtyp_gd

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

  DOUBLE PRECISION             :: p_alp_gd   ! degrees
  DOUBLE PRECISION             :: p_bet_gd   ! degrees
  DOUBLE PRECISION             :: p_gam_gd   ! degrees

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For Lambert, Mercator, and Stereographic, these are the 
!     longitude, -180 < X <= 180, and the latitude, -90 <= Y <= 90, 
!     for the center of the grid's respective Cartesian coordinate system.
! For UTM:  ?
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------
     
  DOUBLE PRECISION             :: xcent_gd   ! degrees longitude
  DOUBLE PRECISION             :: ycent_gd   ! degrees latitude

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (Km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
!-------------------------------------------------------------------------------
     
  DOUBLE PRECISION             :: xorig_gd   ! X-origin [m]
  DOUBLE PRECISION             :: yorig_gd   ! Y-origin [m]

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------
     
  DOUBLE PRECISION             :: xcell_gd   ! X-cell dimension [m]
  DOUBLE PRECISION             :: ycell_gd   ! Y-cell dimension [m]

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
     
  INTEGER                      :: vgtyp_gd

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  CHARACTER*16                 :: vgtpun_gd

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  REAL                         :: vgtop_gd

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  CHARACTER*16                 :: vglvun_gd

!-------------------------------------------------------------------------------
! VGLVS_GD( 1...NLAYS+1 ):
! The list of vertical coordinate surface values in the VGLVUN_GD units
! Layer k extends from VGLVS3D( k ) to VGLVS3D( k+1 ).
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: vglvs_gd   ( : )

!-------------------------------------------------------------------------------
! X3FACE_GD( 0: NLAYS ):
! The list of vertical coordinate surface values in the VGLVUN_GD units 
! coverted to values monotonically increasing with altitude.  ( 1 - VGLVS_GD )
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: x3face_gd  ( : )

!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER*16                 :: coordnam_gd

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER*16                 :: gdname_gd

END MODULE coord
