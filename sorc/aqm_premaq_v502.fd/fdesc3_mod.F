MODULE fdesc3

!-------------------------------------------------------------------------------
! Name:     Models-3 File Descriptors
! Purpose:  Contains Fortran data structures for a MODELS 3 file description.
!           Used to pass data between RDDICT3, WRDICT3, OPEN3, DESC3,
!           and their callers.  Common BDESC3 is used to store the
!           non-character-string data, and CDESC3 is used to store
!           the character-string data (recall that FORTRAN 77 prohibits
!           character and non-character data in the same common)
! Notes:    SHOULD ONLY BE USED AS A NAME BASED ARGUMENT PASSING MECHANISM.
!           The user should have local variables to/from which this data
!           structure is copied immediately prior to or immediately after
!           calls which set or use these COMMONs, since their values are
!           subject to change at any time by the IOAPI.
!
! Set By:
!          DESC3:    Everything in FDESC3.EXT
!
!          RDDICT3:  FTYPE3D, TSTEP3D, NCOLS3D, NROWS3D, NLAYS3D, NVARS3D,
!                    NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D,
!                    XORIG3D, YORIG3D, XCELL3D, YCELL3D, GDNAM3D,
!                    XCENT3D, YCENT3D, VNAME3D, UNITS3D, VDESC3D
!
! Referenced By:
!          OPEN3:    FTYPE3D, SDATE3D, STIME3D, TSTEP3D, NCOLS3D, NROWS3D,
!                    NLAYS3D, NVARS3D, NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D,
!                    P_GAM3D, XORIG3D, YORIG3D, XCELL3D, YCELL3D, GDNAM3D,
!                    XCENT3D, YCENT3D, VNAME3D, UNITS3D, VDESC3D
!
!          WRDICT3:  FTYPE3D, TSTEP3D, NCOLS3D, NROWS3D, NLAYS3D, NVARS3D,
!                    NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D, XORIG3D,
!                    YORIG3D, XCELL3D, YCELL3D, XCELL3D, YCELL3D, GDNAM3D, 
!                    VNAME3D, UNITS3D, VDESC3D
!
! ***       Liberally adapted from EPA's MCIP2.
!
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE parms3

  INTEGER :: ftype3d      ! file type
  INTEGER :: cdate3d      ! creation date   YYYYDDD
  INTEGER :: ctime3d      ! creation time    HHMMSS
  INTEGER :: wdate3d      ! update date     YYYYDDD
  INTEGER :: wtime3d      ! update time      HHMMSS
  INTEGER :: sdate3d      ! file start date YYYYDDD
  INTEGER :: stime3d      ! file start time  HHMMSS
  INTEGER :: tstep3d      ! file time step   HHMMSS
  INTEGER :: mxrec3d      ! maximum time step record number (1,2,...)
  INTEGER :: nvars3d      ! number of species
  INTEGER :: ncols3d      ! number of grid columns
  INTEGER :: nrows3d      ! number of grid rows
  INTEGER :: nlays3d      ! number of layers
  INTEGER :: nthik3d      ! BOUNDARY:  perim thickness (cells)
                          ! SPARSE MATRIX:  number of matrix-cols
  INTEGER :: gdtyp3d      ! grid type:  1=LAT-LON, 2=UTM, ...

!.......   Note that horizontal grid definition information is REAL*8 in order 
!.......   to achieve the required precision in geographic-to/from-grid
!.......   coordinate conversions.  Meanings of the map projection
!.......   specification parameters P_ALP3D, P_BET3D, P_GAM3D depend
!.......   upon the projection type, as follows:
!.......   
!.......   If P_ALP3D < AMISS3 (=-9E36, from PARMS3.EXT), then
!.......   the grid description is missing or invalid.
!.......
!.......   lat-lon:   unused.  Coordinate units are degrees, with
!.......              -180.0 < X <= 180.0,  -90.0 <= Y <= 90.0   
!.......              Note that Western hemisphere longitudes are taken 
!.......              to be negative.
!.......
!.......   Lambert    PROJ_ALPHA <= PROJ_BETA are the two latitudes which
!.......              determine the projection cone; PROJ_GAMMA is the
!.......              central meridian.  Coordinate units are meters.
!.......
!.......   Mercator   PROJ_ALPHA and PROJ_BETA are the latitude and longitude
!.......              of the coordinate origin (within the tangent circle);
!.......              PROJ_GAMMA is the angle between the cylinder axis
!.......              and the North polar axis. Coordinate units are meters.
!.......
!.......   Stereographic   PROJ_ALPHA and PROJ_BETA are the latitude and
!.......              longitude of the point of tangency; PROJ_GAMMA is
!.......              the angle from true North to the Y-axis.  Coordinate
!.......              units are meters.
!.......   
!.......   UTM:  PROJ_ALPHA is the UTM zone, as a double.
!.......              PROJ_BETA and PROJ_GAMMA are unused.
!.......              Note that for safety, PROJ_ALPHA should be
!.......              *>rounded<* to integer.
!.......              Coordinate units are meters.
!.......   
!.......   
!.......   (XCENT3D,YCENT3D):
!.......   For Lat-Lon:  unused.
!.......   For Lambert, Mercator, and Stereographic, these are the 
!.......   longitude, -180 < X <= 180, and the latitude, -90 <= Y <= 90, 
!.......   for the center (0,0) of the respective Cartesian coordinate system.
!.......   For UTM:  these are the UTM offsets in meters (UTM coords for
!.......   the origin relative to a UTM system with origin at the equator
!.......   and central metidian of the UTM zone)
!.......   
!.......   (XORIG3D,YORIG3D) are the location in map units (deg. for lat-lon,
!.......   meters otherwise) of the  lower-right corner of the origin (1,1) 
!.......   cell of the horizontal grid.
!.......   
!.......   (XCELL3D,YCELL3D) are the X-direction and Y-direction lengths
!.......   ((deg. for lat-lon, meters otherwise) of side for cells in a 
!.......   regular grid.  If zero, the grid is taken to be an irregular
!.......   grid described by other means (e.g. a grid-geometry file).
!.......   
!.......   VGTYP3D is the vertical grid type token, VGSIGP3 for sigma-P
!.......   coordinates, VGSIGZ3 for sigma-Z, etc., or IMISS3 for vertical
!.......   coordinates not stored in VGLVS3D (e.g., temporally or spatially
!.......   changing vertical coordinates.
!.......   
!.......   VGTOP3D is the model-top used in the definition of the sigma
!.......   coordinate systems (e.g., for hydrostatic sigma-P, the 
!.......   relationship between P and sigma is given by the following:
!.......   S = ( P - VGTOP3D ) / (P_ref - VGTOP3D )
!.......   
!.......   VGLVS3D( 1, ..., NLAYS3D+1 ) is the list of vertical coordinate 
!.......   values which specify the layers of the file.  In principle, 
!.......   layer K goes from VGLVS3D( K ) to VGLVS3D( K+1 ).

  REAL(8) :: p_alp3d                  ! first, second, third map
  REAL(8) :: p_bet3d                  ! projection descriptive
  REAL(8) :: p_gam3d                  ! parameters.

  REAL(8) :: xcent3d                  ! lon for coord-system X=0
  REAL(8) :: ycent3d                  ! lat for coord-system Y=0
  REAL(8) :: xorig3d                  ! X-coordinate origin of grid (map units)
  REAL(8) :: yorig3d                  ! Y-coordinate origin of grid
  REAL(8) :: xcell3d                  ! X-coordinate cell dimension
  REAL(8) :: ycell3d                  ! Y-coordinate cell dimension

  INTEGER :: vgtyp3d                  !  vertical coordinate type (VGSIGP3, ...)
  REAL    :: vgtop3d                  !  model-top, for sigma coord types.
  REAL    :: vglvs3d ( mxlays3 + 1 )  !  vertical coord values.

  COMMON  / bdesc3 /  &   !  non-character description data
            p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,   &
            xorig3d, yorig3d, xcell3d, ycell3d,            &
            ftype3d, cdate3d, ctime3d, wdate3d, wtime3d,   &
            sdate3d, stime3d, tstep3d, mxrec3d,            &
            nvars3d, ncols3d, nrows3d, nlays3d, nthik3d,   &
            gdtyp3d, vgtyp3d, vgtop3d, vglvs3d, vtype3d

  CHARACTER*16 :: gdnam3d      ! grid name             (length NAMLEN3=16)
  CHARACTER*16 :: upnam3d      ! last program writing to file (NAMLEN3=16)
  CHARACTER*80 :: execn3d      ! value of env vble EXECUTION_ID

  CHARACTER*80 :: fdesc3d( mxdesc3 ) ! file description (MXDLEN3=80)
  CHARACTER*80 :: updsc3d( mxdesc3 ) ! update   "       (MXDLEN3=80)

  INTEGER      :: vtype3d( mxvars3 ) ! variable type:  M3(INT|REAL|DBLE)
  CHARACTER*16 :: vname3d( mxvars3 ) ! variable names (length MXDLEN3=80)
  CHARACTER*16 :: units3d( mxvars3 ) !   "   units or 'none' (MXDLEN3=80)
  CHARACTER*80 :: vdesc3d( mxvars3 ) !   "      descriptions (MXDLEN3=80)

  COMMON  / cdesc3 /   &  !  character-string description data
            gdnam3d, upnam3d, execn3d, fdesc3d, updsc3d,   &
            vname3d, units3d, vdesc3d

END MODULE fdesc3
