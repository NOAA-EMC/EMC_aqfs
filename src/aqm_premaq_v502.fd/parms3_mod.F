MODULE parms3

!-------------------------------------------------------------------------------
! Name:     Parameters for Models-3
! Purpose:  Contains dimensioning parameters, standard file-type, grid-type,
!           (etc.) token values for FORTRAN Models-3 I/O System API.
! Notes:    Dependent upon  M3INT, M3REAL, M3DBLE consistent with NETCDF.EXT
!           Liberally adapted from EPA's MCIP2.
! Revised:  10 Sep 2001  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

! Dimensioning parameters

  INTEGER,      PARAMETER :: mxatts3 =  20   !  max ATDSC3.EXT attribs per var
  INTEGER,      PARAMETER :: mxdesc3 =  60   !  max number of description lines
  INTEGER,      PARAMETER :: mxdlen3 =  80   !  description line length
  INTEGER,      PARAMETER :: mxfile3 =  64   !  max number of open files
  INTEGER,      PARAMETER :: mxlays3 = 100   !  max # of layers per file
  INTEGER,      PARAMETER :: mxvars3 = 2048   !  max number of vars per file
  INTEGER,      PARAMETER :: namlen3 =  16   !  name len (logical names, etc.)


! Token-values ("magic numbers")

  ! File type values

  INTEGER,      PARAMETER :: bndary3 =  2    !  boundary-condition
  INTEGER,      PARAMETER :: custom3 = -1    !  user-structured custom
  INTEGER,      PARAMETER :: dctnry3 =  0    !  dictionary
  INTEGER,      PARAMETER :: dgraph3 = -2    !  directed graph
  INTEGER,      PARAMETER :: grdded3 =  1    !  gridded
  INTEGER,      PARAMETER :: grnest3 =  5    !  nested-grid
  INTEGER,      PARAMETER :: iddata3 =  3    !  ID-referenced data
  INTEGER,      PARAMETER :: kfevnt3 = -3    !  KF cloud event
  INTEGER,      PARAMETER :: profil3 =  4    !  rawind vertical profiles
  INTEGER,      PARAMETER :: ptrfly3 =  8    !  pointer-flyer
  INTEGER,      PARAMETER :: smatrx3 =  6    !  sparse matrix
  INTEGER,      PARAMETER :: tsries3 =  7    !  time series


  ! Variable type values

  INTEGER,      PARAMETER :: m3dble  = 6     !  double precision
  INTEGER,      PARAMETER :: m3int   = 4     !  integer
  INTEGER,      PARAMETER :: m3real  = 5     !  real


  ! Values for CDFID

  INTEGER,      PARAMETER :: buffil3 = -1    !  buffered file
  INTEGER,      PARAMETER :: virfil3 = -2    !  virtual file


  ! OPEN3() flags

  INTEGER,      PARAMETER :: fsnew3  = 3     !  new, read-write
  INTEGER,      PARAMETER :: fsrdwr3 = 2     !  old, read-write
  INTEGER,      PARAMETER :: fsread3 = 1     !  old, read-only
  INTEGER,      PARAMETER :: fscrea3 = 5     !  unknown, write, truncate exist'g
  INTEGER,      PARAMETER :: fsunkn3 = 4     !  unknown, read-write


  ! Grid type values

  INTEGER,      PARAMETER :: lamgrd3 = 2     !  Lambert (RADM)
  INTEGER,      PARAMETER :: latgrd3 = 1     !  lat-lon coords
  INTEGER,      PARAMETER :: mergrd3 = 3     !  Mercator coords
  INTEGER,      PARAMETER :: stegrd3 = 4     !  stereographic
  INTEGER,      PARAMETER :: utmgrd3 = 5     !  UTM (special case Mercator)


  ! Vertical coordinate types
 
  INTEGER,      PARAMETER :: vgsgph3 = 1     !  hydrostatic sigma-P
  INTEGER,      PARAMETER :: vgsgpn3 = 2     !  non-h sigma-P
  INTEGER,      PARAMETER :: vgsigz3 = 3     !  sigma-Z
  INTEGER,      PARAMETER :: vgpres3 = 4     !  pressure (mb)
  INTEGER,      PARAMETER :: vgzval3 = 5     !  Z (m) (above sea lvl)
  INTEGER,      PARAMETER :: vghval3 = 6     !  H (m) (above ground)


  ! Flag values for read

  INTEGER,      PARAMETER :: allays3 = -1    !  read all layers
  CHARACTER*16, PARAMETER :: allvar3 = 'ALL' !  read all variables


  ! Flag values for data quality

  REAL,         PARAMETER :: amiss3  = -9.000E36  !  BADVAL3 < AMISS3 always
  REAL,         PARAMETER :: badval3 = -9.999E36  !  real: "bad" or "missing"
  CHARACTER*16, PARAMETER :: cmiss3  = "????????????????"  !  char: "missing"
  INTEGER,      PARAMETER :: imiss3  = -9999      !  integer: "bad" or "missing"
  INTEGER,      PARAMETER :: okflag3 =  5461      !  integer: "good"

END MODULE parms3
