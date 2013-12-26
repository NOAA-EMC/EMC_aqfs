MODULE gridinfo

!-------------------------------------------------------------------------------
! Name:     Grid Information
! Purpose:  Contains map projection information about input grid.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/grib.misc/gribinfo.
! Revised:  31 Jul 1999  Original version in REGRID.  (NCAR)
!           13 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           30 Apr 2004  Added cone factor (XN) to MAPINFO.  (T. Otte)
!-------------------------------------------------------------------------------

  TYPE mapinfo

    INTEGER :: igrid     ! Integer referring to the type of map projection:
                         ! 0 = lat/lon
                         ! 3 = Lambert Conformal
                         ! 5 = Polar Stereographic grid.
    INTEGER :: nx        ! Number of points in the X direction.
    INTEGER :: ny        ! Number of points in the Y direction.
    REAL    :: truelat1  ! First true latitude (for Polar Ster. and Lam. Conf.)
    REAL    :: truelat2  ! Second true latitude (for Lambert Conformal).
    REAL    :: lov       ! Central longitude of projection (PS and LC).
    REAL    :: lat1      ! Starting latitude (lower-left corner)
    REAL    :: lon1      ! Starting longitude (lower-left corner)
    REAL    :: dx        ! grid-spacing in the X direction (km or degrees)
    REAL    :: dy        ! grid-spacing in the Y direction (km or degrees)
    REAL    :: cenlat    ! center latitude
    REAL    :: cenlon    ! center longitude
    REAL    :: xn        ! cone factor

  END TYPE mapinfo

  REAL(8),       PARAMETER     :: a          = 6367470.0d0
 ! REAL(8),       PARAMETER     :: ahi        = 6371200.0d0
  REAL(8),       PARAMETER     :: ahi        = 6370000.0d0

  TYPE(mapinfo)                :: map

END MODULE gridinfo
