MODULE premaqparm

!-------------------------------------------------------------------------------
! Name:     PREMAQ Parameters
! Purpose:  Contains PREMAQ parameters.
! Notes:    Liberally adapted from EPA's MCIP2 mcipparm_mod.F.
! Revised:  14 Jan 2002  Original version.  (EPA/ORD/NERL/AMD)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           25 Nov 2003  Added dynamic LWATER based on land use
!                        classification system.  (T. Otte)
!           02 Feb 2005  Added "lwater2" based on land use
!                        classification system for the criteria in "etadry"
!                        to cover the whole water body.      (Hsin-mu Lin)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Dimensions of CTM domain.
!-------------------------------------------------------------------------------

  INTEGER            :: ncols         ! number of grid columns
  INTEGER            :: nrows         ! number of grid rows
  INTEGER            :: nlays         ! number of vertical layers
  INTEGER            :: neta1         ! number of upper meteorology constant P interface
  INTEGER            :: neta2         ! number of lower meteorology sigma layers, mid level
  INTEGER            :: netac         ! number of lower CTM sigma layers in Eta2, full level
  INTEGER, PARAMETER :: nthik = 1     ! boundary thickness (cells)
  INTEGER            :: nbndy         ! number of cells in one layer of boundary
  INTEGER            :: nbdrytrim     ! number of meteorology "boundary" points
                                      ! to remove (on each of four sides)
  INTEGER            :: ncg_x         ! coarse grid X
  INTEGER            :: ncg_y         ! coarse grid Y

  INTEGER            :: ndx           ! ratio of met grid size to CTM grid
                                      ! no interpolation for high resolution

  INTEGER            :: nmlays        ! number of met vertical layers, mid level

  INTEGER, PARAMETER :: maxlays = 70  ! max allowed in NLAYS & NMLAYS

!-------------------------------------------------------------------------------
! Horizontal dimensions of "X" domain (CTM + BNDARY area).
!-------------------------------------------------------------------------------

  INTEGER            :: ncols_x
  INTEGER            :: nrows_x

!-------------------------------------------------------------------------------
! Dimensions for MET input data.
!-------------------------------------------------------------------------------

  INTEGER            :: metcol      ! met. grid dimension for columns (E-W)
  INTEGER            :: metrow      ! met. grid dimension for rows (N-S)
  INTEGER            :: metlay      ! met. grid dimension for layers

!-------------------------------------------------------------------------------
! array of the hybrid coordinate sigma value, eta1 & eta2, in running scripts
!-------------------------------------------------------------------------------

  REAL               :: metlayh    ( maxlays )      ! sigma of mid-level (half)

!-------------------------------------------------------------------------------
! Other dimensional parameters.
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: ltotg  =  20  ! number of species for dry dep. velocity

!-------------------------------------------------------------------------------
! Additional parameters to allow MCIP to process full domain of MET input to
! run CCTM.  This option allows using MET input used for other AQMs to link
! with CMAQ CTM.  To activate this function, set NTHIK=0.  For all other cases,
! NTHIKD = NTHIK = 1.
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: nthikd = 1
  INTEGER            :: nbndyd

!-------------------------------------------------------------------------------
! Grid/Domain Related Parameters.
!-------------------------------------------------------------------------------

  INTEGER            :: x0                 ! coordinates of southwest corner of
  INTEGER            :: y0                 ! PREMAQ X grid w.r.t. met grid

!-------------------------------------------------------------------------------
! Model Options.
!-------------------------------------------------------------------------------

  INTEGER            :: iwind              ! no of iterations for WHAT
                                           !  IWIND <=0  -> downward integration
                                           !  IWIND > 0  -> upward integration
  INTEGER            :: metvert            ! input met. vertical structure
                                           !  METVERT =1 -> hydrostatic sigma
                                           !  METVERT =2 -> n-h hybrid sigma-prs
  INTEGER            :: lwater             ! land use category for water
                                           !  LWATER =0  -> orig Eta veg type
                                           !  LWATER =16 -> USGS 24-cat luse
  INTEGER            :: lwater2 = 16       !  LWATER =16 -> USGS 24-cat luse

!-------------------------------------------------------------------------------
! Run-Time Specifications.
!-------------------------------------------------------------------------------

  CHARACTER*24       :: aqf_start          ! YYYY-MO-DD-HH:MI:SS.SSSS
  CHARACTER*24       :: aqf_end            ! YYYY-MO-DD-HH:MI:SS.SSSS

  INTEGER            :: intvl              ! time interval for output [min]
  INTEGER            :: grstep             ! calculated from INTVL (HHMMSS)

  CHARACTER*16       :: coordnam           ! Coordinate name for I/O API
  CHARACTER*16       :: grdnam             ! Grid name for I/O API

!-------------------------------------------------------------------------------
! Coordinates for diagnostic prints.
!-------------------------------------------------------------------------------

  INTEGER            :: lprt_metx          ! X-coordinate in MET domain
  INTEGER            :: lprt_mety          ! Y-coordinate in MET domain
  INTEGER            :: lprt_col           ! COL-coordinate in output domain
  INTEGER            :: lprt_row           ! ROW-coordinate in output domain
  INTEGER            :: lprt_xcol          ! COL-coordinate in X domain
  INTEGER            :: lprt_xrow          ! ROW-coordinate in X domain

END MODULE premaqparm
