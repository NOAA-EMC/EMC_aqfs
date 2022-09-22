!------------------------------------------------------------------------------
!
! read_grid_coords.f90 -- Read 2-D grid coordinates from CMAQ coordinate file.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/ESRL/CIRES/PSD.
! 2019-jun-18	Optionally read terrain elevation from same CMAQ file.
!
! Notes:
!
! In this version, any error reading the coordinate file will
! cause program abort.  The coordinate file is fundamental.
!
! The var names LAT, LON, and HT (height of terrain) are
! currently hard coded for the CMAQ coordinate file.
!
! Coordinate variables in this CMAQ file have four dimensions,
! because they have the CMAQ uniform dimension layout.  This
! routine removes the two vestigial dimensions, TSTEP and LAY.
!
!------------------------------------------------------------------------------

module read__grid_coords

contains

subroutine read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons, &
      elevations)

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   implicit none

! Input arguments.

   character(*), intent(in) :: grid_coord_file		! name of coord file
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: grid_lats(:,:)  ! grid coordinates
   real(dp), intent(out), allocatable :: grid_lons(:,:)  ! (ROW, COL)

   real(dp), intent(out), allocatable, optional :: elevations(:,:)

! Local variables.

   character varname*60
   integer nx, ny, status
   logical elev

! 4-D input arrays to conform to the CMAQ input file.

   real(dp), allocatable :: file_lats(:,:,:,:)	  ! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: file_lons(:,:,:,:)	  ! COL = X, ROW = Y
   real(dp), allocatable :: file_elevs(:,:,:,:)	  ! LAY and TSTEP are vestigial

! Specific var names for aqm.t12z.grdcro2d.ncf, 2014-mar-29 through 2019.

   character(*), parameter :: varname_lat  = 'LAT'
   character(*), parameter :: varname_lon  = 'LON'
   character(*), parameter :: varname_elev = 'HT'

! Read latitude grid into 4-D conforming array.
! Use generic Netcdf reader.  Input array is auto-allocated.

   if (diag >= 1) print *, 'read_grid_coords: Start.'

   varname = varname_lat
   call read_netcdf_var (grid_coord_file, varname, diag, file_lats, status)

! Read longitude grid.

   if (status == normal) then
      varname = varname_lon
      call read_netcdf_var (grid_coord_file, varname, diag, file_lons, status)
   end if

! Read elevations grid, optional.

   elev = (present (elevations))

   if ( (status == normal) .and. elev ) then
      varname = varname_elev
      call read_netcdf_var (grid_coord_file, varname, diag, file_elevs, status)
   end if

! Check for read error.

   if (status /= normal) then
      print *, '*** read_grid_coords: Abort, error reading grid coordinate' &
         // ' file.'
      print *, '*** File = ' // trim (grid_coord_file)
      print *, '*** Var name = ' // trim (varname)
      call exit (1)
   end if

! Transfer to shape of output arrays.  Omit vestigial dimensions.

   nx = size (file_lats, 1)		! COL dimension in file
   ny = size (file_lats, 2)		! ROW dimension in file

   allocate (grid_lats(nx,ny), grid_lons(nx,ny))
   if (elev) allocate (elevations(nx,ny))

   grid_lats = file_lats(:,:,1,1)	! (X, Y) <-- (COL, ROW, LAY, TSTEP)
   grid_lons = file_lons(:,:,1,1)

   if (elev) elevations = file_elevs(:,:,1,1)

! Diagnostics, if requested.

   if (diag >= 2) then
      print '(2(a,i0))',   '   Grid size (lat, lon) = ', ny, ', ', nx
      print '(2(a,f0.3))', '   Min, max latitudes   = ', &
         minval (grid_lats), ', ', maxval (grid_lats)
      print '(2(a,f0.3))', '   Min, max longitudes  = ', &
         minval (grid_lons), ', ', maxval (grid_lons)
      if (elev) print '(2(a,f0.3))', '   Min, max elevations  = ', &
         minval (elevations), ', ', maxval (elevations)
   end if

   if (diag >= 3) print *, 'read_grid_coords: Return.'

end subroutine read_grid_coords

end module read__grid_coords
