!------------------------------------------------------------------------------
!
! read_grid_coords.f90 -- Read 2-D grid coordinates from coordinate file.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!
! Notes:
!
! In this version, any error reading the coordinate file will
! cause program abort.  The coordinate file is fundamental.
!
! The var names LAT and LON are currently hard coded for the
! CMAQ coordinate file.
!
! The grid coordinate variables LAT and LON are dimensioned
! strangely with four dimensions.  This routine removes the two
! vestigial dimensions.
!
!------------------------------------------------------------------------------

module read__grid_coords

contains

subroutine read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   implicit none

! Input arguments.

   character(*), intent(in) :: grid_coord_file		! name of coord file
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: grid_lats(:,:)  ! grid coordinates
   real(dp), intent(out), allocatable :: grid_lons(:,:)  ! (X, Y)

! Local variables.

   character varname*60
   integer nx, ny, status

! 4-D input arrays to conform to the CMAQ input file.

   real(dp), allocatable :: file_lats(:,:,:,:)	  ! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: file_lons(:,:,:,:)	  ! COL = X, ROW = Y
   						  ! LAY and TSTEP are vestigial

! Specific var names for aqm.t12z.grdcro2d.ncf, 2014-mar-29, from Pius Lee.

! If this doesn't work for other files, the we might need to move
! these names out to the config file, or else perform some kind of
! data discovery routine.

   character(*), parameter :: varname_lat = 'LAT'
   character(*), parameter :: varname_lon = 'LON'

! Read latitude grid into 4-D conforming array.
! Use generic Netcdf reader.  Input array is auto-allocated.

   if (diag >= 1) print *, 'read_grid_coords: Start.'

   varname =  varname_lat
   call read_netcdf_var (grid_coord_file, varname, diag, file_lats, status)

! Read longitude grid.

   if (status == normal) then
      varname =  varname_lon
      call read_netcdf_var (grid_coord_file, varname, diag, file_lons, status)
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

   grid_lats = file_lats(:,:,1,1)	! (X, Y) <-- (COL, ROW, LAY, TSTEP)
   grid_lons = file_lons(:,:,1,1)

! Diagnostics, if requested.

   if (diag >= 2) then
      print '(2(a,i0))',   '   Grid size (lat, lon) = ', ny, ', ', nx
      print '(2(a,f0.3))', '   Min, max latitudes   = ', &
         minval (grid_lats), ', ', maxval (grid_lats)
      print '(2(a,f0.3))', '   Min, max longitudes  = ', &
         minval (grid_lons), ', ', maxval (grid_lons)
   end if

   if (diag >= 3) print *, 'read_grid_coords: Return.'

end subroutine read_grid_coords

end module read__grid_coords
