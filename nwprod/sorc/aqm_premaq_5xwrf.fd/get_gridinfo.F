subroutine get_gridinfo(iginfo, ginfo)
  use module_grib
  integer, dimension(40) :: iginfo
  real, dimension(40) :: ginfo
  iginfo = infogrid
  ginfo = gridinfo
end subroutine get_gridinfo
