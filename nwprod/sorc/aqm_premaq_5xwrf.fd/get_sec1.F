subroutine get_sec1(ksec1)
! Return the GRIB Section 1 header information, which has already been
! unpacked by subroutine GRIBHEADER.
  use module_grib
  integer, dimension(100) :: ksec1
  ksec1 = sec1
end subroutine get_sec1
