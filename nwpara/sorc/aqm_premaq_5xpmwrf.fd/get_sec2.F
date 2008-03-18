subroutine get_sec2(ksec2)
! Return the GRIB Section 2 header information, which has already been
! unpacked by subroutine GRIBHEADER.
  use module_grib
  integer, dimension(10) :: ksec2
  ksec2 = sec2
end subroutine get_sec2
