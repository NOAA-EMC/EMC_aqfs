subroutine deallogrib
! Deallocates a couple of arrays that may be allocated.
  use module_grib
#if defined (CRAY)
#else
  if (allocated(grec)) deallocate(grec)
  if (allocated(bitmap)) deallocate(bitmap)
#endif
end subroutine deallogrib
