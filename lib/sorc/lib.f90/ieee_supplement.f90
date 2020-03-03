!------------------------------------------------------------------------------
!
! ieee_supplement.f90 -- Supply missing IEEE arithmetic functions for gfortran.
!
! 1.00	2014-mar-12	Original version.  By Dave Allured, NOAA/PSD/CIRES.
!			Include only ieee_is_nan, for now.
! 1.01	2014-dec-16	Add ieee_value for sNaN only.
!
! Notes:
!
! As of 2014-mar-12, gfortran version 4.8.2, this compiler still
! does not supply any IEEE arithmetic modules, which are part of
! The fortran 2003 standard.  This supplemental module provides
! some of the most needed functions.
!
!-----------------------------------------------------------------------------

module ieee_arithmetic
  implicit none

  private			! visibility control:
  public ieee_is_nan		! all private except for generic interfaces
  public ieee_value

  interface ieee_is_nan					! generic interface
    module procedure ieee_is_nan_real, ieee_is_nan_dbl
  end interface ieee_is_nan

!---------------------------------------
! Module constants.
!---------------------------------------

! Note, these integer values are not universal.  They are just
! definitions in use for this IEEE stub module.

  integer, parameter :: ieee_signaling_nan = 1

!---------------------------------------
! Module procedures.
!---------------------------------------

contains

elemental function ieee_is_nan_real (x) result (out)
  real, intent (in) :: x
  logical out
  out = isnan (x)
end function ieee_is_nan_real

elemental function ieee_is_nan_dbl (x) result (out)
  double precision, intent (in) :: x
  logical out
  out = isnan (x)
end function ieee_is_nan_dbl

elemental function ieee_value (x, class) result (out)
  real,    intent (in) :: x
  integer, intent (in) :: class		  ! class arg is ignored, assumed
  real out				  !   to be ieee_signaling_nan
  out = real (class)			  ! squelch warning
  out = transfer ((/ z'7F800001' /), x)   ! smallest positive signaling NAN
end function ieee_value

end module ieee_arithmetic
