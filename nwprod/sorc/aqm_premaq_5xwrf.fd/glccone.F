subroutine glccone (fsplat,ssplat,sign,confac)
  use module_grib
  implicit none
  real, intent(in) :: fsplat,ssplat
  integer, intent(in) :: sign
  real, intent(out) :: confac
  if (abs(fsplat-ssplat).lt.1.E-3) then
     confac = sind(fsplat)
  else
     confac = log10(cosd(fsplat))-log10(cosd(ssplat))
     confac = confac/(log10(tand(45.-float(sign)*fsplat/2.))- &
          log10(tand(45.-float(sign)*ssplat/2.)))
  endif
end subroutine glccone
