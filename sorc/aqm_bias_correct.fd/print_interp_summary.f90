!------------------------------------------------------------------------------
!
! print_interp_summary.f90 -- Print input summary for read_interp_forecasts.
!
! This is a support routine for the NOAA NCO/ARL/PSL bias correction
! system for CMAQ forecast outputs.
!
! 2022-may-31	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_interp_forecasts.f90 version 2022-may-23.
!
!------------------------------------------------------------------------------

module print__interp_summary
contains

subroutine print_interp_summary (varnames, units, out_data, vmiss)

   use config, only : dp
   implicit none

   character(*), intent(in) :: varnames(:)	  ! model var names (V)
   character(*), intent(in) :: units(:)		  ! units strings (V)
   real(dp),     intent(in) :: out_data(:,:,:,:)  ! output data array (DHVS)
   real(dp),     intent(in) :: vmiss		  ! missing value code

! Local variables.

   character(len(varnames)) var_title
   character(len(units)) units_title
   character fmt1*60

   integer vi, nvars, nvalid, nmiss
   integer width1, width2

   real(dp) percent_miss, vmin, vmean, vmax

   logical, allocatable :: mask_valid(:,:,:)	     ! data mask for single var
   						     ! (days, hours, sites)

! Setup to print var table.
! Left justify the var names and units.  Right justify the numbers.

   nvars = size (varnames)

   var_title   = 'Variable'
   units_title = 'Units'

   width1 = maxval (len_trim ( (/ varnames(:), var_title   /) )) + 3
   width2 = maxval (len_trim ( (/ units(:),    units_title /) ))

! Print summary statistics for each array variable.

   print *
   print '(1x,2a,3a13,a12,a15)', var_title(1:width1), units_title(1:width2), &
      'Min data', 'Mean data', 'Max data', 'No. valid', 'No. missing'

   do vi = 1, nvars
      mask_valid = (out_data(:,:,vi,:) /= vmiss)	! DHS <-- DHVS
      nvalid = count (mask_valid)
      nmiss = size (mask_valid) - nvalid
      percent_miss = (nmiss * 100.0_dp) / size (mask_valid)

      if (nvalid > 0) then
         vmin  = minval (out_data(:,:,vi,:), mask_valid)
         vmax  = maxval (out_data(:,:,vi,:), mask_valid)
         vmean = sum (out_data(:,:,vi,:), mask_valid) / nvalid
      else
         vmin  = vmiss
         vmax  = vmiss
         vmean = vmiss
      end if

!!    fmt1 = "(1x, a10, 2f13.2, 2i13, ' (', f0.1, '%)')"
      fmt1 = "(1x, 2a, 3f13.4, i12, i9, ' (', f0.1, '%)')"
      print fmt1, varnames(vi)(1:width1), units(vi)(1:width2), vmin, vmean, &
         vmax, nvalid, nmiss, percent_miss
   end do

end subroutine print_interp_summary
end module print__interp_summary
