!------------------------------------------------------------------------------
!
! read_netcdf_var.check_nc.f90 -- Low-level error handler for Netcdf errors.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
!
! All Netcdf errors are soft errors in this version.
!
!------------------------------------------------------------------------------

function check_nc (status_nc, op_name) result (error_flag)
   implicit none

   integer,      intent (in) :: status_nc	! netcdf status input
   character(*), intent (in) :: op_name		! netcdf operation name

   logical error_flag				! function result:
   						! true = error, false = normal
                                                ! for if (*) return constructs
! Check Netcdf error code.

   error_flag = (status_nc /= nf90_noerr)	! func result = true if error

! If error detected, print diagnostics.

   if (error_flag) then
      print *, '*** read_netcdf_var: Netcdf error on ' // trim (op_name) // ':'
      print *, '*** ' // trim (nf90_strerror (status_nc))
   end if					! show the library error message

! Return to caller with function result either true or false.
! All Netcdf errors are soft errors in this version.

end function check_nc
