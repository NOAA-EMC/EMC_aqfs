!------------------------------------------------------------------------------
!
! read_netcdf_var.alloc_check.f90 -- Check result from main array allocation.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
! 2022-apr-19	Minor.  Adjust verbosity levels.
!
! Note: The generic support routines use several module variables
! for status exchange, in addition to their calling arguments.
!
!------------------------------------------------------------------------------

function alloc_check (diag, str_len) result (error_flag)
   implicit none

   integer, intent (in)           :: diag	! verbosity level, 0-N
   integer, intent (in), optional :: str_len	! length, for string vars only

   logical error_flag			! function result: false = success,
					! true = soft error & return to main

! Check allocate result, and set return flag accordingly.

   error_flag = (alloc_status /= 0)	! success = status 0 = flag FALSE

   if (error_flag) then
      print *, '*** read_netcdf_var: Allocate error for data array.'
      print *, '*** Compiler run-time error message:'
      print *, '*** ' // trim (alloc_msg)

      if (present (str_len)) then
         print '(a,i0,a,999(2x,i0))', ' *** String length = ', str_len, &
            ', dimension sizes =', dims(2:rank)
      else
         print '(a,999(2x,i0))',      ' *** Dimension sizes =', dims(1:rank)
      end if

      print *, '*** Assume bad file dimensions.'
      print *, '*** Ignore current variable, and continue running.'

! Allocation successful.  Normal return, error flag = false

   else
      if (diag >= 5) print *, '  Read var data.'  ! progress display for next op
   end if

! Return to caller, either way.  If error status is returned, this
! is treated as a soft error.

! Note that the user status "status" remains set to the abort status,
! because the whole read operation is not yet complete.

end function alloc_check
