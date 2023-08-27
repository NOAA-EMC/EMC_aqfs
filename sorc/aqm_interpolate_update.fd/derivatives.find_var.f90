!------------------------------------------------------------------------------
!
! derivatives.find_var.f90 -- Var table lookup routine for derivatives.f90.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! program for CMAQ forecast outputs.
!
! 2022-apr-18	derivatives.find_var.f90:
!		Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
! This function finds the named variable in the var table.
! Origin restrictions are enforced.
! All errors are fatal config errors.  There is no error return.
!
! This routine is linked into the derivatives module by the
! include method.  See the main routine derivatives.f90 for full
! documentation.
!
!------------------------------------------------------------------------------

function find_var (varnames, reader_codes, varname, type_required, formula) &
      result (vi_found)

   use string_utils
   implicit none

   character(*), intent(in)    :: varnames(:)	   ! arrays from main routine
   character(*), intent(in)    :: reader_codes(:)
   character(*), intent(in)    :: varname	   ! name of var to look up
   character(*), intent(in)    :: type_required    ! "file var" or "derived"
   character(*), intent(in)    :: formula	   ! current formula for message

   integer vi_found			! function result: found var index

! Local variables.

   character(len(reader_codes)) reader_code

   integer vi, nvars

! Find the requested var name in the var table.

   nvars    = size (varnames)
   vi_found = -99

   do vi = 1, nvars
      if (varname == varnames(vi)) then
         vi_found = vi
         exit
      end if
   end do

   if (vi_found < 0) then
      print *, '*** derivatives: Variable "' // trim (varname) &
        // '" in formula is not found in var table.'
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

! Verify the required origin type.

   reader_code = reader_codes(vi_found)
   call lowercase (reader_code)                ! case insensitive

   if ((type_required == 'file var') .and. (reader_code == 'derived')) then
      print *, '*** derivatives: Derived variables on right side of formula' &
         // ' are not supported.'
      print *, '*** Var name   = ' // trim (varname)
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

   if ((type_required == 'derived') .and. (reader_code /= 'derived')) then
      print *, '*** derivatives: Derived variable "' // trim (varname) &
         // '" is not marked "derived" in var table.'
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

end function find_var
