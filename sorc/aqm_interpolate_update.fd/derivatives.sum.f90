!------------------------------------------------------------------------------
!
! derivatives.sum.f90 -- Summing function for derivatives.f90.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! program for CMAQ forecast outputs.
!
! 2022-apr-20	derivatives.sum.f90:
!		Original version.  By Dave Allured, NOAA/PSL/CIRES.
! 2022-may-23	Propagate units attributes.
!
! This routine provides the summing function for derivatives.f90,
! including final-stage formula interpretation.  Two or more
! input variable may be summed.  The supported formula expression
! in the config file is:
!
!    x = a + b + c ...
!
! This routine is linked into the derivatives module by the
! include method.  See the main routine derivatives.f90 for full
! documentation.
!
!------------------------------------------------------------------------------

subroutine derivative_sum (varnames, reader_codes, tokens, formula, pattern, &
      vi_out, grid_data, nhours_actual, units, vmiss, diag)
   implicit none

   character(*), intent(in)    :: varnames(:)	     ! arrays from main routine
   character(*), intent(in)    :: reader_codes(:)
   character(*), intent(in)    :: tokens(:)
   character(*), intent(in)    :: formula	     ! formula and pattern
   character(*), intent(in)    :: pattern	     !   for current derivative
   integer,      intent(in)    :: vi_out	     ! var index for result var
   real,         intent(inout) :: grid_data(:,:,:,:) ! arrays from main routine
   integer,      intent(inout) :: nhours_actual(:)
   character(*), intent(inout) :: units(:)	     ! units attributes
   real,         intent(in)    :: vmiss
   integer,      intent(in)    :: diag

! Local variables.

   integer vi_in, nhours_out
   integer p1, p2, first_plus, plus_ptr, ntokens

   logical, allocatable :: data_mask(:,:,:)

! Syntax check for first part of summing expression.

   if (diag >= 3) print *,'Compute derivative ' // trim (varnames(vi_out)) // '.'

   if (pattern(1:5) /= 'n=n+n') then
      print *, '*** derivatives: Invalid formula in config file.'
      print *, '*** Expected:    "var = var + var [+ var] ..."'
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

! Find and validate the first input var in the formula.

   vi_in = find_var (varnames, reader_codes, tokens(3), 'file var', formula)

! Initialize the summing array by copying the first input variable.

   if (diag >= 4) print *, '  Start sum with ' // trim (varnames(vi_in)) // '.'

   grid_data(:,:,vi_out,:) = grid_data(:,:,vi_in,:)

   data_mask = (grid_data(:,:,vi_in,:) /= vmiss)   ! init the missing value mask

   nhours_out = nhours_actual(vi_in)		! init tracking of valid hours

! Also propagate the units attribute from the first input variable.
! Assume all terms in the sum have compatible units.

   units(vi_out) = units(vi_in)

!---------------------------------------------------------------------
! Main loop.  Follow the expression and add each input variable
! to the summing array.
!---------------------------------------------------------------------

   first_plus = 4		! start on first plus sign in the pattern
   ntokens    = size (tokens)

term_loop: &
   do plus_ptr = first_plus, ntokens, 2		! step by 2 for pattern "+n"
      p1 = plus_ptr		! plus token
      p2 = p1 + 1		! input var token

! Syntax check for the next term in the summing expression.
! Expect pattern "+n".
! Will also fail on dangling final plus sign, missing the final variable.

      if (pattern(p1:p2) /= '+n') then
         print *, '*** derivatives: Invalid formula in config file.'
         print *, '*** Expected:    "var = var + var [+ var] ..."'
         print *, '*** Config file: ' // trim (formula)
         print *, '*** Configuration error.  Abort.'
         call exit (1)
      end if

! Find and validate the current input var in the formula.

      vi_in = find_var (varnames, reader_codes, tokens(p2), 'file var', formula)

      if (diag >= 4) print *, '  Add var ' // trim (varnames(vi_in)) // '.'

! Add the current input var to the result array.

! Use the where construct for best efficiency.  However, a subroutine
! call is required, because the fortran where construct does not
! directly accept array section syntax.

      data_mask = data_mask(:,:,:) .and. (grid_data(:,:,vi_in,:) /= vmiss)

      call array_sum(grid_data(:,:,vi_in,:), grid_data(:,:,vi_out,:), data_mask)

      nhours_out = min (nhours_out, nhours_actual(vi_in))
      		      ! track valid hours, in case some are shorter than others

   end do term_loop

! Summing complete.  Return final results.

   nhours_actual(vi_out) = nhours_out	  ! common extent of the input vars

end subroutine derivative_sum

!---------------------------------------------------------------------
! Array calculation.  Provide the where construct for array slices.
!----------------------------------------------------------------------

subroutine array_sum (data_in, data_accum, data_mask)
   implicit none
   real,    intent (in)    :: data_in(:,:,:)
   real,    intent (inout) :: data_accum(:,:,:)
   logical, intent (in)    :: data_mask(:,:,:)

   where (data_mask)
      data_accum = data_accum + data_in
   end where
end subroutine array_sum
