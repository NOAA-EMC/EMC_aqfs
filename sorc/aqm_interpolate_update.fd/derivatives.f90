!------------------------------------------------------------------------------
!
! derivatives.f90 -- Calculate derived gridded fields for interpolator.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2019-may-20	Original version.  By Dave Allured.
!		Calculate U10 and V10 from 10-meter wind speed and direction.
!		Outputs are single precision, matching current inputs.
!
! 2020-feb-12	Bug fix.  Correct sign reversal in formula for V wind.
!
! 2022-apr-13	Minor fixes.  Reduce unnecessary verbosity.
!		Fix program abort for improbable invalid configurations.
! 2022-apr-24	Change from hard coded derivatives, to formula expressions.
! 		Add expression parser.
!		Add support for summing expressions.
!		Split off separate routines for function and summing handlers.
! 2022-may-23	Propagate units attributes from input to output.
!
! Derived gridded fields are calculated, as needed, before interpolating
! grids to site locations.  This is necessary for correct handling of
! vector fields such as wind speed and direction.
!
! The formula handler is simplistic and specific to the bias correction
! application.  Only a very limited set of functions and calculations
! are currently supported:
!
!    x = u_vector (speed, direction)
!    x = v_vector (speed, direction)
!    x = a + b + c + ...
!
! This routine operates on a master data array containing multiple
! variables.  Only the array slots for calculating inputs and outputs
! are processed.  Input parameters identify the derivatives to calculate,
! and their input fields.
!
! On input, there must be a 1:1 association between formulas and
! variables with reader codes marked "derivative".
!
! Unused hours at the end of input variables in the master array
! should be padded with missing values.
!
! The companion array "nhours_actual" is updated for actual number
! of valid forecast hours, for the derivative output variables only,
! based on valid hours of each derivative's input variables.
!
!------------------------------------------------------------------------------

module derivatives_mod
   private				! all private internal routines
   public derivatives			! only main routine is public

! Module constants, internal only.

   character(*), parameter :: module_id = 'derivatives.f90 version 2022-may-23'

   double precision, parameter :: pi = atan2 (0.0d0, -1.0d0)
   double precision, parameter :: deg_rad = pi / 180	! degrees to radians

contains

! Internal support routines.

   include 'derivatives.find_var.f90'
   include 'derivatives.sum.f90'
   include 'derivatives.wind.f90'

!------------------------------------------------------------------------------
! Main derivatives routine.
!------------------------------------------------------------------------------

subroutine derivatives (varnames, reader_codes, formulas, grid_data, &
      nhours_actual, units, vmiss, diag)

   use string_utils
   implicit none

   character(*), intent(in)    :: varnames(:)	      ! var names in data array
   character(*), intent(in)    :: reader_codes(:)     ! keywords for derivatives
   character(*), intent(in)    :: formulas(:)	      ! formulas for derivatives
   real,         intent(inout) :: grid_data(:,:,:,:)  ! gridded forecast fields
  						      !  (X, Y, vars, hours)
   integer,      intent(inout) :: nhours_actual(:)    ! actual hours each var
   character(*), intent(inout) :: units(:)	      ! units attributes
   real,         intent(in)    :: vmiss		      ! missing value in data
   integer,      intent(in)    :: diag		      ! verbosity level, 0-N

! Local variables.

   character(len(varnames)) var
   character(len(reader_codes)) reader_code
   character(len(formulas)) formula, pattern

   character delimiters*10

   integer fi, nformulas
   integer vi, vi_out, nvars
   integer paren, plus, ntokens

   logical valid, function_type, sum_type
   logical assigned(size(varnames))

   logical, save :: first_call = .true.

   character(len(varnames)), allocatable :: tokens(:)

! Initialize.

   if (diag >= 3) print *, 'derivatives: Start.'

   nvars     = size (varnames)
   nformulas = size (formulas)

   if (nformulas == 0) then
      if (diag >= 3) print *, 'derivatives: No derivatives requested.  Return.'
      return
   end if

   if (diag >= 2 .and. first_call) print *, '  Module ID = ' // module_id
   first_call = .false.

   assigned(:) = .false.

!-------------------------------------------------
! Main loop over each formula expression.
!-------------------------------------------------

formula_loop: &
   do fi = 1, nformulas
      formula = formulas(fi)
      if (diag >= 3) print *, 'derivatives: Compute ' // trim (formula) // '.'

! Parse the current expression into names and delimiters.

      delimiters = '=(),+'
      call tokenize (formula, delimiters, tokens, pattern)
      ntokens = size (tokens)

! First and partial syntax check.  Start of assignment formula.

      valid = (ntokens >= 3)
      if (valid) then
         valid = (pattern(1:2) == 'n=')
      end if

      if (.not. valid) then
         print *, '*** derivatives: Invalid formula in config file:'
         print *, '*** Expected:    "var = expression"'
         print *, '*** Config file: "' // trim (formula) // '"'
         print *, '*** Configuration error.  Abort.'
         call exit (1)
      end if

! Find the target variable in the var table.

      var    = tokens(1)		! get target var name in formula
      vi_out = find_var (varnames, reader_codes, var, 'derived', formula)

! Check for double assignment.

      if (assigned(vi_out)) then
         print *, '*** derivatives: Double definition for derived variable "' &
            // trim (var) // '".'
         print *, '*** Config file: ' // trim (formula)
         print *, '*** Configuration error.  Abort.'
         call exit (1)
      end if

      assigned(vi_out) = .true.      ! mark the current derived var as assigned

! Classify the type of expression.

      paren = index (pattern, '(')		! position of first open paren
      plus  = index (pattern, '+')		! position of first plus sign

      if (paren == 0 .and. plus == 0) then
         print *, '*** derivatives: Invalid formula in config file.'
         print *, '*** Expected:    "var = function (var, var)"'  &
            // ' or "var = var + var + ..."'
         print *, '*** Config file: "' // trim (formula) // '"'
         print *, '*** Configuration error.  Abort.'
         call exit (1)
      end if

      if (paren > 0 .and. plus > 0) then	! if both, then classify on
         function_type = (paren < plus)		! first delimiter in the formula
      else
         function_type = (paren > 0)
      end if

      sum_type = (.not. function_type)		! can only be one or the other

      if (diag >= 3) then
         print *, '  Formula      = ' // trim (formula)
         if (function_type) print *, '  Formula type = function'
         if (sum_type)      print *, '  Formula type = sum'
      end if

! Go to the handler for this formula type.

      if (function_type) call wind_functions (varnames, reader_codes, tokens, &
         formula, pattern, vi_out, grid_data, nhours_actual, units, vmiss, diag)

      if (sum_type) call derivative_sum (varnames, reader_codes, tokens, &
         formula, pattern, vi_out, grid_data, nhours_actual, units, vmiss, diag)

   end do formula_loop

! Check for undefined derivative vars.

   do vi = 1, nvars
      reader_code = reader_codes(vi)
      call lowercase (reader_code)                ! case insensitive

      if (reader_code == 'derived') then
         if (.not. assigned (vi)) then
            print *, '*** derivatives: Missing formula for derived variable ' &
               // trim (varnames(vi)) // '.'
            print *, '*** Configuration error.  Abort.'
            call exit (1)
         end if
      end if
   end do

! All done.  Return with all calculated derivatives in slots
! in main data array.

end subroutine derivatives
end module derivatives_mod
