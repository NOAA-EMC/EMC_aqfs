!------------------------------------------------------------------------------
!
! derivatives.wind.f90 -- Wind vector functions for derivatives.f90.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! program for CMAQ forecast outputs.
!
! 2022-apr-20	derivatives.wind.f90:
!		Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Functions to calculate U and V from wind speed and direction.
!		Split off from main routine derivatives.f90.
! 2022-may-23	Propagate units attributes.
!
! This routine performs final-stage formula interpretation, and
! actual wind vector calculations for U and V.  The following
! formula expressions are currently supported:
!
!    x = u_vector (speed, direction)
!    x = v_vector (speed, direction)
!
! This routine is linked into the derivatives module by the
! include method.  See the main routine derivatives.f90 for full
! documentation.
!
!------------------------------------------------------------------------------

subroutine wind_functions (varnames, reader_codes, tokens, formula, pattern, &
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

   character(len(tokens)) func

   integer vi_direction, vi_speed
   integer nhours

   logical, allocatable :: wind_valid_mask(:,:,:)

! Full syntax check for function type expressions.

   if (pattern /= 'n=n(n,n)') then
      print *, '*** derivatives: Invalid formula in config file.'
      print *, '*** Expected:    "var = function (var, var)."'
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

! Check for available function names.  Case sensitive.

   func = tokens(3)

   if (.not. any (func == (/ 'u_vector', 'v_vector' /) )) then
      print *, '*** derivatives: Unknown function name "' // trim (func) &
         // '" in formula.'
      print *, '*** Config file: ' // trim (formula)
      print *, '*** Available functions are u_vector and v_vector.'
      print *, '*** Configuration error.  Abort.'
      call exit (1)
   end if

! Find and validate the input vars for this function.

   vi_speed = find_var (varnames, reader_codes, tokens(5), 'file var', formula)
   vi_direction = &
              find_var (varnames, reader_codes, tokens(7), 'file var', formula)

! Common setup for wind functions.

   if (diag >= 3) print *,'Compute derivative ' // trim(varnames(vi_out)) // '.'

   nhours = minval (nhours_actual((/ vi_direction, vi_speed /)))
					! common number of forecast hours
					! (should be the same)

   wind_valid_mask = (grid_data(:,:,vi_direction,:) /= vmiss) &
               .and. (grid_data(:,:,vi_speed,    :) /= vmiss)

   grid_data(:,:,vi_out,:) = vmiss        ! clear all to missing values
   nhours_actual(vi_out)   = nhours       ! output extent of derivative

! Compute U or V wind derivative.
! Input from two wind variables in data array.
! Output to one derived variable in data array.

! Use the where construct for best efficiency.  However, a subroutine
! call is required, because the fortran where construct does not
! directly accept array section syntax.

   if (func == 'u_vector') then
      call calc_u (grid_data(:,:,vi_direction,:), &
                   grid_data(:,:,vi_speed,    :), &
                   grid_data(:,:,vi_out,      :), &
                   wind_valid_mask)
   else
      call calc_v (grid_data(:,:,vi_direction,:), &
                   grid_data(:,:,vi_speed,    :), &
                   grid_data(:,:,vi_out,      :), &
                   wind_valid_mask)
   end if

! Propagate units attribute to the output.
! U and V will have the same units as the input wind speed.

   units(vi_out) = units(vi_speed)

end subroutine wind_functions

!-------------------------------------------------
! Array calculation routines.
!-------------------------------------------------

! These routines provide the where construct for array slices.

! Note:  Constant deg_rad forces all calculations to double precision.
! Results are then converted back to single on assignment.

!---------------------------------------------------------------------------
!
! Definitions:
!
! Meteorological conventions for wind direction are used.
!
! Wind direction is conventional azimuth.  Rotation is clockwise.
! Origin = zero degrees = wind from north to south.
! 90 degrees            = wind from east to west.
! 180 degrees           = wind from south to north.  Etc.
!
! U Wind:  Positive = wind from west to east.
! V Wind:  Positive = wind from south to north.
!
!---------------------------------------------------------------------------

subroutine calc_u (wind_dir, wind_speed, uwind, wind_valid_mask)
   implicit none
   real,    intent (in ) :: wind_dir(:,:,:)
   real,    intent (in ) :: wind_speed(:,:,:)
   real,    intent (out) :: uwind(:,:,:)
   logical, intent (in ) :: wind_valid_mask(:,:,:)

   where (wind_valid_mask)
      uwind = -sin (wind_dir * deg_rad) * wind_speed
   end where
end subroutine calc_u

!-------------------------------------------------

subroutine calc_v (wind_dir, wind_speed, vwind, wind_valid_mask)
   implicit none
   real,    intent (in ) :: wind_dir(:,:,:)
   real,    intent (in ) :: wind_speed(:,:,:)
   real,    intent (out) :: vwind(:,:,:)
   logical, intent (in ) :: wind_valid_mask(:,:,:)

   where (wind_valid_mask)
      vwind = -cos (wind_dir * deg_rad) * wind_speed
   end where
end subroutine calc_v
