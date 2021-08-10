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
! Derived gridded fields are calculated, as needed, before interpolating
! grids to site locations.  This is necessary for correct handling of
! vector fields such as wind speed and direction.
!
! This routine operates on a master data array containing multiple
! variables.  Only the array slots for calculating inputs and outputs
! are processed.  Input parameters identify the derivatives to calculate,
! and their input fields.
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

   double precision, parameter :: pi = atan2 (0.0d0, -1.0d0)
   double precision, parameter :: deg_rad = pi / 180	! degrees to radians

contains

!-------------------------------------------------

subroutine derivatives (varnames, reader_codes, grid_data, nhours_actual, &
      vmiss, vi_wind_direction, vi_wind_speed, diag)

   use string_utils
   implicit none

   character(*), intent(in)    :: varnames(:)	      ! var names in data array
   character(*), intent(in)    :: reader_codes(:)     ! keywords for derivatives
   real,         intent(inout) :: grid_data(:,:,:,:)  ! gridded forecast fields
  						      !  (X, Y, vars, hours)
   integer,      intent(inout) :: nhours_actual(:)    ! actual hours each var
   real,         intent(in)    :: vmiss		      ! missing value in data
   integer,      intent(in)    :: vi_wind_direction   ! var indices of input
   integer,      intent(in)    :: vi_wind_speed       !   variables
   integer,      intent(in)    :: diag		      ! verbosity level, 0-N

! Local variables.

   character(len(reader_codes)) reader_code, prefix

   integer vi, vi_u10, vi_v10, nvars, nhours
   integer ucount, vcount

   logical, allocatable :: wind_valid_mask(:,:,:)

! Identify the derived variables.  Skip keywords not starting with "derived".

   nvars = size (varnames)

   ucount = 0
   vcount = 0

   do vi = 1, nvars
      reader_code = reader_codes(vi)

      prefix = reader_code(1:7)		! initial check for prefix only
      call lowercase (prefix)		! case insensitive for prefix only

      if (prefix /= 'derived') cycle

      if (reader_code == 'derived.U10') then
         vi_u10 = vi
         ucount = ucount + 1

      else if (reader_code == 'derived.V10') then
         vi_v10 = vi
         vcount = vcount + 1

      else
         print *, '*** derivatives: Unknown derivative keyword "' &
            // trim (reader_code) // '" in config file.'
         print *, '*** Var name = ' // trim (varnames(vi)) // '.'
         print *, '*** Fundamental configuration problem.  Abort.'
         call exit (1)
      end if
   end do

! Return if no derivatives requested.

   if (ucount + vcount == 0) then
      if (diag >= 2) &
         print *, 'derivatives:  No derivatives requested.  Return.'
      return
   end if

! Consistency checks.

   if (ucount > 1 .or. vcount > 1) then
      print *, '*** derivatives: Same derivative selected twice in config file.'
      print *, '*** Invalid configuration.  Abort.'
      call exit (1)
   end if

   if (vi_wind_direction == 0) then
      print *, '*** derivatives:  U or V derivatives are requested.'
      print *, '*** Two wind input variables must be specified in config file.'
      print *, '*** Missing var name for "input wind direction".'
   end if

   if (vi_wind_speed == 0) then
      print *, '*** derivatives:  U or V derivatives are requested.'
      print *, '*** Two wind input variables must be specified in config file.'
      print *, '*** Missing var name for "input wind speed".'
   end if

! Common setup.

   nhours = minval (nhours_actual((/ vi_wind_direction, vi_wind_speed /)) )
   					! common number of forecast hours
					! (should be the same)

   wind_valid_mask = (grid_data(:,:,vi_wind_direction,:) /= vmiss) &
               .and. (grid_data(:,:,vi_wind_speed,    :) /= vmiss)

! Compute U wind derivative.

! Input from two wind variables in data array.
! Output to one derived variable in data array.

   if (ucount == 1) then
      if (diag >= 3) &
         print *, 'Compute derivative ' // trim (varnames(vi_u10)) // '.'

      grid_data(:,:,vi_u10,:) = vmiss		! clear all to missing values
      nhours_actual(vi_u10)   = nhours		! output extent of derivative

      call calc_u (grid_data(:,:,vi_wind_direction,:), &
                   grid_data(:,:,vi_wind_speed,    :), &
                   grid_data(:,:,vi_u10,           :), &
                   wind_valid_mask)
   end if

! Compute V wind derivative.

   if (vcount == 1) then
      if (diag >= 3) &
         print *, 'Compute derivative ' // trim (varnames(vi_v10)) // '.'

      grid_data(:,:,vi_v10,:) = vmiss
      nhours_actual(vi_v10)   = nhours

      call calc_v (grid_data(:,:,vi_wind_direction,:), &
                   grid_data(:,:,vi_wind_speed,    :), &
                   grid_data(:,:,vi_v10,           :), &
                   wind_valid_mask)
   end if

end subroutine derivatives

!-------------------------------------------------
! Array calculation routines.
!-------------------------------------------------

! These routines are broken out to enable efficient array syntax,
! otherwise prevented by detail subsetting of the master data array.

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

end module derivatives_mod
