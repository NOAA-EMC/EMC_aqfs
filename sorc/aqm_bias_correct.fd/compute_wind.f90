!------------------------------------------------------------------------------
!
! compute_wind.f90 -- Compute wind direction and speed from U and V wind.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias correction
! system for CMAQ forecast outputs.
!
! This routine is called by read_interp_forecasts.f90.
!
! 2014-feb-11	Original version used by main_analog.f90, 2014 demo versions.
!		By Dave Allured, NOAA/CIRES/PSD.
!		Adapted from lines 365-421 in main_analog_code.m,
!		  version 2013-jul-9 (PSD3).
!
! 2019-may-30	Adapted for 2019 May bias correction.
!		Remove Netcdf reader.  Wind computations only.
!
! 2020-feb-12	Bug fix in formula to convert (U,V) to wind direction.
!
! Call this routine separately to compute wind direction and speed,
! with the requested variable name in "varname".
!
!------------------------------------------------------------------------------

module compute__wind

! Parameter structure for derivative variable controls.

   type dpar_type
      character(60) derived_wind_dir_var
      character(60) derived_wind_speed_var
      character(60) uwind_var
      character(60) vwind_var
   end type dpar_type

contains

subroutine compute_wind (varname, dpar, udata, vdata, udata_valid, &
    vdata_valid, vmiss, diag, var_data, nhours_valid, status)

  use config, only : dp
  use stdlit
  implicit none

  character(*),   intent(in ) :: varname	! requested var name to compute
  type(dpar_type),intent(in ) :: dpar		! controls for derived vars
  real(dp),       intent(in ) :: udata(:,:)	! U wind input (sites, hours)
  real(dp),       intent(in ) :: vdata(:,:)	! V wind input (sites, hours)
  logical,        intent(in ) :: udata_valid	! true if udata read correctly
  logical,        intent(in ) :: vdata_valid	! true if vdata read correctly
  real(dp),       intent(in ) :: vmiss		! data code for missing value
  integer,        intent(in ) :: diag		! verbosity level, 0=errors only

  real(dp), allocatable, intent(out) :: var_data(:,:)	! computed var data
  							! (sites, hours)
  integer,        intent(out) :: nhours_valid	! actual hours for current var
  integer,        intent(out) :: status		! result code, normal or fail

! Local variables.

  integer nsites

  real(dp), parameter :: rad_deg = 180 / pi

! Start.

  if (diag >= 5) print *, '*** compute_wind: Start.'

  if (diag >= 3) print *, 'Compute ' // trim (varname) // ' from ' &
            // trim (dpar%uwind_var) // ' and ' // trim (dpar%vwind_var) // '.'

! Check for valid source data arrays.
! If any not valid, return error without valid returned data arrays.

  if (.not. (udata_valid .and. vdata_valid)) then
    print *, '*** compute_wind: Missing input variables to compute ' &
                    // trim (varname) // '.'
    print *, '*** Input U or V wind variable is missing or invalid.'
    print *, '*** Check derivative controls in config file.'

    status = fail	! return fail code for soft error handling
    return
  end if

! Get input array dimensions.

! For now, assume udata and vdata are the same size.
! For now, don't check nhours_valid attributes.
! This are generally safe assumptions that should not matter
! to the main program.

  nsites       = size (udata, 1)
  nhours_valid = size (udata, 2)	! this param is returned to caller

! Allocate return array for computed variable.
! Transfer input dimensions to output array.

  if (diag >= 4) print *, 'Allocate computed var array for ' &
                   // trim (varname) // '.'

  allocate (var_data(nsites, nhours_valid))

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

! Compute wind direction.

! Formula credit: NCAR/UCAR Earth Observing Laboratory (EOL)
! https://www.eol.ucar.edu/content/wind-direction-quick-reference
!
! Note double reversal.  Sign flips convert Cartesian counterclockwise
! rotation to desired Meteorological clockwise rotation.
!
! Atan2 returns range -180 to +180.  Use modulo to standardize to 0-360.

  if (varname == dpar%derived_wind_dir_var) then

    where (udata == vmiss .or. vdata == vmiss)
      var_data = vmiss
    elsewhere
      var_data = modulo (rad_deg * atan2 (-udata, -vdata), 360d0)
    end where

! Compute wind speed.

  else if (varname == dpar%derived_wind_speed_var) then

    where (udata == vmiss .or. vdata == vmiss)
      var_data = vmiss
    elsewhere
      var_data = sqrt (udata * udata + vdata * vdata)
    end where

  else
    print *,'*** compute_wind: Unknown variable name "' // trim (varname) // '"'
    status = fail	! return fail code for soft error handling
    return
  end if

! Normal return, requested wind variable was properly computed.

  status = normal

  if (diag >= 5) print *, '*** compute_wind: Normal return.'

end subroutine compute_wind
end module compute__wind
