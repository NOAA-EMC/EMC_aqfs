!------------------------------------------------------------------------------
!
! write_interp_netcdf.f90 -- Write Netcdf daily interpolated forecast files.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-10	Original version.  By Dave Allured.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
!
! This routine writes a single Netcdf file for MET and CMAQ
! hourly forecast data, interpolated to discrete site locations.
! The output file contains one forecast cycle, and multiple
! forecast variables.
!
! This version does not overwrite existing files.  Attempting
! to overwrite will result in a fatal error.
!
! Error handling:
!
! Currently, all Netcdf errors in this writer are considered to
! be serious problems.  All errors result in diagnostic message,
! and program abort.  There are no soft error returns.
!
!------------------------------------------------------------------------------

module write__interp_netcdf
contains

subroutine write_interp_netcdf (outfile, site_ids, site_lats, site_lons, &
      varnames, out_data, vmiss, diag)

   use netwrite3
   implicit none

! Input arguments.

   character(*), intent (in) :: outfile		! name of output file
   character(*), intent (in) :: site_ids(:)	! site ID strings (S)
   real(dp),     intent (in) :: site_lats(:)	! site coordinates (S)
   real(dp),     intent (in) :: site_lons(:)
   character(*), intent (in) :: varnames(:)	! output var names (V)
   real,         intent (in) :: out_data(:,:,:)	! interpolated forecast data
  						!   (sites, vars, hours)
   real,         intent (in) :: vmiss		! missing value for output data
   integer,      intent (in) :: diag		! verbosity level, 0-N

! Local parameters.

   character(*), parameter :: nomiss_char = ' '	! special codes for netwrite3:
   real,         parameter :: no_missing = 0.0	! do not add missing value atts
   real(dp),     parameter :: nomiss_dbl = 0d0

   character(*), parameter :: no_long  = ' '	! special codes for netwrite3:
   character(*), parameter :: no_units = ' '	! suppress other attributes

! Reserve extra Netcdf header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 1500

! Local variables.

   character(200) history, title
   character varexp*100

   integer vi, nvars, ncid
   real vmin, vmax

   logical vmask (size(out_data,1), size(out_data,3))  ! missing value mask for
						       ! one var (sites, hours)

! Create a new netcdf file, and write the initial header.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'write_interp_netcdf: Create new Netcdf file.'
   if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

   title   = 'Interpolated CMAQ and MET forecasts'
   history = 'Created by interpolate_update.f90'

   call netcreate3 (outfile, title, history, reserve_header, ncid, diag)
   						! history time stamp is added

! Write site ID's and site coordinates.  Dimensions are added automatically.

   if (diag >= 3) print *, "  Write site ID's and site coordinates."

   varexp = 'site_id(site)'
   call writevar (varexp, 'AIRNow site ID', no_units, site_ids, nomiss_char)

   varexp = 'site_lat(site)'
   call writevar (varexp, 'Latitude',  'degrees_north', site_lats, nomiss_dbl)

   varexp = 'site_lon(site)'
   call writevar (varexp, 'Longitude', 'degrees_east',  site_lons, nomiss_dbl)

! Range attributes for site coordinates.

   if (diag >= 4) print *, '  Write actual range attribs for site coordinates.'

   call write_var_att ('site_lat', 'actual_range', (/ minval (site_lats), &
      maxval (site_lats) /))
   call write_var_att ('site_lon', 'actual_range', (/ minval (site_lons), &
      maxval (site_lons) /))

! Add each interpolated forecast variable.

   nvars = size (varnames, 1)			! get number of vars to write

   do vi = 1, nvars
      if (diag >= 3) print *, '  Add interpolated variable: ' &
         // trim (varnames(vi))

      varexp = trim (varnames(vi)) // '(site, tstep)'	! define subscripts
      call writevar (varexp, no_long, no_units, out_data(:,vi,:), no_missing)

! Compute range attribute for current variable.
! Assume at least one non-missing value in each sub-array.

      vmask = (out_data(:,vi,:) /= vmiss)	! mask for non-missing data only

      vmin = minval (out_data(:,vi,:), vmask)	! compute actual_range values
      vmax = maxval (out_data(:,vi,:), vmask)	! for current variable

      if (diag >= 4) print '(2(a,f0.5))', '     Actual range = ',vmin,', ',vmax

      call write_var_att (varnames(vi), 'actual_range', (/ vmin, vmax /) )
   end do

! Close output file properly.

   if (diag >= 4) print *, '  File complete, close properly.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 4) print *, 'write_interp_netcdf: Return.'

end subroutine write_interp_netcdf
end module write__interp_netcdf
