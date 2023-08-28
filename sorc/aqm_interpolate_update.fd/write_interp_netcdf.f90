!------------------------------------------------------------------------------
!
! write_interp_netcdf.f90 -- Write Netcdf daily interpolated forecast files.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-10	Original version.  By Dave Allured.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
! 2016-jan-12	Minor change to argument sequence in a library call.
!
! 2019-may-21	File format changes:
!		* Interpolated files now contain varying forecast hours
!		  for different variables, mirroring the source data.
!		* Add nhours_valid attribute for number of valid forecast
!		  hours for each variable.
!		* Add missing_value attribute for each variable.
!		  Used mainly for padding at end of valid forecast hours.
!
! 2022-apr-14	Minor.  Set actual_range to vmiss, when data are all missing.
! 2019-may-23	File format change, minor:
!		Add units attributes to interpolated files.
!
! This routine writes a single Netcdf file for MET and CMAQ
! hourly forecast data, interpolated to discrete site locations.
! The output file contains one forecast cycle, and multiple
! forecast variables.
!
! This version does not overwrite existing files.  Attempting
! to overwrite will result in a fatal error.
!
! All output variables share the same forecast hours physical
! time dimension.  Varying length variables are supported by the
! "nhours_valid" input argument, and "nhours_valid" var attributes
! in output files.
!
! On entry, input array "out_data" should be dimensioned to the
! maximum number of forecast hours in actual use.  This is the
! dimension size that will be written to the output file.
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
      varnames, units, nhours_valid, out_data, vmiss, program_id, diag)

   use netwrite3
   implicit none

! Input arguments.

   character(*), intent (in) :: outfile		! name of output file
   character(*), intent (in) :: site_ids(:)	! site ID strings (S)
   real(dp),     intent (in) :: site_lats(:)	! site coordinates (S)
   real(dp),     intent (in) :: site_lons(:)
   character(*), intent (in) :: varnames(:)	! output var names (V)
   character(*), intent (in) :: units(:)	! output units attributes (V)
   integer,      intent (in) :: nhours_valid(:) ! # valid hours for each var (V)
   real,         intent (in) :: out_data(:,:,:)	! interpolated forecast data
  						!   (sites, vars, hours)
   real,         intent (in) :: vmiss		! missing value for output data
   character(*), intent (in) :: program_id	! writer ID for history att.
   integer,      intent (in) :: diag		! verbosity level, 0-N

! Local parameters.

   character(*), parameter :: nomiss_char = ' '	! special codes for netwrite3:
   real(dp),     parameter :: nomiss_dbl = 0d0

   character(*), parameter :: no_long  = ' '	! special codes for netwrite3:
   character(*), parameter :: no_units = ' '	! suppress other attributes

! Reserve extra Netcdf header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 2000

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
   history = 'Created by ' // trim (program_id)

   call netcreate3 (outfile, title, history, reserve_header, ncid, diag=diag)
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
      call writevar (varexp, no_long, units(vi), out_data(:,vi,:), vmiss)

! Compute range attribute for current variable.

      vmask = (out_data(:,vi,:) /= vmiss)	! mask for non-missing data only

      if (any (vmask)) then
         vmin = minval (out_data(:,vi,:), vmask)   ! compute actual_range values
         vmax = maxval (out_data(:,vi,:), vmask)   ! for current variable
      else
         vmin = vmiss				! set to vmiss only when all
         vmax = vmiss				! values are missing (rare)
      end if

      if (diag >= 4) print '(2(a,f0.5))', '     Actual range = ',vmin,', ',vmax

      call write_var_att (varnames(vi), 'actual_range', (/ vmin, vmax /) )

! Add nhours_valid attribute.

      call write_var_att (varnames(vi), 'nhours_valid', nhours_valid(vi))
   end do

! Close output file properly.

   if (diag >= 4) print *, '  File complete, close properly.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 4) print *, 'write_interp_netcdf: Return.'

end subroutine write_interp_netcdf
end module write__interp_netcdf
