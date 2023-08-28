!------------------------------------------------------------------------------
!
! write_corrected_netcdf_v1.f90 -- Write bias corrected Netcdf output file.
!
! This is a support routine for spread.f90, part of the NOAA
! NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-jul-09	write_corrected_netcdf.f90:
!		Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2016-jan-12	Minor change to argument sequence in a library call.
! 2019-may-18	Change output coordinate names to standard LAT and LON.
! 2021-apr-20	Add site bias arrays to main output file, for diagnostics.
! 2022-may-27	Add units attributes to output files.  The units attributes
!		  are the same for both forecast grids and site arrays.
!		Change main program name in history attribute.
!
! 2022-jun-02	write_corrected_netcdf_v1.f90:  Name change.
!		Split into two versions, V1 traditional and new RRFS/online.
!		This V1 version supports the original 2014-style CMAQ format.
!
! This routine writes a single Netcdf file for bias corrected
! gridded forecasts, for one forecast cycle.  Two main data
! variables are written:  bias corrected forecast grids, and the
! bias grids that were used to adjust the raw forecasts.
!
! This version writes all output data and coordinates as single
! precision floats, not doubles.  This is to match regular CMAQ
! output files, as well as to make compact files.
!
! This version emulates the dimension names and ordering of
! regular CMAQ output files.  The vestigial LAY dimension is
! added.
!
! For CF compatibility, the grid coordinate variables are written
! as 2-D file variables, not 4-D.  The vestigial LAY and TSTEP
! dimensions are necessarily omitted.
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

module write__corrected_netcdf_v1
contains

subroutine write_corrected_netcdf_v1 (outfile, outvar, var_units, grid_lats, &
      grid_lons, corr_grids, bias_grids, uncorr_sites, corr_sites, site_ids, &
      site_lats, site_lons, vmiss, diag)

   use netwrite3		! note, imports the "dp" kind parameter
   implicit none

! Input arguments.

   character(*), intent (in) :: outfile		   ! name of output file
   character(*), intent (in) :: outvar		   ! primary output var name
   character(*), intent (in) :: var_units	   ! output var units string
   real(dp),     intent (in) :: grid_lats(:,:)	   ! grid coordinates (X, Y)
   real(dp),     intent (in) :: grid_lons(:,:)
   real(dp),     intent (in) :: corr_grids(:,:,:)  ! bias corrected forecasts
  						   !   (X, Y, hours)
   real(dp),     intent (in) :: bias_grids(:,:,:)  ! bias grids (X, Y, hours)
   real(dp),     intent (in) :: uncorr_sites(:,:)  ! uncorrected forecasts
						   !   at sites (hours, sites)
   real(dp),     intent (in) :: corr_sites(:,:)	   ! bias corrected forecasts
						   !   at sites (hours, sites)
   character(*), intent (in) :: site_ids(:)	   ! site ID's (sites)
   real(dp),     intent (in) :: site_lats(:)	   ! site coordinates (sites)
   real(dp),     intent (in) :: site_lons(:)
   real(dp),     intent (in) :: vmiss		   ! missing value in data
   integer,      intent (in) :: diag		   ! verbosity level, 0-N

! Local parameters.

   real,         parameter :: no_missing  = 0.0	 ! special codes for netwrite3:
   character(*), parameter :: nomiss_char = ' '	 ! do not add missing value att

   character(*), parameter :: no_long     = ' '	 ! special codes for netwrite3:
   character(*), parameter :: no_units    = ' '	 ! suppress these other attribs

   character(*), parameter :: bias_var = 'bias'  ! var name for bias grids

! CMAQ standard dimensions for gridded file variables.
! Dimensions are in fortran order, not C/ncdump order.

   character(*), parameter :: dims_4d_out = '(COL, ROW, LAY, TSTEP)'

! Dimensions for 2-D grid coordinate variables.
! Vestigial dimensions are omitted for CF compatibility.

   character(*), parameter :: grid_dims = '(COL, ROW)'

! Dimensions for site data arrays.

   character(*), parameter :: site_data_dims = '(TSTEP, site)'

! Reserve extra Netcdf header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 3000

! Local variables.

   character(200) history, title
   character varexp*100, fmt1*50

   integer ncid
   real vmin, vmax

! Data output array with added dimension.  Shared among output vars.
! Match CMAQ dimensions.  LAY is vestigial, size 1.
! Dimensions are in fortran order, not C/ncdump order.
! (COL, ROW, LAY, TSTEP) <-- (X, Y, hours)

   real out (size(corr_grids,1), size(corr_grids,2), 1, size(corr_grids,3))

! Missing value masks to conform to output arrays.

   logical vmask (size(corr_grids,1), size(corr_grids,2), 1, size(corr_grids,3))
   logical vmask2 (size(corr_sites,1), size(corr_sites,2))

!-----------------------------------------------------------
! Create a new netcdf file, and write the initial header.
!-----------------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print *, 'write_corrected_netcdf_v1: Create new Netcdf file.'
   if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

   title   = 'CMAQ bias corrected ' // trim (outvar) // ' forecasts'
   history = 'Created by aqm_bias_correct.f90 and write_corrected_netcdf_v1.f90'

   call netcreate3 (outfile, title, history, reserve_header, ncid, diag=diag)
   						! history time stamp is added

!-------------------------------------------------
! Write grid coordinate variables.
!-------------------------------------------------

! Make CF compliant 2-D coordinate variables with CMAQ var names.
! Dimensions are added automatically.
! Convert double to single precision on the fly.

   if (diag >= 2) print *, '  Write grid coordinate variables.'

   varexp = 'LAT' // grid_dims				! define subscripts
   call writevar (varexp, 'Latitude',  'degrees_north', real (grid_lats), &
      no_missing)

   varexp = 'LON' // grid_dims
   call writevar (varexp, 'Longitude', 'degrees_east',  real (grid_lons), &
      no_missing)

! Latitude range attribute.

   if (diag >= 3) print *, '  Write actual_range attributes for grid' &
      // ' coordinates.'

   vmin = minval (real (grid_lats))
   vmax = maxval (real (grid_lats))

   fmt1 = '(2(a,f0.4))'
   if (diag >= 2) print fmt1, '     LAT actual range = ', vmin, ', ', vmax

   call write_var_att ('LAT', 'actual_range', (/ vmin, vmax /) )

! Longitude range attribute.

   vmin = minval (real (grid_lons))
   vmax = maxval (real (grid_lons))

   if (diag >= 2) print fmt1, '     LON actual range = ', vmin, ', ', vmax

   call write_var_att ('LON', 'actual_range', (/ vmin, vmax /) )

!-------------------------------------------------
! Write primary bias corrected forecast grids.
!-------------------------------------------------

! Convert double to single precision reals.
! Also add vestigial LAY dimension for CMAQ.
! Note that dimension order remains the same, except for added LAY dimension.
! (COL, ROW, LAY, TSTEP) <-- (X, Y, hours)

   if (diag >= 3) print *, '  Convert to single precision:' // trim (outvar)

   out(:,:,1,:) = real (corr_grids(:,:,:))

! Write output array to file.

   if (diag >= 2) print *, '  Write variable: ' // trim (outvar)

   varexp = trim (outvar) // dims_4d_out	! define subscripts
   call writevar (varexp, no_long, var_units, out, real (vmiss) )

! Compute range attribute.

   vmask = (out /= vmiss)			! mask for non-missing data only

   if (any (vmask)) then			! if any non-missing values:
      vmin = minval (out, vmask)		! compute limits normally
      vmax = maxval (out, vmask)
   else
      vmin = vmiss				! in case of all missing:
      vmax = vmiss				! range = missing values
   end if

   fmt1 = '(5x, 2a, f0.4, a, f0.4, 2a)'
   if (diag >= 2) print fmt1, trim (outvar), ' actual range = ', vmin, ', ', &
      vmax, ' ', trim (var_units)

   call write_var_att (outvar, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes.

   call write_var_att (outvar, 'coordinates', 'LAT LON')    ! CF compliant

!-------------------------------------------------
! Write bias grids.
!-------------------------------------------------

! Convert double to single precision reals.
! Also add vestigial LAY dimension for CMAQ.
! Note that dimension order remains the same, except for added LAY dimension.
! (COL, ROW, LAY, TSTEP) <-- (X, Y, hours)

   if (diag >= 3) print *, '  Convert to single precision:' // trim (bias_var)

   out(:,:,1,:) = real (bias_grids(:,:,:))

! Write output array to file.

   if (diag >= 2) print *, '  Write variable: ' // trim (bias_var)

   varexp = trim (bias_var) // dims_4d_out	! define subscripts
   call writevar (varexp, no_long, var_units, out, real (vmiss) )

! Compute range attribute.

   vmask = (out /= vmiss)			! mask for non-missing data only

   if (any (vmask)) then			! if any non-missing values:
      vmin = minval (out, vmask)		! compute limits normally
      vmax = maxval (out, vmask)
   else
      vmin = vmiss				! in case of all missing:
      vmax = vmiss				! range = missing values
   end if

   if (diag >= 2) print fmt1, trim (bias_var), ' actual range = ', vmin, ', ', &
      vmax, ' ', trim (var_units)

   call write_var_att (bias_var, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes.

   call write_var_att (bias_var, 'coordinates', 'LAT LON')    ! CF compliant

!-------------------------------------------------
! Write site ID's and coordinates.
!-------------------------------------------------

! Convert double to single precision on the fly.

   if (diag >= 2) print *, "  Write site ID's and coordinates."

   if (diag >= 2) print '(a,i0)', '     Number of sites = ', size (site_lats)

   call writevar ('site_id(site)',  'Site ID', no_units, site_ids, nomiss_char)

   call writevar ('site_lat(site)', 'Site latitude',  'degrees_north', &
      real (site_lats), no_missing)

   call writevar ('site_lon(site)', 'Site longitude', 'degrees_east', &
      real (site_lons), no_missing)

! Range attributes for site coordinates.

   if (diag >= 3) print *, '  Write actual_range attributes for site' &
      // ' coordinates.'

   vmin = minval (real (site_lats))
   vmax = maxval (real (site_lats))

   fmt1 = '(2(a,f0.4))'
   if (diag >= 2) print fmt1, '     site_lat actual range = ', vmin, ', ', vmax

   call write_var_att ('site_lat', 'actual_range', (/ vmin, vmax /) )

   vmin = minval (real (site_lons))
   vmax = maxval (real (site_lons))

   if (diag >= 2) print fmt1, '     site_lon actual range = ', vmin, ', ', vmax

   call write_var_att ('site_lon', 'actual_range', (/ vmin, vmax /) )

!-------------------------------------------------
! Write uncorrected site data (interpolated).
!-------------------------------------------------

   if (diag >= 2) print *, '  Write variable: uncorrected_site'

   varexp = 'uncorrected_site' // site_data_dims	! define subscripts
   call writevar (varexp, 'Uncorrected forecast at site location', var_units, &
      real (uncorr_sites), real (vmiss) )

   vmask2 = (uncorr_sites /= vmiss)		! mask for non-missing data only

   if (any (vmask2)) then
      vmin = minval (uncorr_sites, vmask2)
      vmax = maxval (uncorr_sites, vmask2)
   else
      vmin = vmiss
      vmax = vmiss
   end if

   fmt1 = '(5x, a, f0.4, a, f0.4, 2a)'
   if (diag >= 2) print fmt1, 'uncorrected_site actual range = ', vmin, ', ', &
      vmax, ' ', trim (var_units)

   call write_var_att ('uncorrected_site', 'actual_range', (/ vmin, vmax /) )

!-------------------------------------------------
! Write bias corrected site data.
!-------------------------------------------------

   if (diag >= 2) print *, '  Write variable: corrected_site'

   varexp = 'corrected_site' // site_data_dims	! define subscripts
   call writevar (varexp, 'Corrected forecast at site location', var_units, &
      real (corr_sites), real (vmiss) )

   vmask2 = (corr_sites /= vmiss)		! mask for non-missing data only

   if (any (vmask2)) then
      vmin = minval (corr_sites, vmask2)
      vmax = maxval (corr_sites, vmask2)
   else
      vmin = vmiss
      vmax = vmiss
   end if

   if (diag >= 2) print fmt1, 'corrected_site   actual range = ', vmin, ', ', &
      vmax, ' ', trim (var_units)

   call write_var_att ('corrected_site', 'actual_range', (/ vmin, vmax /) )

!-------------------------------------------------
! Close output file properly.
!-------------------------------------------------

   if (diag >= 4) print *, '  All data written.  Close file properly.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 2) print '(2a)', 'File complete:', trim (outfile)

   if (diag >= 4) print *, 'write_corrected_netcdf_v1: Return.'

end subroutine write_corrected_netcdf_v1
end module write__corrected_netcdf_v1
