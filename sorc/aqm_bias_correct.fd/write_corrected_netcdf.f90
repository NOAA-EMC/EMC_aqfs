!------------------------------------------------------------------------------
!
! write_corrected_netcdf.f90 -- Write bias corrected Netcdf output file.
!
! This is a support routine for spread.f90, part of the NOAA
! NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-jul-09	Original version.  By Dave Allured.
!
! This routine writes a single Netcdf file for bias corrected
! gridded forecasts, for one forecast cycle.  Two data variables
! are written:  bias corrected forecast grids, and the bias grids
! that were used to adjust the raw forecasts.
!
! This version writes all output grids and grid coordinates as
! single precision floats, not doubles.  This is to match regular
! CMAQ output files, as well as to make compact files.
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

module write__corrected_netcdf
contains

subroutine write_corrected_netcdf (outfile, outvar, grid_lats, grid_lons, &
      corr_grids, bias_grids, vmiss, diag)

   use netwrite3		! note, imports the "dp" kind parameter
   implicit none

! Input arguments.

   character(*), intent (in) :: outfile		   ! name of output file
   character(*), intent (in) :: outvar		   ! primary output var name
   real(dp),     intent (in) :: grid_lats(:,:)	   ! grid coordinates (X, Y)
   real(dp),     intent (in) :: grid_lons(:,:)
   real(dp),     intent (in) :: corr_grids(:,:,:)  ! bias corrected forecasts
  						   !   (X, Y, hours)
   real(dp),     intent (in) :: bias_grids(:,:,:)  ! bias grids (X, Y, hours)
   real(dp),     intent (in) :: vmiss		   ! missing value in data
   integer,      intent (in) :: diag		   ! verbosity level, 0-N

! Local parameters.

   real,         parameter :: no_missing = 0.0	! special code for netwrite3:
						! do not add missing value att

   character(*), parameter :: no_long  = ' '	! special codes for netwrite3:
   character(*), parameter :: no_units = ' '	! suppress these other attribs

   character(*), parameter :: bias_var = 'bias'    ! var name for bias grids

! CMAQ standard dimensions for gridded file variables.
! Dimensions are in fortran order, not C/ncdump order.

   character(*), parameter :: dims_4d_out = '(COL, ROW, LAY, TSTEP)'

! Dimensions for 2-D grid coordinate variables.
! Vestigial dimensions are omitted for CF compatibility.

   character(*), parameter :: grid_dims = '(COL, ROW)'

! Reserve extra Netcdf header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 1000

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

! Missing value mask to conform to output array.

   logical vmask (size(corr_grids,1), size(corr_grids,2), 1, size(corr_grids,3))

!-----------------------------------------------------------
! Create a new netcdf file, and write the initial header.
!-----------------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print *, 'write_corrected_netcdf: Create new Netcdf file.'
   if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

   title   = 'CMAQ bias corrected ' // trim (outvar) // ' forecasts'
   history = 'Created by bias_correct.f90'

   call netcreate3 (outfile, title, history, reserve_header, ncid, diag)
   						! history time stamp is added

!-------------------------------------------------
! Write grid coordinate variables.
!-------------------------------------------------

! Make CF compliant 2-D coordinate variables with CMAQ var names.
! Dimensions are added automatically.
! Convert double to single precision on the fly.

   if (diag >= 2) print *, '  Write grid coordinate variables.'

   varexp = 'ROW' // grid_dims				! define subscripts
   call writevar (varexp, 'Latitude',  'degrees_north', real (grid_lats), &
      no_missing)

   varexp = 'COL' // grid_dims
   call writevar (varexp, 'Longitude', 'degrees_east',  real (grid_lons), &
      no_missing)

! Latitude range attribute.

   if (diag >= 3) print *, '  Write actual_range attributes for grid' &
      // ' coordinates.'

   vmin = minval (real (grid_lats))
   vmax = maxval (real (grid_lats))

   fmt1 = '(2(a,f0.4))'
   if (diag >= 2) print fmt1, '     ROW actual range (latitudes)  = ', vmin, &
      ', ', vmax

   call write_var_att ('ROW', 'actual_range', (/ vmin, vmax /) )

! Longitude range attribute.

   vmin = minval (real (grid_lons))
   vmax = maxval (real (grid_lons))

   if (diag >= 2) print fmt1, '     COL actual range (longitudes) = ', vmin, &
      ', ', vmax

   call write_var_att ('COL', 'actual_range', (/ vmin, vmax /) )

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
   call writevar (varexp, no_long, no_units, out, real (vmiss) )

! Compute range attribute.

   vmask = (out /= vmiss)			! mask for non-missing data only

   if (any (vmask)) then			! if any non-missing values:
      vmin = minval (out, vmask)		! compute limits normally
      vmax = maxval (out, vmask)
   else
      vmin = vmiss				! in case of all missing:
      vmax = vmiss				! range = missing values
   end if

   fmt1 = '(5x, 2a, f0.4, a, f0.4)'
   if (diag >= 2) print fmt1, trim (outvar), ' actual range = ', vmin, ', ', &
      vmax

   call write_var_att (outvar, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes.

   call write_var_att (outvar, 'coordinates', 'ROW COL')    ! CF compliant

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
   call writevar (varexp, no_long, no_units, out, real (vmiss) )

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
      vmax

   call write_var_att (bias_var, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes.

   call write_var_att (bias_var, 'coordinates', 'ROW COL')    ! CF compliant

!-------------------------------------------------
! Close output file properly.
!-------------------------------------------------

   if (diag >= 4) print *, '  File complete, close properly.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 4) print *, 'write_corrected_netcdf: Return.'

end subroutine write_corrected_netcdf
end module write__corrected_netcdf
