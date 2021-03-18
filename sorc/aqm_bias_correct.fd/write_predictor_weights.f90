!------------------------------------------------------------------------------
!
! write_predictor_weights.f90 -- Write predictor weights matrix to Netcdf.
!
! This is a support routine for the NOAA/NCAR bias correction system
! for CMAQ forecast outputs.
!
! 2017-may-17	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2017-may-31	Fix up var names and attributes in output file.
!
! This routine writes a single Netcdf file for newly generated
! optimal predictor weights.  Key labeling information, i.e.
! predictor variable names and site ID's, are also written.
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

module write__predictor_weights
contains

subroutine write_predictor_weights (outfile, var_names, site_ids, &
      pred_weights, new_rmse, diag)

   use netwrite3		! note, imports the "dp" kind parameter
   implicit none

! Input arguments.

   character(*), intent(in) :: outfile		  ! name of output weight file
   character(*), intent(in) :: var_names(:)	  ! predictor var names
   character(*), intent(in) :: site_ids(:)	  ! site ID's
   real(dp),     intent(in) :: pred_weights(:,:)  ! weights array (VAR, SITE)
   real(dp),     intent(in) :: new_rmse(:)	  ! RMSE for weights (SITE)
   integer,      intent(in) :: diag		  ! verbosity level, 0-N

! Local parameters.

   real(dp),     parameter :: nomiss_dbl = 0.0	  ! special netwrite3 codes:
   character(*), parameter :: nomiss_str = ' '	  ! do not add missing value att

   character(*), parameter :: no_units = ' '	  ! special netwrite3 code:
						  ! suppress writing this attr.

! Fixed variable and dimension names within Netcdf predictor weight file.
! Note, dimensions are in fortran order, not C order.

   character(*), parameter :: varexp_vars    = 'var_names (var)'
   character(*), parameter :: varexp_sites   = 'site_ids (site)'
   character(*), parameter :: varexp_weights = 'pred_weights (var, site)'
   character(*), parameter :: varexp_rmse    = 'rmse (site)'

! Reserve extra Netcdf header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 2000

! Local variables.

   character(len(var_names)) varnames2(size(var_names))   ! automatic array
   character(200) history, title, long_name
   character null*1

   integer ncid, i, j, nvars, max_len

! Create new netcdf file, and write the initial header.

   if (diag >= 2) print *
   if (diag >= 2) print *, 'write_predictor_weights: Create new Netcdf file.'
   if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

   title   = 'Predictor weights file'
   history = 'Created by bias_correct.f90'

   call netcreate3 (outfile, title, history, reserve_header, ncid, diag=diag)
   						! history time stamp is added

! Add null padding to var names, for better appearance in ncdump.
! Need to copy the strings to be able to write the padding characters.

   null = char (0)
   varnames2(:) = repeat (null, len (var_names))   ! init strings to all nulls

   max_len = 0
   nvars   = size (var_names)

   do i = 1, nvars				! make null padded var names
      j = len_trim (var_names(i))
      varnames2(i)(1:j) = var_names(i)		! copy only non-blank chars,
      						! leave nulls in rest of string
      max_len = max (max_len, j)		! remember longest var name
   end do

! Write row and column labels.

   if (diag >= 3) print *, '  Write predictor variable names.'
   long_name = 'predictor variable names'
   call writevar (varexp_vars, long_name, no_units, varnames2(:)(1:max_len), &
                                                                     nomiss_str)

   if (diag >= 3) print *, '  Write site ID''s.'
   long_name = 'Obs site IDs'
   call writevar (varexp_sites,   long_name, no_units, site_ids,     nomiss_str)

! Write predictor weights and RMSE.

   if (diag >= 3) print *, '  Write predictor weights.'
   long_name = 'predictor weights'
   call writevar (varexp_weights, long_name, no_units, pred_weights, nomiss_dbl)

   if (diag >= 3) print *, '  Write RMSE for weight sets.'
   long_name = 'RMSE for each weight set'
   call writevar (varexp_rmse,    long_name, no_units, new_rmse,     nomiss_dbl)

! Close output file properly.

   if (diag >= 4) print *, '  File complete, close properly.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 4) print *, 'write_predictor_weights: Return.'

end subroutine write_predictor_weights
end module write__predictor_weights
