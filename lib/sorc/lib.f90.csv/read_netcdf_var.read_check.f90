!------------------------------------------------------------------------------
!
! read_netcdf_var.read_check.f90 -- Check result from main read operation.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
! 2022-apr-19	Minor.  Adjust verbosity levels.
!
! This routine also fetches the missing value and dimension names,
! if requested.
!
! Note: The generic support routines use several module variables
! for status exchange, in addition to their calling arguments.
!
!------------------------------------------------------------------------------

subroutine read_check (status_nc, diag, status, default_vmiss, vmiss, nc_id, &
      var_id, dim_names)
   implicit none

   integer,      intent(in )           :: status_nc	! Netcdf read var status
   integer,      intent(in )           :: diag	  	! verbosity level, 0-N
   integer,      intent(out)           :: status	! status to main caller

   real(dp),     intent(in ), optional :: default_vmiss	! default to match
   							!   output array type
   real(dp),     intent(out), optional :: vmiss		! returned missing value
   integer,      intent(out), optional :: nc_id		! Netcdf file ID
   integer,      intent(out), optional :: var_id	! returned variable ID
   character(*), intent(out), optional :: dim_names(:)	! returned var dim names

! Local variables.

   character(nf90_max_name) name_in
   integer i, att_status, dim_status
   logical dummy

! Default the user return code to operation failed, until all checks
! are complete.

   status = fail

   if (check_nc (status_nc, 'nf90_get_var')) return	! soft error, return

! If requested, read the missing value on the file variable.

check_missing: &
   if (present (vmiss)) then

      if (diag >= 5) print *, '  Read missing_value attribute.'
      att_status = nf90_get_att (ncidi, varidi, 'missing_value', vmiss)

! Check and report unexpected Netcdf error.  Silent for att not found.

      if (att_status /= nf90_enotatt) then		! soft error in any case
         dummy = check_nc (att_status, 'nf90_get_att (missing_value)')
      end if

! If no missing_value, or error, then try the alternate attribute name.

      if (att_status /= nf90_noerr) then

         if (diag >= 5) print *, '  Read _FillValue attribute.'
         att_status = nf90_get_att (ncidi, varidi, '_FillValue', vmiss)

! Check and report unexpected Netcdf error.  Silent for att not found.

         if (att_status /= nf90_enotatt) then		! soft error in any case
            dummy = check_nc (att_status, 'nf90_get_att (_FillValue)')
         end if

! Return a "safe" missing value if neither attribute is present, or on
! unexpected Netcdf error.  Match caller's output array data type.

         if (att_status /= nf90_noerr) vmiss = default_vmiss
      end if

   end if check_missing

! If requested, return optional Netcdf file and variable ID's.

   if (present (nc_id))  nc_id  = ncidi		! copy internal Netcdf ID
   if (present (var_id)) var_id = varidi	! copy internal var ID

! If requested, read dimension names for current variable.

   if (present (dim_names)) then

      if (diag >= 5) print *, '  Read dimension names.'

      do i = 1, rank
         name_in = ' '				! blank fill first, to be safe
         dim_status = nf90_inquire_dimension (ncidi, dimids(i), name_in)

         dummy = check_nc (dim_status, 'nf90_inquire_dimension')
				! report unexpected Netcdf error; treat as soft

         if (diag >= 5) print '(a,i0,2a)', '     Dim #', i, ' = ', trim(name_in)

         if (len_trim (name_in) > len (dim_names)) then
            print *, '*** read_netcdf_var: Dimension name is longer than' &
               // " caller's name variable."
            print *, '*** Soft error.  Dimension name will be truncated.'
         end if

         dim_names(i) = name_in			! save name, maybe truncated
      end do

   end if

! All checks are now complete.  Set success status, and return.

   status = normal

   if (diag >= 5) print *, 'read_netcdf_var: Return.'
   if (diag >= 5) print *

end subroutine read_check
