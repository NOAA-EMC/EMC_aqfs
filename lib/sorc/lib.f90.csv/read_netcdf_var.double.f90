!------------------------------------------------------------------------------
!
! read_netcdf_var.double.f90 -- Specific readers for output array type double.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
!
!------------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Specific reader for 1-D array, output array type double.
!-----------------------------------------------------------------------------

! Documentation for all of the specific numeric routines is
! clustered in this single instance.

subroutine read_netcdf_var_1d (infile, varname, diag, vdata, status, vmiss, &
      nc_id, var_id, dim_names)
   implicit none

! Input arguments.

   character(*), intent(in)            :: infile	! input file name
   character(*), intent(in)            :: varname	! requested var name
   integer,      intent(in)            :: diag		! verbosity level, 0-N

! Output arguments.

   real(dp),     intent(inout), allocatable :: vdata(:)	! data array
   integer,      intent(out)           :: status	! result status, normal
   							!   or fail (stdlit)
   real(dp),     intent(out), optional :: vmiss		! returned missing value
   integer,      intent(out), optional :: nc_id		! Netcdf file ID
   integer,      intent(out), optional :: var_id	! returned variable ID
   character(*), intent(out), optional :: dim_names(:)  ! returned dim names

! Following is all code specific to the rank of the current data array.
! Generic support routines are used as much as possible for the details.
! Note that module variables are used for status exchange in several
! places, to simplify the internal interfaces.

! Determine rank and dim sizes.  No easy way to do this on unallocated array.

   if (allocated (vdata)) vshape(1:1) = shape (vdata)

! Open Netcdf file, read variable metadata, check dimensions,
! and set up for the main allocate statement.

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:1), &
      status)) return				! soft abort on several errors

! Allocate the main data array, as needed.

   if (need_allocate) then			! allocate data array, if needed
      allocate (vdata(dims(1)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag)) return		! soft abort on alloc error
   end if

! Read the entire data array for the current file variable.
! No start or count arrays needed when reading entire array.
! The check function also reads the missing value and dim names, as requested.

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, &
      default_vmiss_dbl, vmiss, nc_id, var_id, dim_names)

end subroutine read_netcdf_var_1d

!-----------------------------------------------------------------------------
! Other specific readers for output array type double.
! For documentation, see the 1-D case above.
!-----------------------------------------------------------------------------

subroutine read_netcdf_var_2d (infile, varname, diag, vdata, status, vmiss, &
      nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real(dp),     intent(inout), allocatable :: vdata(:,:)
   integer,      intent(out)                :: status
   real(dp),     intent(out),   optional    :: vmiss
   integer,      intent(out),   optional    :: nc_id
   integer,      intent(out),   optional    :: var_id
   character(*), intent(out),   optional    :: dim_names(:)

   if (allocated (vdata)) vshape(1:2) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:2), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, &
      default_vmiss_dbl, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_2d

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_3d (infile, varname, diag, vdata, status, vmiss, &
      nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real(dp),     intent(inout), allocatable :: vdata(:,:,:)
   integer,      intent(out)                :: status
   real(dp),     intent(out),   optional    :: vmiss
   integer,      intent(out),   optional    :: nc_id
   integer,      intent(out),   optional    :: var_id
   character(*), intent(out),   optional    :: dim_names(:)

   if (allocated (vdata)) vshape(1:3) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:3), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2),dims(3)), stat=alloc_status, &
         errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, &
      default_vmiss_dbl, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_3d

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_4d (infile, varname, diag, vdata, status, vmiss, &
      nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real(dp),     intent(inout), allocatable :: vdata(:,:,:,:)
   integer,      intent(out)                :: status
   real(dp),     intent(out),   optional    :: vmiss
   integer,      intent(out),   optional    :: nc_id
   integer,      intent(out),   optional    :: var_id
   character(*), intent(out),   optional    :: dim_names(:)

   if (allocated (vdata)) vshape(1:4) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:4), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2),dims(3),dims(4)), stat=alloc_status, &
         errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, &
      default_vmiss_dbl, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_4d
