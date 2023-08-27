!-------------------------------------------------------------------------------
!
! read_netcdf_var.real32.f90 -- Specific readers for output type real (float32).
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-03	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from read_netcdf_var.double.f90 version 2022=apr-02.
!
! Note, the optional vmiss output value is always type double in
! these real(32) interfaces.  Caller should adapt vmiss as needed.
!
! For other code documentation, see comments in the 1-D case in
! read_netcdf_var.double.f90.
!
!-------------------------------------------------------------------------------

subroutine read_netcdf_var_1d_f32 (infile, varname, diag, vdata, status, &
      vmiss, nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real,         intent(inout), allocatable :: vdata(:)
   integer,      intent(out)                :: status
   real(dp),     intent(out),   optional    :: vmiss
   integer,      intent(out),   optional    :: nc_id
   integer,      intent(out),   optional    :: var_id
   character(*), intent(out),   optional    :: dim_names(:)

   if (allocated (vdata)) vshape(1:1) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:1), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, &
      default_vmiss_f32, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_1d_f32

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_2d_f32 (infile, varname, diag, vdata, status, &
      vmiss, nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real,         intent(inout), allocatable :: vdata(:,:)
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
      default_vmiss_f32, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_2d_f32

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_3d_f32 (infile, varname, diag, vdata, status, &
      vmiss, nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real,         intent(inout), allocatable :: vdata(:,:,:)
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
      default_vmiss_f32, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_3d_f32

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_4d_f32 (infile, varname, diag, vdata, status, &
      vmiss, nc_id, var_id, dim_names)
   implicit none

   character(*), intent(in)                 :: infile
   character(*), intent(in)                 :: varname
   integer,      intent(in)                 :: diag

   real,         intent(inout), allocatable :: vdata(:,:,:,:)
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
      default_vmiss_f32, vmiss, nc_id, var_id, dim_names)
end subroutine read_netcdf_var_4d_f32
