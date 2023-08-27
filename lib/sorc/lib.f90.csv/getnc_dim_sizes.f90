!-----------------------------------------------------------------------------
!
! getnc_dim_sizes -- Get rank and dimension sizes for a Netcdf file variable.
!
! This is a generic Netcdf helper routine.
!
! 2022-mar-30	Original version.  By Dave Allured, NOAA/PSL/CIRES.
! 2022-jun-01	Bug fix.  Close file properly on several exit scenarios.
!		Silence more messages at diag level 2.
!
! This version independently opens a Netcdf file, and reads rank
! and dimension sizes for a specified variable.  Status codes for
! several scenarios are returned.  The file is closed on exit.
!
! Caller should check the return codes before reading the
! optional dim_sizes output array.  This array is not allocated
! unless the file variable is present and not scalar.
!
! Error handling:
!
! This version is designed to quietly handle several error
! scenarios, return useful status info to the caller, and keep
! running, rather than abort.
!
! The return status is "normal" only if the file variable exists,
! and the rank and (optionally) dim sizes are determined without
! error.
!
!-----------------------------------------------------------------------------

module getnc__dim_sizes
   private
   public getnc_dim_sizes
contains

subroutine getnc_dim_sizes (infile, varname, diag, rank, status, &
      file_missing, var_missing, dim_sizes)
   use netcdf
   use stdlit, only : normal, fail
   implicit none

! Input arguments.

   character(*),   intent (in)  :: infile	  ! input file name
   character(*),   intent (in)  :: varname	  ! requested var name
   integer,        intent (in)  :: diag		  ! verbosity level, 0-N

! Output arguments.

   integer,        intent (out) :: rank		  ! no. of dimensions, 0=scalar
   integer,        intent (out) :: status	  ! normal or fail
   logical,        intent (out) :: file_missing	  ! status flags
   logical,        intent (out) :: var_missing
   integer, optional, &
      allocatable, intent (out) :: dim_sizes(:)	  ! dim sizes if not scalar

! Local variables.

   integer i, ncid, varid, dimids(9)
   integer status_nc, status_ignore
   logical ex

! Initialize.

   if (diag >= 4) print *, 'getnc_dim_sizes: Start.'

! Default all return codes to fail, until operations complete.
! Support multiple error return points.

   rank         = -99
   status       = fail
   file_missing = .true.
   var_missing  = .true.

! Check for file not found, for best message.

   inquire (file=infile, exist=ex)

   if (.not. ex) then
      if (diag >= 3) print '(2a)', ' *** getnc_dim_sizes: File not found: ', &
         trim (infile)
      return
   end if

! Open input file.

   if (diag >= 4) print '(2a)', '  Open input file: ', trim (infile)

   status_nc = nf90_open (infile, nf90_nowrite, ncid)

   if (check_nc (status_nc, 'nf90_open', diag)) return    ! soft error, return

   if (diag >= 4) print *, '    ncid = ', ncid
   file_missing = .false.

! Look up the var name in the input file.

   if (diag >= 4) print *, '  Look up var name in file: ' // trim (varname)

   status_nc = nf90_inq_varid (ncid, varname, varid)

   if (check_nc (status_nc, 'nf90_inq_varid', diag)) then   ! soft error, return
      status_ignore = nf90_close (ncid)
      return
   end if

   if (diag >= 4) print *, '    Var ID for ' // trim (varname) // ' = ', varid
   var_missing = .false.

! Get number of dimensions and dimension ID's for current variable.

   if (diag >= 4) print *, "  Get rank and dimension ID's for this variable."

   status_nc = nf90_inquire_variable (ncid, varid, ndims=rank, dimids=dimids)

   if (check_nc (status_nc, 'nf90_inquire_variable', diag)) then
      status_ignore = nf90_close (ncid)			! soft error, return
      return
   end if

   if (diag >= 4) print *, '    Number of dimensions = ', rank

! If scalar, then normal return, and do not try to read any dimension sizes.

   if (rank == 0) then
      status_ignore = nf90_close (ncid)
      status = normal
      return
   end if

! If dim sizes not requested, then normal return, and do not try to read them.

   if (.not. present (dim_sizes)) then
      status_ignore = nf90_close (ncid)
      status = normal
      return
   end if

! Read dimension sizes for current variable.

   if (diag >= 4) print *, '  Read dimension sizes.'

   allocate (dim_sizes(rank))
   dim_sizes = -99				! default to error signal

   do i = 1, rank
      status_nc = nf90_inquire_dimension (ncid, dimids(i), len=dim_sizes(i))
      if (check_nc (status_nc, 'nf90_inquire_dimension', diag)) then
         status_ignore = nf90_close (ncid)		! soft error, return
         return
      end if
   end do

   if (diag >= 4) print '(a,999(2x,i0))', '    Var dimensions in file =', &
      dim_sizes(1:rank)

   status_ignore = nf90_close (ncid)
   status = normal				! normal return with dim sizes

end subroutine getnc_dim_sizes


!-----------------------------------------------------
! Low-level soft error handler for Netcdf errors.
!-----------------------------------------------------

function check_nc (status_nc, op_name, diag) result (error_flag)
   use netcdf
   implicit none

   integer,      intent (in) :: status_nc	! netcdf status input
   character(*), intent (in) :: op_name		! netcdf operation name
   integer,      intent (in) :: diag		! verbosity level, 0-N

   logical error_flag				! function result:
   						! true = error, false = normal
                                                ! for if (*) return constructs
! Check Netcdf error code.

   error_flag = (status_nc /= nf90_noerr)	! func result = true if error

! If error detected, print diagnostics.

   if (error_flag .and. (diag >= 3) ) then
      print *, '*** getnc_dim_sizes: Netcdf error on ' // trim (op_name) // ':'
      print *, '*** ' // trim (nf90_strerror (status_nc))
   end if					! show the library error message

! Return to caller with function result either true or false.
! All Netcdf errors are soft errors in this version.

end function check_nc

end module getnc__dim_sizes
