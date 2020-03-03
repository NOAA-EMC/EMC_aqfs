!------------------------------------------------------------------------------
!
! read_netcdf_var.f90 -- Generic reader for single Netcdf variables.
!
! This routine reads a whole array variable from a Netcdf file.
! Multiple dimensions are supported.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!		This version supports only numeric arrays, up to 4 dimensions.
! 2014-may-13	Bug fix.  Close the old input file before opening a new one.
!		Fix file open/close state memory, and diagnostics.
!		Adjust verbosity for 2 = sparse progress display.
! 2014-jun-18	Add reader for 2-D character variables (1-D strings).
! 2014-jun-26	Fix illegal use of shape function.
!
! Input:   infile = Path to Netcdf input file
!	   varname = Name of requested variable.  Case sensitive.
!          diag = Verbosity control, 0 = errors only.  See below for more.
!
! Output:  out = Caller's output array, any desired rank.
!	   status = Simplified operation status, as defined in stdlit.f90:
!		    normal = success, fail = any of several errors.
!
! Notes:
!
! In a single call, this routine reads the specified array
! variable from a Netcdf file.  The whole array is read.
!
! All numeric data types are read generically as type double.
! The rank of the requested file variable must match the caller's
! output array.
!
! The output array is auto-allocated, as needed.  However, auto-
! matic reallocation is not supported.  A previously allocated
! caller's array is not deallocated.  This is done for efficiency
! and consistency checking.
!
! Subsequent reads into the same array must have the same array
! dimensions, otherwise an error is generated.  To recycle the
! same array with changing dimensions, the caller must deallocate
! the array between calls.
!
! Character variables are also supported.  They are reshaped into
! fortran strings on input, with rank reduced by one.
!
! For efficiency, the input file is left open after reading.
! Subsequent reads from the same file will utilize the existing
! open file, until a different file name is requested.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = sparse,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
! Error handling:
!
! This version treats most errors as soft errors.  An error
! message is printed, and "fail" status is returned to the caller.
!
! Limitations:
!
! Array subsetting is not supported.  This is to keep the
! interface simple.
!
! Reallocation (automatic dealloc/realloc) is not currently
! supported.  This version uses fortran 95 allocation semantics,
! not fortran 2003 reallocation.  The intention is allocate the
! main data array only once, then re-use it, for efficiency.
!
! Dimension reordering is not supported.  Dimension names and
! coordinate variables are ignored.  If coordinate variables are
! needed, they should be read independently as separate named
! variables.
!
! Missing value support is not currently included.  Input data
! are assumed to contain all valid data, no missing values.
! Alternatively, the caller may use an assumed missing value
! strategy.
!
! No support for reading attributes is currently included.
!
!------------------------------------------------------------------------------

!-------------------------------------------------
! Module structure and state variables.
!-------------------------------------------------

module read__netcdf_var

   use config, only : dp		! common module linkage
   use netcdf				! for all the internal routines
   use stdlit, only : normal, fail
   implicit none

   private				! visibility
   public read_netcdf_var		! only the generic routine is public

! State variables for this module, saved between public calls.

   character(200) :: previous_file = ' '	! remember open file name
   integer        :: ncid = 0			! remember open file ID

! Internal state variables.  These are used between internal
! routines only, not between public calls.

   character alloc_msg*200			! service return vars
   integer varid, rank, dims(9), alloc_status
   logical need_allocate

! Shared variables for convenience.  Not used between routines.

   integer vshape(9)

! Local named literals.

   character(1), parameter :: null = char (0)	! literal for null character
   logical, parameter :: string_flag = .true.	! string var indicator

! Generic interface.

   interface read_netcdf_var
      module procedure &
         read_netcdf_var_1d, read_netcdf_var_2d, &
         read_netcdf_var_3d, read_netcdf_var_4d, &
         read_netcdf_text_1d
   end interface read_netcdf_var

contains


!-------------------------------------------------
! Specific reader for 1-D array.
!-------------------------------------------------

! Documentation for all of the specific numeric routines is
! clustered in this single instance.

subroutine read_netcdf_var_1d (infile, varname, diag, vdata, status)
   implicit none

! Input arguments.

   character(*), intent(in) :: infile			! input file name
   character(*), intent(in) :: varname			! requested var name
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real(dp),     intent(inout), allocatable :: vdata(:)	! data array
   integer,      intent(out)                :: status	! result status, normal
   							!   or fail (stdlit)

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

   call read_check (nf90_get_var (ncid, varid, vdata), diag, status)

end subroutine read_netcdf_var_1d


!-----------------------------------------------------------------------------
! Specific readers.  For documentation, see the 1-D case above.
!-----------------------------------------------------------------------------

subroutine read_netcdf_var_2d (infile, varname, diag, vdata, status)
   implicit none

   character(*), intent(in) :: infile
   character(*), intent(in) :: varname
   integer,      intent(in) :: diag

   real(dp), intent(inout), allocatable :: vdata(:,:)
   integer,  intent(out)                :: status

   if (allocated (vdata)) vshape(1:2) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:2), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncid, varid, vdata), diag, status)
end subroutine read_netcdf_var_2d

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_3d (infile, varname, diag, vdata, status)
   implicit none

   character(*), intent(in) :: infile
   character(*), intent(in) :: varname
   integer,      intent(in) :: diag

   real(dp), intent(inout), allocatable :: vdata(:,:,:)
   integer,  intent(out)                :: status

   if (allocated (vdata)) vshape(1:3) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:3), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2),dims(3)), stat=alloc_status, &
         errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncid, varid, vdata), diag, status)
end subroutine read_netcdf_var_3d

!-----------------------------------------------------------------------------

subroutine read_netcdf_var_4d (infile, varname, diag, vdata, status)
   implicit none

   character(*), intent(in) :: infile
   character(*), intent(in) :: varname
   integer,      intent(in) :: diag

   real(dp), intent(inout), allocatable :: vdata(:,:,:,:)
   integer,  intent(out)                :: status

   if (allocated (vdata)) vshape(1:4) = shape (vdata)

   if (read_setup (infile, varname, diag, allocated (vdata), vshape(1:4), &
      status)) return

   if (need_allocate) then
      allocate (vdata(dims(1),dims(2),dims(3),dims(4)), stat=alloc_status, &
         errmsg=alloc_msg)
      if (alloc_check (diag)) return
   end if

   call read_check (nf90_get_var (ncid, varid, vdata), diag, status)
end subroutine read_netcdf_var_4d


!-----------------------------------------------------------------------------
! Readers for character variables.
!-----------------------------------------------------------------------------

! Read 2-D file character variable into 1-D fortran string array.

! This version supports only string lengths that match exactly
! between file and program, in other words fixed string lengths.

subroutine read_netcdf_text_1d (infile, varname, diag, strings, status)
   implicit none

   character(*), intent(in) :: infile
   character(*), intent(in) :: varname
   integer,      intent(in) :: diag

   character(*), intent(inout), allocatable :: strings(:)
   integer,      intent(out)                :: status

   if (allocated (strings)) vshape(1:1) = shape (strings)

   if (read_setup (infile, varname, diag, allocated (strings), vshape(1:1), &
      status, len (strings))) return		! extra arg for string length

! Dim 1 is the string length dimension, dim 2 is the data dimension.
! Drop the length dimension, it is handled implicitly from here onward.

   if (need_allocate) then
      allocate (strings(dims(2)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag, string_flag)) return
   end if

! The Netcdf library automatically maps file dimension #1 into the fortran
! string length.

   call read_check (nf90_get_var (ncid, varid, strings), diag, status)
end subroutine read_netcdf_text_1d


!-----------------------------------------------------------------------------
! Generic support routines.  Internal use only.
!-----------------------------------------------------------------------------

! Note: These routines make use of several module variables
! for status exchange, in addition to their calling arguments.


!-------------------------------------------------
! Netcdf read setup for the specific routines.
!-------------------------------------------------

function read_setup (infile, varname, diag, prev_alloc, var_shape, status, &
      len_caller) result (error_flag)
   implicit none

! Input arguments.

   character(*), intent (in ) :: infile		  ! input file name
   character(*), intent (in ) :: varname	  ! requested var name
   integer,      intent (in ) :: diag		  ! diag verbosity level, 0-N
   logical,      intent (in ) :: prev_alloc	  ! previous allocation status
   integer,      intent (in ) :: var_shape(:)	  ! caller's array dimensions
						  ! (undefined if not allocated)
! Output arguments.

   integer,      intent (out) :: status		  ! final status for main caller

! Optional argument for string reads only.

   integer, intent (in), optional :: len_caller	  ! caller's string length

! Function result.

   logical error_flag			! false = success,
					! true = soft error & return to main
! Local variables.

   character fmt1*50
   integer i, ddim1, file_rank, status_nc
   integer dimids(9), prev_dims(9)
   logical ex

! Initialize.

   if (diag >= 4) print *, 'read_netcdf_var: Start.'
   if (diag >= 3) print *, '   Read ', trim (varname)

! Determine expected dimensions.  Compensate for extra string length dimension.
! "rank" is the rank of the file var, not caller's array rank, if character.

   dims(:)      = -99			! init memory and trap errors
   prev_dims(:) = -99

   rank  = size (var_shape)		! expected rank same as caller's array
   ddim1 = 1				! file data subscripts start at 1

   if (present (len_caller)) then	! adjust for extra string length dim:
      rank  = rank + 1			!   expect one extra dimension
      ddim1 = 2				!   file data subscripts start at 2
      prev_dims(1) = len_caller		!   first dimension = string length
   end if

   if (prev_alloc) then				! only if allocated:
      prev_dims(ddim1:rank) = var_shape(:)	! copy primary dim sizes
   end if

! Default both return codes to operation failed, until all operations
! complete.  Support multiple error return points.

   status     = fail			! status code to main caller (stdlit)
   error_flag = .true.			! abort flag to specific interfaces

! Check for input file already open.

   if (infile == previous_file) then
      if (diag >= 4) print *, '  File already open: ' // trim (infile)

! Check for file not found, for best message.

   else
      inquire (file=infile, exist=ex)

      if (.not. ex) then
         if (diag >= 1) print '(2a)', ' *** File not found: ', trim (infile)
         return
      end if

! File exists and is different.  Close previous Netcdf file.

      if (ncid /= 0) then				! zero = no open file
         if (diag >= 4) print *, '  Close previous Netcdf file.'

         if (check_nc (nf90_close (ncid), 'nf90_close')) then
            print *, '*** Serious error.  Abort.'
            call exit (1)
         end if

         ncid = 0					! remember file closed
      end if

! Open the requested Netcdf input file.

      if (diag >= 4 .and. previous_file == ' ') &	! first time only
         print *, 'Netcdf library version = ' // trim (nf90_inq_libvers())

      if (diag >= 4) print '(2a)', ' Open input file: ', trim (infile)

      status_nc = nf90_open (infile, nf90_nowrite, ncid)

      if (check_nc (status_nc, 'nf90_open')) then
         previous_file = 'xxx xxx'		! prevent filename match
         ncid = 0				! mark as file closed, even if
         					!   in strange semi-open state
         return					! soft error, return
      end if

      previous_file = infile				! remember open file

      if (diag >= 4) print *, '  ncid = ', ncid
   end if

! Look up the var name in the input file.

   if (diag >= 4) print *, '  Look up var name in file: ' // trim (varname)

   status_nc = nf90_inq_varid (ncid, varname, varid)

   if (check_nc (status_nc, 'nf90_inq_varid')) return	! soft error, return

   if (diag >= 4) print *, '    Var ID for ' // trim (varname) // ' = ', varid

! Get number of dimensions for current variable.

   if (diag >= 4) print *, '  Get number of dimensions for this variable.'

   status_nc = nf90_inquire_variable (ncid, varid, ndims=file_rank)

   if (check_nc (status_nc, 'nf90_inquire_variable')) return	! soft error

   if (diag >= 4) print *, '    Number of dimensions = ', file_rank

! Consistency check for number of dimensions.

   if (file_rank /= rank) then
      print *, '*** read_netcdf_var: Incorrect number of dimensions for "' &
         // trim (varname) // '" in file.'
      print '(a,i0)', ' *** Number of dimensions in file  = ', file_rank
      print '(a,i0)', ' *** Number of dimensions expected = ', rank
      return						! soft error, return
   end if

! Read dimension ID's for current variable.

   if (diag >= 4) print *, '  Get dimension ID''s.'

   status_nc = nf90_inquire_variable (ncid, varid, dimids=dimids(1:file_rank))

   if (check_nc (status_nc, 'nf90_inquire_variable')) return	! soft error

! Read dimension sizes for current variable.

   if (diag >= 4) print *, '  Read dimension sizes.'

   do i = 1, file_rank
      status_nc = nf90_inquire_dimension (ncid, dimids(i), len=dims(i))

      if (check_nc (status_nc, 'nf90_inquire_dimension')) return   ! soft error
   end do

   if (diag >= 4) print '(a,999(2x,i0))', '     Var dimensions in file =', &
      dims(1:file_rank)

! Check length dimension for string reads.
! Length is not allocatable in fortran 90/95 semantics.

   if (present (len_caller)) then
      if (dims(1) /= len_caller) then
         print '(2a)', '*** read_netcdf_var: First dimension in file must', &
            ' match expected string length.'
         fmt1 = '(a,999(2x,i0))'
         print fmt1,   '*** Expected string length  = ', len_caller
         print fmt1,   '*** First dimension in file = ', dims(1)
         print '(2a)', '*** Ignore current variable, and continue running.'
         return						! soft error, return
      end if
   end if

! Set up for main array allocate statement.
! Check for conforming array dimensions.  Special test for robustness.
! May need to be optionable in a later version.

   need_allocate = .not. prev_alloc		! enable caller's allocate

   if (prev_alloc) then
      if (any (dims(1:rank) /= prev_dims(1:rank))) then
         print *, '*** read_netcdf_var: Inconsistent dimensions between files.'
         fmt1 = '(a,999(2x,i0))'
         print fmt1, ' *** Previous dimension sizes    = ', prev_dims(1:rank)
         print fmt1, ' *** Current file var dimensions = ', dims(1:rank)
         print *, '*** Ignore current variable, and continue running.'
         return						! soft error, return
      end if

      if (diag >= 4) print *, '  Allocate data array.'
   end if

! Setup for main allocate is now complete.  Return continue status.
! The specific routine will next allocate the main data array.

   error_flag = .false.

! Note that the user status "status" remains set to the abort status,
! because the whole read operation is not yet finished.

end function read_setup


!-------------------------------------------------
! Check result from the main array allocation.
!-------------------------------------------------

function alloc_check (diag, str_flag) result (error_flag)
   implicit none

   integer, intent (in)           :: diag	! verbosity level, 0-N
   logical, intent (in), optional :: str_flag	! *PRESENT* for string reads

   logical error_flag			! function result: false = success,
					! true = soft error & return to main

! Check allocate result, and set return flag accordingly.

   error_flag = (alloc_status /= 0)	! success = status 0 = flag FALSE

   if (error_flag) then
      print *, '*** read_netcdf_var: Allocate error for data array.'
      print *, '*** Compiler run-time error message:'
      print *, '*** ' // trim (alloc_msg)

      if (present (str_flag)) then
         print '(a,i0,a,999(2x,i0))', ' *** String length = ', dims(1), &
            ', dimension sizes =', dims(2:rank)
      else
         print '(a,999(2x,i0))',      ' *** Dimension sizes =', dims(1:rank)
      end if

      print *, '*** Assume bad file dimensions.'
      print *, '*** Ignore current variable, and continue running.'

! Allocation successful.  Normal return, error flag = false

   else
      if (diag >= 4) print *, '  Read var data.'  ! progress display for next op
   end if

! Return to caller, either way.  If error status is returned, this
! is treated as a soft error.

! Note that the user status "status" remains set to the abort status,
! because the whole read operation is not yet complete.

end function alloc_check


!-------------------------------------------------
! Check result from main array read operation.
!-------------------------------------------------

subroutine read_check (status_nc, diag, status)
   implicit none

   integer, intent (in ) :: status_nc		! Netcdf read array status
   integer, intent (in ) :: diag		! verbosity level, 0-N
   integer, intent (out) :: status		! status to main caller

! Default the user return code to operation failed, until all checks
! are complete.

   status = fail

   if (check_nc (status_nc, 'nf90_get_var')) return	! soft error, return

! All checks are now complete.  Set success status, and return.

   status = normal

   if (diag >= 4) print *, 'read_netcdf_var: Return.'
   if (diag >= 4) print *

end subroutine read_check


!-------------------------------------------------
! Low-level soft error handler for Netcdf errors.
!-------------------------------------------------

function check_nc (status_nc, op_name) result (error_flag)
   implicit none

   integer,      intent (in) :: status_nc	! netcdf status input
   character(*), intent (in) :: op_name		! netcdf operation name

   logical error_flag				! function result:
   						! true = error, false = normal
                                                ! for if (*) return constructs
! Check Netcdf error code.

   error_flag = (status_nc /= nf90_noerr)	! func result = true if error

! If error detected, print diagnostics.

   if (error_flag) then
      print *, '*** read_netcdf_var: Netcdf error on ' // trim (op_name) // ':'
      print *, '*** ' // trim (nf90_strerror (status_nc))
   end if					! show the library error message

! Return to caller with function result either true or false.
! All Netcdf errors are soft errors in this version.

end function check_nc

end module read__netcdf_var
