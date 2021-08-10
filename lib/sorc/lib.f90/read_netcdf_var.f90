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
! 2014-dec-20	Add missing value support.
! 2014-dec-20	Add reader for var dimension names.
! 2014-dec-24	Add support for character variables of unknown length.
!		F2003 deferred length character variables are not sufficiently
!		  supported through gfortran 4.9.2.
!		So use F90-style specification expression, instead.
!		Include string truncation check.
!
! 2015-nov-16	Add optional return arguments for file and variable ID's,
!		  to support reading attributes, etc.
!
! 2017-mar-27	Minor.  Fix line wrapping in diagnostic print, with ifort.
!
! Input:   infile = Path to Netcdf input file
!	   varname = Name of requested variable.  Case sensitive.
!          diag = Verbosity control, 0 = errors only.  See below for more.
!
! Output:  out = Caller's output array, any desired rank.
!	   status = Simplified operation status, as defined in stdlit.f90:
!		    normal = success, fail = any of several errors.
!	   Supplemental outputs, see read_netcdf_var_1d code for details.
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
! fortran strings on input, with rank reduced by one.  Trailing
! nulls are removed.  Mismatched string lengths are handled
! correctly.
!
! For efficiency, the input file is left open after reading.
! Subsequent reads from the same file will utilize the existing
! open file, until a different file name is requested.
!
! If present in the file, the missing_value or _FillValue
! attribute value is returned in the optional vmiss argument.
! If not present, the value -huge() is returned, providing a safe
! test value either way.
!
! You may omit or ignore the vmiss argument if you assume that
! the input data contains all valid data, no missing values.
! Alternatively, you may use an assumed missing value strategy,
! with caution.
!
! In this version, missing value support and dimension names are
! provided for numeric variables only, not text variables.
!
! The Netcdf file ID and current variable ID are returned in
! optional arguments nc_id and var_id.  They may be used by the
! caller to access attributes, and for other purposes.
!
! The dimension names of the requested variable are also returned
! in the optional dim_names argument.
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
! On any soft error return, all return arguments other than
! "status" are undefined and possibly unallocated.
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
! Dimension reordering is not supported.  Coordinate variables
! are ignored.  If coordinate variables are needed, they should
! be read independently as separate named variables.
!
! Aside from the missing value, no other support for reading
! attributes is currently included.
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
   integer        :: ncidi = 0			! remember open file ID

! Internal state variables.  These are used between internal
! routines only, not between public calls.

   character alloc_msg*200
   integer varidi, rank, dimids(9), dims(9), alloc_status
   logical need_allocate

! Shared variables for convenience.  Not used between routines.

   integer vshape(9)

! Local named literals.

   character(1), parameter :: null  = char (0)		! char constants
   character(1), parameter :: blank = ' '
   logical,      parameter :: string_flag = .true.	! string var indicator

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

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, vmiss, &
      nc_id, var_id, dim_names)

end subroutine read_netcdf_var_1d


!-----------------------------------------------------------------------------
! Specific readers.  For documentation, see the 1-D case above.
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

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, vmiss, &
      nc_id, var_id, dim_names)
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

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, vmiss, &
      nc_id, var_id, dim_names)
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

   call read_check (nf90_get_var (ncidi, varidi, vdata), diag, status, vmiss, &
      nc_id, var_id, dim_names)
end subroutine read_netcdf_var_4d


!-----------------------------------------------------------------------------
! Readers for character variables.
!-----------------------------------------------------------------------------

! Read 2-D file character variable into 1-D fortran string array.
! Mismatched string lengths are supported.

subroutine read_netcdf_text_1d (infile, varname, diag, strings, status)
   implicit none

   character(*), intent (in) :: infile
   character(*), intent (in) :: varname
   integer,      intent (in) :: diag

   character(*), intent (inout), allocatable :: strings(:)
   integer,      intent (out)                :: status

   integer len_caller, len_file		! local vars

   len_caller = len (strings)

   if (allocated (strings)) vshape(1:2) = (/ len_caller, shape (strings) /)
   				! provide string length as the first dimension

   if (read_setup (infile, varname, diag, allocated (strings), vshape(1:2), &
      status, string_flag)) return

! First allocation, caller's string output array.
! Keep caller's original string length, allocate only the array dimensions.

   if (need_allocate) then
      if (diag >= 4) print *, "    First allocation for caller's string array."
      allocate (strings(dims(2)), stat=alloc_status, errmsg=alloc_msg)
      if (alloc_check (diag, len_caller)) return
   end if

! Must use subroutine to allocate input buffer for unknown length
! character strings, without full support for F2003 deferred length
! strings.  (Through gfortran 4.9.2, maybe later.)

   len_file = dims(1)			! char length dimension from file
   call read_part2_text_1d (len_file, diag, strings, status)

end subroutine read_netcdf_text_1d

!-----------------------------------------------------------------------------

subroutine read_part2_text_1d (len_file, diag, strings, status)
   implicit none

   integer,      intent (in   ) :: len_file	! string length in file
   integer,      intent (in   ) :: diag

   character(*), intent (inout) :: strings(:)
   integer,      intent (inout) :: status

! Character input buffer emulating "dynamic" string length,
! via "specification expression".

   character(len_file), allocatable :: inbuf(:)

! Local variables.

   character fmt1*30
   integer i, j, ntrunc

! Second allocation, temp input buffer for the original character variable.
! The string length dimension is now implicit.  Dim 2 is the data dimension.

   if (diag >= 4) print *, '    Second allocation for string input buffer.'

   allocate (inbuf(dims(2)), stat=alloc_status, errmsg=alloc_msg)
   if (alloc_check (diag, len_file)) return

   if (diag >= 4) then
      fmt1 = '(a,9(2x,i0))'
      print fmt1, '       Allocated string length =', len (inbuf)
      print fmt1, '       Allocated dim sizes     =', shape (inbuf)
   end if

! Read in the entire original character variable.  The Netcdf library
! automatically maps file dimension #1 into the fortran string length.

   call read_check (nf90_get_var (ncidi, varidi, inbuf), diag, status)
   if (status /= normal) return

! Trim trailing nulls, and truncate as needed to fit caller's string array.
! Trailing nulls and blanks are converted to all blanks.
! However, embedded nulls are left unchanged.

   strings(:) = ' '				! init output strs to all blanks
   ntrunc = 0

string_loop: &
   do i = 1, dims(2)				! loop over all strings

! Scan characters backward from end of string.
! Start at len_trim for efficiency.  Works correctly regardless of how
! len_trim handles nulls (indeterminate).
! Also works correctly when len_trim = 0, because output strings were cleared.

char_loop: &
      do j = len_trim (inbuf(i)), 1, -1

! Ignore trailing blanks and nulls.
! This leaves all trailing nulls as blanks in the output strings.

         if (any (inbuf(i)(j:j) == (/ blank, null /)) ) cycle char_loop

! Here for all significant leading characters, 1 through j.
! If file string length exceeds caller's length, truncate and print warnings.

         if (j > len (strings)) then
            ntrunc = ntrunc + 1			! count each truncated string
            if (diag >= 2) print '(999a)', '*** String truncated from "', &
               inbuf(i)(1:j), '" to "', inbuf(i)(1:len (strings)), '"'
         end if

! Now we can shortcut the char loop.
! Copy all significant leading characters to output string, efficiently.

         strings(i) = inbuf(i)(1:j)	! truncates any longer than output strs
					!   and does not copy more than wanted

         exit char_loop			! further scanning not needed
      end do char_loop

   end do string_loop

! Final check for truncated strings.

   if (ntrunc > 0 .and. diag >= 2) print '(3(a,i0))', '*** Number of' &
      // ' truncated strings = ', ntrunc, ' out of a total of ', dims(2), '.'

! Return with status = normal, from read_check above.

end subroutine read_part2_text_1d


!-----------------------------------------------------------------------------
! Generic support routines.  Internal use only.
!-----------------------------------------------------------------------------

! Note: These routines make use of several module variables
! for status exchange, in addition to their calling arguments.


!-------------------------------------------------
! Netcdf read setup for the specific routines.
!-------------------------------------------------

function read_setup (infile, varname, diag, prev_alloc, var_shape, status, &
      str_flag) result (error_flag)
   implicit none

! Input arguments.

   character(*),     intent (in ) :: infile	  ! input file name
   character(*),     intent (in ) :: varname	  ! requested var name
   integer,          intent (in ) :: diag	  ! diag verbosity level, 0-N
   logical,          intent (in ) :: prev_alloc	  ! previous allocation status
   integer,          intent (in ) :: var_shape(:) ! caller's array dimensions;
						  ! undefined if not allocated;
   						  ! char dim #1 = string length
   logical, optional,intent (in ) :: str_flag	  ! *PRESENT* for string reads

! Output arguments.

   integer,          intent (out) :: status	  ! final status for main caller

! Function result.

   logical error_flag			! false = success,
					! true = soft error & return to main
! Local variables.

   character fmt1*50
   integer i, file_rank, ddim1, status_nc
   logical ex

! Initialize.

   if (diag >= 4) print *, 'read_netcdf_var: Start.'
   if (diag >= 3) print *, '   Read ', trim (varname)

! Determine expected dimensions.

! For character variables, dim #1 is the string length dimension.
! For character, "rank" is the rank of the file var, not caller's array rank.

   dims(:)      = -99			! init memory and trap errors
   rank = size (var_shape)		! expected rank of file variable

! Default both return codes to operation failed, until all operations
! complete.  Support multiple error return points.

   status     = fail			! status code to main caller (stdlit)
   error_flag = .true.			! abort flag to specific interfaces

! Check for input file already open.

file_check: &
   if (infile == previous_file) then
      if (diag >= 4) print '(2a)', '   File already open: ', trim (infile)

! Check for file not found, for best message.

   else
      inquire (file=infile, exist=ex)

      if (.not. ex) then
         if (diag >= 1) print '(2a)', ' *** File not found: ', trim (infile)
         return
      end if

! File exists and is different.  Close previous Netcdf file.

      if (ncidi /= 0) then				! zero = no open file
         if (diag >= 4) print *, '  Close previous Netcdf file.'

         if (check_nc (nf90_close (ncidi), 'nf90_close')) then
            print *, '*** Serious error.  Abort.'
            call exit (1)
         end if

         ncidi = 0					! remember file closed
      end if

! Open the requested Netcdf input file.

      if (diag >= 4 .and. previous_file == ' ') &	! first time only
         print *, 'Netcdf library version = ' // trim (nf90_inq_libvers())

      if (diag >= 4) print '(2a)', ' Open input file: ', trim (infile)

      status_nc = nf90_open (infile, nf90_nowrite, ncidi)

      if (check_nc (status_nc, 'nf90_open')) then
         previous_file = 'xxx xxx'		! prevent filename match
         ncidi = 0				! mark as file closed, even if
         					!   in strange semi-open state
         return					! soft error, return
      end if

      previous_file = infile				! remember open file

      if (diag >= 4) print *, '  ncidi = ', ncidi

   end if file_check

! Look up the var name in the input file.

   if (diag >= 4) print *, '  Look up var name in file: ' // trim (varname)

   status_nc = nf90_inq_varid (ncidi, varname, varidi)

   if (check_nc (status_nc, 'nf90_inq_varid')) return	! soft error, return

   if (diag >= 4) print *, '    Var ID for ' // trim (varname) // ' = ', varidi

! Get number of dimensions for current variable.

   if (diag >= 4) print *, '  Get number of dimensions for this variable.'

   status_nc = nf90_inquire_variable (ncidi, varidi, ndims=file_rank)

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

   status_nc = nf90_inquire_variable (ncidi, varidi, dimids=dimids(1:file_rank))

   if (check_nc (status_nc, 'nf90_inquire_variable')) return	! soft error

! Read dimension sizes for current variable.

   if (diag >= 4) print *, '  Read dimension sizes.'

   do i = 1, file_rank
      status_nc = nf90_inquire_dimension (ncidi, dimids(i), len=dims(i))

      if (check_nc (status_nc, 'nf90_inquire_dimension')) return   ! soft error
   end do

   if (diag >= 4) print '(a,999(2x,i0))', '     Var dimensions in file =', &
      dims(1:file_rank)

! Set up for main array allocate statement.
! Check for conforming array dimensions.  Special test for robustness.
! May need to be optionable in a later version.

   need_allocate = .not. prev_alloc	! enable caller's allocate

   ddim1 = 1				! for character reads...
   if (present (str_flag)) ddim1 = 2	! ignore differences in length dimension

   if (prev_alloc) then
      if (any (dims(ddim1:rank) /= var_shape(ddim1:rank))) then
         print *, '*** read_netcdf_var: Inconsistent dimensions between files.'
         fmt1 = '(a,999(2x,i0))'
         print fmt1, ' *** Previous dimension sizes    = ', var_shape(1:rank)
         print fmt1, ' *** Current file var dimensions = ', dims(1:rank)
         if (present (str_flag)) print *, &
            '***   (Difference in string length dimension #1 is ignored.)'
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

function alloc_check (diag, str_len) result (error_flag)
   implicit none

   integer, intent (in)           :: diag	! verbosity level, 0-N
   integer, intent (in), optional :: str_len	! length, for string vars only

   logical error_flag			! function result: false = success,
					! true = soft error & return to main

! Check allocate result, and set return flag accordingly.

   error_flag = (alloc_status /= 0)	! success = status 0 = flag FALSE

   if (error_flag) then
      print *, '*** read_netcdf_var: Allocate error for data array.'
      print *, '*** Compiler run-time error message:'
      print *, '*** ' // trim (alloc_msg)

      if (present (str_len)) then
         print '(a,i0,a,999(2x,i0))', ' *** String length = ', str_len, &
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


!-------------------------------------------------------------------
! Check result from main array read operation.
! Also fetch the missing value and dimension names, if requested.
!-------------------------------------------------------------------

subroutine read_check (status_nc, diag, status, vmiss, nc_id, var_id, dim_names)
   implicit none

   integer,      intent(in )           :: status_nc	! Netcdf read var status
   integer,      intent(in )           :: diag	  	! verbosity level, 0-N
   integer,      intent(out)           :: status	! status to main caller

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

      if (diag >= 4) print *, '  Read missing_value attribute.'
      att_status = nf90_get_att (ncidi, varidi, 'missing_value', vmiss)

! Check and report unexpected Netcdf error.  Silent for att not found.

      if (att_status /= nf90_enotatt) then		! soft error in any case
         dummy = check_nc (att_status, 'nf90_get_att (missing_value)')
      end if

! If no missing_value, or error, then try the alternate attribute name.

      if (att_status /= nf90_noerr) then

         if (diag >= 4) print *, '  Read _FillValue attribute.'
         att_status = nf90_get_att (ncidi, varidi, '_FillValue', vmiss)

! Check and report unexpected Netcdf error.  Silent for att not found.

         if (att_status /= nf90_enotatt) then		! soft error in any case
            dummy = check_nc (att_status, 'nf90_get_att (_FillValue)')
         end if

! Return a "safe" missing value if neither attribute is present,
! or on unexpected Netcdf error.

         if (att_status /= nf90_noerr) vmiss = -huge (vmiss)
      end if

   end if check_missing

! If requested, return optional Netcdf file and variable ID's.

   if (present (nc_id))  nc_id  = ncidi		! copy internal Netcdf ID
   if (present (var_id)) var_id = varidi	! copy internal var ID

! If requested, read dimension names for current variable.

   if (present (dim_names)) then

      if (diag >= 4) print *, '  Read dimension names.'

      do i = 1, rank
         name_in = ' '				! blank fill first, to be safe
         dim_status = nf90_inquire_dimension (ncidi, dimids(i), name_in)

         dummy = check_nc (dim_status, 'nf90_inquire_dimension')
				! report unexpected Netcdf error; treat as soft

         if (diag >= 4) print '(a,i0,2a)', '     Dim #', i, ' = ', trim(name_in)

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
