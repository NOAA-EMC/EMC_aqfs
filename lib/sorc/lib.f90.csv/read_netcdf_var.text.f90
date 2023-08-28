!------------------------------------------------------------------------------
!
! read_netcdf_var.text.f90 -- Specific reader for character variables.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
! 2022-apr-19	Minor.  Adjust verbosity levels.
!
!------------------------------------------------------------------------------

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
      if (diag >= 5) print *, "    First allocation for caller's string array."
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

   if (diag >= 5) print *, '    Second allocation for string input buffer.'

   allocate (inbuf(dims(2)), stat=alloc_status, errmsg=alloc_msg)
   if (alloc_check (diag, len_file)) return

   if (diag >= 5) then
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
