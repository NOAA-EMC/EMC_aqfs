!------------------------------------------------------------------------------
!
! read_netcdf_var.setup.f90 -- Generic setup routine for the specific readers.
!
! This is a component include file for module read_netcdf_var.f90.
! For internal use only.  See main module file for documentation.
!
! 2022-apr-02	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_netcdf_var.f90.
! 2022-apr-19	Minor.  Adjust verbosity levels.
!
! Note: The generic support routines use several module variables
! for status exchange, in addition to their calling arguments.
!
!------------------------------------------------------------------------------

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

   if (diag >= 5) print *, 'read_netcdf_var: Start.'
   if (diag >= 4) print *, '   Read ', trim (varname)

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
      if (diag >= 5) print '(2a)', '   File already open: ', trim (infile)

! Check for file not found, for best message.

   else
      inquire (file=infile, exist=ex)

      if (.not. ex) then
         if (diag >= 1) print '(2a)', ' *** File not found: ', trim (infile)
         return
      end if

! File exists and is different.  Close previous Netcdf file.

      if (ncidi /= 0) then				! zero = no open file
         if (diag >= 5) print *, '  Close previous Netcdf file.'

         if (check_nc (nf90_close (ncidi), 'nf90_close')) then
            print *, '*** Serious error.  Abort.'
            call exit (1)
         end if

         ncidi = 0					! remember file closed
      end if

! Open the requested Netcdf input file.

      if (diag >= 5 .and. previous_file == ' ') &	! first time only
         print *, 'Netcdf library version = ' // trim (nf90_inq_libvers())

      if (diag >= 3) print '(2a)', ' Open input file: ', trim (infile)

      status_nc = nf90_open (infile, nf90_nowrite, ncidi)

      if (check_nc (status_nc, 'nf90_open')) then
         previous_file = 'xxx xxx'		! prevent filename match
         ncidi = 0				! mark as file closed, even if
         					!   in strange semi-open state
         return					! soft error, return
      end if

      previous_file = infile				! remember open file

      if (diag >= 5) print *, '  ncidi = ', ncidi

   end if file_check

! Look up the var name in the input file.

   if (diag >= 5) print *, '  Look up var name in file: ' // trim (varname)

   status_nc = nf90_inq_varid (ncidi, varname, varidi)

   if (check_nc (status_nc, 'nf90_inq_varid')) return	! soft error, return

   if (diag >= 5) print *, '    Var ID for ' // trim (varname) // ' = ', varidi

! Get number of dimensions for current variable.

   if (diag >= 5) print *, '  Get number of dimensions for this variable.'

   status_nc = nf90_inquire_variable (ncidi, varidi, ndims=file_rank)

   if (check_nc (status_nc, 'nf90_inquire_variable')) return	! soft error

   if (diag >= 5) print *, '    Number of dimensions = ', file_rank

! Consistency check for number of dimensions.

   if (file_rank /= rank) then
      print *, '*** read_netcdf_var: Incorrect number of dimensions for "' &
         // trim (varname) // '" in file.'
      print '(a,i0)', ' *** Number of dimensions in file  = ', file_rank
      print '(a,i0)', ' *** Number of dimensions expected = ', rank
      return						! soft error, return
   end if

! Read dimension ID's for current variable.

   if (diag >= 5) print *, '  Get dimension ID''s.'

   status_nc = nf90_inquire_variable (ncidi, varidi, dimids=dimids(1:file_rank))

   if (check_nc (status_nc, 'nf90_inquire_variable')) return	! soft error

! Read dimension sizes for current variable.

   if (diag >= 5) print *, '  Read dimension sizes.'

   do i = 1, file_rank
      status_nc = nf90_inquire_dimension (ncidi, dimids(i), len=dims(i))

      if (check_nc (status_nc, 'nf90_inquire_dimension')) return   ! soft error
   end do

   if (diag >= 5) print '(a,999(2x,i0))', '     Var dimensions in file =', &
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

      if (diag >= 5) print *, '  Allocate data array.'
   end if

! Setup for main allocate is now complete.  Return continue status.
! The specific routine will next allocate the main data array.

   error_flag = .false.

! Note that the user status "status" remains set to the abort status,
! because the whole read operation is not yet finished.

end function read_setup
