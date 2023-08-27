!-----------------------------------------------------------------------------
!
! read_var_table.f90 -- Read the analog var table in a bias corr. config file.
!
! This is a support routine for config file readers in the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2020-nov-10	read_var_table.f90:
!		Split off from read_config_file_main.f90.
! 2022-apr-20	Minor.  Interface change for subroutine read_table_lines.
!
! See general documentation within read_config_file_main.f90.
! See a typical bias_correct configuration file for more details.
!
!-----------------------------------------------------------------------------

module read__var_table
contains

subroutine read_var_table (analog_vars, var_lower_limits, var_upper_limits, &
         fvar_num, fpar, kpar, target_model_var, cf, status, lnum)

   use config,           only : dp
   use find__analog,     only : fpar_type
   use kf__luca,         only : kpar_type
   use read__table_lines
   use stdlit,           only : normal, fatal
   implicit none

   character(*), intent(out), allocatable :: analog_vars(:)	! var names
   real(dp), intent(out), allocatable :: var_lower_limits(:)	! var limits
   real(dp), intent(out), allocatable :: var_upper_limits(:)
   integer,          intent (out)     :: fvar_num	! forecast var index no.
   type (fpar_type), intent (inout)   :: fpar		! parameter structures
   type (kpar_type), intent (inout)   :: kpar
   character(*),     intent (in)      :: target_model_var   ! forecast var name
   integer,          intent (in)      :: cf		! config file unit no.
   integer,          intent (out)     :: status		! return status
   integer,          intent (inout)   :: lnum		! track file line number

! Local variables.

   integer, parameter :: max_table_size = 20	! max number of lines in any
   						! single table in config file;
						! at least one more than needed

   character(200) lines(max_table_size)		! line buffer for maximal table

   character(200) errmsg
   character(100) header_expected
   character(20) limits(2)			! input strings for 2 limits
   character circular_str*10, suffix*1

   integer j, n, vi, ios
   integer nvars				! size of var table

   real(dp) num

! Fixed program parameters.

   character(*), parameter :: caller_name = 'read_var_table'

   real(dp), parameter :: celsius_to_kelvin = 273.15	! unit conversion

!-----------------------------------------------------------
! Read analog var table.
!-----------------------------------------------------------

! First read table as raw lines of text.  Get line count.

   print *
   print '(a)', '* Read analog var table in config file:'
   print *

   header_expected = '--------'			! start of line
   call read_table_lines (cf, header_expected, 'no empty', caller_name, &
      lines, nvars, lnum)

! Now have count of analog vars.  Allocate var parameter arrays.

   allocate (analog_vars(nvars),      fpar%is_circular(nvars))
   allocate (var_lower_limits(nvars), var_upper_limits(nvars))

   fvar_num = 0					! setup to locate target var

! Parse table lines.  Fortran free format, space delimited.
! Only read the first four columns.  Any remainders are comments.
! Must read columns 2 and 3 as strings, because of possible C suffix.

var_loop: &
   do vi = 1, nvars
      print '(a)', trim (lines(vi))		! progress display & diagnostic

      read (lines(vi), *, iostat=ios, iomsg=errmsg) analog_vars(vi), &
         limits(1:2), circular_str		! re-read into substrings

      call check_read_error (ios, errmsg, lines(vi))

      fpar%is_circular(vi) = (circular_str == 'Y' .or. circular_str == 'y')

! Handle "C" unit suffixes on limit strings.  Convert Celsius to Kelvin.
! Assume all limit strings are non-blank and valid numbers, after
! suffixes are removed.

      do n = 1, 2				! for each limit string...
         j = len_trim (limits(n))		! get final character
         suffix = limits(n)(j:j)

         if (suffix == 'C') j = j - 1		! omit suffix

         read (limits(n)(1:j), *, iostat=ios, iomsg=errmsg) num
         					! read number part only

         call check_read_error (ios, errmsg, lines(vi))

         if (suffix == 'C') num = num + celsius_to_kelvin  ! convert as needed

         if (n == 1) var_lower_limits(vi) = num      ! save var limit values
         if (n == 2) var_upper_limits(vi) = num
      end do

! Save more for the target forecast variable.

      if (analog_vars(vi) == target_model_var) then
         fvar_num = vi				! target position in var table
         kpar%lower_limit = var_lower_limits(vi)
         kpar%upper_limit = var_upper_limits(vi)
      end if

   end do var_loop

   print *

! Consistency check for the target model var.

   if (fvar_num == 0) then
      print *
      print *, '*** Target model variable is not found in the given var table.'
      print *, '*** Target model variable = ' // trim (target_model_var)
      status = fatal
      return
   end if

   status = normal		! normal exit, table read successfully

end subroutine read_var_table

!-----------------------------------------------------------
! Error handler for table reader.  Local use only.
!-----------------------------------------------------------

! If no error, this routine returns quietly.
! If error, this routine prints diagnostic and halts.

subroutine check_read_error (ios, errmsg, line)
   implicit none

   integer,      intent (in) :: ios
   character(*), intent (in) :: errmsg
   character(*), intent (in) :: line

   if (ios == 0) return

   print '(3a)',  '*** read_var_table: Fatal: Read error in', &
      ' analog var table.'
   print '(3a)',  '*** Line = "', trim (line), '"'
   print '(a,i0)','*** Iostat = ', ios
   print '(3a)',  '*** Iomsg =', trim (errmsg)
   call exit (1)

end subroutine check_read_error

end module read__var_table
