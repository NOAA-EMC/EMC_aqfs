!-----------------------------------------------------------------------------
!
! get_window_offsets.f90 -- Read window offsets parameter in a BC config file.
!
! This is a support routine for config file readers in the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2020-nov-10	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!
! The normal syntax for the window offset parameter is free format,
! as shown here.  Leading and trailing spaces are allowed.  The minus
! and plus signs are required as shown in these exact positions.
! NN must be integers.
!
!     analog search window offsets = -NN, +NN
!
! See a typical bias_correct configuration file for more details.
!
!-----------------------------------------------------------------------------

module get__window_offsets
contains

subroutine get_window_offsets (label, window, cf, status, lnum)

   use get_param_module
   use stdlit,          only : normal, fatal
   use string_utils
   implicit none

   character(*), intent (in)	:: label	! config parameter label
   integer,      intent (out)	:: window(2)	! low and high window offsets
   integer,      intent (in)	:: cf		! config file unit number
   integer,      intent (out)	:: status	! return status
   integer,      intent (inout)	:: lnum		! track file line number

! Local variables.

   character(100) string, token, errmes

   integer i, p1, p2, comma, len1, num
   integer first(2), last(2)

   character(*), parameter :: valid_signs = '-+'  ! signs in required positions

! First read parameter line as a single string.

   call get_param_string (label, string, cf, status, lnum, nonblank)
   if (status /= normal) return

! Parse out the two offsets separated by a single comma.

   comma = index (string, ",")			! zero if comma is missing
   first(1) = 1
   last(1)  = comma - 1
   first(2) = comma + 1
   last(2)  = len_trim (string)

! Parse and validate each of the two offset tokens.
! Each token must be at least two characters with leading sign.
! Missing comma and degenerate fields are handled properly.

   errmes = ' '					! error detect

   do i = 1, 2
      p1 = first(i)				! get initial substring
      p2 = last(i)
      len1 = max (0, p2 - p1 + 1)		! length before trimming

      if (len1 > 0) then			! if not null...
         token = adjustl (string(p1:p2))	! remove leading and trailing
         len1  = len_trim (token)		! spaces
      end if

      if (len1 < 1) then			! null field error
         errmes = 'Missing field, two offsets are required.'
         exit
      end if

!!      print '(5i6,4x,a)', comma, i, p1, p2, len1, '[' // trim (token) // ']'

      if (token(1:1) /= valid_signs(i:i)) then	! sign is wrong or missing
         errmes = 'Invalid sign, offsets must be -NN and +NN.'
         exit
      end if

      if (len1 < 2) then			! missing integer error
         errmes = 'Invalid offset, must be integer.'
         exit
      end if

      call string_to_intu (trim (token(2:)), num)   ! convert string to integer

      if (num < 0) then				! not a simple integer
         errmes = 'Invalid offset, must be integer.'
         exit
      end if

      if (token(1:1) == '-') num = -num		! apply the minus sign

      window(i) = num				! save the final value
   end do

! Error check.

   if (errmes /= ' ') then
      print *
      print '(a)', '*** get_window_offsets:'
      print '(a)', '*** ' // trim (errmes)
      status = fatal			! error exit
   else
      status = normal			! normal exit, return both offsets
   end if

end subroutine get_window_offsets
end module get__window_offsets
