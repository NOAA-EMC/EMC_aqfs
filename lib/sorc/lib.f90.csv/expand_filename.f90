!------------------------------------------------------------------------------
!
! expand_filename.cc.f90 -- Make a file name from parameters and a template.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-apr-24	expand_filename.f90:
!		Original version.  By Dave Allured.
!
! 2022-apr-10	expand_filename.cc.f90:  Bias correction version.
!		Change the substitution code for "cycle time" from HH to CC.
!
! Notes:
!
! This routine is somewhat generalized for the bias correction
! project.  It may be used for different purposes by several
! other routines.
!
! The output variable "outname" must be long enough to contain
! both the unexpanded and expanded template strings.  No checking
! for truncation is done.
!
! In this version, string substitution is NOT recursive.  This
! is a one-pass processor.
!
!------------------------------------------------------------------------------

module expand__filename
contains

subroutine expand_filename (template, year, month, day, cycle_time, outname)

   use string_utils
   implicit none

   character(*), intent(in ) :: template
   integer,      intent(in ) :: year, month, day
   integer,      intent(in ) :: cycle_time

   character(*), intent(out) :: outname

   character ystr*4, mstr*2, dstr*2, cstr*2	! fixed-width strings

! Expand environment variables.

! Note, the current version of resolve_env only handles a single
! environment variable at the beginning of the template.  Check
! with the latest version for support status.

   outname = template			! start with writable copy of template
   call resolve_env (outname)

! Make fixed-width number strings with leading zeros.

   write (ystr, '(i4.4)') year
   write (mstr, '(i2.2)') month
   write (dstr, '(i2.2)') day
   write (cstr, '(i2.2)') cycle_time

! Replace code strings in template.
! Use repeat count 99 for multiple replacements.

   call replace_substring (outname, 'YYYY', ystr, rep_count=99)
   call replace_substring (outname, 'MM',   mstr, rep_count=99)
   call replace_substring (outname, 'DD',   dstr, rep_count=99)
   call replace_substring (outname, 'CC',   cstr, rep_count=99)

end subroutine expand_filename
end module expand__filename
