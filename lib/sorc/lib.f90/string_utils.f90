!-----------------------------------------------------------------------------
!
! string_utils.f90 -- Container module for string utility routines.
!
! This container provides an explicit module interface for
! each included procedure
!
! 1.00	2014-apr-23	Original module container.  By Dave Allured.
!
! Visibility is controlled by each individual routine.
! The default is all top-level objects are public.
!
!-----------------------------------------------------------------------------

module string_utils

! Incorporated by module inheritance.

   use parse_delimited_mod
   use real__to_compact_string
   use resolve__env

   implicit none

! Incorporated by includes.

contains

   include 'anglical.f90'
   include 'anglical_greater.f90'
   include 'anglical_match.f90'
   include 'anglical_str.f90'
   include 'count_substrings.f90'
   include 'int_to_string.f90'
   include 'lowercase.f90'
   include 'real_to_string.f90'
   include 'remove_quotes.f90'
   include 'replace_substring.f90'
   include 'string_to_double_dp.f90'
   include 'string_to_int.f90'
   include 'string_to_intu.f90'
   include 'string_to_real.f90'
   include 'string_to_real_dp.f90'
   include 'strip_blanks.f90'
   include 'uppercase.f90'

end module string_utils
