!-----------------------------------------------------------------------------
!
! get_param_module.f90 -- Container module for get_param config file routines.
!
! This container provides an explicit module interface for
! each included procedure.
!
! 3.00	2016-feb-09	Original module container.  By Dave Allured.
!
! Visibility is controlled by each individual routine.
! The default is all top-level objects are public.
!
!-----------------------------------------------------------------------------

module get_param_module
   implicit none

   logical, parameter :: nonblank = .true.	! optional argument value
   						! for get_param_string
contains

   include 'get_param_int.f90'
   include 'get_param_real.f90'
   include 'get_param_string.f90'
   include 'get_param_yesno.f90'

end module get_param_module
