!------------------------------------------------------------------------------
!
! bias_threshold_filter.f90 -- Site-specific bias threshold filter.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias correction
! program for CMAQ forecast outputs.
!
! 2014-oct-29	Original version.  By Dave Allured.
!
! This routine applies threshold tests to the biases computed for
! a set of individual analogs.  Separate positive and negative
! thresholds are supported.  The output is a logical mask that is
! used by the calling program to disable analogs when their bias
! is out of range.
!
! Test biases for individual analogs are computed on the fly from
! intermediate obs and prediction arrays.
!
! Thresholds are specified individually in a short list of
! different observation sites.  The thresholds are read from
! a site exception file in free format text.  See the subroutine
! read_exception_list.f90 for file details.
!
! Note:  Sites are matched by SITE ID NUMBERS, not lat/lon
! coordinates.
!
! Note:  This filter processes missing values like real data.
! It is assumed that the calling program will apply a proper
! missing value mask after this filter routine is used.
!
!------------------------------------------------------------------------------

module bias__threshold_filter
contains

subroutine bias_threshold_filter (obs, pred, site_id, diag, analog_mask)

   use config, only : dp
   use read__exception_list
   implicit none

   real(dp),     intent(in)          :: obs(:)	     ! obs for a set of analogs
   real(dp),     intent(in)          :: pred(:)	     ! matching predictions
   character(*), intent(in)          :: site_id	     ! current site ID
   integer,      intent(in)          :: diag	     ! verbosity, 0=errs only

   logical, intent(out), allocatable :: analog_mask(:) ! output mask for analogs
   						       ! true = analog enabled
! Run parameters.

   character(*), parameter :: &
      site_exception_file = 'aqm_site_bias_thresholds.txt'

   integer, parameter :: id_len = 9	! station ID string length
   					! (can't use automatic length with save)

! State variables, saved between calls.

   character(id_len), save :: current_site_id = 'xxxxx'

   integer, save :: isite			! list index for current site
   logical, save :: file_is_read = .false.

! Cache for site exception file.  Saved between calls.

   character(id_len), allocatable, save :: site_ids(:)
   integer,                        save :: nsites
   real(dp),          allocatable, save :: thresh_low(:), thresh_high(:)

! Local variables.

   integer nanalogs, si
   real(dp), allocatable :: bias(:)

! Allocate output array.

   nanalogs = size (obs)
   allocate (analog_mask(nanalogs))

! First check whether this site ID is the current active site.
! This prevents unnecessary repeat searches.

site_lookup: &
   if (site_id /= current_site_id) then

! Not the active site.  Read site list file into the cache, first time only.
! Returns nsites = 0 if the site file is absent.

      if (.not. file_is_read) then
         call read_exception_list (site_exception_file, diag, site_ids, &
            thresh_low, thresh_high, nsites)
         file_is_read = .true.
      end if

! Find the current site ID in the list.
! Also handles nsites = 0 properly.

      isite = 0					! default to site not in list

      do si = 1, nsites
         if (site_id == site_ids(si)) then
            isite = si				! found, save site index
            exit
         end if
      end do

      current_site_id = site_id			! remember active site ID

   end if site_lookup

! Return with all analogs enabled, if site is not in the list,
! or site file is absent.

   if (isite == 0) then				! note, all state vars are set
      analog_mask(:) = .true.			! properly on return:  isite,
      return					! nsites, current_site_id,
   end if					! file_is_read, and cache.

! Found active site.  Compute individual bias values.

   allocate (bias(nanalogs))

   bias(:) = obs(:) - pred(:)

! Apply thresholds and create mask.
! TRUE = analog enabled, FALSE = disabled (bias outside the given thresholds).

   analog_mask(:) =    (bias(:) > thresh_low(isite))  &
                 .and. (bias(:) < thresh_high(isite))

! All done.  All state vars are still set properly.

end subroutine bias_threshold_filter

end module bias__threshold_filter
