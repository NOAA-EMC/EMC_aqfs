!------------------------------------------------------------------------------
!
! write_test_day_files.f90 -- Write intermediate filter results to text files.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2016-feb-10	Original version.  By Dave Allured.
!		Spin off from main_analog.f90, version 2016-0208i.
! 2017-apr-04	Support forecast cycle number in file name template.
!		Print output statistics.
!
!------------------------------------------------------------------------------

module write__test_day_files
contains

subroutine write_test_day_files (day_file_template, filter_result, vmiss, &
    cycle_time, diag, site_ids, site_lats, site_lons)

  use config, only : dp
  use string_utils
  implicit none

  character(*), intent(in) :: day_file_template		! file name templates
  real(dp),     intent(in) :: filter_result(:,:,:)	! (days, hours, sites)
  real(dp),     intent(in) :: vmiss			! missing value code
  integer,      intent(in) :: cycle_time		! fcst hour for filename
  integer,      intent(in) :: diag			! diag verbosity level

! Currently unused inputs, for possible future use.

  character(*), intent(in) :: site_ids(:)		! site ID's
  real(dp),     intent(in) :: site_lats(:)		! site coordinates
  real(dp),     intent(in) :: site_lons(:)

  integer, external :: get_free_unit		! function def.

! Local variables.

  character(200) outname
  character(10) cycle_str, day_str

  integer ndays, nhours, nsites, nfiles
  integer outfile, iday, outday1, outday2

! Start.

  print *
  print *, 'write_test_day_files: Write test output day files.'
  print *, '  Writing to ' // trim (day_file_template)

! Suppress compiler warnings for unused inputs.

  iday = vmiss + len (site_ids) + site_lats(1) + site_lons(1)

! Get dimensions.

  ndays  = size (filter_result, 1)	! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (filter_result, 2)	! no. of hours in each forecast cycle
  nsites = size (filter_result, 3)	! number of sites in result array

! Write result data to daily text files.

  nfiles = 0				! init file counter

  outfile = get_free_unit ()		! allocate output unit number, will
					! be released on close last file

!!  outday1 = apar%start_stat		! OBSOLETE 2016-feb-02

  outday1 = ndays			! output only the current forecast date
  outday2 = ndays

  do iday = outday1, outday2

! Create unique file name for current day.

    write (cycle_str, '(i2.2)') cycle_time	! forecast hour string, fixed
    						! length with leading zeros

    write (day_str,   '(i4.4)') iday		! decimal day number, fixed
    						! length with leading zeros

! Insert number strings into file name from template.

    outname = day_file_template
    call replace_substring (outname, 'HH',   trim (cycle_str))
    call replace_substring (outname, 'DDDD', trim (day_str  ))

! Write text file for current day.

    if (diag >= 3) print *, '  Output: ' // trim (outname)

    open  (outfile, file=outname, action='write')
    write (outfile, '(f20.15)') filter_result(iday:iday, :, :)
    close (outfile)

    nfiles = nfiles + 1
  end do

! Output statistics.

  if (diag >= 2) then
    print *, '  Number of day files written = ', nfiles
    print *, '  Number of sites included    = ', nsites
    print *, '  Number of forecast hours    = ', nhours
  end if

  if (diag >= 3) print *, 'write_test_day_files:  Done.'

end subroutine write_test_day_files
end module write__test_day_files
