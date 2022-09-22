!------------------------------------------------------------------------------
!
! write_site_result_files.f90 -- Write intermediate site results to text files.
!
! This writer outputs an alternate text format with bias corrected
! site results, for only the target forecast day.
!
! This is a support routine for the NOAA NCO/ARL/PSL bias
! correction system for CMAQ forecast outputs.
!
! 2020-jun-11	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!		Adapted from write_test_site_files.f90 version 2017-apr-04.
!
!------------------------------------------------------------------------------

module write__site_result_files
contains

subroutine write_site_result_files (site_result_file_template, uncorr_sites, &
    corr_sites, diag, forecast_day_num, forecast_date_index, base_year, &
    calendar, cycle_time, analog_vars, filter_name, site_ids, site_lats, &
    site_lons)

  use config, only : dp
  use expand__filename
  use index_to_date_mod
  use string_utils
  implicit none

  character(*), intent(in) :: site_result_file_template	! file name template
  real(dp),     intent(in) :: uncorr_sites(:,:)		! uncorr. fcst data (HS)
  real(dp),     intent(in) :: corr_sites(:,:)		! corr. fcst data (HS)
  integer,      intent(in) :: diag			! diag verbosity level

! Labeling inputs for file names and header lines.

  integer,      intent(in) :: forecast_day_num		! day no. in train. per.
  integer,      intent(in) :: forecast_date_index	! forecast date index
  integer,      intent(in) :: base_year			! base year for day nos.
  character(*), intent(in) :: calendar			! calendar system
  integer,      intent(in) :: cycle_time		! forecast cycle hour

  character(*), intent(in) :: analog_vars(:)		! predictor var names
  character(*), intent(in) :: filter_name		! selected analog filter

  character(*), intent(in) :: site_ids(:)		! site ID's
  real(dp),     intent(in) :: site_lats(:)		! site coordinates
  real(dp),     intent(in) :: site_lons(:)

  integer, external :: get_free_unit		! function def.

! Local variables.

  character(200) outname1, outname2
  character date_str*30, column_headers*200, header15*15

  integer isite, nsites, npred, i
  integer ihour, nhours
  integer year, month, day, outfile

! Start.

  print *
  print *, 'write_site_result_files: Write site result test files.'
  print *, '  Writing to ' // trim (site_result_file_template)

! Get dimensions.

  nhours = size (corr_sites, 1)		! number of hours in forecast cycle
  nsites = size (corr_sites, 2)		! number of sites in result array

  npred  = size (analog_vars)		! number of predictor variables

! Partially resolve the output file name template.
! Resolve the prefix string, forecast date, and cycle time.

  call index_to_date (forecast_date_index, year, month, day, base_year, &
    calendar)				! get current Y M D integers

  call expand_filename (site_result_file_template, year, month, day, &
    cycle_time, outname1)

  call expand_filename ('YYYY-MM-DD, HHZ', year, month, day, cycle_time, &
    date_str)				! this copy for a file header line

!-----------------------------------------------------------
! Main loop over sites.  Write one output file per site.
!-----------------------------------------------------------

  outfile = get_free_unit ()		! allocate output unit number,
					! will be released on last file close
site_loop: &
  do isite = 1, nsites

! Insert current site ID string, finish resolving the new file name.

    outname2 = outname1
    call replace_substring (outname2, 'SITE_ID', trim (site_ids(isite)))

! Start a new text file for current site.  Write header.

    if (diag >= 3) print *, '  Output: ' // trim (outname2)

    open (outfile, file=outname2, action='write')	! no overwrite protect

    write (outfile, '(2a, 2f12.5)') 'Site: ', trim (site_ids(isite)), &
      site_lats(isite), site_lons(isite)

    write (outfile, '(3a)') 'Forecast date: ', trim (date_str)

    write (outfile, '(3a)') 'Uncorrected and ', trim (filter_name), &
      ' corrected forecast data'

    write (outfile, '(i0, 99(1x,a))') npred, 'predictors:', &
      (trim (analog_vars(i)), i = 1, npred)

    header15 = trim (filter_name)
    column_headers = '    DAY   HOUR    Uncorrected' // adjustr (header15)
    write (outfile, '(a)') trim (column_headers)

! Write data lines, one per forecast hour.

    write (outfile, '(2i7, 2f15.5)') (forecast_day_num, ihour, &
      uncorr_sites(ihour, isite), corr_sites(ihour, isite), ihour = 1, nhours)

    close (outfile)
  end do site_loop

! Output statistics.

  if (diag >= 2) then
    print *, '  Number of site files written       = ', nsites
    print *, '  Number of forecast cycles included = ', 1
    print *, '  Number of forecast hours included  = ', nhours
  end if

  if (diag >= 3) print *, 'write_site_result_files:  Done.'
  if (diag >= 2) print *

end subroutine write_site_result_files
end module write__site_result_files
