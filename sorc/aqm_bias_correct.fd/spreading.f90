!-----------------------------------------------------------------------------
!
! spreading.f90 -- Expand bias corrections to grids, and write output file.
!
! This is the top level routine of the spreading module,
! one of the four main components of the NOAA NCO/ARL/PSD
! bias correction system for CMAQ forecast outputs.
!
! 2014-jul-10	Original top-level spreading control module.
!		By Dave Allured, NOAA/PSD/CIRES.
!
! 2016-jan-20	Rename main routine for name conflict with fortran intrinsic!
! 2016-feb-09	Add output_limit_method parameter.
!
! 2019-may-30	Interface change for read_gridded_vars.
!		Add consistency checks for input file mismatch.
!
! 2020-aug-02	Return bias corrected forecast grids to calling program,
!		  for other uses such as probability forecast.
!		Add option to suppress writing output file, calculate only.
!
! 2021-mar-24	Add support for mixed forecast lengths, rather than failing.
!		Match output lengths to the uncorrected forecast input file.
!
! * Remember to update the date in the module_id below.
!
! This spreading module incorporates three major functions:
!
! * Read raw gridded forecasts for a given forecast cycle.
! * Expand site bias corrections to entire forecast grids.
! * Write bias corrected output file.
!
! Primary inputs:
!
! * Gridded raw forecast file for current forecast cycle.
! * Uncorrected forecasts interpolated to site locations.
! * Bias corrected forecasts at site locations.
! * Auxiliary grid coordinate file, as needed.
! * Site coordinates.
!
! Primary outputs:
!
! * Single bias corrected gridded file containing:
!   1. Bias corrected grids for all forecast hours.
!   2. Bias grids for all forecast hours.
! * Bias corrected grids returned to calling program.
!
! Notes:
!
! Input and output file paths are specified with path templates.
! These contain YYYY MM DD HH substitution strings that will
! resolve to desired file names for the given forecast cycle.
! Templates may include full or relative paths, and they may
! begin with an $ENV environnment variable.
!
! A separate grid coordinate file is needed only when the gridded
! raw forecast files do not include their own grid coordinates.
!
! Forecast date index:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
!-----------------------------------------------------------------------------

module spreading_mod
contains

subroutine spreading (in_gridded_template, grid_coord_file, &
      output_file_template, target_var, output_limit_method, forecast_date, &
      cycle_time, base_year, calendar, uncorr_sites, corr_sites, site_lats, &
      site_lons, vmiss, diag, corr_grids)

   use config, only : dp
   use expand__filename
   use index_to_date_mod
   use read__gridded_vars
   use spread__bias
   use stdlit, only : normal
   use write__corrected_netcdf
   implicit none

   character(*), parameter :: module_id = 'spreading.f90 version 2021-mar-24'

! Input arguments.

   character(*), intent(in) :: in_gridded_template  ! gridded input template
   character(*), intent(in) :: grid_coord_file	    ! aux. grid coordinate file
   character(*), intent(in) :: output_file_template ! output filename template
   character(*), intent(in) :: target_var	    ! target variable name
   character(*), intent(in) :: output_limit_method  ! output limit method name

   integer,      intent(in) :: forecast_date	    ! target date index
   integer,      intent(in) :: cycle_time	    ! forecast cycle time
   integer,      intent(in) :: base_year	    ! base year for date indexes
   character(*), intent(in) :: calendar	  	    ! calendar system in use
   real(dp),     intent(in) :: uncorr_sites(:,:)    ! uncorrected forecasts
						    !   at sites (hours, sites)
   real(dp),     intent(in) :: corr_sites(:,:)	    ! bias corrected forecasts
						    !   at sites (hours, sites)
   real(dp),     intent(in) :: site_lats(:)	    ! site coordinates
   real(dp),     intent(in) :: site_lons(:)
   real(dp),     intent(in) :: vmiss		    ! common missing value code
   integer,      intent(in) :: diag		    ! diag verbosity level, 0-N

! Output argument.

   real(dp), allocatable, intent(out) :: corr_grids (:,:,:)  ! (X, Y, hours)
						    ! bias corrected grids

! Local program parameter.

! Local assumption that all raw forecasts to be processed will be
! in aqm Netcdf format.
! This is a temporary fix to read the raw gridded forecast file.
! See the embedded reader selection in read_gridded_forecasts.f90.

   character(*), parameter :: reader_code = 'reader.aqm'

! Local variables.

   character fdate_str*24, fmt1*60, fmt2*60, outfile*200

   integer year, month, day
   integer nhours_sites, nsites, nhours_out, nhours_common
   integer nhours_expect, nhours_actual_max
   integer ivar, status

! Dynamic arrays.

   integer,  allocatable :: nhours_actual(:)	    ! actual hours for input var

   real(dp), allocatable :: grid_lats(:,:)	    ! grid coordinates (X, Y)
   real(dp), allocatable :: grid_lons(:,:)

   real,     allocatable :: uncorr_grids(:,:,:,:)   ! input raw forecast grids
						    !   (X, Y, [var], hours)
						    ! note single precision here
   real(dp), allocatable :: bias_grids (:,:,:)	    ! output bias grids
						    !   (X, Y, hours)

   real(dp), allocatable :: uncorr_sites2(:,:)      ! site input arrays
   real(dp), allocatable :: corr_sites2(:,:)	    ! after size matching

!-------------------------------------------------------
! Read raw gridded forecast data for target variable.
!-------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, 'spreading: Begin spreading module.'
   if (diag >= 2) print *, '  Module ID = ' // module_id

! Decode the date index for the current forecast cycle.

   call index_to_date (forecast_date, year, month, day, base_year, calendar)
      						! get current Y M D integers

! Get dimensions for input site arrays.

   nhours_sites = size (uncorr_sites, 1)
   nsites       = size (uncorr_sites, 2)

! Expect same number of hours when reading the uncorrected forecast grids.
! Actual forecast hours for target var in file are usually the same,
! but may differ with mixed length training data.

   nhours_expect = nhours_sites

! Utilize the same gridded reader used by the interpolation program.
! To include all possible file configurations, this routine also
! reads the companion grid coordinates.

! This routine reads forecast grids in the original single precision.
! Later they will be converted on the fly to doubles.

   if (diag >= 4) then
      print '(3a)', '   in_gridded_template = [', trim (in_gridded_template),']'
   end if

   call read_gridded_vars ( (/ target_var /), (/ reader_code /), &
      (/ in_gridded_template /), grid_coord_file, year, month, day, &
      cycle_time, nhours_expect, real (vmiss), diag, uncorr_grids, &
      nhours_actual, nhours_actual_max, grid_lats, grid_lons, status)
      			! read single target var into 4-D (X, Y, [var], hours)

! Abort if raw forecast grids for target variable are not available.
! Reader may have also printed some diagnostic details.

   if (status /= normal) then
      fmt1 = '(a,i0.4,3(1x,i0.2),"Z")'
      print *
      print fmt1, '*** spreading: Raw ' // trim (target_var) &
              // ' forecast grids for target forecast cycle are not available.'
      print fmt1, '*** Target forecast cycle = ', year, month, day, cycle_time
      print fmt1, '*** Fatal, can not complete bias correction for this' &
                         // ' forecast cycle.'
      call exit (1)
   end if

! Warn if forecast lengths are mismatched, but continue processing.

   if (  nhours_actual(1)  /= nhours_expect &
    .or. nhours_actual_max /= nhours_expect) then
      fmt1 = '(a,i0.4,3(1x,i0.2),"Z")'
      fmt2 = '(a,i0)'
      print *
      print fmt1, '*** spreading: Warning, inconsistent forecast lengths.'
      print fmt2, '*** nhours_expect     = ', nhours_expect
      print fmt2, '*** nhours_actual(1)  = ', nhours_actual(1)
      print fmt2, '*** nhours_actual_max = ', nhours_actual_max
      print fmt1, '*** Input data sets are mixed lengths, or data set mismatch.'
      print fmt1, '*** Target forecast cycle = ', year, month, day, cycle_time
   end if

!-----------------------------------------------------------
! Expand or contract site arrays to match forecast file.
!-----------------------------------------------------------

! Support mixed forecast lengths between training data and target forecast.
! Pad with missing values as needed, to neutralize bias correction.
! This method is valid to both expand and contract.

   nhours_out    = nhours_actual(1)
   nhours_common = min (nhours_sites, nhours_out)

   if (diag >= 2) then
      fmt1 = '(3x,a,i0)'
      print *
      print fmt1, 'Expand or contract site arrays to match forecast file.'
      print fmt1, '  Site hours input          = ', nhours_sites
      print fmt1, '  Site hours after matching = ', nhours_out
      print *
   end if

   allocate (uncorr_sites2(nhours_out, nsites))
   allocate (corr_sites2  (nhours_out, nsites))

   uncorr_sites2(:,:) = vmiss
   corr_sites2  (:,:) = vmiss

   uncorr_sites2(1:nhours_common,:) = uncorr_sites(1:nhours_common,:)
   corr_sites2  (1:nhours_common,:) = corr_sites  (1:nhours_common,:)

!-------------------------------------------------------
! Spread bias corrections to forecast grids.
!-------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  spreading: Spread bias corrections to grids.'

! Convert gridded input to double precision on the fly, for calculations.

   ivar = 1			! index for target variable input grids;
   				! is 1 because only single var was just read

   call spread_bias (dble (uncorr_grids(:,:,ivar,1:nhours_out)), &
      uncorr_sites2, corr_sites2, grid_lats, grid_lons, site_lats, site_lons, &
      vmiss, diag, output_limit_method, bias_grids, corr_grids)

!-------------------------------------------------------
! Write bias corrected output file.
!-------------------------------------------------------

   if (output_file_template == 'none') then
      print *, &
         '*** spreading: Hourly output file is suppressed.  No file written.'

   else
      call fdate (fdate_str)
      print '(2a)', fdate_str, '  spreading: Write output file.'

! Create actual output file name for current forecast cycle.

      call expand_filename (output_file_template, year, month, day, &
         cycle_time, outfile)

! Write output file.  Data and coordinate vars will be converted from
! double to single precision, when writing to file.

      call write_corrected_netcdf (outfile, target_var, grid_lats, grid_lons, &
         corr_grids, bias_grids, vmiss, diag)
   end if

   if (diag >= 3) print *, 'spreading: Return.'

end subroutine spreading
end module spreading_mod
