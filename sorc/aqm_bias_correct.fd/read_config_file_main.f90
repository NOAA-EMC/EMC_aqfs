!-----------------------------------------------------------------------------
!
! read_config_file_main.f90 -- Read custom config file for bias_corr.f90.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jul-08	Original version.  By Dave Allured.
!		Adapted from the config reader for interpolate_update.
!
! 2016-jan-20	Add site_exception_file parameter.
! 2016-feb-08	Add filter method, number of analogs, common debug controls.
! 2016-feb-09	Check for blank strings in required parameters.
!
! 2017-apr-28	Code restructuring.  Import setup of filter parameter
!		  structures from main_analog.f90 version 2016-feb-15.
! 2017-may-25	Add parameters for predictor weights.
! 2017-jun-01	Add parameters for obs blackout dates.
! 2017-jun-06	Add common code to resolve the optional SUBSET keyword
!		  in the weight file name template.
! 2017-jun-15	Enforce filter method = AnEnMean for weight generation.
!
! Notes:
!
! The configuration file is a simple text file containing file
! paths and specification tables for data variables to be
! processed.
!
! The configuration file contains comments, and is self-
! documenting.  See a typical bias_correct configuration file
! for more details.
!
!-----------------------------------------------------------------------------

module read__config_file_main
contains

subroutine read_config_file_main (config_file, obs_file_template, &
      interp_file_template, in_gridded_template, output_file_template, &
      new_site_list_template, grid_coord_file, pred_weights_file, &
      site_exception_file, target_obs_var, target_model_var, analog_vars, &
      filter_method, output_limit_method, site_file_template, &
      filter_array_file_template, stop_after_filter, &
      obs_blackout_start, obs_blackout_end, apar, fpar, kpar, wpar)

   use analog__ensemble,  only : apar_type
   use config,            only : dp
   use find__analog,      only : fpar_type
   use get_param_module
   use kf__luca,          only : kpar_type
   use read__table_lines
   use stdlit,            only : normal
   use string_utils
   use weight__control,   only : wpar_type
   implicit none

   character(*), intent(in ) :: config_file	! name of config file to read

   character(*), intent(out)              :: obs_file_template
   character(*), intent(out)              :: interp_file_template
   character(*), intent(out)              :: in_gridded_template
   character(*), intent(out)              :: output_file_template
   character(*), intent(out)              :: new_site_list_template
   character(*), intent(out)              :: grid_coord_file
   character(*), intent(out)              :: pred_weights_file
   character(*), intent(out)              :: site_exception_file
   character(*), intent(out)              :: target_obs_var
   character(*), intent(out)              :: target_model_var

   character(*), intent(out), allocatable :: analog_vars(:)

   character(*), intent(out)              :: filter_method
   character(*), intent(out)              :: output_limit_method
   character(*), intent(out)              :: site_file_template
   character(*), intent(out)              :: filter_array_file_template
   integer,      intent(out)              :: obs_blackout_start(3)	! M-D-H
   integer,      intent(out)              :: obs_blackout_end(3)	! M-D-H
   logical,      intent(out)              :: stop_after_filter

   type (apar_type), intent(out)          :: apar	! subsystem parameter
   type (fpar_type), intent(out)          :: fpar	! structures
   type (kpar_type), intent(out)          :: kpar
   type (wpar_type), intent(inout)        :: wpar	! contains prior setting

   integer get_free_unit			! function def.

! Local variables.

   integer, parameter :: max_table_size = 20	! max number of lines in any
   						! single table in config file;
						! at least one more than needed

   character(200) lines(max_table_size)		! line buffer for maximal table

   character(200) errmsg
   character(200) weight_file_template
   character(100) header_expected
   character(20) limits(2)			! input strings for 2 limits
   character circular_str*10, suffix*1
   character subset_str*30
   character required_gen_method*60

   integer j, n, vi, ios, status
   integer cf					! unit number for config file
   integer lnum					! line number within config file
   integer nvars				! size of var table
   integer num_analogs				! selected no. of best analogs

   real(dp) num

! Fixed program parameters.

   real(dp), parameter :: celsius_to_kelvin = 273.15	! unit conversion

! Open config file for input.

   print *
   print '(a)', 'Read configuration file.'
   print '(a)', '  File = ' // trim (config_file)

   cf = get_free_unit ()			! get unit # for control file

   open (cf, file=config_file, status='old', action='read', iostat=ios, &
      iomsg=errmsg)

   lnum = 0					! init line counter

   if (ios /= 0) then
      print '(a,i0)', '*** Fatal: Error opening config file, iostat = ', ios
      print '(2a)',   '*** Iomsg = ', trim (errmsg)
      call exit (1)
   end if

!-----------------------------------------------------------
! Read initial specification lines, in the order listed.
!-----------------------------------------------------------

! Item labels are case sensitive.
! Helper routines skip over comment lines and blank lines.

read_file: &
   do			! one pass control block, for escape handling only

      call get_param_string ('obs file template', obs_file_template, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('interp file template', interp_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('gridded input file template',in_gridded_template,&
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('output file template', output_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('new site list template', new_site_list_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      print *

      call get_param_string ('grid coordinate file', grid_coord_file, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('predictor weights file', weight_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('site exception file', site_exception_file, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('target obs variable', target_obs_var, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('target model variable', target_model_var, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read analog var table.
!-----------------------------------------------------------

! First read table as raw lines of text.  Get line count.

      print *
      print '(a)', 'Read analog var table in config file:'

      header_expected = '--------'		! start of line
      call read_table_lines (cf, header_expected, lines, nvars, lnum)

! Now have count of analog vars.  Allocate var parameter arrays.

      allocate (analog_vars(nvars),       fpar%is_circular(nvars))
      allocate (kpar%lower_limits(nvars), kpar%upper_limits(nvars))

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

            if (suffix == 'C') j = j - 1	! omit suffix

            read (limits(n)(1:j), *, iostat=ios, iomsg=errmsg) num
            					! read number part only

            call check_read_error (ios, errmsg, lines(vi))

            if (suffix == 'C') num = num + celsius_to_kelvin
            					! convert as needed

            if (n == 1) kpar%lower_limits(vi) = num	! insert limit value
            if (n == 2) kpar%upper_limits(vi) = num	! into proper array
         end do

      end do var_loop

      print *

! Consistency check for specified model var.

      if (.not. any (analog_vars(:) == target_model_var)) then
         print *
         print *, '*** Specified target model variable is not in the given' &
            // ' var table.'
         print *, '*** Target model variable = ' // trim (target_model_var)
         exit read_file
      end if

!-----------------------------------------------------------
! Read input filter controls.
!-----------------------------------------------------------

      call get_param_mdhz ('obs blackout start date', obs_blackout_start, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_mdhz ('obs blackout end date', obs_blackout_end, cf, &
         status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read analog filter controls.
!-----------------------------------------------------------

      print *

      call get_param_string ('filter method', filter_method, cf, status, lnum, &
         nonblank)
      if (status /= normal) exit read_file

! Enforce required AnEnMean method if weight generation is selected.

      required_gen_method = 'AnEnMean'

      if ((wpar%gen_weights) .and. (filter_method /= required_gen_method)) then
         print *, '*** read_config_file_main: Switching to filter method = ' &
            // trim (required_gen_method) // '.'
         print *, '*** This method is required when weight generation is' &
            // ' selected.'
         print *
         filter_method = required_gen_method
      end if

      call get_param_int ('number of analogs', num_analogs, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_string ('output limit method', output_limit_method, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read weight generation controls.
!-----------------------------------------------------------

      print *
      print '(a)', '* For weight generation only:'

      call get_param_int ('number of weight increments', wpar%nweights, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('forecast start day for weight generation', &
         wpar%forecast_start_day, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('last forecast hour for weight generation', &
         wpar%forecast_last_hour, cf, status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read debug controls.
!-----------------------------------------------------------

      print *

      call get_param_string ('site file template', site_file_template, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('filter array file template', &
         filter_array_file_template, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_yesno ('stop after filter', stop_after_filter, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      print *
      exit			! normal exit for single pass block structure
   end do read_file

!-----------------------------------------------------------
! End of read_file block.  Exit on read errors.
!-----------------------------------------------------------

   if (status /= normal) then
      print '(a,i0)', '*** read_config_file_main: Abort, config file line' &
         // ' number = ', lnum
      call exit (1)
   end if

!-------------------------------------------------
! Configuration guards.
!-------------------------------------------------

!-------------------------------------------------
! Resolve the weight file name template.
!-------------------------------------------------

! This is common code needed for both the read and write subroutines
! for predictor weight files.

! The embedded SUBSET keyword is optional.  The weight file name will
! remain unchanged in any case, if the SUBSET keyword is not present.

! If weight subsetting is used, then replace the SUBSET keyword string
! with weight subset numbers.  The subset numbers are optionally
! specified on the program command line.  Negative means subsetting
! is not selected.

   pred_weights_file = weight_file_template

   if (wpar%subset1 >= 0) then
      write (subset_str, "(i0,'-',i0)") wpar%subset1, wpar%subset2
      				! variable length integers, no leading zeros
      call replace_substring (pred_weights_file, 'SUBSET', trim (subset_str))

! If subsetting is not selected, then OMIT the SUBSET keyword with
! one adjacent period character.  This applies to both normal bias
! correction mode, as well as full weight generation with no subsetting.

   else
      call replace_substring (pred_weights_file, '.SUBSET.', '.')
   end if

!-------------------------------------------------
! Additional fixed parameters.
!-------------------------------------------------

! Imported from main_analog version 2016-feb-15.
! These parameters could be migrated into the config file, as needed.

! Note, some items within these structures are now dynamically managed
! within filter methods, not here.

   apar%num_an = num_analogs	! Number of best analogs to use for AN
   apar%weight_type = 1		! Type of analog weighting:
				! 0: Do not weight analogs
				! 1: Weight them by the inverse metric
				! 2: Weight them linearly
   apar%skipMissingAnalogs = 1	! Applies to both ANKF and AN
				! 1: Always use num_an analogs, even if
				! some of the best ones are missing
   fpar%useRealTrends = 0	! 1: Use trend, 0: use neighbours
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2

! Correction for speed or ozone or PM2.5.

   kpar%enforce_positive = 1	! enforce correction for values > 0

   fpar%lowerMetric = 0.00001	! Lower bound for allowed metric value

! Kalman filter parameters.

   kpar%varo = 0.005		! variance of observation variance
   kpar%varp = 1		! variance prediction variance

   kpar%ratio = 0.1		! KF method parameter (sigma_ratio)

   kpar%start = (/ 0, 0 /)	! starting point to compute statistics
   kpar%timeZone = 0		! timeZone of measurements (for output graphics)

! Analog parameters.

   fpar%window = 0		! check analog at the exact hour per day

   fpar%trend = (/ 1,1,1 /)	! check hour-1, hour & hour+1 for the var trend
  				! with equal weighting coefficients
				! MUST have odd number of elements.
				! Numbers are the weighted coefficients.
				! 2014-feb-19, MUST have exactly 3 elements.

! Normal exit.  Output parameters and structures are now initialized
! as much as possible at this point.

end subroutine read_config_file_main

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

   print '(3a)',  '*** read_config_file_main: Fatal: Read error in', &
      ' analog var table.'
   print '(3a)',  '*** Line = "', trim (line), '"'
   print '(a,i0)','*** Iostat = ', ios
   print '(3a)',  '*** Iomsg =', trim (errmsg)
   call exit (1)

end subroutine check_read_error

end module read__config_file_main
