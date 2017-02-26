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
      new_site_list_template, grid_coord_file, site_exception_file, &
      target_obs_var, target_model_var, analog_vars, lower_limits, &
      upper_limits, is_circular, filter_method, num_analogs, &
      output_limit_method, site_file_template, filter_array_file_template, &
      stop_after_filter)

   use config, only : dp
   use get_param_module
   use read__table_lines
   use stdlit, only : normal
   implicit none

   character(*), intent(in ) :: config_file	! name of config file to read

   character(*), intent(out)              :: obs_file_template
   character(*), intent(out)              :: interp_file_template
   character(*), intent(out)              :: in_gridded_template
   character(*), intent(out)              :: output_file_template
   character(*), intent(out)              :: new_site_list_template
   character(*), intent(out)              :: grid_coord_file
   character(*), intent(out)              :: site_exception_file
   character(*), intent(out)              :: target_obs_var
   character(*), intent(out)              :: target_model_var

   character(*), intent(out), allocatable :: analog_vars(:)
   real(dp),     intent(out), allocatable :: lower_limits(:)
   real(dp),     intent(out), allocatable :: upper_limits(:)
   logical,      intent(out), allocatable :: is_circular(:)

   character(*), intent(out)              :: filter_method
   integer,      intent(out)              :: num_analogs
   character(*), intent(out)              :: output_limit_method
   character(*), intent(out)              :: site_file_template
   character(*), intent(out)              :: filter_array_file_template
   logical,      intent(out)              :: stop_after_filter

   integer get_free_unit			! function def.

! Local variables.

   integer, parameter :: max_table_size = 20	! max number of lines in any
   						! single table in config file;
						! at least one more than needed

   character(200) lines(max_table_size)		! line buffer for maximal table

   character(200) errmsg
   character(100) header_expected
   character(20) limits(2)			! input strings for 2 limits
   character circular_str*10, suffix*1

   integer j, n, vi, ios, status
   integer cf					! unit number for config file
   integer lnum					! line number within config file
   integer nvars				! size of var table

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
   do					! structure for escape handling only

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

      call get_param_string ('grid coordinate file', grid_coord_file, &
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

! Parse table lines.  Fortran free format, space delimited.
! Only read the first four columns.  Any remainders are comments.
! Must read columns 2 and 3 as strings, because of possible C suffix.

      allocate (analog_vars(nvars), lower_limits(nvars))
      allocate (upper_limits(nvars), is_circular(nvars))

var_loop: &
      do vi = 1, nvars
         print '(a)', trim (lines(vi))		! progress display & diagnostic

         read (lines(vi), *, iostat=ios, iomsg=errmsg) analog_vars(vi), &
            limits(1:2), circular_str		! re-read into substrings

         call check_read_error (ios, errmsg, lines(vi))

         is_circular(vi) = (circular_str == 'Y' .or. circular_str == 'y')

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

            if (n == 1) lower_limits(vi) = num	! insert value in proper array
            if (n == 2) upper_limits(vi) = num
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
! Read post processing and debug controls.
!-----------------------------------------------------------

      call get_param_string ('filter method', filter_method, cf, status, lnum, &
         nonblank)
      if (status /= normal) exit read_file

      call get_param_int ('number of analogs', num_analogs, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_string ('output limit method', output_limit_method, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

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
! End of read_file block.  Normal and error exits.
!-----------------------------------------------------------

! Catch aborts from read_file block.

   if (status /= normal) then
      print '(a,i0)', '*** read_config_file_main: Abort, config file line' &
         // ' number = ', lnum
      call exit (1)
   end if

! All done, output arguments are already assigned.  Return to caller.

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
