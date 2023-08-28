!------------------------------------------------------------------------------
!
! Get standard command line arguments for bias correction main programs.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-apr-23	Original version for interpolate_update.  By Dave Allured.
! 2014-may-12	Add optional keyword argument, "diag=N" for verbosity control.
! 2014-may-29	Comment changes, same routine now supports the bias_correct
!		  main program.
!		Add sanity checks for the two date arguments.
! 2014-jun-23	Echo command arguments to console.
! 2014-jul-23	Fix program name in error message.
!
! 2019-may-16	Add third command line argument, number of forecast hours.
!
! 2022-apr-11	Revert to previous, remove # forecast hours from command line.
!		This parameter moved to config file.
!
! Sample commands:
!
! ./interpolate_update config.interp.pm2.5.5-vars 12z 20140515 20140529 diag=4
! ./interpolate_update config.interp.ozone.7-vars 12z 20190801 20190830
!
! The first four arguments are required.  The fifth argument is
! an optional keyword argument.
!
! Notes:
!
! This routine is called by interpolate_update.f90 to read the
! command line arguments.
!
! In addition to the main command arguments, base_year is returned
! to the calling program.  This is to be used as the reference year
! for all date indices within the current run.  This is a system
! for simple linear date indexing on the real-world calendar.
!
! Base_year is arbitrarily chosen to be the year that includes the
! specified start date.  This ensures that all date indices within
! the current run will be small positive integers.
!
!------------------------------------------------------------------------------

module get__command_args
contains

subroutine get_command_args (prog_name, calendar, config_file, cycle_time, &
      start_date, end_date, base_year, diag)

   use date__index
   use string_utils
   implicit none

   character(*), intent(in   ) :: prog_name	! name of calling program
   character(*), intent(in   ) :: calendar

   character(*), intent(  out) :: config_file
   integer,      intent(  out) :: cycle_time
   integer,      intent(  out) :: start_date, end_date
   integer,      intent(  out) :: base_year
   integer,      intent(inout) :: diag		! set diag to desired default
						! value, before calling

! Local variables.

   character(20) cycle_str, start_date_str, end_date_str
   character(40) diag_str

   integer ios, ndays
   logical valid

   real, parameter :: max_days = 200 * 365.25	! max allowed date interval

! Get command arguments as strings.

   call get_command_argument (1, config_file)
   call get_command_argument (2, cycle_str)
   call get_command_argument (3, start_date_str)
   call get_command_argument (4, end_date_str)
   call get_command_argument (5, diag_str)

! Echo arguments for log file.

   if (diag >= 1) then
      print *
      print '(2a)', 'Command line arguments:'
      print '(2a)', '  Config file = ', trim (config_file)
      print '(2a)', '  Cycle time  = ', trim (cycle_str)
      print '(2a)', '  Start date  = ', trim (start_date_str)
      print '(2a)', '  End date    = ', trim (end_date_str)
      print '(2a)', '  Diag level  = ', trim (diag_str)
   end if

! Check number of arguments.

   if (end_date_str == ' ') then
      print *, '*** get_command_args: Abort, missing command arguments.'
      print *, '*** Four arguments are needed, the fifth is optional.'
      print *, '*** Usage: ./' // trim (prog_name) &
        // ' config-file cycle-time-Z start-date end-date diag=N'
      print *, '*** E.g.:  ./' // trim (prog_name) &
        // ' config.interp.theia.0422 12Z 20140101 20140422'
      call exit (1)
   end if

! Validate the cycle time, and convert to integer.

   call lowercase (cycle_str)			! case insensitive

   valid = any (cycle_str == (/ '00z', '06z', '12z', '18z' /) )
   						! only valid cycles allowed
   if (.not. valid) then
      print *,'*** get_command_args: Abort, invalid cycle time.'
      print *,'*** Must be 00Z, 06Z, 12Z, or 18Z.  Must be two digits plus "Z".'
      call exit (1)
   end if

   call string_to_intu (cycle_str(1:2), cycle_time) ! conv. to unsigned integer

! Set the time line reference year to the first specified year.

   read (start_date_str(1:4), *, iostat=ios) base_year

   if (ios /= 0) base_year = 1800	! ignore errors, will get caught next

! Validate and convert date arguments to integer date indices
! (days since start of base_year, 1-based.  1 = January 1).

   start_date = yyyymmdd_to_index (start_date_str, base_year, calendar)
   end_date   = yyyymmdd_to_index (end_date_str,   base_year, calendar)

   if (start_date > end_date) then
      print *, '*** get_command_args: Abort, date error on command line.'
      print *, '*** Start date must not be later than end date.'
      call exit (1)
   end if

! Check for unreasonable interval.  Really just checking for gross
! date error.

   ndays = end_date - start_date + 1

   if (ndays > max_days) then
      print *, '*** get_command_args: Abort, date error on command line.'
      print *, '*** Excessive interval or length of training period.'
      call exit (1)
   end if

! Process the optional "diag" keyword argument.
! If missing, leave the original "diag" value unchanged.

   if (diag_str /= ' ') then			! if blank, do not process

      call lowercase (diag_str)			! case insensitive
      valid = (diag_str(1:5) == 'diag=')	! check for proper keyword

      if (valid) call string_to_int (trim (diag_str(6:)), diag, valid)
				! suffix to integer, and check for valid number
      if (.not. valid) then
         print *, '*** get_command_args: Abort, invalid diag argument.'
         print *, '*** Must be "diag=N", where N is a signed integer.'
         call exit (1)
      end if

   end if

end subroutine get_command_args

end module get__command_args
