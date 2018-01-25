!------------------------------------------------------------------------------
!
! Get standard command line arguments for bias correction main programs.
!
! This is a support routine for the NOAA/NCAR bias correction system
! for CMAQ forecast outputs.
!
! 2014-apr-23	get_command_args.f90:
!		Original version for interpolate_update.  By Dave Allured.
! 2014-may-12	Add optional keyword argument, "diag=N" for verbosity control.
! 2014-may-29	Comment changes, same routine now supports the bias_correct
!		  main program.
!		Add sanity checks for the two date arguments.
! 2014-jun-23	Echo command arguments to console.
! 2014-jul-23	Fix program name in error message.
!
! 2017-may-17	get_command_args.main.f90:
!		Break off custom version for main program bias_correct.f90.
!		Add gen_weights keyword argument.
! 		Add general processing for keyword arguments.
! 2017-jun-06	Add keyword arguments for start and end weight set numbers.
! 2017-jul-11	Add alternate spelling, either gen_weights or gen-weights.
!
! Sample commands:
!
! ./interpolate_update config.interp.0424 12z 20140515 20140529 diag=4
! ./bias_correct       config.main.0529   12z 20140515 20140529 diag=4
!
! ./bias_correct config.ozone 06z 20160701 20170331 gen_weights diag=3
! ./bias_correct config.ozone 06z 20160701 20170331 \
!                                     gen_weights weight1=101 weight2=150
!
! The first four arguments are required.  Remaining arguments are
! optional keyword arguments.
!
! Notes:
!
! This is the command line interpreter for the main program
! bias_correct.f90.
!
! In addition to the main command arguments, base_year is returned
! to the main program.  This is to be used as the reference year
! for all date indices within the current run.  This is a system
! for linear time indexing on the real-world calendar.
!
! base_year is arbitrarily chosen to be the year that includes the
! specified start date.  This ensures that all date indices within
! the current run will be small positive integers.
!
!------------------------------------------------------------------------------

module get__command_args
contains

subroutine get_command_args (prog_name, calendar, config_file, cycle_time, &
      start_date, end_date, base_year, diag, gen_weights, weight1, weight2)

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
   logical,      intent(  out) :: gen_weights	! true = weight generation mode
   integer,      intent(  out) :: weight1	! first & last weight set nums;
   integer,      intent(  out) :: weight2	! neg. = no subset, gen all sets

! Local variables.

   character(20) cycle_str, start_date_str, end_date_str
   character(80) arg, name, value

   integer ios, ndays, equal
   integer iarg, nargs, last_required_arg

   logical valid

   real, parameter :: max_days = 200 * 365.25	! max allowed date interval

!-------------------------------------------------
! Part 1.  Fixed position arguments.
!-------------------------------------------------

! Get fixed command arguments as strings.

   call get_command_argument (1, config_file)
   call get_command_argument (2, cycle_str)
   call get_command_argument (3, start_date_str)
   call get_command_argument (4, end_date_str)

! Echo fixed arguments for log file.

   print *
   print '(2a)', 'Command line arguments:'
   print '(2a)', '  Config file = ', trim (config_file)
   print '(2a)', '  Cycle time  = ', trim (cycle_str)
   print '(2a)', '  Start date  = ', trim (start_date_str)
   print '(2a)', '  End date    = ', trim (end_date_str)

! Check number of arguments.

   nargs = command_argument_count ()

   last_required_arg = 4

   if (nargs < last_required_arg) then
      print *
      print *, '*** get_command_args: Abort, missing command arguments.'
      print *, '*** At least four arguments are required.'
      print *, '*** Usage: ./' // trim (prog_name) &
        // ' config-file cycle-time-Z start-date end-date diag=N gen_weights'
      print *, '*** E.g.:  ./' // trim (prog_name) &
        // ' config.interp.zeus.0422 12Z 20140101 20140422'
      call exit (1)
   end if

! Validate the cycle time, and convert to integer.

   call lowercase (cycle_str)			! case insensitive

   valid = any (cycle_str == (/ '00z', '06z', '12z', '18z' /) )
   						! only valid cycles allowed
   if (.not. valid) then
      print *, '*** get_command_args: Abort, invalid cycle time.'
      print *, '*** Must be 00Z, 06Z, 12Z, or 18Z.  Must be two digits plus "Z".'
      call exit (1)
   end if

   call string_to_intu (cycle_str(1:2), cycle_time)   ! string to integer

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

!-------------------------------------------------
! Part 2.  Keyword arguments, all optional.
!-------------------------------------------------

! Set defaults for keyword arguments.
! Default for "diag" is already set by caller.

   gen_weights = .false.

   weight1     = -99			! default negative = no subset
   weight2     = -99			!   selected; generate all weight sets

! Loop over all remaining arguments on command line.
! Keyword arguments may be in any order.

! Duplicate arguments are NOT detected at this time.
! The last argument setting takes precedence.

keyword_loop: &
   do iarg = last_required_arg+1, nargs

      call get_command_argument (iarg, arg)	! get next arg as string

! Split arguments that include an equal sign.

      equal = index (arg, '=')

      if (equal /= 0) then
         name  = arg(1:equal)		! keep equal sign with the keyword
         value = arg(equal+1:)
      else
         name  = arg
         value = ' '
      end if

! Optional diag argument.  If missing, caller's original "diag" value
! will be left unchanged as the default.

      if (name == 'diag=') then
         print '(2a)', '  Diag level  = ', trim (value)

         call string_to_int (trim (value), diag, valid)
				! suffix to integer, and check for valid number
         if (.not. valid) then
            print *, '*** get_command_args: Abort, invalid diag argument.'
            print *, '*** Must be "diag=N", where N is a signed integer.'
            call exit (1)
         end if

         cycle keyword_loop

! Optional gen_weights argument.

      else if (arg == 'gen_weights' .or. arg == 'gen-weights') then
         print '(2a)', '  Gen_weights = enabled'
         gen_weights = .true.
         cycle keyword_loop

! Optional weight1 argument.

      else if (name == 'weight1=') then
         print '(2a)', '  weight1 = weight subset start = ', trim (value)

         call string_to_intu (trim (value), weight1)
				! suffix to integer, and check for valid number
         if (weight1 < 1) then
            print *, '*** get_command_args: Abort, invalid weight1 argument.'
            print *, '*** Must be "weight1=N", where N is a positive integer.'
            call exit (1)
         end if

         cycle keyword_loop

! Optional weight2 argument.

      else if (name == 'weight2=') then
         print '(2a)', '  weight2 = weight subset end   = ', trim (value)

         call string_to_intu (trim (value), weight2)
				! suffix to integer, and check for valid number
         if (weight2 < 1) then
            print *, '*** get_command_args: Abort, invalid weight2 argument.'
            print *, '*** Must be "weight2=N", where N is a positive integer.'
            call exit (1)
         end if

         cycle keyword_loop

! Trap unrecognized argument.

      else
         print *, '*** get_command_args: Abort.'
         print *, '*** Unknown command argument: "' // trim (arg) // '"'
         call exit (1)
      end if

   end do keyword_loop

! Report status of missing optional arguments.

   if (.not. gen_weights) print '(2a)', '  Gen_weights = not enabled'

end subroutine get_command_args
end module get__command_args
