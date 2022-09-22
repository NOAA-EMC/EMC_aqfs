!------------------------------------------------------------------------------
!
! blackout_obs_data.f90 -- Erase obs input data for a given blackout interval.
!
! This is a support routine for the NOAA/NCAR bias correction
! system for CMAQ forecast outputs.
!
! 2017-jun-05	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2019-jul-05	Bug fix.  Fix handling of blackout intervals that overlap
!		  ends of the available obs time range.
!
! This routine sets all obs input data to missing values, within
! the specified problematic time interval, such as around the
! Fourth of July.  Blackout is applied to the same dates in every
! calendar year in the current obs data.
!
! The interval is specified to one-hour resolution, by specifying
! calendar dates and hour numbers.  The given interval is
! inclusive.  Both start and end hours are blacked out, along
! with all hours in between.
!
! Blackout is disabled by setting obs_blackout_start to negative
! numbers.  This setting is provided by the config file parser
! when no blackout dates are selected.
!
!------------------------------------------------------------------------------

module blackout__obs_data
contains

subroutine blackout_obs_data (obs, vmiss, obs_start_date, obs_end_date, &
      base_year, calendar, obs_blackout_start, obs_blackout_end, varname, &
      units, diag)

   use config, only : dp
   use date_utils
   implicit none

! Hourly obs time series array to be modified.

   real(dp),     intent(inout) :: obs(:,:)	! (times, sites)
						! always 0Z start, 23Z end
! Input arguments.

   real(dp),     intent(in) :: vmiss		! missing value in data
   integer,      intent(in) :: obs_start_date	! date index, start of obs data
   integer,      intent(in) :: obs_end_date	! date index, end of obs data
   integer,      intent(in) :: base_year	! base year for date indexes
   character(*), intent(in) :: calendar		! calendar system in use
   integer,      intent(in) :: obs_blackout_start(3)	! month, day, hour
   integer,      intent(in) :: obs_blackout_end(3)	! month, day, hour
   character(*), intent(in) :: varname		! obs var name
   character(*), intent(in) :: units		! obs var units string
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Local variables.

   character fmt1*120

   integer mon_start, day_start, hour_start
   integer mon_end, day_end, hour_end
   integer year1, month1, day1
   integer year2, month2, day2
   integer year, ntimes, ti1, ti2, ti1a, ti2a, ind1, ind2
   integer time_dim, nsites_valid, nmiss

   real (dp) vmin, vmax, percent_miss

   logical changed

! Get blackout dates and times.

   if (diag >= 2) print *
   if (diag >= 2) print *, 'blackout_obs_data: Start.'

   mon_start  = obs_blackout_start(1)		! months 1-12
   day_start  = obs_blackout_start(2)		! days   1-31
   hour_start = obs_blackout_start(3)		! hours  0-23

   mon_end    = obs_blackout_end(1)
   day_end    = obs_blackout_end(2)
   hour_end   = obs_blackout_end(3)

! Return if blackout dates not requested.

   if (mon_start < 0) then
      if (diag >= 2) &
         print *, '  No blackout dates requested.  Leave obs data unchanged.'
         print *
      return
   end if

! Get first and last calendar years in obs data.

   call index_to_date (obs_start_date, year1, month1, day1, base_year, calendar)
   call index_to_date (obs_end_date,   year2, month2, day2, base_year, calendar)

! Apply blackout times for each available calendar year within data.

   ntimes  = size (obs, 1)
   changed = .false.

   do year = year1, year2

! Get date indexes for start and end blackout dates, for current year.
! Function will validate and abort with diagnostic on invalid calendar dates.

      ind1 = date_index (year, mon_start, day_start, base_year, calendar)
      ind2 = date_index (year, mon_end,   day_end,   base_year, calendar)

! Get obs time indices for hours to black out.
! Get the virtual window within the current year.

      ti1 = 24 * (ind1 - obs_start_date) + hour_start
      ti2 = 24 * (ind2 - obs_start_date) + hour_end

      if (diag >= 4) then
         fmt1 = '(3x, a, 99i6)'
         print *
         print fmt1,'y1 m1 d1 h1 ind1 ti1 =', year, mon_start, day_start, &
                                                hour_start, ind1, ti1
         print fmt1,'y2 m2 d2 h2 ind2 ti2 =', year, mon_end, day_end, &
                                                hour_end, ind2, ti2
      end if

      if (ti1 > ti2) then
         print *, '*** blackout_obs_data: Invalid blackout dates and times' &
            // ' specified.  Abort.'
         call exit (1)
      end if

! Constrain the virtual time window to the available obs time range.

      ti1a = max (ti1, 1)
      ti2a = min (ti2, ntimes)

! If the actual constrained window is within available obs time range,
! set obs within actual window to missing values.

      if (ti1a <= ti2a) then	   ! window is zero or negative if out of range
         if (diag >= 2) then
            fmt1 = "(3x, a, i4, 2('-',i2.2), i3.2, a, i4, 2('-',i2.2), i3.2," &
               // " a, i0, a, i0)"
            print fmt1, 'Set obs to missing: ', year, mon_start, day_start, &
               hour_start, 'Z to ', year, mon_end, day_end, hour_end, &
               'Z, time indices ', ti1, ' to ', ti2
            if ( (ti1 /= ti1a) .or. (ti2 /= ti2a) ) &
               print '(8x,3(a,i0))', '(Constrained to available obs time' &
                  // ' range, actual time indices ', ti1a, ' to ', ti2a, ')'
         end if
         obs (ti1a:ti2a,:) = vmiss
         changed = .true.
      end if

   end do

! Show final obs statistics.

   if ( (diag >= 2) .and. (.not. changed) ) then
      print *, '*** No blackout dates within the current training period.'
      print *, '*** Leave obs data unchanged.'
   end if

   if (diag >= 2) then
      print *
      print *, 'blackout_obs_data: Obs summary after blackout:'

      time_dim = 1
      nsites_valid = count (any ( (obs /= vmiss), time_dim) )
      					! count <- (sites) <- (times, sites)

      vmin  = minval ( obs, (obs /= vmiss) )
      vmax  = maxval ( obs, (obs /= vmiss) )

      nmiss = count (obs == vmiss)
      percent_miss = (nmiss * 100.0_dp) / size (obs)

      print '(2a)',           '   Var name                  = ', trim (varname)
      print '(2a)',           '   Obs data units            = ', trim (units)
      print '(a,i0)',         '   No. valid sites remaining = ', nsites_valid
      print '(2(a,g0.4))',    "   Min, max obs remaining    = ", vmin,', ',vmax
      print '(a,i0,a,f0.1,a)','   Number of missing values  = ', nmiss, ' (', &
         percent_miss, '%)'
      print '(2a)',           '     after blackout applied'
   end if

   if (diag >= 3) print *
   if (diag >= 3) print *, 'blackout_obs_data: Return.'
   print *

end subroutine blackout_obs_data
end module blackout__obs_data
