!------------------------------------------------------------------------------
!
! read_obs_qc.f90 -- Read obs time series data for the bias correction system.
!		     Perform data quality control on the fly.
!
! This is the top level routine of the quality control module,
! one of the four main components of the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jun-10	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Adapted from read_gridded_vars.f90, version 2014-may-13.
!		For EPA AIRNow BUFR data sets received by NOAA NWS/NCO.
! 2014-jun-12	Switch from symbolic var name to obs var name, for
!		  deciding units conversion.
!		Eliminate sites with all missing data.
! 2014-jun-19	Remove new_site_list_template calling argument.
!		-- This functionality moved to main program.
!		Add a few time logs.
! 2014-jun-22	Eliminate off-grid sites.
!		Restructure the elimination code, add flag array.
! 2014-jun-26	Format bug fix, caught by ifort.
!
! Input:   infile_template = Path template for input data set.  May include
!		leading environment var, and YYYY MM DD substitution strings.
!          varname = BUFR field name for requested data variable, e.g. COPOPM.
!	   start_date, end_date = First and last dates of requested time series.
!          Also see secondary input parameters below.
!
! Output:  site_ids = Site ID strings.
!	   site_lats, site_lons = Site coordinates.
!	   out_data = 2-D array for time series data.  (Time, sites).
!		Contigious hourly time series, no skipped days.
!	   units = Units string for requested variable.  This is the units
!		after automatic units conversion, NOT the units in the
!		original data set.  The returned units string is correct for
!		the returned data array.
!
! Notes:
!
! Several subsystems are incorporated in this routine:
!
! * Read raw obs time series (BUFR reader).
! * Automatic units conversion.
! * Missing value conversion.
! * Obs quality control.
!
! This routine reads time series for one selected variable, all
! sites, and all available dates within the requested time
! period.  Quality control is then performed on the fly, before
! returning QC'd time series to the calling program.
!
! Hourly time series are returned in a 2-D array, time x sites.
! Output arrays are auto-allocated for all sites that pass QC,
! and for the requested time interval.  Missing values are
! inserted for dates and times not available.
!
! Missing values in the input data set are converted to the
! caller's specified missing value.  As of the current version,
! 2014-jun-9, this missing value code must be a negative number,
! for correct operation of the qc_single_station subroutine.
!
! The input data unit for PM2.5 is automatically converted to
! a standard unit which is expected by both the QC subroutine
! and the remainder of the bias correction process.  A units
! string for the converted unit is returned to the caller.
!
! Start and end date indexes:  Integer Gregorian dates, relative
! to 1 = January 1 of base_year.  See index_to_date library
! routine for details.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = sparse,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
! Error handling:
!
! Single-value soft errors are handled invisibly by a lower layer.
! Console warning messages may be produced for some kinds of
! these errors,  Single-value errors are replaced with missing
! values in the result array.
!
! Missing or invalid files are also treated as soft errors, and
! ignored.  Data for such files is included in the output time
! series as all missing values.
!
! Serious errors may be detected at a lower level, resulting in
! a diagnostic message and program abort.
!
!------------------------------------------------------------------------------

module read__obs_qc
contains

subroutine read_obs_qc (infile_template, varname, start_date, end_date, &
      base_year, vmiss, grid_lats, grid_lons, diag, site_ids, site_lats, &
      site_lons, out_data, units)

   use config, only : dp
   use grid_location
   use qc__single_site
   use read__obs_series
   implicit none

! Input arguments.

   character(*), intent(in) :: infile_template	! template for input file paths
   character(*), intent(in) :: varname		! requested var name
   integer,      intent(in) :: start_date	! starting date index
   integer,      intent(in) :: end_date		! ending date index
   integer,      intent(in) :: base_year	! base year for date indexes
   real(dp),     intent(in) :: vmiss		! caller specified missing value
   real(dp),     intent(in) :: grid_lats(:,:)	! 2-D grid coordinates (X,Y)
   real(dp),     intent(in) :: grid_lons(:,:)	!   for screening site locations
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

   character(*), intent(out), allocatable :: site_ids(:)   ! site ID strings (S)
   real(dp),     intent(out), allocatable :: site_lats(:)  ! site coords (S)
   real(dp),     intent(out), allocatable :: site_lons(:)
   real(dp),     intent(out), allocatable :: out_data(:,:) ! time series (T,S)
   character(*), intent(out)              :: units	   ! var units string

! Local variables.

   character(80) orig_units, orig_units2, units_needed
   character fmt_vmiss*40, fdate_str*24

   integer si, si2, nsites_in, ntimes
   integer nvalid, nmiss
   integer nx, ny, noff_grid
   integer nsites_valid, nodata1, nsites_nodata

   real(dp) vmiss_in, vmin, vmax, percent_miss
   real(dp) multiplier

   logical off_grid

   integer, allocatable :: i_corners(:), j_corners(:)	! grid indices
   logical, allocatable :: valid(:)			! track valid sites

! Dynamic arrays for site input data.

   character(len(site_ids)), allocatable :: in_ids(:)	! site ID strings (S)
   real(dp), allocatable :: in_lats(:)			! site coordinates (S)
   real(dp), allocatable :: in_lons(:)
   real(dp), allocatable :: in_data(:,:)		! time series (T,S)

!-----------------------------------------------------------
! Read obs time series for all sites, all requested dates.
!-----------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_obs_series.'

   call read_obs_series (infile_template, varname, start_date, end_date, &
      base_year, diag, in_ids, in_lats, in_lons, in_data, vmiss_in, orig_units)

   ntimes    = size (in_data, 1)		! get time dimensions
   nsites_in = size (in_data, 2)		! get site dimension

! Show raw input statistics.

   print *, &
      '======================================================================='

   fmt_vmiss = '(a,g19.12,a,es9.2,a)'	! format for missing value diags

   if (diag >= 2) then
      vmin  = minval ( in_data, (in_data /= vmiss_in) )
      vmax  = maxval ( in_data, (in_data /= vmiss_in) )

      nmiss = count (in_data == vmiss_in)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      nodata1 = 0
      do si = 1, nsites_in
         if (all (in_data(:,si) == vmiss_in)) nodata1 = nodata1 + 1
      end do

      print '(a,i0)',         ' read_obs_qc:  nsites_in      = ', nsites_in
      print '(a,i0)',         '   No. of sites with no data  = ', nodata1
      print '(a,i0)',         '   Number of time steps       = ', ntimes
      print '(2(a,g0.4))',    '   Min, max raw input data    = ', vmin,', ',vmax
      print '(a,i0,a,f0.1,a)','   Number of missing values   = ', nmiss, &
         ' (', percent_miss, '%)'
      print fmt_vmiss,        '   Missing value              =',  vmiss_in, &
         ' (', vmiss_in, ' single precision)'
      print *
   end if

   call fdate (fdate_str)
   print '(3a)', fdate_str, '  read_obs_qc: Read obs complete.', &
      '  Run QC procedures.'

!-----------------------------------------------------------
! Units conversion of input data.
!-----------------------------------------------------------

! Must perform any needed units conversion BEFORE calling QC routine.

   print *, 'read_obs_qc:  Check for units conversion.'

   orig_units2 = orig_units			! setup for case insensitive
   call lowercase (orig_units2)

! Adaptive method.  First determine the needed units, if any.
! This is a cheap table lookup.  Could be tablified.

   units_needed = 'any'
   if (varname == 'COPOPM') units_needed = 'microgram/m^3'

   if (diag >= 2) then
      print *, '  Var name                   = ', trim (varname)
      print *, '  Input data units           = ', trim (orig_units)
      print *, '  Units needed               = ', trim (units_needed)
      print *
   end if

! Then check for available unit conversions.
! This is another cheap table.  Could also be tablified.

   multiplier = -777			! default to "no conversion" magic num.

   if (units_needed == 'microgram/m^3') then
      if (any (orig_units2 == (/ 'kg/(m**3)', 'kg m-3   ' /))) &
         multiplier = 1.0e9_dp			! kilograms to micrograms
   end if

! Diagnostics for no conversion.

   if (multiplier == -777) then			! if no conversion...
      units = orig_units			! keep origina units

      if (units_needed == 'any') then
         print *, '  No units conversion is needed.  Keep original units.'
      else
         print *, '*** read_obs_qc: Warning: Needed units conversion' &
            // ' is not available.'
         print *, '*** Keeping original units: ', trim (units)
         print *, '*** Results may not be accurate.'
         print *, '*** Check input data, or upgrade this program.'
      end if

! If valid, perform the units conversion now.

! Ignore any possible missing value range conflict, for now.
! Currently the numbers for PM2.5 seem favorable.  Maybe check later.

   else
      units = units_needed			! set the output units
      print '(9a)', ' read_obs_qc:  Apply units conversion, ', &
         trim (orig_units), ' to ', trim (units)
      if (diag>=2) print '(a,es11.4)', '   Multiplier                 =', &
         multiplier
      where (in_data /= vmiss_in) in_data = in_data(:,:) * multiplier
   end if

! Show unit converted statistics.

   if (diag >= 2 .and. multiplier /= -777) then
      vmin  = minval ( in_data, (in_data /= vmiss_in) )
      vmax  = maxval ( in_data, (in_data /= vmiss_in) )

      nmiss = count (in_data == vmiss_in)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      print '(2(a,g0.4))',    '   Min, max converted data    = ', vmin,', ',vmax
      print '(a,i0,a,f0.1,a)','   Number of missing values   = ', nmiss, &
         ' (', percent_miss, '%)'
      print *
   end if

!-----------------------------------------------------------
! Convert to requested missing value.
!-----------------------------------------------------------

! This must also be done before calling the QC routine.
! This is required in this position because of dependencies within
! the current version of the QC subroutine.

   print *, 'read_obs_qc:  Convert to requested missing value code.'

   if (diag >= 2) then
      print fmt_vmiss, '   Input missing value        =', vmiss_in, &
         ' (', vmiss_in, ' single precision)'
      print fmt_vmiss, '   Requested missing value    =', vmiss, &
         ' (', vmiss,    ' single precision)'
   end if

! Same missing values, skip conversion.

   if (vmiss_in == vmiss) then
      print '(2a)', '   *** No conversion needed.'
      print '(2a)', '   *** Requested missing value is the same as in', &
         ' the original data.'

! Different missing values.  Do the conversion now.

   else
      where (in_data == vmiss_in) in_data = vmiss

! Ignore any possible missing value range conflict, for now.
! Currently the numbers for PM2.5 seem favorable.  Maybe check later.
! But still show revised statistics after missing value substitution.

      if (diag >= 2) then
         vmin  = minval ( in_data, (in_data /= vmiss) )
         vmax  = maxval ( in_data, (in_data /= vmiss) )

         nmiss = count (in_data == vmiss)
         percent_miss = (nmiss * 100.0_dp) / size (in_data)

         print '(2(a,g0.4))',     '   Min, max converted data    = ', vmin, &
            ', ', vmax
         print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
            ' (', percent_miss, '%)'
         print *
      end if
   end if

!-----------------------------------------------------------
! Run the main quality control procedure.
!-----------------------------------------------------------

   print *, 'read_obs_qc:  Run obs quality control for each site.'

   allocate (valid(nsites_in))			! clear site valid flags
   valid(:) = .false.

   if (diag>=3) print *, '     After QC:'
   if (diag>=3) print *, '  Site ID       Min data     Max data' &
      // '    No. valid   No. missing'

! Main QC loop over each site.

   nsites_valid = 0

   do si = 1, nsites_in
      call qc_single_site (in_data(:,si), vmiss, diag, in_ids(si))

      nmiss  = count  (in_data(:,si) == vmiss)
      nvalid = ntimes - nmiss

      valid(si) = (nvalid > 0)		      ! mark sites with or without data

      if (valid(si)) nsites_valid = nsites_valid + 1	! count sites with data

! Show statistics line for each site, if selected.

      if (diag >= 3) then
         vmin = minval ( in_data(:,si), (in_data(:,si) /= vmiss) )
         vmax = maxval ( in_data(:,si), (in_data(:,si) /= vmiss) )

         if (nvalid == 0) vmin = vmiss		! fix min and max display
         if (nvalid == 0) vmax = vmiss		! if all missing

         print '(3x,a,2f13.2,2i13)', in_ids(si), vmin, vmax, nvalid, nmiss
      end if
   end do

   nsites_nodata = nsites_in - nsites_valid

! Short summary of site counts.

   if (diag >= 3) print *

   if (diag >= 2 .or. nsites_valid == 0) then
      nmiss = count (in_data == vmiss)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      print '(a,i0)',          '   Number of input sites      = ', nsites_in
      print '(a,i0)',          '   No. valid sites after QC   = ', nsites_valid
      print '(a,i0)',          '   No. of sites with no data  = ', nsites_nodata
      print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
         ' (', percent_miss, '%)'
   end if

! Controlled halt if no valid sites after QC.

   if (nsites_valid == 0) then
      print *
      print '(2a)', '*** read_obs_series: Fatal error, no valid sites', &
         ' after QC.  Abort.'
      call exit (1)
   end if

!-----------------------------------------------------------
! Eliminate sites with off-grid coordinates.
!-----------------------------------------------------------

   if (diag>=2) print *
   print *, 'read_obs_qc:  Eliminate sites with off-grid coordinates.'

   if (diag>=3) print *, '  Call gridlocation, find site locations on', &
      ' output grid.'

! Compute grid indices of cell corners for the site locations.
! Off grid sites return -999 in i, j indices.

   allocate (i_corners(nsites_in), j_corners(nsites_in))

   call gridlocation (in_lats, in_lons, grid_lats, grid_lons, diag, &
      i_corners, j_corners)

! Eliminate off-grid sites with data.
! Sites with no data were already eliminated.

   nx = size (grid_lats, 1)			! get grid dimensions
   ny = size (grid_lats, 2)

   noff_grid = 0

   do si = 1, nsites_in
      off_grid = (    (i_corners(si) < 1 .or. i_corners(si) > nx) &
                 .or. (j_corners(si) < 1 .or. j_corners(si) > ny) )

      if (off_grid .and. valid(si)) then
         valid(si) = .false.
         noff_grid = noff_grid + 1

         in_data(:,si) = vmiss			! clear off-grid data to all
         					! missing, to fix statistics

         if (diag >= 2 .and. noff_grid == 1) then
            print *, 'Summary of off-grid sites, see details above:'
         end if

         if (diag>=2) print '(3a,f11.5,f12.5)', ' *** Eliminate off-grid', &
            ' site with data: ', in_ids(si), in_lats(si), in_lons(si)
      end if
   end do

   nsites_valid = nsites_valid - noff_grid	! adjust count of valid sites

   if (diag>=2) print *
   if (diag>=2) print '(2a,i0)', ' Number of additional sites eliminated', &
      ' for off grid = ', noff_grid

   if (diag>=2) print '(a,i0)',  ' Number of remaining valid sites = ', &
      nsites_valid

! Controlled halt if no valid sites after grid check.

   if (nsites_valid == 0) then
      print *
      print '(2a)', '*** read_obs_series: Fatal error, no valid sites', &
         ' after grid check.  Abort.'
      call exit (1)
   end if

!-----------------------------------------------------------
! Copy remaining data to output arrays.
!-----------------------------------------------------------

   if (diag>=2) print *
   if (diag>=2) print *, 'read_obs_qc:  Copy valid obs data to output arrays.'

   allocate (site_ids(nsites_valid), site_lats(nsites_valid))
   allocate (site_lons(nsites_valid), out_data(ntimes,nsites_valid))

   si2 = 0					! init output site index

   do si = 1, nsites_in
      if (valid(si)) then			! copy valid sites only
         si2 = si2 + 1				! increment output pointer
         site_ids(si2)    = in_ids(si)		! copy metadata
         site_lats(si2)   = in_lats(si)
         site_lons(si2)   = in_lons(si)
         out_data(:,si2)  = in_data(:,si)	! copy time series
      end if
   end do

   if (diag>=2) print '(a,i0)', '   Total number of sites eliminated = ', &
      (nsites_in - nsites_valid)

!-----------------------------------------------------------
! Show final QC statistics.
!-----------------------------------------------------------

   print *
   print *, 'read_obs_qc:  Final obs QC summary:'

   vmin  = minval ( out_data, (out_data /= vmiss) )
   vmax  = maxval ( out_data, (out_data /= vmiss) )

   nmiss = count (out_data == vmiss)
   percent_miss = (nmiss * 100.0_dp) / size (out_data)

   print '(2a)',            '   Var name                   = ', trim (varname)
   print '(2a)',            '   Output data units          = ', trim (units)
   print '(a,i0)',          '   No. valid sites after QC   = ', nsites_valid
   print '(2(a,g0.4))',     "   Min, max QC'ed data        = ", vmin, ', ', vmax
   print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, ' (', &
      percent_miss, '%)'
   print '(2a)',            '     after eliminating sites'
   print *

   if (diag>=3) print *, 'read_obs_qc:  QC complete.  Return to main program.'

end subroutine read_obs_qc
end module read__obs_qc
