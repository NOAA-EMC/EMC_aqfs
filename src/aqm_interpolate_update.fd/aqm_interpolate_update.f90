!------------------------------------------------------------------------------
!
! interpolate_update.f90
!
! Interpolate gridded raw 48-hour forecasts to station locations.
! Update day files in a local archive, as needed.
!
! This is the "interpolation module", one of the four major
! components of the NOAA NCO/ARL/PSD bias correction system
! for CMAQ forecast outputs.
!
! This same program is used both to generate the original
! interpolated local archive, and to add current day files
! on the fly in production mode.
!
! 2014-may-10	Original version of main program and process control.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Core interpolation routines by Irina Djalalova, NOAA/ESRL/PSD3.
! 2014-may-12	Optimize sparse progress display for diag level 2.
! 2014-jul-23	Fix program name in error message.
!
! Input:
!
! Configuration file, with paths and list of vars to include.
! Station file with list of station ID's and coordinates.
! Gridded raw forecast files for MET and CMAQ models.
! Miscellaneous hard coded parameters, see below.
!
! Output:
!
! Forecast data interpolated to station coordinates.
! One file per day, all variables in each file.
!
! Notes:
!
! Output files are created only if they are not already present
! in the interpolated archive.  This version does not overwrite
! or update existing files.
!
! If updates or changes to existing interpolated files are
! needed, then first delete the old files.  Then run this program
! to make replacements.
!
! If the station file changes, the entire archive should be
! regenerated from scratch, so that station ordering and site
! coordinates will remain consistent between interpolated files.
!
!------------------------------------------------------------------------------
!
! Usage, run command with four or five arguments:
!
! ./interpolate_update config-file cycle-time start-date end-date diag=N
!
! config-file	Interpolator config file for current platform and
!		data set configuration.  Includes explicit file
!		pointers and list of target variables to be
!		included in the local archive.
!
! cycle-time	HH, model cycle time, currently 06 or 12.
!		Set to $Cyc for WCOSS real-time operation.
!
! start-date	YYYYMMDD, start and end forecast dates to process.
! end-date
!
! diag=N	Set verbosity level N.  Optional argument.
!		The default is set below in this program.
!
! Example:
!
! ./interpolate_update config.interp.0424 12z 20140101 20140510
!
!------------------------------------------------------------------------------

program interpolate_update

   use config, only : dp
   use get__command_args
   use expand__filename
   use index_to_date_mod
   use interpolate_mod
   use read__config_file
   use read__gridded_vars
   use read__station_file
   use stdlit
   use write__interp_netcdf
   implicit none

! Local variables.

   character(200) config_file, station_file, grid_coord_file
   character(200) interp_file, interp_file_template
   character fmt1*20

   integer nsites, nhours
   integer cycle_time, first_date, last_date, base_year
   integer date_index, year, month, day
   integer diag, status
   integer ndays_total, nskip_exist, ndays_error, nwrite

   real vmiss
   logical ex

! Dynamic arrays.

   character(80),  allocatable :: varnames(:)		   ! var config data (V)
   character(30),  allocatable :: reader_codes(:)	   ! (V)
   character(200), allocatable :: grid_file_templates(:)   ! (V)

   character(9), allocatable :: site_ids(:)		    ! site ID's (S)
   real(dp),     allocatable :: site_lats(:), site_lons(:)  ! site coords (S)

   real(dp), allocatable :: grid_lats(:,:)		! grid coordinates (X,Y)
   real(dp), allocatable :: grid_lons(:,:)

   real, allocatable :: grid_data(:,:,:,:)		! gridded forecast data
  							!  (X, Y, vars, hours)
   real, allocatable :: interp_data(:,:,:)		! interpolated data
  							!  (sites, vars, hours)
! Program parameters.

   character(*), parameter :: prog_name = 'interpolate_update'
   character(*), parameter :: calendar  = 'gregorian'

! Run parameters.

   print *, 'interpolate_update.f90: Start.'

   diag = 2		! set default verbosity: 0 = errors only,
   			! 1 = milestones, 2 = brief progress,
                        ! 3 and up = increasing detail level.

!   nhours = 48		! number of forecast hours in each forecast cycle
   vmiss  = -999	! assumed missing value in data

! Initialize.

   nwrite      = 0				! clear statistics
   nskip_exist = 0
   ndays_error = 0

! Get command line parameters.

   call get_command_args (prog_name, calendar, config_file, cycle_time, &
      first_date, last_date, base_year, diag)

   print*,"hjp999,cycle_time=",cycle_time 
   if ( cycle_time .eq. 6 .or. cycle_time .eq. 12 ) then
     nhours = 72 
   else
     nhours  = 6
   endif

! Read and process the configuration file.

   call read_config_file (config_file, station_file, grid_coord_file, &
      interp_file_template, varnames, reader_codes, grid_file_templates)

! Read site coordinates for interpolation.

   call read_station_file (station_file, site_ids, site_lats, site_lons, nsites)

   if (diag >= 2)  print *

!-----------------------------------------------------------
! Main loop over each date in requested processing period.
!-----------------------------------------------------------

date_loop: &
   do date_index = first_date, last_date

      call index_to_date (date_index, year, month, day, base_year, calendar)
      					! get current Y M D integers

      call expand_filename (interp_file_template, year, month, day, &
         cycle_time, interp_file)	! make interp file name for current date

! Skip this date if an interpolated file for for this date is already made.
! This program only makes new output files, it does not overwrite or update.

      inquire (file=interp_file, exist=ex)

      if (ex) then
         if (diag >= 2) &
            print '(2a)', ' Skipping, made previously: ', trim (interp_file)
         nskip_exist = nskip_exist + 1		! count days skipped because
         cycle date_loop			! file made previously
      end if

! Read all gridded variables to be included in interpolated archive.
! To include all possible file configurations, this routine also reads
! the companion grid coordinates.

      call read_gridded_vars (varnames, reader_codes, grid_file_templates, &
         grid_coord_file, year, month, day, cycle_time, nhours, diag, &
         grid_data, grid_lats, grid_lons, status)

! Skip this date if there is a serious problem with the input data.

      if (status /= normal) then
         ndays_error = ndays_error + 1		! count days with input errors
         cycle date_loop
      end if

! Interpolate the whole data chunk to site locations, for current date.

      call interpolate (grid_data, vmiss, grid_lats, grid_lons, site_lats, &
         site_lons, diag, interp_data)

! Write the output file for this date.

      call write_interp_netcdf (interp_file, site_ids, site_lats, site_lons, &
         varnames, interp_data, vmiss, diag)

      nwrite = nwrite + 1			! count files actually written

   end do date_loop

! Print run summary.

   ndays_total = last_date - first_date + 1

   fmt1 = '(1x,a,i0)'

   print *,    '-------------------------------------------------------------'
   print *,    'Summary of interpolation run:'
   print fmt1, 'Total number of days in run period       = ', ndays_total
   print fmt1, 'Days skipped, files already created      = ', nskip_exist
   print fmt1, 'Days skipped, bad or missing input files = ', ndays_error
   print fmt1, 'Number of new day files written          = ', nwrite

   print *
   print *, 'interpolate.f90: Done.'

end program interpolate_update
