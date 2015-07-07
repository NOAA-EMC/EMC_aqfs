!------------------------------------------------------------------------------
!
! read_gridded_aqm.f90 -- Read Netcdf gridded CMAQ and MET forecast data.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
!
! Notes:
!
! This reader is for the Netcdf gridded archive format for CMAQ
! and MET outputs, provided by Pius Lee, January 10, 2014.
!
! Each call returns gridded forecast data for a single Netcdf
! variable, and a single forecast cycle.
!
! The returned data array has three dimensions: X, Y, and
! forecast hours.  These correspond to the original file
! dimensions COL, ROW, and TSTEP.  The vestigial LAY dimension
! is removed.
!
! The forecast date and cycle start time are implicit, because
! these are embedded in each Netcdf file name.  It is the
! caller's responsibility to align the input day files into a
! correct time sequence.
!
! This routine converts the raw input data in double precision,
! to the single precision output array.
!
!------------------------------------------------------------------------------

module read__gridded_aqm

contains

subroutine read_gridded_aqm (infile, varname, diag, vdata, status)

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   implicit none

   character(*), intent(in ) :: infile		! input file name
   character(*), intent(in ) :: varname		! requested var name
   integer,      intent(in ) :: diag		! verbosity level, 0-N

   real,         intent(out) :: vdata(:,:,:)	! output array (X, Y, hours)
   integer,      intent(out) :: status		! result status, normal or
   						!   fail (stdlit)
! Local variables.

   character fmt1*50
   integer nhours
   integer dims_in4(4), dims_in3(3), expect2(3)
   logical fail1, fail2

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'read_gridded_aqm: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

   call read_netcdf_var (infile, varname, diag, indata, status)

! Read errors are soft errors.
! On read error, return with fail status from lower level.

   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata)			! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )		! omit vestigial LAY dimension

   expect2    = shape (vdata)			! expected dimensions
   expect2(3) = expect2(3) + 1			! with 1 extra hour (MET)

   fail1 = (any (dims_in3 /= shape (vdata)))
   fail2 = (any (dims_in3 /= expect2))

   if (fail1 .and. fail2) then
      print *, '*** read_gridded_aqm: Incorrect var dimensions in file.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = ' // trim (varname)
      fmt1 = '(a,3(1x,i0),a,3(1x,i0),a)'
      print fmt1, ' *** Expected 3-D dimensions     = (', shape (vdata), &
         ') or (', expect2, ')'
      print fmt1, ' *** Current file var dimensions = (', dims_in3(:), ')'
      print *, '*** Fundamental error.  Abort.'
      call exit (1)
   end if

! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours = size (vdata, 3)
   vdata  = real (indata(:,:,1,1:nhours))

! Diagnostics, if requested.

   if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (vdata), ', ', &
         maxval (vdata)
   end if

   if (diag >= 3) print *, 'read_gridded_aqm: Return.'

end subroutine read_gridded_aqm

end module read__gridded_aqm
