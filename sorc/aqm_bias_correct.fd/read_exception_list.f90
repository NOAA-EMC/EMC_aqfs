!------------------------------------------------------------------------------
!
! read_exception_list.f90 -- Read list of bias thresholds for exception sites.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias correction
! program for CMAQ forecast outputs.
!
! 2014-oct-27	Original version.  By Dave Allured.
!		Adapted from read_station_file.f90 version 2014-apr-24.
! 2014-oct-29	Add another column for separate positive and negative
!		  thresholds.
!		Support missing input file, return nsites = 0.
!
! Input file format:
!
! Site list with coordinates and threshold values.
! Plain text, five columns, free format.
!
! The first part is a comment header, variable length, ending
! with one line of dashes.
!
! The second part is five columns: Site ID, latitude, longitude,
! negative bias threshold value, and positive bias threshold
! value.  Free format, items separated by one or more spaces.
! Threshold values may be integers or real numbers.
!
! The coordinates in columns 2 and 3 will be ignored.  They
! are included only for reference and for consistency with the
! primary site list.  The calling program must associate by
! site ID's only.
!
! CAUTION:  Only the coordinates from the list file will be
! printed to the console log.  If these coordinates are different
! than the ones in the primary data files, there will be no
! warning of a mismatch.  This may be potentially misleading.
!
!------------------------------------------------------------------------------

module read__exception_list
contains

subroutine read_exception_list (filename, diag, site_ids, thresh_low, &
      thresh_high, nsites)

   use config, only : dp
   implicit none

   character(*), intent(in )              :: filename	! input file name
   integer,      intent(in )              :: diag	! verbosity, 0=errs only

   character(*), intent(out), allocatable :: site_ids(:)     ! site ID strings
   real(dp),     intent(out), allocatable :: thresh_low(:)   ! neg. and positive
   real(dp),     intent(out), allocatable :: thresh_high(:)  ! threshold values
   integer,      intent(out)              :: nsites	     ! no. of sites read

   integer get_free_unit			! function def.

! Local variables.

   character header*4, errmes*200
   integer infile, ios, i, lnum, nheaders

   real(dp), allocatable :: lats(:), lons(:)	! dummy coords from list file,
						!   DISPLAYED BUT NOT USED
! Open the site exception file.

   print *
   print *, 'read_exception_list: Read site exception list.'
   print *, '  Input file = ' // trim (filename)

   infile = get_free_unit ()			! allocate a fortran unit number
   open (infile, file=filename, status='old', action='read', iostat=ios, &
      iomsg=errmes)

! Handle missing file gracefully.

   if (ios /= 0) then
      print '(a,i0)', '   *** File not found, fortran I/O status = ', ios
      print *,         '  *** Fortran error:'
      print *,         '  ***   ' // trim (errmes)
      print *,         '  *** No threshold tests, all analogs will be enabled.'
      print *

      nsites = 0			! signal missing file, and empty list
      return				! arrays NOT allocated
   end if

! First pass.  Just count the number of actual site lines in the  file.

   nheaders = 0					! count header lines...
   do
      read (infile, '(a)') header		! read start of header line
      nheaders = nheaders + 1
      if (header(1:4) == '----') exit		! dashes = end of header
   end do

   nsites = 0					! count site lines
   do						! blank lines are NOT allowed
      read (infile, *, iostat=ios)
      if (ios /= 0) exit			! end of file = end of site list
      nsites = nsites + 1
   end do

   print '(a,i0)', '   Number of sites in exception list =   ', nsites

! Second pass.  Read site ID's and threshold values into arrays.

   allocate (site_ids(nsites))			! allocate output arrays
   allocate (thresh_low(nsites), thresh_high(nsites))

   allocate (lats(nsites), lons(nsites))	! allocate dummy arrays

   rewind (infile)				! go back to start of file

   do i = 1, nheaders				! skip header lines
      read (infile, '(a)')
   end do

   do i = 1, nsites				! read all site lines
      read (infile, *, iostat=ios) site_ids(i), lats(i), lons(i), &
         thresh_low(i),thresh_high(i)		! free format works here, but
						!   all columns must be present
      if (ios /= 0) then
         lnum = nheaders + i
         print *, '*** read_exception_list: Read error on line ', lnum
         call exit (1)
      end if
   end do

   close (infile)				! all done, close input file
   						!   and release unit number
! Print exception list for log file.

   if (diag >= 2) then
      print *
      print *, '        Site ID   Latitude  Longitude  Low threshold' &
         // '  High threshold'
      print '(i5, 2x, a, 2f11.4, f12.1, f15.1)', &
         (i, site_ids(i), lats(i), lons(i), thresh_low(i), thresh_high(i), &
         i = 1, nsites)
      print *
   end if

end subroutine read_exception_list

end module read__exception_list
