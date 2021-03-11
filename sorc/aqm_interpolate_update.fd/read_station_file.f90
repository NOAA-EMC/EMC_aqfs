!------------------------------------------------------------------------------
!
! read_station_file.f90 -- Read list of site coordinates for bias correction.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-apr-24	Original version.  By Dave Allured.
!
! Input file format:
!
! Site list with coordinates.
! Plain text, three columns, free format.
!
! The first part is a comment header, variable length, ending with
! one line of dashes.
!
! The second part is three columns: Site ID, latitude, longitude.
! Free format, items separated by one or more spaces.
!
!------------------------------------------------------------------------------

module read__station_file
contains

subroutine read_station_file (filename, stn_ids, stn_lats, stn_lons, nsites)

   use config, only : dp
   implicit none

   character(*), intent(in )              :: filename	  ! station file name

   character(*), intent(out), allocatable :: stn_ids(:)	  ! site ID strings
   real(dp),     intent(out), allocatable :: stn_lats(:)  ! site latitudes
   real(dp),     intent(out), allocatable :: stn_lons(:)  ! site longitudes
   integer,      intent(out)              :: nsites	  ! number of sites read

   integer get_free_unit			! function def.

! Local variables.

   character(len(stn_ids)) min_id, max_id
   character header*4

   integer infile, ios, i, lnum, nheaders

! First pass.  Just count the number of actual station lines in the  file.

   print *
   print *, 'read_station_file: Read station list.'
   print *, '  Station file = ' // trim (filename)

   infile = get_free_unit ()			! allocate a fortran unit number
   open (infile, file=filename, status='old', action='read')

   nheaders = 0					! count header lines...
   do
      read (infile, '(a)') header		! read start of header line
      nheaders = nheaders + 1
      if (header == '----') exit		! dashes = end of header
   end do

   nsites = 0					! count date lines
   do
      read (infile, *, iostat=ios)
      if (ios /= 0) exit			! end of file = end of site list
      nsites = nsites + 1
   end do

   print '(a,i0)', '   Number of sites in file      =   ', nsites

! Second pass.  Read site ID's and coordinates into arrays.

   allocate (stn_ids(nsites))			! allocate output arrays
   allocate (stn_lats(nsites), stn_lons(nsites))

   rewind (infile)				! go back to start of file

   do i = 1, nheaders				! skip header lines
      read (infile, '(a)')
   end do

   do i = 1, nsites				! read all station lines
      read (infile, *, iostat=ios) stn_ids(i), stn_lats(i), stn_lons(i)
      						! free format works here
      if (ios /= 0) then
         lnum = nheaders + i
         print *, '*** read_station_file: Read error on line ', lnum
         call exit (1)
      end if
   end do

   close (infile)				! all done, close input file
   						!   and release unit number
! Progress display, show statistics.

   min_id = stn_ids(1)
   max_id = stn_ids(1)

   do i = 2, nsites
      min_id = min (min_id, stn_ids(i))
      max_id = max (max_id, stn_ids(i))
   end do

   print '(4a)', '   Lowest, highest station ID''s =  ', trim (min_id), &
      ', ', trim (max_id)

   print '(2(a,f10.5))', '   Min, max latitudes           = ', &
      minval (stn_lats), ',', maxval (stn_lats)
   print '(2(a,f10.5))', '   Min, max longitudes          = ', &
      minval (stn_lons), ',', maxval (stn_lons)

end subroutine read_station_file

end module read__station_file
