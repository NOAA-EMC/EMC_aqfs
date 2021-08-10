!------------------------------------------------------------------------------
!
! read_config_file_interp.f90 -- Config file reader for interpolate.f90.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-may-01	read_config_file.f90:
!		Original version.  By Dave Allured.
! 2016-feb-10	Update for get_param_module.
!
! 2019-may-17	read_config_file_interp.f90:
!		Change to specific subroutine name.
!		Change config file format to support derived variables.
!		Add optional derived variables to var table, using keywords.
!		Add save/nosave column to var table.
!
! Notes:
!
! The configuration file is a simple text file containing file
! paths and specification tables for data variables to be
! processed.
!
! The configuration file contains comments, and is self-
! documenting.  See a typical interpolator configuration file
! for more details.
!
!------------------------------------------------------------------------------

module read__config_file_interp
contains

subroutine read_config_file_interp (config_file, station_file, &
      grid_coord_file, interp_file_template, vi_wind_direction, &
      vi_wind_speed, varnames, reader_codes, infile_templates, var_save)

   use get_param_module
   use read__table_lines
   use stdlit
   use string_utils
   implicit none

   character(*), intent(in ) :: config_file	! name of config file to read

   character(*), intent(out) :: station_file
   character(*), intent(out) :: grid_coord_file
   character(*), intent(out) :: interp_file_template

   integer,      intent(out) :: vi_wind_direction
   integer,      intent(out) :: vi_wind_speed

   character(*), intent(out), allocatable :: varnames(:)
   character(*), intent(out), allocatable :: reader_codes(:)
   character(*), intent(out), allocatable :: infile_templates(:)
   logical,      intent(out), allocatable :: var_save(:)

   integer get_free_unit		! function def.

! Local variables.

   integer, parameter :: max_table_size = 100	! max number of lines in any
   						! single table in config file;
						! at least one more than needed

   character(200) lines(max_table_size)		! line buffer for maximal table

   character(200) line, err_mes
   character(200) header_expected

   character(len(varnames))     in_varname
   character(len(reader_codes)) prefix

   character(50), allocatable :: file_type_names(:)
   character(50), allocatable :: var_file_types(:)
   character(50), allocatable :: save_strs(:)

   character(len(reader_codes)),     allocatable :: type_reader_codes(:)
   character(len(infile_templates)), allocatable :: type_templates(:)

   integer i, j, k, vi, ios, status

   integer cf				! unit number for control file
   integer lnum				! line number within control file

   integer n_file_types			! size of file type table
   integer nvars			! size of var table

   logical found

! Open config file for input.

   print *
   print *, 'Read configuration file.'
   print *, '  File = ' // trim (config_file)

   cf = get_free_unit ()			! get unit # for control file

   open (cf, file=config_file, status='old', action='read', iostat=ios, &
      iomsg=err_mes)

   lnum = 0					! init line counter

   if (ios /= 0) then
      print '(a,i0,a)', '*** Config file error no. ', ios, '.  Abort.'
      print '(a)',      '*** Fortran error: ' // trim (err_mes)
      call exit (1)
   end if

!-----------------------------------------------------------
! Read initial specification lines, in the order listed.
!-----------------------------------------------------------

! Item labels are case sensitive.
! Helper routines skip over comment lines and blank lines.

read_file: &
   do					! block for escape handling only

      call get_param_string ('station file', station_file, cf, status, lnum, &
         nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('grid coordinate file', grid_coord_file, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('interp file template', interp_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read file table.
!-----------------------------------------------------------

! Column 1 = File type name.
! Column 2 = Read subroutine selector.
! Column 3 = Filename template for gridded input files, including paths.

! First read table as raw lines of text.  Get line count.

      print *, '  Read file table:'
      header_expected = 'file table:'
      call read_table_lines (cf, header_expected, lines, n_file_types, lnum)

! Parse table lines.  Free format, space delimited.
! Only read the first three columns.  Anything else is comments.

! Can't use list-directed formatting (*) for item 3.  Path items
! may contain active characters such as slash, which break
! list-directed formatting.

! Instead, use this cheap parser, which delimits purely on blanks.
! This also handles empty fields correctly, unlike list-directed.

      allocate (file_type_names(n_file_types))
      allocate (type_reader_codes(n_file_types))
      allocate (type_templates(n_file_types))

      do i = 1, n_file_types
         line = adjustl (lines(i))	  ! must ensure left justified to start
         print '(5x,a)', trim (line)	  ! progress display

         j = index (line // ' ', ' ')	  ! first blank after item 1
         k = max (j-1, 1)		  ! end of item, guard against empty
         file_type_names(i) = line(1:k)	  ! get item 1
         j = min (j, len(line)-1)	  ! guard against excessively long item
         line = adjustl (line(j+1:))	  ! step past item 1

         j = index (line, ' ')		  ! first blank after item 2
         k = max (j-1, 1)		  ! end of item, guard against empty
         type_reader_codes(i) = line(1:k) ! get item 2
         line = adjustl (line(j+1:))	  ! step past item 2

         j = index (line, ' ')		  ! first blank after item 3
         k = max (j-1, 1)		  ! end of item, guard against empty
         type_templates(i) = line(1:k)	  ! get item 3
      end do

!-----------------------------------------------------------
! Read var table.
!-----------------------------------------------------------

! Column 1  = Actual var name within input file.  Case sensitive.
! Column 2  = File type name, must match file type above, col. 1 of file table.
!             Or else one of the special "derived" keywords.
! Column 3  = "save" to write to interpolated output file; normal analog vars.
!             "nosave" to not write; vars only for derived var calculations.
! Column 4+ = Comments.

! First read table as raw lines of text.  Get line count.

      print *, '  Read var table:'
      header_expected = 'var table:'
      call read_table_lines (cf, header_expected, lines, nvars, lnum)

! Parse table lines.  Free format, space delimited.
! Only read the first three columns.  Anything else is comments.

      allocate (varnames(nvars))
      allocate (var_file_types(nvars))
      allocate (save_strs(nvars))

      do vi = 1, nvars
         line = adjustl (lines(vi))	  ! must ensure left justified to start
         print '(5x,a)', trim (line)	  ! progress display

         j = index (line // ' ', ' ')	  ! first blank after item 1
         k = max (j-1, 1)		  ! end of item, guard against empty
         varnames(vi) = line(1:k)	  ! get item 1
         j = min (j, len(line)-1)	  ! guard against excessively long item
         line = adjustl (line(j+1:))	  ! step past item 1

         j = index (line, ' ')		  ! first blank after item 2
         k = max (j-1, 1)		  ! end of item, guard against empty
         var_file_types(vi) = line(1:k)	  ! get item 2
         line = adjustl (line(j+1:))	  ! step past item 2

         j = index (line, ' ')		  ! first blank after item 3
         k = max (j-1, 1)		  ! end of item, guard against empty
         save_strs(vi) = line(1:k)	  ! get item 3
      end do

!-----------------------------------------------------------
! Correlate the var file types with the file table.
!-----------------------------------------------------------

! Purpose:  This section matches file type codes in the var table,
! with file types listed in the file table.

      allocate (reader_codes(nvars))
      allocate (infile_templates(nvars))

var_loop: &
      do vi = 1, nvars

! Look up file type for current variable in the file table.

         do j = 1, n_file_types
            if (var_file_types(vi) == file_type_names(j)) then
               reader_codes(vi)     = type_reader_codes(j)
               infile_templates(vi) = type_templates(j)
               cycle var_loop
            end if
         end do

! Check for special keyword in place of normal file type.  Check prefix only.

         prefix = var_file_types(vi)(1:7)
         call lowercase (prefix)	! case insensitive for derived keywords

         if (prefix == 'derived') then
            reader_codes(vi)     = var_file_types(vi)	! return full keyword
            infile_templates(vi) = "none"
            cycle var_loop
         end if

! File type for this var is not recognized.  Abort.

         print *, '*** File type "' // trim (var_file_types(vi)) &
            // '" is not listed in the file table, or derived keyword.'
         print *, '*** Cross referenced from "' // trim (varnames(vi)) &
            // '" in the var table.'
         status = fatal
         exit read_file

      end do var_loop

!-----------------------------------------------------------
! Translate the save/nosave column in the var table.
!-----------------------------------------------------------

! Purpose:  Validate save/nosave keywords, and convert to simple true/false.

      allocate (var_save(nvars))

      do vi = 1, nvars
         call lowercase (save_strs(vi))		   ! allow case insensitive
         var_save(vi) = (save_strs(vi) == 'save')  ! convert strs to true/false

         if ( (.not. var_save(vi)) .and. (save_strs(vi) /= 'nosave') ) then
            print *, '*** Keyword error for variable "' &
               // trim (varnames(vi)) // '" in var table.'
            print *, '*** Column 3 must be "save" or "nosave".'
            status = fatal
            exit read_file
         end if
      end do

!-----------------------------------------------------------
! Read supplemental config lines, in the order listed.
!-----------------------------------------------------------

! Get "input wind direction" var name, and find in var table.

      call get_param_string ('input wind direction', in_varname, cf, status, &
         lnum, nonblank)
      if (status /= normal) exit read_file

      if (in_varname == "none") then		! check for keyword
         vi_wind_direction = 0			! 0 = input var not specified
         found = .true.
      else
         do vi = 1, nvars			! look up in var table
            vi_wind_direction = vi
            found = (varnames(vi) == in_varname)
            if (found) exit
         end do
      end if

      if (.not. found) then
         print *, '*** Variable "' // trim (in_varname) &
            // '" is not found in the var table above.'
         print *, '*** This parameter must be listed in the var table,' &
            // ' or else the keyword "none".'
         status = fatal
         exit read_file
      end if

! Get "input wind speed" var name, and find in var table.

      call get_param_string ('input wind speed', in_varname, cf, status, &
         lnum, nonblank)
      if (status /= normal) exit read_file

      if (in_varname == "none") then		! check for keyword
         vi_wind_speed = 0			! 0 = input var not specified
         found = .true.
      else
         do vi = 1, nvars			! look up in var table
            vi_wind_speed = vi
            found = (varnames(vi) == in_varname)
            if (found) exit
         end do
      end if

      if (.not. found) then
         print *, '*** Variable "' // trim (in_varname) &
            // '" is not found in the var table above.'
         print *, '*** This parameter must be listed in the var table,' &
            // ' or else the keyword "none".'
         status = fatal
         exit read_file
      end if

!-----------------------------------------------------------
! End read_file block, and handle block aborts.
!-----------------------------------------------------------

      exit				! normal exit for single pass block
   end do read_file

! Catch aborts from anywhere in read_file block.
! Ensure "status" is always set appropriately before coming here.

   if (status /= normal) then
      print '(a,i0)', '*** read_config_file: Abort, config file line' &
         // ' number = ', lnum
      call exit (1)
   end if

! All done, output arguments are already assigned.  Return to caller.

end subroutine read_config_file_interp
end module read__config_file_interp
