!23456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-

        module extract_module

        use data_module

        implicit none

        interface extract
          module procedure extract, extract_vp, extract_b, extract_b_vp
        end interface

        contains

! -----------------------------------------------------------------------------

        subroutine extract (domain_info, sdate, stime, timeinc,&
                            nrec, fname1, fname2, ounit, window)

        implicit none

        character (len=16), intent(in) :: fname1, fname2

        type(domain_info_rec), intent(in) :: domain_info
        integer, intent(in) :: sdate, stime
        integer, intent(in) :: timeinc, nrec, ounit
        logical, intent(in) :: window

        include "PARMS3.EXT"           ! I/O parameters definitions.
        include "IODECL3.EXT"          ! I/O definitions and declarations.

        integer :: jdate               ! current model date , coded yyyyddd.
        integer :: jtime               ! current model time , coded hhmmss.

        integer :: istep, nsteps, s_step
        integer :: i, l, r, c, cq, cr, lc, stat

!       real :: data(domain_info%ncols, domain_info%nrows, domain_info%nlays)

!.......   Start up the I/O API. Note that INIT3() returns the FORTRAN
!.......   unit number for the program's log.

        if (.not. allocated(idata)) then
           allocate(idata(domain_info%gncols, domain_info%gnrows,&
                          domain_info%gnlays), stat=stat) 
           allocate(odata(domain_info%ncols, domain_info%nrows, &
                          domain_info%nlays), stat=stat)
        end if

        nsteps = nrec
        jdate = sdate
        jtime = stime

        do istep = 1, nsteps
           do i = 1, domain_info%nspcs
            if ( .not. read3( fname1, domain_info%spcname(i), allays3,&
                  jdate, jtime, idata ) ) then
                 print *, 'could not read from ', fname1
                 stop
              end if

            if (window) then
      odata(1:domain_info%ncols,1:domain_info%nrows,1:domain_info%nlays)&
                  = idata(domain_info%scol:domain_info%ecol,&
                          domain_info%srow:domain_info%erow,&
                          domain_info%slay:domain_info%elay)
              else
                 odata(:,:,1:domain_info%nlays) = &
                 idata(:,:,domain_info%slay:domain_info%elay)
              end if

              if (ounit .eq. 0) then
               if ( .not. write3(fname2, domain_info%spcname(i), jdate,&
                                  jtime, odata)) then
                    print *, 'could not write to ', fname2
                    stop
                 end if
              else
                 write (ounit, *) ' '
                 write (ounit, 20) 'species=', domain_info%spcname(i),&
                       ' jdate = ', jdate, ' jtime = ', jtime
 20              format (a11, a16, a9, i7, a9, i6)
                 do l = 1, domain_info%nlays
                    write (ounit, 22) '   layer = ', l
 22                 format (a11, i3)
                    write (ounit, *) ' '
                    do r = 1, domain_info%nrows
                       cq = domain_info%ncols / 5
                       cr = mod(domain_info%ncols,5)
                       do c = 1, cq
                         write (ounit, 24)& 
                               '(',(c-1)*5+1,',',r,',',l,')',&
                               (odata(lc,r,l), lc=(c-1)*5+1,c*5)
 24                       format (a, i3, a, i3, a, i3, 1a, 5f13.6)
                       end do
                       if (cr .gt. 0) then
                          write (ounit, 24) & 
                               '(',(c-1)*5+1,',',r,',',l,')',&
                         (odata(lc,r,l), lc=(c-1)*5+1,domain_info%ncols)
                       end if
                       write (ounit, 24) 

!                      do c = 1, ncols
!                         write (ounit, 24) 
!    &                          '(',c,',',r,',',l,')',odata(c,r,l)
!24                       format (5x, a, i3, a, i3, a, i3, 1a, f13.6)
!                      end do

                    end do
                 end do
              end if
           end do
           if (istep < nrec) then
              call nextime (jdate, jtime, timeinc)
           end if
        end do

        return
        end subroutine extract

! -----------------------------------------------------------------------------

       subroutine extract_vp (domain_info, sdate, stime, timeinc, nrec,& 
                              fname1)

        implicit none

        character (len=16), intent(in) :: fname1

        type(domain_info_rec), intent(in) :: domain_info
        integer, intent(in) :: sdate, stime
        integer, intent(in) :: timeinc, nrec

        include "PARMS3.EXT"            ! I/O parameters definitions.
        include "IODECL3.EXT"           ! I/O definitions and declarations.

        integer :: jdate                ! current model date , coded yyyyddd.
        integer :: jtime                ! current model time , coded hhmmss.

        integer :: istep, nsteps, s_step, ounit
        integer :: i, l, r, c, cq, cr, lc

        real :: data(domain_info%gncols, domain_info%gnrows,& 
                     domain_info%gnlays)

        ounit = 10

        open (unit = ounit, file='vp.dat', status='unknown')

        nsteps = nrec
        jdate = sdate
        jtime = stime

        do istep = 1, nsteps
           do i = 1, domain_info%nspcs
              if ( .not. read3( fname1, domain_info%spcname(i), &
                  allays3,jdate, jtime, data ) ) then
                 print *, 'could not read from ', fname1
                 stop
              end if

              write (ounit, *) ' '
              write (ounit, 20) 'species=', domain_info%spcname(i),&
                    ' jdate = ', jdate, ' jtime = ', jtime
 20           format (a11, a16, a9, i7, a9, i6)

              do r = domain_info%srow, domain_info%erow
                 do c = domain_info%scol, domain_info%ecol
                    write (ounit, *) ' '
                    write (ounit, 21) ' col = ', c, ' row = ', r
 21                 format (a7, i3, a7, i3)
                    do l = 1, domain_info%nlays
                       write (ounit, 22) '   layer = ', l, data(c,r,l)
 22                    format (a11, i3, f15.4)
                    end do
                 end do
              end do
           end do
           call nextime (jdate, jtime, timeinc)
        end do

        close (ounit)

        return
        end subroutine extract_vp

! --------------------------------------------------------------------------

        subroutine extract_b (domain_info, sdate, stime, timeinc,& 
                              nrec, fname1, fname2, ounit, window, flag)

        implicit none

        character (len=16), intent(in) :: fname1, fname2

        type(domain_info_rec), intent(in) :: domain_info
        integer, intent(in) :: sdate, stime
        integer, intent(in) :: timeinc, nrec, ounit
        logical, intent(in) :: window
        integer, intent(in) :: flag

        include "PARMS3.EXT"           ! I/O parameters definitions.
        include "IODECL3.EXT"          ! I/O definitions and declarations.
        include "FDESC3.EXT"

        integer :: jdate               ! current model date , coded yyyyddd.
        integer :: jtime               ! current model time , coded hhmmss.

        integer :: istep, nsteps, s_step, i, l, obndy_size, stat

!.......   Start up the I/O API. Note that INIT3() returns the FORTRAN
!.......   unit number for the program's log.

        nsteps = nrec
        jdate = sdate
        jtime = stime

        if ( .not. open3( fname2, fsrdwr3, 'extract' ) ) then
           write(6,*)
           write(6,*) ' could not open file:', fname2
           write(6,*)
           stop
        end if

        obndy_size = 2 * nthik3d * ( domain_info%ncols + & 
                     domain_info%nrows + 2 * nthik3d )

        if (.not. allocated(idata)) then
           allocate(idata(domain_info%gnrows, 1,&
                  domain_info%gnlays), stat=stat)
           allocate(odata(obndy_size, 1, domain_info%nlays), stat=stat)
        end if

        do istep = 1, nsteps
           do i = 1, domain_info%nspcs
              do l = domain_info%slay, domain_info%elay
                 if ( .not. read3( fname1, domain_info%spcname(i), l, &
                    jdate, jtime, idata(:,:,l-domain_info%slay+1)) ) then
                    print *, 'could not read from ', fname1
                    stop
                 end if
              end do

         odata(:,:,1:domain_info%nlays) = idata(:,:,1:domain_info%nlays)

        if ( .not. write3(fname2, domain_info%spcname(i), jdate, & 
                     jtime,odata)) then
                 print *, 'could not write to ', fname2
                 stop
              end if
           end do
           call nextime (jdate, jtime, timeinc)
        end do

        return
        end subroutine extract_b

! --------------------------------------------------------------------------

        subroutine extract_b_vp (domain_info, sdate, stime, timeinc, &
                        nrec,fname1, flag)

        implicit none

        character (len=16), intent(in) :: fname1

        type(domain_info_rec), intent(in) :: domain_info
        integer, intent(in) :: sdate, stime
        integer, intent(in) :: timeinc, nrec
        integer, intent(in) :: flag

        include "PARMS3.EXT"           ! I/O parameters definitions.
        include "IODECL3.EXT"          ! I/O definitions and declarations.
        include "FDESC3.EXT"

        integer :: jdate               ! current model date , coded yyyyddd.
        integer :: jtime               ! current model time , coded hhmmss.

        integer :: istep, nsteps, s, i, level, index, stat, size, & 
                      start,  end, last, ounit
        character (len = 200) :: myfmt
        real :: boundary(0:max(domain_info%ncols, domain_info%nrows))
        real, allocatable :: data(:)
        integer, allocatable :: num(:)
        logical :: invalid_boundary_def = .false.

! check boundary input is correct
        if ((domain_info%srow .lt. 0) .or. & 
           (domain_info%erow .gt. domain_info%nrows+1) .or.& 
           (domain_info%srow .gt. domain_info%erow) .or.&
           (domain_info%scol .lt. 0) .or. &
           (domain_info%ecol .gt. domain_info%ncols+1) .or.& 
           (domain_info%scol .gt. domain_info%ecol)) then
            invalid_boundary_def = .true.
        else if (domain_info%scol .eq. domain_info%ecol) then
            if ((domain_info%scol .eq. 0) .or. (domain_info%ecol .eq.& 
                   domain_info%ncols+1)) then
            else
               invalid_boundary_def = .true.
            end if
        else if (domain_info%srow .eq. domain_info%erow) then
            if ((domain_info%srow .eq. 0) .or. (domain_info%erow .eq. & 
                   domain_info%nrows+1)) then
            else
               invalid_boundary_def = .true.
            end if
        end if
        if (invalid_boundary_def) then
           print *, ' '
           print *, ' Error: Incorrect boundary definition'
           print *, ' '
           stop
        end if

        allocate(data(max(domain_info%ncols, domain_info%nrows)+1),&
                    stat=stat)
        allocate(num(max(domain_info%ncols, domain_info%nrows)+1), &
                    stat=stat)

        ounit = 10

        open (unit = ounit, file='vp.dat', status='unknown')

        nsteps = nrec
        jdate = sdate
        jtime = stime

        if (.not. allocated(idata)) then
           allocate(idata(domain_info%gnrows, 1, domain_info%gnlays), &
               stat=stat)
        end if

        if (domain_info%scol .eq. domain_info%ecol) then
           size = domain_info%erow - domain_info%srow + 1
           index = 0
           do i = domain_info%srow, domain_info%erow
              index = index + 1
              num(index) = i
           end do
        else
           size = domain_info%ecol - domain_info%scol + 1
           index = 0
           do i = domain_info%scol, domain_info%ecol
              index = index + 1
              num(index) = i
           end do
        end if

        do istep = 1, nsteps
           do s = 1, domain_info%nspcs

            write (ounit, *) ' '
            write (ounit, 20) ' species = ', domain_info%spcname(s), &
                    ' jdate = ', jdate, ' jtime = ', jtime
 20           format (a11, a16, a9, i7, a9, i6)
              write (ounit, *) ' '

              write (myfmt, 25) '(a5, ', (12 * size - 5) / 2, 'x, a5)'
 25           format (a5, i5.5, a6)

              if (domain_info%scol .eq. domain_info%ecol) then
                 write (ounit, myfmt) 'lvl  ', 'row #'
              else
                 write (ounit, myfmt) 'lvl  ', 'col #'
              end if

              write (myfmt, 30) '(9x, ', size, '(i3.3, 9x))'
 30           format (a5, i3.3, a11)
              write (ounit, myfmt) (num(i), i = 1, size)

              write (myfmt, 30) '(i3, ', size, '(f12.5))'
 35           format (a5, i3.3, a8)

              index = 0
              do level = domain_info%elay, domain_info%slay, -1
                 index = index + 1

               if ( .not. read3( fname1, domain_info%spcname(s), level,&
                                   jdate, jtime, idata(:,:,index) ) ) then
                    print *, 'could not read from ', fname1
                    stop
                 end if

              if (domain_info%scol .eq. domain_info%ecol) then
               if (domain_info%scol .eq. 0) then                ! west
                start = 2 * domain_info%ncols + domain_info%nrows + 4
                end   = start + domain_info%nrows
                last  = start - domain_info%ncols - 1
                boundary(0:domain_info%nrows) = idata(start:end,1,index)
                boundary(domain_info%nrows+1:domain_info%nrows+1) = & 
                     idata(last:last,1,index)
              data(1:size) = boundary(domain_info%srow:domain_info%erow)
            else if (domain_info%scol .eq. domain_info%ncols+1) then    ! east
                   start = domain_info%ncols + 2
                   end   = start + domain_info%nrows
                   last  = start - 1
                   boundary(1:domain_info%nrows+1) = idata(start:end,1,index)
                   boundary(0:0) = idata(last:last,1,index)
              data(1:size) = boundary(domain_info%srow:domain_info%erow) 
            end if
           else if (domain_info%srow .eq. domain_info%erow) then
           if (domain_info%srow .eq. domain_info%nrows+1) then         ! north
               start = domain_info%ncols + domain_info%nrows + 3
               end   = start + domain_info%ncols
               last  = start - 1
               boundary(0:domain_info%ncols) = idata(start:end,1,index)
              boundary(domain_info%ncols+1:domain_info%ncols+1) = idata(last:last,1,index)
             data(1:size) = boundary(domain_info%scol:domain_info%ecol) 
           else if (domain_info%srow .eq. 0) then                      ! south
               start = 1
               end   = start + domain_info%ncols
               last  = domain_info%gnrows - domain_info%nrows 
               boundary(1:domain_info%ncols+1) = idata(start:end,1,index)
               boundary(0:0) = idata(last:last,1,index)
              data(1:size) = boundary(domain_info%scol:domain_info%ecol) 
           end if
          end if

          write (ounit, myfmt) level, (data(i), i = 1, size)

         end do
      
       end do
         call nextime (jdate, jtime, timeinc)
      end do

        deallocate(data, num)

        return
        end subroutine extract_b_vp

        end module extract_module
