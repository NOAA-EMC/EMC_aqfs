!23456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-

        module setup_module

        use data_module

        implicit none

        contains

! ----------------------------------------------------------------------------
        subroutine setup (domain_info, ftype, sdate, stime, timeinc, nrec,&
                          fname1, fname2, mysdate, mystime, mynsteps, mytsteps,&
                          mysrec, myerec, ounit, spc, dinfo, vp, window, timeind)

        implicit none

        include 'PARMS3.EXT'           ! I/O parameters definitions.
        include 'IODECL3.EXT'          ! I/O definitions and declarations.
        include 'FDESC3.EXT'
        include 'STATE3.EXT'

        character (len = 16), intent(in) :: fname1, fname2
        type(domain_info_rec), intent(inout) :: domain_info
        integer, intent(inout) :: ftype
        integer, intent(out) :: sdate, stime, timeinc, nrec
        integer, intent(in) :: mysdate, mystime, mynsteps, mytsteps,&
                               mysrec, myerec, ounit
        logical, intent(in) :: spc, dinfo, vp, window, timeind

        integer :: perim
        integer :: i, k, stat
        integer :: tstepid
        character (len = 16) :: tstep
        character (len = 256) :: eqname2

        character (len=16), allocatable :: lunit(:)
        character (len=80), allocatable :: lvdesc(:)
        integer, allocatable :: lvtype(:)

        integer, external :: index1

        call nameval (fname2, eqname2)

        if ( .not. open3( fname1, fsread3, 'setup' ) ) then
           write(6,*)
           write(6,*) ' could not open file:', fname1
           write(6,*)
           stop
        end if
        if ( .not. desc3 (fname1) ) then
           write(6,*)
           write(6,*) ' could not get file description:', fname1
           write(6,*)
           stop
        end if

        ftype = ftype3d

        if (ftype .eq. 2) then   ! boundary file
           perim = nthik3d * 2 * ( ncols3d + nrows3d + 2 * nthik3d)
           domain_info%gnrows = perim
           domain_info%gncols = -1
           domain_info%ncols = ncols3d
           domain_info%nrows = nrows3d
        else
           domain_info%ncols = ncols3d
           domain_info%gncols = ncols3d
           domain_info%nrows = nrows3d
           domain_info%gnrows = nrows3d
        end if

        domain_info%nlays = nlays3d
        domain_info%gnlays = nlays3d
        domain_info%nspcs = nvars3d
        sdate = sdate3d
        stime = stime3d
        timeinc = tstep3d
        domain_info%alp   = p_alp3d
        domain_info%bet   = p_bet3d
        domain_info%gam   = p_gam3d
        domain_info%xcent = xcent3d
        domain_info%ycent = ycent3d
        domain_info%xorig = xorig3d
        domain_info%yorig = yorig3d
        domain_info%xcell = xcell3d
        domain_info%ycell = ycell3d

        if (timeinc .eq. 0) then
           timeinc = 1
        else if (mytsteps .gt. 0) then
           timeinc = mytsteps
        end if

        nrec = mxrec3d

        if (ftype .eq. 6) then
           goto 99
        end if

        domain_info%spcname(1:nvars3d) = vname3d(1:nvars3d)
 
 99     continue

        if (dinfo) then
           call display_info (ftype, domain_info, sdate, stime, nrec)
        else
	print *, ' ==d== ', mynsteps, mytsteps, mysrec
           if ((mynsteps .ne. -99) .or. (mytsteps .ne. -99)) then   ! for -t case
              call init_my_set (mysdate, mystime, mynsteps, mytsteps, tstep3d,& 
                                timeinc, sdate, stime, nrec)
           else if (mysrec .ne. -99) then                           ! for -r case
              call init_my_set2 (mysrec, myerec, sdate, stime, timeinc, nrec)
           end if

           if (domain_info%slay .ne. -1) then
              domain_info%nlays = domain_info%elay - domain_info%slay + 1
           else
              domain_info%slay = 1
              domain_info%elay = domain_info%nlays
           end if

           if (spc) then
              call ext_subset_list (domain_info%spcname, domain_info%nspcs)
           end if

           if (ounit .eq. 0) then

              if ( .not. open3( fname1, fsread3, 'setup' ) ) then
                 write(6,*)
                 write(6,*) ' could not open file:', fname1
                 write(6,*)
              end if
              if ( .not. desc3 (fname1) ) then
                 write(6,*)
                 write(6,*) ' could not get file description:', fname1
                 write(6,*)
              end if

              allocate (lunit(domain_info%nspcs), stat=stat)
              allocate (lvdesc(domain_info%nspcs), stat=stat)
              allocate (lvtype(domain_info%nspcs), stat=stat)

              do i = 1, domain_info%nspcs
                 k = index1 (domain_info%spcname(i), nvars3d, vname3d)
                 lunit(i)  = units3d(k)
                 lvdesc(i) = vdesc3d(k)
                 lvtype(i) = vtype3d(k)
              end do

              units3d(1:domain_info%nspcs) = lunit
              vdesc3d(1:domain_info%nspcs) = lvdesc
              vtype3d(1:domain_info%nspcs) = lvtype

              deallocate (lunit, lvdesc, lvtype)

              do i = 1, domain_info%nspcs
                 vname3d(i) = domain_info%spcname(i)
              end do

              nvars3d = domain_info%nspcs
              sdate3d = sdate
              stime3d = stime

              if (window) then
                 ncols3d = domain_info%ecol - domain_info%scol + 1
                 domain_info%ncols = ncols3d
                 nrows3d = domain_info%erow - domain_info%srow + 1
                 domain_info%nrows = nrows3d
                 xorig3d = xorig3d + (domain_info%scol - 1) * xcell3d
                 yorig3d = yorig3d + (domain_info%srow - 1) * ycell3d
              end if

              if (domain_info%slay .ne. -1) then
                 do i = domain_info%slay, domain_info%elay+1
                    vglvs3d(i-domain_info%slay+1) = vglvs3d(i)
                 end do
              end if

              nlays3d = domain_info%nlays

              if (.not. vp) then
                 if (timeind) then
                    tstep3d = 0
                 else if (mytsteps .gt. 0) then
                    tstep3d = mytsteps
                 end if
                 if ( .not. open3( fname2, fsnew3, 'setup' ) ) then
                    write(6,*)
                    write(6,*) ' could not open file:', fname2
                    write(6,*)
                 end if
              end if
           else
              open (unit = ounit, file = eqname2, status = 'unknown')
           end if
        end if

        end subroutine setup

! ----------------------------------------------------------------------------
        subroutine display_info (ftype, domain_info, sdate, stime, nrec)

        type(domain_info_rec), intent(in) :: domain_info
        integer, intent(in) :: ftype, sdate, stime, nrec

        integer :: i, j

        print *, ' '
        print *, ' Display file information:'
        print *, ' '
        if (ftype .eq. 2) then
           print *, 'nbndy   =', domain_info%gnrows
           print *, 'nrow    =', domain_info%nrows
           print *, 'ncol    =', domain_info%ncols
        else
           print *, 'nrow    =', domain_info%gnrows
           print *, 'ncol    =', domain_info%gncols
        end if 
        print *, 'nlay    =', domain_info%gnlays
        print *, 'nspc    =', domain_info%nspcs
        print *, 'sdate   =', sdate
        print *, 'stime   =', stime
        print *, 'nsteps  =', nrec
        do i = 1, domain_info%nspcs, 5
           if (i .eq. 1) then
              write (6, 10) ' spcname = ', domain_info%spcname(i:i+4)
 10           format (a11, 5a16)
           else
              j = min (i+4, domain_info%nspcs)
              write (6, 10) '           ', domain_info%spcname(i:j)
           end if
        end do
        print *, ' '

        end subroutine display_info

! ----------------------------------------------------------------------------
        subroutine init_my_set (mysdate, mystime, mynsteps, mytsteps, tsteps,& 
                                timeinc, sdate, stime, nrec)

        integer, intent(in)  :: mysdate, mystime, mynsteps, mytsteps, tsteps, timeinc
        integer, intent(inout) :: sdate, stime
        integer, intent(inout) :: nrec

        integer :: loc_sdate, loc_stime, loc_count, loc_tsteps
        logical :: found

        if ((mysdate .lt. sdate) .or.&
            ((mysdate .eq. sdate) .and. (mystime .lt. stime))) then
           print *, ' '
           print *, ' File sdate: ', sdate
           print *, ' File stime: ', stime
           print *, ' '
           print *, ' Invalid date and/or time'
           print *, ' Program terminates'
           print *, ' '
           stop
        else
           found = .false.
           loc_sdate = sdate
           loc_stime = stime
           loc_count = 0
           do while (.not. found)
              loc_count = loc_count + 1
              if ((loc_sdate .eq. mysdate) .and. (loc_stime .eq. mystime)) then
                 found = .true.
              else
                 call nextime (loc_sdate, loc_stime, tsteps)
              end if
           end do

           if (mynsteps .eq. -1) then
              nrec = (nrec - loc_count + 1) / (timeinc / tsteps)
           else 
              nrec = min(((nrec - loc_count + 1) / (timeinc / tsteps)), mynsteps)
           end if

           sdate = mysdate
           stime = mystime
        end if

        end subroutine init_my_set

! ----------------------------------------------------------------------------
        subroutine init_my_set2 (mysrec, myerec, sdate, stime, timeinc, nrec)

        integer, intent(in)  :: mysrec, myerec, timeinc
        integer, intent(inout) :: sdate, stime
        integer, intent(inout) :: nrec

        integer :: i

        if ((myerec .lt. mysrec) .or. &
            (mysrec .lt. 0) .or. (myerec .gt. nrec)) then
           print *, ' '
           print *, ' File sdate: ', sdate
           print *, ' File stime: ', stime
           print *, ' '
           print *, ' Invalid date and/or time'
           print *, ' Program terminates'
           print *, ' '
           stop
        else
           do i = 1, mysrec - 1
              call nextime (sdate, stime, timeinc)
           end do
           nrec = myerec - mysrec + 1
        end if

        end subroutine init_my_set2

! ----------------------------------------------------------------------------
        subroutine ext_subset_list (spcname, nspcs)

        character (len = 16), intent(inout) :: spcname(mxvars3)
        integer, intent(inout) :: nspcs

        character (len = mxvars3*16) :: list
        integer :: tnspcs
        logical :: stop
        integer :: m, n
        character (len = 16) :: tspcname(200), buf
        logical :: found

        call nameval ('spc_list', list)
        tnspcs = 0
        m = len(trim(list))
        n = 0
        do while (n .lt. m)
           read (list(1+n:m), *) buf
           tnspcs = tnspcs + 1
           tspcname(tnspcs) = buf
           n = len(trim(buf))+1+n
        end do

        stop = .false.
        m = 0
        do while ((.not. stop) .and. (m .lt. tnspcs))
           m = m + 1
           buf = tspcname(m)
           found = .false.
           n = 0
           do while ((.not. found) .and. (n .lt. nspcs))
              n = n + 1
              if (trim(buf) .eq. trim(spcname(n))) then
                 found = .true.
              end if
           end do
           if (.not. found) then
              print *, ' '
              print *, ' Invalid species: ', buf
              print *, ' Program terminates' 
              print *, ' '
              stop 
           end if
        end do
        do m = 1, tnspcs
           spcname(m) = tspcname(m)
        end do
        nspcs = tnspcs

        end subroutine ext_subset_list

        end module setup_module
