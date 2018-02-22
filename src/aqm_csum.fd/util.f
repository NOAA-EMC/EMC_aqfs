C ----------------------------------------------------------------------------
        subroutine extract_vlist (formula, n_formula, n_output_spc)

        use get_env_module
        use data_module

        implicit none

        type(formula_type), intent(inout) :: formula(:)
        integer, intent(in)               :: n_formula
        integer, intent(inout)            :: n_output_spc

        character (len=256) :: fname
        logical :: eof, continuation, found_constant
        integer :: loc_n_formula, stat, str_len, i, j, str_char_index, 
     $             nvar, n_seq, n_constant
        integer, parameter :: funit = 10
        character (len=2000) :: buf, loc_buf
        character (len=20) :: tbuf
        character (len=16), allocatable :: buf_seq(:,:)

        call get_env (fname, 'vfile', ' ')

        allocate (buf_seq(1000, n_formula), stat=stat)

        open (unit = funit, file=fname, status='unknown')

! extract all lexical expressions and store them in buf_seq
        eof = .false.
        continuation = .false.
        loc_n_formula = 0
        do while (.not. eof)
           read (funit, 10, iostat=stat) buf
 10        format (a2000)
           if (stat .ne. 0) then
              eof = .true.
           else
              buf = trim(adjustl(buf))
              str_len = len(trim(buf))

              if (str_char_index(buf, str_len, 1, '=') .gt. 0) then
                 loc_n_formula = loc_n_formula + 1
                 continuation = .false.
              else
                 continuation = .true.
                 i = 1
              end if

              if (.not. continuation) then

                 i = str_char_index(buf, str_len, 1, ',')
                 formula(loc_n_formula)%formula_spc_unit = buf(1:i-1)

                 if (formula(loc_n_formula)%formula_spc_unit(1:1) == ' ') then
                    formula(loc_n_formula)%output_spc = .false.
                 else
                    formula(loc_n_formula)%output_spc = .true.
                    n_output_spc = n_output_spc + 1
                 end if

                 j = str_char_index(buf, str_len, i+1, '=')
                 formula(loc_n_formula)%formula_spc_name = trim(adjustl(buf(i+1:j-1)))
                 j = j + 1     ! advanced to the character after '=' sign

                 loc_buf = trim(adjustl(buf(j:str_len)))
                 formula(loc_n_formula)%coef = 1.0
                 nvar = 0
                 buf_seq(:, loc_n_formula) = ''
              else
                 loc_buf = buf
              end if 

              j = 1
              str_len = len(trim(loc_buf))

              do while (j .lt. str_len)
                 call extract (j , str_len, loc_buf, tbuf)
                 nvar = nvar + 1
                 buf_seq(nvar, loc_n_formula) = tbuf
              end do
              formula(loc_n_formula)%n_spc = nvar

           end if
        end do

        do i = 1, n_formula
           formula(i)%group(0) = 0
           formula(i)%file_number = 0
           nvar = 0
           n_seq = 0
           n_constant = 0
           do j = 1,  formula(i)%n_spc
! handle group elements
              if (buf_seq(j,i)(1:1) == ':') then
                 formula(i)%group(0) = formula(i)%group(0) + 1
                 if (mod(formula(i)%group(0), 2) == 1) then
                    formula(i)%group(formula(i)%group(0)) = nvar + 1
                 else
                    formula(i)%group(formula(i)%group(0)) = nvar
                 end if
                 n_seq = n_seq + 1
                 formula(i)%sequence(n_seq) = ':'
! handle regular species
              else if ((buf_seq(j,i)(1:1) .ge. 'A') .and. (buf_seq(j,i)(1:1) .le. 'Z')) then
                 nvar = nvar + 1
                 str_len = len(trim(buf_seq(j,i)))
                 if (buf_seq(j,i)(str_len-1:str_len-1) == ',') then
                    formula(i)%spc_name(nvar) = buf_seq(j,i)(1:str_len-2)
                    read (buf_seq(j,i)(str_len:str_len), *) formula(i)%file_number(nvar)
                 else
                    formula(i)%spc_name(nvar) = buf_seq(j,i)
                    formula(i)%file_number(nvar) = 1
                 end if
                 n_seq = n_seq + 1
                 formula(i)%sequence(n_seq) = 'e'
! handle numbers
              else if ((buf_seq(j,i)(1:1) .ge. '1') .and. (buf_seq(j,i)(1:1) .le. '9')) then
                 if ((buf_seq(j+1,i)(1:1) .ge. 'A') .and. (buf_seq(j+1,i)(1:1) .le. 'Z')) then
                    read (buf_seq(j,i), *) formula(i)%coef(nvar+1)
                 else
                    n_constant = n_constant + 1
                    read (buf_seq(j,i), *) formula(i)%constant(n_constant)
                    n_seq = n_seq + 1
                    formula(i)%sequence(n_seq) = 'c'
                 end if
! handle : and /
              else
                 n_seq = n_seq + 1
                 formula(i)%sequence(n_seq) = buf_seq(j,i)(1:1)
              end if
           end do

           formula(i)%n_spc = nvar
           formula(i)%n_constant = n_constant
           formula(i)%n_seq = n_seq

        end do

!       do i = 1, n_formula
!          write (6, '(a12, 3i5)') ' ==d== nspc ', formula(i)%n_spc, formula(i)%n_seq
!          write (6, '(a12, 10i5)') ' ==d== grp  ', formula(i)%group(0:formula(i)%group(0))
!          write (6, '(a12, 10f7.2)') ' ==d== cons ', formula(i)%constant
!          write (6, '(a12, 100a2)') ' ==d== seq  ', formula(i)%sequence(1:formula(i)%n_seq)
!          do j = 1,  formula(i)%n_spc
!             write (6, '(a12, 2i5, 2x, a16, i5, f5.2)') ' ==d== spc  ', i,j, formula(i)%spc_name(j),
!    $              formula(i)%file_number(j), formula(i)%coef(j)
!          end do
!          write (6, *) ' '
!       end do

        close (funit)

        deallocate (buf_seq)

        end subroutine extract_vlist

C ----------------------------------------------------------------------------
        subroutine extract (pos, str_len, buf, tbuf)

        implicit none

        integer, intent(inout) :: pos
        integer, intent(in)    :: str_len
        character (len = 2000), intent(in) :: buf
        character (len = 20), intent(out)  :: tbuf

        integer :: i, j
        logical :: done, found

        tbuf = ' '
        done = .false.
        i = pos
        j = 0
        do while (.not. done)
           if (i .gt. str_len) then
              done = .true.
           else if (buf(i:i) .eq. ' ') then
              done = .true.
              found = .false.
! proceed to the beginning of next character string
              do while (.not. found)
                 i = i + 1
                 if (buf(i:i) .ne. ' ') then
                    found = .true.
                 end if
              end do
           else
              j = j + 1
              tbuf(j:j) = buf(i:i)
              i = i + 1
           end if
        end do
        pos = i

        end subroutine extract

C ----------------------------------------------------------------------------
        integer function str_char_index (buf, length, starting_pt, delimiter)

        implicit none

        character (len=2000), intent(in) :: buf
        integer, intent(in) :: length, starting_pt
        character (len=1), intent(in) :: delimiter

        integer :: i
        logical :: found

        found = .false.
        i = starting_pt - 1
        do while ((.not. found) .and. (i .lt. length))
          i = i + 1
          if (buf(i:i) .eq. delimiter) then
             found = .true.
          end if
        end do

        if (.not. found) then
           str_char_index = -1
        else
           str_char_index = i
        end if

        end function str_char_index

C ----------------------------------------------------------------------------
        integer function find_variable (vname, vlist, nvlist)

        implicit none

        character (len=16), intent(in) :: vname
        character (len=16), intent(in) :: vlist(:)
        integer, intent(in)            :: nvlist

        integer :: i
        logical :: found

        found = .false.
        i = 0
        do while ((.not. found) .and. (i < nvlist))
          i = i + 1
          if (vname == vlist(i)) then
             found = .true.
          end if
        end do

        if (.not. found) then
           find_variable = -1
        else
           find_variable = i
        end if

        end function find_variable

C ----------------------------------------------------------------------------
        subroutine setup (outfname, formula, n_formula, n_output_spc, nrows,
     &                    ncols, nlays, sdate, stime, timeinc, colsum)

        use get_env_module
        use data_module

        implicit none

        include 'PARMS3.EXT'           ! I/O parameters definitions.
        include 'IODECL3.EXT'          ! I/O definitions and declarations.
        include 'FDESC3.EXT'

        character (len = 16), intent(in) :: outfname
        type(formula_type), intent(in)   :: formula(:)
        integer, intent(in)              :: n_formula, n_output_spc
        logical, intent(in)              :: colsum
        integer, intent(out)             :: nrows, ncols, nlays,
     &                                      sdate, stime, timeinc

        character (len = 16) :: infname
        integer :: v, k, stat, jdate, jtime, nsteps, rm_first_step
        character (len=16), allocatable :: lvname(:), lunit(:) 
        character (len=80), allocatable :: lvdesc(:)
        integer, allocatable :: lvtype(:)
        integer, external :: index1
        logical :: file_update, rm_last_step, surface, keep_first_step

        call get_env (file_update, 'file_update', .false.)
        call get_env (rm_last_step, 'rm_last_step', .false.)
        call get_env (surface, 'surface', .false.)
        call get_env (rm_first_step, 'rm_first_step', 0)
        call get_env (keep_first_step, 'keep_first_step', .false.)

        infname = 'infile01_001'

        allocate (lvname(n_output_spc), stat=stat)
        allocate (lunit(n_output_spc), stat=stat)
        allocate (lvdesc(n_output_spc), stat=stat)
        allocate (lvtype(n_output_spc), stat=stat)

        if (.not. open3(infname, fsread3, 'csum')) then
           print *, ' Error: Could not open ', infname
        end if

        if (.not. desc3(infname)) then
           print *, ' Error: Could not desc3 ', infname
        end if

        ncols = ncols3d
        nrows = nrows3d

        sdate = sdate3d
        stime = stime3d

        timeinc = tstep3d

        if (.not. keep_first_step) then
           do nsteps = 1, rm_first_step
              call nextime (sdate, stime, timeinc)
           end do
        end if

        sdate3d = sdate
        stime3d = stime

        k = 0
        do v = 1, n_formula
           if (formula(v)%output_spc) then
              k = k + 1
              lvname(k) = formula(v)%formula_spc_name
              lunit(k)  = formula(v)%formula_spc_unit
              lvdesc(k) = ' '
              lvtype(k) = m3real
           end if
        end do 

        if (file_update) then
           if (.not. open3(outfname, fsrdwr3, 'csum')) then
              print *, ' Error: Could not open ', outfname
           end if
           if (.not. desc3(outfname)) then
              print *, ' Error: Could not desc3 ', outfname
           end if
           if (nvars3d .ne. n_output_spc) then
              print *, ' Error: Number of variables is mis-match'
              stop
           end if
           do v = 1, n_output_spc
              k = index1(lvname(v), nvars3d, vname3d)
              if (k .le. 0) then
                 print *, ' Error: Variable ', trim(lvname(v)), ' does not exist'
                 stop
              end if
              if (lunit(v) .ne. units3d(k)) then
                 print *, ' Error: Variable ', trim(lvname(v)), ' has a different unit'
                 stop
              end if
           end do
           if (ncols .ne. ncols3d) then
              print *, ' Error: column dimensions are different'
              stop
           end if
           if (nrows .ne. nrows3d) then
              print *, ' Error: row dimensions are different'
              stop
           end if
           jdate = sdate3d
           jtime = stime3d
           if (rm_last_step) then
              nsteps = mxrec3d - 1
           else
              nsteps = mxrec3d
           end if
           do k = 1, nsteps
              call nextime (jdate, jtime, timeinc)
           end do
           if ((jdate .ne. sdate) .or. (jtime .ne. stime)) then
              print *, ' Error: date or time mis-match', jdate, sdate, jtime, stime
              stop
           end if
           nlays = nlays3d
        else
           nvars3d = n_output_spc
           nlays = nlays3d
           if (colsum) then
              nlays = nlays3d
              nlays3d = 1
           else if (surface) then
              nlays3d = 1
              nlays = nlays3d
           end if
           vname3d(1:n_output_spc) = lvname
           units3d(1:n_output_spc) = lunit
           vdesc3d(1:n_output_spc) = lvdesc
           vtype3d(1:n_output_spc) = lvtype
           fdesc3d(1:n_output_spc) = ''

           if (.not. open3(outfname, fsnew3, 'csum')) then
              print *, ' Error: Could not open ', outfname
           end if
        end if

        deallocate (lvname, lunit, lvdesc, lvtype)

        end subroutine setup

C ----------------------------------------------------------------------------
        subroutine compute (outfname, formula, n_formula, nrows, ncols,
     &                      nlays, sdate, stime, timeinc, colsum)

        use data_module
        use get_env_module

        implicit none

        include 'PARMS3.EXT'           ! I/O parameters definitions.
        include 'IODECL3.EXT'          ! I/O definitions and declarations.
        include 'FDESC3.EXT'

        type(formula_type), intent(in)   :: formula(:)
        integer, intent(in) :: n_formula, nrows, ncols, nlays, sdate,
     &                         stime, timeinc
        logical, intent(in) :: colsum
        character (len=16), intent(in) :: outfname

        character (len=16), allocatable :: infname(:)
        integer :: jdate, jtime

        integer r, c, i, j, k, n, istep, nsteps, numfile, stat, 
     &          rm_last_step, rm_first_step, nrec,
     &          remove_very_first_step, remove_very_last_step, n_dif_file,
     &          max_nvar, myallays, v, seq_pt, group_pt, constant_pt

        real, allocatable :: data(:, :, :, :), pmdata(:, :, :, :), tpmdata(:, :, :)
        real :: loc_constant
        logical :: keep_first_step, keep_last_step, done

        interface
          integer function find_variable (vname, vlist, nvlist)
            character (len=16), intent(in) :: vname
            character (len=16), intent(in) :: vlist(:)
            integer, intent(in)            :: nvlist
          end function find_variable
        end interface

        call get_env (numfile, 'numfile', 1)
        call get_env (rm_last_step, 'rm_last_step', 0)
        call get_env (rm_first_step, 'rm_first_step', 0)
        call get_env (keep_last_step, 'keep_last_step', .false.)
        call get_env (keep_first_step, 'keep_first_step', .false.)
        call get_env (n_dif_file, 'n_dif_file', 1)
        call get_env (numfile, 'numfile', 1)

        max_nvar = maxval(formula(:)%n_spc)

        allocate (data(ncols, nrows, nlays, max_nvar),
     $            pmdata(ncols, nrows, nlays, n_formula),
     $            tpmdata(ncols, nrows, nlays),
     $            infname(n_dif_file),
     $            stat=stat)

        jdate = sdate
        jtime = stime

        if (nlays == 1) then
           myallays = 1
        else
           myallays = ALLAYS3
        end if

        do n = 1, numfile

           do j = 1, n_dif_file
              write (infname(j), '(a6, i2.2, a1, i3.3)') 'infile', j, '_', n

              if (.not. open3(infname(j), fsread3, 'csum')) then
                 print *, ' Error: Could not open ', infname(j)
                 stop
              end if
              if (j == 1) then
                 if (.not. desc3(infname(j))) then
                    print *, ' Error: Could not desc3 ', infname(j)
                 end if
                 nrec = mxrec3d
              end if
           end do

           if ((n == 1) .and. (.not. keep_first_step)) then
              remove_very_first_step = 1
           else
              remove_very_first_step = 0
           end if

           if ((n == numfile) .and. (.not. keep_last_step)) then
              remove_very_last_step = 1
           else
              remove_very_last_step = 0
           end if

           if (n .eq. 1) then
              nsteps = nrec - rm_last_step - remove_very_first_step
           else if (n .eq. numfile) then
              nsteps = nrec - rm_first_step - remove_very_last_step
           else
              nsteps = nrec - rm_first_step - rm_last_step
           end if

           do istep = 1, nsteps

              pmdata = 0.0
              do i = 1, n_formula

                 do j = 1, formula(i)%n_spc
                    if (formula(i)%file_number(j) == 0) then
                       v = find_variable (formula(i)%spc_name(j), formula(:)%formula_spc_name, i)
                       data(:,:,:,j) = pmdata(:,:,:,v)
                    else
                       if ( .not. read3( infname(formula(i)%file_number(j)), 
     &                                   formula(i)%spc_name(j), myallays,
     &                                   jdate, jtime, data(:,:,:,j) ) ) then
                          print *, 'could not read from ', infname(formula(i)%file_number(j))
                          stop
                       end if
                    end if
                 end do

                 v = 0
                 seq_pt = 0
                 group_pt = 1
                 constant_pt = 0
                 loc_constant = 1.0
                 do while (seq_pt < formula(i)%n_seq)
                    seq_pt = seq_pt + 1
! regular specie
                    if (formula(i)%sequence(seq_pt) == 'e') then
                       v = v + 1
                       pmdata(:,:,:,i) = pmdata(:,:,:,i) + data(:,:,:,v) * formula(i)%coef(v)
! block specie
                    else if (formula(i)%sequence(seq_pt) == ':') then
                       tpmdata = 0.0
                       do k = formula(i)%group(group_pt), formula(i)%group(group_pt+1)
                          v = v + 1
                          seq_pt = seq_pt + 1
                          tpmdata(:,:,:) = tpmdata(:,:,:) + data(:,:,:,v) * formula(i)%coef(v)
                       end do
                       group_pt = group_pt + 2
                       seq_pt = seq_pt + 2
                       v = v + 1
                       if (formula(i)%sequence(seq_pt) == 'e') then
                          pmdata(:,:,:,i) = pmdata(:,:,:,i) + loc_constant * tpmdata(:,:,:) * data(:,:,:,v)
                       else if (formula(i)%sequence(seq_pt) == '/') then
                          seq_pt = seq_pt + 1
                          pmdata(:,:,:,i) = pmdata(:,:,:,i) + loc_constant * tpmdata(:,:,:) / data(:,:,:,v)
                       end if 
                       loc_constant = 1.0
! compute constant
                    else if (formula(i)%sequence(seq_pt) == 'c') then
                       done = .false.
                       loc_constant = 1.0
                       do while (.not. done)
                          if (formula(i)%sequence(seq_pt) == 'c') then
                             constant_pt = constant_pt + 1 
                             loc_constant = loc_constant * formula(i)%constant(constant_pt)
                             seq_pt = seq_pt + 1
                          else if ((formula(i)%sequence(seq_pt+1) == ':') .or.
     &                             (formula(i)%sequence(seq_pt+1) == 'e')) then
                             done = .true.
                             seq_pt = seq_pt - 1
                          else
                             constant_pt = constant_pt + 1 
                             loc_constant = loc_constant / formula(i)%constant(constant_pt)
                             seq_pt = seq_pt + 2
                          end if
                       end do
                    end if
                 end do

              end do

              if (colsum) then
                 do i = 1, n_formula
                    do r = 1, nrows3d
                       do c = 1, nrows3d
                          pmdata(c,r,1,i) = sum(pmdata(c,r,1:nlays,i))
                       end do
                    end do
                 end do
                 do v = 1, n_formula
                    if (formula(v)%output_spc) then
                       if ( .not. write3(outfname, formula(v)%formula_spc_name,
     &                                   jdate, jtime, pmdata(:,:,1,v))) then
                          print *, 'could not write to ', outfname
                          stop
                       end if
                    end if
                 end do
              else
                 do v = 1, n_formula
                    if (formula(v)%output_spc) then
                       if ( .not. write3(outfname, formula(v)%formula_spc_name,
     &                                   jdate, jtime, pmdata(:,:,:,v))) then
                          print *, 'could not write to ', outfname
                          stop
                       end if
                    end if
                 end do
              end if

              call nextime (jdate, jtime, timeinc)
           end do

           do j = 1, n_dif_file
              if (.not. close3(infname(j))) then
                 print *, ' Error: Could not close ', infname(j)
                 stop
              end if
           end do
        end do

        if ( .not. shut3() ) then
              write( 6, * )
     &               'could not shut down ipc files correctly'
        end if

        deallocate (infname, data, pmdata, tpmdata)

        return
        end subroutine compute
