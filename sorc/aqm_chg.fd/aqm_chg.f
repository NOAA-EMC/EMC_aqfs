        program driver

        implicit none

        include 'PARMS3.EXT'
        include 'IODECL3.EXT'
        include 'FDESC3.EXT'
        include 'STATE3.EXT'

        character (len = 16) :: infile, outfile
        character (len = 16) :: oldvar(mxvars3), newvar(mxvars3)
        integer :: istep, envint, injtime, injdate, outjtime, outjdate,
     $             newtime, newdate, stat, v, m, n, status, nspcs, bndy,
     $             slvl, elvl, n_subset_spc
        character (len = 16), allocatable :: spcname(:), subset_spcname(:)
        character (len = 16) :: buf, newgrid_name
        character (len = 2000) :: vlist
        real :: value
        real, allocatable :: data(:,:,:)
        logical :: done, chgtime, chgvar, chggrid, chgvaluel, chgcent, 
     $             chgorig, chgtstep, spc, chgp, chvgtype
        logical, external :: envyn
        real, external :: envreal
        integer, external :: index1
        integer, allocatable :: subset_spc_index(:)
        double precision :: envdble

        interface
          subroutine ext_subset_list (spcname, nspcs, subset_spc_index)
            character (len = 16), intent(inout) :: spcname(:)
            integer, intent(inout) :: nspcs, subset_spc_index(:)
          end subroutine ext_subset_list
        end interface

        infile = 'infile'
        outfile = 'outfile'

        chgtime   = ENVYN ('chgtime', ' ', 'N', status)
        chgvar    = ENVYN ('chgvar', ' ', 'N', status)
        chggrid   = ENVYN ('chggrid', ' ', 'N', status)
        chgvaluel = ENVYN ('chgvaluel', ' ', 'N', status)
        chgcent   = ENVYN ('chgcent', ' ', 'N', status)
        chgorig   = ENVYN ('chgorig', ' ', 'N', status)
        chgtstep  = ENVYN ('chgtstep', ' ', 'N', status)
        chgp      = ENVYN ('chgp', ' ', 'N', status)
        chvgtype  = ENVYN ('chvgtype', ' ', 'N', status)

        if (chgvar) then
           call nameval ('var_list', vlist)
           nspcs = 0
           done = .false.
           m = len(trim(vlist))
           n = 1
           do while ((.not. done) .and. (n .le. m))
              read (vlist(n:m), *, iostat=stat) buf
              if (stat .ne. 0) then
                 done = .true.
              else
                 nspcs = nspcs + 1
                 if (mod(nspcs, 2) .eq. 1) then
                    oldvar((nspcs+1)/2) = buf
                 else
                    newvar(nspcs/2) = buf
                 end if
                 n = len(trim(buf))+n+1
              end if
           end do
           nspcs = nspcs / 2
        end if

        if ( .not. open3( infile, fsread3, 'driver' ) ) then
           write(6,*)
           write(6,*) ' could not open file:', infile
           write(6,*)
           stop
        end if
        if ( .not. desc3 (infile) ) then
           write(6,*)
           write(6,*) ' could not get file description:', infile
           write(6,*)
           stop
        end if

        if (chgp) then
           p_alp3d = ENVDBLE ('palpha', ' ', 1.0, status)
           p_bet3d = ENVDBLE ('pbeta', ' ', 1.0, status)
           p_gam3d = ENVDBLE ('pgamma', ' ', 1.0, status)
        end if

        if (chvgtype) then
           vgtyp3d = ENVINT ('vgtype', ' ', 1.0, status)
        end if

        if (chgvaluel) then
           slvl = ENVINT ('slvl', ' ', 1, status)
           elvl = ENVINT ('elvl', ' ', nlays3d, status)
           value = ENVREAL ('value', ' ', 0.0, status)
           spc = ENVYN ('spc', ' ', 'N', status)
           n_subset_spc = nvars3d
           if (spc) then
              allocate (subset_spcname(nvars3d), spcname(nvars3d), subset_spc_index(nvars3d), stat=stat)
              spcname = vname3d(1:nvars3d)
              subset_spcname = spcname
              call ext_subset_list (subset_spcname, n_subset_spc, subset_spc_index)
           else
              do v = 1, nvars3d
                 subset_spc_index(v) = v
              end do
           end if
        end if

!       if (tstep3d .eq. 0) then
!          tstep3d = 10000
!       end if

        injtime = stime3d
        injdate = sdate3d

        if (chgcent) then
           xcent3d = ENVREAL ('xcent', ' ', 1, status)
           ycent3d = ENVREAL ('ycent', ' ', 1, status)
        end if

        if (chgorig) then
           xorig3d = ENVREAL ('xorig', ' ', 1, status)
           yorig3d = ENVREAL ('yorig', ' ', 1, status)
        end if

        if (chgtstep) then
           tstep3d = ENVINT ('tstep', ' ', 10000, status)
        end if

        if (chgtime) then
           newtime = envint ('newtime', ' ', 0, stat)
           newdate = envint ('newdate', ' ', 0, stat)
           stime3d = newtime
           sdate3d = newdate
        end if

        if (chggrid) then
           call nameval ('newgrid', newgrid_name)
           gdnam3d = newgrid_name
        end if

        outjtime = stime3d
        outjdate = sdate3d

        if (ftype3d .eq. BNDARY3) then
           bndy = 2 * nthik3d * (ncols3d + nrows3d + 2 * nthik3d)
           allocate (data(bndy, 1, nlays3d), stat=stat)
        else
           allocate (data(ncols3d, nrows3d, nlays3d), stat=stat)
        end if
        if (stat .ne. 0) then
           print *, 'Error: Allocating memory'
           stop
        end if

        if (chgvar) then
           do m = 1, nspcs
              n = index1(oldvar(m), nvars3d, vname3d)
              vname3d(n) = newvar(m)
           end do
        end if

        if ( .not. open3( outfile, fsnew3, 'driver' ) ) then
           write(6,*)
           write(6,*) ' could not open file:', outfile
           write(6,*)
           stop
        end if

        do istep = 1, mxrec3d
           do v = 1, nvars3d
              if ( .not. read3(infile, vname3d(v), allays3,
     &                         injdate, injtime, data ) ) then
                 print *, 'could not read from ', infile
                 stop
              end if

              if (chgvaluel) then
!                do v = 1, n_subset_spc
!                   data(:,:,slvl:elvl,subset_spc_index(v)) = value
                    data(:,:,slvl:elvl) = value
!                end do
              end if

              if ( .not. write3(outfile, vname3d(v), outjdate,
     &                          outjtime, data)) then
                 print *, 'could not write to ', outfile
                 stop
              end if
           end do

           if (istep < mxrec3d) then
              call nextime (injdate, injtime, tstep3d)
              call nextime (outjdate, outjtime, tstep3d)
           end if
        end do

        deallocate (data)

        if ( .not. shut3() ) then
              write( logdev, * )
     &               'could not shut down ipc files correctly'
        end if

        end program driver

C ----------------------------------------------------------------------------
        subroutine ext_subset_list (spcname, nspcs, subset_spc_index)

        implicit none

        include 'PARMS3.EXT'

        character (len = 16), intent(inout) :: spcname(:)
        integer, intent(inout) :: nspcs, subset_spc_index(:)

        character (len = mxvars3*16) :: list
        integer :: tnspcs
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

        m = 0
        do while (m .lt. tnspcs)
           m = m + 1
           buf = tspcname(m)
           found = .false.
           n = 0
           do while ((.not. found) .and. (n .lt. nspcs))
              n = n + 1
              if (trim(buf) .eq. trim(spcname(n))) then
                 found = .true.
                 subset_spc_index(m) = n
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
