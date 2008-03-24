	module getinfo_module

        implicit none

        interface getinfo
          module procedure getinfo, getinfo_bin
        end interface

        contains

C ----------------------------------------------------------------------------
        subroutine getinfo (ncols, nrows, nlays, nspcs, ftype, 
     &                      sdate, stime, timeinc, nrec, fname1, 
     &                      spcname, spc, info, mysdate, mystime, 
     &                      mynsteps)

        implicit none

        character (len = 16), intent(in) :: fname1
        character (len = 16), intent(out) :: spcname(200)
        integer, intent(inout) :: ftype
        integer, intent(out) :: ncols, nrows, nlays, nspcs,
     &                          sdate, stime, timeinc, nrec 
        integer, intent(in) :: mysdate, mystime, mynsteps
        logical, intent(in) :: spc, info

	include "netcdf.inc"

        integer :: mtlen
        parameter (mtlen=2000)

        integer :: perim, perim_id
        character (len = 16) :: tspcname(20)
	integer :: ncid, rcode, ttype, tlen
        character (len = 2000) :: varlist
        character (len = 31) :: perim_name
        integer :: varlen
        parameter (varlen = 16)
	integer :: i, l1, l2, tnspcs
	integer :: tstepid, stat
        character (len = 16) :: tstep, buf
        character (len = 256) :: eqname, list, tlist
        logical :: stop, found
        integer :: m, n

        call nameval (fname1, eqname)

        ncid = ncopn (eqname, ncnowrit, rcode)

        call ncagt (ncid, ncglobal, 'FTYPE', ftype, rcode)

        if (ftype .eq. 2) then   ! boundary file
           perim_id = ncdid(ncid, 'PERIM', rcode)
           call ncdinq(ncid, perim_id, perim_name, perim, rcode)
           nrows = perim
           ncols = -1
        else
           call ncagt (ncid, ncglobal, 'NROWS', nrows, rcode)
           call ncagt (ncid, ncglobal, 'NCOLS', ncols, rcode)
        end if

        call ncagt (ncid, ncglobal, 'NLAYS', nlays, rcode)
        call ncagt (ncid, ncglobal, 'NVARS', nspcs, rcode)
        call ncagt (ncid, ncglobal, 'SDATE', sdate, rcode)
        call ncagt (ncid, ncglobal, 'STIME', stime, rcode)
        call ncagt (ncid, ncglobal, 'TSTEP', timeinc, rcode)

        if (timeinc .eq. 0) then
           timeinc = 1
        end if

c -- get maximum record number
        tstepid = NCDID (ncid, 'TSTEP', rcode)
        call ncdinq(ncid, tstepid, tstep, nrec, rcode)

        if (ftype .eq. 6) then
           goto 99
        end if
 
c -- check var-list storage max size
	call ncainq (ncid, NCGLOBAL, 'VAR-LIST', ttype, tlen, rcode)
        if (tlen > mtlen) then
           write (*, *) 'VAR-LIST attribute too big !'
           call exit
        else
           call ncagtc (ncid, ncglobal, 'VAR-LIST', 
     &                  varlist, mtlen, rcode)
        end if

c -- extract species names
        l1 = 1
        l2 = varlen
        do i = 1, nspcs                                                         
           spcname(i) = varlist(l1:l2)
           l1 = l1 + varlen
           l2 = l2 + varlen
        end do                                                                  

 99     continue

        if (info) then
           print *, ' '
           print *, ' Display file information:'
           print *, ' '
           print *, 'nrow  = ', nrows
           print *, 'ncol  = ', ncols
           print *, 'nlay  = ', nlays
           print *, 'nspc  = ', nspcs
           print *, 'sdate = ', sdate
           print *, 'stime = ', stime
           print *, ' '
           stop
        else
           if (mynsteps .ne. -99) then

              if ((mysdate .lt. sdate) .or. ((mysdate .eq. sdate)
     &              .and. (mystime .lt. stime))) then
                 print *, ' '
                 print *, ' File sdate: ', sdate
                 print *, ' File stime: ', stime
                 print *, ' '
                 print *, ' Invalid date and/or time'
                 print *, ' Program terminates'
                 print *, ' '
                 stop
              else
                 sdate = mysdate
                 stime = mystime
                 nrec = mynsteps
              end if
           end if

           if (spc) then
              call nameval ('spc_list', list)
              tnspcs = 0
              stop = .false.
              m = len(trim(list))
              n = 0
              do while (.not. stop)
                read (list(1+n:m), *, iostat=stat) buf
                if (stat .ne. 0) then
                   stop = .true.
                else
                   tnspcs = tnspcs + 1
                   tspcname(tnspcs) = buf
                   n = len(trim(buf))+1+n
                end if
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
           end if
        end if

        return                                                                  
        end subroutine getinfo

C ----------------------------------------------------------------------------
        subroutine getinfo_bin (ncols, nrows, nlays, nspcs, ftype,
     &                          sdate, stime, timeinc, nrec, fname1,
     &                          spcname, spc, info, mysdate, mystime,
     &                          mynsteps, flag)

        implicit none

        character (len = 16), intent(in) :: fname1
        character (len = 16), intent(out) :: spcname(200)
        integer, intent(out) :: ftype
        integer, intent(out) :: ncols, nrows, nlays, nspcs,
     &                          sdate, stime, timeinc, nrec
        integer, intent(in) :: mysdate, mystime, mynsteps, flag
        logical, intent(in) :: spc, info

        character (len = 256) :: eqname, fname
        integer   i, l, lftype

        do l = 1, 256
           fname(l:l) = ' '
        end do

        call nameval (fname1, eqname)
        l = len(eqname)
        fname(1:l-4) = eqname(5:l)

        call OPEN_BINARY_FILE (ncols, nrows, nlays, nspcs, lftype,
     &                         sdate, stime, timeinc, nrec, fname)

        ftype = lftype

        if (lftype .ne. 6) then
           do i = 0, nspcs-1
              call extract_spcname (i, spcname(i+1))
           end do
        end if

        end subroutine getinfo_bin

	end module getinfo_module
