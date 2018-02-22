        module sub_module

        implicit none

        interface get_date_time
          module procedure get_date_time, get_date_time_bin
        end interface

        contains

C ----------------------------------------------------------------------------
        subroutine get_date_time (fname, sdate, stime, timeinc, 
     &                            nrec, mysdate, mystime, mynsteps)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(out) :: sdate, stime, timeinc, nrec 
        integer, intent(in) :: mysdate, mystime, mynsteps

        include "netcdf.inc"

        integer :: mtlen
        parameter (mtlen=2000)

        integer :: perim, perim_id
        integer :: ncid, rcode, ttype, tlen
        character (len = 2000) :: varlist
        character (len = 31) :: perim_name
        integer :: varlen
        parameter (varlen = 16)
        integer :: i, l1, l2
        integer :: tstepid
        character (len = 16) :: tstep
        character (len = 256) :: eqname

        call nameval (fname, eqname)

        ncid = ncopn (eqname, ncnowrit, rcode)

        call ncagt (ncid, ncglobal, 'SDATE', sdate, rcode)
        call ncagt (ncid, ncglobal, 'STIME', stime, rcode)
        call ncagt (ncid, ncglobal, 'TSTEP', timeinc, rcode)

        if (timeinc .eq. 0) then
           timeinc = 1
        end if

c -- get maximum record number
        tstepid = NCDID (ncid, 'TSTEP', rcode)
        call ncdinq(ncid, tstepid, tstep, nrec, rcode)

        if (mynsteps .ne. -99) then
           call init_my_set (mysdate, mystime, mynsteps, sdate, stime, nrec)
        end if

        end subroutine get_date_time

C ----------------------------------------------------------------------------
        subroutine get_date_time_bin (fname, sdate, stime, timeinc,
     &                                nrec, mysdate, mystime, mynsteps, flag)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(out) :: sdate, stime, timeinc, nrec
        integer, intent(in) :: mysdate, mystime, mynsteps, flag

        character (len = 256) :: eqname, lfname
        integer :: i, leng

        do i = 1, 256
           lfname = ' '
        end do

        call nameval (fname, eqname)

        leng = len(trim(eqname))

        lfname (1:leng) = eqname(5:leng)

        call OPEN_BINARY_FILE (sdate, stime, timeinc, nrec, lfname)

        if (mynsteps .ne. -99) then
           call init_my_set (mysdate, mystime, mynsteps, sdate, stime, nrec)
        end if

        end subroutine get_date_time_bin

C ----------------------------------------------------------------------------
        subroutine init_my_set (mysdate, mystime, mynsteps, sdate, stime, nrec)

        integer, intent(in)  :: mysdate, mystime, mynsteps
        integer, intent(inout) :: sdate, stime
        integer, intent(out) :: nrec

        if ((mysdate .lt. sdate) .or.
     $      ((mysdate .eq. sdate) .and. (mystime .lt. stime))) then
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

        end subroutine init_my_set

        end module sub_module
