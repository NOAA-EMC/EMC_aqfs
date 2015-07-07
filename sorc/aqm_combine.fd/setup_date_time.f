	program driver

	use sub_module

        integer :: funit

        character (len = 16) :: fname

        integer :: sdate                ! current model date , coded yyyyddd.
        integer :: stime                ! current model time , coded hhmmss.
        integer :: edate                
        integer :: etime                
        integer :: mystime
        integer :: mysdate
        integer :: mynsteps
        integer :: timeinc
        integer :: nrec

        logical :: ncf

        integer :: envint, status
        logical, external :: envyn

c       funit = init3()

        fname = 'INFILE1'
        ncf = ENVYN ('ncf', ' ', ncf, status)
        mysdate = ENVINT ('mysdate', ' ', 0, status)
        mystime = ENVINT ('mystime', ' ', 0, status)
        mynsteps = ENVINT ('mynsteps', ' ', 0, status)

        if (ncf) then
           call get_date_time (fname, sdate, stime, timeinc,
     &                         nrec, mysdate, mystime, mynsteps)
        else
            call get_date_time (fname, sdate, stime, timeinc,
     &                         nrec, mysdate, mystime, mynsteps, 1)
        end if

        edate = sdate
        etime = stime
        do i = 1, nrec-1
           call nextime (edate, etime, timeinc)
        end do

        open (unit = 10, file='header_defn', status='unknown')
        write (10, 15) '/ variable definitions for extractions'
 15     format (a38)
        write (10, 16) '/'
 16     format (a1)
        write (10, 17) '#Start   ', sdate, stime
 17     format (a9, i7, 2x, i6)
        write (10, 17) '#End     ', edate, etime

        close (10)

	end program driver
