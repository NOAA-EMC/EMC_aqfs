C-------------------------------------------------------------------  
C ncols   -- number of columns in the data (like IMAXIN)
C nrows   -- number of rows in the data    (like JJMAXIN)
C nlays   -- number of layers in the data  (grib handles one layer at a time)
C nspcs   -- number of species             (like nflds)
C sdate   -- starting date of the data     (with stime,sets KPDS(8:12)
C stime   -- starting time of the data 
C timeinc -- time increment                (fcst time unit: KPDS(13)
C fname   -- input file name
C -------------------------------------------------------------------  
        subroutine notcdf_in(ncols,nrows,nlays,nspcs,nrec,
     &                       N,sdate,stime,spcname)  

        use getinfo_module
        use getstart_module

        implicit none

        integer  ncols, nrows, nlays, nspcs, funit

        character*16 fname

        integer sdate         ! current model date , coded yyyyddd.
        integer stime         ! current model time , coded hhmmss. 
        integer mystime, mysdate, mynsteps, timeinc, nrec, ftype

        logical display, spc, info
        real    tol

	character*16 spcname(200)

        integer envint, status
        real envreal

        LOGICAL, EXTERNAL :: ENVYN
        integer N,kk
        INCLUDE "SPECIES.comm"

C*********************************************************************        

        funit = 6

        fname = 'file1'

        display = ENVYN ('display', ' ', display, status)
        spc = ENVYN ('spc', ' ', spc, status)
        info = ENVYN ('info', ' ', info, status)

        tol = ENVREAL ('tol', ' ', 0, status)
        mysdate = ENVINT ('mysdate', ' ', 0, status)
        mystime = ENVINT ('mystime', ' ', 0, status)
        mynsteps = ENVINT ('mynsteps', ' ', 0, status)

           call getinfo (ncols, nrows, nlays, nspcs, ftype, sdate, 
     &                 stime, timeinc, nrec, fname, spcname, spc, 
     &                 info, mysdate, mystime, mynsteps, 1)

        if (.not. info) then
           if (ncols .eq. -1) then    ! boundary file
CC              call getstart_b (nrows, nlays, nspcs, sdate, stime, 
CC     &             timeinc, nrec, fname, spcname, 
CC     &             display, tol, funit)

           else
               call getstart (ncols,nrows,nlays,nspcs,sdate,stime,
     &              timeinc, nrec, fname, spcname, 1, 
     &              display, tol, funit, N)
           end if
        end if

        write (funit, *) ' '
        write (funit, *) 'Notcdf_in ',N,' step successfully done.'
        write (funit, *) ' '

        end
