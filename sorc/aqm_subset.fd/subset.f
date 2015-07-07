! ----------------------------------------------------------------------------  
! ncols   -- number of columns in the data                                      
! nrows   -- number of rows in the data                                         
! nlays   -- number of layers in the data                                       
! nspcs   -- number of spceies                                                  
! sdate   -- starting date of the data                                          
! stime   -- starting time of the data                                          
! timeinc -- time increment                                                     
! fname1  -- input file one                                                     
! fname2  -- input file two                                                     
! ----------------------------------------------------------------------------  
                                                                                
        program subset

        use data_module
        use setup_module
        use extract_module

        implicit none

        include "IODECL3.EXT"           ! I/O definitions and declarations.

        integer :: funit

        character (len=16) :: fname1, fname2

        integer :: sdate                ! current model date , coded yyyyddd.
        integer :: stime                ! current model time , coded hhmmss. 

        integer :: mystime
        integer :: mysdate
        integer :: mynsteps
        integer :: mytsteps

        integer :: mysrec
        integer :: myerec

        integer :: timeinc                                                   
        integer :: nrec
        integer :: ounit
        integer :: ftype

        type (domain_info_rec) :: domain_info

        logical :: spc, info, outformat, ncf, vp, window, timeind

        integer :: envint, status, logdev

        LOGICAL, EXTERNAL :: ENVYN

!***********************************************************************        

        call check_mismatch

        logdev = init3()

        funit = 6

        fname1 = 'file1'
        fname2 = 'file2'

        spc = ENVYN ('spc', ' ', spc, status)
        info = ENVYN ('info', ' ', info, status)
        outformat = ENVYN ('output', ' ', outformat, status)

        if (outformat) then
           ounit = 10
        else
           ounit = 0
        end if

        mysdate = ENVINT ('mysdate', ' ', 0, status)
        mystime = ENVINT ('mystime', ' ', 0, status)
        mynsteps = ENVINT ('mynsteps', ' ', 0, status)
        mytsteps = ENVINT ('mytsteps', ' ', 0, status)
        mysrec = ENVINT ('mysrec', ' ', 0, status)
        myerec = ENVINT ('myerec', ' ', 0, status)

        domain_info%slay = ENVINT ('myslay', ' ', 0, status)
        domain_info%elay = ENVINT ('myelay', ' ', 0, status)
        domain_info%scol = ENVINT ('myscol', ' ', 0, status)
        domain_info%ecol = ENVINT ('myecol', ' ', 0, status)
        domain_info%srow = ENVINT ('mysrow', ' ', 0, status)
        domain_info%erow = ENVINT ('myerow', ' ', 0, status)

        ncf = ENVYN ('ncf', ' ', ncf, status)
        vp = ENVYN ('vp', ' ', vp, status)
        window = ENVYN ('window', ' ', window, status)
        timeind = ENVYN ('timeind', ' ', timeind, status)

        call setup (domain_info, ftype, sdate, stime, timeinc, nrec, &
                  fname1, fname2, mysdate, mystime, mynsteps, mytsteps,&
                  mysrec, myerec, ounit, spc, info, vp, window, timeind)

        if (.not. info) then
           if (ftype .eq. 2) then   ! boundary file
              if (vp) then
                 call extract (domain_info, sdate, stime,&
                              timeinc, nrec, fname1, 1)
              else
               call extract (domain_info, sdate, stime, timeinc, nrec ,&
                              fname1, fname2, ounit, window, 1)
              end if 
           else 
              if (vp) then
                 call extract (domain_info, sdate, stime, &
                               timeinc, nrec, fname1)
              else
                 call extract (domain_info, sdate, stime, timeinc,&
                               nrec, fname1, fname2, ounit, window)
              end if
           end if
        end if

        if ( .not. shut3() ) then
              write( logdev, * ) &
                     'could not shut down ipc files correctly'
        end if

        if (allocated(idata)) then
           deallocate(idata)
        end if

        if (allocated(odata)) then
           deallocate(odata)
        end if

        if (.not. info) then
           write (funit, *) ' '
           write (funit, *) ' Subset extraction is successfully done !'
           write (funit, *) ' '
        end if

        end program subset
