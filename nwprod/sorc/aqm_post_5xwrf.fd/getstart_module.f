        module getstart_module

	use readdata_module

        implicit none

        interface getstart
        module procedure getstart, getstart_var, 
     &         getstart_b, getstart_m
        end interface

        contains

C -----------------------------------------------------------------

        subroutine getstart (ncols, nrows, nlays, nspcs, sdate, 
     &             stime, timeinc, nrec, fname, spcname, 
     &             display, tol, funit, N)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: ncols,nrows,nlays,nspcs,sdate,stime
        integer, intent(in) :: timeinc, nrec, funit, N
        logical, intent(in) :: display
        real, intent(in)    :: tol
        character (len = 16), intent(in) :: spcname(nspcs)

        include "IODECL3.EXT" ! I/O definitions and declarations.

        integer :: jdate, jtime
        INCLUDE "SPECIES.comm"

        jdate = sdate
        jtime = stime

        call readdata (species, ncols, nrows, nlays, 
     &                 nspcs, fname, jdate, jtime, spcname)

        return
        end subroutine getstart

C --------------------------------------------------------------------------
        subroutine getstart_var (ncols, nrows, nlays, nspcs, 
     &             sdate, stime, timeinc, nrec, fname, 
     &             spcname, flag, display, tol, funit, N)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: ncols, nrows, nlays, nspcs, 
     &                         sdate, stime, N
        integer, intent(in) :: timeinc, nrec, flag, funit
        logical, intent(in) :: display
        real, intent(in)    :: tol
        character (len = 16), intent(in) :: spcname(nspcs)

        include "IODECL3.EXT" ! I/O definitions and declarations.

        integer :: jdate, jtime, i, j ,k
        integer :: loc_spcs, ierror

        REAL, ALLOCATABLE,DIMENSION(:,:,:) :: data1
        INCLUDE "SPECIES.comm"

        character (len = 16) :: s
        integer :: c, r, l
        real :: maxabs, v1, v2, fmax, fmin

        ALLOCATE(data1(ncols,nrows,nlays), STAT = ierror)

        jdate = sdate
        jtime = stime

        call nextime (jdate, jtime, (N-1)*timeinc)
        do loc_spcs = 1, nspcs

            call readdata (data1, ncols, nrows, nlays, 
     &      loc_spcs, fname, jdate, jtime, spcname(loc_spcs))
            do k=1,nlays
               do j=1,nrows
                  do i=1,ncols
                     species(i,j,k,loc_spcs)=data1(i,j,k)
                  enddo
                enddo
             enddo

         if (N .eq. 33) then   !!Diagnostic ^^^^
            fmin=1e20
            fmax=1e-20
            do k=1,nlays
               do j=1,nrows
                  do i=1,ncols
                     if(data1(i,j,k).ge.fmax) fmax=data1(i,j,k)
                     if(data1(i,j,k).le.fmin) fmin=data1(i,j,k)
                  enddo
               enddo
             enddo
      write(6,*) "jdate=",jdate,"jtime=",jtime,"N*timeinc=",N*timeinc
      write(6,*)"S=",spcname(loc_spcs),"fmax=",fmax,"fmin=",fmin,
     &   "jdate, jtime, timeinc",jdate, jtime, timeinc
           endif                      !!Diagnostic VVVV
          end do

        DEALLOCATE(data1)

        return
        end subroutine getstart_var

C -------------------------------------------------------------------
        subroutine getstart_b (nrows, nlays, nspcs, sdate, stime,
     &                         timeinc, nrec, fname, spcname, 
     &                         display, tol, funit)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: nrows, nlays, nspcs, sdate, stime
        integer, intent(in) :: timeinc, nrec, funit
        logical, intent(in) :: display
        real, intent(in)    :: tol
        character (len = 16), intent(in) :: spcname(nspcs)

        include "IODECL3.EXT"      ! I/O definitions and declarations.

        integer :: jdate, jtime, ierror

        integer :: istep
        REAL, ALLOCATABLE,DIMENSION(:,:,:) :: data1
        ALLOCATE(data1(nrows,nlays,nspcs), STAT = ierror)

        jdate = sdate
        jtime = stime

        do istep = 1, nrec

           call readdata (data1, nrows, nlays, nspcs, fname, 
     &                    jdate, jtime, spcname)

        end do
        DEALLOCATE(data1)
        return
        end subroutine getstart_b

C ------------------------------------------------------------------
        subroutine getstart_m (ncols, nrows, fname1, 
     &             fname2, display, funit)

        implicit none

        integer, intent(in) :: ncols, nrows, funit
        character (len = 16), intent(in) :: fname1, fname2
        logical, intent(in) :: display

        include "IODECL3.EXT"  ! I/O definitions and declarations.
        include "PARMS3.EXT"   ! I/O parameters definitions.

        integer :: jdate, jtime, fid

        integer :: data1(ncols+nrows)
        integer :: data2(ncols+nrows)

        character (len = 16) :: pname = 'getstart_m'
        character (len = 120) :: xmsg
c       integer :: xstat1 = 1

        integer, external  :: name2fid
        logical, external :: rdsmatrx


        if ( open3( fname1, fsread3, pname ) ) then

           fid = name2fid( fname1 )

           if ( .not. rdsmatrx( fid, -1, 1, data1 ) ) then
              xmsg = 'could not read sparse matrix file #1' // fname1
              call m3exit( pname, jdate, jtime, xmsg, xstat1 )
           end if
        end if

        if ( open3( fname2, fsread3, pname ) ) then

           fid = name2fid( fname2 )

           if ( .not. rdsmatrx( fid, -1, 1, data2 ) ) then
              xmsg = 'could not read sparse matrix file #2' // fname1
              call m3exit( pname, jdate, jtime, xmsg, xstat1 )
           end if
        end if

        return
        end subroutine getstart_m

        end module getstart_module
