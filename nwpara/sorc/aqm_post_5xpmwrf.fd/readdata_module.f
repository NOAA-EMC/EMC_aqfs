        module readdata_module

        implicit none

        interface readdata
          module procedure readdata3, readdata4, readdata4_spc
        end interface

        contains

C -----------------------------------------------------------------------------

        subroutine readdata3 (data, nrows, nlays, nspcs,
     $                       fname, jdate, jtime, spcname)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: nrows, nlays, nspcs
        real, intent(out) :: data(nrows, nlays, nspcs)
        character (len = 16), intent(in) :: spcname(nspcs)
        integer, intent(in) :: jdate, jtime

        include "PARMS3.EXT"           ! I/O parameters definitions.
        include "IODECL3.EXT"          ! I/O definitions and declarations.

        character (len = 120) :: xmsg = ' '
        character (len = 16)  :: pname = 'readdata'
c       integer :: xstat1 = 1
        integer :: spc

        if ( open3( fname, fsread3, pname ) ) then

           data = 0.0

           do spc = 1, nspcs
              if ( .not. read3( fname, spcname(spc), allays3,
     &             jdate, jtime, data( 1,1,spc ) ) ) then
                 xmsg = 'could not read ' // spcname(spc) // ' from '
     &                  // fname
                 call m3exit( pname, jdate, jtime, xmsg, xstat1 )
              end if
           end do
        else
           write(6,*)
           write(6,*) ' could not open file.'
           write(6,*)
        end if

        return
        end subroutine readdata3

C -----------------------------------------------------------------------------

        subroutine readdata4 (data, ncols, nrows, nlays, nspcs,
     $                       fname, jdate, jtime, spcname)

        implicit none

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: ncols, nrows, nlays, nspcs
        real, intent(out) :: data(ncols, nrows, nlays, nspcs)
        character (len = 16), intent(in) :: spcname(nspcs)
        integer, intent(in) :: jdate, jtime

        include "PARMS3.EXT"           ! I/O parameters definitions.            
        include "IODECL3.EXT"          ! I/O definitions and declarations.      

        character (len = 120) :: xmsg = ' '
        character (len = 16)  :: pname = 'readdata'
c       integer :: xstat1 = 1
        integer :: spc

        if ( open3( fname, fsread3, pname ) ) then

           data = 0.0

           do spc = 1, nspcs                                                    
              if ( .not. read3( fname, spcname(spc), allays3,                   
     &             jdate, jtime, data( 1,1,1,spc ) ) ) then                     
                 xmsg = 'could not read ' // spcname(spc) // ' from '           
     &                  // fname                                                
                 call m3exit( pname, jdate, jtime, xmsg, xstat1 )               
              end if                                                            
           end do                                                               
        else                                                                    
           write(6,*)                                                           
           write(6,*) ' could not open file.'                                   
           write(6,*)                                                           
        end if                                                                  
                                                                                
        return                                                                  
        end subroutine readdata4

C -----------------------------------------------------------------------------

        subroutine readdata4_spc (data, ncols, nrows, nlays, spcs,
     $                            fname, jdate, jtime, spcname)
                                                                                
        implicit none                                                           

        character (len = 16), intent(in) :: fname
        integer, intent(in) :: ncols, nrows, nlays, spcs
        real, intent(out) :: data(ncols, nrows, nlays)
        character (len = 16), intent(in) :: spcname
        integer, intent(in) :: jdate, jtime

        include "PARMS3.EXT"           ! I/O parameters definitions.            
        include "IODECL3.EXT"          ! I/O definitions and declarations.      

        character (len = 120) :: xmsg = ' '
        character (len = 16)  :: pname = 'readdata'
c       integer :: xstat1 = 1
        integer :: spc

        if ( open3( fname, fsread3, pname ) ) then

           data = 0.0

           if ( .not. read3( fname, spcname, allays3,                   
     &          jdate, jtime, data( 1,1,1 ) ) ) then                     
              xmsg = 'could not read ' // spcname // ' from '           
     &               // fname                                                
              call m3exit( pname, jdate, jtime, xmsg, xstat1 )               
           end if                                                            
        else                                                                    
           write(6,*)                                                           
           write(6,*) ' could not open file.'                                   
           write(6,*)                                                           
        end if                                                                  
                                                                                
        return
        end subroutine readdata4_spc

        end module readdata_module
