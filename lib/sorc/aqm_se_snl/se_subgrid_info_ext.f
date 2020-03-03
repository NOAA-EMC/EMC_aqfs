
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_subgrid_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: -- all these variables with prefix se_ are for stencil exchange library 
C          only
C
C to define sub-grid common variables:
C
C se_my_subgrid_beglev -- beginning level number of sub-grid
C se_my_subgrid_endlev -- ending level number of sub-grid
C se_subgrid_ind       -- same as se_gl_ind but on fine sub grid
C se_subgrid_send      -- holding processor number info where data is sending 
C                         to
C se_subgrid_recv      -- holding processor number info where data is receiving 
C                         from
C se_subgrid_send_ind  -- holding row (first two entries) and column (second 
C                         two entries) dimensions of data for sending
C se_subgrid_recv_ind  -- holding row (first two entries) and column (second 
C                         two entries) dimensions of data for receiving
C --------------------------------------------------------------------------

	module se_subgrid_info_ext

          integer :: se_my_subgrid_beglev
          integer :: se_my_subgrid_endlev

          integer, allocatable, save, target :: se_subgrid_ind (:, :, :)
          integer, pointer :: se_subgrid_ind_ptr (:, :, :)

          integer, allocatable, save, target :: se_subgrid_send_ind (:, :, :)
          integer, allocatable, save, target :: se_subgrid_recv_ind (:, :, :)
          integer, pointer :: se_subgrid_send_ind_ptr (:, :, :)
          integer, pointer :: se_subgrid_recv_ind_ptr (:, :, :)

          integer, allocatable, save, target :: se_subgrid_send(:)
          integer, allocatable, save, target :: se_subgrid_recv(:)
          integer, pointer :: se_subgrid_send_ptr(:)
          integer, pointer :: se_subgrid_recv_ptr(:)

        end module se_subgrid_info_ext
