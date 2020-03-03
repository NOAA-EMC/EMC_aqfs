C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_reconfig_grid_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: -- all these variables with prefix se_ are for stencil exchange library 
C          only
C       -- variable *_ptr is a pointer type variable of its counter part *
C
C to define grid reconfiguration process common variables:
C
C se_reconfig_grid_send_ind -- indexes determine which part of the data is
C                              sending
C se_reconfig_grid_recv_ind -- indexes determine which part of the data is 
C                              receiving
C se_reconfig_grid_send     -- PE number indicates where data is sending to
C se_reconfig_grid_recv     -- PE number indicates where data is coming from
C se_grid1_map  -- mapping info of grid 1
C se_grid2_map  -- mapping info of grid 2
C --------------------------------------------------------------------------

	module se_reconfig_grid_info_ext

          integer, allocatable, save, target :: se_reconfig_grid_send_ind(:,:,:)
          integer, allocatable, save, target :: se_reconfig_grid_recv_ind(:,:,:)
          integer, pointer :: se_reconfig_grid_send_ind_ptr (:, :, :)
          integer, pointer :: se_reconfig_grid_recv_ind_ptr (:, :, :)

          integer, allocatable, save, target :: se_reconfig_grid_send(:)
          integer, allocatable, save, target :: se_reconfig_grid_recv(:)
          integer, pointer :: se_reconfig_grid_send_ptr(:)
          integer, pointer :: se_reconfig_grid_recv_ptr(:)

          integer, allocatable, target :: se_grid1_map(:,:,:)
          integer, pointer :: se_grid1_map_ptr(:,:,:)
          integer, allocatable, target :: se_grid2_map(:,:,:)
          integer, pointer :: se_grid2_map_ptr(:,:,:)

        end module se_reconfig_grid_info_ext
