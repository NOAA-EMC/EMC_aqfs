# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/work/rep/STENEX/src/se_snl/Makefile,v 1.2 2006/02/17 12:52:59 yoj Exp $

SHELL=/bin/sh

LIBNAME = libaqm_se_snl.a

FC = ftn
MPI_INC = /opt/cray/mpt/7.2.0/gni/mpich2-intel/140/include 

FFLAGS = -fixed -132 -O3 -override-limits -fno-alias -mp1 -fp-model precise -I$(MPI_INC)



#> the following order is important
 OBJS = \
  se_comm_info_ext.o\
  se_disp_info_ext.o\
  se_domain_info_ext.o\
  se_pe_info_ext.o\
  se_subgrid_info_ext.o\
  se_reconfig_grid_info_ext.o\
  se_bndy_copy_info_ext.o\
  se_internal_util_module.o\
  se_global_max_module.o\
  se_global_min_module.o\
  se_global_sum_module.o\
  se_util_module.o\
  se_data_send_module.o\
  se_data_recv_module.o\
  se_slice_module.o\
  se_comm_module.o\
  se_gather_module.o\
  se_data_copy_module.o\
  se_reconfig_grid_module.o\
  se_bndy_copy_module.o\
  swap_sandia.o\
  swap_sandia_routines.o\
  se_init_module.o\
  se_term_module.o\
  se_twoway_comm_module.o\
  se_modules.o  


$(LIBNAME) : $(OBJS)
	$(FC) $(FFLAGS) -c $?
	ar -crv ../../$@ $(OBJS)
	ranlib ../../$@
#	ar -crv $@ $?
#	chmod 644 $@

clean:
	rm -f *.a $(OBJS) *.mod
