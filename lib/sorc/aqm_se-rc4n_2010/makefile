# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/work/rep/STENEX/src/se_snl/Makefile,v 1.2 2006/02/17 12:52:59 yoj Exp $

SHELL=/bin/sh

LIBNAME = libaqm_sef90_2010.a

FC = mpfort

#FFLAGS = -132 -O3 -fp-model precise -convert big_endian -assume byterecl -g -traceback ##$(FSTD) -Q -O3 -qstrict
FFLAGS = -132 -O3 -fp-model precise -assume byterecl  ##$(FSTD) -Q -O3 -qstrict

#> the following order is important
 OBJS = \
 se_comm_info_ext.o \
 se_disp_info_ext.o \
 se_domain_info_ext.o \
 se_pe_info_ext.o \
 se_subgrid_info_ext.o \
 se_reconfig_grid_info_ext.o \
 se_bndy_copy_info_ext.o \
 se_ori_ext.o \
 se_internal_util_module.o \
 se_util_module.o \
 se_data_send_module.o \
 se_data_recv_module.o \
 se_global_min_module.o \
 se_global_max_module.o \
 se_global_sum_module.o \
 se_slice_module.o \
 se_comm_module.o \
 se_gather_module.o \
 se_data_copy_module.o \
 se_bndy_copy_module.o \
 se_reconfig_grid_module.o \
 se_init_module.o \
 se_term_module.o \
 se_modules.o \
 swap_sandia.o \
 swap_sandia_routines.o

$(LIBNAME) : $(OBJS)
	$(FC) $(FFLAGS) -c $?
	ar -crv $@ $(OBJS)
	ranlib $@
#	ar -crv $@ $?
#	chmod 644 $@

clean:
	rm -f *.a $(OBJS) *.mod
