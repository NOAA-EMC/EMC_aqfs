# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/work/rep/STENEX/src/se/Makefile,v 1.7 2000/04/26 19:08:41 yoj Exp $

SHELL       = /bin/sh

LIBNAME     = libaqm_pario.a

LIOAPI      = ../../libaqm_ioapi.a

MPI_INC    = ${CRAY_MPICH2_DIR}/include
IOAPI_INC1 = ../../sorc/aqm_ioapi_v31/Linux3_x86_64intel 
IOAPI_INC2 = ../../sorc/aqm_ioapi_v31/ioapi/fixed_src

FC         = ftn
FSTD       = -fixed -132 -O3 -override-limits -fno-alias -mp1 -fp-model precise 


#DEBUG      = -g -traceback
DEBUG      = 

FFLAGS = $(FSTD)  -assume byterecl   -I $(IOAPI_INC1) -I $(IOAPI_INC2) -I $(MPI_INC) 

 OBJS= \
  pinterpb_mod.o\
  piomaps_mod.o\
  alloc_data_mod.o\
  boundary.o\
  get_write_map.o\
  growbuf.o\
  gtndxhdv.o\
  interpol.o\
  pinterpb.o\
  pio_init.o\
  pio_re_init.o\
  pm3err.o\
  pm3exit.o\
  pm3warn.o\
  pshut3.o\
  pwrite3.o\
  ptrwrite3.o\
  pwrgrdd.o\
  readbndy.o\
  subdmap.o\
  wrsubmap.o\
  parutilio.o

.SUFFIXES: .F .f .c

$(LIBNAME) : $(OBJS)
	$(FC) $(FFLAGS) -c $<
	ar -rv  -o ../../$@ $?
	chmod 644 ../../$@
clean:
	/bin/rm -f *.a *.o *.mod
