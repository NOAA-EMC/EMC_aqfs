# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/work/rep/STENEX/src/se/Makefile,v 1.7 2000/04/26 19:08:41 yoj Exp $

mydir=/naqfc/save/${USER}
envir=para
model_ver=v4.6.3

SHELL=/bin/sh

LIBNAME = libaqm_pario.a

LIOAPI = /naqfc/save/${USER}/nw${envir}/cmaq.${model_ver}/lib/libaqm_ioapi.a

FC = mpfort
FSTD = -fixed -132 -O2  -fp-model precise 

DEBUG = -g -traceback

#FFLAGS = -132 -O3 -fp-model precise $(DEBUG)
FFLAGS = $(FSTD)  -assume byterecl   -I $(IOAPI) . 

 OBJS= \
 pinterpb_mod.o \
 piomaps_mod.o \
 alloc_data_mod.o \
 boundary.o \
 get_write_map.o \
 growbuf.o \
 gtndxhdv.o \
 initm3io.o \
 interpol.o \
 pinterpb.o \
 pio_init.o \
 pio_end.o \
 pm3err.o \
 pm3exit.o \
 pm3warn.o \
 pshut3.o \
 pwrite3.o \
 pwrgrdd.o \
 readbndy.o \
 subdmap.o \
 wrsubmap.o

.SUFFIXES: .F .f .c

$(LIBNAME) : $(OBJS)
	$(FC) $(FFLAGS) $(LIOAPI) -c $<
	ar -rv  -o ../../$@ $?
	chmod 644 ../../$@
clean:
	/bin/rm -f *.a *.o *.mod

# mv *.o *.a $M3TOOLS/pario
# $(FC) $(FFLAGS) -c $(?:.o=.f)
# $(FC) $(FFLAGS) -c $(?:.o=.f)
