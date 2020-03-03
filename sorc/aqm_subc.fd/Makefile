SHELL=/bin/sh

MODEL        = aqm_setup_date_timesp

IOAPI        = ../../lib/sorc/aqm_ioapi_v31/ioapi
IOAPI_LIB    = ../../lib/libaqm_ioapi.a

NETCDF2      = ${NETCDF}
NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}

LIB =$(IOAPI_LIB) \
     $(NETCDF_LIB)


FC           = ifort -132 -O3 -D_LARGE_FILES
CC           = cc -D_LARGE_FILES -DFLDMN=1

INCLUDES     = -I $(IOAPI)
INCLUDES-h   = -I $(IOAPI)
INCLUDES-net = -I $(NETCDF2)/include

# nameval.o 

OBJS= \
subc.o \
subf.o \
setup_date_time.o

.SUFFIXES: .F .f

$(MODEL): $(OBJS)
	$(FC) $(LINK_FLAGS) $(OBJS) $(LIB) -qopenmp -o $@

.c.o:
	$(CC) $(C_FLAGS) $(CPP_FLAGS) $(INCLUDES-h) -c $<

.f.o:
	$(FC) $(F_FLAGS) $(INCLUDES) $(INCLUDES-net) -c $<

clean:
	-rm -f *.o *.mod $(OBJS)
install:
	-cp $(MODEL) ../../exec/.
clobber: clean
	-rm -f $(MODEL)
