SHELL=/bin/sh

MODEL        = aqm_combine 

IOAPI        = ../../lib/sorc/aqm_ioapi/ioapi
IOAPI_LIB    = ../../lib/libaqm_ioapi.a

NETCDF2      = $(NETCDF)
NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}

LIB          =   $(IOAPI_LIB) \
     $(NETCDF_LIB)


FC           = ${COMP} -132 -O3 -D_LARGE_FILES
CC           = ${C_COMP} -D_LARGE_FILES -DFLDMN=1

INCLUDES     = -I $(IOAPI)
INCLUDES-h   = -I $(IOAPI)
INCLUDES-net = -I $(NETCDF2)/include

# nameval.o 

OBJS= \
combine.o

.SUFFIXES: .F .f

$(MODEL): $(OBJS)
	$(FC) $(LINK_FLAGS) $(OBJS) $(LIB) -o ../../exec/$@

.c.o:
	$(CC) $(C_FLAGS) $(CPP_FLAGS) $(INCLUDES-h) -c $<

.f.o:
	$(FC) $(F_FLAGS) $(INCLUDES) $(INCLUDES-net) -c $<

clean:
	-rm -f *.o *.mod $(OBJS)

