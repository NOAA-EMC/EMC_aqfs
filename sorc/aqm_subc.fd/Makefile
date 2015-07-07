SHELL=/bin/sh

MODEL        = aqm_setup_date_timesp

IOAPI        = ../../lib/sorc/aqm_ioapi/ioapi
netCDF       = /usrx/local/NetCDF/3.6.3
IOAPI_LIB    = ../../lib

LIB          = -L$(IOAPI_LIB) -laqm_ioapi -L$(netCDF)/lib -lnetcdf

FC           = ifort -132 -O3 -D_LARGE_FILES
CC           = icc -D_LARGE_FILES -DFLDMN=1

INCLUDES     = -I $(IOAPI)
INCLUDES-h   = -I $(IOAPI)
INCLUDES-net = -I $(netCDF)/include

# nameval.o 

OBJS= \
subc.o \
subf.o \
setup_date_time.o

.SUFFIXES: .F .f

$(MODEL): $(OBJS)
	$(FC) $(LINK_FLAGS) $(OBJS) $(LIB) -o ../../exec/$@

.c.o:
	$(CC) $(C_FLAGS) $(CPP_FLAGS) $(INCLUDES-h) -c $<

.f.o:
	$(FC) $(F_FLAGS) $(INCLUDES) $(INCLUDES-net) -c $<

clean:
	-rm -f *.o *.mod $(OBJS)

