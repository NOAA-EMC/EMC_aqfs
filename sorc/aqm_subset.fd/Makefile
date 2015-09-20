SHELL=/bin/sh

MODEL      = aqm_subset_x

# Compiler Definitions
FC         = ${F_COMP} 
CC         = cc

# Preprocessor
FPP         = 

IOAPI_LIB   =../../lib/libaqm_ioapi.a 

NETCDF2      = $(NETCDF)
NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}

INCLUDES    = -I ../../lib/sorc/aqm_ioapi/ioapi -I $(NETCDF2)/include -I.

# Compiler Flags
F_FLAGS = -132 -O3 -free -D_LARGE_FILES
C_FLAGS = -D_LARGE_FILES
LINK_FLAGS = 

CPP_FLAGS = \
 -D_LARGE_FILES

# Libraries
LIBRARIES =  $(IOAPI_LIB) \
     $(NETCDF_LIB)

# List of Object files needed for linking
OBJECTS= \
check_mismatch.o \
data_module.o \
setup_module.o \
extract_module.o \
subset.o

.SUFFIXES: .F .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o ../../exec/$@

.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.f.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.c.o:
	$(CC) -c $(INCLUDES) $(C_FLAGS) $(INCLUDES-h) $(CPP_FLAGS) $<

ext_subset_list.o: 
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) 

clean:
	-rm -f *.mod $(OBJECTS) ../../exec/$@

