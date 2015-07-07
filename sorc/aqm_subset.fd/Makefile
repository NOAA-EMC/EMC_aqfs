SHELL=/bin/sh

MODEL      = aqm_subset_x

# Compiler Definitions
FC         = ifort
CC         = cc

# Preprocessor
FPP         = 

IOAPI_LIB   = ../../lib 
netCDF      = /usrx/local/NetCDF/3.6.3

NCF_LIB     = $(netCDF)/lib

INCLUDES = -I ../../lib/sorc/aqm_ioapi/ioapi -I $(netCDF)/include -I.

# Compiler Flags
F_FLAGS = -132 -O3 -free -D_LARGE_FILES
C_FLAGS = -D_LARGE_FILES
LINK_FLAGS = 

CPP_FLAGS = \
 -D_LARGE_FILES

# Libraries
LIBRARIES = \
  -L$(IOAPI_LIB) -laqm_ioapi \
  -L$(NCF_LIB) -lnetcdf 

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

