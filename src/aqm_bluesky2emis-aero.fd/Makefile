SHELL=/bin/sh

  MODEL = aqm_bluesky2emis-aero

# Compiler Definitions
FC = ifort
CC = cc

# Preprocessor
FPP  = 

  LIB_LOC    = ../../lib

  IOAPI  = $(LIB_LOC)/sorc/aqm_ioapi_v31
  
  NETCDFPATH = ${NETCDF}
  
  INCLUDES = -I $(IOAPI)/ioapi/fixed_src -I ${NETCDF}/include -I.

# Compiler Flags
  F_FLAGS = -O3
  C_FLAGS = -D_LARGE_FILES
  LINK_FLAGS =  -openmp 


# Libraries
LIBRARIES = \
  -L$(LIB_LOC) -laqm_ioapi \
  -L${NETCDF}/lib -lnetcdf 

# List of Object files needed for linking
OBJECTS= bluesky2emis-aero.o 

.SUFFIXES: .F .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o ../../exec/$@

.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.f.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.c.o:
	$(CC) -c $(INCLUDES) $(C_FLAGS) $(INCLUDES-h) $(CPP_FLAGS) $<

clean:
	-rm -f $(OBJECTS)

