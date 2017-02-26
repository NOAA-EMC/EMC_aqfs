SHELL=/bin/sh

  MODEL = aqm_chg

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
  F_FLAGS =  -extend_source 132 -vec-report0 -nodefines -c -O2
  C_FLAGS = -D_LARGE_FILES
  LINK_FLAGS =  -liomp5 


# Libraries
LIBRARIES = \
  -L$(LIB_LOC) -laqm_ioapi \
  -L${NETCDF}/lib -lnetcdf 

# List of Object files needed for linking
OBJECTS= \
aqm_chg.o

OBJSC= my_getenv.o

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

