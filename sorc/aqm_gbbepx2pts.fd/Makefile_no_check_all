#SHELL=/bin/sh

TARGET =  aqm_gbbepx2pts

# Compiler Definitions
 FC = ifort
 CC = cc

# Preprocessor
 FPP  = 

 LIB_LOC    = ../../lib

 IOAPI  = $(LIB_LOC)
  
 INCLUDES = -I$(IOAPI)/include -I${NETCDF_DIR}/include

# Compiler Flags
 F_FLAGS = -O3
 C_FLAGS = -D_LARGE_FILES
 LINK_FLAGS =  -qopenmp 

# Libraries
 LIBRARIES = -L$(LIB_LOC) -laqm_ioapi $(NETCDF_LDFLAGS)

# List of Object files needed for linking
 OBJECTS= aqm_gbbepx2pts.o 

.SUFFIXES: .F .f .c

$(TARGET): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@
.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.f.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.c.o:
	$(CC) -c $(INCLUDES) $(C_FLAGS) $(INCLUDES-h) $(CPP_FLAGS) $<
clean:
	-rm -f $(OBJECTS)
install:
	-cp -p $(TARGET) ../../exec/.
clobber: clean
	-rm -f $(TARGET)
