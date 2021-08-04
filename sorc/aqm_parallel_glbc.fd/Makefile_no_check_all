 
TARGET     =  aqm_parallel_glbc

OBJECTS = aqm_parallel_glbc.f

LIB_LOC    = ../../lib
IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a

MPI_ROOT = $(MPICH_DIR)

NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}
 
FC         = ifort 
 
F_FLAGS    = -O -FR -assume byterec

NEMSIO_LIB9  = ${NEMSIO_LIB} 

BACIO_LIB  = ${BACIO_LIB4}

W3_LIB     = ${W3NCO_LIBd} ${W3EMC_LIBd}

LIBRARIES = $(IOAPI_LIB) $(NETCDF_LIB) $(NEMSIO_LIB9) $(NEMSIOGFS_LIB) $(BACIO_LIB) \
	   $(W3_LIB)  -L$(MPI_ROOT)/lib -lmpich
 
 
INCLUDES = -I$(LIB_LOC)/include -I$(NEMSIO_INC) -I$(NEMSIOGFS_INC) -I$(MPI_ROOT)/include

$(TARGET): $(OBJECTS)
	$(FC) $(F_FLAGS) -openmp -o $@ $(INCLUDES) $(OBJECTS) $(LIBRARIES)

clean:
	rm -f *.mod  
install: 	
	-cp -p $(TARGET)  ../../exec/.
clobber: clean
	rm -f $(TARGET) 
