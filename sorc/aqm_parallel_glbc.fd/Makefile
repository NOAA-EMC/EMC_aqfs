 
TARGET     = aqm_parallel_glbc

OBJECTS    = aqm_parallel_glbc.f

LIB_LOC    = ../../lib
IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a

NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}
 
FC         = mpiifort 
 
#F_FLAGS    = -O3 -FR -assume byterec -traceback -g
F_FLAGS    = -O3 -FR -assume byterec

NEMSIO_LIB9  = ${NEMSIO_LIB} 

BACIO_LIB  = ${BACIO_LIB4}

W3_LIB     = ${W3NCO_LIBd} ${W3EMC_LIBd}

LIBRARIES = $(IOAPI_LIB) $(NETCDF_LIB) ${NEMSIO_LIB} ${NEMSIOGFS_LIB} $(BACIO_LIB) \
	   $(W3_LIB)  -L${MPICH_DIR}/lib -lmpifort 
 
 
INCLUDES = -I$(LIB_LOC)/include -I${NEMSIO_INC} -I${NEMSIOGFS_INC} \
	  -I${W3_INC} -I${BACIO_INC4} -I${MPICH_DIR}/include 

$(TARGET): $(OBJECTS)
	$(FC) $(F_FLAGS) -qopenmp -o $@ $(INCLUDES) $(OBJECTS) $(LIBRARIES)

clean:
	rm -f *.mod  
install: 	
	-cp -p $(TARGET)  ../../exec/.
clobber: clean
	rm -f $(TARGET) 
