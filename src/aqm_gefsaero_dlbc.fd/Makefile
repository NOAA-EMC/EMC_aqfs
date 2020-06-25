 
TARGET     =  aqm_gefsaero_dlbc

OBJECTS = aqm_gefsaero_dlbc.f

LIB_LOC    = ../../lib
IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a

NETCDF_LIB   = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F}
 
FC         = ifort 
 
F_FLAGS    = -O -FR -assume byterec

NEMSIO_LIB9  = ${NEMSIO_LIB} 

BACIO_LIB  = ${BACIO_LIB4}

W3_LIB     = ${W3NCO_LIBd} ${W3EMC_LIBd}

LIBRARIES = $(IOAPI_LIB) $(NETCDF_LIB) $(NEMSIO_LIB9) $(NEMSIOGFS_LIB) $(BACIO_LIB) \
	   $(W3_LIB)
 
 
INCLUDES = -I$(LIB_LOC) -I$(NEMSIO_INC) -I$(NEMSIOGFS_INC) -I$(LIB_LOC)/sorc/aqm_ioapi_v31/ioapi

$(TARGET): $(OBJECTS)
	$(FC) $(F_FLAGS) -openmp -o $@ $(INCLUDES) $(OBJECTS) $(LIBRARIES)

clean:
	rm -f *.mod  
install: 	
	-cp $(TARGET)  ../../exec/.
clobber: clean
	rm -f $(TARGET) 
