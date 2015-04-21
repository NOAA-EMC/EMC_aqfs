 DIR       = $(fdir)/nw$(envir)/cmaq.$(model_ver)

 LIB_LOC    = $(DIR)/lib 
 LNTCDF     = /usrx/local/NetCDF/3.6.3/lib
 
 TARGET     = ngac-bnd-nemsio.met1
 
 FC         = ifort
 
 F_FLAGS    = -O -FR -assume byterec
 
 LIBRARIES = -L$(LIB_LOC) -laqm_ioapi -L$(LNTCDF) -lnetcdf -L/nwprod/lib -lnemsio_v2.2.0 -lbacio_4 -lw3emc_d  -lw3nco_d
 
 OBJECTS = ngac-bnd-nemsio.met1.f 
 
 INCLUDES = -I/nwprod/cmaq.v4.6.4/lib/sorc/aqm_ioapi/ioapi -I/nwprod/lib/incmod/nemsio_v2.2.0	

$(TARGET): $(OBJECTS)
	$(FC) $(F_FLAGS) -o ../../exec/$@ $(INCLUDES) $(OBJECTS) $(LIBRARIES)

clean:
	rm -rf ../../exec/$(TARGET)
