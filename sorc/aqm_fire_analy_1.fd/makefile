
 TARGET = aqm_fire_analy_1 

 BASE = $(PWD)

 FC    = ifort 
 CC    = cc

 F_FLAGS    = -extend_source 132 -nodefines -c -O3 
 C_FLAGS    = -v -g 
 LINK_FLAGS = -liomp5 -lpthread

 LIB_LOC    = ../../lib
 IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a

 NETCDFPATH = ${NETCDF}
 NETCDF_LIB = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}

 LIBRARIES = $(IOAPI_LIB) \
            $(NETCDF_LIB)

 INCLUDES = -I$(NETCDFPATH)/include -I$(LIB_LOC)/include


 OBJECTS =\
 aqm_fire_analy_1.o 

.SUFFIXES: .F .f .c

$(TARGET): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -openmp -o ../../exec/$@

.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $(INCLUDES) $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
