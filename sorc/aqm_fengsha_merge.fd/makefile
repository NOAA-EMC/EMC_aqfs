#
 TARGET = aqm_fengsha_merge 
 BASE = $(PWD)

 FC   = ${COMP}
 CC    = cc

 F_FLAGS    = -extend_source 132 -vec-report0 -nodefines -c -O2 
 C_FLAGS    = -v -g -I/opt/ibmhpc/pe1301/mpich2/intel/include64
 LINK_FLAGS = -liomp5 -lpthread

 DUST_INC = $(BASE)

 LIB_LOC    = ../../lib
 IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a
 NETCDFPATH = ${NETCDF}
 NETCDF_LIB2 = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS} 

 LIBRARIES = $(IOAPI_LIB) \
             $(NETCDF_LIB2)


 INCLUDES = \
 -DSUBST_GRID_ID= \
 -DSUBST_CONST=\"$(DUST_INC)/CONST.EXT\" \
 -DSUBST_FILES_ID=\"$(DUST_INC)/FILES_CTM.EXT\" \
 -DSUBST_IOPARMS=\"$(DUST_INC)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(DUST_INC)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(DUST_INC)/IODECL3.EXT\" \
 -DSUBST_DUSTCT=\"$(DUST_INC)/DUST_CONST.EXT\" 
 
 OBJECTS =\
 DUST_MERGE.o

.SUFFIXES: .F .f .c

$(TARGET): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o ../../exec/$@

.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
