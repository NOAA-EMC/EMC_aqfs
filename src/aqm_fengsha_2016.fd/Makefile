
 TARGET     = aqm_fengsha

 BASE       = $(PWD)

 FC         = ifort #${COMP_MP}

 CC         = cc

 F_FLAGS    = -extend_source 132 -nodefines -c -O2 
# F_FLAGS    = -extend_source 132 -nodefines -c -O2 -check bounds
# F_FLAGS    = -extend_source 132 -vec-report0 -nodefines -c -O2 
 C_FLAGS    = -v -g #-I/opt/ibmhpc/pe1301/mpich2/intel/include64
 LINK_FLAGS = -liomp5 -lpthread

 DUST_INC   = $(BASE)
# LIB_LOC    = /gpfs/hps/emc/naqfc/noscrub/arl.aqm/cmaq5.0.2/lib
 LIB_LOC    = ../../lib
 IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a
 NETCDFPATH = ${NETCDF}
 NETCDF_LIB2 = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS} 

 LIBRARIES  = $(IOAPI_LIB) \
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
 OPDUST.o \
 DUST_EMIS.o \
 cropcal.o \
 TFBELOW.o \
 TFABOVE.o \
 DUST_DRIVER.o 

.SUFFIXES: .F .f .c

$(TARGET): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -openmp -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
install:
	-cp $(TARGET) ../../exec/.
clobber: clean
	-rm -f $(TARGET)
