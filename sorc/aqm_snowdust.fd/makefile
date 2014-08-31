 usrdir     = /naqfc/save/$(USER)
 envir      = para
 DIR       = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
# DIR        = $(usrdir)/nw$(envir)/cmaq.v4.6.3

 LIB_LOC    = $(DIR)/lib
 LNTCDF     = /usrx/local/NetCDF/3.6.3/lib

 TARGET = aqm_snowdust

 BASE = $(PWD)

 FC    = /usr/bin/mpfort
 CC    = cc

 F_FLAGS    = -extend_source 132 -vec-report0 -nodefines -c -O2 
 C_FLAGS    = -v -g -I/opt/ibmhpc/pe1301/mpich2/intel/include64
 LINK_FLAGS = -liomp5 -lpthread

 DUST_INC = $(BASE)
 LIBRARIES = -L$(LIB_LOC) -laqm_ioapi -L$(LNTCDF) -lnetcdf

 INCLUDES = \
 -DSUBST_GRID_ID= \
 -DSUBST_IOPARMS=\"$(DUST_INC)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(DUST_INC)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(DUST_INC)/IODECL3.EXT\"


 OBJECTS =\
 DUSTSNOW.o 

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
