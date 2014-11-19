# envir      = test
# fdir       = ${fdir:-}
 DIR        = ${fdir}/nw$(envir)/cmaq.$(model_ver)
#
 LIB_LOC    = $(DIR)/lib
 LNTCDF     = /usrx/local/NetCDF/3.6.3/lib
#
# TARGET = dust_merge.exe
 TARGET = aqm_fengsha_merge 
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
