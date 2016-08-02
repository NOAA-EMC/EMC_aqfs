CMD         =  aqm_cmaq2grib2_v2 
SRCS        =  aqm_cmaq2grib2_v2.f90 

LIB_LOC     = ../../lib
IOAPI_LIB   = $(LIB_LOC)/libaqm_ioapi.a
NETCDF_LIB  =  ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}

W3_LIB      = ${W3NCO_LIB4} ${W3EMC_LIB4}

LIBS   =  $(IOAPI_LIB) \
          $(NETCDF_LIB) \
          ${G2_LIB4} \
	  $(W3_LIB) \
          ${PNG_LIB} \
	  ${BACIO_LIB4} \
	  ${JASPER_LIB} \
	  ${Z_LIB}

INC    = $(LIB_LOC)/sorc/aqm_ioapi_v31/ioapi

FC      = ifort 

FLAGS  = -I$(INC) 

$(CMD): $(SRCS)
	$(FC) -o ../../exec/$(@) $(FLAGS)  $(SRCS) $(LIBS) -openmp 
clean :
	rm -rf ../../exec/$(CMD)
