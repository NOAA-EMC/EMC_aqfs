
CMD         = aqm_cmaq2grib2_v1
SRCS        = aqm_cmaq2grib2_v1.f90

LIB_LOC     = ../../lib
IOAPI_LIB   = ../../lib/libaqm_ioapi.a

NETCDF_LIB  = ${NETCDF_LDFLAGS} ${NETCDF_LDFLAGS_F} 
W3_LIB      = ${W3NCO_LIB4} ${W3EMC_LIB4}
G2_LIB      = ${G2_LIB4}
PNG_LIB2    = ${PNG_LIB}
BACIO_LIB   = ${BACIO_LIB4}
JASPER_LIB2 = ${JASPER_LIB}
Z_LIB2      = ${Z_LIB}
#MDL_LIB2    = ${MDL_LIB4}
MDL_LIB2    = 


LIBS   = $(IOAPI_LIB) \
 	 $(NETCDF_LIB)  \
         $(G2_LIB) \
         $(W3_LIB) \
         $(PNG_LIB2) \
         $(BACIO_LIB) \
	 $(JASPER_LIB2) \
	 $(Z_LIB2) \
	 $(MDL_LIB2)

INC    = $(LIB_LOC)/sorc/aqm_ioapi/ioapi

FC     = ${COMP} 

FLAGS  = -I$(INC) 

$(CMD): $(SRCS) 
	$(FC) -o ../../exec/$(@) $(FLAGS) $(SRCS) $(LIBS) 
clean :
	rm -rf ../../exec/$(CMD)
