

CMD      = aqm_post_maxi_CHA
SRCS     = cmaq-maxi2grib.f90 

LIB_LOC    = ../../lib
IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a
NETCDFPATH = ${NETCDF}
NETCDF_LIB = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}

IP_LIB     = ${IP_LIB4}
SP_LIB     = ${SP_LIB4}
BACIO_LIB  = ${BACIO_LIB4}
W3_LIB     = ${W3NCO_LIB4} ${W3EMC_LIB4}
TRANSUTIL  = ${TRANSUTIL_LIB4}


LIBS     = $(IOAPI_LIB) \
           $(NETCDF_LIB) \
           $(IP_LIB) \
           $(W3_LIB) \
           $(SP_LIB) \
           $(BACIO_LIB) \
           $(TRANSUTIL)

INC    = $(LIB_LOC)/sorc/aqm_ioapi/ioapi

FC     = ${COMP} 

FLAGS  = -I$(INC) 

$(CMD): $(SRCS)
	$(FC) -o ../../exec/$(@) $(FLAGS)  $(SRCS) $(LIBS) 
clean :
	rm -rf ../../exec/$(CMD)
