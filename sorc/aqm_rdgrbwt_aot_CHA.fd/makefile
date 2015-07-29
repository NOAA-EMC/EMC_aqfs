
CMD      = aqm_rdgrbwt_aot_CHA
SRCS     = rdgrbwt_aot_CHA.f 

LIB_LOC    = ../../lib
IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a
NETCDFPATH = /usrx/local/NetCDF/3.6.3/
NETCDF_LIB = $(NETCDFPATH)/lib/libnetcdf_c++.a $(NETCDFPATH)/lib/libnetcdf.a

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
	$(FC) -o ../../exec/$(@) $(FLAGS) $(SRCS) $(LIBS) 
clean :
	rm -rf ../../exec/$(CMD)
