#fdir     =${fdir:-}
fdir     = /naqfc/save/${USER}
model_ver = v4.6.6
envir=dev
DIR      =${fdir}/nw$(envir)/cmaq.$(model_ver)
CMD      = aqm_cmaq2grib2
SRCS     = aqm_cmaq2grib2.f90

LIB_LOC  = $(DIR)/lib
IOAPI    = -laqm_ioapi
NETCDF   = -L/usrx/local/NetCDF/3.6.3/lib -lnetcdf

LIBS =   -L$(LIB_LOC)  $(IOAPI) \
	-L/usrx/local/NetCDF/3.6.3/lib -lnetcdf \
	-L/nwprod/lib -lg2_4 -lw3nco_4 -lmdl_4 -lbacio_4 -ljasper -lpng -lz

#
INC    = $(LIB_LOC)/sorc/aqm_ioapi/ioapi

FC     = ifort

FLAGS  = -I$(INC) 

$(CMD): $(SRCS) 
	$(FC) -o ../../exec/$(@) $(FLAGS) $(SRCS) $(LIBS) 
clean :
	rm -rf ../../exec/$(CMD)
