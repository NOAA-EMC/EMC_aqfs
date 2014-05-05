envir    =prod
model_ver=v4.6.0
#EMC
#usrdir   =/naqfc/save/${USER}

# NCO
usrdir=

DIR      =$(usrdir)/nw$(envir)/cmaq.$(model_ver)
#CMD      = aqm_cmaq_maxi2grib_CHA 
CMD      = aqm_post_maxi_CHA
SRCS     = cmaq-maxi2grib.f90 

LIB_LOC  = $(DIR)/lib
IOAPI    = -laqm_ioapi
NETCDF   = -L/usrx/local/NetCDF/3.6.3/lib -lnetcdf

LIBS =   -L$(LIB_LOC)  $(IOAPI) \
	 -L/usrx/local/NetCDF/3.6.3/lib -lnetcdf \
	 -L/nwprod/lib -lip_4 \
         -L/nwprod/lib -lw3nco_v2.0.3_4 -lw3emc_v2.0.3_4 \
         -L/nwprod/lib -lsp_4 -lbacio_4 -ltransutil_4

INC    = $(LIB_LOC)/sorc/aqm_ioapi/ioapi

FC     = ifort

FLAGS  = -I$(INC) 

$(CMD): $(SRCS)
	$(FC) -o $(@) $(FLAGS)  $(SRCS) $(LIBS) 
clean :
	rm -rf $(CMD)
