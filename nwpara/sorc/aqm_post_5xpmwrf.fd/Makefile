#!/bin/ksh
#
SRCSC= nameval.c sub.c
SRCS1= readdata_module.f getinfo_module.f
SRCS2= getstart_module.f

SRCS=	ADD_WMO.f PRDGEN.f CTL_RDR.f \
	w3fi63.f FNDBIT.f GET_PDS.f MAK_WMO.f PUT_GB.f w3fi71.f \
        w3fi75.f SET_O11.f SET_O14.f SET_O23.f SET_O56.f SET_TBL.f \
        SET_WMO.f EXTEND.f FILTER_SC.f FILTER_UV.f GRIB_IN.f \
        GRIB_OUT.f INTERP_PPT.f INTERP_SC.f INTERP_UV.f \
        READ_SORT_CTL.f READ_SORT_GRIB.f TILE_OUT.f notcdf_in.f \
        gribitb.f gfs_derive.f cmaq_output.f cmaq_inhouse.f \
        cmaq_mie_conc.f GET_BITS.f

OBJSC= nameval.o sub.o
OBJS1= readdata_module.o getinfo_module.o
OBJS2= getstart_module.o

OBJS=	ADD_WMO.o PRDGEN.o CTL_RDR.o \
	w3fi63.o FNDBIT.o GET_PDS.o MAK_WMO.o PUT_GB.o w3fi71.o \
        w3fi75.o SET_O11.o SET_O14.o SET_O23.o SET_O56.o SET_TBL.o \
        SET_WMO.o EXTEND.o FILTER_SC.o FILTER_UV.o GRIB_IN.o \
        GRIB_OUT.o INTERP_PPT.o INTERP_SC.o INTERP_UV.o \
        READ_SORT_CTL.o READ_SORT_GRIB.o TILE_OUT.o notcdf_in.o \
        gribitb.o gfs_derive.o cmaq_output.o cmaq_inhouse.o \
        cmaq_mie_conc.o GET_BITS.o

COMMS=  SPECIES.comm
DEPS= $(COMMS) $(INCLUDES)

# Tunable parameters
#
# FC            Name of the fortran compiling system to use
# CC            Name of the cc compiling system to use
# FCFLAGS       Flags to fortran conpiler 
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#

FC =		ncepxlf
FCFLAGS =	qfixed=80 -qstrict -O3 -WF,-D_LARGE_FILES
CC =		cc -D_LARGE_FILES
LDFLAGS =	
LIBS =          -L/nwprod/lib -laqm_notcdf \
                -L/nwprod/lib -lw3_4 -lgem_4 -lip_4 -lbacio_4 -lsp_4 -lessl \
                -L/nwprod/lib -laqm_ioapi \
                -L/usr/lib -lxlsmp


CMD =		aqm_post_5xpmwrf
PROFLIB =	-lprof

IOAPI  = /nwprod/lib/sorc/aqm_ioapi
INCLUDES = -I $(IOAPI)/ioapi/fixed_src
INCLUDES-h = -I $(IOAPI)/ioapi
#INCLUDES-net = -I $(netCDF)/include

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
# FFLAGS =         -O -q64   -qnosave  -C
FFLAGS =         -O3 -qstrict -qnosave  -C

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build prdgen.
#
all:		$(CMD)

$(CMD):		$(OBJSC) $(OBJS1) $(OBJS2) $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJSC) $(OBJS1) $(OBJS2) $(OBJS) $(LIBS)

$(OBJSC) : $(SRCSC)
	$(CC) -c $(INCLUDES-h) $<

$(OBJS1) : $(SRCS1)
	$(FC) $(FFLAGS) $(INCLUDES) $(INCLUDES-net) -c $<

$(OBJS2) : $(OBJS1) $(SRCS1)
	$(FC) $(FFLAGS) $(INCLUDES) $(INCLUDES-net) -c $<

# Make the profiled version of the command and call it prdgen.prof
#
$(CMD).prof:	$(OBJSC) $(OBJS1) $(OBJS2) $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJSC) $(OBJS1) $(OBJS2) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS) $(OBJSC) $(OBJS1) $(OBJS2)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
