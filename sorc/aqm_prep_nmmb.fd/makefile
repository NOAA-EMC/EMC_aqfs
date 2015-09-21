SHELL=/bin/sh
#
#
SRCS=	ADD_WMO.f PRDGEN.f CTL_RDR.f \
	FNDBIT.f GET_PDS.f MAK_WMO.f PUT_GB.f \
	SET_O11.f SET_O14.f SET_O23.f SET_O56.f SET_TBL.f SET_WMO.f \
        EXTEND.f FILTER_SC.f FILTER_UV.f GRIB_IN.f GRIB_OUT.f INTERP_PPT.f \
        INTERP_SC.f INTERP_UV.f READ_SORT_CTL.f READ_SORT_GRIB.f TILE_OUT.f

OBJS=	ADD_WMO.o PRDGEN.o CTL_RDR.o \
	FNDBIT.o GET_PDS.o MAK_WMO.o PUT_GB.o \
	SET_O11.o SET_O14.o SET_O23.o SET_O56.o SET_TBL.o SET_WMO.o \
        EXTEND.o FILTER_SC.o FILTER_UV.o GRIB_IN.o GRIB_OUT.o INTERP_PPT.o \
        INTERP_SC.o INTERP_UV.o READ_SORT_CTL.o READ_SORT_GRIB.o TILE_OUT.o

# Tunable parameters
FC        =	${F_COMP} -openmp -convert big_endian
LDFLAGS =	    
IP_LIB     = ${IP_LIB4}
SP_LIB     = ${SP_LIB4}
BACIO_LIB  = ${BACIO_LIB4}
W3_LIB     = ${W3NCO_LIB4} ${W3EMC_LIB4}
TRANSUTIL  = ${TRANSUTIL_LIB4} 


LIBS =  $(IP_LIB) \
        $(W3_LIB) \
        $(SP_LIB) \
        $(BACIO_LIB) \
        $(TRANSUTIL)


CMD =		aqm_prep_nmmb

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =         -O2  #  -qnosave  -C

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build prdgen.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o ../../exec/$(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it prdgen.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f ../../exec/$(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
