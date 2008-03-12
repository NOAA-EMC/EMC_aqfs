#!/bin/ksh
#

OBJS1= rdgrbwt_60hyb_hrly.o

FC =	xlf
LDFLAGS =
LIBS =	-L/nwprod/lib -laqm_notcdf \
	-L/nwprod/lib -lw3_4 -lgem_4 -lip_4 -lbacio_4 -lsp_4 -lessl \
	-L/nwprod/lib -laqm_ioapi \
	-L/usr/lib -lxlsmp

CMD =	aqm_60hyb_hrly
all:	$(CMD)

$(CMD):	$(OBJS1)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS1) $(LIBS)

