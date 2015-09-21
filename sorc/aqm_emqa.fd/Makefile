
LIB_LOC    = ../../lib

mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

LDIRS      =-L/$(mydir)/$(EDSS_EXE)

NETCDFPATH = ${NETCDF}
NETCDF_LIB2 = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}

LIBS_SYS  = $(LIB_LOC)/libaqm_smoke.a \
            $(LIB_LOC)/libaqm_fileset.a \
            $(LIB_LOC)/libaqm_edsstools.a \
            $(LIB_LOC)/libaqm_ioapi.a \
            $(NETCDF_LIB2)

LIBS_COMPILE =

SMK_BIN   = ../../exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=../..//lib/sorc/aqm_filesetapi
EDSS_TOOLS=../../lib/sorc/aqm_edss_tools

FC = ${F_COMP} 
FFLAGS = -O2 -save -zero -extend_source -nbs -W0 -I$(mydir)/inc \
	 -I$(FILESETAPI)/$(EDSS_EXE) \
	 -I$(FILESETAPI)      \
	 -I$(mydir)/$(EDSS_EXE) \
	 -I$(EDSS_TOOLS)/$(EDSS_EXE)
	 
INC =$(mydir)/inc
IOINC =$(mydir)/inc
ETINC =$(mydir)/inc
MD_SRC =$(mydir)
ETDSRC =$(mydir)
FS_ROOT =$(mydir)


LIBS       =  $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a


all: aqm_smkreport
#
.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<	

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

SMR=smkreport.o asgnbins.o bldrepidx.o genrprt.o openrepin.o   \
    openrepout.o qarepin.o rdgrps.o rdrepin.o rdrprts.o repmrggrd.o repunits.o\
    scanrepc.o selectsrc.o wrrephdr.o wrrepout.o

aqm_smkreport: $(SMR) $(ALLI) $(LIBPATH)
	echo SMKREPORT:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(SMR) $(LIBS)

#
#
include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgnbins.o: $(MODSOURC) $(MODLISTS) $(MODREPRT) $(MODREPBN) $(MODELEV) \
            $(MODSTCY) $(MODINFO) $(MODGRID)
bldrepidx.o: $(MODREPRT) $(MODREPBN) $(MODTMPRL) $(MODINFO) $(MODFILESET) $(MODCNTRL)
genrprt.o: $(MODSOURC) $(MODREPRT) $(MODREPBN) $(MODCNTRL) $(MODINFO) $(MODSTCY) 
openrepin.o: $(MODREPRT) $(MODREPBN) $(MODTMPRL) $(MODINFO) $(MODGRID) $(MODFILESET) $(MODCNTRL)
qarepin.o: $(MODREPRT) $(MODSTCY) $(MODINFO)
rdgrps.o: $(MODSOURC)  $(MODREPRT) $(MODREPBN) $(MODLISTS) $(MODGRID)
rdrepin.o: $(MODSOURC)  $(MODREPRT) $(MODREPBN) $(MODELEV) $(MODLISTS) \
           $(MODINFO) $(MODGRID) $(MODFILESET) $(MODCNTRL)
rdrprts.o: $(MODREPRT) $(MODINFO)
repmrggrd.o: $(MODSOURC) $(MODREPRT) $(MODREPBN) $(MODINFO) 
repunits.o: $(MODREPRT) $(MODREPBN)  $(MODTMPRL) $(MODINFO) 
scanrepc.o: $(MODREPRT) $(MODINFO) 
selectsrc.o: $(MODSOURC) $(MODREPRT) $(MODREPBN) $(MODINFO) 
smkreport.o: $(MODREPRT) $(MODREPBN) $(MODINFO) $(MODGRID)
wrrephdr.o: $(MODSOURC) $(MODLISTS) $(MODREPRT) $(MODREPBN) $(MODSTCY) \
            $(MODINFO) $(MODGRID)
wrrepout.o: $(MODSOURC) $(MODLISTS) $(MODREPRT) $(MODREPBN) $(MODSTCY) \
            $(MODINFO) 
#
#
clean:
	/bin/rm -f *.o
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
