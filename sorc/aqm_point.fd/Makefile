# Version @(#)$Id: Makefile,v 1.15 2005/03/30 01:29:49 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/point/Makefile,v $
# Date    $Date: 2005/03/30 01:29:49 $ 
#

#usrdir     = /naqfc/save/$(USER)
#envir      = para
#model_ver  = v4.6.3
DIR        = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
LIB_LOC    = $(DIR)/lib

NCF_LIB      = /usrx/local/NetCDF/3.6.3/lib

mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

LDIRS      =-L/$(mydir)/$(EDSS_EXE)


LIBS_SYS  = -laqm_smoke -laqm_fileset -laqm_edsstools -laqm_ioapi -lnetcdf
LIBS_COMPILE =

SMK_BIN   = $(DIR)/exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=$(DIR)/lib/sorc/aqm_filesetapi
EDSS_TOOLS=$(DIR)/lib/sorc/aqm_edss_tools

FC = ifort
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

LIBS       = -L$(LIB_LOC) -L$(NCF_LIB) $(LIBS_SYS) $(LIBS_COMPILE)
LIBS_PVM      = $(LDIRS) -liocpl $(LIBS) -lpvm3 -mp $(LIBS_COMPILE)
LIBS_PVMDB    = $(LDIRS) -liocpl $(LIBS_DEBUG) -lpvm3 -mp $(LIBS_COMPILE)

LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a

#
all: aqm_laypoint aqm_elevpoint


pvm: aqm_laypoint.pvm

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

#
LAY=laypoint.o openlayout.o preplm.o plmris.o postplm.o plsprd.o \
    fire_preplm.o fire_plmris.o fire_postplm.o 

#
aqm_laypoint: $(LAY) $(ALLI) $(LIBPATH)
	echo LAYPOINT:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(LAY) $(LIBS)
#
aqm_laypoint.pvm: $(LAY) $(ALLI) $(LIBPATH)
	echo LAYPOINT \(pvm\):
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(LAY) $(LIBS_PVM)
#
ELV=elevpoint.o asgngrps.o openeout.o plumesti.o wpingstk.o mxgrpemis.o \
    rpelvcfg.o 

#
aqm_elevpoint: $(ELV) $(ALLI) $(LIBPATH)
	echo ELEVPOINT:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(ELV) $(LIBS)
#
#

include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgngrps.o: $(MODSOURC) $(MODELEV) $(MODINFO)
laypoint.o: $(MODSOURC) $(MODELEV) $(MODGRID) $(MODINFO)
mxgrpemis.o: $(MODSOURC) $(MODELEV) $(MODINFO) $(MODFILESET)
elevpoint.o: $(MODSOURC) $(MODELEV) $(MODINFO) $(MODGRID)
openeout.o: $(MODSOURC) $(MODINFO)
openlayout.o: $(MODELEV) $(MODGRID) $(MODINFO)
rpelvcfg.o: $(MODREPRT) $(MODELEV) $(MODINFO)
wpingstk.o: $(MODELEV)
#
clean:
	/bin/rm -f *.o 
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
