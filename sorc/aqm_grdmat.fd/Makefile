# Version @(#)$Id: Makefile,v 1.14 2004/06/28 14:16:44 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/grdmat/Makefile,v $
# Date    $Date: 2004/06/28 14:16:44 $ 
#

#usrdir     = /naqfc/save/$(USER)
#envir      = para
DIR      = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
#
mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

LDIRS      =-L/$(mydir)/$(EDSS_EXE)

LIB_LOC   = $(DIR)/lib

NCF_LIB      = /usrx/local/NetCDF/3.6.3/lib

LIBS_SYS  = -laqm_smoke -laqm_fileset -laqm_edsstools -laqm_ioapi -lnetcdf
LIBS_DEBUG_SYS = -lsmoke.debug -lfileset.debug -ledsstools.debug -lioapi.debug -lnetcdf.debug	
LIBS_COMPILE =
 
SMK_BIN   = $(DIR)/exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=$(DIR)/lib/sorc/aqm_filesetapi
EDSS_TOOLS=$(DIR)/lib/sorc/aqm_edss_tools

FC        = ifort
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


LIBS       = -L$(LIB_LOC) -L$(NCF_LIB)  $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a 
#

all: aqm_grdmat

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

#
GRD= asgnsurg.o genagmat.o genmgmat.o genpgmat.o grdmat.o lnk2grd.o opengmat.o \
     sizgmat.o setfrac.o
#
aqm_grdmat: $(GRD) $(ALLI) $(LIBPATH)
	echo GRDMAT
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(GRD) $(LIBS)
#

include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgnsurg.o: $(MODSOURC) $(MODXREF) $(MODSURG) $(MODINFO) 
genagmat.o: $(MODSOURC) $(MODXREF) $(MODSURG) $(MODLISTS) $(MODINFO) $(MODGRID)
genmgmat.o: $(MODSOURC) $(MODXREF) $(MODSURG) $(MODLISTS) $(MODINFO) $(MODGRID)
grdmat.o: $(MODSOURC) $(MODXREF) $(MODINFO) $(MODGRID) $(MODFILESET)
opengmat.o: $(MODINFO)
setfrac.o: $(MODSURG)
sizgmat.o: $(MODSOURC) $(MODXREF) $(MODGRID) $(MODSURG)
#
clean:
	/bin/rm -f *.o 
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
