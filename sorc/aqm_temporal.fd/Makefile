# Version @(#)$Id: Makefile,v 1.16 2004/06/21 17:16:28 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/temporal/Makefile,v $
#

#usrdir     = /naqfc/save/$(USER)
#envir      = para
#model_ver  = v4.6.3
DIR        = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
LIB_LOC    = $(DIR)/lib

NCF_LIB      = /usrx/local/NetCDF/3.6.3/lib

mydir=$(PWD)
EDSS_EXE=Linux2_x86ifort

LDIRS =	-L/$(mydir)/$(EDSS_EXE)

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
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a

#

all: aqm_temporal

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $< 

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

#
TMP= asgntpro.o genhemis.o mktmat.o opentmp.o opentmpin.o \
     setdaylt.o temporal.o tmnamunt.o updtmat.o wrtsup.o

#
aqm_temporal: $(TMP) $(ALLI) $(LIBPATH)
	echo TEMPORAL:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(TMP) $(LIBS)
#

include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgntpro.o: $(MODSOURC) $(MODXREF) $(MODTMPRL) $(MODINFO)
genhemis.o: $(MODSOURC) $(MODXREF) $(MODTMPRL) $(MODEMFAC) $(MODMET) $(MODINFO) \
            $(MODDAYHR)
mktmat.o: $(MODTMPRL) 
opentmp.o: $(MODEMFAC) $(MODINFO) $(MODFILESET)
opentmpin.o: $(MODMET) $(MODINFO) $(MODGRID)
setdaylt.o: $(MODSOURC) $(MODSTCY) $(MODINFO)
temporal.o: $(MODSOURC) $(MODXREF) $(MODTMPRL) $(MODEMFAC) $(MODMET) $(MODINFO) \
            $(MODDAYHR) $(MODLISTS) $(MODGRID) $(MODMBSET)
tmnamunt.o: $(MODEMFAC) $(MODINFO)
updtmat.o: $(MODSOURC) $(MODXREF) $(MODTMPRL) $(MODDAYHR)
wrtsup.o: $(MODSOURC) $(MODXREF) $(MODTMPRL) $(MODDAYHR)
#
clean:
	/bin/rm -f *.o
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
