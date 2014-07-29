# Version @(#)$Id: Makefile,v 1.6 2003/12/02 15:38:32 cseppan Exp $
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/emmod/Makefile,v $
# Date    $Date: 2003/12/02 15:38:32 $ 

#usrdir     = /naqfc/save/$(USER)
#envir      = para
DIR      = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
#DIR        = $(usrdir)/nw$(envir)/cmaq.v4.6.3
LIB_LOC    = $(DIR)/lib
#


mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

FILESETAPI =$(DIR)/lib/sorc/aqm_filesetapi
EDSS_TOOLS =$(DIR)/lib/sorc/aqm_edss_tools


FC         = ifort
FFLAGS     = -O2 -save -zero -extend_source -nbs -W0 -I$(mydir)/inc \
	 -I$(FILESETAPI)/$(EDSS_EXE) \
	 -I$(FILESETAPI)      \
	 -I$(mydir)/$(EDSS_EXE) \
	 -I$(EDSS_TOOLS)/$(EDSS_EXE)
	 
MD_SRC =$(mydir)
INC =$(mydir)/inc
	 
.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<     

FILES =  modsourc.o modmobil.o modxref.o modtmprl.o modspro.o modcntrl.o \
         modlists.o modinfo.o moddayhr.o modsurg.o modemfac.o modstcy.o \
         modmerge.o modelev.o modmet.o modbiog.o modreprt.o modrepbn.o \
         modmbset.o modbeis3.o modar2pt.o
         

INCLUDES = $(INC)/EMPRVT3.EXT $(INC)/EMSTRG3.EXT

include $(MD_SRC)/modmake.inc

all: lib

lib: $(MODALL) Makefile

clean:
	/bin/rm -f $(MD_OUT)/*.o $(MD_OUT)/*.mod $(MD_OUT)/*.M
