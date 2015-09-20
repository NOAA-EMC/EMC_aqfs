
DIR        =$(PWD)
EDSS_EXE   =Linux2_x86ifort
LIB_DIR    =../../lib

FILESETAPI =$(LIB_DIR)/sorc/aqm_filesetapi
EDSS_TOOLS =$(LIB_DIR)/sorc/aqm_edss_tools


FC         = ${F_COMP} 
FFLAGS     = -O2 -save -zero -extend_source -nbs -W0 -I$(DIR)/inc \
	 -I$(FILESETAPI)/$(EDSS_EXE) \
	 -I$(FILESETAPI)      \
	 -I$(DIR)/$(EDSS_EXE) \
	 -I$(EDSS_TOOLS)/$(EDSS_EXE)
	 
MD_SRC =$(DIR)
INC =$(DIR)/inc
	 
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
