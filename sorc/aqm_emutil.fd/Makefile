# Version @(#)$Id: Makefile,v 1.22 2005/07/20 19:08:06 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/emutil/Makefile,v $
# Date    $Date: 2005/07/20 19:08:06 $ 
#

 DIR      = $(usrdir)/nw$(envir)/cmaq.$(model_ver)
 LIB_LOC   = $(DIR)/lib
# #
 mydir=$(PWD)
 EDSS_EXE=Linux2_x86ifort
#
 LDIRS = -L/$(mydir)/$(EDSS_EXE)
#
 LIBS_SYS  = -laqm_smoke -laqm_fileset -laqm_edsstools -laqm_ioapi -lnetcdf
#
 LIBS_COMPILE =
#
 SMK_BIN   = $(DIR)/exec
 TOOLS_BIN = $(LIB_LOC)
#
 FILESETAPI=$(DIR)/lib/sorc/aqm_filesetapi
 EDSS_TOOLS=$(DIR)/lib/sorc/aqm_edss_tools
 ESMOKE_TOOLS=$(DIR)/lib/sorc/aqm_smoke
 EMMOD=$(DIR)/sorc/aqm_emmod.fd
#

SMK_BIN = $(mydir)/$(EDSS_EXE)
TOOLS_BIN = $(mydir)/$(EDSS_EXE)

FC = ifort
FFLAGS = -O2 -save -zero -extend_source -nbs -W0 -I$(mydir)/inc \
	 -I$(FILESETAPI)/$(EDSS_EXE) \
	 -I$(FILESETAPI)      \
	 -I$(mydir)/$(EDSS_EXE) \
	 -I$(EMMOD)/$(EDSS_EXE) \
	 -I$(ESMOKE_TOOLS)/$(EDSS_EXE) \
	 -I$(EDSS_TOOLS)/$(EDSS_EXE)
	 
#FFLAGS = -O2 -save -zero -extend_source -nbs -W0 \
        -I/ptmpp1/Li.Pan/tmp_smoke/subsys/ioapi_includes \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/filesetapi/Linux2_x86ifort \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/edss_tools/src/inc  \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/ioapi_includes \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/smoke/src/inc  \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/filesetapi  \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/smoke/src/emmod/Linux2_x86ifort \
	-I/ptmpp1/Li.Pan/tmp_smoke/subsys/edss_tools/src/modules/Linux2_x86ifort	 

	
	
	
INC =$(mydir)/inc
IOINC =$(mydir)/inc
ETINC =$(mydir)/inc
MD_SRC =$(mydir)
ETDSRC =$(mydir)
FS_ROOT =$(mydir)

LIBS       = -L$(LIB_LOC) $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a

##

all: aggwndw beld3to2 cemscan extractida invsplit metcombine metscan pktreduc \
     smk2emis surgtool aqm_bluesky2inv aqm_uam2ncf 
     
.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<	


ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

AGGW=aggwndw.o

aggwndw: $(AGGW) $(ALLI) $(LIBPATH)
	echo AGGWNDW:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(AGGW) $(LIBS)

B3TO2=beld3to2.o

beld3to2: $(B3TO2) $(ALLI) $(LIBPATH)
	echo BELD3TO2:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(B3TO2) $(LIBS)

BL2IV=bluesky2inv.o

aqm_bluesky2inv: $(BL2IV) $(ALLI) $(LIBPATH)
	echo BlueSky2Inv:
	$(FC) $(FFLAGS) -o ../../exec/$(@) $(BL2IV) $(LIBS)

CEM=cemscan.o

cemscan: $(CEM) $(ALLI) $(LIBPATH)
	echo CEMScan:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(CEM) $(LIBS)

EXTI=extractida.o

extractida: $(EXTI) $(ALLI) $(LIBPATH)
	echo EXTRACTIDA:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(EXTI) $(LIBS)

GEOF=geofac.o

geofac: $(GEOF) $(ALLI) $(LIBPATH)
	echo GEOFAC:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(GEOF) $(LIBS)

INVS=invsplit.o

invsplit: $(INVS) $(ALLI) $(LIBPATH)
	echo INVSPLIT:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(INVS) $(LIBS)

MCMB=metcombine.o

metcombine: $(MCMB) $(ALLI) $(LIBPATH)
	echo METCOMBINE:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MCMB) $(LIBS)

MSCN=metscan.o
#
metscan: $(MSCN) $(ALLI) $(LIBPATH)
	echo METSCAN:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MSCN) $(LIBS)
#
PKT=pktreduc.o

pktreduc: $(PKT) $(ALLI) $(LIBPATH)
	echo PKTREDUC:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(PKT) $(LIBS)

SMKE=smk2emis.o

smk2emis: $(SMKE) $(ALLI) $(LIBPATH)
	echo SMK2EMIS:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(SMKE) $(LIBS)

SURG=surgtool.o

surgtool: $(SURG) $(ALLI) $(LIBPATH)
	echo SURGTOOL:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(SURG) $(LIBS)

UAMC=uam2ncf.o

aqm_uam2ncf: $(UAMC) $(ALLI) $(LIBPATH)
	echo UAM2NCF:
	$(FC) $(FFLAGS) -o ../../exec/$(@) $(UAMC) $(LIBS)

#
## Module dependencies
#
include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

invsplit.o : $(MODINFO) 
	$(FC) $(FFLAGS) -c -o $@ invsplit.f

surgtool.o : $(MODGRID) $(MODSURG)
	$(FC) $(FFLAGS) -c -o $@ surgtool.f

smk2emis.o  : $(MODINFO)
	$(FC) $(FFLAGS) -c -o $@ smk2emis.f

clean:
	/bin/rm -f *.o
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
