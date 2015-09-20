# Version @(#)$Id: Makefile,v 1.12 2004/06/21 17:16:46 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/spcmat/Makefile,v $
# Date    $Date: 2004/06/21 17:16:46 $ 
#

LIB_LOC      = ../../lib

mydir=$(PWD)
EDSS_EXE=Linux2_x86ifort

LDIRS =	-L/$(mydir)/$(EDSS_EXE)

NETCDFPATH = ${NETCDF}
NETCDF_LIB2 = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}

LIBS_SYS  = $(LIB_LOC)/libaqm_smoke.a \
            $(LIB_LOC)/libaqm_fileset.a \
            $(LIB_LOC)/libaqm_edsstools.a \
            $(LIB_LOC)/libaqm_ioapi.a \
            $(NETCDF_LIB2)

LIBS       =  $(LIBS_SYS) $(LIBS_COMPILE)

LIBS_COMPILE =
 
SMK_BIN   = ../../exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=../../lib/sorc/aqm_filesetapi
EDSS_TOOLS=../../lib/sorc/aqm_edss_tools

FC 	= ${F_COMP} 
FFLAGS 	= -O2 -save -zero -extend_source -nbs -W0 -I$(mydir)/inc \
	 -I$(FILESETAPI)/$(EDSS_EXE) \
	 -I$(FILESETAPI)      \
	 -I$(mydir)/$(EDSS_EXE) \
	 -I$(EDSS_TOOLS)/$(EDSS_EXE)
	 
INC 	=$(mydir)/inc
IOINC	=$(mydir)/inc
ETINC	=$(mydir)/inc
MD_SRC 	=$(mydir)
ETDSRC 	=$(mydir)
FS_ROOT =$(mydir)
	 
LIBS       =  $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a

#

all: aqm_spcmat

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT

#
SPC= spcmat.o opensmat.o asgnspro.o chknonhap.o
     
#
aqm_spcmat: $(SPC) $(ALLI) $(LIBPATH)
	echo SPCMAT:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(SPC) $(LIBS)
#
#
include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgnspro.o: $(MODSOURC) $(MODXREF) $(MODSPRO) $(MODINFO)
chknonhap.o: $(MODLISTS) $(MODSPRO)
opensmat.o: $(MODINFO) $(MODFILESET)
spcmat.o: $(MODSOURC) $(MODSPRO) $(MODINFO) $(MODEMFAC) $(MODFILESET) $(MODLISTS)
#
clean:
	/bin/rm -f *.o
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
