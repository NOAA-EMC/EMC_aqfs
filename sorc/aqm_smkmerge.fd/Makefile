# Version @(#)$Id: Makefile,v 1.18 2005/05/30 22:09:33 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/smkmerge/Makefile,v $
# Date    $Date: 2005/05/30 22:09:33 $ 
#

LIB_LOC      = ../../lib

mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

LDIRS      =-L/$(mydir)/$(EDSS_EXE)

NETCDFPATH = ${NETCDF}
NETCDF_LIB2 = ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}

LIBS_SYS  = $(LIB_LOC)/libaqm_smoke.a \
            $(LIB_LOC)/libaqm_fileset.a \
            $(LIB_LOC)/libaqm_edsstools.a \
            $(LIB_LOC)/libaqm_ioapi.a \
            $(NETCDF_LIB2)

LIBS_COMPILE =

SMK_BIN   = ../../exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=../../lib/sorc/aqm_filesetapi
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
LIBS2       =  $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a

LIBS_PVM      = $(LDIRS) -liocpl $(LIBS) -lpvm3 -mp
LIBS_PVMDB    = $(LDIRS) -liocpl $(LIBS_DEBUG) -lpvm3 -mp

#

all: aqm_smkmerge aqm_mrggrid aqm_mrgelev

pvm: aqm_smkmerge.pvm

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT $(INC)/PARMS3.EXT

#
MRG= smkmerge.o allocmrg.o bldmrgidx.o getmrgev.o mrgmult.o mrgonams.o \
     mrgvnams.o openmrgin.o openmrgout.o wmrgemis.o wrmrgrep.o initstcy.o \
     rdmrginv.o mrgelev.o mrgunits.o mrgbio.o wmrgelev.o
#
aqm_smkmerge: $(MRG) $(ALLI) $(LIBPATH)
	echo SMKMERGE:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MRG) $(LIBS)
#
#
aqm_smkmerge.pvm: $(MRG) $(ALLI) $(LIBPATH)
	echo SMKMERGE \(pvm\):
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MRG) $(LIBS_PVM)
#
MGD= mrggrid.o setoutdate.o
#
aqm_mrggrid: $(MGD) $(ALLI) $(LIBPATH)
	echo MRGGRID:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MGD) $(LIBS2)
#
MGE= mrgasciielev.o setoutdate.o
#
aqm_mrgelev: $(MGE) $(ALLI) $(LIBPATH)
	echo MRGELEV:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(MGE) $(LIBS2)
#
#
## Module dependencies
#
include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

allocmrg.o  : $(MODMERGE) $(MODCNTRL) $(MODSTCY) $(MODELEV) $(MODGRID) 
bldmrgidx.o : $(MODMERGE)
getmrgev.o  : $(MODMERGE)
initstcy.o  : $(MODMERGE) $(MODSTCY)
mrgelev.o   : $(MODMERGE) $(MODCNTL) $(MODELEV)
mrggrid.o   : $(MODGRID)
mrgmult.o   : $(MODMERGE) $(MODSTCY) $(MODELEV)
mrgonams.o  : $(MODMERGE)
mrgunits.o  : $(MODMERGE)
mrgvnams.o  : $(MODMERGE)
openmrgin.o : $(MODMERGE) $(MODELEV) $(MODGRID) $(MODFILESET) $(MODINFO)
openmrgout.o: $(MODMERGE) $(MODELEV) $(MODGRID) 
rdmrginv.o: $(MODMERGE)
smkmerge.o  : $(MODMERGE) $(MODCNTRL) $(MODSTCY) $(MODSURG) $(MODELEV) $(MODGRID) $(MODLISTS)
wmrgelev.o  : $(MODMERGE) $(MODSOURC) $(MODELEV) $(MODGRID) 
wmrgemis.o  : $(MODMERGE) $(MODELEV)
wrmrgrep.o  : $(MODMERGE) $(MODSTCY) $(MODGRID) 

clean:
	/bin/rm -f *.o
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
