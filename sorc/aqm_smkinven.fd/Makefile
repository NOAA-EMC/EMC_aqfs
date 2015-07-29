# Version @(#)$Id: Makefile,v 1.22 2005/08/01 01:54:09 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/smkinven/Makefile,v $
#
LIB_LOC    = ../../lib

mydir      =$(PWD)
EDSS_EXE   =Linux2_x86ifort

LDIRS      =-L/$(mydir)/$(EDSS_EXE)

NETCDFPATH = ${NETCDF}
NETCDF_LIB = ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}


LIBS_SYS  = $(LIB_LOC)/libaqm_smoke.a \
            $(LIB_LOC)/libaqm_fileset.a \
            $(LIB_LOC)/libaqm_edsstools.a \
            $(LIB_LOC)/libaqm_ioapi.a \
            $(NETCDF_LIB)

LIBS_COMPILE =

SMK_BIN   = ../../exec
TOOLS_BIN = $(LIB_LOC)

FILESETAPI=../../lib/sorc/aqm_filesetapi
EDSS_TOOLS=../../lib/sorc/aqm_edss_tools

FC =  ${COMP}
	 
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

#
LIBS       =  $(LIBS_SYS) $(LIBS_COMPILE)
LIBPATH    = $(LIB_LOC)/libaqm_smoke.a $(LIB_LOC)/libaqm_edsstools.a


all: aqm_grwinven aqm_smkinven

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<
.F.o:
	$(FC) $(FFLAGS) -c -o $@ $<

ALLI=$(INC)/EMCNST3.EXT $(INC)/EMSTRG3.EXT
#
GRW=grwinven.o opengrwout.o srcmem.o wrinvchr.o wrinvpol.o
#
#
aqm_grwinven: $(GRW) $(ALLI) $(LIBPATH)
	echo GRWINVEN:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(GRW) $(LIBS)
#
#

RAW=    adjustinv.o asgnar2pt.o asgnnhapx.o chklstfl.o fixstk.o genpdout.o \
        gethdr.o getpdinfo.o initinfo.o \
        openinvin.o openinvout.o openpdout.o procinven.o procinvsrcs.o rdcempd.o \
        rdemspd.o rdepspd.o rdgrdapi.o \
        rdinvsrcs.o rdlooppd.o smkinven.o \
        srcmem.o wrinvchr.o wrinvemis.o wrinvpol.o wrptref.o wrpdemis.o wrepinven.o \
        setnonhap.o rdsrcidaar.o rdsrcntiar.o rddataidaar.o rdinvdata.o rddatantiar.o \
        procar2pt.o rdsrcidamb.o rdsrcntimb.o rddataidamb.o rddatantimb.o \
        rdsrcidapt.o rddataidapt.o rdsrcemsmb.o rddataemsmb.o rdsrcemsar.o \
        rddataemsar.o rdsrcemspt.o rddataemspt.o procemspt.o \
        rddevemspt.o rdfacemspt.o rdprocemspt.o rdstkemspt.o \
        rdsrcntipt.o rdsrcntinp.o rddatantipt.o rddatantinp.o rdcemsum.o
#
#
aqm_smkinven: $(RAW) $(ALLI) $(LIBPATH)
	echo SMKINVEN:
	$(FC) $(FFLAGS) -o $(SMK_BIN)/$(@) $(RAW) $(LIBS)
#
#
include $(MD_SRC)/modmake_emmod.inc
include $(ETDSRC)/modmake_edss.inc
include $(FS_ROOT)/modmake_file.inc

asgnar2pt.o: $(MODSOURC) $(MODXREF) $(MODINFO)
asgnnhapx.o: $(MODSOURC) $(MODXREF) $(MODINFO)
fixstk.o: $(MODSOURC)
genpdout.o: $(MODSOURC) $(MODLISTS) $(MODINFO) $(MODDAYHR) $(MODSTCY)
gethdr.o: $(MODLISTS) $(MODSTCY) $(MODINFO)
getpdinfo.o: $(MODINFO) $(MODDAYHR)
grwinven.o: $(MODSOURC) $(MODINFO) $(MODFILESET)
initinfo.o: $(MODINFO)
opengrwout.o: $(MODINFO)
openinvout.o: $(MODLISTS) $(MODINFO) $(MODFILESET)
openpdout.o: $(MODINFO)
procar2pt.o: $(MODSOURC) $(MODXREF) $(MODINFO) $(MODAR2PT)
procemspt.o: $(MODINFO) $(MODSOURC) $(MODLISTS)
procinven.o: $(MODSOURC) $(MODLISTS) $(MODINFO)
procinvsrcs.o: $(MODSOURC) $(MODINFO) $(MODMOBIL)
rdcempd.o: $(MODSOURC) $(MODLISTS) $(MODINFO) $(MODDAYHR)
rdcemsum.o: $(MODLISTS) $(MODDAYHR)
rddataemsar.o: $(MODINFO) $(MODLISTS)
rddataemsmb.o: $(MODINFO)
rddataemspt.o: $(MODINFO) $(MODLISTS)
rddataidaar.o: $(MODINFO)
rddataidamb.o: $(MODINFO)
rddataidapt.o: $(MODINFO)
rddatantiar.o: $(MODINFO)
rddatantimb.o: $(MODINFO)
rddatantinp.o: $(MODINFO)
rddatantipt.o: $(MODINFO)
rddevemspt.o: $(MODINFO) $(MODSOURC)
rdemspd.o: $(MODSOURC) $(MODLISTS) $(MODINFO) $(MODDAYHR) 
rdepsar.o: $(MODSOURC) $(MODLISTS) $(MODINFO)
rdepspd.o: $(MODSOURC) $(MODLISTS) $(MODSTCY) $(MODINFO) $(MODDAYHR)
rdepspt.o: $(MODSOURC) $(MODLISTS) $(MODINFO)
rdfacemspt.o: $(MODINFO) $(MODSOURC)
rdgrdapi.o: $(MODSOURC) $(MODINFO)
rdinvdata.o: $(MODSOURC) $(MODINFO) $(MODLISTS) $(MODMOBIL)
rdinvsrcs.o: $(MODSOURC) $(MODINFO) $(MODLISTS) $(MODMOBIL)
rdlooppd.o: $(MODINFO)
rdprocemspt.o: $(MODINFO) $(MODSOURC)
rdsrcemsar.o: $(MODLISTS)
rdsrcemsmb.o: $(MODINFO)
rdsrcemspt.o: $(MODLISTS)
rdsrcidaar.o:
rdsrcidamb.o: $(MODINFO)
rdsrcidapt.o:
rdsrcntiar.o: $(MODLISTS)
rdsrcntimb.o: $(MODLISTS)
rdsrcntinp.o: $(MODLISTS)
rdsrcntipt.o: $(MODLISTS)
rdstkemspt.o: $(MODINFO) $(MODSOURC)
setnonhap.o: $(MODSOURC) $(MODLISTS) $(MODXREF) $(MODINFO)
smkinven.o: $(MODSOURC) $(MODLISTS) $(MODINFO)
srcmem.o: $(MODSOURC)
wrepinven.o: $(MODSOURC) $(MODINFO) $(MODLISTS) $(MODAR2PT)
wrinvchr.o: $(MODSOURC) $(MODINFO)
wrinvemis.o: $(MODSOURC) $(MODLISTS) $(MODINFO) $(MODFILESET)
wrinvpol.o: $(MODFILESET)
wrpdemis.o: $(MODSOURC) $(MODINFO) $(MODDAYHR) 
wrptref.o: $(MODINFO)

clean:
	/bin/rm -f *.o 
	/bin/rm -f $(MODALL) $(IOMODALL) $(FSMODALL)
