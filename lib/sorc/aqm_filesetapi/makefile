# Version $Id: Makefile,v 1.7 2004/06/18 15:18:27 cseppan Exp $ 
# Path    $Source: /afs/isis/depts/cep/emc/apps/archive/filesetapi/filesetapi/Makefile,v $ 
# Date    $Date: 2004/06/18 15:18:27 $

# Makefile for FileSetAPI library

mydir=./

EDSS_EXE=Linux2_x86ifort

FS_BIN = ${mydir}/${EDSS_EXE}
IOINC = ${mydir}/ioapi_includes

FS_ROOT = ${mydir}

FC = ${F_COMP} 

FFLAGS =  -O2 -save -zero -extend_source -nbs -W0 -I${IOINC} -I${FS_BIN}

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<
.F.o:
	$(FC) $(FFLAGS) -c -o $@ $<

include $(FS_ROOT)/modmake.inc

FILES = \
  appendname.o  chkfileset.o  chksetdesc.o  cleanup.o     closeset.o  \
  createset.o   descset.o     openset.o     promptset.o   readset.o   \
  writeset.o

all: lib

lib: $(FSMODALL) $(FILES)
	ar rv ../../libaqm_fileset.a $(FILES) 
	ar rv ../../libaqm_fileset.a $(FSMODALL)

debug: $(FSMODALL) $(FILES)
	ar rv $(FS_BIN)/libfileset.debug.a $(FILES) 
	ar rv $(FS_BIN)/libfileset.debug.a $(FSMODALL)
#
# Module and include dependencies
#

chkfileset.o: $(MODFILESET)
chksetdesc.o: $(MODFILESET)
cleanup.o:    $(MODFILESET)
closeset.o:   $(MODFILESET) $(IOINC)/IODECL3.EXT
createset.o:  $(MODFILESET) $(IOINC)/IODECL3.EXT
descset.o:    $(MODFILESET) $(IOINC)/IODECL3.EXT
openset.o:    $(MODFILESET) $(IOINC)/IODECL3.EXT
readset.o:    $(MODFILESET) $(IOINC)/IODECL3.EXT
writeset.o:   $(MODFILESET) $(IOINC)/IODECL3.EXT

clean: 
	/bin/rm -f *.o $(FS_BIN)/*.o $(FS_BIN)/*.mod $(FS_BIN)/*.M
	
