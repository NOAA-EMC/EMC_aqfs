# @(#)$Header: /afs/isis/depts/cep/emc/apps/archive/filesetapi/filesetapi/modmake.inc,v 1.4 2004/06/03 14:10:45 cseppan Exp $
#
# Include file for making modules and doing needed copies, etc.
#

FS_BIN = $(FS_ROOT)/$(EDSS_EXE)

MODFILESET = $(FS_BIN)/modfileset.o

#
FSMODALL   = $(MODFILESET)

# 
# Module dependencies...  

$(MODFILESET): $(FS_ROOT)/modfileset.f $(IOINC)/PARMS3.EXT $(IOINC)/FDESC3.EXT
	$(FC) $(FFLAGS) -c -o $@ $<
	if ( test -f modfileset.mod ) ; then mv modfileset.mod $(FS_BIN) ; fi
	if ( test -f modfileset.M   ) ; then mv modfileset.M   $(FS_BIN) ; fi
	if ( test -f MODFILESET.mod ) ; then mv MODFILESET.mod $(FS_BIN) ; fi
