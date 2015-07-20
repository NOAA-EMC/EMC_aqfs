#.........................................................................
# how to make aqm excutables.
# 1. export BASEDIR=/nwprod/lib/sorc/aqm_ioapi
# 2. export BIN=AIX
# 3. make
# 4. cd notcdf
# 5. make
# 6. /nwprod/lib/sorc/aqm_ioapi/AIX
# 7. chmod 775 *
# 8. mv libnotcdf.a /nwprod/lib/libaqm_notcdf.a
# 9. mv lib_ioapi.a /nwprod/lib/libaqm_ioapi.a
#.........................................................................
#.........................................................................
# VERSION "@(#)$Header$"
#      EDSS/Models-3 I/O API Version 3.
#.........................................................................
# COPYRIGHT
#       (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
#       (C) 2003 by Baron Advanced Meteorological Systems.
#       Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
#       See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Directories:
#       $(BASEDIR)  is the root directory for the I/O API library source,
#                   the M3Tools and M3Test source,the  HTML documentation,
#                   and the (machine/compiler/flag-specific) binary
#                   object/library/executable directories.
#       $(HTMLDIR)  is the web documentation
#       $(IODIR)    is the I/O API library source
#       $(TOOLDIR)  is the "M3TOOLS" source
#       $(TESTDIR)  is the "IOTESTS" source and binaries
#       $(OBJDIR)   is the current machine/compiler/flag-specific
#       object/library/executable directory
#.........................................................................
# Note On Library Versions:
#       Environment variable "BIN" specifies library version up to
#       link- and compile-flag compatibility.  Dependecies upon machine,
#       OS, and compiler are found in file "Makeinclude.$(BIN).
#       IN PARTICULAR, pay attention to the notes for various versions
#       that may be built for Linux x86 with the Portland Group
#       compilers:  see comments in $(IODIR)/Makeinclude.Linux2_x86pg
#.........................................................................
# Special Make-targets
#       all:      OBJDIR, FIXDIR, libioapi.a, and executables, with
#                 coupling mode enabled
#       nocpl:    OBJDIR, FIXDIR, libioapi.a, and executables, with
#                 coupling mode disabled
#       f77:      OBJDIR, FIXDIR, libioapi.a, and F77-based executables
#       nocpl77:  OBJDIR, FIXDIR, libioapi.a, and F77-based executables,
#                 with coupling mode disabled
#       lib:      OBJDIR, FIXDIR, libioapi.a
#       clean:    remove .o's, libioapi.a, and executables from OBJDIR
#       dirs:     make OBJDIR and FIXDIR directories
#       fix:      FIXDIR and extended-fixed-source INCLUDE-files
#       gtar:     GZipped tar-file of the source and docs
#       nametest: test of name-mangling compatibility (requires that
#                 libnetcdf.a be manually placed into $(OBJDIR))
#
######################################################################

BASEDIR=$(PWD)

IODIR   = $(BASEDIR)/ioapi
FIXDIR  = $(IODIR)/fixed_src
HTMLDIR = $(BASEDIR)/HTML
TESTDIR = $(BASEDIR)/iotests
TOOLDIR = $(BASEDIR)/m3tools
NOTDIR  = $(BASEDIR)/notcdf
OBJDIR  = $(BASEDIR)/$(BIN)
#
#      ----------------------   TOP-LEVEL TARGETS:   ------------------
#
all:  dirs fix
	(cd $(IODIR)   ; make all)
	(cd $(TOOLDIR) ; make all)
	(cd $(TESTDIR) ; make all)

nocpl:  dirs fix
	(cd $(IODIR)   ; make -f Makefile.nocpl all)
# (cd $(TOOLDIR) ; make -f Makefile.nocpl all)
	(cd $(NOTDIR) ; make -f Makefile all)
##  (cd $(TESTDIR) ; make -f Makefile.nocpl all)

clean:
	(cd $(IODIR)   ; make clean)
	(cd $(TOOLDIR) ; make clean)
	(cd $(TESTDIR) ; make clean)

dirs: $(OBJDIR) $(FIXDIR)

f77:  dirs fix
	(cd $(IODIR)   ; make all)
	(cd $(TOOLDIR) ; make f77)

nocpl77:  dirs fix
	(cd $(IODIR)   ; make -f Makefile.nocpl all)
	(cd $(TOOLDIR) ; make -f Makefile.nocpl f77)

fix:  dirs
	(cd $(IODIR)   ; make fixed_src)

gtar:
	cd $(BASEDIR); \
gtar cvfz ioapi_3.tar.gz --dereference -X $(BASEDIR)/exclude \
Makefile ioapi HTML m3tools iotests notcdf

lib:  dirs
	(cd $(IODIR)   ; make all)

not:  dirs
	(cd $(NOTDIR)   ; make all)

nametest:  lib $(OBJDIR)/libnetcdf.a

$(FIXDIR):
	mkdir $(FIXDIR)

$(OBJDIR):
	mkdir $(OBJDIR)


