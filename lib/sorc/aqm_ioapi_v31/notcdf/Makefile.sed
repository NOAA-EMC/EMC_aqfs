#
#.........................................................................
# Version "@(#)$Header$"
# Models-3 *not*CDF interface, to satisfy linker references to netCDF
# in environments for which calls to "libnetcdf.a" are forbidden.
# Copyright (C) 2003 Baron Advanced Meteorological Systems, LLC.
# Distributed under the GNU GENERAL LESSER PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#
#       -------------------     Definitions:   -------------------------
#
.SUFFIXES: .m4 .c .F .f
#
BASEDIR = IOAPI_BASE
INSTDIR = LIBINSTALL

SRCDIR  = $(BASEDIR)/notcdf
IODIR   = $(BASEDIR)/ioapi
OBJDIR  = $(BASEDIR)/$(BIN)
#
# Architecture dependent stuff
# Assumes FC is an f90 (otherwise, "make f77").
#
MAKEINCLUDE
#
#
#
FFLAGS = -I$(IODIR) $(DEFINEFLAGS) $(FOPTFLAGS) $(ARCHFLAGS)
#
fSRC = notcdf.f
#
OBJ = $(fSRC:.f=.o)
#
LIB = libnotcdf.a
#
MESG1 = 'To use the "notCDF" interface, you must > MANUALLY < copy'
MESG2 = '"libnotcdf.a" to "libnetcdf.a" in directory '$(BASEDIR)/$(BIN)
#
#       -------------------     Targets:   -----------------------------
#
all: $(LIB)
#
clean:  $(OBJDIR)
	cd $(SRCDIR); rm *.o *.a core* ; cd $(OBJDIR); rm $(OBJ) $(LIB)
#
install: $(INSTDIR)
	echo "Installing notCDF in $(INSTDIR)" ; cd $(OBJDIR); cp $(LIB) $(INSTDIR)
#
.f.o:  $(IODIR)/Makeinclude.$(BIN)
	cd $(OBJDIR); $(FC) -c $(FFLAGS) $(SRCDIR)/$<
#
$(LIB): $(OBJ)
	cd $(OBJDIR); $(AR) $(ARFLAGS) $(LIB) $(OBJ); echo $(MESG1); echo $(MESG2)
#


