#
#.........................................................................
# Version "@(#)$Header$"
# EDSS/Models-3 IOTESTS
# Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
# (C) 2003-2004 Baron Advanced Meteorological Systems, LLC.
# Distributed under the GNU GENERAL PUBLIC LICENSE version 2
# See file "GPL.txt" for conditions of use.
#.........................................................................
#
#    ASSUMPTION:  $(FC) is an F90.
#
#       ---------------     Definitions:   -------------------------

.SUFFIXES: .m4 .c .F .f

BASEDIR = IOAPI_BASE

SRCDIR  = $(BASEDIR)/iotests
IODIR   = $(BASEDIR)/ioapi
OBJDIR  = $(BASEDIR)/$(BIN)
F90DIR  = $(BASEDIR)/$(BIN)f90

# Architecture dependent stuff

MAKEINCLUDE

PVMINCLUDE


CFLAGS = -I$(PVM_ROOT)/include $(ARCHFLAGS) $(PARFLAGS) $(COPTFLAGS)

FFLAGS =  -I$(IODIR) -I. -DIOAPICPL $(FOPTFLAGS) $(ARCHFLAGS) $(PARFLAGS)

LDFLAGS = -I$(IODIR) -DIOAPICPL $(DEFINEFLAGS) $(ARCHFLAGS) $(PARFLAGS)

#  Incompatibility between netCDF versions before / after v4.1.1:
#  For netCDF v4 and later, you may also need the extra libraries
#  given by netCDF command
#
#          nc-config --libs
#
#LIBS = -L${OBJDIR} -lioapi -lnetcdf  $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)
LIBS  = -L$(OBJDIR) -lioapi -lnetcdff- -lnetcdf $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)

VPATH = $(OBJDIR)

.f.o:
	$(FC) $(FFLAGS) -c $<

.F.o:
	$(FC) $(FFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

FSRC = \
cnttest.f     cpltest.f     initqux.f     interp_test.f \
inqtest.f     readprofile.f setenv_test.f sortictest.f  \
testqux.f     writeprofile.f

CSRC = int4.c

OBJ = $(FSRC:.f=.o)$(CSRC:.c=.o)

ESRC= $(IODIR)/CONST3.EXT  $(IODIR)/FDESC3.EXT  \
      $(IODIR)/IODECL3.EXT $(IODIR)/NETCDF.EXT  \
      $(IODIR)/PARMS3.EXT

QLIB = libqux.a

EXE = \
cnttest     cpltest    int4       interp_test readprofile \
setenv_test sortictest testqux    writeprofile

all:  $(EXE)

clean:
	rm $(OBJ) $(QLIB) $(EXE)

rmexe:
	rm $(EXE)

cnttest:  cnttest.o  $(ESRC)
	$(FC) $(LDFLAGS) -o cnttest cnttest.o  $(LIBS)

cpltest:  cpltest.o  $(ESRC)
	$(FC) $(LDFLAGS) -o cpltest cpltest.o  $(LIBS)

int4:  int4.o
	$(CC) $(LDFLAGS) -o int4 int4.o

interp_test:  interp_test.o  $(ESRC)
	$(FC) $(LDFLAGS) -o interp_test interp_test.o  $(LIBS)

inqtest:  inqtest.o  $(ESRC)
	$(FC) $(LDFLAGS) -o inqtest inqtest.o  $(LIBS)

readprofile:  readprofile.o  $(ESRC)
	$(FC) $(LDFLAGS) -o readprofile readprofile.o  $(LIBS)

setenv_test:  setenv_test.o  $(ESRC)
	$(FC) $(LDFLAGS) -o setenv_test setenv_test.o  $(LIBS)

sortictest:  sortictest.o  $(ESRC)
	$(FC) $(LDFLAGS) -o sortictest sortictest.o  $(LIBS)

testqux:  testqux.o $(QLIB)
	$(FC) $(LDFLAGS) -o testqux testqux.o -L. -lqux

writeprofile:  writeprofile.o  $(ESRC)
	$(FC) $(LDFLAGS) -o writeprofile writeprofile.o  $(LIBS)

$(QLIB):  initqux.o
	ar -cr $(QLIB) initqux.o

