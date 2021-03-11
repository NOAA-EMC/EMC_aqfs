#   Makefile generated for serial execution using program bldmake
#
#   Generation date [04/03/2020  12:48:22]
#   Configuration file [/u/Youhua.Tang/noscrub/CMAQ/CMAQ_5.3.1/POST/combine/scripts/BLD_combine_v531_intel/combine_v531.cfg]
#   Using GIT repository [/u/Youhua.Tang/noscrub/CMAQ/CMAQ_5.3.1/POST/combine]
#   Full Compiler Paths when Makefile was Built:
#       FC = /opt/cray/craype/2.3.0/bin/ftn
#       CC = /opt/cray/craype/2.3.0/bin/cc
#   Library Paths:
#      $(LIB)/ioapi/include_files -> ioapi_inc_intel
#      $(LIB)/ioapi/lib -> ioapi_lib_intel
#      $(LIB)/mpi -> mpi_lib_intel
#      $(LIB)/netcdf -> netcdf_lib_intel
#      $(LIB)/netcdff -> netcdff_lib_intel
#
#   Command-Line Options:      
#      DEBUG = TRUE or true -- turn on debug flags 
#  OR  debug = true or TRUE -- turn on debug flags 
#  Can set either variable by using the setenv command for
#  a debugging session with multiple compilations
#
#------------------------------------------------- 

 EXEC = aqm_combine_v531

 FC = ifort
 CC = cc

 LIB = ../../lib
 include_path = -I$(LIB)/include -I$(NETCDF_DIR)/include
 WARN = 
 FSTD = -O3 -fno-alias -mp1 -fp-model source -ftz -simd -align all -xHost -vec-guard-write -unroll-aggressive
 DBG  = -O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback

 ifneq (,$(filter $(debug), TRUE true True T ))
     DEBUG = TRUE
 endif

 ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -fixed -132 $(DBG) $(include_path)
     f90_FLAGS = -free $(DBG) $(include_path)
 else
     f_FLAGS   = -fixed -132 $(FSTD) $(include_path)
     f90_FLAGS = -free $(FSTD) $(include_path)
 endif

 F_FLAGS   = $(f_FLAGS)
 F90_FLAGS = $(f90_FLAGS)
 C_FLAGS   = -I.

 LINKER     = $(FC)
 LINK_FLAGS = 

 CPP = $(FC)
 cpp_flags = 

 ifneq (,$(filter $(isam), TRUE true True T ))
     CPP_FLAGS   = $(cpp_flags) -Disam
 else
     CPP_FLAGS   = $(cpp_flags)
 endif

 IOAPI  = -L$(LIB) -laqm_ioapi
 LIBRARIES = $(NETCDF_LDFLAGS) $(IOAPI) -qopenmp
SRC = \
  module_file.o \
  module_evaluator.o \
  module_specdef.o \
  combine.o \
  utils.o

OBJS = \
  $() \
  $(SRC)

.SUFFIXES: .F .f .c .F90 .f90

$(EXEC): $(OBJS)
	$(LINKER) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(F_FLAGS) $<

.F90.o:
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f90.o:
	$(FC) -c $(F90_FLAGS) $<

.c.o:
	$(CC) -c $(C_FLAGS) $<

clean:
	rm -f $(OBJS) *.mod

install:
	-cp -p $(EXEC) ../../exec

clobber: clean
	rm -f $(EXEC)
# dependencies

combine_.o:	module_specdef.o module_file.o module_evaluator.o
module_evaluator.o:	module_file.o
module_specdef.o:	module_file.o
