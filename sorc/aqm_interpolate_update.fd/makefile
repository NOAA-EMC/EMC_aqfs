#---------------------------------------------------------------------------
# 2014-may-10	Original make file for interpolate_update.f90.
#		By Dave Allured, NOAA/ESRL/PSD/CIRES.
# 2014-jul-16	Clean version for new release.
#
# This make file uses a single large compile command, rather
# than incremental compiles.  Environment variables are used to
# keep the make file generic for different compilers.
#
# Fortran source files are divided into three main groups in
# separate directories.  Order of compilation is managed with
# make macros that group and order the source files.  Within
# each bottom group,  file lists are mostly alphabetical,
# sometimes not, as needed.
#
#---------------------------------------------------------------------------

Prg    =  aqm_interpolate_update

# Library directories.

Plib   =   ../../lib/sorc/lib.bias
Lib    =   ../../lib/sorc/lib.f90

# Command argument sequences.

NETCDF =  /usrx/local/NetCDF/4.2/serial
nc1    =  -I$(NETCDF)/include -L$(NETCDF)/lib
nc2    =  -L/usrx/local/HDF4/4.2.8/lib 
nc3    =  -L/usrx/local/HDF5/1.8.9/serial/lib 
nc31   =  -L/usrx/local/NetCDF/4.2/serial/lib -lnetcdff -lnetcdf
nc4    =  -L/usrx/local/szip-2.1/lib  
nc5    =  -lz -lmfhdf -ldf -lhdf5_hl -lhdf5 -lsz -ljpeg 
nc6    =   /usrx/local/szip-2.1/lib/libsz.so.2 

#setenv  LIB_NETCDF  $(nc1)

Libraries = $(nc1) $(nc2)  $(nc3) $(nc31)  $(nc4)  $(nc5) $(nc6) 

#F90_CMD = ifort
F90_CMD = /usrx/local/intel/14.0.1/composer_xe_2013_sp1.2.144/bin/intel64/ifort 


F90_FLAGS1 = -g -heap-arrays -assume byterecl -assume noold_maxminloc 
F90_FLAGS2 = -traceback -check all -check noarg_temp_created
F90_FLAGS3 = -warn all -warn errors 
F90_FLAGS5 = -fstack-security-check -fpe-all=0 
F90_FLAGS1 = -O0


#Libraries  = 	
Link.f90   =	$(F90_CMD) $(F90_FLAGS)

F90_CONFIG =  f90_config.gfortran.f90

# Lib = General fortran support library.

Lib1 =		$(Lib)/$(F90_CONFIG)		$(Lib)/config.f90 \
		$(Lib)/stdlit.f90

Lib2 =		$(Lib)/get_free_unit.f90	$(Lib)/get_param_string.f90 \
		$(Lib)/leap_check.f90		$(Lib)/lowercase.f90 \
		$(Lib)/message_nolog.f90	$(Lib)/parse_delimited.f90 \
		$(Lib)/parse_varexp.f90		$(Lib)/read_line.f90 \
		$(Lib)/real_to_compact_string.f90 \
		$(Lib)/resolve_env.f90		$(Lib)/string_utils.f90

Lib3 =		$(Lib)/date_index.f90		$(Lib)/index_to_date.f90 \
		$(Lib)/netwrite3.no-udunits.f90

Subs.lib =	$(Lib1) $(Lib2) $(Lib3)

# Plib = Shared project support library.

Subs.plib =	$(Plib)/expand_filename.f90	$(Plib)/get_command_args.f90 \
		$(Plib)/gridlocation.f90	$(Plib)/read_netcdf_var.f90 \
		$(Plib)/read_grid_coords.f90	$(Plib)/read_gridded_aqm.f90 \
		$(Plib)/read_gridded_vars.f90	$(Plib)/read_table_lines.f90

# Local support routines in main program directory.

Subs.local =	interpolate.f90			read_config_file.f90 \
		read_station_file.f90		write_interp_netcdf.f90

# Main program.

Subs.main =	$(Subs.lib) $(Subs.plib) $(Subs.local)

F90_CLEANUP = *.o *.mod *__genmod.f90

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	$(Lib)/*.f90 $(Plib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.main) $(Hidden) makefile
	$(Link.f90) $(Subs.main) $@.f90 $(Libraries) -o ../../exec/$@
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf ../../exec/$(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null
