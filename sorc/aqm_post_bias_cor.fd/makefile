#---------------------------------------------------------------------------
#
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

Prg     =  aqm_post_bias_cor_grib2

# Library directories.

Plib    =   ../../lib/sorc/lib.bias
Lib     =   ../../lib/sorc/lib.f90

NCEPLib =   /nwprod/lib

# Command argument sequences.

NETCDFPATH = ${NETCDF}
nc1    =  -I$(NETCDFPATH)/include  ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}
nc2    =  -L/usrx/local/HDF4/4.2.8/lib
#nc2    = ${HDF4_LDFLAGS} ${HDF4_LDFLAGS_F}
nc3    = ${HDF5_LDFLAGS}
#nc4    =  ${SZ_LIB}
nc5    = -lz -lmfhdf

#setenv  LIB_NETCDF  $(nc1)

Libraries =  $(nc1) $(nc2)  $(nc3)  $(nc4) $(nc5)

F90_CMD    = ${COMP} 

F90_FLAGS1 = -g -heap-arrays -assume byterecl -assume noold_maxminloc
F90_FLAGS2 = -traceback -check all -check noarg_temp_created
F90_FLAGS3 = -warn all -warn errors
F90_FLAGS4 = -fstack-security-check -fpe-all=0
F90_FLAGS5 = -O0 -diag-disable 13003

F90_FLAGS  = $(F90_FLAGS1) $(F90_FLAGS2)  \
             $(F90_FLAGS3) $(F90_FLAGS4)  \
             $(F90_FLAGS5)

Link.f90 =	$(F90_CMD) $(F90_FLAGS)

F90_CONFIG   =  f90_config.gfortran.f90
F90_IEEE_SUP =


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
		$(Lib)/netwrite3.no-udunits.f90 $(Lib)/next_time.f90 

Lib4 =		-L/nwprod/lib/w3nco/v2.0.3  -lw3nco_v2.0.3_4 \
		-L/nwprod/lib/w3emc/v2.0.3  -lw3emc_v2.0.3_4 \
		-L/nwprod/lib -lsp_4 -lbacio_4 -lip_4 \
		-L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbacio_4 -lsp_4 -lg2_4 -ljasper -lpng -lz


Subs.lib =	$(Lib1) $(Lib2) $(Lib3) 

# Plib = Shared project support library.

Subs.plib =	$(Plib)/expand_filename.f90	$(Plib)/get_command_args.f90 \
		$(Plib)/gridlocation.f90	$(Plib)/read_netcdf_var.f90 \
		$(Plib)/read_grid_coords.f90	$(Plib)/read_gridded_aqm.f90 \
		$(Plib)/day_mon.f90 \
		$(Plib)/read_gridded_vars.f90	$(Plib)/read_table_lines.f90

# Local support routines in main program directory.

#Subs.local =	interpolate.f90			read_config_file.f90 \
#		read_station_file.f90		write_interp_netcdf.f90

# Main program.

Subs.main =	$(Subs.lib) $(Subs.plib)  

F90_CLEANUP = *.o *.mod *__genmod.f90

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	$(Lib)/*.f90 $(Plib)/*.f90

#$(Prg):	aqm.post1_bias_correct.f90 $(Subs.main) $(Hidden) makefile
$(Prg):	$(Prg).f90 $(Subs.main) $(Hidden) makefile
	$(Link.f90) $(Subs.main) $@.f90 $(Libraries) -o ../../exec/$@ $(Lib4)
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf ../../exec/$(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null
