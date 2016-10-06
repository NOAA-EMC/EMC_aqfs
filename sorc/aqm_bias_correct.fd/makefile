#---------------------------------------------------------------------------
#
# 2014-jul-16	Make file for bias_correct main program.
#		By Dave Allured, NOAA/ESRL/PSD/CIRES.
#		Clean version for release.
# 2015-oct-29	Add support routines for bias threshold filter.
#
# 2016-jan-12	Upgrade to Dave's current fortran library structure.
# 2016-jan-20	Fix name conflict.  Change spread.f90 to spreading.f90.
# 2016-feb-04	Add anenmean_method and kfan_method.
# 2016-feb-05	Add $F90_PARALLEL, instead of compiler specific options.
# 2016-feb-08	Add get_param_int, get_param_yesno, write_test_files.
# 2016-feb-09	Update for get_param_module.
# 2016-feb-10	Split write_test_files into two routines.
# 2016-feb-15	Replace kf_analog.f90 with simplified x_analog.f90.
# 2016-feb-24	Remove $F90_PARALLEL, it is problematical.  Control parallel
#		  and optimizations exclusively through $F90_FLAGS.
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

Prg =		aqm_bias_correct

# Library directories.

#Plib =		$(LIB_BIAS)
#Lib =		$(LIB_F90)
Plib     =   ../../lib/sorc/lib.bias
Lib      =   ../../lib/sorc/lib.f90
#


#LIB_NETCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdff
LIB_NETCDF = -I$(NETCDF)/include ${NETCDF_LDFLAGS} 

F90_CMD = ifort 

LIB_BUFR = ${BUFR_LIB4}

#F90_FLAGS1 = -g -heap-arrays -assume byterecl -assume noold_maxminloc -assume realloc_lhs
F90_FLAGS1 = -g -heap-arrays -assume byterecl -assume noold_maxminloc 
F90_FLAGS2 = -traceback -warn all -warn errors 
F90_FLAGS3 = -fstack-security-check -fpe-all=0
F90_FLAGS4 = -O0
#F90_FLAGS5 = -openmp 
F90_FLAGS5 = -qopenmp

F90_FLAGS  = $(F90_FLAGS1) $(F90_FLAGS2)  \
             $(F90_FLAGS3) $(F90_FLAGS4)  \
             $(F90_FLAGS5)

F03_ALLOCATE = -assume realloc_lhs
F90_CLEANUP = *.o *.mod *__genmod.f90
F90_CONFIG = f90_config.gfortran.f90
F90_IEEE_SUP = 

#
# Command argument sequences.

Libraries =	$(LIB_BUFR) $(LIB_NETCDF)
Link.f90 =	$(F90_CMD) $(F90_FLAGS) $(F03_ALLOCATE)

# Lib = General fortran support library.

Lib1 =		$(Lib)/$(F90_CONFIG)		$(Lib)/config.f90 \
		$(F90_IEEE_SUP)			$(Lib)/parse_varexp.f90 \
		$(Lib)/stdlit.f90		$(Lib)/string_utils.f90

Lib2 =		$(Lib)/date_utils.f90		$(Lib)/distance_btw_coords.f90 \
		$(Lib)/get_free_unit.f90	$(Lib)/get_param_module.f90 \
		$(Lib)/message_nolog.f90	$(Lib)/netwrite3.no-udunits.f90\
                $(Lib)/read_line.f90		$(Lib)/shell-sort.f90

Subs.lib =	$(Lib1) $(Lib2)

# Plib = Shared project support library.

Subs.plib =	$(Plib)/bufrlib_interface.f90	$(Plib)/expand_filename.f90 \
		$(Plib)/get_command_args.f90	$(Plib)/gridlocation.f90 \
		$(Plib)/read_bufr_var.f90	$(Plib)/read_netcdf_var.f90 \
		$(Plib)/read_grid_coords.f90	$(Plib)/read_gridded_aqm.f90 \
		$(Plib)/read_gridded_vars.f90	$(Plib)/read_table_lines.f90 \
		$(Plib)/shell-sort.descending.f90 \
		$(Plib)/std_dev.f90

# Local support routines in main program directory.

Analog1 =	read_exception_list.f90		stdevTNcirc.f90 \
		wind_dir_error.f90

Analog2 =	find_analog_v1.f90		kf_luca.f90 \
		x_analog.f90			anenmean_method.f90 \
		kfan_method.f90			main_analog.f90

Local1 =	align_obs_to_forecasts.f90	grid_distances.f90 \
		qc_single_site.f90		read_config_file_main.f90 \
		read_interp_forecasts.f90	read_obs_series.f90 \
		read_obs_qc.f90			spread_bias.f90 \
		write_corrected_netcdf.f90	write_site_list.f90 \
		write_test_day_files.f90	write_test_site_files.f90

Local2 =	spreading.f90

Subs.local =	$(Analog1) $(Analog2) $(Local1) $(Local2)

# Main program.

Subs.main =	$(Subs.lib) $(Subs.plib) $(Subs.local)

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	$(Lib)/*.f90 $(Plib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.main) $(Hidden) makefile
	$(Link.f90) $(Subs.main) $@.f90 $(Libraries) -o ../../exec/$@
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf $(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null
