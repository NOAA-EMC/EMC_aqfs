# 2023-feb-04	Original makefile version for convert_airnow_csv.f90.
#		By Dave Allured, NOAA/PSL/CIRES.

Prg =		convert_airnow_csv

F90_CMD  =      mpiifort
Lib      =	../../lib/sorc/lib.f90.airnow

LIB_NETCDF = -I${NETCDF}/include -L${NETCDF}/lib -lnetcdff

F90_FLAGS1 = -g -heap-arrays -assume byterecl -assume noold_maxminloc 
F90_FLAGS2 = -traceback -warn all,noexternal -warn errors 
F90_FLAGS3 = -fstack-security-check -fpe-all=0  
F90_FLAGS4 = -check all -no-wrap-margin

F90_FLAGS  = $(F90_FLAGS1) $(F90_FLAGS2)  \
	     $(F90_FLAGS3) $(F90_FLAGS4)  
F90_CLEANUP = *.o *.mod *__genmod.f90

Link.f90 =	$(F90_CMD) $(F90_FLAGS)

#Lib =		$(LIB_F90)
Libraries =	$(LIB_NETCDF)

Subs.lib =	$(Lib)/config.f90		$(Lib)/stdlit.f90 \
		$(Lib)/string_utils.f90		$(Lib)/date_utils.f90 \
		$(Lib)/expand_filename.f90	$(Lib)/shell-sort.f90 \
		$(Lib)/netwrite3.no-udunits.f90

Subs.local =	get_command_args.convert.f90	get_airnow_field_table.f90 \
                verify_date_time.f90 		read_airnow_csv.f90 \
                write_airnow_netcdf.f90

Subs.all =	$(Subs.lib) $(Subs.local)

Hidden =	*.f90 $(Lib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.all) $(Hidden) makefile
	$(Link.f90) $(Subs.all) $@.f90 $(Libraries) -o $@
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf $(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null

install:
	-cp $(Prg) ../../exec/.
clobber: clean
	-rm -f $(Prg)
