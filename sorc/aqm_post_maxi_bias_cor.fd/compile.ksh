
#module unload ics
#module load ics/14.0.2 
#switch module ics/14.0.2 
cp makefile_grib1  makefile 
make clean
make
cp makefile_grib2  makefile
make clean
make

