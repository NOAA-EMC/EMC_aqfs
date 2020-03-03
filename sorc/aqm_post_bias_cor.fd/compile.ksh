
#module switch ics/14.0.1
cp makefile_grib1  makefile 
make clean
make
cp makefile_grib2  makefile
make clean
make

