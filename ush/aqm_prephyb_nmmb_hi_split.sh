#!/bin/ksh 

set -ax 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]; then
 export  hr_list1="00 01"
 export  hr_list2="04 05"
 export  hr_list3="02 06"
 export  hr_list4="03"
else
 export  hr_list1="00 01 08 12 16 20 24 28 32 36 40 44 48"
 export  hr_list2="04 05 09 13 17 21 25 29 33 37 41 45"
 export  hr_list3="02 06 10 14 18 22 26 30 34 38 42 46"
 export  hr_list4="03 07 11 15 19 23 27 31 35 39 43 47"
fi

export h_seg=$1
if [ ${h_seg} -eq 1 ];  then export hr_list=$hr_list1 ; fi
if [ ${h_seg} -eq 2 ];  then export hr_list=$hr_list2 ; fi
if [ ${h_seg} -eq 3 ];  then export hr_list=$hr_list3 ; fi
if [ ${h_seg} -eq 4 ];  then export hr_list=$hr_list4 ; fi

export fhr
for fhr in $hr_list
do
echo "fhr=$fhr"
export pgm=aqm_nmm_prep
# . prep_step
export FORT21="$FIXaqm/aqm_grid138.txt"
export gridspecs_139="lambert:202.5:19:21 197.92:82:12000 17.61:54:12000"


$WGRIB2 $COMINnam/nam.t${cyc}z.bgrd3d${fhr}.tm00 | grep -F -f ${FORT21} | $WGRIB2 -i -grib inputs_${fhr}.grib $COMINnam/nam.t${cyc}z.bgrd3d${fhr}.tm00 > ${pgmout}.${fhr}.$h_seg 2>errfile
$WGRIB2 inputs_${fhr}.grib -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv inputs_${fhr}.grib.uv >> ${pgmout}.${fhr}.$h_seg 2>errfile
#$WGRIB2 inputs_${fhr}.grib.uv -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
$WGRIB2 inputs_${fhr}.grib.uv -set_bitmap 1 -set_grib_type s -new_grid_winds grid -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
    -new_grid_interpolation bilinear -if ":(WEASD|APCP|NCPCP|ACPCP|SNOD):" -new_grid_interpolation budget -fi \
    -if ":(TMP:surface|VEG|CCOND|SFEXC|PRES:tropopause|LAI|HPBL|HGT:planetary boundary layer):" -new_grid_interpolation neighbor -fi \
    -new_grid ${gridspecs_139} aqm.t${cyc}z.nmm${fhr}.tm00.uv >> ${pgmout}.${fhr}.$h_seg 2>errfile
$WGRIB2 aqm.t${cyc}z.nmm${fhr}.tm00.uv -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv aqm.t${cyc}z.nmm${fhr}.tm00 >> ${pgmout}.${fhr}.$h_seg 2>errfile

  export err=$?;err_chk

  let "fhr=fhr+1"
  typeset -Z2 fhr
done
