#!/bin/ksh

set -ax 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]; then
 export  hr_list1="00"
 export  hr_list2="01"
 export  hr_list3="02"
 export  hr_list4="03"
 export  hr_list5="04"
 export  hr_list6="05"
 export  hr_list7="06"
else
 export  hr_list1="00 07 14 21 28 35 42 49 56 63 70"
 export  hr_list2="01 08 15 22 29 36 43 50 57 64 71"
 export  hr_list3="02 09 16 23 30 37 44 51 58 65 72"
 export  hr_list4="03 10 17 24 31 38 45 52 59 66"
 export  hr_list5="04 11 18 25 32 39 46 53 60 67"
 export  hr_list6="05 12 19 26 33 40 47 54 61 68"
 export  hr_list7="06 13 20 27 34 41 48 55 62 69"
fi

export h_seg=$1
if [ ${h_seg} -eq 1 ];  then export hr_list=$hr_list1 ; fi
if [ ${h_seg} -eq 2 ];  then export hr_list=$hr_list2 ; fi
if [ ${h_seg} -eq 3 ];  then export hr_list=$hr_list3 ; fi
if [ ${h_seg} -eq 4 ];  then export hr_list=$hr_list4 ; fi
if [ ${h_seg} -eq 5 ];  then export hr_list=$hr_list5 ; fi
if [ ${h_seg} -eq 6 ];  then export hr_list=$hr_list6 ; fi
if [ ${h_seg} -eq 7 ];  then export hr_list=$hr_list7 ; fi

export fhr
for fhr in $hr_list
do
echo "fhr=$fhr"
export pgm=aqm_fv3gfs_prep
# . prep_step
export FORT21="$FIXaqm/aqm_grid138.txt"
export gridspecs_139="lambert:202.5:19:21 197.92:82:12000 17.61:54:12000"

export infile=$COMINfv3/gfs.t${cyc}z.master.grb2f0${fhr}

$WGRIB2 $infile | grep -F -f ${FORT21} | $WGRIB2 -i -grib inputs_${fhr}.grib $infile > ${pgmout}.${fhr}.$h_seg 2>errfile
$WGRIB2 inputs_${fhr}.grib -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv inputs_${fhr}.grib.uv >> ${pgmout}.${fhr}.$h_seg 2>errfile
$WGRIB2 inputs_${fhr}.grib.uv -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
    -new_grid_interpolation bilinear -if ":(WEASD|APCP|NCPCP|ACPCP|SNOD):" -new_grid_interpolation budget -fi \
    -if ":(TMP:surface|VEG|CCOND|SFEXC|PRES:tropopause|LAI|HPBL|HGT:planetary boundary layer):" -new_grid_interpolation neighbor -fi \
    -new_grid ${gridspecs_139} aqm.t${cyc}z.nmm${fhr}.tm00.uv >> ${pgmout}.${fhr}.$h_seg 2>errfile
$WGRIB2 aqm.t${cyc}z.nmm${fhr}.tm00.uv -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv aqm.t${cyc}z.nmm${fhr}.tm00 >> ${pgmout}.${fhr}.$h_seg 2>errfile

  export err=$?;err_chk

  let "fhr=fhr+1"
  typeset -Z2 fhr
done
