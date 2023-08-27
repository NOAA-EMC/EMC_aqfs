#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_airnow_converter.sh 
# Script description:  converting airnow csv to netcdf format for previous 3 days
#
# Author:  Kai Wang and Jianping Huang  07-18-2023
#
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd ${DATA}

# Retrieve real-time airnow data for the last three days and convert them into netcdf
for ipdym in {1..3}; do

  case $ipdym in
      1)
        cvt_input_dir="$COMINbicorm1/airnow/csv"
        cvt_output_dir="$COMINbicorm1/airnow/netcdf"
        cvt_pdy="${PDYm1}"
        ;;
      2)
        cvt_input_dir="$COMINbicorm2/airnow/csv"
        cvt_output_dir="$COMINbicorm2/airnow/netcdf"
        cvt_pdy="${PDYm2}"
        ;;
      3)
        cvt_input_dir="$COMINbicorm3/airnow/csv"
        cvt_output_dir="$COMINbicorm3/airnow/netcdf"
        cvt_pdy="${PDYm3}"
        ;;
   esac

  cvt_input_fp="${cvt_input_dir}/YYYY/YYYYMMDD/HourlyAQObs_YYYYMMDDHH.dat"
  cvt_output_fp="${cvt_output_dir}/YYYY/YYYYMMDD/HourlyAQObs.YYYYMMDD.nc"


  startmsg
  $EXECaqm/convert_airnow_csv ${cvt_input_fp} ${cvt_output_fp} ${cvt_pdy} ${cvt_pdy} >> $pgmout 2>errfile
  export err=$?;err_chk

done

echo "Airnow converter is done for " ${PDY} 
