#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_post_maxi_cs.sh
# Script description:  CMAQ post processing for daily surface maximum O3/PM2.5
#
# Author:  Youhua Tang  Org: NP22  Date: 2009-06-30
#
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd ${DATA}

if [ -e ${DATA}/out ]; then
   echo "${DATA}/out exits !"
else
   mkdir -p ${DATA}/out 
fi

ln -s ${COMOUT}/pm2.5.corrected.${PDY}.${cyc}z.nc  a.nc 

export chk=1 
export chk1=1 
# today 00z file exists otherwise chk=0

cat >bias_cor_max.ini <<EOF1
&control
varlist='pm25_24h_ave','pm25_1h_max'
outfile='aqm-pm25_bc'
id_gribdomain=148
max_proc=${post_proc_hour}
/
EOF1

flag_run_bicor_max=yes
## 06z needs b.nc to find current day output from 04Z to 06Z
if [ "${cyc}" == "06" ]; then
   if [ -s ${COMOUT}/pm2.5.corrected.${PDY}.00z.nc ]; then
      ln -s  ${COMOUT}/pm2.5.corrected.${PDY}.00z.nc  b.nc 
   elif [ -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc ]; then
      ln -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc  b.nc
      chk=0
   else 
      flag_run_bicor_max=no
   fi
fi

if [ "${cyc}" == "12" ]; then
   ## 12z needs b.nc to find current day output from 04Z to 06Z
   if [ -s ${COMOUT}/pm2.5.corrected.${PDY}.00z.nc ]; then
      ln -s ${COMOUT}/pm2.5.corrected.${PDY}.00z.nc  b.nc
   elif [ -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc ]; then
      ln -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc  b.nc
      chk=0
   else
      flag_run_bicor_max=no
   fi

   ## 12z needs c.nc to find current day output from 07Z to 12z
   if [ -s ${COMOUT}/pm2.5.corrected.${PDY}.06z.nc ]; then
      ln -s ${COMOUT}/pm2.5.corrected.${PDY}.06z.nc  c.nc
   elif [ -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc ]; then
      ln -s ${COMOUTm1}/pm2.5.corrected.${PDYm1}.12z.nc  c.nc
      chk1=0
   else
      flag_run_bicor_max=no
   fi
fi
if [ "${flag_run_bicor_max}" == "yes" ]; then
   #-------------------------------------------------
   # write out grib2 format 
   #-------------------------------------------------
   rm -rf errfile
   startmsg
   ${EXECaqm}/aqm_post_maxi_bias_cor_grib2  ${PDY} ${cyc} ${chk} ${chk1}
   export err=$?;err_chk
   
   
   # split into two files: one for 24hr_ave and one for 1h_max
   
   ${WGRIB2} aqm-pm25_bc.148.grib2  |grep  "PMTF"   | ${WGRIB2} -i  aqm-pm25_bc.148.grib2  -grib aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2 
   ${WGRIB2} aqm-pm25_bc.148.grib2  |grep  "PDMAX1" | ${WGRIB2} -i  aqm-pm25_bc.148.grib2  -grib aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2 
   
   if [ "$envir" = "para6z" ] ; then
      cp ${DATA}/aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2  ${COMOUT_grib}/${RUN}.$PDY/
      cp ${DATA}/aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2   ${COMOUT_grib}/${RUN}.$PDY/
   fi
   cp ${DATA}/aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2 ${COMOUT}/
   cp ${DATA}/aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2  ${COMOUT}/
else
   echo "MISSING b.nc and/or c.nc of previous cycle pm2.5.corrected.[PDY|PDYm1].t[00|06]z.nc and can not run ${pgm} maxi"
   echo "It is possible that this is an initial run for a new experiemnt, require manauel check"
fi
