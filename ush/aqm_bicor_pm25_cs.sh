#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_pm25_cs.sh 
# Script description:  is used to do bias correctio for PM2.5 
#
# Author:  Jianping Huang  Org: NP22  Date: 2015-06-30
#
# H.-C. Huang  Nov 22 2019   Use new site list sites.valid.pm25.20190815.06z.list
######################################################################
set -xa

export OMP_STACKSIZE=58000000
#export OMP_NUM_THREADS=24
#export MKL_NUM_THREADS=24
#export KMP_AFFINITY=disabled

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd $DATA

if [ -e ${DATA}/out ] ;
then
 echo "${DATA}/out exits !"
else
 mkdir -p ${DATA}/out 
fi

rm -rf data

mkdir -p ${DATA}/data  ${DATA}/sites  ${DATA}/data/coords

cp -rp  $PARMaqm/aqm.*grdcro2d.ncf   ${DATA}/data/coords/

ln -s $COMINm1/aqm_sites.valid.pm25.$PDYm1.12z.list ${DATA}/sites/sites.valid.pm25.12z.list

ln -s ${COMINbicordat}/bcdata* ${DATA}/data/

ln -s $PARMaqm/aqm_bias_thresholds.pm2.5.2015.1030.32-sites.txt ${DATA}/bias_thresholds.pm2.5.2015.1030.32-sites.txt 

startmsg  
#aprun -n 1 -d 24 -cc none $EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm2.5.bias_corr ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
$EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm2.5.bias_corr ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk

if [ ${envir} = 'para6z' ] ;
then
 cp  $DATA/out/pm2.5.corrected*   $COMOUT_grib
fi

 cp $DATA/out/pm2.5.corrected*   $COMOUT

if [ ${cycle} = 't12z'  ] ; then
 cp $DATA/sites/sites.valid.pm25.$PDY.${cyc}z.list  $COMOUT
fi
