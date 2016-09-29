#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_pm25_cs.sh 
# Script description:  is usd to do bias correctio for PM2.5 
#
# Author:  Jianping Huang  Org: NP22  Date: 2015-06-30
#
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd $DATA

if [ -e ${DATA}/out ] ;
then
 echo "${DATA}/out exits !"
else
 mkdir -p ${DATA}/out 
fi


#rm -rf  data/interpolated  data/airnow  data/grid
rm -rf data

mkdir -p data sites coords

#ln -s $PARMaqm/aqm_site_bias_thresholds.txt .

ln -s $PARMaqm/aqm.*grdcro2d.ncf  coords/
ln -s $PARMaqm/aqm_sites.valid.20140617.12z.list sites/ 
ln -s $PARMaqm/aqm_bias_thresholds.2015.1030.32-sites.txt ./bias_thresholds.2015.1030.32-sites.txt 

ln -s ${COMINbicordat}/bcdata* data/

startmsg  
$EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm25_bias_cor_omp  ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk


if [ ${envir} = 'para1' ] ; 
then
 cp  $DATA/out/pm2.5.corrected*  ${COMOUT_grib} 
fi
if [ ${envir} = 'para2' ] ;
then
 cp  $DATA/out/pm2.5.corrected*   $COMOUT_grib
 cp  $DATA/out/pm2.5.corrected* /naqfc/noscrub/${USER}/bias/corrected_v3
fi

 cp $DATA/out/pm2.5*   $COMOUT

