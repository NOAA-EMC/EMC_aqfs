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

export OMP_STACKSIZE=60000000
export OMP_NUM_THREADS=16
export MKL_NUM_THREADS=16

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd $DATA

if [ -e ${DATA}/out ] ;
then
 echo "${DATA}/out exits !"
else
 mkdir -p ${DATA}/out 
fi

rm -rf data

mkdir -p data sites data/coords

ln -s $PARMaqm/aqm.*grdcro2d.ncf  data/coords/

ln -s ${COMINbicordat}/bcdata* data/

startmsg  
aprun -n 1 -d 16 -cc none $EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.ozone.8pred.equal-weights  ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk


if [ "${envir}" = 'para5' ] ; 
then
 cp $DATA/out/ozone.corrected*  ${COMOUT_grib} 
fi

 cp $DATA/out/ozone.corrected*   $COMOUT

