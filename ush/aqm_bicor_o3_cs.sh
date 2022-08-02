#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_pm25_cs.sh 
# Script description:  is used to do bias correctio for PM2.5 
#
# Author:  Jianping Huang  Org: NP22  Date: 2015-06-30
#
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

mkdir -p ${DATA}/data ${DATA}/sites ${DATA}/data/coords

cp -rp  $PARMaqm/aqm.*grdcro2d.ncf  ${DATA}/data/coords/

ln -s ${COMINbicordat}/bcdata* ${DATA}/data/

startmsg  
$EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.ozone.bias_corr  ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk


if [ "${envir}" = 'para' ] ; 
then
 cp $DATA/out/ozone.corrected*  ${COMOUT_grib} 
fi

 cp $DATA/out/ozone.corrected*   $COMOUT
if [ ${cycle} = 't12z'  ] ; then
 cp $DATA/sites/sites.valid.ozone.$PDY.${cyc}z.list  $COMOUT
fi

