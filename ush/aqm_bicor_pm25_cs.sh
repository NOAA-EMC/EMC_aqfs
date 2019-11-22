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
export OMP_NUM_THREADS=16
export MKL_NUM_THREADS=16
export KMP_AFFINITY=disabled

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

#mkdir -p data sites coords
mkdir -p data sites data/coords

ln -s $PARMaqm/aqm.*grdcro2d.ncf  data/coords/
# Oct 21 2019 H-C Huang config files never use *12z.list*
#ln -s $PARMaqm/aqm_sites.valid.pm25.20170818.06z.list sites/sites.valid.20170818.12z.list
ln -s $PARMaqm/aqm_sites.valid.pm25.20190815.06z.list sites/sites.valid.pm25.20190815.06z.list

ln -s ${COMINbicordat}/bcdata* data/

ln -s $PARMaqm/aqm_bias_thresholds.pm2.5.2015.1030.32-sites.txt ./bias_thresholds.pm2.5.2015.1030.32-sites.txt 

startmsg  
aprun -n 1 -d 16 -cc none $EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm2.5.5pred.equal-weights ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk

# JY if [ ${envir} = 'para' ] ; 
#then
# cp  $DATA/out/pm2.5.corrected*  ${COMOUT_grib} 
#fi
if [ ${envir} = 'para13' ] ;
then
 cp  $DATA/out/pm2.5.corrected*   $COMOUT_grib
fi

 cp $DATA/out/pm2.5.corrected*   $COMOUT

