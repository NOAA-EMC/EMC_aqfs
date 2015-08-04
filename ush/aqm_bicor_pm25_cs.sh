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

mkdir sites 

rm -rf  data/interpolated  data/airnow  data/grid

mkdir -p data/interpolated/${Yr}
mkdir -p data/interpolated/${Yr4w}
mkdir -p data/grid/${Yr}
mkdir -p data/grid/${Yr4w}
mkdir -p data/airnow/${Yr}
mkdir -p data/airnow/${Yr1}
mkdir -p data/airnow/${Yr4w}


ln -s $PARMaqm/aqm.*grdcro2d.ncf data/
ln -s $COMINbicor/interpolated/${Yr}/*      data/interpolated/${Yr}/
ln -s $COMINbicor4w/interpolated/${Yr4w}/*  data/interpolated/${Yr4w}/
ln -s $COMINbicor/grid/${Yr}/*        data/grid/${Yr}/
ln -s $COMINbicor4w/grid/${Yr4w}/*    data/grid/${Yr4w}/
ln -s $COMINbicor/airnow/${Yr}/*      data/airnow/${Yr}/
ln -s $COMINbicor1/airnow/${Yr1}/*    data/airnow/${Yr1}/
ln -s $COMINbicor4w/airnow/${Yr4w}/*  data/airnow/${Yr4w}/

startmsg  
$EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm25_bias_cor  ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk


if [ ${envir} = 'para1' ] ; 
then
 cp  $DATA/out/pm2.5.corrected*   $COMOUT_grib
 cp  $DATA/out/pm2.5.corrected* /naqfc/noscrub/${USER}/bias/corrected
fi
 cp $DATA/out/pm2.5*   $COMOUT

