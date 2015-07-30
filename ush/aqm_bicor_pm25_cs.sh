#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_cmaq_maxi.sh
# Script description:  CMAQ post processing for daily surface maximum O3/PM2.5
#
# Author:  Youhua Tang  Org: NP22  Date: 2009-06-30
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
ln -s $BICOR_DATA/interpolated/${Yr}/*      data/interpolated/${Yr}/
ln -s $BICOR_DATA4w/interpolated/${Yr4w}/*  data/interpolated/${Yr4w}/
echo "hjp11"
ln -s $BICOR_DATA/grid/${Yr}/*        data/grid/${Yr}/
echo "hjp22"
ln -s $BICOR_DATA4w/grid/${Yr4w}/*    data/grid/${Yr4w}/
echo "hjp33"
ln -s $BICOR_DATA/airnow/${Yr}/*      data/airnow/${Yr}/
ln -s $BICOR_DATA1/airnow/${Yr1}/*    data/airnow/${Yr1}/
ln -s $BICOR_DATA4w/airnow/${Yr4w}/*  data/airnow/${Yr4w}/

startmsg  
$EXECaqm/aqm_bias_correct ${PARMaqm}/aqm_config.pm25_bias_cor  ${cyc}Z  $BC_STDAY $PDY >> $pgmout 2>errfile
export err=$?;err_chk


if [ ${envir} = 'para1' ] ; 
then
 cp  $DATA/out/pm2.5.corrected*   $COMOUT_grib
 cp  $DATA/out/pm2.5.corrected* /naqfc/noscrub/${USER}/bias/corrected
fi
 cp $DATA/out/pm2.5*   $COMOUT

