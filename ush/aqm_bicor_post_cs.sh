#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_cmaq_maxi.sh
# Script description:  CMAQ post processing for daily surface PM2.5 with bias correction 
#
# Author:  
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

ln -s $COMOUT/pm2.5.corrected.${PDY}.${cyc}z.nc .

##------------------------
# convert from netcdf to grib1 format
#startmsg  
#$EXECaqm/aqm_post_bias_cor pm2.5.corrected.${PDY}.${cyc}z.nc pm25 ${PDY} $cyc 
#export err=$?;err_chk

#cp -rp $DATA/aqm.t${cyc}z.25pm* $COMOUT

#if [ -e $COMOUT_grib/$PDY ] ; then
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT_grib/$PDY 
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT
#else
# mkdir -p $COMOUT_grib/$PDY
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT_grib/$PDY
#fi

##------------------------
# convert from netcdf to grib2 format
startmsg
$EXECaqm/aqm_post_bias_cor_grib2 pm2.5.corrected.${PDY}.${cyc}z.nc pm25 ${PDY} $cyc
export err=$?;err_chk

cp -rp $DATA/aqm.t${cyc}z.25pm*bc.grb2 $COMOUT

if [ -e $COMOUT_grib/$PDY ] ; then
 cp $DATA/aqm.t${cyc}z.25pm*bc.grb2 $COMOUT_grib/$PDY
else
 mkdir -p $COMOUT_grib/$PDY
 cp $DATA/aqm.t${cyc}z.25pm*bc.grb2 $COMOUT_grib/$PDY
fi

