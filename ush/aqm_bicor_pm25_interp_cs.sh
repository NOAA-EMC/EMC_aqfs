#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_interp_cs.sh 
# Script description:  interpolating CMAQ outpout to AirNow observational sites 
#
# Author:  Jianping Huang  07-10-2015 
#
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd $DATA

mkdir -p data/coords site-lists

mkdir -p out/pm25/$Yr

#===================================================`
#ln -s $PARMaqm/sites.valid.pm25.20170531.06z.list  $DATA/site-lists
ln -s $PARMaqm/aqm_sites.valid.pm25.20170818.06z.list  $DATA/site-lists/sites.valid.pm25.20170818.06z.list
ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    $DATA/data/coords
ln -s $PARMaqm/aqm_config.interp.pm25.0707.9-vars  $DATA
ln -s ${COMINbicor}   $DATA/data

startmsg
$EXECaqm/aqm_interpolate_update  aqm_config.interp.pm25.0707.9-vars ${cyc}z $PDY $PDY  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/pm25/$Yr ] 
then 
  cp -p $DATA/out/pm25/$Yr/*nc  ${COMINbicor}/interpolated/pm25/$Yr
else
  mkdir -p ${COMINbicor}/interpolated/pm25/$Yr
  cp -p  $DATA/out/pm25/$Yr/*nc  ${COMINbicor}/interpolated/pm25/$Yr
fi

echo "Interploation is done for " $PDY 
