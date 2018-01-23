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

mkdir -p out/ozone/$Yr

#===================================================`
ln -s $PARMaqm/sites.valid.ozone.20170531.06z.list  $DATA/site-lists
ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    $DATA/data/coords
ln -s $PARMaqm/aqm_config.interp.ozone.0707.8-vars  $DATA
ln -s ${COMINbicor}   $DATA/data

startmsg
$EXECaqm/aqm_interpolate_update  aqm_config.interp.ozone.0707.8-vars ${cyc}z $PDY $PDY  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/ozone/$Yr ] 
then 
  cp -p $DATA/out/ozone/$Yr/*nc  ${COMINbicor}/interpolated/ozone/$Yr
else
  mkdir -p ${COMINbicor}/interpolated/ozone/$Yr
  cp -p  $DATA/out/ozone/$Yr/*nc  ${COMINbicor}/interpolated/ozone/$Yr
fi

echo "Interploation is done for " $PDY 
