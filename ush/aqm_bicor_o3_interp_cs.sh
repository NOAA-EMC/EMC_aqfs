#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_interp_cs.sh 
# Script description:  interpolating CMAQ outpout to AirNow observational sites 
#
# Author:  Jianping Huang  07-10-2015 
#
# H.-C. Huang  Nov 22 2019   Use new site list sites.valid.ozone.20190815.06z.list
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd $DATA

mkdir -p data/coords site-lists

mkdir -p out/ozone/$Yr

#===================================================`
#ln -s $PARMaqm/sites.valid.ozone.20200815.06z.list  $DATA/site-lists
if [ -s $COMINm1/sites.valid.ozone.$PDYm1.12z.list ] ; then
 ln -s $COMINm1/sites.valid.ozone.$PDYm1.12z.list    $DATA/site-lists/sites.valid.ozone.12z.list
else
 ln -s $PARMaqm/sites.valid.ozone.20210313.12z.list  $DATA/site-lists/sites.valid.ozone.12z.list
fi

ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    $DATA/data/coords
ln -s $PARMaqm/aqm_config.interp.ozone.8-vars  $DATA
ln -s ${COMINbicor}   $DATA/data

startmsg
$EXECaqm/aqm_interpolate_update  aqm_config.interp.ozone.8-vars ${cyc}z ${bc_interp_hr} $PDY $PDY  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/ozone/$Yr ] 
then 
  cp -p $DATA/out/ozone/$Yr/*nc  ${COMINbicor}/interpolated/ozone/$Yr
else
  mkdir -p ${COMINbicor}/interpolated/ozone/$Yr
  cp -p  $DATA/out/ozone/$Yr/*nc  ${COMINbicor}/interpolated/ozone/$Yr
fi

#cp -p $DATA/sites/sites.valid.ozone.$PDY.${cyc}z.list  $COMIN

echo "Interploation is done for " $PDY 
