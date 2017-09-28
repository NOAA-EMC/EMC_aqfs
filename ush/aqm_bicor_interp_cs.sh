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

mkdir data

mkdir -p out/$Yr/$Mn

#===================================================`
#ln -s $PARMaqm/aqm_sites.valid.20140617.12z.list  $DATA/data 
ln -s $PARMaqm/aqm_sites.valid.pm25.20170818.list  $DATA/data/aqm_sites.valid.20170818.12z.list 
ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    $DATA/data
ln -s ${COMINbicor}/grid   $DATA/data

startmsg
$EXECaqm/aqm_interpolate_update  ${PARMaqm}/aqm_config.interp ${cyc}z $PDY $PDY  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/$Yr/$Mn ] 
then 
  cp -p $DATA/out/$Yr/$Mn/*nc  ${COMINbicor}/interpolated/$Yr/$Mn 
else
  mkdir -p ${COMINbicor}/interpolated/$Yr/$Mn
  cp -p  $DATA/out/$Yr/$Mn/*nc  ${COMINbicor}/interpolated/$Yr/$Mn
fi

echo "Interploation is done for " $PDY 
