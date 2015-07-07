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

mkdir data

mkdir -p out/$Yr/$Mn

#===================================================`
ln -s $PARMaqm/aqm_sites.valid.20140617.12z.list  $DATA/data 
ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    $DATA/data
ln -s ${BICOR_DATA}/grid   $DATA/data

startmsg
$EXECaqm/aqm_interpolate_update ${PARMaqm}/aqm_config.interp ${cyc}z $STARTDAY $ENDDAY
export err=$?;err_chk

if [ -e ${BICOR_DATA}/interpolated/$Yr/$Mn ] 
then 
  cp -p $DATA/out/$Yr/$Mn/*nc  ${BICOR_DATA}/interpolated/$Yr/$Mn 
else
  mkdir -p ${BICOR_DATA}/interpolated/$Yr/$Mn
  cp -p  $DATA/out/$Yr/$Mn/*nc  ${BICOR_DATA}/interpolated/$Yr/$Mn
fi

echo "Interploation is done for " $PDY 
