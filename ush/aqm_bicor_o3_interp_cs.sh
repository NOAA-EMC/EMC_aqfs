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
# K. Wang      Jul 19 2023   Updated to PSL's 2023/04 version of BC code
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd ${DATA}

mkdir -p data/coords site-lists

mkdir -p out/ozone/${Yr}

#===================================================`
ln -s $PARMaqm/sites.valid.ozone.20230331.12z.list  ${DATA}/site-lists

ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    ${DATA}/data/coords
ln -s $PARMaqm/aqm_config.interp.ozone.8-vars.${cyc}z  ${DATA}
ln -s ${COMINbicor}   ${DATA}/data

startmsg
$EXECaqm/aqm_interpolate_update aqm_config.interp.ozone.8-vars.${cyc}z ${cyc}z ${PDY} ${PDY}  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/ozone/${Yr} ] 
then 
  cp ${DATA}/out/ozone/${Yr}/*nc  ${COMINbicor}/interpolated/ozone/${Yr}
else
  mkdir -p ${COMINbicor}/interpolated/ozone/${Yr}
  cp ${DATA}/out/ozone/${Yr}/*nc  ${COMINbicor}/interpolated/ozone/${Yr}
fi

echo "Interploation is done for " ${PDY} 
