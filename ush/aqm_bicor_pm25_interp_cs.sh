#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_interp_cs.sh 
# Script description:  interpolating CMAQ outpout to AirNow observational sites 
#
# Author:  Jianping Huang  07-10-2015 
#
# H.-C. Huang  Nov 22 2019   Use new site list sites.valid.pm25.20190815.06z.list
# K. Wang      Jul 19 2023   Updated to PSL's 2023/04 version of BC code
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

export bc_fcst_hr=72

cd ${DATA}

mkdir -p data/coords site-lists

mkdir -p out/pm25/${Yr}

#===================================================`
ln -s $PARMaqm/sites.valid.pm25.20230331.12z.list ${DATA}/site-lists

ln -s $PARMaqm/aqm.t12z.grdcro2d.ncf    ${DATA}/data/coords
ln -s $PARMaqm/aqm_config.interp.pm2.5.5-vars.${cyc}z  ${DATA}
ln -s ${COMINbicor}   ${DATA}/data

startmsg
$EXECaqm/aqm_interpolate_update aqm_config.interp.pm2.5.5-vars.${cyc}z ${cyc}z ${PDY} ${PDY}  >> $pgmout 2>errfile
export err=$?;err_chk

if [ -e ${COMINbicor}/interpolated/pm25/${Yr} ] 
then 
  cp ${DATA}/out/pm25/${Yr}/*nc  ${COMINbicor}/interpolated/pm25/${Yr}
else
  mkdir -p ${COMINbicor}/interpolated/pm25/${Yr}
  cp ${DATA}/out/pm25/${Yr}/*nc  ${COMINbicor}/interpolated/pm25/${Yr}
fi

echo "Interploation is done for " ${PDY} 
