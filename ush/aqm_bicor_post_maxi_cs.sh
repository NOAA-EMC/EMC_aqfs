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

ln -s $COMOUT/pm2.5.corrected.${PDY}.${cyc}z.nc  a.nc 
#export CMAQBCFILE1=$COMIN/pm2.5.corrected.${PDY}.${cyc}z.nc

export chk=1 
# today 00z file exists otherwise chk=0

if [ $cyc =  '06' ]; then
 if [ -s $COMOUT/pm2.5.corrected.${PDY}.00z.nc ]; then
   ln -s  $COMOUT/pm2.5.corrected.${PDY}.00z.nc  b.nc 
 else 
   ln -s $COMOUTm1/pm2.5.corrected.${PDYm1}.12z.nc  b.nc
   chk=0
 fi
fi

if [ $cyc = '12' ] ; then
 if [ -s $COMOUT/pm2.5.corrected.${PDY}.00z.nc ]; then 
  ln -s $COMOUT/pm2.5.corrected.${PDY}.00z.nc  b.nc
 else
  ln -s $COMOUTm1/pm2.5.corrected.${PDYm1}.12z.nc  b.nc
  chk=0
 fi
  ln -s $COMOUT/pm2.5.corrected.${PDY}.06z.nc  c.nc
fi
#-----------------------------------------------------------
# write out grib1 format
#startmsg  
#$EXECaqm/aqm_post_maxi_bias_cor   pm25 ${PDY} $cyc $chk 
#export err=$?;err_chk


#if [ $envir = "para1" ] ; then
# if [ -e $COMOUT_grib/$PDY ] ; then
#  cp $DATA/aqm.t${cyc}z.24hpm25-ave.148.bc.grib $COMOUT_grib/$PDY
#  cp $DATA/aqm.t${cyc}z.1hpm25-max.148.bc.grib $COMOUT_grib/$PDY
# else
#  mkdir -p $COMOUT_grib/$PDY
#  cp $DATA/aqm.t${cyc}z.24hpm25-ave.148.bc.grib $COMOUT_grib/$PDY
#  cp $DATA/aqm.t${cyc}z.1hpm25-max.148.bc.grib $COMOUT_grib/$PDY

# fi
#fi
# cp $DATA/aqm.t${cyc}z.24hpm25-ave.148.bc.grib $COMOUT
# cp $DATA/aqm.t${cyc}z.1hpm25-max.148.bc.grib $COMOUT

# write out grib2 format 
#-------------------------------------------------
startmsg
$EXECaqm/aqm_post_maxi_bias_cor_grib2  pm25 ${PDY} $cyc $chk
export err=$?;err_chk

if [ $envir = "para1" ] ; then
 cp $DATA/aqm.t${cyc}z.24hpm25-ave.148.bc.grib2 $COMOUT_grib/$PDY/aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2
 cp $DATA/aqm.t${cyc}z.1hpm25-max.148.bc.grib2  $COMOUT_grib/$PDY/aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2

fi
 cp $DATA/aqm.t${cyc}z.24hpm25-ave.148.bc.grib2 $COMOUT/aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2
 cp $DATA/aqm.t${cyc}z.1hpm25-max.148.bc.grib2  $COMOUT/aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2



