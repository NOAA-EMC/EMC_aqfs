#!/bin/ksh

set -ax
echo "It is extracting PM2.5 and met inputs for Bias Correction"

cd $DATA
startmsg
$USHaqm/aqm_subset.sh -l "1 1" -s "PRES DENS QV QR" $COMIN/aqm.$cycle.metcro3d.ncf Spec_humid.${cycle}.ncf  >> $pgmout 2>errfile
export err=$?;err_chk

startmsg
$USHaqm/aqm_subset.sh -s "PRSFC TEMPG TEMP2 WSPD10 WDIR10 HFX QFX CFRAC CSDSF PBL2" $COMIN/aqm.$cycle.metcro2d.ncf sfc_met_n_PBL.${cycle}.ncf >> $pgmout 2>errfile

export err=$?;err_chk
startmsg
$USHaqm/aqm_subc.sh  $PARMaqm/aqm_temp25pm_aero6 $COMIN/aqm.$cycle.aconc_sfc.ncf aqm.${cycle}.pm25.ncf >> $pgmout 2>errfile
export err=$?;err_chk

cp $DATA/aqm.${cycle}.pm25.ncf.s  $DATA/aqm.${cycle}.O3_pm25.ncf

date
