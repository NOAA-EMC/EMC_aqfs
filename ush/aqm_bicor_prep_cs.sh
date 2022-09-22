#!/bin/ksh
#
# H.-C. Huang  Oct 21 2019   Remove the production of Spec_humid.${cycle}.ncf and UV_wind.${cycle}.ncf
#                            Per email instruction of Dave Allure ESRL/GSD on October 21 2019
#
echo "It is extracting PM2.5 and met inputs for Bias Correction"

#
startmsg
$USHaqm/aqm_subset.sh "PRSFC TEMPG TEMP2 WSPD10 WDIR10 RGRND RN RC HFX LH CFRAC PBL" $COMIN/aqm.$cycle.metcro2d.ncf sfc_met_n_PBL.${cycle}.ncf >> $pgmout 2>errfile
export err=$?;err_chk

startmsg
$USHaqm/aqm_subc.sh  $PARMaqm/aqm_temp25pm_aero6_gas $COMIN/aqm.$cycle.aconc_sfc.ncf aqm.${cycle}.O3_pm25.ncf >> $pgmout 2>errfile
export err=$?;err_chk


cp $DATA/aqm.${cycle}.O3_pm25.ncf.s  $DATA/aqm.${cycle}.O3_pm25.ncf

date
