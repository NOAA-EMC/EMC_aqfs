#!/bin/ksh

set -xa 

cd $DATA

nowdate=`$NDATE | cut -c1-8`

#firedate=${1:-$nowdate}
firedate=$PDYm1

fyear=`echo $firedate | cut -c1-4`
fmonth=`echo $firedate | cut -c5-6`
fday=`echo $firedate | cut -c7-8`
#fyear=`echo $PDYm1 | cut -c1-4`
#fmonth=`echo $PDYm1 | cut -c5-6`
#fday=`echo $PDYm1 | cut -c7-8`

#typeset -Z2 cyc

echo $fyear $fmonth $fdate $cyc

#cat > tmp.ini << EOF
cat > fire.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 start=$cyc
 dirname='${DATA}/'
 &end
EOF
 
#namecut tmp.ini fire.ini
#rm tmp.ini
export PDYp1=$PDY

if [ -s ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.tar ] ; then
#if [ -s ${smoke_emis}/smokecs.$PDY/files_fires_cs.tar ] ; then
 cp ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.tar $DATA
# cp ${smoke_emis}/smokecs.$PDY/files_fires_cs.tar $DATA
else
 echo "can not locate files_fires_cs.tar in /com "
 exit 1
fi
#if [ -s ${smoke_emis}/smokecs.$PDYp1/EMITIMES ] ; then
if [ -s ${smoke_emis}/smokecs.$PDY/EMITIMES ] ; then
 cp ${smoke_emis}/smokecs.$PDY/EMITIMES $DATA
# cp ${smoke_emis}/smokecs.$PDYp1/EMITIMES $DATA
else
 echo "can not locate EMITIMES in /com "
 exit 1
fi

cd $DATA
tar -xvf files_fires_cs.tar 
ln -sf $COMINm1/aqm.t${cyc}z.grdcro2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.grddot2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metbdy3d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metcro2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metcro3d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metdot3d.ncf $DATA/.
#ln -sf $COMINm1/bnd-ngac-dust.$PDY.5x-35L.ncf $DATA/.
ln -sf $COMINm1/aqm_conus_geos_ngac_dust_${PDYm1}_35L.ncf $DATA/.
ln -sf $COMINm1/aqm.${cycle}.emission.${PDYm1}.windust_snowc.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.grdcro2d.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.grddot2d.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.metbdy3d.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.metcro2d.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.metcro3d.ncf $DATA/.
#ln -sf $COMIN/aqm.${cycle}.metdot3d.ncf $DATA/.
#ln -sf $COMIN/bnd-ngac-dust.$PDY.5x-35L.ncf $DATA/.
#ln -sf $COMIN/aqm_conus_geos_ngac_dust_${PDY}_35L.ncf $DATA/.

#ln -sf $COMIN/aqm.${cycle}.emission.${PDY}.windust_snowc.ncf $DATA/.


cd -

export GRID=$DATA/aqm.${cycle}z.grdcro2d.ncf
export MCRO3=$DATA/aqm.${cycle}z.metcro3d.ncf
export MDOT3=$DATA/aqm.${cycle}z.metdot3d.ncf
export MCRO2=$DATA/aqm.${cycle}z.metcro2d.ncf

export OUTPUT1=$DATA/aqm.${PDYm1}.${cycle}.play3d.fire.ncf
export OUTPUT2=$DATA/aqm.${PDYm1}.${cycle}.smokefire2d.ncf
export OUTPUT3=$DATA/aqm.${PDYm1}.${cycle}.smokefire3d.ncf


if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

#$EXECaqm/smoke2cmaq_35layer_anal_1 > tmpfire.out
$EXECaqm/aqm_fire_analy_1  > tmpfire.out
export err=$?;
err_chk


#jp cat > tmp.ini << EOF
 cat > cmaq.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 start=$cyc
 dirname='${DATA}/'
 &end
EOF
 
#namecut tmp.ini cmaq.ini
#rm tmp.ini

export FIRE3D=$DATA/aqm.${PDYm1}.${cycle}.smokefire3d.ncf
export OEMIS=$DATA/aqm.${cycle}.emission.$PDYm1.windust_snowc.ncf
export NEMIS=$DATA/aqm.${cycle}.emission+fire.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

#$EXECaqm/smoke2cmaq_35layer_anal_2 > tmpsmoke.out
$EXECaqm/aqm_fire_analy_2  > tmpsmoke.out 
export err=$?;
err_chk


#mv $COMOUTm1/aqm.${cycle}.emission.ncf $COMOUTm1/aqm.${cycle}.emission_old.ncf
#cp $DATA/aqm.${cycle}.emission+fire.ncf $COMOUTm1/aqm.${cycle}.emission+fire_r.ncf
#cp $DATA/aqm.${cycle}.emission+fire.ncf $COMOUTm1/aqm.${cycle}.emission.ncf

#mv $COMOUTm1/aqm.${cycle}.emission.ncf $COMOUTm1/aqm.${cycle}.emission_old.ncf
cp $DATA/aqm.${cycle}.emission+fire.ncf $COMOUTm1/aqm.${cycle}.emission+fire_r.ncf
mv $DATA/aqm.${cycle}.emission+fire.ncf $COMOUTm1/aqm.${cycle}.emission_r.ncf

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
exit
