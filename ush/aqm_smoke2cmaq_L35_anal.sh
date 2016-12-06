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
 
export PDYp1=$PDY

if [ -s ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.t${cyc}z.tar ] ; then
 cp ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.t${cyc}z.tar $DATA/files_fires_cs.tar
else
 echo "can not locate files_fires_cs.tar in /com "
 exit 1
fi
if [ -s ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z ] ; then
 cp ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z $DATA/EMITIMES
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
ln -sf $COMINm1/aqm_conus_geos_ngac_dust_${PDYm1}_35L.ncf $DATA/.
ln -sf $COMINm1/aqm.${cycle}.emission.${PDYm1}.windust_snowc.ncf $DATA/.

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

$EXECaqm/aqm_fire_analy_1  > tmpfire.out
export err=$?;
err_chk


 cat > cmaq.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 start=$cyc
 dirname='${DATA}/'
 &end
EOF
 
export FIRE3D=$DATA/aqm.${PDYm1}.${cycle}.smokefire3d.ncf
export OEMIS=$DATA/aqm.${cycle}.emission.$PDYm1.windust_snowc.ncf
export NEMIS=$DATA/aqm.${cycle}.emission+fire.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

$EXECaqm/aqm_fire_analy_2  > tmpsmoke.out 
export err=$?;
err_chk

mv $DATA/aqm.${cycle}.emission+fire.ncf $COMOUTm1/aqm.${cycle}.emission_r.ncf
ln -s $COMOUTm1/aqm.${cycle}.emission_r.ncf $COMOUTm1/aqm.${cycle}.emission+fire_r.ncf

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
exit
