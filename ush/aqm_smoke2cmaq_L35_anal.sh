#!/bin/ksh

set -xa 

cd $DATA

nowdate=`$NDATE | cut -c1-8`

firedate=${1:-$nowdate}

fyear=`echo $firedate | cut -c1-4`
fmonth=`echo $firedate | cut -c5-6`
fday=`echo $firedate | cut -c7-8`

echo $fyear $fmonth $fdate $cyc

cat > tmp.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 start=$cyc
 dirname='${COMOUT}/'
 &end
EOF
 
namecut tmp.ini fire.ini
rm tmp.ini

if [ -s ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.tar ] ; then
 cp ${smoke_emis}/smokecs.$PDYp1/files_fires_cs.tar $COMOUT
else
 echo "can not locate files_fires_cs.tar in /com "
 exit 1
fi
if [ -s ${smoke_emis}/smokecs.$PDYp1/EMITIMES ] ; then
 cp ${smoke_emis}/smokecs.$PDYp1/EMITIMES $COMOUT
else
 echo "can not locate EMITIMES in /com "
 exit 1
fi

cd $COMOUT
tar -xvf $COMOUT/files_fires_cs.tar 
ln -sf $COMIN/aqm.t12z.grdcro2d.ncf $COMOUT/.
ln -sf $COMIN/aqm.t12z.grddot2d.ncf $COMOUT/.
ln -sf $COMIN/aqm.t12z.metbdy3d.ncf $COMOUT/.
ln -sf $COMIN/aqm.t12z.metcro2d.ncf $COMOUT/.
ln -sf $COMIN/aqm.t12z.metcro3d.ncf $COMOUT/.
ln -sf $COMIN/aqm.t12z.metdot3d.ncf $COMOUT/.
ln -sf $COMIN/bnd-ngac-dust.$PDY.5x-35L.ncf $COMOUT/.
ln -sf $COMIN/aqm.t${cyc}z.emission.${PDY}.windust_snowc.ncf $COMOUT/.

cd -

export GRID=$COMOUT/aqm.t${cyc}z.grdcro2d.ncf
export MCRO3=$COMOUT/aqm.t${cyc}z.metcro3d.ncf
export MDOT3=$COMOUT/aqm.t${cyc}z.metdot3d.ncf
export MCRO2=$COMOUT/aqm.t${cyc}z.metcro2d.ncf

export OUTPUT1=$COMOUT/aqm.${PDY}.t${cyc}z.play3d.fire.ncf
export OUTPUT2=$COMOUT/aqm.${PDY}.t${cyc}z.smokefire2d.ncf
export OUTPUT3=$COMOUT/aqm.${PDY}.t${cyc}z.smokefire3d.ncf

$EXECaqm/smoke2cmaq_35layer_anal_1 > tmpfire.out
export err=$?;
err_chk


cat > tmp.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 dirname='${COMOUT}/'
 &end
EOF
 
namecut tmp.ini cmaq.ini
rm tmp.ini

export FIRE3D=$COMOUT/aqm.${PDY}.t${cyc}z.smokefire3d.ncf
export OEMIS=$COMOUT/aqm.t${cyc}z.emission.$PDY.windust_snowc.ncf
export NEMIS=$COMOUT/aqm.t${cyc}z.emission+fire.ncf

$EXECaqm/smoke2cmaq_35layer_anal_2 > tmpsmoke.out
export err=$?;
err_chk


#rm $COMOUT/aqm.${cycle}.emission.ncf
cp $COMOUT/aqm.${cycle}.emission+fire.ncf $COMOUT/aqm.${cycle}.emission.ncf

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

exit
