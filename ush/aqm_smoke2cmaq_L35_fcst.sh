#!/bin/ksh

set -xa 

cd $DATA

nowdate=`$NDATE| cut -c1-8`


firedate=${1:-$nowdate}

fyear=`echo $PDY | cut -c1-4`
fmonth=`echo $PDY | cut -c5-6`
fday=`echo $PDY | cut -c7-8`

echo $fyear $fmonth $fdate $cyc

daybefore=`$NDATE -24 ${firedate}${cyc}`
echo $daybefore

fyearm1=`echo $daybefore |cut -c 1-4`
fmonthm1=`echo $daybefore |cut -c 5-6`
fdaym1=`echo $daybefore |cut -c 7-8`

echo $fyearm1 $fmonthm1 $fdaym1

cat > tmp.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 byear=$fyearm1
 bmon=$fmonthm1
 bday=$fdaym1
 start=$cyc
 numrec=$nstep
 dirname='${DATA}/'
 &end
EOF
 
namecut tmp.ini fire.ini
rm tmp.ini files_fires_cs.tar

if [ -s ${smoke_emis9}/files_fires_cs.tar ] ; then
 cp -rp ${smoke_emis9}/files_fires_cs.tar $DATA
else
 echo "can not locate files_fires_cs.tar in /com "
 exit 1
fi
cd $DATA
tar -xvf files_fires_cs.tar 
cd -

export GRID=$COMOUT/aqm.t${cyc}z.grdcro2d.ncf
export MCRO3=$COMOUT/aqm.t${cyc}z.metcro3d.ncf
export MDOT3=$COMOUT/aqm.t${cyc}z.metdot3d.ncf
export MCRO2=$COMOUT/aqm.t${cyc}z.metcro2d.ncf

export OUTPUT1=$DATA/aqm.${PDY}.t${cyc}z.play3d.fire.ncf
export OUTPUT2=$DATA/aqm.${PDY}.t${cyc}z.smokefire2d.ncf
export OUTPUT3=$DATA/aqm.${PDY}.t${cyc}z.smokefire3d.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

$EXECaqm/aqm_fire_fcst_1 > tmpfire.out
export err=$?;
err_chk


cat > tmp.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
 numrec=$nstep
 dirname='${DATA}/'
 &end
EOF
 
namecut tmp.ini cmaq.ini
rm tmp.ini

export FIRE3D=$DATA/aqm.${PDY}.t${cyc}z.smokefire3d.ncf
export OEMIS=$DATA/aqm.t${cyc}z.emission.$PDY.windust_snowc.ncf
export NEMIS=$DATA/aqm.t${cyc}z.emission+fire.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

$EXECaqm/aqm_fire_fcst_2 > tmpsmoke.out
export err=$?;
err_chk

cp $COMOUT/aqm.${cycle}.emission.ncf $COMOUT/aqm.${cycle}.emission_old.ncf
cp $DATA/aqm.${cycle}.emission+fire.ncf $COMOUT/
cp $DATA/aqm.${cycle}.emission+fire.ncf $COMOUT/aqm.${cycle}.emission.ncf

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

