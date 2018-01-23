#!/bin/ksh

set -xa 

cd $DATA

nowdate=`$NDATE | cut -c1-8`

## waiting for HYSPLIT BlueSky fire emissions about 10 minutes "
ic=0
while [ $ic -lt 60 ]
do
 if [ -s ${smoke_emis}/smokecs.$PDY/files_fires_cs.t${cyc}z.tar ]
  then
    echo  ${smoke_emis}/smokecs.$PDY/files_fires_cs.t${cyc}z.tar "exists!"
     break
  else
     let "ic=ic+1"
      sleep 10
 fi
done

fyear=`echo $PDYm1 | cut -c1-4`
fmonth=`echo $PDYm1 | cut -c5-6`
fday=`echo $PDYm1 | cut -c7-8`

echo $fyear $fmonth $fdate $cyc

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
 echo "No files_fires_cs.tar from HYSPLIT/BlueSky"
 exit 
fi
if [ -s ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z ] ; then
 cp ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z $DATA/EMITIMES
else
 echo "No EMITIMES from HYSPLIT/BlueSky"
 exit 1
fi

tar -xvf files_fires_cs.tar 
ln -sf $COMINm1/aqm.t${cyc}z.grdcro2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.grddot2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metbdy3d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metcro2d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metcro3d.ncf $DATA/.
ln -sf $COMINm1/aqm.t${cyc}z.metdot3d.ncf $DATA/.
ln -sf $COMINm1/aqm_conus_geos_ngac_dust_${PDYm1}_35L.ncf $DATA/.
ln -sf $COMINm1/aqm.${cycle}.emission.${PDYm1}.windust_snowc.ncf $DATA/.

export GRID=$DATA/aqm.${cycle}.grdcro2d.ncf
export MCRO3=$DATA/aqm.${cycle}.metcro3d.ncf
export MDOT3=$DATA/aqm.${cycle}.metdot3d.ncf
export MCRO2=$DATA/aqm.${cycle}.metcro2d.ncf

export OUTPUT1=$DATA/aqm.${PDYm1}.${cycle}.play3d.fire.ncf
export OUTPUT2=$DATA/aqm.${PDYm1}.${cycle}.smokefire2d.ncf
export OUTPUT3=$DATA/aqm.${PDYm1}.${cycle}.smokefire3d.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

$EXECaqm/aqm_fire_analy_1  > tmpfire.out
export err=$?;
err_chk

##
## For plume-height diagnosis output
##
# to archive fire profile files 
#jp if [ "${RUN_ENVIR}" == "emc" ]; then
   archive_run=analy
   archive_dir=fire_${archive_run}_${PDYm1}_t${cyc}z
   archive_tar=fire_${archive_run}_profile_t${cyc}z.tar.gz
   mkdir -p ${archive_dir}
   mv fire_${archive_run}_profile*.txt   ${archive_dir}
   mv fire_${archive_run}_gridindx_*.txt ${archive_dir}
   cp -p tmpfire.out                     ${archive_dir}
   tar -czf ${archive_tar} ${archive_dir}
   mv ${archive_tar}                     $COMOUTm1
#jp fi
## For plume-height diagnosis output

 cat > cmaq.ini << EOF
 &control
 syear=$fyear
 smon=$fmonth
 sday=$fday
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
