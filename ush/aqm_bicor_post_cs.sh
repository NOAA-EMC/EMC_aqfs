#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_post_cs.sh 
# Script description:  used to post-process bias corrected PM2.5 
#
# Author:  
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

ln -s $COMOUT/pm2.5.corrected.${PDY}.${cyc}z.nc .

##------------------------
# convert from netcdf to grib1 format
#startmsg  
#$EXECaqm/aqm_post_bias_cor pm2.5.corrected.${PDY}.${cyc}z.nc pm25 ${PDY} $cyc 
#export err=$?;err_chk

#cp -rp $DATA/aqm.t${cyc}z.25pm* $COMOUT

#if [ -e $COMOUT_grib/$PDY ] ; then
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT_grib/$PDY 
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT
#else
# mkdir -p $COMOUT_grib/$PDY
# cp $DATA/aqm.t${cyc}z.25pm* $COMOUT_grib/$PDY
#fi

##------------------------
# convert from netcdf to grib2 format

export id_gribdmn=148
startmsg
$EXECaqm/aqm_post_bias_cor_grib2 pm2.5.corrected.${PDY}.${cyc}z.nc pm25 ${PDY} $cyc ${id_gribdmn}  >> $pgmout 2>errfile 
export err=$?;err_chk

if [ "$SENDCOM" = 'YES' ]
then
    for pmfile in $DATA/aqm.t${cyc}z.pm25*bc*.grib2;do
        ifile=$(basename ${pmfile})
        cp -rp ${ifile} $COMOUT/
        if [ "$SENDDBN" = 'YES' ]; then
            $DBNROOT/bin/dbn_alert MODEL AQM_PM $job $COMOUT/${ifile}
        fi
    done
fi


if [ -e $COMOUT_grib/${RUN}.$PDY ] ; then
 cp $DATA/aqm.t${cyc}z.pm25*bc*.grib2 $COMOUT_grib/${RUN}.$PDY
else
 mkdir -p $COMOUT_grib/${RUN}.$PDY
 cp $DATA/aqm.t${cyc}z.pm25*bc*.grib2 $COMOUT_grib/${RUN}.$PDY
fi

echo EXITING $0

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

