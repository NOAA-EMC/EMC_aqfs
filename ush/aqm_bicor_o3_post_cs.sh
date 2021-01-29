#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_bicor_post_cs.sh 
# Script description:  used to post-process bias corrected O3 
#
# Author:  Jianping Huang 09/02/2017
#
######################################################################
set -xa

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd ${DATA}

if [ -e ${DATA}/out ] ;
then
 echo "${DATA}/out exits !"
else
 mkdir -p ${DATA}/out 
fi

ln -s ${COMOUT}/ozone.corrected.${PDY}.${cyc}z.nc .

#
cat >bias_cor.ini <<EOF1
&control
varlist='O3','O3_8hr'
infile='ozone.corrected.${PDY}.${cyc}z.nc'
outfile='aqm.${cycle}.awpozcon_bc'
id_gribdomain=148
/
EOF1

##------------------------
# convert from netcdf to grib2 format

#export id_gribdmn=148
startmsg
#$EXECaqm/aqm_post_bias_cor_grib2 pm2.5.corrected.${PDY}.${cyc}z.nc pm25 ${PDY} $cyc ${id_gribdmn} 
${EXECaqm}/aqm_post_bias_cor_grib2 ${PDY} $cyc 
export err=$?;err_chk

if [ "${SENDCOM}" = 'YES' ]
then
    for pmfile in ${DATA}/aqm.t${cyc}z.awpozcon*bc*.grib2;do
        ifile=$(basename ${pmfile})
        cp -rp ${ifile} ${COMOUT}/
        # JY - remove all ozone hourly /com data alert  - 11/05/201
        # if [ "$SENDDBN" = 'YES' ]; then
        #    $DBNROOT/bin/dbn_alert MODEL AQM_PM $job ${COMOUT}/${ifile}
        # fi
    done
fi

#if [ "${envir}" = "para" .or. "${envir}" = "para5" ] ;
if [ "${envir}" = "para6b" ] 
then
  echo "copying to developer's personal directory"
 if [ -e ${COMOUT_grib}/${RUN}.${PDY} ] ; then
  cp ${DATA}/aqm.t${cyc}z.awpozcon*bc*.grib2 ${COMOUT_grib}/${RUN}.${PDY}
 else
  mkdir -p ${COMOUT_grib}/${RUN}.${PDY}
  cp ${DATA}/aqm.t${cyc}z.awpozcon*bc*.grib2 ${COMOUT_grib}/${RUN}.${PDY}
 fi
fi
echo EXITING $0

########################################################

msg='ENDED NORMALLY.'
postmsg "${jlogfile}" "${msg}"

