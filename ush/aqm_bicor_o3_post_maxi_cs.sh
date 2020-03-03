#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_cmaq_maxi.sh
# Script description:  CMAQ post processing for daily surface maximum O3/PM2.5
#
# Author:  Jianping.Huang 09/03/2017 
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

ln -s $COMOUT/ozone.corrected.${PDY}.${cyc}z.nc  a.nc 

export chk=1 
# today 00z file exists otherwise chk=0

cat >bias_cor_max.ini <<EOF1
&control
varlist='O3_1h_max','O3_8h_max'
outfile='aqm-maxi_bc'
id_gribdomain=148
/
EOF1

#outfile='aqm.${cycle}.max_o3_bc'

if [ $cyc =  '06' ]; then
 if [ -s $COMOUT/ozone.corrected.${PDY}.00z.nc ]; then
   ln -s  $COMOUT/ozone.corrected.${PDY}.00z.nc  b.nc 
 else 
   ln -s $COMOUTm1/ozone.corrected.${PDYm1}.12z.nc  b.nc
   chk=0
 fi
fi

if [ $cyc = '12' ] ; then
 if [ -s $COMOUT/ozone.corrected.${PDY}.00z.nc ]; then 
  ln -s $COMOUT/ozone.corrected.${PDY}.00z.nc  b.nc
 else
  ln -s $COMOUTm1/ozone.corrected.${PDYm1}.12z.nc  b.nc
  chk=0
 fi
  ln -s $COMOUT/ozone.corrected.${PDY}.06z.nc  c.nc
fi
#-------------------------------------------------

# write out grib2 format 
#-------------------------------------------------
rm -rf errfile
startmsg
$EXECaqm/aqm_post_maxi_bias_cor_grib2  ${PDY} $cyc $chk 
export err=$?;err_chk

# split into max_1h and max_8h files and copy to grib227

$WGRIB2 aqm-maxi_bc.148.grib2 |grep "OZMAX1" | $WGRIB2 -i aqm-maxi_bc.148.grib2 -grib  aqm.${cycle}.max_1hr_o3_bc.148.grib2
$WGRIB2 aqm-maxi_bc.148.grib2 |grep "OZMAX8" | $WGRIB2 -i aqm-maxi_bc.148.grib2 -grib  aqm.${cycle}.max_8hr_o3_bc.148.grib2

#export grid227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0"
export grid227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000"
$COPYGB2  -g "$grid227" -x  -i"1 1"  aqm.${cycle}.max_1hr_o3_bc.148.grib2 aqm.${cycle}.max_1hr_o3_bc.227.grib2
$COPYGB2  -g "$grid227" -x  -i"1 1"  aqm.${cycle}.max_8hr_o3_bc.148.grib2 aqm.${cycle}.max_8hr_o3_bc.227.grib2

#==========
#if [ "${envir}" = "para" .or. "${envir}" = "para5" ] ;
if [ "${envir}" = "para5" ] ;
then
  echo "copying to developer's personal directory"
 if [ -e $COMOUT_grib/${RUN}.$PDY ] ; then
  cp $DATA/aqm.${cycle}.max_*hr_o3_bc.*.grib2  ${COMOUT_grib}/${RUN}.$PDY
 else
  mkdir -p $COMOUT_grib/${RUN}.$PDY
  cp $DATA/aqm.${cycle}.max_*hr_o3_bc.*.grib2  ${COMOUT_grib}/${RUN}.$PDY
 fi
fi


if [ $cyc -eq 06 -o $cyc -eq 12 ] && [ "${SENDCOM}" = "YES" ]; then
 cp -rp $DATA/aqm.${cycle}.max_*hr_o3_bc.*.grib2  $COMOUT/

 if [ "$SENDDBN" = 'YES' ] ; then
  $DBNROOT/bin/dbn_alert MODEL AQM_MAX $job $COMOUT/aqm.${cycle}.max_1hr_o3_bc.227.grib2
  $DBNROOT/bin/dbn_alert MODEL AQM_MAX $job $COMOUT/aqm.${cycle}.max_8hr_o3_bc.227.grib2
 fi
fi

exit
########################################################
# Add WMO header for daily 1h and 8h max O3 
########################################################
for hr in 1 8
do
echo 0 > filesize
export XLFRTEOPTS="unit_vars=yes"
export FORT11=aqm.${cycle}.max_${hr}hr_o3_bc.227.grib2
export FORT12="filesize"
export FORT31=
export FORT51=aqm-${hr}hro3-maxi.227.grib2.temp
${TOCGRIB2SUPER} < $PARMaqm/wmo/grib2_cmaq-${hr}hro3-maxi.${cycle}.227

echo `ls -l  aqm-${hr}hro3-maxi.227.grib2.temp | awk '{print $5} '` > filesize
export XLFRTEOPTS="unit_vars=yes"
export FORT11=aqm-${hr}hro3-maxi.227.grib2.temp
export FORT12="filesize"
export FORT31=
export FORT51=awpaqm.${cycle}.${hr}ho3-max.227.grib2
${TOCGRIB2SUPER} < $PARMaqm/wmo/grib2_cmaq-${hr}hro3-maxi.${cycle}.227
done


##############################
# Post Files to PCOM
##############################

 if test "$SENDCOM" = 'YES'
 then
     cp awpaqm.${cycle}.*o3-max.227.grib2            $PCOM/

##############################
# Distribute Data
##############################
   if [ "$SENDDBN_NTC" = 'YES' ] ; then
       $DBNROOT/bin/dbn_alert $DBNALERT_TYPE $NET $job $PCOM/awpaqm.${cycle}.1ho3-max.227.grib2
       $DBNROOT/bin/dbn_alert $DBNALERT_TYPE $NET $job $PCOM/awpaqm.${cycle}.8ho3-max.227.grib2
   else
      msg="File $output_grb.$job not posted to db_net."
      postmsg "$jlogfile" "$msg"
   fi
 fi

