#!/bin/ksh
######################################################################

#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_post_pm25_bicor_cs.sh.ecf
# Script description:  CMAQ post processing for bias_correction daily surface maximum O3/PM2.5
#
# Author:  Jianping Huang 06/29/2015 
#  8/14/2017
#
#
# H.-C. Huang  Oct 21 2019   Remove the lines of Spec_humid.${cycle}.ncf and UV_wind.${cycle}.ncf
#                            Per email instruction of Dave Allure ESRL/GSD on October 21 2019
# H.-C. Huang  Mar 18 2021   Implement new bias correction package suitable for both 48 and 72 hours
#                            from Jianping's version
# H.-C. Huang  Mar 18 2021   Modified code for 2-stage pos processing output.
#                            Allow aqm.${cycle}.O3_pm25.ncf and sfc_met_n_PBL.${cycle}.ncf
#                            to be overwritten later by the one derived
#                            from 72 hours forecast output
#
######################################################################
set -xa

export pgm=aqm_cs_pm25_bias_correct

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}
export id_grib=148

case $cyc in
 00) export bc_interp_hr=06;;
 06) export bc_interp_hr=72;;
 12) export bc_interp_hr=72;;
 18) export bc_interp_hr=06;;
esac

cd ${DATA}
#-----------------------------------------------------------------------------
# STEP 1:  Extracting PM2.5, O3, and met variables from CMAQ input and outputs

ic=1
while [ $ic -lt 120 ]; do
   if [ -s ${COMIN}/aqm.t${cyc}z.aconc_sfc.ncf ]; then
      echo "cycle ${cyc} post1 is done!"
      break 
   else
      let "ic=ic+1"
      sleep 10
    fi
done

if [ $ic -ge 120 ]; then
    err_exit "****FATAL ERROR***** - COULD NOT LOCATE:${COMIN}/aqm.${cycle}.aconc_sfc.ncf"
fi

##
## For 2 stage post-processing
## PM bias correction always need to wait till O3 bias produce ${COMOUTbicor}/grid/${PDY}/aqm.${cycle}.O3_pm25.ncf
## To avoid 48 hour file previously generated been used by 72 hour processing
##
   ic=1
   while [ $ic -lt 120 ]; do
      if [ -s ${COMOUTbicor}/grid/${PDY}/aqm.${cycle}.O3_pm25.ncf ]; then
         echo " ${COMOUTbicor}/grid/${PDY}/aqm.${cycle}.O3_pm25.ncf  exists ! "
         break
      else
         ((ic++))
         sleep 10
      fi
   done

   if [ $ic -ge 120 ]; then
      err_exit "****FATAL ERROR***** - COULD NOT LOCATE:${COMOUTbicor}/grid/${PDY}/aqm.${cycle}.O3_pm25.ncf"
   fi

#
#-----------------------------------------------------------------------
# STEP 2 :  Intepolating CMAQ PM2.5 into AIRNow sites

startmsg
${USHaqm}/aqm_bicor_pm25_interp_cs.sh  >> $pgmout 2>errfile 
export err=$?; err_chk

#-----------------------------------------------------------------------
# STEP 3:  Performing Bias Correction for PM2.5 
startmsg
${USHaqm}/aqm_bicor_pm25_cs.sh 
export err=$?; err_chk
#------------------------------------------------------------------------
#
# STEP 4:  converting netcdf to grib format
startmsg
${USHaqm}/aqm_bicor_post_cs.sh   >> $pgmout 2>errfile
export err=$?; err_chk

#
#--------------------------------------------------------------
# STEP 5: calculating 24-hr ave PM2.5
if [ ${cyc} -eq 06 -o  ${cyc} -eq 12 ] ; then
startmsg
${USHaqm}/aqm_bicor_post_maxi_cs.sh   >> $pgmout 2>errfile
export err=$?; err_chk

# interpolate to grid 227

oldgrib2file1=aqm.t${cyc}z.ave_24hr_pm25_bc.148.grib2
newgrib2file1=aqm.t${cyc}z.ave_24hr_pm25_bc.227.grib2
export grid227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000"
${COPYGB2}  -g "$grid227" -x  -i"1 1"  ${oldgrib2file1}  ${newgrib2file1} 

oldgrib2file2=aqm.t${cyc}z.max_1hr_pm25_bc.148.grib2
newgrib2file2=aqm.t${cyc}z.max_1hr_pm25_bc.227.grib2
${COPYGB2}  -g "$grid227" -x  -i"1 1"  ${oldgrib2file2}  ${newgrib2file2}

if [ "$envir" = "para6z" ] ; then
   cp aqm.${cycle}.max_1hr_pm25_bc.148.grib2   ${COMOUT_grib}/${RUN}.${PDY}
   cp aqm.${cycle}.ave_24hr_pm25_bc.148.grib2  ${COMOUT_grib}/${RUN}.${PDY}
   cp aqm.${cycle}.max_1hr_pm25_bc.227.grib2   ${COMOUT_grib}/${RUN}.${PDY}
   cp aqm.${cycle}.ave_24hr_pm25_bc.227.grib2  ${COMOUT_grib}/${RUN}.${PDY}
fi

if [ "${SENDCOM}" = "YES" ]; then
   cp aqm.${cycle}.max_1hr_pm25_bc.148.grib2   ${COMOUT}/
   cp aqm.${cycle}.ave_24hr_pm25_bc.148.grib2  ${COMOUT}/
   cp aqm.${cycle}.max_1hr_pm25_bc.227.grib2   ${COMOUT}/
   cp aqm.${cycle}.ave_24hr_pm25_bc.227.grib2  ${COMOUT}/
  if [ ${SENDDBN} = YES ]; then
     ${DBNROOT}/bin/dbn_alert MODEL AQM_MAX ${job} ${COMOUT}/aqm.t${cyc}z.max_1hr_pm25_bc.227.grib2
     ${DBNROOT}/bin/dbn_alert MODEL AQM_PM ${job} ${COMOUT}/aqm.t${cyc}z.ave_24hr_pm25_bc.227.grib2
  fi
fi

fi

#############################################
export fhr=01
typeset -Z2 fhr

case ${cyc} in
 00) endfhr=06;;
 06) endfhr=${post_proc_hour};;
 12) endfhr=${post_proc_hour};;
 18) endfhr=06;;
esac

while [ ${fhr} -le ${endfhr} ]
do
 cat ${DATA}/aqm.${cycle}.pm25_bc.f${fhr}.${id_grib}.grib2 >> tmpfile_pm25_bc
 let "fhr=fhr+1"
 typeset -Z2 fhr
done

export grid227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000"
${COPYGB2}  -g "$grid227" -x  -i"1 1" tmpfile_pm25_bc aqm.t${cyc}z.grib2_pm25_bc.227

if [ ${SENDCOM} = YES ]; then
  cp  tmpfile_pm25_bc  ${COMOUT}/aqm.t${cyc}z.ave_1hr_pm25_bc.148.grib2
  cp  aqm.t${cyc}z.grib2_pm25_bc.227 ${COMOUT}/aqm.t${cyc}z.ave_1hr_pm25_bc.227.grib2
  if [ ${SENDDBN} = YES ]; then
     ${DBNROOT}/bin/dbn_alert MODEL AQM_PM ${job} ${COMOUT}/aqm.t${cyc}z.ave_1hr_pm25_bc.227.grib2
  fi
fi

#
#--------------------------------------------------------------
# STEP 6: adding WMO header  
# Create AWIPS GRIB2 data for Bias-Corrected PM2.5
###################################################
## file aqm.t${cyc}z.grib2_pm25_bc.227 is aqm.t${cyc}z.ave_1hr_pm25_bc.227.grib2
if [ ${cyc} -eq 06 -o ${cyc} -eq 12 ] && [ "${SENDCOM}" = "YES" ] ; then
  echo 0 > filesize
  export XLFRTEOPTS="unit_vars=yes"
  export FORT11=aqm.t${cyc}z.grib2_pm25_bc.227
  export FORT12="filesize"
  export FORT31=
  export FORT51=aqm.t${cyc}z.grib2_pm25_bc.227.temp
  ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_pm25_bc.${cycle}.227
  export err=$?;err_chk
#
  echo `ls -l aqm.t${cyc}z.grib2_pm25_bc.227.temp  | awk '{print $5} '` > filesize
  export XLFRTEOPTS="unit_vars=yes"
  export FORT11=aqm.t${cyc}z.grib2_pm25_bc.227.temp
  export FORT12="filesize"
  export FORT31=
  export FORT51=awpaqm.t${cyc}z.1hpm25-bc.227.grib2
  ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_pm25_bc.${cycle}.227
  export err=$?;err_chk

####################################################
 rm -rf filesize
 echo 0 > filesize
 export XLFRTEOPTS="unit_vars=yes"
 export FORT11=aqm.${cycle}.max_1hr_pm25_bc.227.grib2
 export FORT12="filesize"
 export FORT31=
 export FORT51=aqm.${cycle}.max_1hr_pm25_bc.227.grib2.temp
 ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_max_1hr_pm25_bc.${cycle}.227

 echo `ls -l  aqm.${cycle}.max_1hr_pm25_bc.227.grib2.temp | awk '{print $5} '` > filesize
 export XLFRTEOPTS="unit_vars=yes"
 export FORT11=aqm.${cycle}.max_1hr_pm25_bc.227.grib2.temp
 export FORT12="filesize"
 export FORT31=
 export FORT51=awpaqm.${cycle}.daily-1hr-pm25-max-bc.227.grib2
 ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_max_1hr_pm25_bc.${cycle}.227

 rm filesize
#  daily_24hr_ave_PM2.5
 echo 0 > filesize
 export XLFRTEOPTS="unit_vars=yes"
 export FORT11=aqm.${cycle}.ave_24hr_pm25_bc.227.grib2
 export FORT12="filesize"
 export FORT31=
 export FORT51=aqm.${cycle}.ave_24hr_pm25_bc.227.grib2.temp
 ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_ave_24hrpm25_bc_awp.${cycle}.227

 echo `ls -l  aqm.${cycle}.ave_24hr_pm25_bc.227.grib2.temp | awk '{print $5} '` > filesize
 export XLFRTEOPTS="unit_vars=yes"
 export FORT11=aqm.${cycle}.ave_24hr_pm25_bc.227.grib2.temp
 export FORT12="filesize"
 export FORT31=
 export FORT51=awpaqm.${cycle}.24hr-pm25-ave-bc.227.grib2
 ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_aqm_ave_24hrpm25_bc_awp.${cycle}.227

##############################
# Post Files to PCOM
##############################
 if test "${SENDCOM}" = "YES"
   then
     cp awpaqm.${cycle}.1hpm25-bc.227.grib2             ${PCOM}/
     cp awpaqm.${cycle}.daily-1hr-pm25-max-bc.227.grib2 ${PCOM}/
     cp awpaqm.${cycle}.24hr-pm25-ave-bc.227.grib2      ${PCOM}/

##############################`
# Distribute Data
##############################

     if [ "${SENDDBN_NTC}" = "YES" ] ; then
       ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.1hpm25-bc.227.grib2
       ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.daily-1hr-pm25-max-bc.227.grib2
       ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.24hr-pm25-ave-bc.227.grib2
     else
          msg="File output_grb.${job} not posted to db_net."
          postmsg "$msg"
     fi
  fi
fi
#######################################################
msg='ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
exit

