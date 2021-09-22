#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_post_o3_bicor_cs.sh.ecf
# Script description:  CMAQ post processing for bias-correction daily surface maximum O3/PM2.5
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

export pgm=aqm_cs_o3_bias_correct

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
    err_exit "FATAL ERROR - COULD NOT LOCATE:${COMIN}/aqm.${cycle}.aconc_sfc.ncf"
fi

#------------------------------------------------------
## remove any pre-exit aqm.${cycle}.O3_pm25.ncf sfc_met_n_PBL.${cycle}.ncf
## for 2-stage post processing [ 48 hour first then 72 hour 2nd
if [ -s $COMOUTbicor/grid/$PDY/aqm.${cycle}.O3_pm25.ncf ]; then
   echo " $COMOUTbicor/grid/$PDY/aqm.${cycle}.O3_pm25.ncf  exists ! "
   rm -f $COMOUTbicor/grid/$PDY/aqm.${cycle}.O3_pm25.ncf
   rm -f $COMOUTbicor/grid/$PDY/sfc_met_n_PBL.${cycle}.ncf
fi

   startmsg
   ${USHaqm}/aqm_bicor_prep_cs.sh  >> $pgmout 2>errfile  
   export err=$?; err_chk

   f1=aqm.${cycle}.O3_pm25.ncf
   f3=sfc_met_n_PBL.${cycle}.ncf
 
   if [ -d $COMOUTbicor/grid/$PDY ] ; then
      echo " $COMOUTbicor/grid/$PDY exits "
   else
      mkdir -p $COMOUTbicor/grid/$PDY
   fi
#
# exaqm_post_pm25_bicor_cs.sh.ecf will check the existence of $f1 before starting the BC_PM
# Thus, copy $f1 later to make sure $f3 is already available
#
    cp ${DATA}/$f3 $COMOUTbicor/grid/$PDY
    cp ${DATA}/$f1 $COMOUTbicor/grid/$PDY

    if [ "${RUN_ENVIR}" == "nco" ]; then
      ecflow_client --event release_pm25_bicor
    fi
#-----------------------------------------------------------------------
# STEP 2 :  Intepolating CMAQ PM2.5 into AIRNow sites

startmsg
${USHaqm}/aqm_bicor_o3_interp_cs.sh  >> $pgmout 2>errfile 
export err=$?; err_chk

#-----------------------------------------------------------------------
# STEP 3:  Performing Bias Correction for PM2.5 
startmsg
${USHaqm}/aqm_bicor_o3_cs.sh 
export err=$?; err_chk

#------------------------------------------------------------------------
# STEP 4:  converting netcdf to grib format
startmsg
${USHaqm}/aqm_bicor_o3_post_cs.sh   >> $pgmout 2>errfile
export err=$?; err_chk

#
#--------------------------------------------------------------
# STEP 5: calculating 24-hr ave PM2.5
if [ $cyc -eq 06 -o  $cyc -eq 12 ] ; then
startmsg
${USHaqm}/aqm_bicor_o3_post_maxi_cs.sh   
export err=$?; err_chk

fi
#
#-------------------------------------
typeset -Z2 fhr
export fhr=01

case $cyc in
 00) endfhr=06;;
 06) endfhr=${post_proc_hour};;
 12) endfhr=${post_proc_hour};;
 18) endfhr=06;;
esac

rm -rf tmpfile

while [ ${fhr} -le ${endfhr} ]
do
 if [ "${SENDCOM}" = 'YES' ]
 then

  for field in awpozcon_bc
  do
    cp ${DATA}/aqm.${cycle}.${field}.f${fhr}.${id_grib}.grib2 $COMOUT/
  done

 fi

# create GRIB file to convert to grid 227 then to GRIB2 for NDFD
  cat ${DATA}/aqm.${cycle}.awpozcon_bc.f${fhr}.${id_grib}.grib2 >> tmpfile
  if [ ${fhr} -le 07 ]; then
   cat ${DATA}/aqm.${cycle}.awpozcon_bc.f${fhr}.${id_grib}.grib2 >> tmpfile.1hr
  else
   ${WGRIB2} ${DATA}/aqm.${cycle}.awpozcon_bc.f${fhr}.${id_grib}.grib2 -d 1 -append -grib tmpfile.1hr
   export err=$?;err_chk
   ${WGRIB2} ${DATA}/aqm.${cycle}.awpozcon_bc.f${fhr}.${id_grib}.grib2 -d 2 -append -grib tmpfile.8hr
   export err=$?;err_chk
  fi
  let "fhr=fhr+1"
  typeset -Z2 fhr
done

###############
# Convert ozone Concentration to grid 227 in GRIB2 format
###############
echo ' &NLCOPYGB IDS(180)=1, /' > ozcon_scale

newgrib2file1=aqm.t${cyc}z.ave_1hr_o3_bc.227.grib2
newgrib2file2=aqm.t${cyc}z.ave_8hr_o3_bc.227.grib2

export grid227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000"

${COPYGB2}  -g "$grid227" -x  -i"1 1" tmpfile.1hr  ${newgrib2file1} 
cp tmpfile.1hr     $COMOUT/aqm.t${cyc}z.ave_1hr_o3_bc.148.grib2
cp aqm.t${cyc}z.ave_1hr_o3_bc.227.grib2 $COMOUT/

if [ $cyc -eq 06 -o $cyc -eq 12 ]  ; then
 ${COPYGB2}  -g "$grid227" -x  -i"1 1" tmpfile.8hr  ${newgrib2file2} 
 cp tmpfile.8hr     $COMOUT/aqm.t${cyc}z.ave_8hr_o3_bc.148.grib2
 cp aqm.t${cyc}z.ave_8hr_o3_bc.227.grib2 $COMOUT/
fi

if [ "${SENDDBN}" = 'YES' ] ; then
   ${DBNROOT}/bin/dbn_alert MODEL AQM_CONC ${job} $COMOUT/aqm.t${cyc}z.ave_1hr_o3_bc.227.grib2
  if [ $cyc -eq 06 -o $cyc -eq 12 ]  ; then
   ${DBNROOT}/bin/dbn_alert MODEL AQM_CONC ${job} $COMOUT/aqm.t${cyc}z.ave_8hr_o3_bc.227.grib2
  fi
fi

if [ "${RUN_ENVIR}" == "emc" ] && [ "${envir}" = 'para6z' ]; then
  echo "copying to developer's personal directory"
 if [ -e ${COMOUT_grib}/${RUN}.$PDY ] ; then
   cp ${DATA}/aqm.${cycle}.ave_*hr_o3_bc.*.grib2  ${COMOUT_grib}/${RUN}.$PDY
   cp tmpfile.1hr     ${COMOUT_grib}/${RUN}.$PDY/aqm.t${cyc}z.ave_1hr_o3_bc.148.grib2
  if [ $cyc -eq 06 -o $cyc -eq 12 ]  ; then
   cp tmpfile.8hr     ${COMOUT_grib}/${RUN}.$PDY/aqm.t${cyc}z.ave_8hr_o3_bc.148.grib2
  fi
 else
  mkdir -p ${COMOUT_grib}/${RUN}.$PDY
  cp ${DATA}/aqm.${cycle}.ave_*hr_o3_bc.227.grib2  ${COMOUT_grib}/${RUN}.$PDY
  cp tmpfile.1hr     ${COMOUT_grib}/${RUN}.$PDY/aqm.t${cyc}z.ave_1hr_o3_bc.148.grib2
  if [ $cyc -eq 06 -o $cyc -eq 12 ]  ; then
   cp tmpfile.8hr     ${COMOUT_grib}/${RUN}.$PDY/aqm.t${cyc}z.ave_8hr_o3_bc.148.grib2
  fi
 fi
fi


#################################################
#    Part III:  Insert WMO header to GRIB files
#################################################

if [ $cyc -eq 06 -o $cyc -eq 12 ] && [ "${SENDCOM}" = 'YES' ] ; then

##################################################
# Create AWIPS GRIB data for 1hr and 8hr ave ozone
##################################################
   for hr in 1 8
   do
       echo 0 > filesize
       export XLFRTEOPTS="unit_vars=yes"
       export FORT11=aqm.t${cyc}z.ave_${hr}hr_o3_bc.227.grib2
       export FORT12="filesize"
       export FORT31=
       export FORT51=grib2.t${cyc}z.awp5xozconnmmb_aqm_${hr}-bc.temp
       ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_cmaq_ave_${hr}hr_o3_bc-awpozcon.${cycle}.227
       export err=$?;err_chk

       echo `ls -l grib2.t${cyc}z.awp5xozconnmmb_aqm_${hr}-bc.temp  | awk '{print $5} '` > filesize
       export XLFRTEOPTS="unit_vars=yes"
       export FORT11=grib2.t${cyc}z.awp5xozconnmmb_aqm_${hr}-bc.temp
       export FORT12="filesize"
       export FORT31=
       export FORT51=awpaqm.t${cyc}z.${hr}ho3-bc.227.grib2
       ${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_cmaq_ave_${hr}hr_o3_bc-awpozcon.${cycle}.227
       export err=$?;err_chk

##########################################################
# Create AWIPS GRIB data for dailly 1-hr and 8hr max ozone
##########################################################
	echo 0 > filesize
	export XLFRTEOPTS="unit_vars=yes"
	export FORT11=aqm.t${cyc}z.max_${hr}hr_o3_bc.227.grib2
	export FORT12="filesize"
	export FORT31=
	export FORT51=aqm.t${cyc}z.max_${hr}hr_o3-bc.227.grib2.temp
	${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_cmaq-${hr}hro3_bc-maxi.${cycle}.227

	echo `ls -l  aqm.t${cyc}z.max_${hr}hr_o3-bc.227.grib2.temp | awk '{print $5} '` > filesize
	export XLFRTEOPTS="unit_vars=yes"
	export FORT11=aqm.t${cyc}z.max_${hr}hr_o3-bc.227.grib2.temp
	export FORT12="filesize"
	export FORT31=
	export FORT51=awpaqm.${cycle}.${hr}ho3-max-bc.227.grib2
	${TOCGRIB2SUPER} < ${PARMaqm}/wmo${post_proc_hour}/grib2_cmaq-${hr}hro3_bc-maxi.${cycle}.227

##############################
# Post Files to PCOM
##############################
       if test "${SENDCOM}" = 'YES'
       then
           cp awpaqm.t${cyc}z.${hr}ho3-bc.227.grib2  ${PCOM}/
           cp awpaqm.t${cyc}z.${hr}ho3-max-bc.227.grib2  ${PCOM}/

##############################
# Distribute Data
##############################
        if [ "${SENDDBN}" = 'YES' ] ; then
          ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.t${cyc}z.${hr}ho3-bc.227.grib2
          ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.t${cyc}z.${hr}ho3-max-bc.227.grib2
        else
          msg="File output_grb.${job} not posted to db_net."
          postmsg "$msg"
        fi
       fi
   done
fi
#######################################################
msg='ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
exit
        

