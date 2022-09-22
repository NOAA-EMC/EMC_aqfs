#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_post1_cb6r3_ak.maxi.sh.ecf
# Script description:  CMAQ post processing for daily surface maximum O3/PM2.5
#
# Youhua Tang  Org: NP22  Date: 2009-06-30
# Jianping Huang : 10/1/2016
#          Change grib1 to grib2 
#          added new products for PM2.5 with WMO Headers  
# Ho-Chun Huang   12/17/2020  revised for v5.3.1
######################################################################
set -xa

msg="JOB ${job} HAS BEGUN"
postmsg   "${msg}"

export pgm=aqm_ak_post1_maxi

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}

cd ${DATA}

while [ ! -s ${COMIN}/aqm.${cycle}.aconc_sfc.ncf ]; do
   sleep 120    # wait until the CMAQ output is ready
done

flag_run_maxi=yes
export CMAQFILE1=${COMIN}/aqm.${cycle}.aconc_sfc.ncf
if [ -s ${COMIN}/aqm.t00z.aconc_sfc.ncf ] ; then 
   export CMAQFILE2=${COMIN}/aqm.t00z.aconc_sfc.ncf
elif [ -s ${COMINm1}/aqm.t12z.aconc_sfc.ncf ];  then 
   export CMAQFILE2=${COMINm1}/aqm.t12z.aconc_sfc.ncf
elif [ -s ${COMINm2}/aqm.t12z.aconc_sfc.ncf ];  then
   export CMAQFILE2=${COMINm2}/aqm.t12z.aconc_sfc.ncf
else  ## 06z and 12z both need CMAQFILE2 to find current day output from 04Z to 06Z
   echo "WARNING :: ERROR can not find CMAQFILE2 for daily MAX AVE computatuion"
   flag_run_maxi=no
fi

if [ ${cyc} -eq 06 ]; then   ## 06z does not need CMAQFILE3 to find current day output from 04Z
   if [ -s ${COMIN}/aqm.t00z.aconc_sfc.ncf ] ; then
      export CMAQFILE3=${COMIN}/aqm.t00z.aconc_sfc.ncf
   elif [ -s ${COMINm1}/aqm.t12z.aconc_sfc.ncf ] ; then
      export CMAQFILE3=${COMINm1}/aqm.t12z.aconc_sfc.ncf
   elif [ -s ${COMINm2}/aqm.t12z.aconc_sfc.ncf ] ; then
      export CMAQFILE3=${COMINm2}/aqm.t12z.aconc_sfc.ncf
   fi
fi
if [ ${cyc} -eq 12 ]; then
   if [ -s ${COMIN}/aqm.t06z.aconc_sfc.ncf ] ; then 
      export CMAQFILE3=${COMIN}/aqm.t06z.aconc_sfc.ncf
   elif [ -s ${COMINm1}/aqm.t12z.aconc_sfc.ncf ] ; then
      export CMAQFILE3=${COMINm1}/aqm.t12z.aconc_sfc.ncf
   elif [ -s ${COMINm2}/aqm.t12z.aconc_sfc.ncf ] ; then
      export CMAQFILE3=${COMINm2}/aqm.t12z.aconc_sfc.ncf
   else  ## 12z needs CMAQFILE3 to find current day output from 07Z to 12z
      echo "WARNING :: ERROR can not find CMAQFILE3 for daily MAX AVE computatuion"
      flag_run_maxi=no
   fi
fi

if [ "${flag_run_maxi}" == "yes" ]; then
cat >cmaq-maxi2grib.ini <<EOF5
&control
markutc=05
outfile='aqm-maxi.140.grib2'
varlist='o3_1hr','o3_8hr'
id_gribdomain=140
/
EOF5
   
   startmsg
   ${EXECaqm}/aqm_post_maxi_CHA_grib2_v2   >> ${pgmout} 2>errfile
   export err=$?;err_chk
   
   # interpolate to grid 198
   
   
   ${WGRIB2} aqm-maxi.140.grib2 |grep "OZMAX1" | ${WGRIB2} -i aqm-maxi.140.grib2 -grib  aqm.${cycle}.1ho3-max.140.grib2
   ${WGRIB2} aqm-maxi.140.grib2 |grep "OZMAX8" | ${WGRIB2} -i aqm-maxi.140.grib2 -grib  aqm.${cycle}.8ho3-max.140.grib2
   
   export grid198="20 6 0 0 0 0 0 0 825 553 40530000 181429000 8  60000000 210000000 5953000 5953000 0 64 25000000 25000000"
   ${COPYGB2}  -g "${grid198}" -x  -i"1 1"  aqm-maxi.140.grib2  aqm-maxi.198.grib2 
   ${WGRIB2} aqm-maxi.198.grib2 |grep "OZMAX1" | ${WGRIB2} -i aqm-maxi.198.grib2 -grib  aqm-1hro3-maxi.198.grib2
   ${WGRIB2} aqm-maxi.198.grib2 |grep "OZMAX8" | ${WGRIB2} -i aqm-maxi.198.grib2 -grib  aqm-8hro3-maxi.198.grib2
   
   # write out pm2.5 in grib2 format
   
cat >cmaq-maxi2grib.ini <<EOF5
&control
markutc=05
outfile='aqm-pm25_24hr.140.grib2'
varlist='pm25_1hr','pm25_24hr'
id_gribdomain=140
/
EOF5
   
   startmsg
   ${EXECaqm}/aqm_post_maxi_CHA_grib2_v2  >> ${pgmout} 2>errfile
   export err=$?;err_chk
   
   ${COPYGB2}  -g "${grid198}" -x  -i"1 1" aqm-pm25_24hr.140.grib2 aqm-pm25_24hr.198.grib2
   
   #-----------------------------------------
   ${WGRIB2} aqm-pm25_24hr.140.grib2 |grep "PDMAX1" | ${WGRIB2} -i aqm-pm25_24hr.140.grib2 -grib aqm.${cycle}.1hpm25-max.140.grib2
   export err=$?;err_chk
   
   ${WGRIB2} aqm-pm25_24hr.140.grib2 |grep "PMTF" | ${WGRIB2} -i aqm-pm25_24hr.140.grib2 -grib  aqm.${cycle}.24hpm25-ave.140.grib2
   export err=$?;err_chk
   #-----------------------------------------
   ${WGRIB2} aqm-pm25_24hr.198.grib2 |grep "PDMAX1" | ${WGRIB2} -i aqm-pm25_24hr.198.grib2 -grib aqm.${cycle}.1hpm25-max.198.grib2
   export err=$?;err_chk
   ${WGRIB2} aqm-pm25_24hr.198.grib2 |grep "PMTF"   | ${WGRIB2} -i aqm-pm25_24hr.198.grib2 -grib aqm.${cycle}.24hrpm25-ave.198.grib2
   export err=$?;err_chk
   
   ## file aqm-1hro3-maxi.198.grib2             is aqm.${cycle}.max_1hr_o3.198.grib2
   ## file aqm-8hro3-maxi.198.grib2             is aqm.${cycle}.max_8hr_o3.198.grib2
   for hr in 1 8
   do
   
   echo 0 > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm-${hr}hro3-maxi.198.grib2
   export FORT12="filesize"
   export FORT31=
   export FORT51=aqm-${hr}hro3-maxi.198.grib2.temp
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm-${hr}hro3-maxi.${cycle}.198
   
   echo `ls -l  aqm-${hr}hro3-maxi.198.grib2.temp | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm-${hr}hro3-maxi.198.grib2.temp
   export FORT12="filesize"
   export FORT31=
   export FORT51=awpaqm.${cycle}.${hr}ho3-max.198.grib2
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm-${hr}hro3-maxi.${cycle}.198
   
   done 
   ########################################################
   # Add WMO header for daily 1h PM2.5 and 24hr_ave PM2.5
   ########################################################
   ## file aqm.${cycle}.1hpm25-max.198.grib2    is aqm.${cycle}.max_1hr_pm25.198.grib2
   #  daily_1hr_max_PM2.5
   rm -rf filesize
   echo 0 > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm.${cycle}.1hpm25-max.198.grib2
   export FORT12="filesize"
   export FORT31=
   export FORT51=aqm.${cycle}.max_1hr_pm25.198.grib2.temp
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_max_1hr_pm25.${cycle}.198
   
   echo `ls -l  aqm.${cycle}.max_1hr_pm25.198.grib2.temp | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm.${cycle}.max_1hr_pm25.198.grib2.temp
   export FORT12="filesize"
   export FORT31=
   export FORT51=awpaqm.${cycle}.daily-1hr-pm25-max.198.grib2
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_max_1hr_pm25.${cycle}.198
   
   
   rm filesize
   ## file aqm.${cycle}.24hrpm25-ave.198.grib2  is aqm.${cycle}.ave_24hr_pm25.198.grib2
   #  daily_24hr_ave_PM2.5
   echo 0 > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm.${cycle}.24hrpm25-ave.198.grib2
   export FORT12="filesize"
   export FORT31=
   export FORT51=aqm.${cycle}.24hrpm25-ave.198.grib2.temp
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_ave_24hrpm25_awp.${cycle}.198
   
   echo `ls -l  aqm.${cycle}.24hrpm25-ave.198.grib2.temp | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm.${cycle}.24hrpm25-ave.198.grib2.temp
   export FORT12="filesize"
   export FORT31=
   export FORT51=awpaqm.${cycle}.24hr-pm25-ave.198.grib2
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_ave_24hrpm25_awp.${cycle}.198
   
   
          ##############################
          # Post Files to PCOM
          ##############################
   
          if test "${SENDCOM}" = 'YES'
          then
              cp awpaqm.${cycle}.*o3-max.198.grib2            ${PCOM}/
              cp awpaqm.${cycle}.daily-1hr-pm25-max.198.grib2 ${PCOM}/ 
              cp awpaqm.${cycle}.24hr-pm25-ave.198.grib2      ${PCOM}/ 
   
             ##############################
             # Distribute Data
             ##############################
   
             if [ "${SENDDBN_NTC}" = "YES" ] ; then
                ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.1ho3-max.198.grib2
                ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.8ho3-max.198.grib2
                ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.daily-1hr-pm25-max.198.grib2
                ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.${cycle}.24hr-pm25-ave.198.grib2
             else
                msg="File output_grb.${job} not posted to db_net."
                postmsg   "${msg}"
             fi
          fi
   
   if [ ${cyc} -eq 06 -o ${cyc} -eq 12 ] && [ "${SENDCOM}" = "YES" ]; then
      cp aqm-1hro3-maxi.198.grib2             ${COMOUT}/aqm.${cycle}.max_1hr_o3.198.grib2
      cp aqm-8hro3-maxi.198.grib2             ${COMOUT}/aqm.${cycle}.max_8hr_o3.198.grib2
      cp aqm.${cycle}.1ho3-max.140.grib2      ${COMOUT}/aqm.${cycle}.max_1hr_o3.140.grib2
      cp aqm.${cycle}.8ho3-max.140.grib2      ${COMOUT}/aqm.${cycle}.max_8hr_o3.140.grib2
      cp aqm.${cycle}.1hpm25-max.198.grib2    ${COMOUT}/aqm.${cycle}.max_1hr_pm25.198.grib2
      cp aqm.${cycle}.24hrpm25-ave.198.grib2  ${COMOUT}/aqm.${cycle}.ave_24hr_pm25.198.grib2
      cp aqm.${cycle}.1hpm25-max.140.grib2    ${COMOUT}/aqm.${cycle}.max_1hr_pm25.140.grib2
      cp aqm.${cycle}.24hpm25-ave.140.grib2   ${COMOUT}/aqm.${cycle}.ave_24hr_pm25.140.grib2
      if [ "${SENDDBN}" = 'YES' ] ; then
         ${DBNROOT}/bin/dbn_alert MODEL AQM_MAX ${job} ${COMOUT}/aqm.${cycle}.max_1hr_o3.198.grib2
         ${DBNROOT}/bin/dbn_alert MODEL AQM_MAX ${job} ${COMOUT}/aqm.${cycle}.max_8hr_o3.198.grib2
         ${DBNROOT}/bin/dbn_alert MODEL AQM_MAX ${job} ${COMOUT}/aqm.${cycle}.max_1hr_pm25.198.grib2
         ${DBNROOT}/bin/dbn_alert MODEL AQM_MAX ${job} ${COMOUT}/aqm.${cycle}.ave_24hr_pm25.198.grib2
      fi
   fi
else
   echo "MISSING aconc_sfc.ncf of previous cycle and can not run ${pgm}"
   echo "It is possible that this is an initial run for a new experiemnt, require manauel check"
fi
