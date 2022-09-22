#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_post1_cb6r3_hi.sh.ecf
# Script description:  Run CMAQ post processing 
#
# Author:  Marina Tsidulko and Pius Lee  Org: NP22  Date: 2004-03-31
#
# Abstract: This script runs CMAQ post processing
#
# Script history log:
# 2003-07-03    Marina Tsidulko
# 2003-07-15    Julia Zhu, modified for production
# 2004-03-31    Pius Lee, notcdf and upgrades 
# 2010-02-01    Jianping Huang  camq2grib and wgrib 
# 2013-06-29    Jianping Huang  modified for WCOSS 
# 2017-01-21    Jianping Huang  modified for CMAQ v5.0.2 on Cray 
# 2020-05-08    Youhua Tang  revise it for CMAQ 5.3.1
# 2020-12-17    Ho-Chun Huang revise it for CMAQ 5.3.1 for implementation
######################################################################
set -xa
msg="JOB ${job} HAS BEGUN"
postmsg   "${msg}"

export pgm=aqm_hi_post1

cd ${DATA}

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}
export XLFRTEOPTS="unit_vars=yes"
export id_grib=139

#########################################################
# Part I: Calculate PM2.5 based on CMAQ original output 
#########################################################

startmsg
rm -rf ${COMOUT}/aqm.${cycle}.aconc_sfc.ncf

export SPECIES_DEF=${PARMaqm}/SpecDef_cb6r3_ae7_sfc_subset.txt
export INFILE1=${COMOUT}/aqm.${cycle}.aconc.ncf
if [ ! -s ${INFILE1} ]; then
   err_exit "****FATAL ERROR**** - COULD NOT LOCATE: ${INFILE1}"
fi
export INFILE2=${COMOUT}/aqm.${cycle}.metcro3d.ncf
export INFILE3=${COMOUT}/aqm.${cycle}.apmdiag.ncf
export INFILE4=${COMOUT}/aqm.${cycle}.metcro2d.ncf

export OUTFILE=${COMOUT}/aqm.${cycle}.aconc_sfc.ncf

${EXECaqm}/aqm_combine_v531
export err=$?;err_chk

#########################################################
# Part II: Convert Machine netcdf format to Grib format
#	  input file is "${OUTFILE}"
#########################################################

if [ -s ${OUTFILE} ]; then
   ln -sf ${OUTFILE} ${DATA}/
else  
   err_exit "****FATAL ERROR**** - COULD NOT LOCATE: ${OUTFILE}"
fi

#-------------
. prep_step

export CHEM3D=${OUTFILE}

cat >cmaq2grib2.ini <<EOF5
&control
varlist='O3','O3_8hr'
metlist='  '
outfile='aqm.${cycle}.awpozcon'
ozonecatfile='aqm.${cycle}.awpozcat'
nlayers=1
id_gribdomain=139
ave1hr=.true.
/
EOF5

startmsg
${EXECaqm}/aqm_cmaq2grib2_v2 >> ${pgmout} 2>errfile
export err=$?;err_chk

# write out pm2.5 in  grib2 format

rm -rf cmaq2grib2.ini

cat >cmaq2grib2.ini <<EOF5
&control
varlist='PM2.5'
metlist='    '
outfile='aqm.${cycle}.pm25'
nlayers=1
id_gribdomain=139
ave1hr=.true.
/
EOF5

startmsg
${EXECaqm}/aqm_cmaq2grib2_v2 >> ${pgmout} 2>errfile
export err=$?;err_chk

#
cat >cmaq2grib2.ini <<EOF5
&control
varlist='O3','NO','NO2','NOY','VOC','PM25_TOT','PM25_EC','PM25_NH4','PM25_NO3','PM25_OC','PM25_SO4','PMC_TOT'
metlist='    '
outfile='aqm.${cycle}.chem_sfc'
nlayers=1
id_gribdomain=139
ave1hr=.true.
/
EOF5

startmsg
${EXECaqm}/aqm_cmaq2grib2_v3 >> ${pgmout} 2>errfile
export err=$?;err_chk

typeset -Z2 fhr
export fhr=01

case ${cyc} in
   00) endfhr=06;;
   06) endfhr=72;;
   12) endfhr=72;;
   18) endfhr=06;;
esac

rm -rf tmpfile

if [ "${SENDCOM}" == "YES" ]; then
   for pmfile in ${DATA}/aqm.${cycle}.pm25*; do
      ifile=$(basename ${pmfile})
      cp ${ifile} ${COMOUT}/
   done
fi

while [ ${fhr} -le ${endfhr} ]; do
   if [ "${SENDCOM}" == "YES" ]; then
      for field in awpozcon awpozcat chem_sfc; do
         cp ${DATA}/aqm.${cycle}.${field}.f${fhr}.${id_grib}.grib2 ${COMOUT}/
      done
   fi
#
# Create GRIB file to convert to grid 196 then to GRIB2 for NDFD
# ALERT HHC : tmpfile.8hr only be produced for 06z and 12z where fhr > 07
#
   cat ${DATA}/aqm.${cycle}.awpozcon.f${fhr}.${id_grib}.grib2 >> tmpfile
   cat ${DATA}/aqm.${cycle}.pm25.f${fhr}.${id_grib}.grib2 >> tmpfile_pm25

   if [ ${fhr} -le 07 ]; then
      cat ${DATA}/aqm.${cycle}.awpozcon.f${fhr}.${id_grib}.grib2 >> tmpfile.1hr
   else
      ${WGRIB2} ${DATA}/aqm.${cycle}.awpozcon.f${fhr}.${id_grib}.grib2 -d 1 -append -grib tmpfile.1hr
      export err=$?;err_chk
      ${WGRIB2} ${DATA}/aqm.${cycle}.awpozcon.f${fhr}.${id_grib}.grib2 -d 2 -append -grib tmpfile.8hr
      export err=$?;err_chk
   fi

   let "fhr=fhr+1"
   typeset -Z2 fhr
done

#################################################
# Part III:  Insert WMO header to GRIB files
#################################################
## Follow OPERAIONAL cmaq.v5.1.9 operational the if loop started here for AK and HI
if [ $cyc -eq 06 -o $cyc -eq 12 ] && [ "$SENDCOM" = "YES" ] ; then

###############
# Convert ozone Concentration to grid 196 in GRIB2 format
###############
echo ' &NLCOPYGB IDS(180)=1, /' > ozcon_scale
 
export grid196="10 6 0 0 0 0 0 0 321 225 18073000 198475000 56 20000000 23088000 206131000 64 0 2500000 2500000"
${COPYGB2} -g "${grid196}" -x -i"1 1" tmpfile      aqm.t${cyc}z.grib2_${RUN}ozconnmmb.196
${COPYGB2} -g "${grid196}" -x -i"1 1" tmpfile_pm25 aqm.t${cyc}z.grib2_${RUN}pm25nmmb.196
${COPYGB2} -g "${grid196}" -x -i"1 1" tmpfile.1hr  aqm.t${cyc}z.ave_1hr_o3.196.grib2  
flag_8hr_o3=no
if [ -s tmpfile.8hr ]; then
   flag_8hr_o3=yes
   ${COPYGB2} -g "${grid196}" -x -i"1 1" tmpfile.8hr  aqm.t${cyc}z.ave_8hr_o3.196.grib2  
fi

cp aqm.t${cyc}z.ave_1hr_o3.196.grib2 ${COMOUT}/
if [ "${flag_8hr_o3}" == "yes" ]; then
   cp aqm.t${cyc}z.ave_8hr_o3.196.grib2 ${COMOUT}/
fi
cp aqm.t${cyc}z.grib2_${RUN}pm25nmmb.196 ${COMOUT}/aqm.t${cyc}z.ave_1hr_pm25.196.grib2

if [ "${SENDDBN}" == "YES" ]; then
   ${DBNROOT}/bin/dbn_alert MODEL AQM_CONC ${job} ${COMOUT}/aqm.t${cyc}z.ave_1hr_o3.196.grib2
   ${DBNROOT}/bin/dbn_alert MODEL AQM_CONC ${job} ${COMOUT}/aqm.t${cyc}z.ave_1hr_pm25.196.grib2
   if [ "${flag_8hr_o3}" == "yes" ]; then
      ${DBNROOT}/bin/dbn_alert MODEL AQM_CONC ${job} ${COMOUT}/aqm.t${cyc}z.ave_8hr_o3.196.grib2
   fi
fi
   
##############################
# Create AWIPS GRIB data
##############################

   for hr in 1 8; do
       echo 0 > filesize
       export XLFRTEOPTS="unit_vars=yes"
       export FORT11=aqm.t${cyc}z.ave_${hr}hr_o3.196.grib2
       export FORT12="filesize"
       export FORT31=
       export FORT51=grib2.t${cyc}z.awp${RUN}ozconnmmb_aqm_${hr}.temp
       ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_ave_${hr}hr_o3-awpozcon.${cycle}.196
       export err=$?;err_chk
#
       echo `ls -l grib2.t${cyc}z.awp${RUN}ozconnmmb_aqm_${hr}.temp  | awk '{print $5} '` > filesize
       export XLFRTEOPTS="unit_vars=yes"
       export FORT11=grib2.t${cyc}z.awp${RUN}ozconnmmb_aqm_${hr}.temp
       export FORT12="filesize"
       export FORT31=
       export FORT51=awpaqm.t${cyc}z.${hr}ho3.196.grib2
       ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_ave_${hr}hr_o3-awpozcon.${cycle}.196
       export err=$?;err_chk

       ##############################
       # Post Files to PCOM
       ##############################

       if [ "${SENDCOM}" == "YES" ]; then
           cp awpaqm.t${cyc}z.${hr}ho3.196.grib2  ${PCOM}/

          ##############################
          # Distribute Data
          ##############################

          if [ "${SENDDBN}" == "YES" ]; then
             ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.t${cyc}z.${hr}ho3.196.grib2
          else
             msg="File output_grb.${job} not posted to db_net."
             postmsg   "${msg}"
          fi
       fi
   done
## fi
#########################################
#  Create AWIPS GRIB2 data for PM2.5
#########################################
   cp ${DATA}/aqm.t${cyc}z.grib2_${RUN}pm25nmmb.196  ${COMOUT}/

   ## file aqm.t${cyc}z.grib2_${RUN}pm25nmmb.196 is aqm.t${cyc}z.ave_1hr_pm25.196.grib2
   echo 0 > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=aqm.t${cyc}z.grib2_${RUN}pm25nmmb.196
   export FORT12="filesize"
   export FORT31=
   export FORT51=grib2.t${cyc}z.awp${RUN}pm25nmmb.temp
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_${RUN}pm25nmmb.${cycle}.196
   export err=$?;err_chk
#
   echo `ls -l grib2.t${cyc}z.awp${RUN}pm25nmmb.temp  | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=grib2.t${cyc}z.awp${RUN}pm25nmmb.temp
   export FORT12="filesize"
   export FORT31=
   export FORT51=grib2.t${cyc}z.awp${RUN}pm25nmmb.196
   ${TOCGRIB2SUPER} < ${PARMaqm}/wmo/grib2_aqm_${RUN}pm25nmmb.${cycle}.196
   export err=$?;err_chk

##############################
# Post PM2.5 Files to PCOM
##############################
   if [ "${SENDCOM}" == "YES" ]; then
      cp grib2.t${cyc}z.awp${RUN}pm25nmmb.196  ${PCOM}/awpaqm.t${cyc}z.1hpm25.196.grib2
      echo "no files copy"

      ##############################
      # Distribute Data
      ##############################

      if [ "${SENDDBN_NTC}" == "YES" ] ; then
         ${DBNROOT}/bin/dbn_alert ${DBNALERT_TYPE} ${NET} ${job} ${PCOM}/awpaqm.t${cyc}z.1hpm25.196.grib2
      else
         msg="File ${output_grb}.${job} not posted to db_net."
         postmsg   "${msg}"
      fi
   fi

#######################
fi
#######################################################
msg='ENDED NORMALLY.'
postmsg   "${msg}"

################## END OF SCRIPT #######################
exit
