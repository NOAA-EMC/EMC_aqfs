#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
# Script name:         exaqm_post3_cb06_hi.sh.ecf
# Script description:  Run CMAQ Mie extinction post processing 
# Abstract: This script runs CMAQ post processing
#
# Script history log:
# 2010-02-01    Jianping Huang 
# 2020-12-17    Ho-Chun Huang   Revise original code to retrived model
#                               computed AOT in aqm.${cycle}.rj_1.ncf
######################################################################
set -x

msg="JOB $job HAS BEGUN"
postmsg   "${msg}"

export pgm=aqm_hi_post3

cd ${DATA}

export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_HIGH}
export XLFRTEOPTS="unit_vars=yes"

## Hawaii domain size
export imx=80
export jmx=52
export id_grib=139

export CHEM2D=${COMIN}/aqm.${cycle}.rj_1.ncf

DATE=$(echo ${PDY} | cut  -c 1-8)
yyyy=$(echo ${PDY} | cut  -c 1-4)
mm=$(echo ${PDY}   | cut  -c 5-6)
dd=$(echo ${PDY}   | cut  -c 7-8)

export DATE yyyy mm dd

case ${cycle} in
   t00z) endfhr=06;;
   t06z) endfhr=72;;
   t12z) endfhr=72;;
   t18z) endfhr=06;;
esac

 . prep_step
cat >cmaq2grib2_aot.ini <<EOF5
&control
outfile='${DATA}/aqm.${cycle}.aot'
nlayers=1
id_gribdomain=139
/
EOF5

startmsg
${EXECaqm}/aqm_rdgrbwt_aot_CHA_g2 ${imx} ${jmx}  >> ${pgmout} 2>errfile
export err=$?;err_chk

ic=1
if [ "${SENDCOM}" == "YES" ]; then
   while [ ${ic} -le ${endfhr} ]; do
      fhr=`printf %2.2d ${ic}`
      mv ${DATA}/aqm.${cycle}.aot.f${fhr}.${id_grib}.grib2 ${COMOUT}/
      ((ic++))
   done
fi

echo EXITING $0

msg='ENDED NORMALLY.'
postmsg   "${msg}"

exit

