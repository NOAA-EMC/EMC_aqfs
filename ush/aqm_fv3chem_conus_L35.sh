#!/bin/ksh
################################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         aqm_fv3chem_L35.sh
# Script description:  derive regional AQM LBC from operational global aerosol model
#
# 05/26/2020  Ho-Chun Huang  LBC only derived at 06Z and 12Z
# 05/26/2020  Ho-Chun Huang  GEFS-aerosol 06Z run uses day-1 fire emission.
#                            But GEFS 06Z runs after CMAQ 06Z start, thus CMAQ 06Z
#                            can only use GEFS 00Z output (using day-2 fire emission)
#                            for CMAQ LBC at PDY 06Z.  At 12Z, re-calculate LBC using
#                            GEFS 06Z data for CMAQ PDY 12Z and 18Z & PDY+1 00Z
#
################################################################################

set -xa 

export pgm=aqm_prep_cs_lbc
cd ${DATA}

cyc=00
cycledate=${1:-${PDY}}{$cyc}
cyear=`echo ${cycledate} | cut -c1-4`
cmonth=`echo ${cycledate} | cut -c5-6`
cdate=`echo ${cycledate} | cut -c7-8`
ic=`/bin/date --date=${cyear}'/'${cmonth}'/'${cdate} +%j`
cjulian=`printf %3.3d ${ic}`

if [ "${cycle}" == "t06z" ]; then
   lbc_cyc=t00z
else
   lbc_cyc=t06z
   ## ALERT HHC for 1 cycle testing
   if [ "${FLAG_ONE_CYCLE}" == "YES" ]; then lbc_cyc=t00z; fi ## for one cycle testing
fi
# LBC_INI, LBC_END, and LBC_FREQ are defined in ~/jobs/JAQM_PREP_CS
let ic=${LBC_INI}
let endhour=${LBC_END}
let lbc_int=${LBC_FREQ}
let num_file=${endhour}/${lbc_int}+1
while [ ${ic} -le ${endhour} ]; do
   icnt=`printf %3.3d ${ic}`
   if [ -s ${LBCIN}/geaer.${lbc_cyc}.atmf${icnt}.nemsio ]; then
      ln -s ${LBCIN}/geaer.${lbc_cyc}.atmf${icnt}.nemsio geaer.${lbc_cyc}.atmf${icnt}.nemsio
   else
      echo "WARNING can not find ${LBCIN}/geaer.t${lbc_cyc}.atmf${icnt}.nemsio"
   fi
   ## let ic=${ic}+${lbc_int}
   ((ic=ic+${lbc_int}))
done
 
cat > ngac-bnd-nemsio.ini <<EOF
&control
 begyear=${cyear}  
 begdate=${cjulian}
 begtime=${cyc}    
 dtstep=${lbc_int}        
 numts=${num_file}
 bndname='NO2','NO','O3','NO3','OH','HO2','N2O5','HNO3','HONO','PNA',
 'H2O2','CO','SO2','SULF','PAN','FACD','AACD','PACD','UMHP','MGLY',
 'OPEN','CRES','FORM','ALD2','PAR','OLE','TOL','ISOP','ETH','XYL',
 'ASO4J','ASO4I','ASOIL','NH3','NUMATKN','NUMACC','NUMCOR',
 'SRFATKN','SRFACC','AOTHRJ',AECJ,APOCJ
 checkname='AOTHRJ','ASOIL','AECJ','APOCJ'
 mofile='./geaer.${lbc_cyc}.atmf','.nemsio'
 checklayer=1    
&end

Species converting Factor
# Gocart ug/m3 to regional ug/m3
'dust1'    2  ## 0.2-2um diameter: assuming mean diameter is 0.3 um (volume= 0.01414x10^-18 m3) and density is 2.6x10^3 kg/m3 or 2.6x10^12 ug/m3.so 1 particle = 0.036x10^-6 ug
'AOTHRJ'  1.0   'NUMACC' 27205909.
'dust2'    4  ## 2-4um
'AOTHRJ'  0.45    'NUMACC'  330882.  'ASOIL'  0.55   'NUMCOR'  50607.
'dust3'    2  ## 4-6um
'ASOIL'   1.0   'NUMCOR' 11501.
'dust4'    2   ## 6-12um
'ASOIL'  0.7586   'NUMCOR' 1437.
'bc1'      2     # kg/kg
'AECJ'     1.0   'NUMACC' 6775815.
'bc2'  2     # kg/kg
'AECJ'     1.0   'NUMACC' 6775815.
'oc1'  2     # kg/kg OC -> organic matter
'APOCJ'    1.0   'NUMACC' 6775815.
'oc2'  2
'APOCJ'  1.0   'NUMACC' 6775815.

EOF

##
##  Ideally the MET should already be created ahead of LBC computation
##  Simply a fail-safe option in case odd things occurred
##
if [ -s ${COMIN}/aqm.${lbc_cyc}.metcro3d.ncf ] ; then  ## using current cycle CMAQ MET
   export METEO3D=${COMIN}/aqm.${lbc_cyc}.metcro3d.ncf
   export TOPO=${COMIN}/aqm.${lbc_cyc}.grdcro2d.ncf
else
   if [ "${cycle}" == "t06z" ]; then  ## using previous LONG cycle CMAQ MET
      export METEO3D=${COMINm1}/aqm.t12z.metcro3d.ncf
      export TOPO=${COMINm1}/aqm.t12z.grdcro2d.ncf
   else
      ## ALERT HHC for 1 cycle testing
      if [ "${FLAG_ONE_CYCLE}" == "YES" ]; then 
         export METEO3D=${COMIN}/aqm.t12z.metcro3d.ncf
         export TOPO=${COMIN}/aqm.t12z.grdcro2d.ncf
      else
         export METEO3D=${COMIN}/aqm.t06z.metcro3d.ncf
         export TOPO=${COMIN}/aqm.t06z.grdcro2d.ncf
      fi
   fi
fi

if [ $RUN = 'aqm' ]; then
   export BND1=${FIXaqm}/aqm_conus_12km_geos_2006${cmonth}_static_35L.ncf
   export BND2=${COMOUT}/aqm_conus_geos_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf        # output bnd files
elif [ $RUN = 'HI' ]; then
   export BND1=${FIXaqm}/HI_80X52_mean_2002${cmonth}_GEOSCHEM-35L-tracer.fv3.ncf
   export BND2=${COMOUT}/aqm_HI_geos_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf
elif [ $RUN = 'AK' ]; then
   export BND1=${FIXaqm}/aqm_AK_cb05_ae4_mean_${cmonth}.35L.ncf
   export BND2=${COMOUT}/aqm_AK_geos_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf
else
   echo " unknown domain $RUN "
   exit 1
fi
export CHECK2D=${COMOUT}/check_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf

rm -rf chkreads.log

startmsg
${EXECaqm}/aqm_fv3chem_dlbc.x  >> ${pgmout} 2>errfile 
export err=$?;err_chk
