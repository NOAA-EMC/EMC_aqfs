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
# 08/06/2020  Ho-Chun Huang  AQ group decision has been made to use one-cycle back
#                            GEFS-aerosol output if target GEFS-Aerosol output is not
#                            available at the time of CMAQ run (see previous log).
#                            Email will be sent to NCO SPA team for this situation.
#                            According to Li Pan, this situation almost will not occur.
#
################################################################################

set -xa 

export pgm=aqm_prep_cs_lbc
cd ${DATA}

##
## Assigned time information of target GEFS output for t06z or t12z cycle that generates new LBC file for current day
##
lbc_day=${PDY}
if [ "${cycle}" == "t06z" ]; then
   lbc_cyc=t00z
else
   lbc_cyc=t06z
   ## ALERT HHC for 1 cycle testing
   if [ "${FLAG_ONE_CYCLE}" == "YES" ]; then lbc_cyc=t00z; fi ## for one cycle testing
fi
gefscyc=`echo ${lbc_cyc} | cut -c2-3`
# LBC_INI, LBC_END, and LBC_FREQ are defined in ~/jobs/JAQM_PREP_CS
# Checking GEFS-Aerosol LBC files
echo "DIAG : Lateral BC files ingested from ${LBCIN}"
flag_lbc_exist=yes
let ic=${LBC_INI}
let endhour=${LBC_END}
let lbc_int=${LBC_FREQ}
let num_file=${endhour}/${lbc_int}+1
while [ ${ic} -le ${endhour} ]; do
   icnt=`printf %3.3d ${ic}`
   if [ -s ${LBCIN}/${gefscyc}/chem/sfcsig/geaer.${lbc_cyc}.atmf${icnt}.nemsio ]; then
      ln -s ${LBCIN}/${gefscyc}/chem/sfcsig/geaer.${lbc_cyc}.atmf${icnt}.nemsio geaer.${lbc_cyc}.atmf${icnt}.nemsio
   else
      echo "WARNING can not find ${LBCIN}/${gefscyc}/chem/sfcsig/geaer.${lbc_cyc}.atmf${icnt}.nemsio"
      flag_lbc_exist=no
      break
   fi
   ((ic=ic+${lbc_int}))
done
lbccyc=${lbc_cyc}
if [ "${flag_lbc_exist}" == "no" ]; then     ## check one cycle back GEFS-Aerosol files
   /bin/rm -rf ${DATA}/geaer.*                      ## clean previous partial links in LBCIN file check above
   current_lbccyc=`echo ${lbc_cyc} | cut -c2-3`
   cdate=${PDY}${current_lbccyc}
   new_lbc_time=$( ${NDATE} -6 ${cdate} )   ## push one cycle back for GEFS output
   new_lbc_day=`echo ${new_lbc_time} | cut -c1-8`
   new_lbc_cyc=`echo ${new_lbc_time} | cut -c9-10`
   lbccyc=t${new_lbc_cyc}z
   LBCIN2=${LBCIN}
   if [ "${new_lbc_day}" == "${PDYm1}" ]; then
      LBCIN2=${LBCINm1}
      echo "WARNING :: Switch GEFS LBC input directory from ${LBCIN} to ${LBCIN2}"
   fi
   flag_lbc2_exist=yes
   let ic=${LBC_INI}
   let endhour=${LBC_END}
   let lbc_int=${LBC_FREQ}
   let num_file=${endhour}/${lbc_int}+1
   while [ ${ic} -le ${endhour} ]; do
      icnt=`printf %3.3d ${ic}`
      if [ -s ${LBCIN2}/${new_lbc_cyc}/chem/sfcsig/geaer.${lbccyc}.atmf${icnt}.nemsio ]; then
         ln -s ${LBCIN2}/${new_lbc_cyc}/chem/sfcsig/geaer.${lbccyc}.atmf${icnt}.nemsio geaer.${lbccyc}.atmf${icnt}.nemsio
      else
         echo "WARNING can not find ${LBCIN2}/${new_lbc_cyc}/chem/sfcsig/geaer.${lbccyc}.atmf${icnt}.nemsio"
         flag_lbc2_exist=no
         break
      fi
      ((ic=ic+${lbc_int}))
   done
   if [ "${flag_lbc2_exist}" == "yes" ]; then
      lbc_day=${new_lbc_day}
      if [ "${RUN_ENVIR}" == "nco" ]; then
         echo "~s ${lbc_cyc} GEFS output for ${cycle} CMAQ run are missing ${lbccyc} output are used CMAQ RUN SOFT FAILED" | mail SABSupervisor@noaa.gov
      else
         echo "~s ${lbc_cyc} GEFS output for ${cycle} CMAQ run are missing ${lbccyc} output are used CMAQ RUN SOFT FAILED" | mail ho-chun.huang@noaa.gov
      fi
      lbc_day=${new_lbc_day}
   else
      if [ "${RUN_ENVIR}" == "nco" ]; then
         echo "~s Both ${lbc_cyc} and ${lbccyc} GEFS output for ${cycle} CMAQ run are missing. CMAQ RUN HARD FAILED" | mail SABSupervisor@noaa.gov
      else
         echo "~s Both ${lbc_cyc} and ${lbccyc} GEFS output for ${cycle} CMAQ run are missing. CMAQ RUN HARD FAILED" | mail ho-chun.huang@noaa.gov
      fi
      err=9898
      postmsg "ERROR IN ${pgm} for GEFS LBC files"
      err_chk
   fi
fi
##
## Use exact timing information of selected GEFS output
##
lbc_cyc=${lbccyc}
cyc=`echo ${lbc_cyc} | cut -c2-3`
cyear=`echo ${lbc_day} | cut -c1-4`
cmonth=`echo ${lbc_day} | cut -c5-6`
cdate=`echo ${lbc_day} | cut -c7-8`
ic=`/bin/date --date=${cyear}'/'${cmonth}'/'${cdate} +%j`
cjulian=`printf %3.3d ${ic}`

cat > gefs-bnd-nemsio.ini <<EOF
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
 'SRFATKN','SRFACC','AOTHRJ','AECJ','APOCJ','ANH4J','ANO3J','ANAJ','ACLJ'
 checkname='AOTHRJ','ASOIL','AECJ','APOCJ','ASO4J','ANH4J','ANO3J','ANAJ','ACLJ'
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
      ## ALERT for engineering 4 cycle test
         export METEO3D=${COMIN}/aqm.t12z.metcro3d.ncf
         export TOPO=${COMIN}/aqm.t12z.grdcro2d.ncf
   fi
fi
if [ ! -s ${METEO3D} ]; then
   echo "ERROR, can not find ${METEO3D}"
   err=999
   err_chk
fi
if [ ! -s ${TOPO} ]; then
   echo "ERROR, can not find ${TOPO}"
   err=999
   err_chk
fi
#
# ALERT August 27 2020 : HHC ARL suggest to reverse LBC file used as before
#  From August 06 to August 27 :   export BND1=${FIXaqm}/lbc-gmi-adj2-${cmonth}.5x-L35.ncf
if [ $RUN = 'aqm' ]; then
   export BND1=${FIXaqm}/aqm_conus_12km_geos_2006${cmonth}_static_FV3_35L.ncf
   export BND2=${COMOUT}/aqm_conus_geos_fv3chem_aero_${PDY}_35L.ncf                # output CONUS BND files
   export BND2_cyc=${COMOUT}/aqm_conus_geos_fv3chem_aero_${PDY}_${cycle}_35L.ncf   # output CONUS BND files with cycle information
elif [ $RUN = 'HI' ]; then
   export BND1=${FIXaqm}/HI_80X52_mean_2002${cmonth}_GEOSCHEM-35L-tracer.fv3.ncf
   export BND2=${COMOUT}/aqm_HI_geos_fv3chem_aero_${PDY}_35L.ncf                   # output HI    BND files
   export BND2_cyc=${COMOUT}/aqm_HI_geos_fv3chem_aero_${PDY}_${cycle}_35L.ncf      # output HI    BND files with cycle information
elif [ $RUN = 'AK' ]; then
   export BND1=${FIXaqm}/aqm_AK_cb05_ae4_mean_${cmonth}.35L.ncf
   export BND2=${COMOUT}/aqm_AK_geos_fv3chem_aero_${PDY}_35L.ncf                   # output AK    BND files
   export BND2_cyc=${COMOUT}/aqm_AK_geos_fv3chem_aero_${PDY}_${cycle}_35L.ncf      # output AK    BND files with cycle information
else
   echo " unknown domain $RUN "
   exit 1
fi
export CHECK2D=${COMOUT}/check_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf

if [ ! -s ${BND1} ]; then    ## Soft Fail
   echo "WARNING ***  Can not find ${BND1} to produce ${BND2}, MANUAL INSPECTION required, model run continue"
   export err=999
   err_chk
fi
rm -rf chkreads.log

startmsg
${EXECaqm}/aqm_fv3chem_dlbc  >> ${pgmout} 2>errfile 
export err=$?;err_chk

##
## Keep record of the LBC used in differretn cycle
## CMAQ FCST run will always use the latest BND2 of the day, i.e., 12Z produced LBC will replace LBC produced at 06Z
## if [ -s ${BND2} ]; then cp -p ${BND2} ${BND2_cyc}; fi
if [ -s ${BND2} ]; then
   cp -p ${BND2} ${BND2_cyc}
else
   echo "Can not find ${BND2}"
   err=888
   err_chk
fi
##
