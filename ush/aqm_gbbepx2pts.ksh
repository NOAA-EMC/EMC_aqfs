#!/bin/ksh -x
##
## For operational/NRT and developmental retro-run, one should use day-1 fire emissions to mimic
##     operational environment. Using a day-2 fire emissions is a fail-over option during operational run.
## Add warning message to alert NCO for missing fire emission files in 
##     /lfs/h1/ops/prod/dcom/${PDY}/firewx
## PDY's GBBEPx FIRE EMISSION only in the dcom $PDY directory
##
## 10/31/2021   Jianping Huang perform update for WCOSS2 transition 
## 08/03/2022   Ho-Chun Huang  perform update for new dcom configuration that PDY fire emission
##                             can only be found in /lfs/h1/ops/prod/dcom/${PDY}/firewx
export pgm=aqm_prep_cs_fire_emi
fire_emission_hdr=GBBEPx_all01GRID.emissions_v003
if [ ${FCST} = "NO" ] ; then  ## For 24-hour-back analysis run using PDYm1 fire emission
   flag_with_gbbepx=yes
   if [ -s ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc ]; then
      FIREDATE=${PDYm1}
      emisfile=${fire_emission_hdr}_${PDYm1}.nc
      COMIN9=${COMINfirem1}
   elif [ -s ${COMINfirem2}/${fire_emission_hdr}_${PDYm2}.nc ]; then
      FIREDATE=${PDYm2}
      emisfile=${fire_emission_hdr}_${PDYm2}.nc
      COMIN9=${COMINfirem2}
      echo "WARNING NO ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc"
   else
      echo "WARNING NO ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc"
      echo "WARNING NO ${COMINfirem2}/${fire_emission_hdr}_${PDYm2}.nc"
      flag_with_gbbepx=no
   fi 
else   ## For day1, day2, and day3 forecast runs using PDYm1 fire emission OR create control run
   flag_with_gbbepx=yes
   if [ -s ${COMINfire}/${fire_emission_hdr}_${PDY}.nc ] && [ "${FLAG_TODAY_FIRE}" == "YES" ]; then
      FIREDATE=${PDY}
      emisfile=${fire_emission_hdr}_${PDY}.nc
      COMIN9=${COMINfire}
      echo "WARNING using current day fire emission in forecast mode is only for estabilishing a refernce case"
      echo "WARNING in operational environment, only day-1 fire emission is available for current day forecast"
   elif [ -s ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc ]; then
      FIREDATE=${PDYm1}
      emisfile=${fire_emission_hdr}_${PDYm1}.nc
      COMIN9=${COMINfirem1}
   elif [ -s ${COMINfirem2}/${fire_emission_hdr}_${PDYm2}.nc ]; then
      FIREDATE=${PDYm2}
      emisfile=${fire_emission_hdr}_${PDYm2}.nc
      COMIN9=${COMINfirem2}
      echo "WARNING NO ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc"
   else
      echo "WARNING NO ${COMINfirem1}/${fire_emission_hdr}_${PDYm1}.nc"
      echo "WARNING NO ${COMINfirem2}/${fire_emission_hdr}_${PDYm2}.nc"
      flag_with_gbbepx=no
   fi 
fi


if [ "${flag_with_gbbepx}" == "yes" ]; then
   echo "=========================================================="
   echo "Current cycle uses fire emission from ${COMIN9}/${emisfile}"
   echo "=========================================================="
##
##  Present fire emission scheme includes gas-phase emission
##      check aqm.t*z.fire_emi_cs.ncf with CO, SO2, NO, NO2, NH3 output
##
##  However, in present version, NOx emission has been turn off.
##  In the content of gbbepx2pts.ini below;
##
##  With NOx emission ON  use ( CORRECT   spelling for the one in GBBEPx file )
##  =====> 'NOx'    2   # 90% to NO (mw 30), 10% to NO2 (mw 46), mw 31.6 in average, kg->mole
##
##  With NOx emission OFF use ( INCORRECT spelling for the one in GBBEPx file )
##  =====> 'NOX'    2   # 90% to NO (mw 30), 10% to NO2 (mw 46), mw 31.6 in average, kg->mole
##
ln -s ${COMIN9}/${emisfile} ${emisfile}

FRPRATIO=${FRPRATIO:-1.0}
cat>gbbepx2pts.ini<<!
&control
efilein='./${emisfile}'
markutc=18
burnarea_ratio=0.1
frpfactor=${FRPRATIO}
startdate=${FIREDATE}06
nduration=127
tdiurnal=0.03033772, 0.03033772, 0.03033772, 0.03033772, 0.03033772,
       0.03033772, 0.03033772, 0.03434459, 0.03720664, 0.04006869,
       0.05724098, 0.07441328, 0.09158558, 0.09730967, 0.06868918,
       0.04006869, 0.03434459, 0.03033772, 0.03033772, 0.03033772,
       0.03033772, 0.03033772, 0.03033772, 0.03033772
dfrac=1.0,0.25,0.25
emname='CO','NO','NO2','SO2','NH3','PEC','POC','PMOTHR','PNO3','PSO4',
'PAL','PCA','PCL','PFE','PK','PMG','PMN','PNA','PNCOM','PNH4','PSI','PTI'
/

Species Converting Factor
'CO'      1   # kg ->mole
'CO'   35.7
'NOx'    2   # 90% to NO (mw 30), 10% to NO2 (mw 46), mw 31.6 in average, kg->mole
'NO'   28.481   'NO2'  3.164557
'SO2'    1    # kg -> mole
'SO2'  15.625
'NH3'    1
'NH3'  58.82
'BC'     1    # kg -> g  
'PEC'   0.  # 1000. 
'OC'     1
'POC'   0.  # 1000. 
'PM2.5' 16    # pm2.5 splitt factor kg -> g
'PEC' 94.9   'POC' 461.8  'PMOTHR'  13.7 'PNO3' 1.323 'PSO4' 12.6 
'PAL' 0.6075 'PCA' 3.858 'PCL' 41.5 'PFE' 0.434 'PK' 29.4
'PMG' 0.314  'PNA' 5.7335 'PNCOM' 323.2 'PNH4' 8.7915 'PSI' 1.8185  'PTI' 0.0515
!

   export IOAPI_ISPH=20 # make consistent with met-preprocessor R_earth=6370000m
   if [ ${RUN} = 'cs' ]; then
      export GRIDDESC=${PARMaqm}/aqm_griddesc05
      export GRID_NAME=AQF_cs
      export TOPO=${FIXaqm}/aqm_gridcro2d.landfac.cs.ncf
      regid='cs'
   elif [ ${RUN} = 'hi' ]; then
      export GRIDDESC=${PARMaqm}/aqm_griddeschi
      export GRID_NAME=AQF_hi
      export TOPO=${FIXaqm}/aqm_gridcro2d.landfac.hi.ncf
      regid=${RUN}
   elif [ ${RUN} = 'ak' ]; then
      export GRIDDESC=${PARMaqm}/aqm_griddescak
      export GRID_NAME=AQF_ak
      export TOPO=${FIXaqm}/aqm_gridcro2d.landfac.ak.ncf
      regid=${RUN}
   else
      echo " unknown domain ${RUN} "
      exit 1
   fi
   
   # output
   if [ "${FCST}" = "YES" ]; then
      export STACK_GROUP=aqm.${cycle}.fire_location_${regid}.ncf
      export PTFIRE=aqm.${cycle}.fire_emi_${regid}.ncf
   else
      export STACK_GROUP=aqm.${cycle}.fire_location_${regid}_r.ncf
      export PTFIRE=aqm.${cycle}.fire_emi_${regid}_r.ncf
   fi
   
   startmsg
   ${EXECaqm}/aqm_gbbepx2pts
   export err=$?;err_chk
   
   if [ -s ${PTFIRE} ] && [ -s ${STACK_GROUP} ]; then
      if [ "${FCST}" = "YES" ]; then
         cp ${DATA}/${PTFIRE}      ${COMOUT}
         cp ${DATA}/${STACK_GROUP} ${COMOUT}
      else
         cp ${DATA}/${PTFIRE}      ${COMOUTm1}
         cp ${DATA}/${STACK_GROUP} ${COMOUTm1}
      fi
   else
      echo "WARNING can not find both ${DATA}/${PTFIRE} and ${DATA}/${STACK_GROUP}.  Assuming no fire today FCST=${FCST}"
   fi

else
   echo "WARNING :: NO Current and Previous day's GBBEPX fire emission input.  Assuming no fire today"
fi
