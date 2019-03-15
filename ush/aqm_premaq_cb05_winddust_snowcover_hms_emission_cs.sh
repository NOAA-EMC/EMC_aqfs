#!/bin/ksh 

set -xa
#-----------------------------------------------------------------
#step 1 fengsha
#-----------------------------------------------------------------
cd $DATA
export domainfile=$PARMaqm/aqm_cs_domain
export FLANDA=$FIXaqm/LANDA_US12_442X265
export FLANDT=$FIXaqm/LAND_TOTALS_US12_442X265 
export CROPMAP01=$FIXaqm/aqm_CROPMAP01_cs
export CROPMAP04=$FIXaqm/aqm_CROPMAP04_cs
export CROPMAP08=$FIXaqm/aqm_CROPMAP08_cs
export METMOD=NAM	# MM5 or NAM soil types;
#export RAINMAP          # Not used so far

sdate=$PDY
jdate=$DAYOFYEAR
jtime=${cyc}0000

if [ ${cyc} = '00' -o ${cyc} = '18' ]
then
 export nstep=7
else
 export nstep=49 #25
fi

cfg=dust.cfg
year=`echo $sdate | cut -c 1-4`

if [ -e $cfg ]; then
 rm -rf  $cfg
fi
  
echo $jdate $jtime $nstep > $cfg

export MET_CRO_2D=${COMIN}/aqm.${cycle}.metcro2d.ncf 
export GRID_CRO_2D=$FIXaqm/aqm.grdcro2d_new.ncf
export CTM_DUST=$DATA/CCTM_DUST_${sdate}.ncf

#--------------------------------
 ic=0
  while [ $ic -lt 300 ]
  do
    if [ -s $MET_CRO_2D ]
    then
      echo  $MET_CRO_2D "exists!"
      break
    else
      let "ic=ic+1"
      sleep 10
    fi

    if [ $ic -ge 180 ]
    then
      err_exit "COULD NOT LOCATE:$MET_CRO_2D"
    fi
  done

#---------------------------------

if [ -e $CTM_DUST ]; then
 rm $CTM_DUST
fi

export pgm=aqm_fengsha
${EXECaqm}/aqm_fengsha  
export err=$?;err_chk
  
if [ -e $CTM_DUST ]; then 
 echo "dust emissions were generated"
else
 echo "error in dust emissions generation"
 exit 31
fi

# Merge the dust output with 3d emissions

export EMIS_1=${DATA}/aqm.${cycle}.emission.${sdate}.windust.ncf
export EMIS_2=${DATA}/CCTM_DUST_${sdate}.ncf 

if [ -e $EMIS_1 ]; then
 rm -rf $EMIS_1 
fi
mv $COMOUT/aqm.${cycle}.emission.ncf $COMOUT/aqm.${cycle}.emission_0.ncf
ln -s $COMOUT/aqm.${cycle}.emission_0.ncf $COMOUT/aqm.${cycle}.emission.ncf
cp ${COMIN}/aqm.${cycle}.emission.ncf $EMIS_1  
    
if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

export pgm=aqm_fengsha
#startmsg
${EXECaqm}/aqm_fengsha_merge_2016  >>  $pgmout 2>errfile
export err=$?;err_chk

if [ -e $EMIS_1 ]; then
 echo "dust emissions merge done"
 cp ${DATA}/CCTM_DUST_${sdate}.ncf $COMOUT/CCTM_DUST_${cycle}_${sdate}.ncf
else
 echo "error in dust emissions merge"
 exit 32
fi

#--------------------------------------------------------------------------
#step 2 snowcovering
#--------------------------------------------------------------------------
emis3d=aqm.${cycle}.emission.${PDY}.windust
metc2d=aqm.${cycle}.metcro2d

ln -s ${MET_CRO_2D} $DATA 

export EMIS3D=$DATA/${emis3d}.ncf
export METC2D=$COMIN/${metc2d}.ncf

if [ -e chkreads.log ] ; then
 rm -rf chkreads.log
fi

export pgm=aqm_snowdust
startmsg
$EXECaqm/aqm_snowdust_2016
export err=$?;err_chk

mv ${DATA}/${emis3d}.ncf ${COMOUT}/aqm.${cycle}.emission.${PDY}.windust_snowc.ncf

echo "it is done huang99"

if [ -s ${COMOUT}/aqm.${cycle}.emission.ncf  ] 
then

 rm -rf ${COMOUT}/aqm.${cycle}.emission.ncf
 ln -s  ${COMOUT}/aqm.${cycle}.emission.${PDY}.windust_snowc.ncf ${COMOUT}/aqm.${cycle}.emission.ncf  
 ln -s  ${COMOUT}/aqm.${cycle}.emission.${PDY}.windust_snowc.ncf ${DATA}/

fi


#-----------------------------------------------------------------
#step 3 HMS
# check availablity of fires inside CONUS domain before call hms fire emission 
#  if there is no file, then go to 
#  if [ ${cyc} = '00' -o  ${cyc} = '06' ]  
if [ $Mn -ge 5 ] && [ $Mn -le 9 ] ; then
  if [ ${cyc} = '00' ]  
  then
   smoke_emis9=${smoke_emis}/smokecs.$PDYm1
  else
   smoke_emis9=${smoke_emis}/smokecs.$PDY 
  fi

  if [ -e chkreads.log ] ; then
    rm -rf chkreads.log
  fi

  if [ ${cyc} = '06' ] 
  then 
   file_emitime=${smoke_emis9}/EMITIMES.t06z
   file_fire=${smoke_emis9}/files_fires_cs.t06z.tar
  else
   file_emitime=${smoke_emis9}/EMITIMES.t12z
   file_fire=${smoke_emis9}/files_fires_cs.t12z.tar
  fi 


  if [ -s ${file_emitime}  ]
  then 
    ln -s ${file_emitime} $DATA/EMITIMES
#    ln -s ${file_emitime} $COMOUT 

  
   test_file=$DATA/FIRE_CHECK

  $EXECaqm/aqm_fire_checking_2016
  export err=$?;err_chk

  sleep 30
  rm -rf log_fire.log

  if [[ -s ${test_file} ]] ; then
    grep -ni "THERE ARE FILES inside the CONUS DOMAIN" ${test_file} > log_fire.log
  fi
  if [[ -s log_fire.log ]] ; then

   if [ ${cyc} = '00' ] ; then   
    $USHaqm/aqm_smoke2cmaq_L35_fcst.sh $PDYm1 
   else
    $USHaqm/aqm_smoke2cmaq_L35_fcst.sh $PDY
   fi     
  fi
  else
   echo "WARNING: NO HYSPLIT fire emissions available for CMAQ: $file_emitime!!"
  fi 

fi
########################################################
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
################## END OF SCRIPT #######################
