#!/bin/ksh 

set -xa
#-----------------------------------------------------------------
#step 1 fengsha
#-----------------------------------------------------------------
if [ ! -d $COMOUT ] ; then
  mkdir -p $COMOUT
fi

cd $DATA
export domainfile=$PARMaqm/aqm_cs_domain
export FLANDA=$FIXaqm/LANDA_US12_442X265
export FLANDT=$FIXaqm/LAND_TOTALS_US12_442X265 
export CROPMAP01=$FIXaqm/aqm_CROPMAP01_cs
export CROPMAP04=$FIXaqm/aqm_CROPMAP04_cs
export CROPMAP08=$FIXaqm/aqm_CROPMAP08_cs
export METMOD=NAM		# MM5 or NAM soil types;
#export RAINMAP          # Not used so far

sdate=$PDY
jdate=$DAYOFYEAR
jtime=${CYC}0000

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
cp ${COMIN}/aqm.${cycle}.emission.ncf $EMIS_1  
    
export pgm=aqm_fengsha
#startmsg
mpirun.lsf ${EXECaqm}/aqm_fengsha_merge  >>  $pgmout 2>errfile
export err=$?;err_chk

if [ -e $EMIS_1 ]; then
 echo "dust emissions merge done"
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

export pgm=aqm_snowdust
#startmsg
mpirun.lsf  $EXECaqm/aqm_snowdust
export err=$?;err_chk

cp ${DATA}/${emis3d}.ncf ${DATA}/${emis3d}_snowc.ncf

#-----------------------------------------------------------------
#step 3 HMS
# check availablity of fires inside CONUS domain before call hms fire emission 
#  if there is no file, then go to 
  if [ ${CYC} = '00' -o  ${CYC} = '06' ]  
  then
   smoke_emis9=${smoke_emis}/smoke.$PDYm1
  else
   smoke_emis9=${smoke_emis}/smoke.$PDY 
  fi

  if [ -s ${smoke_emis9}/EMITIMES  ]
  then 
   ln -s ${smoke_emis9}/EMITIMES $DATA 
  test_file=$DATA/FIRE_CHECK

  $EXECaqm/aqm_fire_checking
  export err=$?;err_chk

  sleep 30
  rm -rf log_fire.log

  if [[ -s ${test_file} ]] ; then
    grep -ni "THERE ARE FILES inside the CONUS DOMAIN" ${test_file} > log_fire.log
  fi
  if [[ -s log_fire.log ]] ; then
    $USHaqm/aqm_premaq_cb05_hms_emission_cs.sh
  else
   cp -rp $DATA/aqm.t${cyc}z.emission.${PDY}.windust_snowc.ncf  $COMOUT/
   cp -rp $COMOUT/aqm.${cycle}.emission.ncf $COMOUT/aqm.${cycle}.emission_old.ncf
   cp -rp $DATA/aqm.$cycle.emission.$PDY.windust_snowc.ncf $COMOUT/aqm.${cycle}.emission.ncf
  fi
  else
   echo "no HYSPLIT fire emissions availabe for CMAQ"
   cp -rp $DATA/aqm.t${cyc}z.emission.${PDY}.windust_snowc.ncf  $COMOUT/
   cp -rp $COMOUT/aqm.${cycle}.emission.ncf $COMOUT/aqm.${cycle}.emission_old.ncf
   cp -rp $DATA/aqm.$cycle.emission.$PDY.windust_snowc.ncf $COMOUT/aqm.${cycle}.emission.ncf
  fi 

########################################################

msg='ENDED NORMALLY.'
#postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################










