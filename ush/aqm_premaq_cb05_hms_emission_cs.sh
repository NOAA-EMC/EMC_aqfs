#!/bin/ksh 

echo "hjp is calling hms emission"
set -xa
#-----------------------------------------------------------------
export PROMPTFLAG=F
#-----------------------------------------------------------------
#  BLUESKY HYSPLIT AND NAM FILES
#------------------------------------------------------------------
#jp if [ -s /com/hysplit/prod/smoke.$PDY/files_fires.tar ] ; then
 if [ -s ${smoke_emis9}/files_fires.tar ] ; then
#jp cp /com/hysplit/prod/smoke.$PDY/files_fires.tar $DATA
  cp ${smoke_emis9}/files_fires.tar $DATA
 else
 echo "can not locate files_fires.tar in /com "
 exit 1
fi
tar -xvf $DATA/files_fires.tar >xxx.list
#-----------------------------------------------------------------
# Get Hysplit fire data duration record
#  ** note this file contain information for the duration of fire
#     and can be used for eliminating short life time fore for not
#     being barried to our 72hr fcst
#-----------------------------------------------------------------
 cp $FEMIT $DATA/EMITIMES
#-----------------------------------------------------------------
# create log directory for information output
#-----------------------------------------------------------------
export Bluesky_out=$LOG1/$PDY
mkdir -p ${Bluesky_out}

cd ${Bluesky_out}
################################################################
#
# For  PRODUCTION emission format, the emission file is 72hr
#  ==> Yesterday   today    tomorrow
# * the bluesky information that we get is for previous day and
#   we assume the persistence fro the next 48 hr
#
################################################################
date1=$PDYm1
date2=`/nwprod/util/exec/ndate +24 ${date1}00 |cut -c 1-8`
date3=`/nwprod/util/exec/ndate +48 ${date1}00 |cut -c 1-8` 
date4=`/nwprod/util/exec/ndate +72 ${date1}00 |cut -c 1-8`
date=${date1}

#====================================
if [ ${CYC} = '00' -o  ${CYC} = '06' ]
then
 date9=$PDYm2 
else
 date9=$PDYm1 
fi
#====================================

export TMP_DIR=$HYPTMP/tmp/${date}
mkdir -p $TMP_DIR
#-----------------------------------------------------------------
# get count of files in tar file
#-----------------------------------------------------------------
nfiles=`ls -1 $DATA/NOAA????_${date9}.OUT |wc -l`
let ct=1
while [ $ct -le $nfiles ]; do
 num=${ct}
 typeset -Z4 num
#-----------------------------------------------------------------
# files in "files_fire.tar" 
#-----------------------------------------------------------------
 data=`tail -1 $DATA/NOAA${num}_${date9}.OUT`
#-----------------------------------------------------------------
# Check Duration of fire and eliminate count as specified "XSFIRE"
#-----------------------------------------------------------------
let nump3=num+3
flife=`head -$nump3 $DATA/EMITIMES | tail -1 | cut -c 18-21`
echo "flife = $flife $XSFIRE "

farea=`head -$nump3 $DATA/EMITIMES | tail -1 | cut -c 50-53`
echo "farea = $farea 0"
farea=`echo $farea*100 |bc -l | cut -d. -f1`
echo "farea = $farea 0"

if [ $flife -ge $XSFIRE -a $farea -gt 1 ]; then
#-----------------------------------------------------------------
# files in "files_fire.tar" 
#-----------------------------------------------------------------
data=`tail -1 $DATA/NOAA${num}_${date9}.OUT`
#-----------------------------------------------------------------
# Varilables in "NOAA???????" from untared "files_fires.tar"
#-----------------------------------------------------------------
  btime=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f1`

  lat=`grep Latitude  $DATA/NOAA${num}_${date9}.OUT |cut -d= -f2`
  lon=`grep Longitude $DATA/NOAA${num}_${date9}.OUT |cut -d= -f2`
  area=`grep Area     $DATA/NOAA${num}_${date9}.OUT |cut -d= -f2`

  heat=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f2`
  heat=`echo $heat /6.0 |bc -l | cut -d. -f1`

  pm25=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f3`
  pm10=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f4`
  pm=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f5`
   
##  pm25=`echo $pm25 /6.0 /1.46 |bc -l | cut -d. -f1`
##  pm10=`echo $pm10 /6.0 /1.46 |bc -l | cut -d. -f1`
##  pm=`echo $pm /6.0 |bc -l | cut -d. -f1`

##by lpan 0807  
  pm25=`echo $pm25 /6.0 /1.46 |bc -l`
  pm10=`echo $pm10 /6.0 /1.46 |bc -l`
  pm=`echo $pm /6.0 |bc -l`
  
  co=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f6 `
  co2=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f7`
  ch4=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f8`
   
#  co=`echo $co /6.0 |bc -l | cut -d. -f1`
#  co2=`echo $co2 /6.0 |bc -l | cut -d. -f1`
#  ch4=`echo $ch4 /6.0 |bc -l | cut -d. -f1`
  co=`echo $co /6.0 *0.0 |bc -l | cut -d. -f1`
  co2=`echo $co2 /6.0 *0.0 |bc -l | cut -d. -f1`
  ch4=`echo $ch4 /6.0 *0.0 |bc -l | cut -d. -f1`


  nmhc=`tail -1 $DATA/NOAA${num}_${date9}.OUT | cut -d, -f9`
   
#  nmhc=`echo $nmhc /6.0 |bc -l | cut -d. -f1`
  nmhc=`echo $nmhc /6.0 *0.0 |bc -l | cut -d. -f1`

#-----------------------------------------------------------------
  zone=`echo $lon /15.0 -0.5  |bc -l | cut -d. -f1`
  zonem1=`echo 13 + $zone |bc`

  let fips=88880+zonem1
#-----------------------------------------------------------------
# information of each fire for 72 hr by the presistence assumption
#-----------------------------------------------------------------
  id=NOAAFIRE${num}-${fips}_${date9}
  state=FF
  comma=,
  FILE=NOAA

  if [ -e $TMP_DIR/$FILE${num}.txt ]; then
     rm -f $TMP_DIR/$FILE${num}.txt
  fi

  for day in 1 2 3 4; do
    let datex=date$day

   echo ${datex}${comma}${id}${comma}${lat}${comma}${lon}${comma} \
        ${state}${comma}${area}${comma}${btime}${comma}${heat}${comma} \
        ${pm25}${comma}${pm10}${comma}${pm}${comma}${co}${comma} \
        ${co2}${comma}${ch4}${comma}${nmhc}${comma} \
        ${nh3}${comma}${so2}${comma}${nox}\
        > $TMP_DIR/$FILE${num}.dy$day
   done
 fi
 let ct=ct+1
done

#-----------------------------------------------------------------
# Gather filename of information of each file for future process
#-----------------------------------------------------------------
 ls -1 $TMP_DIR/NOAA*.dy1 > filelist.${date}.day1.lst
 ls -1 $TMP_DIR/NOAA*.dy2 > filelist.${date}.day2.lst
 ls -1 $TMP_DIR/NOAA*.dy3 > filelist.${date}.day3.lst
 ls -1 $TMP_DIR/NOAA*.dy4 > filelist.${date}.day4.lst
#======================================================================
# Process the Fire data from Bluesky (or Hysplit) into smoke inventory:
# It is for PRODUCTION emission 72hr format
#  ==>  Yesterday   today    tomorrow
#       ( jday1     jday2     jday3 )
#======================================================================
 jday1=$yesterday

 let jday2=jday1+1
 let jday3=jday1+2
 let jday4=jday1+3

#-- COMMON VERIABLES

 export VARLIST="AREA, HFLUX, PM2_5, PM10, PMC, CO, CO2, CH4, NMHC, TOG, NH3, SO2, NOX"
 export COSTCY=$FIXaqm/aqm_costcy.fire.txt

#-- PROCESS Bluesky for each day
 for iday in 1 2 3 4; do
   let  jday=jday$iday
  export G_STDATE=$jday

  export FILELIST=filelist.${date}.day$iday.lst
  export PTDAY=ptday.bluesky.${date}.day$iday.txt
  export PTINV=ptinv.bluesky.${date}.day$iday.txt
  export LOGFILE=bluesky2inv.${date}.day$iday.log

  if [ -e $LOGFILE ]; then
    rm -f $LOGFILE
  fi
export pgm=aqm_bluesky2inv
#startmsg

  $EXECaqm/aqm_bluesky2inv       #----- Main Execution file
  export err=$?;err_chk
 done
################################################################
#  put  each day's information (3 files) into one file
#      "ptday.bluesky.$date.ida.txt" ==> PTDAY
################################################################
#-- delete old files if exist
 if [ -e header.txt ]; then
   rm -f header.txt
 fi
#-- write '#EMS-95 ' in the 1st line of file header.txt
 echo '#EMS-95 ' > header.txt
#-- merge information into 1 file
cat header.txt ptday.bluesky.$date.day1.txt ptday.bluesky.$date.day2.txt ptday.bluesky.$date.day3.txt \
                ptday.bluesky.$date.day4.txt > ptday.bluesky.$date.ida.txt.tmp

mv ptday.bluesky.$date.ida.txt.tmp ptday.bluesky.$date.ida.txt
rm -f header.txt
#============= Assign merged file "ptday.bluesky.$date.ida.txt" to PTDAY ======
export PTDAY=ptday.bluesky.$date.ida.txt
#-------------------------------------------------------------------
# Write information into file "ptday.bluesky.$date.ida.lst"
#-------------------------------------------------------------------
 echo '#LIST ' > ptday.bluesky.$date.ida.lst
 ls -1 $PWD/$PTDAY >> ptday.bluesky.$date.ida.lst
#-------------------------------------------------------------------
# Save the information of 1st day (yesterday)
#-------------------------------------------------------------------
 mv ptinv.bluesky.$date.day1.txt ptinv.bluesky.$date.ida.txt
 rm pt???.bluesky.$date.day?.txt
 
#-------------------------------------------------------------
# export NOTCDF_FATAL=F
#-------------------------------------------------------------
# Execution files' location
#-------------------------------------------------------------
 export LOG2=${LOG2:-}
 export LSMK=${LSMK:-}
 export FEMS=${FEMS:-}
#--------------------------------------------------------
# get today's date & Julian
#--------------------------------------------------------
 jday=${YY}${jj}
#--------------------------------------------------------
# meteorological input for plume rise
#--------------------------------------------------------
 #JPexport METDIR=/ptmpp1/$LOGNAME/tmp/aqm.$PDY
 export METDATA_AVAILABLE=Y

 if [ METDATA_AVAILABLE='Y' ]; then

    if [ -d $METDIR ]; then
       rm -rf $METDIR
    fi
    mkdir -p $METDIR

    cd $METDIR
    ln -s $COMOUT/aqm.$cycle.metcro2d.ncf
    ln -s $COMOUT/aqm.$cycle.metcro3d.ncf
    ln -s $COMOUT/aqm.$cycle.metdot3d.ncf
 fi
#--------------------------------------------------------
# Directories used for input data.
#--------------------------------------------------------
#----- inputs from "step1"
 export bluesky=$LOG1
#--------------------------------------------------------
# Directories used for output data.
#--------------------------------------------------------
 export STATIC=$HYPTMP/static/$PDY
 export SCENARIO=$FEMS
#----- remove existing and create new directory
 if [ -d $STATIC ]; then
    rm -rf $STATIC
 fi
 mkdir -p $STATIC

 if [ -d $SCENARIO ]; then
    rm -rf $SCENARIO
 fi
 mkdir -p $SCENARIO

#--------------------------------------------------------
# Directories used for log files from various execution
#--------------------------------------------------------
 export LOGS=$LOG2/logs-$PDY
 if [ -d $LOGS ]; then
    rm -rf $LOGS
 fi
 mkdir -p $LOGS
#--------------------------------------------------------
# Input files generated by bluesky2inv from step1
#--------------------------------------------------------
  export PTINV=$bluesky/$PDY/ptinv.bluesky.$PDYm1.ida.txt
  export PTDAY=$bluesky/$PDY/ptday.bluesky.$PDYm1.ida.lst
#--------------------------------------------------------
# Ancillary input files used by multiple SMOKE programs
#--------------------------------------------------------
 export PSTK=$FIXaqm/aqm_pstk.m3.txt
 export INVTABLE=$FIXaqm/aqm_invtable_criteria.txt
#--------------------------------------------------------
# Environment variables for Smkinven
#--------------------------------------------------------
 export SMK_SOURCE='P'
 export SMK_DEFAULT_TZONE=5
 export OUTZONE=0

 export SMK_NHAPEXCLUDE_YN=N
 export IMPORT_AVEINV_YN=Y 
 export DAY_SPECIFIC_YN=Y
 export HOUR_SPECIFIC_YN=N
 export HOURLY_FIRE_YN=Y
 export WKDAY_NORMALIZE=N
 export FILL_ANNUAL=N 
 export VELOC_RECALC=N 
 export WEST_HSPHERE=N 
 export RAW_DUP_CHECK=N
 export WRITE_ANN_ZERO=Y

#--------------------------------------------------------
# Smkinven output files
#--------------------------------------------------------
 export INVDIR=$LSMK/$PDY
 export INVNAME1=pnts.h.$PDY
 export INVNAME2=psrc.h.$PDY

 export PNTS=$INVDIR/$INVNAME1.map.fire.txt
 export PSRC=$INVDIR/$INVNAME2.txt
 export PDAY=$INVDIR/pday_raw.$PDY.ncf 
 export PSCC=$INVDIR/PSCC.$PDY.txt

 export REPINVEN=$INVDIR/repinven.$PDY.rpt   # report file

 export PNTDAT=$INVDIR/${INVNAME1}_dat

#----- remove existing and create new directory
 if [ -d $INVDIR ]; then
    rm -rf $INVDIR
 fi
 mkdir -p $INVDIR

 if [ -d $PNTDAT ]; then
    rm -rf $PNTDAT
 fi
 mkdir -p $PNTDAT

#--------------------------------------------------------
# Run Smkinven and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/smkinven.raw.fire.$PDY.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi

 export pgm=aqm_smkinven
# startmsg
 $EXECaqm/aqm_smkinven
 export err=$?;err_chk
#--------------------------------------------------------
# Define grid to use for modeling: Grdmat
#--------------------------------------------------------
 export IOAPI_GRIDNAME_1=US12_442X265
 export GRIDDESC=$FIXaqm/aqm_GRIDDESC_cs
#--------------------------------------------------------
# Grdmat Output gridding matrix
#--------------------------------------------------------
export PGMAT=$STATIC/pgmat_raw.$IOAPI_GRIDNAME_1.$PDY.fire.ncf
#--------------------------------------------------------
# Run Grdmat and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/grdmat_raw.$IOAPI_GRIDNAME_1.$PDY.fire.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi
 export pgm=aqm_grdmat
 #startmsg
 $EXECaqm/aqm_grdmat
 export err=$?;err_chk
#--------------------------------------------------------
# Define speciation mechanism and input files: Spcmat
#--------------------------------------------------------
 export SPC=cmaq.cb4p25
 export POLLUTANT_CONVERSION=N
 export GSREF=$FIXaqm/gsref_ptfire_cmaq_cb05_soa_2003af_29apr2008.txt  
 export GSPRO=$FIXaqm/gspro_ptfire_cmaq_cb05_soa_2003af_05may2008.txt
 export GSCNV=$FIXaqm/gscnv_cb05_notoxics_cmaq_new_11mar2008_v2.txt
#--------------------------------------------------------
# Spcmat Output speciation matrices
#--------------------------------------------------------
 export PSMAT_L=$STATIC/psmat_l_raw.$SPC.$PDY.fire.ncf
 export PSMAT_S=$STATIC/psmat_s_raw.$SPC.$PDY.fire.ncf
 export PSSUP=$STATIC/pssup.$SPC.$PDY.fire.txt
#--------------------------------------------------------
# Run Spcmat and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/spcmat_raw.$SPC.$PDY.fire.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi
 export pgm=aqm_spcmat
 # startmsg 
$EXECaqm/aqm_spcmat
 export err=$?;err_chk
#--------------------------------------------------------
# Define input files for Temporal program: Temporal
#--------------------------------------------------------
 export HOLIDAYS=$FIXaqm/noholidays.txt
 export DAY_SPECIFIC_YN=Y
 export PTREF=$FIXaqm/amptref.m3.us+can.txt
 export PTPRO=$FIXaqm/amptpro.small-daytime.us+can.txt
 #----- Assume a 48hr forecast (need 49 data from the 1st beginning)
 export G_STDATE=$jday
 export G_STTIME=${CYC}0000
 export G_RUNLEN=${nstep}0000
#--------------------------------------------------------
# Output temporal allocation and supplemental files
#--------------------------------------------------------
 export PTMP=$SCENARIO/ptmp_raw.$PDY.$cycle.fire.ncf
 export PTSUP=$SCENARIO/ptsup_raw.$PDY.$cycle.fire.txt
#--------------------------------------------------------
# Run temporal and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/temporal_raw.$PDY.fire.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi
 export pgm=aqm_temporal
# startmsg
 $EXECaqm/aqm_temporal
 export err=$?;err_chk
#--------------------------------------------------------------------
# Plume rise calculation
# if meteorological data is available ==> run met-dependent programs
#--------------------------------------------------------------------
 if [ METDATA_AVAILABLE='Y' ]; then
#----- Define env variables for Laypoint program
  export FIRE_PLUME_YN=Y
  export USE_BLUESKY_DATA=Y
  export SMK_EMLAYS=${SMKLAY:-}
#----- Met. input files
  export MET_CRO_3D=$METDIR/aqm.$cycle.metcro3d.ncf
  export MET_DOT_3D=$METDIR/aqm.$cycle.metdot3d.ncf
#----- Output layer fractions file
  export PLAY=$SCENARIO/play_raw.$PDY.$cycle.fire.ncf
#----- Run Laypoint and write logfile
  export LOGFILE=$LOGS/laypoint_raw.$PDY.fire.log
  if [ -f $LOGFILE ]; then
       rm -f $LOGFILE
  fi
  export pgm=aqm_laypoint
  #startmsg
  $EXECaqm/aqm_laypoint
   export err=$?;err_chk
 fi
#--------------------------------------------------------
# Define env variables for Smkmerge program
#--------------------------------------------------------
 export MRG_SOURCE='P'
 export MRG_BYDAY
 export MRG_TEMPORAL_YN=Y        # temporal
 export MRG_SPCMAT_YN=Y          # speciation
 export MRG_GRDOUT_YN=Y          # gridding matrix
 export MRG_REPSTA_YN=Y
 export MRG_REPCNY_YN=Y
 export MRG_LAYERS_YN=Y
 export SMK_PING_METHOD=0
 export SMK_ASCIIELEV_YN=N
 export SMK_AVEDAY_YN=N
 export MRG_GRDOUT_UNIT='moles/s'
 export MRG_TOTOUT_UNIT='moles/day'
 export PSMAT=$PSMAT_L
#----------------------------------------------------------
# Merge initial emission and extra emission from fire
#----------------------------------------------------------
# Output 3D emissions and report files
#--------------------------------------------------------
 export PGTS3D_L=$SCENARIO/pgts3d_l.$PDY.$cycle.fire.ncf
 export PGT=$SCENARIO/pgt.$PDY.$cycle.fire.ncf
 
 export REPPGTS_L=$STATIC/reppgts_l.$PDY.fire.rpt
 export REPPGT=$STATIC/reppgt.$PDY.fire.rpt

 if [ -f $PGTS3D_L ]; then
    rm -f $PGTS3D_L
 fi

 if [ -f $PGT ]; then
    rm -f $PGT
 fi
#--------------------------------------------------------
#   Run Smkmerge and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/smkmerge.$PDY.fire.nospec.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi
 export pgm=aqm_smkmerge
# startmsg
 $EXECaqm/aqm_smkmerge
 export err=$?;err_chk
#--------------------------------------------------------
# get inventory reports from Smkreport program
#--------------------------------------------------------
 export REPSTAT=$LOGS/Smkreports
 mkdir -p $REPSTAT
# REports to be created; note temporal must be used to capture
# day-specific fires
 export REPORT1=$REPSTAT/ptrep.state.$PDY.fire.rpt
 export REPORT3=$REPSTAT/ptrep.scc.$PDY.fire.rpt
 export REPORT5=$REPSTAT/ptgrep.state.$IOAPI_GRIDNAME_1.$PDY.fire.rpt
 export REPORT6=$REPSTAT/ptsrep.state.$PDY.fire.rpt
 export PSMAT=$PSMAT_S
 export REPCONFIG=$FIXaqm/repconfig.fire.inv.state.txt
#--------------------------------------------------------
# Run Smkreport and write logfile
#--------------------------------------------------------
 export LOGFILE=$LOGS/smkreport.$PDY.fire.log
 if [ -f $LOGFILE ]; then
    rm -f $LOGFILE
 fi
 export pgm=aqm_smkreport
# startmsg
 $EXECaqm/aqm_smkreport
 export err=$?;err_chk
#-----------------------------------------------------------------
# Location of log3 files, starting merge file
#-----------------------------------------------------------------
 LOG3=${LOG3:-}
 mkdir -p $LOG3
 export FILELIST=$LOG3/filelist.$PDY.txt
 if [ -e $FLIELIST ]; then
    rm -f $FILELIST
 fi
#-----------------------------------------------------------------
# Location of emission files for MRGGRID process
#-----------------------------------------------------------------

 cd $FEMS

 cp -rp $DATA/aqm.$cycle.emission.$PDY.windust_snowc.ncf   $COMOUT
 cp -rp $DATA/aqm.$cycle.emission.$PDY.windust_snowc.ncf   $FEMS

 export EGTS3D_L=$FEMS/aqm.$cycle.emission.$PDY.windust_snowc.ncf
 export PGTS3D_L=$FEMS/pgts3d_l.$PDY.$cycle.fire.ncf

 echo 'PGTS3D_L' >  $FILELIST
 echo 'EGTS3D_L' >> $FILELIST

#-----------------------------------------------------------------
# Information needed for MRGGRID
#-----------------------------------------------------------------
 export MRG_DIFF_DAYS=N
#-----------------------------------------------------------------
# Location of Execution files
#-----------------------------------------------------------------
 export LOGFILE=$LOG3/mrggrid.$PDY.$cycle.fire_HYSPLIT.log
 export OUTFILE=$FEMS/aqm.$cycle.$PDY.emis+fire_HYSPLIT.ncf

 if [ -e $LOGFILE ]; then
    rm -f $LOGFILE
 fi

 if [ -e $OUTFILE ]; then
    rm -f $OUTFILE
 fi
#-----------------------------------------------------------------
# Execution mrggrid
#-----------------------------------------------------------------
 export pgm=aqm_mrggrid
 $EXECaqm/aqm_mrggrid
 export err=$?;err_chk
 
 if [ ! -f $OUTFILE ]; then
    echo 'there is no fire lasting more than 24 hours'
    cp $COMOUT/aqm.$cycle.emission.$PDY.windust_snowc.ncf $OUTFILE
 fi 

cp $OUTFILE $COMOUT

cp -rp $COMOUT/aqm.${cycle}.emission.ncf $COMOUT/aqm.${cycle}.emission_old.ncf
cp -rp $COMOUT/aqm.${cycle}.$PDY.emis+fire_HYSPLIT.ncf $COMOUT/aqm.${cycle}.emission.ncf


########################################################

msg='ENDED NORMALLY.'
#postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################










