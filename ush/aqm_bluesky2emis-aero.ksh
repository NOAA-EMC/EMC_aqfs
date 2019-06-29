#!/bin/ksh -x

# 4/18/2019   modified for calculating fire emission to support FV3GFS/CMAQ 

if [ -s ${smoke_emis}/smokecs.$PDY/files_fires_cs.t${cyc}z.tar ] ; then
 cp -p ${smoke_emis}/smokecs.$PDY/files_fires_cs.t${cyc}z.tar $DATA/files_fires_cs.tar
else
 echo "No files_fires_cs.tar from HYSPLIT/BlueSky"
 exit 
fi
if [ -s ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z ] ; then
 cp -p ${smoke_emis}/smokecs.$PDY/EMITIMES.t${cyc}z $DATA/EMITIMES
else
 echo "No EMITIMES from HYSPLIT/BlueSky"
 exit 1
fi

cd $DATA
tar -xvf files_fires_cs.tar 

nfiles=`ls -1 NOAA????_${PDYm1}.OUT |wc -l`

cat>bluesky2emis-aero.ini<<!
&control
prefix='NOAA'
suffix='_${PDYm1}.OUT'
nfiles=$nfiles
emtimefile='EMITIMES'
em24hr_ext=.TRUE.
startdate=${PDYm1}00
nduration=121
tdiurnal=0.00570114, 0.00570114, 0.00570114, 0.00570114,0.00570114, 0.00570114, 0.00570114, 0.00570114, 0.00570114,
 0.00570114, 0.02000401, 0.04000801, 0.07001400, 0.10002000, 0.13002600, 0.16003200, 0.17003400, 0.12002400, 0.07001401,
 0.04000801, 0.00570114, 0.00570114, 0.00570114, 0.00570114
dfrac=4,1,1
emi_factor_wf=1.
emi_factor_rx=0.114
if_fcst='$FCST'
emname='CO','PEC','POC','PMOTHR','PNO3','PSO4','PMC',
'PAL','PCA','PCL','PFE','PK','PMG','PMN','PNA','PNCOM','PNH4','PSI','PTI'
/

Species Converting Factor
'pm25' 16    # pm2.5 splitt factor g -> g
'PEC' 0.0949 'PMOTHR'  0.0137 'PNO3' 0.001323 'POC' 0.4618 'PSO4' 0.0126
'PAL' 6.075E-4 'PCA' 0.003858 'PCL' 0.0415 'PFE' 4.34E-4 'PK' 0.0294
'PMG' 3.14E-4  'PNA' 0.0057335 'PNCOM' 0.3232 'PNH4' 0.0087915 'PSI' 0.0018185  'PTI' 5.15E-5
'pmcoarse' 1  # PM10-PM25
'PMC'   1.0
'co'     1   # g ->mole
'CO'   0.0   # 0.0357
#'ch4'     1
#'CH4'    0.0623
#'nmhc'  11      # g-> mole 1VOC=1.25454774 TOG 
#'ALDX'  4.579e-4 'ETH' 8.546e-3 'ETHA' 4.368e-3 'FORM' 2.986e-6 
#'IOLE'  1.162e-4 'OLE' 2.175e-3 'PAR' 0.0281 'TERP' 1.597e-4 'TOL' 3.987e-4
#'UNR'  8.16e-3  'XYL'  3.023e-4
!

export IOAPI_ISPH=19
export GRID_NAME=AQF_CONUS
if [ "$FCST" = "YES" ]; then
 export GRIDDESC=$COMIN/aqm_griddesc05
 export TOPO=$COMIN/aqm.$cycle.grdcro2d.ncf # $COMIN/aqm.t12z.grdcro2d.ncf
else
 export GRIDDESC=$COMINm1/aqm_griddesc05
 export TOPO=$COMINm1/aqm.$cycle.grdcro2d.ncf
fi
# output 
export STACK_GROUP=aqm.$cycle.fire_location_cs.ncf
export PTFIRE=aqm.$cycle.fire_emi_cs.ncf

startmsg
$EXECaqm/aqm_bluesky2emis-aero
export err=$?;err_chk

if [ "$FCST" = "YES" ]; then
 CHK_DIR=$COMIN 
else
 CHK_DIR=$COMINm1
fi
if [ -s $PTFIRE -a -s $STACK_GROUP ]; then
#  if [ -s $CHK_DIR/$PTFIRE ]; then
#   mv $CHK_DIR/$PTFIRE $CHK_DIR/${PTFIRE}.1
#  fi
#  if [ -s $CHK_DIR/$STACK_GROUP ]; then
#   mv $CHK_DIR/$STACK_GROUP $CHK_DIR/${STACK_GROUP}.1
#  fi  

 if [ "$FCST" = "YES" ]; then
  mv $DATA/aqm*fire*ncf $CHK_DIR
 else
  mv $DATA/aqm.$cycle.fire_location_cs.ncf $CHK_DIR/aqm.$cycle.fire_location_cs_r.ncf 
  mv $DATA/aqm.$cycle.fire_emi_cs.ncf $CHK_DIR/aqm.$cycle.fire_emi_cs_r.ncf 
 fi

else
 echo "bluesky2emis-aero run failed"
 exit 1
fi
