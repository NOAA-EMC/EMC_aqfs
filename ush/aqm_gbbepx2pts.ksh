#!/bin/ksh -x

if [ "0" = "1" ] ; then


if [ ! -s $COMIN/GBBEPx_addVIIRS.emisX.001.$PDY.nc ]; then
 if wget -N -O $COMIN/GBBEPx_addVIIRS.emisX.001.$PDY.nc https://gsce-dtn.sdstate.edu/index.php/s/ttOjJBOvXlxcVPe/download?path\=\%2F\&files\=GBBEPx_addVIIRS.emisX.001.$PDY.nc
  then
  echo "succesfully download GBBEPx_addVIIRS.emisX.001.$PDY.nc"
  FIREDATE=$PDY
 elif wget -N -O $COMIN/GBBEPx_addVIIRS.emisX.001.$PDYm1.nc https://gsce-dtn.sdstate.edu/index.php/s/ttOjJBOvXlxcVPe/download?path\=\%2F\&files\=GBBEPx_addVIIRS.emisX.001.$PDYm1.nc
  then
  echo "succesfully download GBBEPx_addVIIRS.emisX.001.$PDYm1.nc"
  FIREDATE=$PDYm1 
 elif wget -N -O $COMIN/GBBEPx_addVIIRS.emisX.001.$PDYm2.nc https://gsce-dtn.sdstate.edu/index.php/s/ttOjJBOvXlxcVPe/download?path\=\%2F\&files\=GBBEPx_addVIIRS.emisX.001.$PDYm2.nc
  then
  echo "succesfully download GBBEPx_addVIIRS.emisX.001.$PDYm2.nc"
  FIREDATE=$PDYm2 
 else
  echo "error occurred in downloading"
  exit 1  
 fi
else
 FIREDATE=$PDY
fi

fi

if [ $FCST = "NO" ] ; then
 FIREDATE=$PDYm1
 emisfile=GBBEPx_all01GRID.emissions_v003_$PDYm1.nc
 COMIN9=$COMINm1
elif [ -s $COMIN/GBBEPx_all01GRID.emissions_v003_$PDY.nc ]; then
 COMIN9=$COMIN
 emisfile=GBBEPx_all01GRID.emissions_v003_$PDY.nc
 FIREDATE=$PDY 
 else [ -s $COMIN/GBBEPx_all01GRID.emissions_v003_$PDYm1.nc ] 
 COMIN9=$COMIN
 emisfile=GBBEPx_all01GRID.emissions_v003_$PDYm1.nc
 FIREDATE=$PDYm1
fi

cat>gbbepx2pts.ini<<!
&control
efilein='$COMIN9/$emisfile'
markutc=18
burnarea_ratio=0.1
frpfactor=1.0
startdate=${FIREDATE}06
nduration=127
tdiurnal=0.03033772, 0.03033772, 0.03033772, 0.03033772, 0.03033772,
       0.03033772, 0.03033772, 0.03434459, 0.03720664, 0.04006869,
       0.05724098, 0.07441328, 0.09158558, 0.09730967, 0.06868918,
       0.04006869, 0.03434459, 0.03033772, 0.03033772, 0.03033772,
       0.03033772, 0.03033772, 0.03033772, 0.03033772 
dfrac=1.00,0.25,0.25
emname='CO','NO','NO2','SO2','NH3','PEC','POC','PMOTHR','PNO3','PSO4',
'PAL','PCA','PCL','PFE','PK','PMG','PMN','PNA','PNCOM','PNH4','PSI','PTI'
/

Species Converting Factor
'CO'     1   # kg ->mole
'CO'     0
'NOX'    2   # 90% to NO (mw 30), 10% to NO2 (mw 46), mw 31.6 in average, kg->mole
'NO'     0   'NO2'  0 
'SO2'    1    # kg -> mole
'SO2'    0 
'NH3'    1
'NH3'    0 
'BC'     1    # kg -> g  
'PEC'    0.  # 1000. 
'OC'     1
'POC'    0.  # 1000. 
'PM2.5' 16    # pm2.5 splitt factor kg -> g
'PEC' 94.9   'POC' 461.8  'PMOTHR'  13.7 'PNO3' 1.323 'PSO4' 12.6 
'PAL' 0.6075 'PCA' 3.858 'PCL' 41.5 'PFE' 0.434 'PK' 29.4
'PMG' 0.314  'PNA' 5.7335 'PNCOM' 323.2 'PNH4' 8.7915 'PSI' 1.8185  'PTI' 0.0515
!

export IOAPI_ISPH=19
export GRID_NAME=AQF_CONUS
export GRIDDESC=$COMIN/aqm_griddesc05
export TOPO=$FIXaqm/aqm_gridcro2d.landfac.5x.ncf

# output
#export STACK_GROUP=$COMIN/stack_groups_ptfire_${PDY}.5x.ncf
#export PTFIRE=$COMIN/inln_mole_ptfire_${PDY}.5x.ncf
export STACK_GROUP=aqm.$cycle.fire_location_cs.ncf
export PTFIRE=aqm.$cycle.fire_emi_cs.ncf

startmsg
$EXECaqm/aqm_gbbepx2pts
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
#  mv $DATA/aqm*fire*ncf $CHK_DIR
  cp -rp $DATA/aqm*fire*ncf $CHK_DIR
 else
  mv $DATA/aqm.$cycle.fire_location_cs.ncf $CHK_DIR/aqm.$cycle.fire_location_cs_r.ncf
  mv $DATA/aqm.$cycle.fire_emi_cs.ncf $CHK_DIR/aqm.$cycle.fire_emi_cs_r.ncf
 fi

else
 echo "aqm_gbbepx2emis-aero run failed"
 exit 1
fi


