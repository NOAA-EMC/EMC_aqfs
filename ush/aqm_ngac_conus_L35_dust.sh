#!/bin/ksh

set -xa 

cd $DATA

cyc=00

nowdate=`${NDATE}| cut -c1-8`

cycledate=${1:-$nowdate}$cyc

cyear=`echo $cycledate | cut -c1-4`
cmonth=`echo $cycledate | cut -c5-6`
cdate=`echo $cycledate | cut -c7-8`
cjulian=`/bin/date --date=$cyear'/'$cmonth'/'$cdate +%j`
typeset -Z3 cjulian

NGACFOLDER=${NGAC_DIR}/ngac.$cyear$cmonth$cdate
if [ ! -s $NGACFOLDER/ngac.t${cyc}z.sigf48 ]; then
 echo " can not find $NGACFOLDER/ngac.t${cyc}z.sigf48 "
 exit 1
fi 

outdir=$COMOUT
if [ ! -s $outdir ]; then
 mkdir -p $outdir
fi 
 
cat > ngac-bnd-nemsio.ini <<EOF
&control
 begyear=$cyear  
 begdate=$cjulian
 begtime=$cyc    
 dtstep=3        
 numts = 31
 bndname='NO2','NO','O3','NO3','OH','HO2','N2O5','HNO3','HONO','PNA',
  'H2O2','CO','SO2','SULF','PAN','FACD','AACD','PACD','UMHP','MGLY',
  'OPEN','CRES','FORM','ALD2','PAR','OLE','TOL','ISOP','ETH','XYL',  
  'ASO4J','ASO4I','A25J','ASOIL','NH3','NUMATKN','NUMACC','NUMCOR',
  'SRFATKN','SRFACC'
 checkname='A25J','ASOIL'
 mofile='$NGACFOLDER/ngac.t${cyc}z.sigf',''
 checklayer=1    
&end

Species converting Factor
# Gocart ug/m3 to regional ug/m3 
'du001'    2  ## 0.2-2um (kg/kg) assuming mean diameter is 1 um (volume= 0.5236x10^-18 m3) and density is 2.6x10^4 g/m3 or 2.6x10^10 ug/m3. so 1 particle = 1.36x10^-8 ug
'AOTHRJ'         1.0    'NUMACC'  73529400.
'du002'    4  ## 2-4um assuming mean diameter is 2.5um for J (1 ug= 73529400*0.5^3/1.25^3 = 0.064*73529400 = 4705800 #) and 3.5 for ASOIL
'AOTHRJ'        0.4187  'NUMACC'  1970300.   'ASOIL'  0.5813  'NUMCOR'  996900.
'du003'    2  ## 4-6um assume mean diamter is 5um
'ASOIL'   1.0   'NUMCOR'  588200.
'du004'    2   ## 6-12um  assume mean diamter is 9
'ASOIL'  0.7586   'NUMCOR' 76500.
EOF

export TOPO=$FIXaqm/aqm.grdcro2d_new.ncf
if [ -s $COMIN/aqm.t${cyc}z.metcro3d.ncf ] ; 
then
 export METEO3D=$COMIN/aqm.t${cyc}z.metcro3d.ncf
else
 export METEO3D=$COMINm1/aqm.t12z.metcro3d.ncf
fi
#export BND1=$FIXaqm/aqm_conus_12km_geos_2006${cmonth}_static_35L.ncf
export BND1=$FIXaqm/aqm_conus_12km_geos_2006${cmonth}_static_FV3_35L.ncf


export BND2=$outdir/aqm_conus_geos_ngac_dust_${cyear}${cmonth}${cdate}_35L.ncf        # output bnd files
export CHECK2D=$outdir/check_ngac_dust_${cyear}${cmonth}${cdate}_35L.ncf

rm -rf chkreads.log

startmsg
$EXECaqm/aqm_ngac_dust_dlbc  >> $pgmout 2>errfile 
export err=$?;err_chk
