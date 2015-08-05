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
  'ASO4J','ASO4I','A25J','ASOIL','NH3'
 checkname='A25J','ASOIL'
 mofile='$NGACFOLDER/ngac.t${cyc}z.sigf',''
 checklayer=1    
&end

Species converting Factor
# Gocart ug/m3 to regional ug/m3
'du001'    1  ## 0.2-2um (kg/kg)
'A25J'         1.0 
'du002'    2  ## 2-4um
'A25J'        0.4187   'ASOIL'  0.5813
'du003'    1  ## 4-6um
'ASOIL'   1.0
'du004'    1   ## 6-12um
'ASOIL'  0.7586
EOF

export TOPO=$FIXaqm/aqm.grdcro2d_new.ncf
export METEO3D=$outdir/aqm.t${cyc}z.metcro3d.ncf
export BND1=$FIXaqm/aqm_conus_12km_geos_2006${cmonth}_static_35L.ncf


export BND2=$outdir/aqm_conus_geos_ngac_dust_${cyear}${cmonth}${cdate}_35L.ncf        # output bnd files
export CHECK2D=$outdir/check_ngac_dust_${cyear}${cmonth}${cdate}_35L.ncf

startmsg
$EXECaqm/aqm_ngac_dust_dlbc  >> $pgmout 2>errfile 
export err=$?;err_chk
