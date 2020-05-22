#!/bin/ksh

set -xa 

cd $DATA

cyc=00

#nowdate=`${NDATE}| cut -c1-8`

#cycledate=${1:-$nowdate}$cyc
cycledate=${1:-$PDY}$cyc

cyear=`echo $cycledate | cut -c1-4`
cmonth=`echo $cycledate | cut -c5-6`
cdate=`echo $cycledate | cut -c7-8`
cjulian=`/bin/date --date=$cyear'/'$cmonth'/'$cdate +%j`
typeset -Z3 cjulian

#FV3CHEMFOLDER=${FV3CHEM_DIR}/gfs.$cyear$cmonth$cdate/00
#if [ ! -s $FV3CHEMFOLDER/gfs.t${cyc}z.atmf120.nemsio ]; then
#if [ -s ${FV3CHEM_DIR}/gfs.${PDY}/00/gfs.t${cyc}z.atmf120.nemsio ]; then
#  FV3CHEMFOLDER=${FV3CHEM_DIR}/gfs.${PDY}/00
#elif [ -s ${FV3CHEM_DIR}/gfs.${PDYm1}/00/gfs.t${cyc}z.atmf120.nemsio ]; then
#  FV3CHEMFOLDER=${FV3CHEM_DIR}/gfs.${PDYm1}/00
#elif [ -s ${FV3CHEM_DIR}/gfs.${PDYm2}/00/gfs.t${cyc}z.atmf120.nemsio ]; then
#  FV3CHEMFOLDER=${FV3CHEM_DIR}/gfs.${PDYm2}/00
#else
# echo " can not find $FV3CHEMFOLDER/gfs.t${cyc}z.atmf120.nemsio "
# exit 1
#fi 
if [ -s   ${COMINgefs}/${cyc}/chem/sfcsig/geaer.t${cyc}z.atmf120.nemsio ]; then
  GEFSAEROFOLDER=${COMINgefs}/${cyc}/chem/sfcsig
elif [ -s ${COMINgefsm1}/${cyc}/chem/sfcsig/geaer.t${cyc}z.atmf120.nemsio ]; then
  GEFSAEROFOLDER=${COMINgefsm1}/${cyc}/chem/sfcsig
else
 echo " can not find $GEFSAEROFOLDER/geaer.t${cyc}z.atmf120.nemsio "
 exit 1
fi

outdir=$COMOUT
if [ ! -s $outdir ]; then
 mkdir -p $outdir
fi 
 
cat > gefs-bnd-nemsio.ini <<EOF
&control
 begyear=$cyear  
 begdate=$cjulian
 begtime=$cyc    
 dtstep=6        
 numts = 31
 bndname='NO2','NO','O3','NO3','OH','HO2','N2O5','HNO3','HONO','PNA',
 'H2O2','CO','SO2','SULF','PAN','FACD','AACD','PACD','UMHP','MGLY',
 'OPEN','CRES','FORM','ALD2','PAR','OLE','TOL','ISOP','ETH','XYL',
 'ASO4J','ASO4I','ASOIL','NH3','NUMATKN','NUMACC','NUMCOR',
 'SRFATKN','SRFACC','AOTHRJ',AECJ,APOCJ
 checkname='AOTHRJ','ASOIL','AECJ','APOCJ'
 mofile='$GEFSAEROFOLDER/geaer.t${cyc}z.atmf','.nemsio'
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

export TOPO=$FIXaqm/aqm.grdcro2d_new.ncf
if [ -s $COMIN/aqm.t${cyc}z.metcro3d.ncf ] ; 
then
 export METEO3D=$COMIN/aqm.t${cyc}z.metcro3d.ncf
else
 export METEO3D=$COMINm1/aqm.t12z.metcro3d.ncf
fi
export BND1=$FIXaqm/aqm_conus_12km_geos_2006${cmonth}_static_35L.ncf


export BND2=$outdir/aqm_conus_geos_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf        # output bnd files
export CHECK2D=$outdir/check_fv3chem_aero_${cyear}${cmonth}${cdate}_35L.ncf

rm -rf chkreads.log

startmsg
$EXECaqm/aqm_fv3chem_dlbc  >> $pgmout 2>errfile 
export err=$?;err_chk
