#!/bin/ksh

set -ax
export fdir=/naqfc/save/${USER}  
export usrdir=/naqfc/save/${USER}
export model_ver=v4.6.2
export envir=prod
export fdir=${fdir:-}
export fdir=${fdir}/nw${envir}/cmaq.${model_ver}/sorc

for src in prep_nmmb premaq_nmmb_v46 fcst_nmmb_v46 cmaq2grib post_maxi_CHA rdgrbwt_aot_CHA 
do
cd ${fdir}/aqm_${src}.fd
make clean
make
mv ${fdir}/aqm_${src}.fd/aqm_${src} ../../exec
done
