#!/bin/ksh

set -ax
if [ ${USER}  = "Jianping.Huang" ]
then
  export fdir=/naqfc/save/${USER}  
  export usrdir=/naqfc/save/${USER}
  export model_ver=v4.6.9
  export envir=dev
else
  export model_ver=v4.6.9
  export envir=para
fi

export fdir=${fdir:-}
export usrdir=${usrdir:-}
export fdir1=${fdir}/nw${envir}/cmaq.${model_ver}/sorc

for src in prep_nmmb premaq_nmmb_v46 fcst_nmmb_v46 cmaq2grib post_maxi_CHA rdgrbwt_aot_CHA snowdust fengsha fengsha_merge fire_checking 

do
cd ${fdir1}/aqm_${src}.fd
make clean
make 
done
