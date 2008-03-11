#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
# Script name:         aqm_5xwrf_gfs_138hbl.sh
# Script description:  Interpolate gfs O3 onto hyb levels
# Author:      Pius Lee Org: NP22 
# 2006-05-01 Luke Lin modified for production
#####################################################################
set -xa
msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

cd $DATA

echo "Start of aqm_5xwrf_gfs_138hbl.sh"

DATE=$(echo $PDY | cut  -c 1-8)
yyyy=$(echo $PDY | cut  -c 1-4)
mm=$(echo $PDY   | cut  -c 5-6)
dd=$(echo $PDY   | cut  -c 7-8)

export DATE yyyy mm dd 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]
then
   export endfhr=06
elif [  ${cycle} = 't06z' ]
then
   export endfhr=48
else
   export endfhr=48
fi

export fhr=00
typeset -Z2 fhr endfhr

while [ $fhr -le $endfhr ]
do

 export pgm=aqm_60hyb_gfs 
 . prep_step
 cat <<EOF5 >input${fhr}.prd
aqm.${cycle}_O3_${fhr}
aqm.${cycle}_O3_${fhr}
EOF5
 
 startmsg
  $EXECaqm/aqm_60hyb_gfs < input${fhr}.prd >> $pgmout 2>errfile
  export err=$?;err_chk
  cp $DATA/aqm.${cycle}.O3h${fhr}   $COMIN/.
 let "fhr=fhr+3"
 typeset -Z2 fhr
done

echo EXITING $0

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
