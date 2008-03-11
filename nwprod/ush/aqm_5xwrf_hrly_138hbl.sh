#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
# Script name:         aqm_5xwrf_hrly_138hbl.sh
# Script description:  Interpolate gfs O3 onto hyb levels
# Author:      Pius Lee Org: NP22 
# 2006-05-01 Luke Lin modified for production
#####################################################################
set -xa
msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

cd $DATA

echo "Start of aqm_5xwrf_hrly_138hbl.sh"

DATE=$(echo $PDY | cut  -c 1-8)
yyyy=$(echo $PDY | cut  -c 1-4)
mm=$(echo $PDY   | cut  -c 5-6)
dd=$(echo $PDY   | cut  -c 7-8)

export DATE yyyy mm dd 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]
then
   export endfhr=07
elif [  ${cycle} = 't06z' ]
then
   export endfhr=49
else
   export endfhr=49
fi

export fhr=00
export fhrp3=03
typeset -Z2 fhr fhrp3 endfhr

while [ $fhr -le $endfhr ]
do
 export pgm=aqm_60hyb_hrly
 . prep_step
 cat <<EOF5 >input${fhr}.prd
aqm.${cycle}.O3h${fhr}
aqm.${cycle}.O3h${fhrp3}
EOF5
 
 startmsg
  $EXECaqm/aqm_60hyb_hrly < input${fhr}.prd >> $pgmout 2>errfile
  export err=$?;err_chk
  let "fhrp1=fhr+1"
  let "fhrp2=fhr+2"
  typeset -Z2 fhrp1 fhrp2 
  cp $DATA/aqm.${cycle}.O3hb${fhr}   $COMIN/.
  cp $DATA/aqm.${cycle}.O3hb${fhrp1} $COMIN/.
  cp $DATA/aqm.${cycle}.O3hb${fhrp2} $COMIN/.
 let "fhr=fhr+3"
 let "fhrp3=fhrp3+3"
 typeset -Z2 fhr
done

echo EXITING $0

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
