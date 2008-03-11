#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_mkprecip_5x.sh
# Script description:  Run CMAQ post processing 
#
# Author:  Geoff Manikin and Pius Lee  Org: NP22  Date: 2001-11-03
#
# Abstract: This script runs CMAQ post processing
#
# Script history log:
# 2005-11-02    Geoff Manikin, modified from production
# 2005-11-03    Pius Lee, modified for grid 138
######################################################################
set -xa

cd $DATA

. prep_step

############################################################
# Part I: calculate precip buckets emptying out at 3rd hours
############################################################
export fhr=00
case $cyc in
 00) endfhr=48;;
 06) endfhr=48;;
 12) endfhr=48;;
 18) endfhr=48;;
esac

while [ $fhr -le $endfhr ]
do

if [ $cyc -eq 00 -o $cyc -eq 12 ] ; then
 if [ $fhr -ne 00 -a $fhr -ne 03 -a $fhr -ne 15 -a $fhr -ne 27 -a \
      $fhr -ne 39  -a $fhr -ne 01 -a $fhr -ne 02 -a \
      $fhr -ne 13 -a $fhr -ne 14 -a $fhr -ne 25 -a $fhr -ne 26 -a \
      $fhr -ne 37 -a $fhr -ne 38  ]; then
   let mod=fhr%3
   if [ $mod -eq 0 ]; then
      let fhr3=fhr-3
   else
      let fhr3=fhr-mod
   fi
   typeset -Z2 fhr3
###start grid 138 processing
   cp $COMIN/aqm.${cycle}.aqfnam${fhr3}.tm00 ${fhr}.aqmnam${fhr3}
   cp $COMIN/aqm.${cycle}.aqfnam${fhr3}.tm00    .
   cp $COMIN/aqm.${cycle}.aqfnam${fhr}.tm00     .
   $utilexec/grbindex aqm.${cycle}.aqfnam${fhr3}.tm00 ${fhr}.aqmnami${fhr3}.tm00

   $utilexec/grbindex aqm.${cycle}.aqfnam${fhr}.tm00 ${fhr}.aqmnami${fhr}.tm00     
   cp aqm.${cycle}.aqfnam$fhr.tm00 ${fhr}.aqmnam${fhr}
   export pgm=aqm_makeprecip_138;
   . prep_step
   export XLFUNIT_13="${fhr}.aqmnam${fhr3}"
   export XLFUNIT_14="${fhr}.aqmnami${fhr3}.tm00"
   export XLFUNIT_15="${fhr}.aqmnam${fhr}"
   export XLFUNIT_16="${fhr}.aqmnami${fhr}.tm00"
   export XLFUNIT_50="3precip138.${fhr}"
   export XLFUNIT_51="3cprecip138.${fhr}"

cat > input.prd <<EOF5
$fhr
$fhr3
EOF5

   $EXECaqm/aqm_makeprecip_138 < input.prd >>$pgmout 2>errfil
   export err=$?;err_chk

   #cat 3precip138.${fhr} >> aqm.${cycle}.nam${fhr}.tm00
   #cat 3cprecip138.${fhr} >> aqm.${cycle}.nam${fhr}.tm00
   #cp aqm.${cycle}.nam${fhr}.tm00 $COMOUT/aqm.${cycle}.nam${fhr}.tm00
   cat 3precip138.${fhr} >> aqm.${cycle}.aqfnam${fhr}.tm00
   cat 3cprecip138.${fhr} >> aqm.${cycle}.aqfnam${fhr}.tm00
   cp aqm.${cycle}.aqfnam${fhr}.tm00 $COMOUT/aqm.${cycle}.aqfnam${fhr}.tm00

  fi
 fi

 let "fhr=fhr+1"
 typeset -Z2 fhr
done

################## END OF SCRIPT #######################

exit
