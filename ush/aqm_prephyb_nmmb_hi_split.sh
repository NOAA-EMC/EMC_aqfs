#!/bin/ksh

set -ax 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]; then
 export  hr_list1="00 01"
 export  hr_list2="04 05"
 export  hr_list3="02 06"
 export  hr_list4="03"
else
 export  hr_list1="00 01 08 12 16 20 24 28 32 36 40 44 48"
 export  hr_list2="04 05 09 13 17 21 25 29 33 37 41 45"
 export  hr_list3="02 06 10 14 18 22 26 30 34 38 42 46"
 export  hr_list4="03 07 11 15 19 23 27 31 35 39 43 47"
fi

export h_seg=$1
if [ ${h_seg} -eq 1 ];  then export hr_list=$hr_list1 ; fi
if [ ${h_seg} -eq 2 ];  then export hr_list=$hr_list2 ; fi
if [ ${h_seg} -eq 3 ];  then export hr_list=$hr_list3 ; fi
if [ ${h_seg} -eq 4 ];  then export hr_list=$hr_list4 ; fi

export fhr
for fhr in $hr_list
do
 $WGRIB -s  $COMNMM/nam.t${cyc}z.bgrd3d${fhr}.tm00 | \
  egrep -v "(:PRMSL:|:DZDT:|:FICE:|:TCOND:|:FRAIN:|:FRIME:|:LWHR:|:LRGHR:|:CNVHR:)" | \
 $WGRIB -i -grib $COMNMM/nam.t${cyc}z.bgrd3d${fhr}.tm00 -o nam.t${cyc}z.bgrd3d${fhr}.tm00
#===============================================
# using previous 6-hr or -24 hr fsct bgrd3d files as a proxy
#------------------------------------------------
case $cyc in
00) export flnm3=nam.t00z.bgrd3d24.tm00;;
06) export flnm3=nam.t06z.bgrd3d24.tm00;;
12) export flnm3=nam.t12z.bgrd3d24.tm00;;
18) export flnm3=nam.t18z.bgrd3d24.tm00;;
esac
 fsz1=0
 fsz2=0
 fc=0
 if [ $fhr = 00 ] ; then
 rm -rf nam.t${cyc}z.bgrd3d00.tm00

 $WGRIB -s  $COMNMM/nam.t${cyc}z.bgrd3d00.tm00 | \
  egrep -v "(:PRMSL:|:DZDT:|:FICE:|:TCOND:|:FRAIN:|:FRIME:|:LWHR:|:LRGHR:|:CNVHR:)" | \
 $WGRIB -i -grib $COMNMM/nam.t${cyc}z.bgrd3d00.tm00 -o nam.t${cyc}z.bgrd3d00.tm00

 $WGRIB -s  $COMNMM/nam.t${cyc}z.bgrd3d01.tm00 | \
  egrep -v "(:PRMSL:|:DZDT:|:FICE:|:TCOND:|:FRAIN:|:FRIME:|:LWHR:|:LRGHR:|:CNVHR:)" | \
 $WGRIB -i -grib $COMNMM/nam.t${cyc}z.bgrd3d01.tm00 -o nam.t${cyc}z.bgrd3d01.tm00

 export flnm1=nam.t${cyc}z.bgrd3d00.tm00
 export flnm2=nam.t${cyc}z.bgrd3d01.tm00
 fsz1=`ls -l $flnm1 | awk '{print $5}'`
 fsz2=`ls -l $flnm2 | awk '{print $5}'`
 ((fc=${fsz2}*7/10))
 fi
 if [ $fsz1 -lt $fc ] ; then
 cp -rp $flnm1 nam.t${cyc}z.bgrd3d00.tm00_proxy
 $WGRIB -s  $COMNMMm1/$flnm3 | \
  egrep -v "(:PRMSL:|:DZDT:|:FICE:|:TCOND:|:FRAIN:|:FRIME:|:LWHR:|:LRGHR:|:CNVHR:)" | \
  $WGRIB -i -grib $COMNMMm1/$flnm3 -o $flnm3
 ln -sf $flnm3 fort.11
 ln -sf nam.t${cyc}z.bgrd3d00.tm00_proxy fort.51
cat > itag  << EOF
$PDY$cyc
-24
EOF
/nwprod/util/exec/overdate.dgexgrib < itag > errfile
cp -rp nam.t${cyc}z.bgrd3d00.tm00_proxy nam.t${cyc}z.bgrd3d00.tm00
fi
#=====================================================

#  export pgm=aqm_nmm_prep
  echo $fhr
  export pgm=nmm_prdgen
#  . prep_step
  export FORT10="master3.ctl"
  export FORT21="$FIXaqm/aqm_wgt_bgrd12_139"

  cat <<EOF5 >input${fhr}.prd
nam.${cycle}.bgrd3d${fhr}.tm00
EOF5
  echo 'about to cat input'
  cat input${fhr}.prd

  rm -rf ${pgmout}.$h_seg
  startmsg
  $EXECaqm/aqm_prep_nmmb < input${fhr}.prd >> ${pgmout}.$h_seg 2>errfile
  export err=$?;err_chk

   if [ "${PARAFLAG}" = "YES" ]
   then
    rm -rf log_met_prep_grep1
    test_file1=$DATA/${pgmout}.$h_seg
     while [ ! -s log_met_prep_grep1 ] ; do
      if  [[ -s ${test_file1} ]] ; then
        grep -ni "END OF RESOURCE STATISTICS"  ${test_file1}   > log_met_prep_grep1
        break
        sleep 10
      else
       sleep 10
      fi
      sleep 10
     done
   fi

# added the following part to check file record number
# if the record number is not 848, reran itr;
# if the number is still not correct, exit and stop
#

  nrd_test=`/nwprod/util/exec/wgrib -v meso.AQFNMM${fhr} | wc -l`
  if [ $nrd_test -ne 847 ]; then
   echo "The record number of file is not correct at " $fhr
  startmsg
  $EXECaqm/aqm_prep_nmmb < input${fhr}.prd >> ${pgmout}.$h_seg 2>errfile
  export err=$?;err_chk
  nrd_test=`/nwprod/util/exec/wgrib -v meso.AQFNMM${fhr} | wc -l`
  if [ $nrd_test -ne 847 ]; then
   echo "The file was not completed after rerun"
   exit 999
  fi
  fi

  let "fhr=fhr+1"
  typeset -Z2 fhr
done
