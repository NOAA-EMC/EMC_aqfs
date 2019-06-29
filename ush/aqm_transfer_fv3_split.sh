#!/bin/ksh

set -ax 

if [ ${cycle} = 't00z' -o ${cycle} = 't18z' ]; then
 export  hr_list1="00 01"
 export  hr_list2="04 "
 export  hr_list3="02 "
 export  hr_list4="03"
 export  hr_list5="05"
 export  hr_list6="06"
else
# export  hr_list1="00 01 08 12 16 20 24 28 32 36 40 44 48"
# export  hr_list2="04 05 09 13 17 21 25 29 33 37 41 45"
# export  hr_list3="02 06 10 14 18 22 26 30 34 38 42 46"
# export  hr_list4="03 07 11 15 19 23 27 31 35 39 43 47"
 export  hr_list1="00 01 12 18 24 30 36 42 48 54 60 66 72"
 export  hr_list2="04 07 13 19 25 31 37 43 49 55 61 67"
 export  hr_list3="02 08 14 20 26 32 38 44 50 56 62 68"
 export  hr_list4="03 09 15 21 27 33 39 45 51 57 63 69"
 export  hr_list5="05 10 16 22 28 34 40 46 52 58 64 70"
 export  hr_list6="06 11 17 23 29 35 41 47 53 59 65 71"

fi

export h_seg=$1
if [ ${h_seg} -eq 1 ];  then export hr_list=$hr_list1 ; fi
if [ ${h_seg} -eq 2 ];  then export hr_list=$hr_list2 ; fi
if [ ${h_seg} -eq 3 ];  then export hr_list=$hr_list3 ; fi
if [ ${h_seg} -eq 4 ];  then export hr_list=$hr_list4 ; fi
if [ ${h_seg} -eq 5 ];  then export hr_list=$hr_list5 ; fi
if [ ${h_seg} -eq 6 ];  then export hr_list=$hr_list6 ; fi

export fhr
for fhr in $hr_list
do
echo "fhr=$fhr"
export pgm=aqm_transfer_fv3

typeset -Z3 fhr

    FILE1=gfs.t${cyc}z.atmf${fhr}.nemsio
    FILE2=gfs.t${cyc}z.sfcf${fhr}.nemsio
    FILE3=gfs.t${cyc}z.logf${fhr}.nemsio
    for file in ${FILE1} ${FILE2} ${FILE3}
      do
        if [ ! -s ${file} ]
        then
         cd $COMOUT
#         scp -p Jianping.Huang@surge.ncep.noaa.gov:${FV3_PARA}/gfs.${PDY}/${cyc}/${file} ./
#         scp -p Jianping.Huang@gyre.ncep.noaa.gov:${FV3_PARA}/gfs.${PDY}/${cyc}/${file} ./
       ln -s ${FV3_PARA}/gfs.${PDY}/${cyc}/${file} ./
        fi
       done

  export err=$?;err_chk

  let "fhr=fhr+1"
  typeset -Z2 fhr
done
