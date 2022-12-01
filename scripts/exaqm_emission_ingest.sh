#!/bin/bash
#######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_emission_ingest.sh.ecf
# Script description:  check the emission files on number of 
#                      files and file size for monthly, yearly and
#                      jtable.
#
#  #Input Environment Variables:
#    DCOMIN             # /dcom/us007003/aqm_emission/${envir}
#    COMOUT             # /com/${NET}/${envir}/emission
#
set -x

##################
# define mail list
##################

# for the production mailing list

if [ ${USER::4} != ops. ]; then
  export maillist=$(whoami)@noaa.gov
elif [ $envir != prod ]; then
  export maillist=${maillist:-"nco.spa@noaa.gov"}
else
  export maillist="nco.spa@noaa.gov,youhua.tang@noaa.gov,ho-chun.huang@noaa.gov"
fi
export SENDMAIL=${SENDMAIL:-Y}  ## change sendmail to SENDMAIL to avoid confusion with /usr/sbin/sendmail

# calculate number days of the next month
days_in_next_month=`cal $(date +%m -d 'next month') $(date +%Y) | grep -v '[A-Za-z]' | wc -w`
echo "number days in the next month= $days_in_next_month"

# calculate next month
next_month=`date +'%m' -d 'next month'`
echo " the next month= $next_month"

#  calculate next year 
next_year=`date +%Y -d 'next year'`
echo " the next year= $next_year"

# get the current year 
current_year=`date +%Y`
echo " the current year= $current_year"

# get the current day  
current_day=`date +%d`
echo " the current day= $current_day"

if (( $next_month == 01 )); then
   yyyymm=${yyyymm:-$next_year$next_month}
else
   yyyymm=${yyyymm:-$current_year$next_month}
fi

echo " the next month emission file directory= $yyyymm"

number_days_next_year=`date -d "$next_year/12/31" +"%j"`
echo "number days of the next year= $number_days_next_year"


###############################################################
# This section checks and copies the monthly emission files
###############################################################
i=1
#i=0
if [ $i -eq 1 ]; then
#where the monthly emission directory
in_dir=$DCOMIN/$yyyymm
out_dir=$COMOUT/$yyyymm

#    copy monthly emission files from /dcom to /com
# dcom directory check
if [ ! -d "$in_dir" ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- AQM monthly emission /dcom directory not exist"
    echo " FATAL ERROR --- /dcom monthly emission directory $in_dir not found" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi 
    err_exit "FATAL ERROR- dcom monthly emission directory $in_dir not found"
fi
 
# dcom directory files check
no_files_in_dcom=$(ls $in_dir/* | wc -l)

if (( $no_files_in_dcom == 0 )); then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- No ARL AQM monthly emission file in /dcom directory"
    echo " FATAL ERROR --- No ARL AQM monthly emission file in $in_dir" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
    err_exit "FATAL ERROR- No ARL AQM monthly emission file in $in_dir"
fi
 
mkdir -p -m 775 $out_dir
# JY - will comment out the following two lines ??
cp $in_dir/*AK* $out_dir/.
cp $in_dir/*HI* $out_dir/.
cp $in_dir/*5x* $in_dir/emis*ncf $out_dir/.
cp $in_dir/gfs.geo*nc $out_dir/.
# check the file number monthly directory

no_file_in_com=`ls $out_dir/* | wc -l`
#no_file_shouldbe=$(( 4 * $days_in_next_month ))
no_file_shouldbe=40

if [ $no_file_in_com -lt $no_file_shouldbe ]; then
    echo " the number files copied to com= $no_file_in_com "
    echo " the number files should be in com= $no_file_shouldbe "
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- number of ARL AQM monthly emission files is incorrect"
    echo " FATAL ERROR --- number of ARL AQM monthly emission files in $out_dir is incorrect" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
    err_exit "FATAL ERROR- number of ARL AQM monthly emission files in $out_dir is incorrect"
fi
 
# check the file size
total_filesize_in_AK=`du -Dbc $out_dir/*AK* | tail -1 | cut -f1`
small_filesize_in_AK=`ls -l $out_dir/*AK* | cut -d' ' -f5 | sort -u | tail -1`

if [ $total_filesize_in_AK -lt 20000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="Warning --- total file sizes of ARL AQM monthly AK domain emission are too small"
    echo " Warning --- total file sizes of ARL AQM monthly AK domain in $out_dir are too small" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
   echo " Warning --- total file sizes of ARL AQM monthly AK domain in $out_dir are too small"
fi
 
if [ $small_filesize_in_AK -lt 10000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- the file size of ARL AQM monthly AK domain emission is incorrect"
    echo " FATAL ERROR --- the file size of ARL AQM monthly AK domain in $out_dir is incorrect" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
    err_exit "FATAL ERROR- the monthly emission AK domain file size is not large enough"
fi
 
total_filesize_in_HI=`du -Dbc $out_dir/*HI* | tail -1 | cut -f1`
small_filesize_in_HI=`ls -l $out_dir/*HI* | cut -d' ' -f5 | sort -u | tail -1`
 
if [ $total_filesize_in_HI -lt 18000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="Warning --- total file sizes of ARL AQM monthly HI domain emission are too small"
    echo " Warning --- total file sizes of ARL AQM monthly HI domain in $out_dir are too small" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
   echo " Warning --- total file sizes of ARL AQM monthly HI domain in $out_dir are too small"
fi
 
if [ $small_filesize_in_HI -lt 10000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- the file size of ARL AQM monthly HI domain emission is incorrect"
    echo " FATAL ERROR --- the file size of ARL AQM monthly HI domain in $out_dir is incorrect" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi 
    err_exit "FATAL ERROR- the monthly emission HI domain file size is not large enough"
fi
 
total_filesize_in_CONUS=`du -Dbc $out_dir/*5x* $out_dir/emis* | tail -1 | cut -f1`
small_filesize_in_CONUS=`ls -l $out_dir/*5x* $out_dir/emis* | cut -d' ' -f5 | sort -u | tail -1`

if [ $total_filesize_in_CONUS -lt 220000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="Warning --- total file sizes of ARL AQM monthly CONUS domain emission are too small"
    echo " Warning --- total file sizes of ARL AQM monthly CONUS domain in $out_dir too small" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
   echo " Warning --- total file sizes of ARL AQM monthly CONUS domain in $out_dir too small"
fi
 
if [ $small_filesize_in_CONUS -lt 100000000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="FATAL --- the file size of ARL AQM monthly CONUS domain emission is incorrect"
    echo " FATAL ERROR --- the file size of ARL AQM monthly CONUS domain in $out_dir is incorrect" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
    err_exit "FATAL ERROR- the monthly emission CONUS domain file size is not large enough"
fi

total_filesize_in_geo=`du -Dbc $out_dir/gfs.geo* | tail -1 | cut -f1`
if [ $total_filesize_in_geo -lt 200000000 ]; then
   if [ $SENDMAIL = 'Y' ]; then 
    subject="Warning --- GEO file size of ARL AQM monthly is too small"
    echo " Warning --- GEO file size of ARL AQM monthly in $out_dir is too small" > email_body
    cat email_body |mail.py -s "$subject" $maillist
   fi
   echo " Warning --- GEO file size of ARL AQM monthly in $out_dir is too small"
fi
 
#################################
# Monthly emission files are fine
#################################
 if [ $SENDMAIL = 'Y' ]; then
   subject="The monthly ARL AQM emission files have been successfully copied"
   echo "The monthly ARL AQM emission files have been successfully copied from $in_dir to $out_dir ." > email_body
   echo "Please double check and validate." >> email_body
   cat email_body |mail.py -s "$subject" $maillist 
 fi
fi
