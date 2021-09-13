#!/bin/sh
#

model=$1
cyc=$2
pgm=$model$cyc

set +x
[ -z "$utilscript" ] && utilscript=/nwprod/util/ush
if test "$err" -ne '0'
then
  echo "*******************************************************"
  echo "******  PROBLEM ARCHIVING $pgm RETURN CODE $err  ******"
  echo "*******************************************************"
  msg1="PROBLEM ARCHIVING $pgm RETURN CODE $err"
  sh $utilscript/postmsg.sh "$msg1"
else
  echo " --------------------------------------------- "
  echo " ********** COMPLETED ARCHIVE $pgm  **********"
  echo " --------------------------------------------- "
  msg="ARCHIVE of $pgm COMPLETED NORMALLY"
  sh $utilscript/postmsg.sh "$msg"

  if test "$SENDECF" = "YES"
  then
    ###########################################################
    ecflow_client --event $pgm
    ###########################################################
 
    ###########################################################
    rate_label_log=$DATA/rate_label_log
    echo "node:$LSB_HOSTS  PDY=$PDY " > $rate_label_log
    /nwprod/spa_util/hpss/hpss_transfer_rates.pl $LSB_OUTPUTFILE | grep "Total for all Jobs" >> $rate_label_log
    ecflow_client --label info "`cat $rate_label_log | grep -v output `"
    ###########################################################

  fi
fi

exit
