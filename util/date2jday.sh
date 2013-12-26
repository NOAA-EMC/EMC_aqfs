#! /bin/sh
# set -ax
if test $# -ne 1
then
   echo "usage: $0 yyyyddd|yyyymmdd"
   exit 1
fi

input_date=$1
strlen=${#input_date}


case $strlen in
  7 )
      year=`echo $input_date | cut -c1-4`
      jday=`echo $input_date | cut -c5-7 | sed 's/^[0]*//'`;
      let nhours=$jday*24-24
      yyyymmddhh=`/nwprod/util/exec/ndate $nhours ${year}010100`
      yyyymmdd=`echo $yyyymmddhh | cut -c1-8`

      echo $yyyymmdd
      ;;

  8 )
      year=`echo $input_date | cut -c1-4`
      let yearm1=$year-1

      hours=`/nwprod/util/exec/nhour ${input_date}00 ${yearm1}123100`
      let ddd=$hours/24

      if [ $ddd -lt 100 ] ; then
        ddd=0${ddd}
        if [ $ddd -lt 10 ] ; then
          ddd=0${ddd}
        fi
      fi

      echo ${year}${ddd}
      ;;

  * )
      echo "usage: $0 yyyyddd|yyyymmdd"
      ;;
esac

exit
