#!/bin/sh
################################################################3
#
#  This script will tar up the directory specified by the first
#  argument ($1) and place the tar file on the HPSS server
#  under ${HPSSOUT}.  The tar file is put in the directory
#  appropriate for data valid for the day specified as the second 
#  command line argument ($2).
#  The third argument, if present, indicates whether the tar file 
#  should be written to the 1-year, 2-year, or permanent archive.
#  Valid values for the 3rd argument are "1", "2" or "perm"
#
#  Usage: rhist_savedir.sh Directory Date(YYYYMM format) [1|2|perm]
#
#  Where: Directory  = Directory to be tarred.
#
################################################################3
set -x

if [ $# -ne 2 ] && [ $# -ne 3 ] 
then
  echo "Usage: rhist_savedir.sh Directory Date(YYYYMM format) [1|2|perm]"
  exit 1
fi 
#
#   Get directory to be tarred from the first command line argument,
#   and check to make sure that the directory exists.
#

dir=$1
if [ ! -d $dir ]
then
  echo "rhist_savedir.sh:  Directory $dir does not exist."
  exit 2
fi 

#
#   cd to the directory to be tarred.
# 

cd $dir

#
#   Generate the name of the tarfile, which should be the same
#   as the absolute path name of the directory being
#   tarred, except that "/" are replaced with "_".
#

tarfile=`echo $PWD | cut -c 2- | tr "/" "_"`
tarfile=${tarfile}.tar

year=`echo $2 | cut -c 1-4`
yearmo=`echo $2 | cut -c 1-6`

#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS or HSM.
#

if [ $TSM_FLAG = 'NO' ]
then
  if [ $# -eq 3 ]
  then
      if [ $3 = "1" ]
      then
           hpssdir=${HPSSOUT}/1year/rh${year}/$2
      elif [ $3 = "2" ]
      then
           hpssdir=${HPSSOUT}/2year/rh${year}/$2
      else
           hpssdir=${HPSSOUT}/rh${year}/$2
      fi
  else
      hpssdir=${HPSSOUT}/rh${year}/$2
  fi

# Check to see if tar file index already exists

  hsi "ls -l ${hpssdir}/${tarfile}.idx"
  tar_file_exists=$?
  if [ $tar_file_exists -eq 0 ]
  then
    echo "File  $tarfile already saved."
    exit 0
  fi
elif [ $TSM_FLAG = 'YES' ]
then
  if [ $# -eq 3 ]
  then
      if [ $3 = "1" ]
      then
           rhistdir=${TSMOUT}/1year/rh${year}/$2
      elif [ $3 = "2" ]
      then
           rhistdir=${TSMOUT}/2year/rh${year}/$2
      else
           rhistdir=${TSMOUT}/rh${year}/$2
      fi
  else
      rhistdir=${TSMOUT}/rh${year}/$2
  fi
  ssh ibmtsm1.ncep.noaa.gov "mkdir -p -m 755 $rhistdir"
  #
  #   Check if the tarfile already exists.  If it does, assume that
  #   the data for the corresponding directory has already been
  #   tarred and saved.
  #
  size=`ssh ibmtsm1.ncep.noaa.gov ls -l ${rhistdir}/${tarfile} | awk '{print \$5}'`

  if [  -n "$size" ]
  then
    if [ $size -gt 0 ]
    then
       echo "Directory $dir already saved."
       exit 0
    fi
  fi
fi

# If on Stratus:
#   htar is used to create the archive, -P creates
#   the directory path if it does not already exist,
#   and an index file is also made.

if [ $TSM_FLAG = 'NO' ]
then
  date
  htar -P -cvf ${hpssdir}/$tarfile .
  err=$?

  if [ $err -ne 0 ]
  then
    echo "rhist_savedir.sh:  File $tarfile was not successfully created."
    exit 3
  fi
  date

 #
 #   Read the tarfile and save a list of files that are in the tar file.
 #
 
  htar -tvf $hpssdir/$tarfile
  err=$?
  if [ $err -ne 0 ]
  then
    echo "rhist_savedir.sh:  Tar file $tarfile was not successfully read to"
    echo "             generate a list of the files."
    exit 4
  fi
 
#
#  If on Cirrus send to HSM
#
elif [ $TSM_FLAG = 'YES' ]
then
  #
  #   Tar up the directory and put the tarred file in the 
  #   appropriate directory in ${TSMOUT}.
  #  

  date
  gtar -cvf ${DATA}/$tarfile .
  err=$?
  if [ $err -ne 0 ]
  then
    echo "savedir.sh:  Directory $dir was not successfully tarred."
    exit 3
  fi 
  $SCP $SCP_CONFIG ${DATA}/$tarfile ibmtsm1.ncep.noaa.gov:$rhistdir/$tarfile
  date
fi
