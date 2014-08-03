#!/bin/ksh

#set -ax
export wrkdir=$(pwd)
echo $wrkdir

for src in ioapi
do
cd $wrkdir/aqm_$src
compile.sh
done

for src in pario-n se-rc4n_2010 
do
cd $wrkdir/aqm_$src
make clean
make
done

for src in filesetapi edss_tools smoke 
do
cd $wrkdir/aqm_$src
make clean
make all
done

