#!/bin/ksh

set -ax
if [ ${USER} = 'Jianping.Huang' ] ; then
 mydir=/naqfc/save/${USER}
else
 mydir=
fi
export BASEDIR=${mydir}/nwprod/cmaq.v4.6.0/lib/sorc/aqm_se-rc4n_2010
make clean
make 
cp ${BASEDIR}/libaqm_sef90_2010.a ${mydir}/nwprod/cmaq.v4.6.0/lib/


