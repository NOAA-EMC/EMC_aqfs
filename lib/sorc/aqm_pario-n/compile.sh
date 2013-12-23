#!/bin/ksh

set -ax
if [ ${USER} = 'Jianping.Huang' ] ; then
 mydir=/naqfc/save/${USER}
else
 mydir=
fi
export BASEDIR=${mydir}/nwprod/cmaq.v4.6.0/lib/sorc/aqm_pario-n
make clean
make 
cp ${BASEDIR}/libaqm_pario.a ${mydir}/nwprod/cmaq.v4.6.0/lib


