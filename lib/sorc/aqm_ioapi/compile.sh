#!/bin/ksh

set -ax 
if [ ${USER} = 'Jianping.Huang' ] ; then
 mydir=/naqfc/save/${USER}
else
 mydir=
fi
# envir=dev
# model_ver=v4.6.9
export BASEDIR=${mydir}/nw${envir}/cmaq.${model_ver}/lib/sorc/aqm_ioapi
export BIN=Linux2_x86_64ifort
rm -rf ${BIN}
make nocpl
cp ${BASEDIR}/${BIN}/libioapi.a  ${mydir}/nw${envir}/cmaq.${model_ver}/lib/libaqm_ioapi.a

