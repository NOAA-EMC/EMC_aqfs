#!/bin/ksh

set -ax 
#if [ ${USER} = 'Jianping.Huang' ] ; then
 mydir=/naqfc/save/${USER}
 envir=para
 model_ver=v4.6.2
#else
# mydir=
#fi
export BASEDIR=${mydir}/nw${envir}/cmaq.${model_ver}/lib/sorc/aqm_ioapi
export BIN=Linux2_x86_64ifort
rm -rf ${BIN}
make nocpl
cp ${BASEDIR}/${BIN}/libioapi.a  ${mydir}/nw${envir}/cmaq.${model_ver}/lib/libaqm_ioapi.a

