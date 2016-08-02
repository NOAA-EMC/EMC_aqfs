#!/bin/ksh

set -ax 
if [ ${USER} = 'Jianping.Huang' ] ; then
# mydir=/naqfc/save/${USER}
 mydir=/u/${USER}/noscrub
 
else
 mydir=
fi
envir=dev
model_ver=v5.0.2
export BASEDIR=${mydir}/nw${envir}/cmaq.${model_ver}/lib/sorc/aqm_ioapi_v31
export BIN=Linux3_x86_64intel
export IOAPI_OFFSET_64=YES
make configure
cd ioapi
make clean
make
cp ${BASEDIR}/${BIN}/libioapi.a  ${mydir}/nw${envir}/cmaq.${model_ver}/lib/libaqm_ioapi.a
cp ${BASEDIR}/${BIN}/*mod         ${mydir}/nw${envir}/cmaq.${model_ver}/lib/include

