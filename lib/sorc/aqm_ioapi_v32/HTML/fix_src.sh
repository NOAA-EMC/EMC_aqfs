#!/bin/sh
#.........................................................................
# Version "@(#)$Header$"
# EDSS/Models-3 I/O API.  Copyright (C) 2002 MCNC
# Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Script to take I/O API-style Fortran source that is compatible with both 
#  standard F77/F90 fixed source form and F90 free source form, and produce
#  a file compatible with F77/F90 extended-column fixed source form
#.........................................................................
#  USAGE
#       fix_src.csh <input-file> <output-file>
#.........................................................................
#  IBM NOTE
#       IBM's "csh" misbehaves, so you may need to substitute "tcsh"
#       as indicated above
#
# May 24 2021 Ho-Chun Huang  Create fix_src.sh from fix_src.csh for EMC CMAQ implementation
#.........................................................................

if [ $# -ne 2 ]; then
   echo "Usage:  fix_src.sh <input-file> <output-file>"
   exit
fi

sed -e 's/ *&$//' < $1 > $2

foo=$?
if [ ${foo} -ne 0 ];  then
   echo "ERROR ${foo} in script fix_src.sh"
else
   echo "Successfully finished in script fix_src.sh"
fi
echo " "
echo " "
exit

