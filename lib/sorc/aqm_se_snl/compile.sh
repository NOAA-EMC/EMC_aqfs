#!/bin/ksh
set -ax
make clean
make 
cp -rp *mod ../../include
