#!/bin/sh

template=$1
infile=$2
outfile=$3

echo "==========================="
echo " Input template    = ${template}"
echo " Input input file  = ${infile}"
echo " Input output file = ${outfile}"
echo "==========================="

export mysdate=-99
export mystime=-99
export mynsteps=-99
export layer=-1

export INFILE1=${infile}
export OUTFILE="${outfile}.s -v"
export ncf=Y

${EXECaqm}/aqm_setup_date_timesp  

Template=${template}
HeadDefn="./header_defn"
SpecDefn="./species_defn"

cat ${HeadDefn} ${Template} > ${SpecDefn}

export SPECIES_DEF=${SpecDefn}

## turn off excessive WRITE3 logging
export IOAPI_LOG_WRITE N

${EXECaqm}/aqm_bicor_combine

rm ${HeadDefn} ${SpecDefn}

exit
