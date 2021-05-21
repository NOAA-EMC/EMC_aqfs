#!/bin/sh

SPCLIST=$1
infile=$2
outfile=$3

echo "==========================="
echo " Input spc_list    = ${SPCLIST}"
echo " Input input file  = ${infile}"
echo " Input output file = ${outfile}"
echo "==========================="
#set default values
export mysdate=-99
export mystime=-99
export mynsteps=-99
export mytsteps=-99
export mysrec=-99
export myerec=-99
export myslay=-1
export myelay=-1
export myscol=-99
export myecol=-99
export mysrow=-99
export myerow=-99
export spc=N
export display=N
export output=N
export tol=0
export info=N
export vp=N
export window=N
export delete=Y
export timeind=N

if  [ ! -s ${infile} ]; then
   echo " "
   echo " File ${infile} does not exist"
   echo " "
   exit
fi

if [ -s  ${outfile} ]; then
   if [ "${delete}" == "Y" ]; then
      rm -f ${outfile}
   else
      echo " "
      echo " Output file ${outfile} exists. Please remove it or use -rm option."
      echo " "
      exit
   fi
fi

export spc=Y
export spc_list=${SPCLIST}
export ncf=Y
export file1=${infile}
export file2=${outfile}

ls -al ${infile}

echo " "

${EXECaqm}/aqm_subset

exit
