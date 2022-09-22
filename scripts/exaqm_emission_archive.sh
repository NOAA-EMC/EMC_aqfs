####################################################################
echo "------------------------------------------------"
echo "JAQM_EMISSION_ARCHIVE - " 
echo "------------------------------------------------"
echo "History: MAY 2015 - First implementation of this new script."

#####################################################################

cd $DATA

#####################################################################
#
# RUNHISTORY JOB
#
# This job uses script aqm_rhist_savedir.sh to tar up and save a specified
# operational directory in the appropriate directory under /hsmprod
# on the HPSS server, ncos70a.
#
#####################################################################

########################################
set -x
msg="JOB $job HAS BEGUN"
postmsg "$msg"
##########################################

# Run setpdy and initialize PDY variables
setpdy.sh
. PDY
errsum=0

# obtain the next month
  next_month=`date +'%m' -d 'next month'`
  echo " the next month= $next_month"
 
# obtain the next year
  next_year=`date +%Y -d 'next year'`
  echo " the next year= $next_year"
 
# obtain the current year
  current_year=`date +%Y`
  echo " the current year= $current_year"
 
  prevmon=`date -d "$PDY -1 month" +%Y%m`
##############################################################
# archive monthly emission files
##############################################################
   if (( $next_month == 01 ))
   then
       tar_dir=${next_year}${next_month}
   else
       tar_dir=${current_year}${next_month}
   fi

   $USHrhist/aqm_rhist_savedir.sh ${INPEMIDIR}/${tar_dir} ${tar_dir} perm
   $USHrhist/aqm_rhist_savedir.sh ${INPEMIDIRhps}/${tar_dir} ${tar_dir} perm
   $USHrhist/aqm_rhist_savedir.sh /gpfs/hps/nco/ops/com/aqm/prod/bcdata.$prevmon $prevmon 5
   export err=$?;let errsum=errsum+err;$USHrhist/aqm_rhist_errchk.sh monthly

##############################################################
# archive yearly emission files
##############################################################
### TEST AREA ######
#  next_month=01
#  echo " TEST the next month= $next_month"
#  next_year=2015
#  echo " TEST the next year= $next_year"
### TEST AREA ######

   if (( $next_month == 01 ))
   then
       tar_dir=${next_year}
       $USHrhist/aqm_rhist_savedir.sh ${INPEMIDIR}/${tar_dir} ${tar_dir} perm
       $USHrhist/aqm_rhist_savedir.sh ${INPEMIDIRhps}/${tar_dir} ${tar_dir} perm
       export err=$?;let errsum=errsum+err;$USHrhist/aqm_rhist_errchk.sh yearly
   else
       echo "Yearly emission is not the right time to HPSS archive"
   fi

##############################################################
# archive yearly jtable files
##############################################################
   if (( $next_month == 01 ))
   then
       tar_dir=${next_year}
       $USHrhist/aqm_rhist_savedir.sh ${INPEMIDIR}/cb05_Jtable ${tar_dir} perm
       export err=$?;let errsum=errsum+err;$USHrhist/aqm_rhist_errchk.sh jtable
   else
       echo "Yearly jtable is not the right time to HPSS archive"
   fi

#####################################################################

# GOOD RUN
set +x
echo "**************JOB RHIST COMPLETED NORMALLY ON THE IBM"
echo "**************JOB RHIST COMPLETED NORMALLY ON THE IBM"
echo "**************JOB RHIST COMPLETED NORMALLY ON THE IBM"
set -x

msg="JOB $job HAS COMPLETED NORMALLY."
echo $msg
postmsg "$msg"

############## END OF SCRIPT #######################
