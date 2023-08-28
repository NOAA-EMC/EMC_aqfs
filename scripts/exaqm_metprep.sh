#!/bin/ksh -x 
#####################################################################
#  UNIX Script Documentation Block
#  Script Name: exaqm_prep_cs.sh.ecf
# 
#  Abstract: Parallel Meteorological pre-processor for CMAQ. 
# 
# 
# Scripts History log:
#   2020-05-05   Youhua Tang, initial version
#
#####################################################################

msg="JOB $job HAS BEGUN"
postmsg "$msg"

export pgm=aqm_metprep
if [ ${RUN} == "hi" ]; then
   NCOLS=80
   NROWS=52
   PROJPARM=2.,19.,21.,-157.5,-157.5,20.53
   DOMAINS=-480000.,-312000.,12000.,12000.,80,52
   grd_suffix=${RUN}
   GRDNAM='AQF_hi'
elif [ ${RUN} == "ak" ]; then
   NCOLS=199
   NROWS=163
   PROJPARM=2.,57.,63.,-148.6,-148.6,63.21
   DOMAINS=-1194000.,-978000.,12000.,12000.,199,163
   grd_suffix=${RUN}
   GRDNAM='AQF_ak'
elif [ ${RUN} == "cs" ]; then
   NCOLS=442
   NROWS=265
   PROJPARM=2.,33.,45.,-97.,-97.,40
   DOMAINS=-2508000.,-1716000.,12000.,12000.,442,265
   grd_suffix='05'
   GRDNAM='AQF_cs'
else
   echo "unknown domain ${RUN}"
   exit 1
fi

cd $DATA

# setpdy.sh in ~/jobs/JAQM_PREP_CS should define from value from PDYm7 to PDYp7
case $cyc in 
 00) export endfhr=06
     export endday=${PDY};;
 06) export endfhr=72
     export endday=${PDYp3};;
 12) export endfhr=72
     export endday=${PDYp3};;
 18) export endfhr=06
     export endday=${PDYp1};;
esac

let "endcyc=cyc+endfhr"
while [ ${endcyc} -ge 24 ]; do
   let "endcyc=endcyc-24"
done
typeset -Z2 endcyc
echo ${endcyc} 

let NTIMES=${endfhr}+1
let penodes=${NTIMES}/8
if [ ${NTIMES}%8 -ge 1 ]; then
   let penodes=${penodes}+1
fi

SYYYY=`echo ${PDY} |cut -c1-4`
SMM=`echo ${PDY} |cut -c5-6`
SDD=`echo ${PDY} |cut -c7-8`
EYYYY=`echo ${endday} |cut -c1-4`
EMM=`echo ${endday} |cut -c5-6`
EDD=`echo ${endday} |cut -c7-8`

if [ ! -d ${COMINgfs} ]; then
   echo "*****FATAL ERROR***** - COULD NOT LOCATE:${COMINgfs}"
else
   echo "DIAG : meterology files ingested from ${COMINgfs}"
fi

###
### Check hourly GFS output for both atm and sfc files
###
metcyc=`echo ${cycle} | cut -c2-3`
icnt=0
let tend=${endfhr}
while [ ${icnt} -le ${tend} ]; do
   fhr=`printf %3.3d ${icnt}`
   ##
   ## To assure the gfs.t${cyc}z.sfcf${endfhr}.nc and gfs.t${cyc}z.atmf${endfhr}.nc are ready to be used, but not merely existed
   ##
## if [ 1 == 2 ] ; then
   targte_file=${COMINgfs}/gfs.${cycle}.logf${fhr}.txt
   icnt_gfs=0
   while [ ${icnt_gfs} -lt 300 ]; do
      if [ -s ${targte_file} ]; then
         echo "Found file : ${targte_file}"
         break
      else
         echo "===== Waiting for file : ${targte_file}"
         ((icnt_gfs++))
         sleep 10
      fi
   done
   if [ ${icnt_gfs} -ge 180 ]; then
      err_exit "*****FATAL ERROR***** - COULD NOT LOCATE:${COMINgfs}/gfs.${cycle}.logf${fhr}.txt"
   fi
   
## fi

   icnt_gfs=0
   while [ ${icnt_gfs} -lt 300 ]; do
      if [ -s ${COMINgfs}/gfs.${cycle}.atmf${fhr}.nc ]; then
         ln -s ${COMINgfs}/gfs.${cycle}.atmf${fhr}.nc gfs.${cycle}.atmf${fhr}.nc
         echo "Found file : ${COMINgfs}/gfs.${cycle}.atmf${fhr}.nc"
         break
      else
         ((icnt_gfs++))
         sleep 10
      fi 
      if [ ${icnt_gfs} -ge 180 ]; then
         err_exit "*****FATAL ERROR***** - COULD NOT LOCATE:${COMINgfs}/gfs.${cycle}.atmf${fhr}.nc"
      fi
   done

   icnt_gfs=0
   while [ ${icnt_gfs} -lt 300 ]; do
      if [ -s ${COMINgfs}/gfs.${cycle}.sfcf${fhr}.nc ]; then
         ln -s ${COMINgfs}/gfs.${cycle}.sfcf${fhr}.nc gfs.${cycle}.sfcf${fhr}.nc
         echo "FOUND file : ${COMINgfs}/gfs.${cycle}.sfcf${fhr}.nc"
         break
      else
         ((icnt_gfs++))
         sleep 10
      fi 
      if [ ${icnt_gfs} -ge 180 ]; then
         err_exit "*****FATAL ERROR***** - COULD NOT LOCATE:${COMINgfs}/gfs.${cycle}.sfcf${fhr}.nc"
      fi
   done
   ((icnt++))
done

if [ ! -d ${COMOUT} ]; then mkdir -p ${COMOUT}; fi

#
# Searching for latest GVF file with 2 days back time latency in NCO $DCOMROOT
#
echo "DIAG : Greenness Fraction ingested from ${COMINgvf}"
viirs_sat=$(echo ${GVF_FHR} | awk -F'_' '{print $3}' )
echo "Using VIIRS GVF product from satellite ${viirs_sat}"
cdate=${PDY}${cyc}
PDYm10=$(${NDATE} -240 ${cdate} | cut -c1-8)
FIRSTDAY=${PDYm2}
LASTDAY=${PDYm10}
NOW=${FIRSTDAY}
if [ -s tlist ]; then /bin/rm -f tlist; fi
while [ ${NOW} -ge ${LASTDAY} ]; do
   ls ${COMINgvf}/${GVF_FHR}*_e${NOW}* > tlist      ## error message is expected if no file can be found
   if [ -s tlist ]; then break; fi
   cdate=${NOW}${cyc}
   NOW=$(${NDATE} -24 ${cdate} | cut -c1-8 )
done
grib2_gvf=`tail tlist`
if [ "${grib2_gvf}" == "" ]; then                   ## if no GVF files found for JPSS-1, then try NPP
   NOW=${FIRSTDAY}
   if [ -s tlist ]; then /bin/rm -f tlist; fi
   while [ ${NOW} -ge ${LASTDAY} ]; do
      ls ${COMINgvf}/${GVF_FHR_2}*_e${NOW}* > tlist      ## error message is expected if no file can be found
      if [ -s tlist ]; then break; fi
      cdate=${NOW}${cyc}
      NOW=$(${NDATE} -24 ${cdate} | cut -c1-8 )
   done
   grib2_gvf=`tail tlist`
   if [ "${grib2_gvf}" == "" ]; then
      echo "ERROR WARNING - Can not find any VIIRS Green Vegetation fraction file (j01 or npp) over last 10 day in ${COMINgvf}"
   fi
fi

if [ "${grib2_gvf}" != "" ] && [ -s ${grib2_gvf} ]; then         ## sanitary check
   echo "Found GVF input file - ${grib2_gvf}"

   ln -s ${grib2_gvf} DCOM_GVF_FILE    ## link DCOM file to $DATA local filename

   nacc_gvf_prefix="nacc_gvf"
   NACC_GVF_IN=${nacc_gvf_prefix}.nc
    
   ${USHaqm}/viirsgrib2nc4.py -v -f DCOM_GVF_FILE -o ${nacc_gvf_prefix} -x ${WGRIB2}

   if [ -s ${NACC_GVF_IN} ]; then
      echo "Found CMAQ GVF input file - ${NACC_GVF_IN}"
   else
      echo "ERROR WARNING *** Can not find ${NACC_GVF_IN} after using ${grib2_gvf}"
      NACC_GVF_IN="no_file"
   fi
else
   echo "ERROR WARNING - GFS encoded Green Vegetation Fraction file will be used"
   NACC_GVF_IN="no_file"
fi

GEO_NC=${COMINemi}/gfs.geo.${SMM}.nc
if [ -s ${GEO_NC} ]; then
   GEO_MONTHLY=${GEO_NC}
else
   ## 2021 indicates the year that static files being updated, need to updated year information
   ## in next implementation process if it WILL BE replaced
   GEO_MONTHLY=${FIXaqm}/geo_static_${SMM}_2021.nc
   echo "ERROR WARNING - Can not find ${GEO_NC}. Static file ${GEO_MONTHLY} is used"
fi
#
cat>namelist.mcip<<!
&FILENAMES
  file_gd    = 'GRIDDESC'
  file_mm    = './gfs.${cycle}.atmf','.nc'
  file_sfc   = './gfs.${cycle}.sfcf','.nc'
  file_geo   = '${GEO_MONTHLY}'
  file_viirs_gvf = '${NACC_GVF_IN}'
  file_viirs_lai = 'no_file'
  ioform     =  1
 &END

 &USERDEFS
  inmetmodel =  3
  dx_in      =  12000
  dy_in      =  12000
  met_cen_lat_in =  0.0
  met_cen_lon_in =  0.0
  lpv        =  0
  lwout      =  1
  luvbout    =  1
  ifdiag_pbl = .FALSE.
  ifviirs_gvf = .TRUE.
  ifviirs_lai = .FALSE.
  iffengsha_dust = .TRUE.
  ifbioseason = .TRUE.
  mcip_start = '${SYYYY}-${SMM}-${SDD}-${cyc}:00:00.0000'
  mcip_end   = '${EYYYY}-${EMM}-${EDD}-${endcyc}:00:00.0000'
  intvl      =  60
  coordnam   = 'AQF_RPO'
  grdnam     = '${GRDNAM}'
  ctmlays    =  1.000000, 0.995253, 0.990479, 0.985679, 0.980781,
              0.975782, 0.970684, 0.960187, 0.954689, 0.936895, 
              0.930397, 0.908404, 0.888811, 0.862914, 0.829314, 
              0.786714, 0.735314, 0.645814, 0.614214, 0.582114, 
              0.549714, 0.511711, 0.484394, 0.451894, 0.419694, 
              0.388094, 0.356994, 0.326694, 0.297694, 0.270694, 
              0.245894, 0.223694, 0.203594, 0.154394, 0.127094, 0.000000
  btrim      =  -1
  lprt_col   =  0
  lprt_row   =  0
  ntimes     = ${NTIMES}
  wrf_lc_ref_lat = 40.0
  projparm = ${PROJPARM}
  domains = ${DOMAINS}
 &END

 &WINDOWDEFS
  x0         =  1
  y0         =  1
  ncolsin    =  ${NCOLS}
  nrowsin    =  ${NROWS}
 &END
!

export IOAPI_CHECK_HEADERS=T

export GRID_BDY_2D=aqm.${cycle}.grdbdy2d.ncf
export GRID_CRO_2D=aqm.${cycle}.grdcro2d.ncf
export GRID_DOT_2D=aqm.${cycle}.grddot2d.ncf
export MET_BDY_3D=aqm.${cycle}.metbdy3d.ncf
export MET_CRO_2D=aqm.${cycle}.metcro2d.ncf
export MET_CRO_3D=aqm.${cycle}.metcro3d.ncf
export MET_DOT_3D=aqm.${cycle}.metdot3d.ncf
export LUFRAC_CRO=aqm.${cycle}.lufraccro.ncf
export SOI_CRO=aqm.${cycle}.soicro.ncf
export MOSAIC_CRO=aqm.${cycle}.mosaiccro.ncf

startmsg
mpiexec -n ${NTIMES} --cpu-bind core ${EXECaqm}/aqm_nacc
export err=$?
if [ $err -ne 0 ]; then
   err_chk
else
   msg="${pgm} completed normally"
   postmsg "${msg}"
fi

if [ "${SENDCOM}" = 'YES' ]; then
  mv GRIDDESC  ${COMOUT}/aqm_griddesc${grd_suffix}
  mv *.ncf ${COMOUT}
fi  
