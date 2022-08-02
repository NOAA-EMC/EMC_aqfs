#!/bin/ksh
######################################################################
#  UNIX Script Documentation Block
#                      .
# Script name:         exaqm_fcst.sh
# Script description:  Run CMAQ model:Community Multiscale Air Quality
#		       chemical transport model.
#
#  #Input Environment Variables:
#    NPCOL_NPROW      	# domain decompositon (#division_Col;Row)
#    nprocs	      	# number of processors
#    GRIDDESC		# horizontal grid definitions
#    GRID_NAME		# grid name
#    AVG_CONC_SPCS	# Species for integral average conc
#    ACONC_BLEV_ELEV	# layer range for integral average conc
#    IOAPI_LOG_WRITE=F	# excessive WEITE3 logging
#                         default to F (off)
#
#   Change Logs:
#
#   05/06/2020   Youhua Tang    refresh it for CMAQ 5.3.1
#   05/25/2020   Ho-Chun Huang  revision for operational 4-cycle format
#   06/02/2020   Ho-Chun Huang  replace date2julian with date2jday.sh [JDAY|yymmdd] 
#                               in /gpfs/hps/nco/ops/nwprod/prod_util.v#.#.#/ush
#                               NEED module load prod_util in top ecf scripts
#   06/03/2020   Ho-Chun Huang  Add dynamic node and processor setting
#   09/04/2020   Ho-Chun Huang  Bugfix for the timestep that output aqm.t06z.soil06_r.ncf,
#                               default is output soil profile at fcst hours 6 for the initial
#                               start of next cycle. But for analysis run, the soil profile needs
#                               to be output at the end of analysis run, i.e., at hours 24.  The
#                               time stamp match with t06z fcst cycle time info.  Define SOILOUT_EDATE
#                               and SOILOUT_ETIME to overwrite default.  Same fix has been applied
#                               to t12z cycle.
#   12/17/2020   Ho-Chun Huang  Merge ARL code delivery for dust and fire emissions bug fix, new plume
#                               choice "Sofiev plume rise", BI-direction NH3, BIOSEASON switch, and
#                               HI/AK unified code structure.
#####################################################################
set -xa

msg="JOB ${job} HAS BEGUN"
postmsg   "${msg}"

export pgm=aqm_cmaq_ak

cd ${DATA}

APPL=v531
EXEC=aqm_cmaq_${APPL}
CFG=CCTM_${APPL}
MECH=cb6r3_ae7_aq
RUN9=AK

if [ "${FCST}" == "YES" ]; then

## Note soil_file has a time-stamp that needs to match with IC time.  No alternative soil file can be used except for 06Z run
case ${cyc} in
 00) restart_file1=${COMINm1}/aqm.t18z.cgrid.ncf
     restart_log1=${COMINm1}/aqm.t18z.log
     restart_file2=${COMINm1}/aqm.t12z.cgrid.ncf
     restart_log2=${COMINm1}/aqm.t12z.log
     restart_file3=${COMINm1}/aqm.t06z.cgrid.ncf
     restart_log3=${COMINm1}/aqm.t06z.log
     restart_file4=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log4=${COMINm2}/aqm.t12z.log
     restart_file5=${COMINm2}/aqm.t06z.cgrid.ncf
     restart_log5=${COMINm2}/aqm.t06z.log
     soil_file=${COMINm1}/aqm.t18z.soil06.ncf
     NSTEPS=060000;;
 06) restart_file1=${COMINm1}/aqm.t06z.cgrid_r.ncf
     restart_log1=${COMINm1}/aqm.t06z.log
     restart_file2=${COMIN}/aqm.t00z.cgrid.ncf
     restart_log2=${COMIN}/aqm.t00z.log
     restart_file3=${COMINm1}/aqm.t12z.cgrid.ncf
     restart_log3=${COMINm1}/aqm.t12z.log
     restart_file4=${COMINm1}/aqm.t06z.cgrid.ncf
     restart_log4=${COMINm1}/aqm.t06z.log
     restart_file5=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log5=${COMINm2}/aqm.t12z.log
     soil_file=${COMIN}/aqm.t00z.soil06.ncf
     if [ ! -s ${soil_file} ] && [ -s ${COMINm1}/aqm.t06z.soil06_r.ncf ]; then
        soil_file=${COMINm1}/aqm.t06z.soil06_r.ncf
     fi
     NSTEPS=720000;;
 12) restart_file1=${COMINm1}/aqm.t12z.cgrid_r.ncf
     restart_log1=${COMINm1}/aqm.t12z.log
     restart_file2=${COMIN}/aqm.t06z.cgrid.ncf
     restart_log2=${COMIN}/aqm.t06z.log
     restart_file3=${COMINm1}/aqm.t12z.cgrid.ncf
     restart_log3=${COMINm1}/aqm.t12z.log
     restart_file4=${COMINm1}/aqm.t06z.cgrid.ncf
     restart_log4=${COMINm1}/aqm.t06z.log
     restart_file5=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log5=${COMINm2}/aqm.t12z.log
     soil_file=${COMIN}/aqm.t06z.soil06.ncf            # Using latest meteorology produced soil file, i.e. t06z
     if [ ! -s ${soil_file} ] && [ -s ${COMINm1}/aqm.t12z.soil06_r.ncf ]; then
        soil_file=${COMINm1}/aqm.t12z.soil06_r.ncf
     fi
     if [ "${SINGLECYC}" == "YES" ]; then 
        soil_file=${COMINm1}/aqm.t12z.soil24.ncf 
        JDAYp1=`date2jday.sh ${PDYp1}`
        export SOILOUT_EDATE=${JDAYp1}
        export SOILOUT_ETIME=${cyc}"0000"
        ## For single cycle retro run 
        ## The 24 hours analysis run + 72 hour forecast run needs
        ## to use day-1 fire emission for both analysis and forecast
        ## runs.  Thus, restrat_file should be aqm.t12z.cgrid_r.ncf
        ## Current ARL retro runs seem to use current day fire emission
        ## for daily 24 hours forecast run.  This is the best-case scenario
        ## and should be used as a reference for comparison.  The
        ## performance of ARL runs may not respentative for near-real-time forecasting.
        restart_file1=${COMINm1}/aqm.t12z.cgrid.ncf
        restart_file2=${COMINm1}/aqm.t12z.cgrid.ncf
        restart_file3=${COMINm1}/aqm.t12z.cgrid.ncf
        restart_file4=${COMINm1}/aqm.t12z.cgrid.ncf
        restart_file5=${COMINm1}/aqm.t12z.cgrid.ncf
        restart_log1=${COMINm1}/aqm.t12z.log
        restart_log2=${COMINm1}/aqm.t12z.log
        restart_log3=${COMINm1}/aqm.t12z.log
        restart_log4=${COMINm1}/aqm.t12z.log
        restart_log5=${COMINm1}/aqm.t12z.log
     fi
     NSTEPS=720000;;
 18) restart_file1=${COMIN}/aqm.t12z.cgrid.ncf
     restart_log1=${COMIN}/aqm.t12z.log
     restart_file2=${COMIN}/aqm.t06z.cgrid.ncf
     restart_log2=${COMIN}/aqm.t06z.log
     restart_file3=${COMINm1}/aqm.t12z.cgrid.ncf
     restart_log3=${COMINm1}/aqm.t12z.log
     restart_file4=${COMINm1}/aqm.t06z.cgrid.ncf
     restart_log4=${COMINm1}/aqm.t06z.log
     restart_file5=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log5=${COMINm2}/aqm.t12z.log
     soil_file=${COMIN}/aqm.t12z.soil06.ncf
     NSTEPS=060000;;
esac

else

#######################################################
## below is prepared for 24-hr backward 
##  run for using late smoke emissions for previous day
#######################################################

JDAY=`date2jday.sh ${PDY}`

## default is run for 6 hour then output soil file for next run in 4cycle setting
## overwrite default to lenth of run time to be at the start of next 06z forecast run,
## i.e., timestmp at JDAY and ${cyc}0000 but not JDAY-1 at (${cyc}+6)"0000"
export SOILOUT_EDATE=${JDAY}
export SOILOUT_ETIME=${cyc}"0000"
case ${cyc} in
 06) restart_file1=${COMINm1}/aqm.t00z.cgrid.ncf
     restart_log1=${COMINm1}/aqm.t00z.log
     restart_file2=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log2=${COMINm2}/aqm.t12z.log
     restart_file3=${COMINm2}/aqm.t06z.cgrid.ncf
     restart_log3=${COMINm2}/aqm.t06z.log
     restart_file4=${COMINm3}/aqm.t12z.cgrid.ncf
     restart_log4=${COMINm3}/aqm.t12z.log
     soil_file=${COMINm1}/aqm.t00z.soil06.ncf     
     if [ ! -s ${soil_file} ] && [ -s ${COMINm2}/aqm.t06z.soil06_r.ncf ]; then
        soil_file=${COMINm2}/aqm.t06z.soil06_r.ncf
     fi
     NSTEPS=240000;;
 12) restart_file1=${COMINm1}/aqm.t06z.cgrid.ncf
     restart_log1=${COMINm1}/aqm.t06z.log
     restart_file2=${COMINm2}/aqm.t12z.cgrid.ncf
     restart_log2=${COMINm2}/aqm.t12z.log
     restart_file3=${COMINm2}/aqm.t12z.cgrid_r.ncf
     restart_log3=${COMINm2}/aqm.t06z.log
     restart_file4=${COMINm3}/aqm.t12z.cgrid.ncf
     restart_log4=${COMINm3}/aqm.t12z.log
     if [ "${SINGLECYC}" == "YES" ]; then 
        restart_file1=${COMINm2}/aqm.t12z.cgrid_r.ncf
        restart_file2=${COMINm2}/aqm.t12z.cgrid_r.ncf
        restart_file3=${COMINm2}/aqm.t12z.cgrid_r.ncf
        restart_file4=${COMINm2}/aqm.t12z.cgrid_r.ncf
        restart_file5=${COMINm2}/aqm.t12z.cgrid_r.ncf
        restart_log1=${COMINm2}/aqm.t12z.log
        restart_log2=${COMINm2}/aqm.t12z.log
        restart_log3=${COMINm2}/aqm.t12z.log
        restart_log4=${COMINm2}/aqm.t12z.log
        restart_log5=${COMINm2}/aqm.t12z.log
        soil_file=${COMINm2}/aqm.t12z.soil24_r.ncf   # ${COMINm1}/aqm.t06z.soil06.ncf
        export SOILOUT_EDATE=${JDAY}
        export SOILOUT_ETIME=${cyc}"0000"
     else
        soil_file=${COMINm1}/aqm.t06z.soil06.ncf
        if [ ! -s ${soil_file} ] && [ -s ${COMINm2}/aqm.t12z.soil06_r.ncf ]; then
           soil_file=${COMINm2}/aqm.t12z.soil06_r.ncf
        fi
     fi
     NSTEPS=240000;;
esac

fi  ## END IF ${FCST} loop

if [ -s "${restart_file1}" ]; then
   restart_file=${restart_file1}
   restart_log=${restart_log1}
elif [ -s "${restart_file2}" ]; then
   restart_file=${restart_file2}
   restart_log=${restart_log2}
elif [ -s "${restart_file3}" ]; then
   restart_file=${restart_file3}
   restart_log=${restart_log3}
elif [ -s "${restart_file4}" ]; then
   restart_file=${restart_file4}
   restart_log=${restart_log4}
elif [ -s "${restart_file5}" ]; then
   restart_file=${restart_file5}
   restart_log=${restart_log5}
fi

if [ -s "${restart_file}" ]; then
   export START=WARM
   export NEW_START=N
   ln -sf ${restart_file}   $DATA/CONC.${APPL}.ncf
   export ICFILE=$DATA/CONC.${APPL}.ncf
else
   export START=COLD
   export NEW_START=Y
   export ICFILE=${FIXaqm}/aqm.${RUN}12z.init.cgrid_2017.ncf
fi
echo $START

if [ "${FCST}" == "YES" ]; then
   STDATE=`date2jday.sh ${PDY}`
else
   STDATE=`date2jday.sh ${PDYm1}`
fi

STTIME=${cyc}"0000"
TSTEP=010000

export GRIDDESC=${PARMaqm}/aqm_griddesc${RUN}
export GRID_NAME=AQF_${RUN}

export LOGFILE=${DATA}/${APPL}.log

SPCS_1="ASO4I ANO3I ANH4I ANAI ACLI AECI ALVPO1I ASVPO1I ASVPO2I APOCI APNCOMI"
SPCS_2="ALVOO1I ALVOO2I ASVOO1I ASVOO2I AOTHRI ASO4J ANO3J ANH4J ANAJ ACLJ AECJ"
SPCS_3="ALVPO1J ASVPO1J ASVPO2J ASVPO3J APOCJ APNCOMJ ALVOO1J ALVOO2J ASVOO1J ASVOO2J"
SPCS_4="ASVOO3J AOTHRJ AIVPO1J AISO1J AISO2J AISO3J AMT1J AMT2J AMT3J AMT4J AMT5J"
SPCS_5="AMT6J AMTNO3J AMTHYDJ AGLYJ ASQTJ AORGCJ AOLGBJ AOLGAJ APCSOJ AAVB1J AAVB2J"
SPCS_6="AAVB3J AAVB4J AFEJ ASIJ ATIJ ACAJ AMGJ AMNJ AALJ AKJ ASO4K ANO3K ANH4K ACLK"
SPCS_7="ASOIL ACORS ASEACAT O3 NO NO2 NO3 N2O5 HONO HNO3 PNA CRON CLNO2"
SPCS_8s="CLNO3 PAN PANX OPAN NTR1 NTR2 INTR"
SPCS_8="CLNO3 PAN PANX OPAN NTR1 NTR2 INTR PAR ETHA PRPA MEOH ETH ETOH OLE ACET TOL"
SPCS_9="XYLMN BENZENE FORM GLY KET ETHY ALD2 IOLE ALDX ISOP TERP NAPH APIN"
export CONC_SPCS="O3"
# export CONC_SPCS="ALL"
# export CONC_BLEV_ELEV=" 1 1" #> CONC file layer range; comment to write all layers to CONC
# export AVG_CONC_SPCS="ALL"
export AVG_CONC_SPCS="${SPCS_1} ${SPCS_2} ${SPCS_3} ${SPCS_4} ${SPCS_5} ${SPCS_6} ${SPCS_7} ${SPCS_8} ${SPCS_9}"
export ACONC_BLEV_ELEV=" 1 1" #> ACONC file layer range; comment to write all layers to ACONC
export AVG_FILE_ENDTIME=Y     #> override default beginning ACONC timestamp [ default: N ]

export CTM_MAXSYNC=720       #> max sync time step (sec) [default: 720]
export CTM_MINSYNC=60       #> min sync time step (sec) [default: 60]
export CTM_CKSUM=Y           #> write cksum report [ default: Y ]
export CLD_DIAG=N            #> write cloud diagnostic file [ default: N ]
export CTM_AERDIAG=Y #N         #> aerosol diagnostic file [ default: N ]
export CTM_PHOTDIAG=Y        #> photolysis diagnostic file [ default: N ]
export CTM_SSEMDIAG=N        #> sea-salt emissions diagnostic file [ default: N ]
export CTM_WB_DUST=Y #Y         #> use NAQFC, NACC-CMAQ Fengsha inline windblown dust emissions [ default: Y ]
export CTM_ERODE_AGLAND=N    #> use agricultural activity for windblown dust [ default: N ]; ignore if CTM_WB_DUST = N
## export CTM_DUSTEM_DIAG=N     #> windblown dust emissions diagnostic file [ default: N ]; ignore if CTM_WB_DUST = N
export CTM_DUSTEM_DIAG=Y     #> windblown dust emissions diagnostic file [ default: N ]; ignore if CTM_WB_DUST = N
export CTM_LTNG_NO=N #Y         #> turn on lightning NOx [ default: N ]
## Turn off WVEL output in CONC and ACONC
export CTM_WVEL=N            #> save derived vertical velocity component to conc file [ default: N ]
export KZMIN=Y               #> use Min Kz option in edyintb [ default: Y ], otherwise revert to Kz0UT
export CTM_ILDEPV=Y          #> calculate in-line deposition velocities [ default: Y ]
export CTM_MOSAIC=N          #> landuse specific deposition velocities [ default: N ]
export CTM_ABFLUX=N          #> Ammonia bi-directional flux for in-line deposition velocities [ default: N ]; ignore if CTM_ILDEPV = N
export CTM_HGBIDI=N          #> Mercury bi-directional flux for in-line deposition velocities [ default: N ]; ignore if CTM_ILDEPV = N
export CTM_SFC_HONO=Y        #> Surface HONO interaction [ default: Y ]; ignore if CTM_ILDEPV = N
export CTM_DEPV_FILE=Y #N       #> write diagnostic file for deposition velocities [ default: N ]
export CTM_BIOGEMIS=Y        #> calculate in-line biogenic emissions [ default: N ]
export B3GTS_DIAG=Y          #> write biogenic mass emissions diagnostic file [ default: N ]; ignore if CTM_BIOGEMIS = N
export CTM_PT3DEMIS=Y        #> calculate in-line plume rise for elevated point emissions [ default: N ]
## Turn off 3D point source emission output (including fire)
export PT3DDIAG=N            #> optional 3d point source emissions diagnostic file [ default: N]; ignore if CTM_PT3DEMIS = N
export PT3DFRAC=N            #> optional layer fractions diagnostic (play) file(s) [ default: N]; ignore if CTM_PT3DEMIS = N
export IOAPI_LOG_WRITE=F     #> turn on excess WRITE3 logging [ options: T | F ]
export FL_ERR_STOP=N         #> stop on inconsistent input files
export PROMPTFLAG=F          #> turn on I/O-API PROMPT*FILE interactive mode [ options: T | F ]
export IOAPI_OFFSET_64=YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]

#Additional CMAQv5.3.1 options--------------------------------------------------------------------------------------------
export PX_VERSION=N          #> PX LSM
export CLM_VERSION=N         #> CLM LSM
export NOAH_VERSION=Y        #> NOAH LSM
export SIGMA_SYNC_TOP=0.7    #> top sigma level thru which sync step determined [ default: 0.7 ]
#export ADV_HDIV_LIM=0.95    #> maximum horiz. div. limit for adv step adjust [ default: 0.9 ]
export CTM_ADV_CFL=0.95      #> max CFL [ default: 0.75]
#export RB_ATOL=1.0E-09      #> global ROS3 solver absolute tolerance [ default: 1.0E-07 ]

#Science Options
export CTM_OCEAN_CHEM=Y      #> Flag for ocean halogen chemistry and sea spray aerosol emissions [ default: Y ]
export CTM_WBDUST_BELD=NOBELD #> landuse database for identifying dust source regions  !Not used for NAQFC, NACC-CMAQ Inline Fengsha
export CTM_FST=N             #> mosaic method to get land-use specific stomatal flux
export CTM_BIDI_FERT_NH3=F   #> subtract fertilizer NH3 from emissions because it will be handled
export CTM_GRAV_SETL=Y       #> vdiff aerosol gravitational sedimentation [ default: Y ]
export VERTEXT=N             #> Vertical Extraction Options.  If Y need file defined as "export VERTEXT_COORD_PATH=${HOMEaqm}/scripts/lonlat.csv"
if [ "${VERTEXT}" == "Y" ] && [ ! -s ${VERTEXT_COORD_PATH} ]; then
   echo "WARNING :: exaqm_cmaqv531_ak.sh.ecf : VERTEXT is set as ${VERTEXT} and no ${VERTEXT_COORD_PATH} can be found"
fi

#> I/O Controls
export IOAPI_CHECK_HEADERS=N #> check file headers [ options: Y | N ]
export CTM_EMISCHK=N         #> Abort CMAQ if missing surrogates from emissions Input files
export EMISDIAG=F            #> Print Emission Rates at the output time step after they have been
                             #>   scaled and modified by the user Rules [options: F | T or 2D | 3D | 2DSUM ]
                             #>   Individual streams can be modified using the variables:
                             #>       GR_EMIS_DIAG_## | STK_EMIS_DIAG_## | BIOG_EMIS_DIAG
                             #>       MG_EMIS_DIAG    | LTNG_EMIS_DIAG   | DUST_EMIS_DIAG
                             #>       SEASPRAY_EMIS_DIAG
                             #>   Note that these diagnostics are different than other emissions diagnostic
                             #>   output because they occur after scaling.
export EMIS_SYM_DATE=N       #> Master switch for allowing CMAQ to use the date from each Emission file
                             #>   rather than checking the emissions date against the internal model date.
                             #>   [options: T | F or Y | N]. If false (F/N), then the date from CMAQ's internal
                             #>   time will be used and an error check will be performed (recommended). Users
                             #>   may switch the behavior for individual emission files below using the variables:
                             #>       GR_EM_SYM_DATE_## | STK_EM_SYM_DATE_## [default : N ]
export EMISDIAG_SUM=F        #> Print Sum of Emission Rates to Gridded Diagnostic File

#> Diagnostic Output Flags
export NLAYS_PHOTDIAG="1"    #> Number of layers for PHOTDIAG2 and PHOTDIAG3 from
                             #>     Layer 1 to NLAYS_PHOTDIAG  [ default: all layers ]
#export NWAVE_PHOTDIAG="294 303 310 316 333 381 607"  #> Wavelengths written for variables
                                                      #>   in PHOTDIAG2 and PHOTDIAG3
                                                      #>   [ default: all wavelengths ]
export CTM_PMDIAG=Y          #> Instantaneous Aerosol Diagnostic File [ default: Y ]
export CTM_APMDIAG=Y         #> Hourly-Average Aerosol Diagnostic File [ default: Y ]
export APMDIAG_BLEV_ELEV="1 1"  #> layer range for average pmdiag = NLAYS
export VDIFF_DIAG_FILE=N     #> vdiff & possibly aero grav. sedimentation diagnostic file [ default: N ]
export LTNGDIAG=N            #> lightning diagnostic file [ default: N ]
export PLMRISE_OPT=${PLMRISE_OPT:-1}         # fire plume rise: 1 Briggs;2 Sofiev
#------------------------------------------------------------------------------------------------------------

DISP=delete

#ozone column data
OMIfile=OMI_1979_to_2017.dat

## Directory defined in JAQM_FORECAST_AK
echo "DIAG : meterology files ingested from ${COMINmet}"
echo "DIAG : emission   files ingested from ${COMINemi}"
echo "DIAG : Lateral BC files ingested from ${COMINbc}"

## Check CMAQ PREP completion with the production of CMAQ MET files
if [ ! -s ${COMINmet}/aqm.${cycle}.metcro3d.ncf ]; then
   echo "======================================================================"
   err_exit "FATAL ERROR - COULD NOT LOCATE:${COMINmet}/aqm.${cycle}.metcro3d.ncf"
fi

export EMISSCTRL_NML=${PARMaqm}/EmissCtrl_${MECH}.nml

export GRID_CRO_2D=${COMINmet}/aqm.${cycle}.grdcro2d.ncf
export GRID_DOT_2D=${COMINmet}/aqm.${cycle}.grddot2d.ncf
export MET_CRO_2D=${COMINmet}/aqm.${cycle}.metcro2d.ncf
export MET_CRO_3D=${COMINmet}/aqm.${cycle}.metcro3d.ncf
export MET_DOT_3D=${COMINmet}/aqm.${cycle}.metdot3d.ncf
export MET_BDY_3D=${COMINmet}/aqm.${cycle}.metbdy3d.ncf
export SOI_CRO=${COMINmet}/aqm.${cycle}.soicro.ncf
export LUFRAC_CRO=${COMINmet}/aqm.${cycle}.lufraccro.ncf

# synchronize the nodes
${FSYNC} ${COMINmet}/aqm.${cycle}.grdcro2d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.grddot2d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.metcro2d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.metcro3d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.metdot3d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.metbdy3d.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.soicro.ncf
${FSYNC} ${COMINmet}/aqm.${cycle}.lufraccro.ncf

#> Gridded Emissions files
export N_EMIS_GR=2
export GR_EMIS_001=${COMINemi}/${RUN9}-emis-noRWC-cb6-${YM}.ncf #surface emission file
export GR_EMIS_LAB_001=GRIDDED_EMIS
export GR_EM_SYM_DATE_001=F
export GR_EMIS_002=${COMINemi}/${RUN9}-emis-RWC-cb6-${YM}.ncf #surface RWC file
export GR_EMIS_LAB_002=GRIDDED_RWC
export GR_EM_SYM_DATE_002=F
if [ ! -s ${GR_EMIS_001} ]; then
   echo "======================================================================"
   err_exit "FATAL ERROR - COULD NOT LOCATE:${GR_EMIS_001}"
fi
if [ ! -s ${GR_EMIS_002} ]; then
   echo "======================================================================"
   err_exit "FATAL ERROR - COULD NOT LOCATE:${GR_EMIS_002}"
fi

## CMAQv5.3 In-Line Point Emissions Files
for sectors in ptegu ptnonipm pt_oilgas cmv_c1c2 cmv_c3 othpt ; do
if [ ! -s ${COMINemi}/inln_mole_${sectors}_${YM}_12${RUN9}_cmaq_cb6_2016fh_16j.ncf ]; then
   echo "======================================================================"
   err_exit "FATAL ERROR - COULD NOT LOCATE:${COMINemi}/inln_mole_${sectors}_${YM}_12${RUN9}_cmaq_cb6_2016fh_16j.ncf"
fi

if [ ! -s ${COMINemi}/stack_groups_${sectors}_12${RUN9}_2016fh_16j.ncf ]; then
   echo "======================================================================"
   err_exit "FATAL ERROR - COULD NOT LOCATE:${COMINemi}/stack_groups_${sectors}_12${RUN9}_2016fh_16j.ncf"
fi
done

## Type of point emission diagnosis output is defined as TYPE_POINT_EMIS_OUT ~/jobs/JAQM_FORECAST_AK
## 2DSUM [default] or 3D
ic=0
for sectors in ptegu ptnonipm pt_oilgas cmv_c1c2 cmv_c3 othpt ; do
   if [ -s ${COMINemi}/inln_mole_${sectors}_${YM}_12${RUN9}_cmaq_cb6_2016fh_16j.ncf ] && \
      [ -s ${COMINemi}/stack_groups_${sectors}_12${RUN9}_2016fh_16j.ncf ]; then
      let ic=${ic}+1
      typeset -Z3 ic
      export STK_GRPS_${ic}=${COMINemi}/stack_groups_${sectors}_12${RUN9}_2016fh_16j.ncf
      export STK_EMIS_${ic}=${COMINemi}/inln_mole_${sectors}_${YM}_12${RUN9}_cmaq_cb6_2016fh_16j.ncf
      export STK_EMIS_LAB_${ic}=${sectors}
      export STK_EMIS_DIAG_${ic}=${TYPE_POINT_EMIS_OUT}
      export STK_EM_SYM_DATE_${ic}=F
   fi
done

## fire emission
if [ "$FCST" == "YES" ]; then
   FIRE_DIR=${COMIN}
   FIRE_SUFFIX=${cycle}
else
    FIRE_DIR=${COMINm1}   # 06z run for yesterday only
    FIRE_SUFFIX=${cycle}
fi

## Type of fire emission diagnosis output is defined as TYPE_FIRE_EMIS_OUT ~/jobs/JAQM_FORECAST_AK
## 2DSUM [default] or 3D
export FLAG_WF_ON="NO"
if [ -s ${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}_r.ncf ] && [ "${FCST}" == "NO" ]; then
   let ic=${ic}+1
   export FLAG_WF_ON="YES"
   typeset -Z3 ic
   export STK_GRPS_${ic}=${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_location_${RUN}_r.ncf
   export STK_EMIS_${ic}=${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}_r.ncf
   FIRE_EMIS_LABEL="PT_FIRES"
   export STK_EMIS_LAB_${ic}=${FIRE_EMIS_LABEL}
   export STK_EMIS_DIAG_${ic}=${TYPE_FIRE_EMIS_OUT}
   export STK_EM_SYM_DATE_${ic}=F   
elif [ -s ${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}.ncf ] && [ "${FCST}" == "YES" ]; then
   let ic=${ic}+1
   export FLAG_WF_ON="YES"
   typeset -Z3 ic
   export STK_GRPS_${ic}=${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_location_${RUN}.ncf
   export STK_EMIS_${ic}=${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}.ncf
   FIRE_EMIS_LABEL="PT_FIRES"
   export STK_EMIS_LAB_${ic}=${FIRE_EMIS_LABEL}
   export STK_EMIS_DIAG_${ic}=${TYPE_FIRE_EMIS_OUT}
   export STK_EM_SYM_DATE_${ic}=F
else
   echo "WARNING :: ========================================================"
   echo "WARNING :: Can not find ${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}_r.ncf or ${FIRE_DIR}/aqm.${FIRE_SUFFIX}.fire_emi_${RUN}.ncf"
   echo "WARNING :: Assuming no fire for this cycle"
   echo "WARNING :: ========================================================"
fi

if [ ${ic} -le 0 ]; then
   export CTM_PT3DEMIS=N
else
   export N_EMIS_PT=${ic}          #> Number of elevated source groups
   export CTM_PT3DEMIS=Y
   export LAYP_STDATE=${STDATE}
   export LAYP_STTIME=${STTIME}
   export LAYP_NSTEPS=${NSTEPS}
fi

#------------------------------------------------------
# ICFILE is now defined in above statement when NEW_START is determined
#------------------------------------------------------

# In-line biogenic emissions configuration

if [ "${CTM_BIOGEMIS}" == "Y" ]; then
   export GSPRO=${FIXaqm}/gspro_biog_static_2012_naqfc.txt
   export B3GRD=${FIXaqm}/b3grd_${RUN}_US12_bv314.ncf
   export BIOG_SPRO=B10C5 #< speciation profile to use > e.g. B10C5
   export BIOSW_YN=Y      #< use frost date switch? > defaults to Y
   export SUMMER_YN=N     #< Use summer normalized emissions? > defaults to Y 
   export PX_VERSION=N
   export B3GTS_DIAG=Y    
   export SOILINP=${soil_file}     #CCTM_D502b_Linux2_x86_64intel.SOILOUT.CMAQ-BENCHMARK_${PDYm1}
                                   #> Biogenic NO soil input file; ignore if NEW_START = Y
   if [ "${BIOSW_YN}" == "Y" ]; then
      export BIOSEASON=${MET_CRO_2D} #NAQFC/NACC_CMAQ bioseason soft switch based on season and T2
   fi

   #related to restart soil information file
   if [ -s ${SOILINP} ]; then
      export NEW_START=N
   else
      export NEW_START=Y
   fi
   # Sanitary Check
   if [ ! -s ${GSPRO} ]; then
      echo "ERROR WARNING :: ================== FATAL ERROR ======================"
      echo "ERROR WARNING :: Can not find ${GSPRO}"
      echo "ERROR WARNING :: ================== FATAL ERROR ======================"
   fi
   if [ ! -s ${B3GRD} ]; then
      echo "ERROR WARNING :: ================== FATAL ERROR ======================"
      echo "ERROR WARNING :: Can not find ${B3GRD}"
      echo "ERROR WARNING :: ================== FATAL ERROR ======================"
   fi
fi

if [ "${CTM_ERODE_AGLAND}" == "Y" ]; then
   # Input variables for BELD3 Landuse option
   export CROPMAP01=${FIXaqm}/aqm_CROPMAP01_cs
   export CROPMAP04=${FIXaqm}/aqm_CROPMAP04_cs
   export CROPMAP08=${FIXaqm}/aqm_CROPMAP08_cs
fi

#-----------------------------------------------------------------
# OCEAN FILE FOR THE Aerosol run
#-----------------------------------------------------------------

export OCEAN_1=${FIXaqm}/SSMASK_${RUN}12_199X163.ncf

#> Bidirectional ammonia configuration
if [ ${CTM_ABFLUX} == 'Y' ]; then
   # need to modify for FEST-C v1.4.
   export E2C_SOIL=${FIXaqm}/test_soil_cs_new.nc
   export E2C_CHEM=${COMINemi}/FERT_12km_5x_time${YM}.ncf
   export E2C_LU=${FIXaqm}/beld4_AQF_cs_output.ncf
fi
#------------------------------------------------------
# output files
#------------------------------------------------------
export CTM_APPL=${CFG}

# In-line biogenic emissions output files
if [ "${CTM_BIOGEMIS}" == "Y" ]; then
   export B3GTS_S=${DATA}/"B3GTS_S".${CTM_APPL}
   export SOILOUT=${DATA}/"SOILOUT".${CTM_APPL}
fi

# set floor file (neg concs)
export FLOOR_FILE=${DATA}/FLOOR_${CTM_APPL}

test=`ls CTM_LOG_*.${CTM_APPL}`
if [ "$test" != "" ] ; then
 if [ "${DISP}" == "delete" ] ; then
    echo "ancillary log files being deleted" 
    for file in $test
    do
       echo "deleting ${file}"
       rm ${file}
    done
 else
    echo "*** Logs exist - run ABORTED ***" 
    exit 1 
 fi 
fi 

## COMINbc is defined in JAQM_FORECAST_AK for LBC location [same as CMAQ MET location]
export BNDY_GASC_1=${FIXaqm}/aqm_${RUN}_cb05_ae4_mean_${MM}.35L.ncf
if [ ${cycle} = "t00z" ] || [ "${FCST}" == "NO" ]; then    ## if FCST=NO then use previous day LBC for 24-rerun
   if [ -s ${COMINm1}/aqm_${RUN}_geos_fv3chem_aero_${PDYm1}_35L.ncf ]; then
      export BNDY_GASC_1=${COMINm1}/aqm_${RUN}_geos_fv3chem_aero_${PDYm1}_35L.ncf
   elif [ -s ${COMINm2}/aqm_${RUN}_geos_fv3chem_aero_${PDYm2}_35L.ncf ]; then
      export BNDY_GASC_1=${COMINm2}/aqm_${RUN}_geos_fv3chem_aero_${PDYm2}_35L.ncf
   elif [ -s ${COMINm3}/aqm_${RUN}_geos_fv3chem_aero_${PDYm3}_35L.ncf ]; then
      export BNDY_GASC_1=${COMINm3}/aqm_${RUN}_geos_fv3chem_aero_${PDYm3}_35L.ncf
   fi
else
   if [ -s ${COMIN}/aqm_${RUN}_geos_fv3chem_aero_${PDY}_35L.ncf ]; then
      export BNDY_GASC_1=${COMIN}/aqm_${RUN}_geos_fv3chem_aero_${PDY}_35L.ncf
   elif [ -s ${COMINm1}/aqm_${RUN}_geos_fv3chem_aero_${PDYm1}_35L.ncf ]; then
      export BNDY_GASC_1=${COMINm1}/aqm_${RUN}_geos_fv3chem_aero_${PDYm1}_35L.ncf
   elif [ -s ${COMINm2}/aqm_${RUN}_geos_fv3chem_aero_${PDYm2}_35L.ncf ]; then
      export BNDY_GASC_1=${COMINm2}/aqm_${RUN}_geos_fv3chem_aero_${PDYm2}_35L.ncf
   fi
fi

if [ ! -f ${BNDY_GASC_1}  ]; then
  export BNDY_GASC_1=${FIXaqm}/aqm_${RUN}_cb05_ae4_mean_${MM}.35L.ncf
fi

export BNDY_AERO_1=${BNDY_GASC_1}
export BNDY_NONR_1=${BNDY_GASC_1}
export BNDY_TRAC_1=${BNDY_GASC_1}
export BCFILE=${BNDY_GASC_1}

#-----------------------------------------------------
# for the run control
#------------------------------------------------------
export CTM_STDATE=${STDATE}
export CTM_STTIME=${STTIME}
export CTM_RUNLEN=${NSTEPS}
export CTM_TSTEP=${TSTEP}
export EMIS_1=${GR_EMIS_001}
export EMIS_2=${GR_EMIS_002}
export INIT_GASC_1=${ICFILE}
export INIT_AERO_1=${INIT_GASC_1}
export INIT_NONR_1=${INIT_GASC_1}
export INIT_TRAC_1=${INIT_GASC_1}
export OMI=${FIXaqm}/${OMIfile}

#CMAQv5.3.1
export INIT_CONC_1=${ICFILE}
export BNDY_CONC_1=${BNDY_GASC_1}
export OPTICS_DATA=${PARMaqm}/PHOT_OPTICS.dat
TR_DVpath=${COMIN}
TR_DVfile=${MET_CRO_2D}

# species defn & photolysis
export gc_matrix_nml=${PARMaqm}/GC_${MECH}.nml
export ae_matrix_nml=${PARMaqm}/AE_${MECH}.nml
export nr_matrix_nml=${PARMaqm}/NR_${MECH}.nml
export tr_matrix_nml=${PARMaqm}/Species_Table_TR_0.nml

# check for photolysis input data
export CSQY_DATA=${PARMaqm}/CSQY_DATA_${MECH}
if [ ! -s ${CSQY_DATA} ] ; then
   echo " ${CSQY_DATA}  not found "
   exit 1
fi

############################
## output files
###########################
export          OUTDIR=${DATA}
export      CTM_CONC_1=${DATA}/CONC.${CTM_APPL} 
export        A_CONC_1=${DATA}/ACONC.${CTM_APPL}
export         S_CGRID=${DATA}/CGRID.${CTM_APPL}
export   CTM_DRY_DEP_1=${DATA}/DRYDEP.${CTM_APPL}
export   CTM_WET_DEP_1=${DATA}/WETDEP1.${CTM_APPL}
export   CTM_DEPV_DIAG=${DATA}/DEPV.${CTM_APPL} 
export         B3GTS_S=${DATA}/B3GTS_S.${CTM_APPL}
export         SOILOUT=${DATA}/SOILOUT.${CTM_APPL}
export CTM_DUST_EMIS_1=${DATA}/DUST_EMIS.${CTM_APPL}
#CMAQv5.3
export    CTM_PMDIAG_1=${DATA}/PMDIAG.${CTM_APPL}     #> On-Hour Particle Diagnostics
export   CTM_APMDIAG_1=${DATA}/APMDIAG.${CTM_APPL}     #> Hourly Avg. Particle Diagnostics
export        CTM_RJ_1=${DATA}/RJ_1.${CTM_APPL}
export        CTM_RJ_2=${DATA}/RJ_2.${CTM_APPL}
export        CTM_RJ_3=${DATA}/RJ_3.${CTM_APPL}

if [ "${FLAG_3DPOINT_EMIS_OUT}" == "YES" ] && [ "${CTM_PT3DEMIS}" == "Y" ] && [ "${PT3DDIAG}" == "Y" ]; then
   export   CTM_PT3D_DIAG=${DATA}/PT3D.${CTM_APPL} 
fi

flist1="${CTM_CONC_1} ${S_CGRID} ${A_CONC_1} ${CTM_DRY_DEP_1} ${CTM_DEPV_DIAG} ${CTM_PT3D_DIAG}"
flist2="${B3GTS_S} ${SOILOUT} ${CTM_WET_DEP_1} ${CTM_WET_DEP_2} ${CTM_VIS_1} ${CTM_DIAM_1} ${CTM_RJ_1}"
flist3="${CTM_RJ_2} ${CTM_SSEMIS_1} ${CTM_DUST_EMIS_1} ${CTM_IPR_1} ${CTM_IPR_2} ${CTM_IPR_3} ${CTM_IRR_1}"
flist4="${CTM_IRR_2} ${CTM_IRR_3} ${CTM_DEPV_FST} ${CTM_DEPV_MOS} ${CTM_DRY_DEP_FST} ${CTM_DRY_DEP_MOS}"
flist="${flist1} ${flist2} ${flist3} ${flist4}"

unalias rm

export ff

for ff in ${flist};  do 
   if [ "${ff}" != "-v" ]; then
      file=`echo ${ff} | cut -d' ' -f1`
      if [ -s ${file} ] ; then
         echo " ${file} already exists "
         if [ ${DISP} == "delete" ] ; then
            echo " ${file} being deleted "
            rm -f ${file}
         elif [ ${DISP} == "update" ] ; then
            echo " ${file} being updated "
         else
            echo " *** RUN ABORTED *** "
            exit 1
         fi
      fi
   fi
done

NS=`echo ${NSTEPS} |cut -c1-2`
msg="Starting CMAQ Forecast at F00 out to F${NS}"
postmsg   "${msg}"

startmsg
echo "EXECUTE X.u2c"
export MEMORY_AFFINITY=MCM
num_col=32
num_row=8

export NPCOL_NPROW="${num_col} ${num_row}"
let ntasks=${num_col}*${num_row}

mpiexec -n $ntasks -ppn 128 --cpu-bind core ${EXECaqm}/${EXEC} >> ${pgmout} 2>errfile
export err=$?

#------------------------------------------------------
# copy output to /com
#------------------------------------------------------
if [ "${SENDCOM}" == "YES" ]  && [ "${FCST}" == "YES" ] && [ -s ${DATA}/SOILOUT.${CTM_APPL} ]; then
   mv ${DATA}/CONC.${CTM_APPL}       ${COMOUT}/aqm.${cycle}.conc.ncf
   mv ${DATA}/ACONC.${CTM_APPL}      ${COMOUT}/aqm.${cycle}.aconc.ncf
   mv ${DATA}/CGRID.${CTM_APPL}      ${COMOUT}/aqm.${cycle}.cgrid.ncf
   mv ${DATA}/DRYDEP.${CTM_APPL}     ${COMOUT}/aqm.${cycle}.drydep.ncf
   mv ${DATA}/WETDEP1.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.wetdep1.ncf
   mv ${DATA}/DEPV.${CTM_APPL}       ${COMOUT}/aqm.${cycle}.depv.ncf
   mv ${DATA}/PMDIAG.${CTM_APPL}     ${COMOUT}/aqm.${cycle}.pmdiag.ncf
   mv ${DATA}/APMDIAG.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.apmdiag.ncf
   mv ${DATA}/B3GTS_S.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.b3gt2.ncf
   if [ "${SINGLECYC}" == "YES" ]; then
      mv ${DATA}/SOILOUT.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.soil24.ncf
   else
      mv ${DATA}/SOILOUT.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.soil06.ncf
   fi
   if [ "${CTM_DUSTEM_DIAG}" == "Y" ]; then
      mv ${DATA}/DUST_EMIS.${CTM_APPL}  ${COMOUT}/aqm.${cycle}.dustemis.ncf
   fi
   mv ${DATA}/RJ_1.${CTM_APPL}       ${COMOUT}/aqm.${cycle}.rj_1.ncf
   mv ${DATA}/RJ_2.${CTM_APPL}       ${COMOUT}/aqm.${cycle}.rj_2.ncf
   mv ${DATA}/RJ_3.${CTM_APPL}       ${COMOUT}/aqm.${cycle}.rj_3.ncf 
   if [ -s ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc ] && [ "${PT3DDIAG}" == "Y" ]; then
      mv ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.fireemis.ncf
   else
      if [ "${FLAG_WF_ON}" == "YES" ] && [ "${PT3DDIAG}" == "Y" ]; then
         echo "WARNING :: FLAG_WF_ON = ${FLAG_WF_ON} but can not find ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc"
      fi
   fi
   if [ "${FLAG_POINT_EMIS_OUT}" == "YES" ]; then
      if [ -s ${DATA}/CCTM_EMDIAG_CMV_C1C2_12_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_CMV_C1C2_12_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.cmv_c1c2_12.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_CMV_C3_12_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_CMV_C3_12_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.cmv_c3_12.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PTEGU_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PTEGU_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.ptegu.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PTNONIPM_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PTNONIPM_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.ptnonipm.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PT_OILGAS_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PT_OILGAS_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.pt_oilgas.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_OTHPT_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_OTHPT_${CTM_APPL}.nc ${COMOUT}/aqm.${cycle}.othpt.ncf
      fi
   fi
   if [ "${FLAG_3DPOINT_EMIS_OUT}" == "YES" ] && [ "${CTM_PT3DEMIS}" == "Y" ] && [ "${PT3DDIAG}" == "Y" ]; then
      mv ${DATA}/PT3D.${CTM_APPL}    ${COMOUT}/aqm.${cycle}.pt3d.ncf 
   fi
fi
if [ "${SENDCOM}" == "YES" ] && [ "${FCST}" == "NO" ] && [ -s ${DATA}/SOILOUT.${CTM_APPL} ]; then
   mv ${DATA}/CONC.${CTM_APPL}       ${COMOUTm1}/aqm.${cycle}.conc_r.ncf
   mv ${DATA}/ACONC.${CTM_APPL}      ${COMOUTm1}/aqm.${cycle}.aconc_r.ncf
   mv ${DATA}/CGRID.${CTM_APPL}      ${COMOUTm1}/aqm.${cycle}.cgrid_r.ncf
   mv ${DATA}/DRYDEP.${CTM_APPL}     ${COMOUTm1}/aqm.${cycle}.drydep_r.ncf
   mv ${DATA}/WETDEP1.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.wetdep1_r.ncf
   mv ${DATA}/DEPV.${CTM_APPL}       ${COMOUTm1}/aqm.${cycle}.depv_r.ncf
   mv ${DATA}/PMDIAG.${CTM_APPL}     ${COMOUTm1}/aqm.${cycle}.pmdiag_r.ncf
   mv ${DATA}/APMDIAG.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.apmdiag_r.ncf
   mv ${DATA}/B3GTS_S.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.b3gt2_r.ncf
   if [ "${SINGLECYC}" == "YES" ]; then
      mv ${DATA}/SOILOUT.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.soil24_r.ncf
   else
      mv ${DATA}/SOILOUT.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.soil06_r.ncf
   fi
   if [ "${CTM_DUSTEM_DIAG}" == "Y" ]; then
      mv ${DATA}/DUST_EMIS.${CTM_APPL}  ${COMOUTm1}/aqm.${cycle}.dustemis_r.ncf
   fi
   mv ${DATA}/RJ_1.${CTM_APPL}       ${COMOUTm1}/aqm.${cycle}.rj_1_r.ncf
   mv ${DATA}/RJ_2.${CTM_APPL}       ${COMOUTm1}/aqm.${cycle}.rj_2_r.ncf
   mv ${DATA}/RJ_3.${CTM_APPL}       ${COMOUTm1}/aqm.${cycle}.rj_3_r.ncf 
   if [ -s ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc ] && [ "${PT3DDIAG}" == "Y" ]; then
      mv ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.fireemis_r.ncf
   else
      if [ "${FLAG_WF_ON}" == "YES" ]; then
         echo "WARNING :: FLAG_WF_ON = ${FLAG_WF_ON} but can not find ${DATA}/CCTM_EMDIAG_${FIRE_EMIS_LABEL}_${CTM_APPL}.nc"
      fi
   fi
   if [ "${FLAG_POINT_EMIS_OUT}" == "YES" ]; then
      if [ -s ${DATA}/CCTM_EMDIAG_CMV_C1C2_12_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_CMV_C1C2_12_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.cmv_c1c2_12_r.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_CMV_C3_12_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_CMV_C3_12_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.cmv_c3_12_r.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PTEGU_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PTEGU_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.ptegu_r.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PTNONIPM_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PTNONIPM_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.ptnonipm_r.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_PT_OILGAS_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_PT_OILGAS_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.pt_oilgas_r.ncf
      fi
      if [ -s ${DATA}/CCTM_EMDIAG_OTHPT_${CTM_APPL}.nc ]; then
         mv ${DATA}/CCTM_EMDIAG_OTHPT_${CTM_APPL}.nc ${COMOUTm1}/aqm.${cycle}.othpt_r.ncf
      fi
   fi
   if [ "${FLAG_3DPOINT_EMIS_OUT}" == "YES" ] && [ "${CTM_PT3DEMIS}" == "Y" ] && [ "${PT3DDIAG}" == "Y" ]; then
      mv ${DATA}/PT3D.${CTM_APPL}    ${COMOUTm1}/aqm.${cycle}.pt3d_r.ncf 
   fi
fi

if [ $err -ne 0 ]; then
   err_chk
else
   msg="${pgm} completed normally"
   echo "${PDY}" "done" >${COMOUT}/aqm.${cycle}.log
   postmsg   "${msg}"
fi

echo Exiting $0

exit
