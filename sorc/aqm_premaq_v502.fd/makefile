################################################################################ #
#     Makefile for PREMAQ  in EPA IBM
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#     The following macros will be of interest:
#
#         TARGET   - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         SMP      - threading
#         TRAPS    - runtime traps for floating point exceptions
#         DEBUG    - -g
#         MEM      - user data area and stack size
#         MAP      - load map
#         W3LIB    - w3lib
#         BALIB    - /nwprod/w3lib90/bacio_4
#         ESSL     - ESSL library
#         MASS     - MASS library
#
#################################################################################
#
# Define the name of the executable
DIR       = .


TARGET       = aqm_premaq_v502

#... Location of library

NETCDFPATH = ${NETCDF}
NETCDF_LIB = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}

IOAPIPATH    = ../../lib


#  .. Location of complier

 CC          = icc 

#... Location of complier &  OPTIONS

 FC          = ifort
 OMPFLAG1    =
 OMPFLAG2    =
 LINK_FLAGS  = #-q64
 DEBUG       = 
 DEBUG       =  #-g -traceback  

#... OPTIONS

  OPTS       = -assume byterecl
  OPTS1      = -free -O3 -fp-model precise -mp1 #-qfree -O3 -qstrict -q64
  OPTS2      = -fixed -132 -O3 -override-limits -mp1 -fp-model precise -I . #-qfixed=132 -O3 -qstrict -q64
  SMP        =
  TRAPS      =
  DEFS       = -DBIT32 -DIBM -DIFORT #-WF,-DBIT32,-DIBM
  INCLUDES   = -I${G2_INC4} -I${IP_INC4} -I${PNG_INC} -I${JASPER_INC} -I${Z_INC} \
                -Iedss_tools -Ismokelib ${CRAY_IOBUF_INCLUDE_OPTS}

  IOAPI_LIB   = $(IOAPIPATH)/libaqm_ioapi.a
#
### Assemble Options
#
  CFLAGS       = -DIFORT #-q64
  LOPTS        =

  FFLAGS       =  $(OPTS) $(OPTS1) $(TRAPS) #$(DEBUG)
  FFLAGS_FIXED = $(OPTS) $(OPTS2) $(TRAPS) #$(DEBUG)
  LDFLAGS      = $(MEM) $(MAP) $(SMP) $(PROFILE)

  LIBS    = $(IOAPI_LIB) \
            $(NETCDF_LIB) \
            ${W3NCO_LIB4} \
            ${W3EMC_LIB4} \
 	    ${G2_LIB4} \
	    ${IP_LIB4}     \
	    $(SP_LIB4)     \
            ${PNG_LIB}     \
            ${JASPER_LIB}  \
            ${Z_LIB} ${BACIO_LIB4}
#
# object files
#

MODULES = \
  parms3_mod.o \
  const_mod.o \
  choutcom_mod_o3tracers.o \
  const_mete_mod.o \
  const_pbl_mod-PX.o \
  coord_mod.o \
  date_pack_mod.o \
  date_time_mod.o \
  fdesc3_mod.o \
  file_mod-2010.o \
  gridinfo_mod-all.o \
  groutcom_mod.o \
  iodecl3_mod.o \
  mcoutcom_mod-NMMB.o \
  mdoutcom_mod.o \
  metinfo_mod.o \
  metvars_mod_WRF-NMMB.o \
  premaqparm_mod_WRF-NMMB.o \
  table_mod.o \
  vgrd_mod.o \
  xvars_mod-NMMB.o

MODULES_E = \
  modgrid.o \
  modbeis3.o \
  modbeis3v1.o \
  modbeis3v11.o \
  modbiog.o \
  modcntrl.o \
  moddayhr.o \
  modelev.o \
  modemfac.o \
  modinfo.o \
  modlists.o \
  modmerge.o \
  modmobil.o \
  modreprt.o \
  modsourc.o \
  modspro.o \
  modstcy.o \
  modsurg.o \
  modtmprl.o \
  modxref.o \
  modmbset.o \
  modfileset.o \
  mod_laypoint.o \
  mod_smkmerge.o \
  mod_mobile.o \
  mod_mrggrid.o \
  mod_temporal.o \
  mod_rw.o

OBJS = \
  premaq_WRF.o \
  alloc_ctm_o3tracers-NMMB.o \
  alloc_met_WRF-NMMB.o \
  alloc_x-NMMB.o \
  bcldprc_ak.o \
  check_flags_WRF-tmp-NMMB.o \
  chembc_o3tracers_PV_p02.o \
  chkgrib_WRF-B.o \
  close_files.o \
  collapx.o \
  comheader.o \
  crs2dot.o \
  dealloc_ctm_o3tracers-NMMB.o \
  dealloc_met_WRF-NMMB.o \
  dealloc_x-NMMB.o \
  dynflds-pv.o \
  emis_report.o \
  emissions-xmobil.o \
  etadry-2PX_canopyH.o \
  fill_metarys_WRF-2010-NMMB.o \
  fillsigma_WRF-NMMB.o \
  filltable_WRF-NMMB.o \
  getcentpoint-all.o \
  getmet-2010.o \
  getpblht_hydro.o \
  getpblht_hydro-acm.o \
  getprs_WRF-NMMB.o \
  getsdt.o \
  graceful_stop.o \
  gridgeometry-all.o \
  gridout.o \
  hlconst.o \
  init_met_WRF-NMMB.o \
  init_x-NMMB.o \
  layht_AQF.o \
  locate.o \
  met3dsup_AQF_hydro-NMMB.o \
  metcro_rhoJ-NMMB.o \
  metdot.o \
  metgrid2ctm_AQF-NMMB.o \
  metvars2ctm_WRF-Lai_4-Fricv_flip-mixht-NMMB.o \
  null_x.o \
  pblpkg.o \
  pblsup-acm.o \
  prep_emis-xmobil.o \
  pvorticity.o \
  rdgrib-2010.o \
  rdgribspecs_WRF-2010-NMMB.o \
  readchembc_gfs-top.o \
  readlanda.o \
  readnml_WRF-2010-NMMB.o \
  refstate_AQF.o \
  reset_flags_WRF-NMMB.o \
  setgriddefs_WRF-NMMB.o \
  setup_WRF-2010.o \
  heap_sort_down.o \
  statflds.o \
  vertarys_AQF-NMMB.o \
  verthyd_hydro.o \
  vertnhy.o \
  wind.o \
  wrgdesc.o \
  w3fi63.o \
  w3fi71.o \
  xxx-solar-off.o

OBJS_EDSSLIB = \
  checkmem.o  \
  chkint.o    \
  dscm3grd.o  \
  fltrneg.o   \
  getfline.o  \
  getm3epi.o  \
  hdrmiss3.o  \
  padzero.o   \
  polmesg.o   \
  unitfac.o   \
  chkreal.o   \
  dscm3lay.o  \
  getcfdsc.o  \
  getifdsc.o  \
  getnlist.o  \
  multunit.o  \
  parsline.o  \
  rd3mask.o   \
  unitmatch.o

#EDSS_LIB
# lambert.o   \


# SMKLIB
# asgnpsi.o   \
# getisize.o  \
# rdefxref.o \
#  tmprinfo.o \

OBJS_FILESET = \
  openset.o \
  descset.o \
  closeset.o \
  readset.o \
  chksetdesc.o \
  chkfileset.o \
  createset.o \
  appendname.o \
  cleanup.o \
  promptset.o \
  rdsetmask.o \
  rdmapmask.o \
  writeset.o \
  rdsrghdr.o \
  openphys.o \
  flterr.o

OBJS_SMKLIB = \
  alocchrt.o  \
  alocctbl.o  \
  alocetbl.o  \
  alocgtbl.o  \
  alocmtbl.o  \
  alocstbl.o  \
  alocttbl.o  \
  applreac.o  \
  bldcsrc.o   \
  blkorcmt.o  \
  chkcpvar.o  \
  chkgrid.o   \
  chkptdef.o  \
  chksrcno.o  \
  convrtxy.o  \
  dscsprof.o  \
  efsetup.o   \
  fillchrt.o  \
  fillctbl.o  \
  filletbl.o  \
  fillgtbl.o  \
  fillmtbl.o  \
  fillstbl.o  \
  fillttbl.o  \
  fltrxref.o  \
  fmtcsrc.o   \
  genuslst.o  \
  getctgry.o  \
  getformt.o  \
  getidasz.o  \
  getiname.o  \
  getsinfo.o  \
  grd2cnty.o  \
  ingrid.o \
  initem.o \
  mbsccadj.o \
  normtpro.o \
  parscsrc.o \
  pdsetup.o \
  progdesc.o \
  rdcodnam.o \
  rdeproc.o \
  rdgmat.o \
  rdhdays.o \
  rdinvchr.o \
  rdinvmap.o \
  rdinvpol.o \
  rdlines.o \
  rdmvinfo.o \
  rdpelv.o \
  rdrmat.o \
  rdsccdsc.o \
  rdsmat.o \
  rdstcy.o \
  rdtprof.o \
  rdtref.o \
  rdumat.o \
  verchar.o \
  wrdaymsg.o \
  xreftbl.o \

OBJS_BEIS = \
  beis311.o \
  czangle311.o \
  getpar311.o \
  hrbeis313.o \
  hrno312.o \
  prebmet311.o \
  rdbpro311.o \
  pre_beis.o \
  emis_end.o

OBJS_LAYPOINT = \
  laypoint-acm.o \
  openlayout.o \
  preplm.o \
  plmris.o \
  postplm.o \
  plsprd.o \
  pre_point.o

OBJS_SMKMERGE = \
  smkmerge.o \
  allocmrg.o \
  bldmrgidx.o \
  getmrgev.o \
  initstcy.o \
  mrgbio.o \
  mrgelev.o \
  mrgmult.o \
  mrgonams.o \
  mrgunits.o \
  mrgvnams.o \
  openmrgin.o \
  openmrgout.o \
  rdmrginv.o \
  wmrgelev.o \
  wmrgemis.o \
  wrmrgrep.o \
  pre_smkmerge.o

OBJS_TEMPORAL = \
  asgntpro.o \
  genhemis.o \
  mktmat.o \
  opentmp.o \
  opentmpin.o \
  setdaylt.o \
  temporal.o \
  pre_temporal.o \
  tmnamunt.o \
  updtmat.o \
  wrtsup.o \
  setscctype.o \
  ioapi_grd_size.o \
  alocptbl.o \
  fillptbl.o \
  alocatbl.o \
  fillatbl.o \
  rdmappol.o \

OBJS_MOBILE = \
  nlsqrm6.o \
  pre_mobile-2007.o \
  mobile_emis_model.o

OBJS_MRGGRID = \
  pre_mrggrid-xmobil.o \
  mrggrid-xmobil.o


#########

all:
	@$(MAKE) $(MODULES) $(MODULES_E)
	@$(MAKE) $(TARGET)

.SUFFIXES: .F .f .c

$(TARGET):  $(MODULES) $(MODULES_E) $(OBJS) $(OBJS_EDSSLIB) $(OBJS_FILESET) \
	    $(OBJS_SMKLIB) $(OBJS_BEIS) $(OBJS_LAYPOINT) $(OBJS_SMKMERGE) \
	    $(OBJS_TEMPORAL) $(OBJS_MOBILE) $(OBJS_MRGGRID)
	$(FC) $(LDFLAGS) $(LINK_FLAGS) -o  ../../exec/$(TARGET) $(FFLAGS) $(LOPTS) \
	    $(MODULES) $(MODULES_E) $(OBJS) $(OBJS_EDSSLIB) $(OBJS_FILESET) \
	    $(OBJS_SMKLIB) $(OBJS_BEIS) $(OBJS_LAYPOINT) $(OBJS_SMKMERGE) \
	    $(OBJS_TEMPORAL) $(OBJS_MOBILE) $(OBJS_MRGGRID) $(LIBS) -openmp ${CRAY_IOBUF_POST_LINK_OPTS}	    

bcldprc_ak.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c bcldprc_ak.F

chembc_o3tracers_PV_p02.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c chembc_o3tracers_PV_p02.F

collapx.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c collapx.F

crs2dot.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c crs2dot.F

etadry-2PX_canopyH.o:
	$(FC) $(FFLAGS) $(OMPFLAG2) $(DEFS) $(INCLUDES) -c etadry-2PX_canopyH.F

getprs_WRF-NMMB.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c getprs_WRF-NMMB.F

gridgeometry-all.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c gridgeometry-all.F

met3dsup_AQF_hydro-NMMB.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c met3dsup_AQF_hydro-NMMB.F

metcro_rhoJ-NMMB.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c metcro_rhoJ-NMMB.F

metdot.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c metdot.F

pblpkg.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c pblpkg.F

pblsup-acm.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c pblsup-acm.F

refstate_AQF.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c refstate_AQF.F

verthyd_hydro.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c verthyd_hydro.F

vertnhy.o:
	$(FC) $(FFLAGS) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c vertnhy.F

beis311.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c beis311.f

bldmrgidx.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c bldmrgidx.f

czangle311.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c czangle311.f

hrbeis313.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c hrbeis313.f

hrno312.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG2) $(DEFS) $(INCLUDES) -c hrno312.f

mktmat.o:
	$(FC) $(FFLAGS_FIXED) $(OMPFLAG1) $(DEFS) $(INCLUDES) -c mktmat.f

.f.o:
	$(FC) $(FFLAGS_FIXED) $(DEFS) $(INCLUDES) -c $<

readset.o:
	$(FC) $(FFLAGS_FIXED) $(DEFS) $(INCLUDES) -c readset.F

writeset.o:
	$(FC) $(FFLAGS_FIXED) $(DEFS) $(INCLUDES) -c writeset.F

.F.o:
	$(FC) $(FFLAGS) $(DEFS) $(INCLUDES) -c $<

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

clean:
	rm -f *.o *.mod *.lst $(TARGET)
