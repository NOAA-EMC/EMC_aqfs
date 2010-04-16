 MODEL = CCTM_N4a 

 LIB_LOC = /nwprod
### STENEX = /nwpara/lib/
### LSTENEX = -L$(STENEX) -laqm_sef90
 STENEX = /meso/save/wx20pl/mytools/se-rc4n
 LSTENEX = -L$(STENEX) -lsef90

### LPARIO  = -L$(LIB_LOC)/lib -laqm_pario
 LPARIO  = -L/meso/save/wx20pl/mytools/pario-n  -lpario
### LPARIO  = -L/meso/save/wx20dw/tools/pario-n_64  -lpario
 LPARIO  = -L/meso/save/wx20dw/tools/PARIO -lpario

 LIOAPI  = -L/meso/save/wx20pl/mytools/ioapi_3/AIX -lioapi
### LIOAPI  = -L/meso/save/wx20dw/tools/ioapi_3_64/AIX -lioapi
### LNETCDF = /usrx/local/netcdf.3.5.0/lib -lnetcdf
 LNETCDF = -L/meso/save/wx20pl/mytools/netcdf-3.6.0-p1/lib  -lnetcdf
 LNTCDF  = -L/meso/save/wx20pl/mytools/ioapi_3/AIX -lnotcdf

 LSMP    = -L/usr/lib -lxlsmp
 MPI     = /usr/lpp/ppe.poe/include/thread
 FC      = /usr/bin/ncepmpxlf90
 CC      = /usr/bin/ncepcc

 FSTD    = -d -qfixed=132 -qintlog -qspillsize=4096 -qarch=auto -qmaxmem=-1

# on by default at ncep:
#   -O (-O2), -g, -qarch=auto, -q64, -qstrict
# -qstrict applies to -O3 and higher,
# -g does NOT imply no optimization. -O0 ("oh zero") is no optimization  # FLFLAGS  had added ${FLT}

 HPM_DIR = /usr/local/bin/hpm
 HPM_INC = -I$(HPM_DIR)/include
 HPM_LIB = -L$(HPM_DIR)/lib -lhpm_r -lpmapi -lm

 FLT = -qflttrap=zero:ov:inv:en -qsigtrap
 F_FLAGS = $(FSTD) -Q -O3 -qstrict -I$(STENEX) -I .
 C_FLAGS = -O3
 LINK_FLAGS =

#######################################################^^#from_Jeff
# BASE = /vortex/home/yoj/src/novrel/delic/BLD_N3a
#> see ~wdx/lib/scom ...
# lib_basedir = /home/wdx/lib
# myFC = ifort
# myCC = /usr/bin/cc
# system = ia64
# compiler = ifc
# LIOAPI = Linux2_ia64ifort
# FFLAGS = -fixed -extend_source -O3 -fno-alias -mp1
#FFLAGS = -fixed -extend_source -g -fno-alias -check bounds -fpe0 -d-lines -ftrapuv -traceback
#FFLAGS = -fixed -extend_source -g -fno-alias -check bounds -ftrapuv -traceback
# FC    = $(myFC)
# CC    = $(myCC)
# FPP   = $FC
# lib_path   = $(lib_basedir)/${system}/${compiler}
# F_FLAGS    = $(FFLAGS) -I $(lib_path)/se_snl -I .
# C_FLAGS    = -v -O3
# LINK_FLAGS = -i-static 
#######################################################VV#from_Jeff

 CPP_FLAGS  = \
 -WF,-Dparallel \
 -WF,-DINTERPB=PINTERPB \
 -WF,-DM3ERR=PM3ERR \
 -WF,-DM3EXIT=PM3EXIT \
 -WF,-DM3WARN=PM3WARN \
 -WF,-DSHUT3=PSHUT3 \
 -WF,-DWRITE3=PWRITE3 \
 -WF,-DSUBST_MODULES=SE_MODULES \
 -WF,-DSUBST_BARRIER=SE_BARRIER \
 -WF,-DSUBST_GLOBAL_MAX=SE_GLOBAL_MAX \
 -WF,-DSUBST_GLOBAL_MIN=SE_GLOBAL_MIN \
 -WF,-DSUBST_GLOBAL_MIN_DATA=SE_GLOBAL_MIN_DATA \
 -WF,-DSUBST_GLOBAL_TO_LOCAL_COORD=SE_GLOBAL_TO_LOCAL_COORD \
 -WF,-DSUBST_GLOBAL_SUM=SE_GLOBAL_SUM \
 -WF,-DSUBST_GLOBAL_LOGICAL=SE_GLOBAL_LOGICAL \
 -WF,-DSUBST_LOOP_INDEX=SE_LOOP_INDEX \
 -WF,-DSUBST_SUBGRID_INDEX=SE_SUBGRID_INDEX \
 -WF,-DSUBST_HI_LO_BND_PE=SE_HI_LO_BND_PE \
 -WF,-DSUBST_SUM_CHK=SE_SUM_CHK \
 -WF,-DSUBST_INIT_ARRAY=SE_INIT_ARRAY \
 -WF,-DSUBST_COMM=SE_COMM \
 -WF,-DSUBST_MY_REGION=SE_MY_REGION \
 -WF,-DSUBST_SLICE=SE_SLICE \
 -WF,-DSUBST_GATHER=SE_GATHER \
 -WF,-DSUBST_DATA_COPY=SE_DATA_COPY \
 -WF,-DSUBST_IN_SYN=SE_IN_SYN 

#####################################################^^#from_Jeff
# LIBRARIES = \
# -L$(lib_path)/pario -lpario \
# -L$(lib_path)/ioapi_3/$(LIOAPI) -lioapi \
# -L$(lib_path)/se_snl -lse_snl \
# -L$(lib_path)/mpich/lib -lmpich \
# -L$(lib_path)/netcdf/lib -lnetcdf
#####################################################VV#from_Jeff

LIBRARIES = \
 $(LSTENEX) \
 $(LNETCDF) \
 $(LIOAPI) \
 $(LPARIO)

 BASE_INC = /meso/save/wx20pl/cmaq4.7.1a/cmaq471a_ppm
 MPI_INC = $(MPI)

 INCLUDES = \
 -WF,-DSUBST_GRID_ID= \
 -WF,-DSUBST_PE_COMM=\"$(BASE_INC)/PE_COMM.EXT\" \
 -WF,-DSUBST_CONST=\"$(BASE_INC)/CONST.EXT\" \
 -WF,-DSUBST_FILES_ID=\"$(BASE_INC)/FILES_CTM.EXT\" \
 -WF,-DSUBST_EMPR_VD=\"$(BASE_INC)/EMISPRM.vdif.EXT\" \
 -WF,-DSUBST_EMPR_CH=\"$(BASE_INC)/EMISPRM.chem.EXT\" \
 -WF,-DSUBST_IOPARMS=\"$(BASE_INC)/PARMS3.EXT\" \
 -WF,-DSUBST_IOFDESC=\"$(BASE_INC)/FDESC3.EXT\" \
 -WF,-DSUBST_IODECL=\"$(BASE_INC)/IODECL3.EXT\" \
 -WF,-DSUBST_RXCMMN=\"$(BASE_INC)/RXCM.EXT\" \
 -WF,-DSUBST_RXDATA=\"$(BASE_INC)/RXDT.EXT\" \
 -WF,-DSUBST_GC_SPC=\"$(BASE_INC)/GC_SPC.EXT\" \
 -WF,-DSUBST_GC_EMIS=\"$(BASE_INC)/GC_EMIS.EXT\" \
 -WF,-DSUBST_GC_ICBC=\"$(BASE_INC)/GC_ICBC.EXT\" \
 -WF,-DSUBST_GC_DIFF=\"$(BASE_INC)/GC_DIFF.EXT\" \
 -WF,-DSUBST_GC_DDEP=\"$(BASE_INC)/GC_DDEP.EXT\" \
 -WF,-DSUBST_GC_DEPV=\"$(BASE_INC)/GC_DEPV.EXT\" \
 -WF,-DSUBST_GC_ADV=\"$(BASE_INC)/GC_ADV.EXT\" \
 -WF,-DSUBST_GC_CONC=\"$(BASE_INC)/GC_CONC.EXT\" \
 -WF,-DSUBST_GC_G2AE=\"$(BASE_INC)/GC_G2AE.EXT\" \
 -WF,-DSUBST_GC_G2AQ=\"$(BASE_INC)/GC_G2AQ.EXT\" \
 -WF,-DSUBST_GC_SCAV=\"$(BASE_INC)/GC_SCAV.EXT\" \
 -WF,-DSUBST_GC_WDEP=\"$(BASE_INC)/GC_WDEP.EXT\" \
 -WF,-DSUBST_AE_SPC=\"$(BASE_INC)/AE_SPC.EXT\" \
 -WF,-DSUBST_AE_EMIS=\"$(BASE_INC)/AE_EMIS.EXT\" \
 -WF,-DSUBST_AE_ICBC=\"$(BASE_INC)/AE_ICBC.EXT\" \
 -WF,-DSUBST_AE_DIFF=\"$(BASE_INC)/AE_DIFF.EXT\" \
 -WF,-DSUBST_AE_DDEP=\"$(BASE_INC)/AE_DDEP.EXT\" \
 -WF,-DSUBST_AE_DEPV=\"$(BASE_INC)/AE_DEPV.EXT\" \
 -WF,-DSUBST_AE_ADV=\"$(BASE_INC)/AE_ADV.EXT\" \
 -WF,-DSUBST_AE_CONC=\"$(BASE_INC)/AE_CONC.EXT\" \
 -WF,-DSUBST_AE_A2AQ=\"$(BASE_INC)/AE_A2AQ.EXT\" \
 -WF,-DSUBST_AE_SCAV=\"$(BASE_INC)/AE_SCAV.EXT\" \
 -WF,-DSUBST_AE_WDEP=\"$(BASE_INC)/AE_WDEP.EXT\" \
 -WF,-DSUBST_NR_SPC=\"$(BASE_INC)/NR_SPC.EXT\" \
 -WF,-DSUBST_NR_EMIS=\"$(BASE_INC)/NR_EMIS.EXT\" \
 -WF,-DSUBST_NR_ICBC=\"$(BASE_INC)/NR_ICBC.EXT\" \
 -WF,-DSUBST_NR_DIFF=\"$(BASE_INC)/NR_DIFF.EXT\" \
 -WF,-DSUBST_NR_DDEP=\"$(BASE_INC)/NR_DDEP.EXT\" \
 -WF,-DSUBST_NR_DEPV=\"$(BASE_INC)/NR_DEPV.EXT\" \
 -WF,-DSUBST_NR_ADV=\"$(BASE_INC)/NR_ADV.EXT\" \
 -WF,-DSUBST_NR_N2AE=\"$(BASE_INC)/NR_N2AE.EXT\" \
 -WF,-DSUBST_NR_CONC=\"$(BASE_INC)/NR_CONC.EXT\" \
 -WF,-DSUBST_NR_N2AQ=\"$(BASE_INC)/NR_N2AQ.EXT\" \
 -WF,-DSUBST_NR_SCAV=\"$(BASE_INC)/NR_SCAV.EXT\" \
 -WF,-DSUBST_NR_WDEP=\"$(BASE_INC)/NR_WDEP.EXT\" \
 -WF,-DSUBST_TR_SPC=\"$(BASE_INC)/TR_SPC.EXT\" \
 -WF,-DSUBST_TR_EMIS=\"$(BASE_INC)/TR_EMIS.EXT\" \
 -WF,-DSUBST_TR_ICBC=\"$(BASE_INC)/TR_ICBC.EXT\" \
 -WF,-DSUBST_TR_DIFF=\"$(BASE_INC)/TR_DIFF.EXT\" \
 -WF,-DSUBST_TR_DDEP=\"$(BASE_INC)/TR_DDEP.EXT\" \
 -WF,-DSUBST_TR_DEPV=\"$(BASE_INC)/TR_DEPV.EXT\" \
 -WF,-DSUBST_TR_ADV=\"$(BASE_INC)/TR_ADV.EXT\" \
 -WF,-DSUBST_TR_T2AQ=\"$(BASE_INC)/TR_T2AQ.EXT\" \
 -WF,-DSUBST_TR_SCAV=\"$(BASE_INC)/TR_SCAV.EXT\" \
 -WF,-DSUBST_TR_WDEP=\"$(BASE_INC)/TR_WDEP.EXT\" \
 -WF,-DSUBST_PACTL_ID=\"$(BASE_INC)/PA_CTL.EXT\" \
 -WF,-DSUBST_PACMN_ID=\"$(BASE_INC)/PA_CMN.EXT\" \
 -WF,-DSUBST_PADAT_ID=\"$(BASE_INC)/PA_DAT.EXT\" \
 -WF,-DSUBST_MPICH=\"$(MPI_INC)/mpif.h\"

 OBJECTS =\
 VGRD_DEFN.o \
 CGRID_SPCS.o \
 HGRD_DEFN.o \
 GRID_CONF.o \
 PCGRID_DEFN.o \
 STD_CONC.o \
 AVG_CONC.o \
 adjadv.o \
 PMEM_DEFN.o \
 BIOG_EMIS.o \
 BEIS_DEFN.o \
 DEPVVARS.o \
 DEPV_DEFN.o \
 UDTYPES.o \
 STK_EMIS.o \
 STK_PRMS.o \
 PTMAP.o \
 PTBILIN.o \
 PTMET.o \
 PT3D_DEFN.o \
 AERO_EMIS.o \
 EMIS_DEFN.o \
 VDIFF_MAP.o \
 hrdata_mod.o \
 AERO_INFO.o \
 SOA_NEWT.o \
 PAGRD_DEFN.o \
 advstep.o \
 driver.o \
 hcontvel.o \
 hveloc.o \
 sciproc.o \
 wr_aconc.o \
 wr_cgrid.o \
 wr_conc.o \
 par_init.o \
 par_term.o \
 diffmsg.o \
 flcheck.o \
 grdcheck.o \
 initscen.o \
 load_cgrid.o \
 opconc.o \
 setup_logdev.o \
 couple.o \
 ppmv_msmr.o \
 advbc_map.o \
 hppm.o \
 rdbcon.o \
 xadvppm.o \
 yadvppm.o \
 zfdbc.o \
 vcontvel.o \
 vppm.o \
 zadvppm.o \
 deform.o \
 hcdiff3d.o \
 hdiff.o \
 rho_j.o \
 beis3.o \
 cgrid_depv.o \
 checkmem.o \
 chkgrid.o \
 conv_cgrid.o \
 czangle.o \
 delta_zs.o \
 eddyx.o \
 fire_plmris.o \
 gas_depv_map.o \
 getfline.o \
 getparb.o \
 hrno.o \
 m3dry.o \
 matrix.o \
 opddep.o \
 opdepv_diag.o \
 opemis.o \
 openlayout.o \
 opmet.o \
 oppt3d_diag.o \
 opssemis.o \
 parsline.o \
 plmris.o \
 plsprd.o \
 preplm.o \
 rddepv.o \
 rdmet.o \
 ssemis.o \
 tmpbeis.o \
 tri.o \
 ungridb2.o \
 vdiffacm2.o \
 wrdaymsg.o \
 write3_distr.o \
 phot.o \
 hrcalcks.o \
 hrdriver.o \
 hrg1.o \
 hrg2.o \
 hrg3.o \
 hrg4.o \
 hrinit.o \
 hrprodloss.o \
 hrrates.o \
 hrsolver.o \
 aero_driver.o \
 aero_subs.o \
 hetchem.o \
 isocom.o \
 isofwd.o \
 isorev.o \
 opdiam.o \
 opvis.o \
 orgaer5.o \
 aero_depv.o \
 acmcld.o \
 aq_map.o \
 aqchem.o \
 cldproc_acm.o \
 convcld_acm.o \
 getalpha.o \
 hlconst.o \
 indexn.o \
 rescld.o \
 scavwdep.o \
 set_aeconcmin.o \
 pa_init.o \
 pa_irr.o \
 pa_irr_ctl.o \
 pa_mkhdr.o \
 pa_output.o \
 pa_update.o \
 cksummer.o \
 findex.o \
 get_envlist.o \
 lstepf.o \
 subhdomain.o \
 subhfile.o 

.SUFFIXES: .F .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

.c.o:
	$(CC) $(C_FLAGS) -c $<

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

clean:
	rm -f $(OBJECTS) $(MODEL) F*.f *.mod
