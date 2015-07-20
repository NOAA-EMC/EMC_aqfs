CMD        = aqm_fcst_nmmb_v46

LIB_LOC    = ../../lib
STENEX     = $(LIB_LOC)/sorc/aqm_se-rc4n_2010
STENEX_LIB = $(LIB_LOC)/libaqm_sef90_2010.a
 
PARIO_LIB  = $(LIB_LOC)/libaqm_pario.a

IOAPI_LIB  = $(LIB_LOC)/libaqm_ioapi.a
NETCDFPATH = ${NETCDF}
NETCDF_LIB = -I$(NETCDFPATH)/include ${NETCDF_LDFLAGS_CXX} ${NETCDF_LDFLAGS}

MPI        = /opt/ibmhpc/pe1301/mpich2/intel/include64
FC         = ${COMP_MP} 
CC         = ${C_COMP_MP} 

FSTD       = -fixed -132 -override-limits -mp1 -fp-model precise

# DBG = -g -traceback
DBG        = -g 

# Compiler Flags
F_FLAGS    = $(FSTD) -O3 -assume byterecl $(DBG) -I$(STENEX) -I .

C_FLAGS    = -O3 #-O3
#LINK_FLAGS =
#LINK_FLAGS = -bnoquiet

CPP_FLAGS = \
 -Dparallel \
 -DINTERPB=PINTERPB \
 -DSUBST_MODULES=SE_MODULES \
 -DSUBST_BARRIER=SE_BARRIER \
 -DSUBST_GLOBAL_MAX=SE_GLOBAL_MAX \
 -DSUBST_GLOBAL_SUM=SE_GLOBAL_SUM \
 -DSUBST_GLOBAL_MIN_DATA=SE_GLOBAL_MIN_DATA \
 -DSUBST_LOOP_INDEX=SE_LOOP_INDEX \
 -DSUBST_SUBGRID_INDEX=SE_SUBGRID_INDEX \
 -DSUBST_HI_LO_BND_PE=SE_HI_LO_BND_PE \
 -DSUBST_SUM_CHK=SE_SUM_CHK \
 -DSUBST_COMM=SE_COMM \
 -DSUBST_DATA_COPY=SE_DATA_COPY

LIBRARIES = \
$(STENEX_LIB) \
$(IOAPI_LIB) \
$(NETCDF_LIB) \
$(PARIO_LIB) \
$(IOAPI_LIB)
#

 INCL1 =./
 INCL2 = $(INCL1)
 INCL3 = $(INCL1)
 INCL4 = $(INCL1)

INCLUDES = \
 -DSUBST_PE_COMM=\"$(INCL1)/PE_COMM.EXT\" \
 -DSUBST_FILES_ID=\"$(INCL1)/FILES_CTM.EXT\" \
 -DSUBST_IOPARMS=\"$(INCL1)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(INCL1)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(INCL1)/IODECL3.EXT\" \
 -DSUBST_CONST=\"$(INCL1)/CONST.EXT\" \
 -DSUBST_EMPR_VD=\"$(INCL1)/EMISPRM.vdif.EXT\" \
 -DSUBST_EMPR_CH=\"$(INCL1)/EMISPRM.chem.EXT\" \
 -DSUBST_RXCMMN=\"$(INCL2)/RXCM.EXT\" \
 -DSUBST_RXDATA=\"$(INCL2)/RXDT.EXT\" \
 -DSUBST_GC_SPC=\"$(INCL2)/GC_SPC.EXT\" \
 -DSUBST_GC_EMIS=\"$(INCL2)/GC_EMIS.EXT\" \
 -DSUBST_GC_ICBC=\"$(INCL2)/GC_ICBC.EXT\" \
 -DSUBST_GC_DIFF=\"$(INCL2)/GC_DIFF.EXT\" \
 -DSUBST_GC_DDEP=\"$(INCL2)/GC_DDEP.EXT\" \
 -DSUBST_GC_DEPV=\"$(INCL2)/GC_DEPV.EXT\" \
 -DSUBST_GC_ADV=\"$(INCL2)/GC_ADV.EXT\" \
 -DSUBST_GC_CONC=\"$(INCL2)/GC_CONC.EXT\" \
 -DSUBST_GC_G2AE=\"$(INCL2)/GC_G2AE.EXT\" \
 -DSUBST_GC_G2AQ=\"$(INCL2)/GC_G2AQ.EXT\" \
 -DSUBST_GC_SCAV=\"$(INCL2)/GC_SCAV.EXT\" \
 -DSUBST_GC_WDEP=\"$(INCL2)/GC_WDEP.EXT\" \
 -DSUBST_AE_SPC=\"$(INCL2)/AE_SPC.EXT\" \
 -DSUBST_AE_EMIS=\"$(INCL2)/AE_EMIS.EXT\" \
 -DSUBST_AE_ICBC=\"$(INCL2)/AE_ICBC.EXT\" \
 -DSUBST_AE_DIFF=\"$(INCL2)/AE_DIFF.EXT\" \
 -DSUBST_AE_DDEP=\"$(INCL2)/AE_DDEP.EXT\" \
 -DSUBST_AE_DEPV=\"$(INCL2)/AE_DEPV.EXT\" \
 -DSUBST_AE_ADV=\"$(INCL2)/AE_ADV.EXT\" \
 -DSUBST_AE_CONC=\"$(INCL2)/AE_CONC.EXT\" \
 -DSUBST_AE_A2AQ=\"$(INCL2)/AE_A2AQ.EXT\" \
 -DSUBST_AE_SCAV=\"$(INCL2)/AE_SCAV.EXT\" \
 -DSUBST_AE_WDEP=\"$(INCL2)/AE_WDEP.EXT\" \
 -DSUBST_NR_SPC=\"$(INCL2)/NR_SPC.EXT\" \
 -DSUBST_NR_EMIS=\"$(INCL2)/NR_EMIS.EXT\" \
 -DSUBST_NR_ICBC=\"$(INCL2)/NR_ICBC.EXT\" \
 -DSUBST_NR_DIFF=\"$(INCL2)/NR_DIFF.EXT\" \
 -DSUBST_NR_DDEP=\"$(INCL2)/NR_DDEP.EXT\" \
 -DSUBST_NR_DEPV=\"$(INCL2)/NR_DEPV.EXT\" \
 -DSUBST_NR_ADV=\"$(INCL2)/NR_ADV.EXT\" \
 -DSUBST_NR_N2AE=\"$(INCL2)/NR_N2AE.EXT\" \
 -DSUBST_NR_N2AQ=\"$(INCL2)/NR_N2AQ.EXT\" \
 -DSUBST_NR_SCAV=\"$(INCL2)/NR_SCAV.EXT\" \
 -DSUBST_NR_WDEP=\"$(INCL2)/NR_WDEP.EXT\" \
 -DSUBST_TR_SPC=\"$(INCL3)/TR_SPC.EXT\" \
 -DSUBST_TR_EMIS=\"$(INCL3)/TR_EMIS.EXT\" \
 -DSUBST_TR_ICBC=\"$(INCL3)/TR_ICBC.EXT\" \
 -DSUBST_TR_DIFF=\"$(INCL3)/TR_DIFF.EXT\" \
 -DSUBST_TR_DDEP=\"$(INCL3)/TR_DDEP.EXT\" \
 -DSUBST_TR_DEPV=\"$(INCL3)/TR_DEPV.EXT\" \
 -DSUBST_TR_ADV=\"$(INCL3)/TR_ADV.EXT\" \
 -DSUBST_TR_T2AQ=\"$(INCL3)/TR_T2AQ.EXT\" \
 -DSUBST_TR_SCAV=\"$(INCL3)/TR_SCAV.EXT\" \
 -DSUBST_TR_WDEP=\"$(INCL3)/TR_WDEP.EXT\" \
 -DSUBST_PACTL_ID=\"$(INCL4)/PA_CTL.EXT\" \
 -DSUBST_PACMN_ID=\"$(INCL4)/PA_CMN.EXT\" \
 -DSUBST_PADAT_ID=\"$(INCL4)/PA_DAT.EXT\" \
 -DSUBST_MPICH=\"$(MPI)/mpif.h\"

F90_MODULES = \
 VGRD_DEFN.o \
 CGRID_SPCS.o \
 HGRD_DEFN.o \
 GRID_CONF.o \
 cgrid_defn.o \
 ddep_defn.o \
 wdep_defn.o \
 aconc_defn.o \
 vis_defn.o \
 wvel_defn.o \
 AERO_EMIS.o \
 AERO_INFO_AE4.o \
 SOA_NEWT.o \
 mpim.o

DRIVER = \
 driver.o \
 ctk.o \
 advstep.o \
 sciproc_yamo.o \
 collect.o \
 assemble.o

INIT = \
 initscen.o \
 load_cgrid.o \
 opconc_yamo.o \
 opaconc.o

COUPLE = \
 couple.o

#opssemis.o \#

VDIFF_BASE = \
 eddyread.o \
 opddep.o \
 opemis.o \
 opmet.o \
 rddepv.o \
 rdemis.o \
 ssemis.o \
 rdmet.o \
 tridiag.o \
 vdiff_map.o \
 conv_cgrid.o

VDIFF = \
 vdiffacm2.o \
 eddyx.o \
 matrix.o \
 tri.o \
 $(VDIFF_BASE)

#DIFF = \#
#vdiffim.o \#
#edyintb.o \#
#$(VDIFF_BASE)

ADV_BASE = \
 hcontvel.o \
 hadvyppm.o \
 rdbcon.o \
 advbc_map.o \
 zfdbc.o \
 zadvyamo.o

#DV = \#
#hppm.o \#
#x_ppm.o \#
#x_yamo.o \#
#y_ppm.o \#
#y_yamo.o \#
#$(ADV_BASE)

ADV = \
 hppm_swap.o \
 x_ppm_swap.o \
 x_yamo_swap.o \
 y_ppm_swap.o \
 y_yamo_swap.o \
 $(ADV_BASE)

HDIFF = \
 hdiff.o \
 rho_j.o \
 hcdiff3d.o \
 deform.o

CLOUD = \
 cldproc_acm.o \
 convcld_acm.o \
 acmcld.o \
 set_aeconcmin.o \
 rescld.o \
 aq_map.o \
 aqchem.o \
 getalpha.o \
 hlconst.o \
 indexn.o \
 scavwdep.o \
 opwdep.o

CHEM = \
 hrdata_mod.o \
 hrdriver.o \
 hrinit.o \
 hrsolver.o \
 hrprodloss.o \
 hrrates.o \
 hrcalcks.o \
 hrg1.o \
 hrg2.o \
 hrg3.o \
 hrg4.o \
 phot_radatt.o

AERO = \
 aero_depv.o \
 aero_driver.o \
 aero_subs_nohet_n2o5.o \
 isocom.o \
 isofwd.o \
 isorev.o \
 opvis.o

UTIL = \
 cksummer.o \
 findex.o \
 get_envlist.o \
 subhfile.o \
 lstepf.o \
 setup_logdev.o

OBJECTS = $(F90_MODULES) $(DRIVER) $(INIT) $(COUPLE) $(ADV) $(HDIFF) \
          $(VDIFF) $(CHEM) $(AERO) $(CLOUD) $(PROCAN) $(UTIL)

.SUFFIXES: .F .f .c

$(CMD) : $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o ../../exec/$@

.c.o:
	$(CC) $(C_FLAGS) -c $<

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

clean:
	/bin/rm -f $(OBJECTS) ../../exec/$(CMD) F*.f *.mod
