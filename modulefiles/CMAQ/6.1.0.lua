setenv ("envir","canned") 
setenv ("aqm_model","CMAQ") 

setenv ("sys","uname sysname")
setenv ("CMAQ","/lfs/h2/emc/physics/noscrub/Jianping.Huang/canned/packages/cmaq.v6.1.0/modulefiles/CMAQ")
setenv ("lname","CMAQ")


local envvar_ver=os.getenv("envvar_ver")
load ("envvar",envvar_ver)

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver")
load ("PrgEnv-intel",PrgEnv_intel_ver)

local craype_ver=os.getenv("craype_ver")
load ("craype",craype_ver)

local intel_ver=os.getenv("intel_ver")
load ("intel",intel_ver)

local cray_mpich_ver=os.getenv("cray_mpich_ver")
load ("cray-mpich",cray_mpich_ver)

local cray_pals_ver=os.getenv("cray_pals_ver")
load ("cray-pals",cray_pals_ver)

local nemsio_ver=os.getenv("nemsio_ver")
load ("nemsio",nemsio_ver)

local w3emc_ver=os.getenv("w3emc_ver")
load ("w3emc",w3emc_ver)

local w3nco_ver=os.getenv("w3nco_ver")
load ("w3nco",w3nco_ver)

local bufr_ver=os.getenv("bufr_ver")
load ("bufr",bufr_ver)

local libpng_ver=os.getenv("libpng_ver")
load ("libpng",libpng_ver)

local bacio_ver=os.getenv("bacio_ver")
load ("bacio",bacio_ver)

local zlib_ver=os.getenv("zlib_ver")
load ("zlib",zlib_ver)

local jasper_ver=os.getenv("jasper_ver")
load ("jasper",jasper_ver)

local libjpeg_ver=os.getenv("libjpeg_ver")
load ("libjpeg",libjpeg_ver)

local netcdf_ver=os.getenv("netcdf_ver")
load ("netcdf",netcdf_ver)

local hdf5_ver=os.getenv("hdf5_ver")
load ("hdf5",hdf5_ver)

local g2_ver=os.getenv("g2_ver")
load ("g2",g2_ver)
