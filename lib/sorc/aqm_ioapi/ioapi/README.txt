
                        THE EDSS/MODELS-3
        INPUT/OUTPUT APPLICATIONS PROGRAMMING INTERFACE


I/O API DOCUMENTS:
 
    The EDSS Documentation for the EDSS/Models-3 I/O API is available
    only as HTML -- it was designed from the start as a heavily
    cross-linked hyperdocument, and is not available in linear
    dead-tree document-forms such as PDF. The EDSS Documentation for
    the EDSS/Models-3 I/O API is copyright (C) 1992-2002 MCNC and
    Carlie J. Coats Jr, and copyright (C) 2003 Baron Advanced
    Meteorological Systems.  It may be found at URL
    
        http://www.emc.mcnc.org/products/ioapi/index.html

CURRENT VERSION AVAILABILITY

    The current version of the I/O API is Version 2.2.  It is available
    in source code form from URL

        http://www.emc.mcnc.org/products/ioapi/AVAIL.html

    New features of this release are documented at URL

        http://www.emc.mcnc.org/products/ioapi/NEWSTUFF.html


I/O API INSTALLATION

    The build-process for the I/O API uses the standard UNIX
    "make" command, and is driven by compiler/linker-compatibility
    considerations implemented in terms of "Makeinclude" files
    and an environment variable "BIN" that characterizes this
    compatibility type.  "Makeinclude.$BIN" files are available
    for the following platforms (and also for some debug- and other
    variants, denoted by dbg i8, and r8 suffixes):

        * AIX (32-bit, with default name-mangling rules)
        * AIX_ (32-bit, with Feldmanish name-mangling rules)
        * HP-UX using f90
        * IRIX5 "old-32" binary object-format, using f77
        * IRIX5f90 "old-32" binary object-format, using f90
        * IRIX6 (same as IRIX6n32)
        * IRIX64 "-64" binary type, using f77
        * IRIX64f90 "-64" binary type, using f90
        * IRIX6n32 "-n32" binary type
        * IRIX6n32f90 "-n32" binary type, using f90
        * IRIX6n32f90dbg "-n32" binary type, using f90, compiling for debug
        * Linux2_alpha using GNU gcc and Compaq fort
        * Linux2_ia64 using gcc and g77
        * Linux2_ia64eifc using the Intel efc and ecc
        * Linux2_x86 using gcc and g77
        * Linux2_x86ifc using the Intel ifc and icc
        * Linux2_x86lf95 using the Lahey-Fujitsu f95 and GNU gcc
        * Linux2_x86pg using the Portland Group f90 and cc
        * Alpha OSF1 using Compaq f90 and cc
        * SunOS5 using f77
        * SunOS5f90 using f90
        * cray UNICOS10/F90

    From these examples and from a knowledge of the compiler user
    manual, it should be relatively easy to build Makeinclude files for
    most UNIX or UNIX-like platforms and compilers.  Instructions for
    building the I/O API library libioapi_v2.1.a  and the I/O API tool
    executable programs are as follows:



       1. Download the gzipped tar-file ioapi_22.tar.gz. It contains
          directories "ioapi_22" for the I/O API library source code,
          and "ioapi_22/Tools" for the related tool programs.

       2. cd to the directory under which you wish to build the
          I/O API. gunzip and untar the "ioapi_22.tar.gz" (with
          Gnu tar,

              tar xvfz ioapi_v2.1.tar.gz

          does unzip-untar all in one step).

       3. setenv BIN <machinetype> where <machinetype> matches the
          extension on one of the "Makeinclude.*" (writing your own
          Makeinclude if yours is not one of the supported systems).
          The usual pattern for generating BIN is

              setenv BIN `uname -s``uname -r | cut -d. -f1` 

          although there are exceptions where more work is needed for
          Cray, SGI and Linux systems, and F90 or DEBUG compiles.

       4. There are three "Makefile"s:  "Makefile", "Makefile.cpl",
          and "Makefile.nocpl".  The default  "Makefile" is the same
          as "Makefile.cpl", and builds the library with Coupling Mode
          enabled.  To build with Coupling Mode disabled, copy
          "Makefile.nocpl" to "Makefile".

       5. The default directory for both executables and object
          libraries is in directory "../$BIN" relative to the source
          code directories for the I/O API and tools. Edit the
          "Makefile" to put "SRCDIR" "OBJDIR" wherever you want it
          (if you want somewhere other than the default "./$BIN"
          location).

          NOTE 1:  Different compilers generate linker-visible object
          names in different ways (some with multiple options...).
          It is important that all of the compiles (including those
          for netCDF, PVM, and other libraries) for an entire
          executable program use the same scheme; this is controlled by
          various parts of the ARCHFLAGS variable in the
          "Makeinclude.$BIN" files.

          NOTE 2:  By default on most systems, OpenMP parallelism is
          enabled; see the OMPFLAGS variable in "Makeinclude.$BIN".
          The I/O API does not have parallel sections of its own;
          however, enabling OpenMP does allow the activation of
          critical sections allowing the I/O&nbsp;API to be
          thread-safe for OpenMP-parallel programs (like the
          MAQSIP-RT air quality model, the WRF or MCPL-enabled MM5
          meteorology models, research versions of SMOKE, and others.

       6. In the I/O API library source directory "ioapi_22", type
          "make" to build the object library. The current build process
          will generate "$OBJDIR/libioapi.a"; use "mv" to change it to
          "libm3io.a", if desired.

       7. If necessary, get netCDF and build "libnetcdf.a"; if you're
          building with Coupling Mode active, do the same for PVM.
       
       8. Copy or link ("ln -s ...")  the "libnetcdf.a" (and "libpvm3.a"
          if you built it) to your OBJDIR.

       9. In the I/O API tool source directory ioapi_tools, type either
          "make" (if you have a F90-compliant Fortran compiler), or
          "make f77" (if you don't) to build the tool-program
          executables (adjusting the "LIB" make-variable if you
          renamed "$OBJDIR/libioapi.a" in the preceeding steps). Note
          that there are a number of these programs that do require
          F90:
          dayagg", "ginterp", "m3cple", "m3agmax", "m3agmask",
          "m3combo", "m3merge, mtxblend", "mtxbuild", "mtxcalc",
          "mtxcple", "presterp", "and selmrg2d".

    


