
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/cloud/cloud_acm/cldproc_acm.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CLDPROC ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION:  RADM/ACM and Resolved cloud process driver
C
C  PRECONDITIONS REQUIRED:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C  REVISION  HISTORY:
C      Date   Who             What
C    -------- ---             -----------------------------------------
C     11/21/00 J. Young       enable appending timesteps to wetdep1 and wetdep2
C     3/01/98 S.Roselle       modified to output 2 wet deposition files
C     8/12/97 S.Roselle       added conversion of cgrid units to mol/mol units
C     6/14/94 Dongming Hwang  configuration management prototype
C     Dec 00  J. Young        move CGRID_MAP into f90 module
C     Sep 01  J. Young        Dyn Alloc - Use HGRD_DEFN
C     Jan 05  J. Young        dyn alloc - establish both horizontal & vertical
C                             domain specifications in one module
C     May 05  J. Pleim        Replaced RADMcld with RADMacmcld
C     6/08/05 S.Roselle       added new cloud diagnostic variables
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE WDEP_DEFN

      USE SE_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

C...........INCLUDES and their descriptions

      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/PARMS3.EXT"          ! I/O parameters definitions
      INCLUDE "/meso/save/wx20dw/tools/ioapi_3/ioapi/fixed_src/FDESC3.EXT"          ! file header data structure

!.........................................................................
! Version "@(#)$Header$"
!    EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
!    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
!    See file "LGPL.txt" for conditions of use.
!....................................................................
!  INCLUDE FILE  IODECL3.EXT
!
!
!  DO NOT EDIT !!
!
!       The EDSS/Models-3 I/O API depends in an essential manner
!       upon the contents of this INCLUDE file.  ANY CHANGES are
!       likely to result in very obscure, difficult-to-diagnose
!       bugs caused by an inconsistency between standard "libioapi.a"
!       object-libraries and whatever code is compiled with the
!       resulting modified INCLUDE-file.
!
!       By making any changes to this INCLUDE file, the user
!       explicitly agrees that in the case any assistance is 
!       required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
!       as a result of such changes, THE USER AND/OR HIS PROJECT OR
!       CONTRACT AGREES TO REIMBURSE MCNC AND/OR THE I/O API AUTHOR,
!       CARLIE J. COATS, JR., AT A RATE TRIPLE THE NORMAL CONTRACT
!       RATE FOR THE SERVICES REQUIRED.
!
!  CONTAINS:  declarations and usage comments for the Models-3 (M3)
!             Interprocess Communication Applications Programming
!             Interface (API)
!
!  DEPENDENT UPON:  consistency with the API itself.
!
!  RELATED FILES:  PARM3.EXT, FDESC3.EXT
!
!  REVISION HISTORY:
!       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental
!       Programs
!
!       Modified  2/2002 by CJC:  updated dates, license, compatibility
!       with both free and fixed Fortran 9x source forms
!
!....................................................................

        LOGICAL         CHECK3  !  is JDATE:JTIME available for FNAME?
        LOGICAL         CLOSE3  !  close FNAME
        LOGICAL         DESC3   !  Puts M3 file descriptions into FDESC3.EXT
        LOGICAL         FILCHK3 ! check file type and dimensions
        INTEGER         INIT3   !  Initializes M3 API and returns unit for log
        LOGICAL         SHUT3   !  Shuts down API
        LOGICAL         OPEN3   !  opens an M3 file
        LOGICAL         READ3   !  read M3 file for variable,layer,timestep
        LOGICAL         WRITE3  !  write timestep to M3 file
        LOGICAL         XTRACT3 !  extract window from timestep in a M3 file
        LOGICAL         INTERP3 !  do time interpolation from a M3 file
        LOGICAL         DDTVAR3 !  do time derivative from M3 file

        LOGICAL         INTERPX !  time interpolation from a window
                                !  extraction from an M3 gridded file
!!        LOGICAL      PINTERPB !  1 time interpolation from an
                                !  M3 boundary file

        LOGICAL         INQATT3 !  inquire attributes in M3 file
        LOGICAL         RDATT3  !  read numeric attributes by name from M3 file
        LOGICAL         WRATT3  !  add new numeric attributes "
        LOGICAL         RDATTC  !  read CHAR attributes       "
        LOGICAL         WRATTC  !  add new CHAR attributes    "

        LOGICAL         SYNC3   !  flushes file to disk, etc.

        EXTERNAL        CHECK3 , CLOSE3,  DESC3  , FILCHK3, INIT3  ,
     &                  SHUT3  , OPEN3  , READ3  , WRITE3 , XTRACT3,
     &                  INTERP3, DDTVAR3, INQATT3, RDATT3 , WRATT3 ,
     &                  RDATTC , WRATTC,  SYNC3,   INTERPX ! , PINTERPB

!.......................................................................
!..................  API FUNCTION USAGE AND EXAMPLES  ..................
!.......
!.......   In the examples below, names (FILENAME, PROGNAME, VARNAME)
!.......   should be CHARACTER*16, STATUS and RDFLAG are LOGICAL, dates
!.......   are INTEGER, coding the Julian date as YYYYDDD, times are
!.......   INTEGER, coding the time as HHMMSS, and LOGDEV is the FORTRAN
!.......   INTEGER unit number for the program's log file; and layer,
!.......   row, and column specifications use INTEGER FORTRAN array
!.......   index conventions (in particular, they are based at 1, not
!.......   based at 0, as in C).
!.......   Parameter values for "ALL...", for grid and file type IDs,
!.......   and for API dimensioning values are given in PARMS3.EXT;
!.......   file descriptions are passed via commons BDESC3 and CDESC3
!.......   in file FDESC3.EXT.
!.......
!.......   CHECK3():  check whether timestep JDATE:JTIME is available 
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CHECK3 ( FILENAME, VNAME, JDATE, JTIME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (data-record not available in file FNAME)
!.......       END IF
!.......
!.......   CLOSE3():  check whether timestep JDATE:JTIME is available 
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CLOSE3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... could not flush file to disk successfully,
!.......           or else file not currently open.
!.......       END IF
!.......
!.......   DESC3():   return description of file FILENAME to the user
!.......   in commons BDESC3 and CDESC3, file FDESC3.EXT.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DESC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened)
!.......       END IF
!.......       ...
!.......       (Now common FDESC3 (file FDESC3.EXT) contains the descriptive
!.......       information for this file.)
!.......
!.......   FILCHK3():   check whether file type and dimensions for file 
!.......   FILENAME match the type and dimensions supplied by the user.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = FILCHK3 ( FILENAME, FTYPE, NCOLS, NROWS, NLAYS, NTHIK )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file type and dimensions do not match
!.......                the supplied FTYPE, NCOLS, NROWS, NLAYS, NTHIK)
!.......       END IF
!.......       ...
!.......
!.......   INIT3():  set up the M3 API, open the program's log file, and
!.......   return the unit FORTRAN number for log file.  May be called
!.......   multiple times (in which case, it always returns the log-file's
!.......   unit number).  Note that block data INITBLK3.FOR must also be
!.......   linked in.
!.......   FORTRAN usage is:
!.......
!.......       LOGDEV = INIT3 ( )
!.......       IF ( LOGDEV .LT. 0 ) THEN
!.......           ... (can't proceed:  probably can't open the log.
!.......                Stop the program)
!.......       END IF
!.......
!.......   OPEN3():  open file FILENAME from program PROGNAME, with
!.......   requested read-write/old-new status.  For files opened for WRITE,
!.......   record program-name and other history info in their headers.
!.......   May be called multiple times for the same file (in which case,
!.......   it returns true unless the request is for READ-WRITE status
!.......   for a file already opened READ-ONLY).  Legal statuses are:
!.......   FSREAD3: "old read-only"
!.......   FSRDWR3: "old read-write"
!.......   FSNEW3:  "new (read-write)"
!.......   FSUNKN3: "unknown (read_write)"
!.......   FORTRAN usage is:
!.......
!.......       STATUS = OPEN3 ( FILENAME, FSTATUS, PROGNAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (process the error)
!.......       END IF
!.......
!.......   READ3():  read data from FILENAME for timestep JDATE:JTIME,
!.......   variable VNAME, layer LAY, into location  ARRAY.
!.......   If VNAME==ALLVARS3=='ALL         ', reads all variables;
!.......   if LAY==ALLAYS3==-1, reads all layers.
!.......   Offers random access to the data by filename, date&time, variable,
!.......   and layer.  For DICTIONARY files, logical name for file being
!.......   requested maps into the VNAME argument.  For time-independent
!.......   files (including DICTIONARY files), JDATE and JTIME are ignored.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = READ3 ( FILENAME, VNAME, LAY, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (read failed -- process this error.)
!.......       END IF
!.......
!.......   SHUT3():  Flushes and closes down all M3 files currently open.
!.......   Must be called before program termination; if it returns FALSE
!.......   the run must be considered suspect.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SHUT3 ( )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (Flush of files to disk probably didn't work;
!.......                look at netCDF error messages)
!.......       END IF
!.......
!.......   WRITE3():  write data from ARRAY to file FILENAME for timestep
!.......   JDATE:JTIME.  For GRIDDED, BUONDARY, and CUSTOM files, VNAME
!.......   must be a variable found in the file, or else ALLVARS3=='ALL'
!.......   to write all variables from ARRAY.  For other file types,
!.......   VNAME _must_ be ALLVARS3.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = WRITE3 ( FILENAME, VNAME, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (write failed -- process this error.)
!.......       END IF
!.......
!.......   XTRACT3():  read/extract gridded data into location  ARRAY
!.......   from FILENAME for time step JDATE:JTIME, variable VNAME
!.......   and the data window defined by
!.......       LOLAY  <=  layer   <=  HILAY,
!.......       LOROW  <=  row     <=  HIROW,
!.......       LOCOL  <=  column  <=  HICOL
!.......   FORTRAN usage is:
!.......
!.......       STATUS = XTRACT3 ( FILENAME, VNAME,
!.......   &                      LOLAY, HILAY,
!.......   &                      LOROW, HIROW,
!.......   &                      LOCOL, HICOL,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (extract failed -- process this error.)
!.......       END IF
!.......
!.......   INTERP3():  read/interpolate gridded, boundary, or custom data 
!.......   into location  ARRAY from FILENAME for time JDATE:JTIME, variable 
!.......   VNAME, and all layers.  Note use of ASIZE = transaction size =
!.......   size of ARRAY, for error-checking.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERPX ( FILENAME, VNAME, CALLER, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   INTERPX():  read/interpolate/window gridded, boundary, or custom
!.......   data into location  ARRAY from FILENAME for time JDATE:JTIME, 
!.......   variable VNAME, and all layers.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERPX ( FILENAME, VNAME, CALLER, 
!.......   &                      COL0, COL1, ROW0, ROW1, LAY0, LAY1,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (windowed interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   DDTVAR3():  read and calculate mean time derivative (per second) 
!.......   for gridded, boundary, or custom data.  Put result into location  
!.......   ARRAY from FILENAME for time JDATE:JTIME, variable VNAME, and all 
!.......   layers.  Note use of ASIZE = transaction size = size of ARRAY, 
!.......   for error-checking.  Note  d/dt( time-independent )==0.0
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DDTVAR3 ( FILENAME, VNAME, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   INQATT():  inquire how many attributes there are for a
!.......   particular file and variable (or for the file globally,
!.......   if the variable-name ALLVAR3 is used)), and what the 
!.......   names, types, and array-dimensions of these attributes are.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INQATT3( FNAME, VNAME, MXATTS, 
!.......   &                     NATTS, ANAMES, ATYPES, ASIZES )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!....... 
!.......   RDATT3():  Reads an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX,
!.......   &                    ASIZE, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3, 
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATT3( FNAME, VNAME, 
!.......   &                     ANAME, ATYPE, AMAX, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......   
!.......   RDATTC():  Reads a CHARACTER string attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes a CHARACTER string attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3, 
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   SYNC3():   Synchronize FILENAME with disk (flush output;
!.......   re-read header and invalidate data-buffers for input.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SYNC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened, or disk-synch failed)
!.......       END IF
!.......       ...
!.......
!................   end   IODECL3.EXT   ....................................

!     INCLUDE SUBST_HGRD_ID          ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID          ! vertical dimensioning parameters
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_SPC.EXT"           ! gas chemistry species table
!     INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_WDEP.EXT"          ! wet deposition table for gases
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_SPC.EXT"           ! aerosol species table
!     INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/AE_WDEP.EXT"          ! wet deposition table for aerosols
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_SPC.EXT"           ! non-reactive species table
!     INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/NR_WDEP.EXT"          ! wet deposition table for non-reactives
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_SPC.EXT"           ! tracer species table
!     INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/TR_WDEP.EXT"          ! wet deposition table for tracers

      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/CONST.EXT"            ! constants
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/FILES_CTM.EXT"         ! file name parameters

!     INCLUDE SUBST_COORD_ID         ! coordinate and domain definitions (req IOPARMS)

      CHARACTER( 120 ) :: XMSG = ' ' ! exit status message string

C...........PARAMETERS and their descriptions:

C # of wet deposition species
!     INTEGER, PARAMETER :: N_SPC_WDEP = N_GC_WDEP
!    &                                 + N_AE_WDEP
!    &                                 + N_NR_WDEP
!    &                                 + N_TR_WDEP
!     INTEGER, PARAMETER :: N_SPC_WDEPD = N_SPC_WDEP + 1

!     REAL, PARAMETER :: AVO = 6.0221367E+23 ! Avogadro's Constant [ #/mol ]

      REAL, PARAMETER :: CNV1 = MWAIR * 1.0E-9
      REAL, PARAMETER :: CNV1I = 1.0 / CNV1
      REAL, PARAMETER :: CNV2 = MWAIR * 1.0E-3
      REAL, PARAMETER :: CNV2I = 1.0 / CNV2
!     REAL, PARAMETER :: CNV3 = MWAIR * 1.0E+3 / AVO  ! -> ppmV
      REAL, PARAMETER :: CNV3 = CNV2 / AVO            ! -> mol/mol
      REAL, PARAMETER :: CNV3I = 1.0 / CNV3

C...........ARGUMENTS and their descriptions

!     REAL          CGRID( NCOLS, NROWS, NLAYS, * )  ! concentrations
!     REAL       :: CGRID( :,:,:,: )                 ! concentrations
!     REAL, POINTER :: CGRID( :,:,:,: )                 ! concentrations
      INTEGER       JDATE            ! current model date, coded YYYYDDD
      INTEGER       JTIME            ! current model time, coded HHMMSS
      INTEGER       TSTEP( 2 )       ! model time step, coded HHMMSS

C...........Local Variables

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

!     LOGICAL, SAVE :: CLD_DIAG          ! flag to output cloud diagnostic files

      CHARACTER( 16 ), SAVE :: PNAME = 'CLDPROC' ! driver program name
      CHARACTER( 16 ) :: VNAME            ! input variable name list

      INTEGER       COL              ! column subscript indices
      INTEGER       FINI             ! ending position
      INTEGER       ICLDTYPE         ! 1: raining, 2: either CNP or PFW
      INTEGER       L                ! loop counter
      INTEGER       LAY              ! layer subscript indices
      INTEGER       MDATE            ! middle of this time step
      INTEGER       MTIME            ! middle of this time step
      INTEGER       NDATE            ! middle of this time step
      INTEGER       NTIME            ! middle of this time step
      INTEGER, SAVE :: NNAE          ! number of #/m3 species
      INTEGER, SAVE :: NQAE          ! number of ug/m3 species
      INTEGER, SAVE :: NSAE          ! number of m2/m3 species
      INTEGER       ROW              ! row subscript indices
      INTEGER       SPC              ! species subscript indices
      INTEGER       STRT             ! starting position
      INTEGER       STATUS           !  ENVINT status
      INTEGER       VAR              ! variable subscript indices
      INTEGER       ALLOCSTAT
!     INTEGER, SAVE :: WDEP_MAP( N_SPC_WDEPD ) ! wet deposition map to CGRID

      INTEGER, SAVE :: LOGDEV           ! output log unit number

      INTEGER, SAVE :: QAE( N_AE_SPCD ) ! CGRID pointer to ug/m3 species
      INTEGER, SAVE :: NAE( N_AE_SPCD ) ! CGRID pointer to #/m3 species
      INTEGER, SAVE :: SAE( N_AE_SPCD ) ! CGRID pointer to m2/m3 species

!     REAL          DENS    ( NCOLS,NROWS,NLAYS ) ! air density (kg/m3)
!     REAL          CONV_DEP( NCOLS,NROWS,N_SPC_WDEP+8 ) ! convective wdep only
!     REAL          TOT_DEP ( NCOLS,NROWS,N_SPC_WDEP+1 ) ! total wdep
      REAL, ALLOCATABLE, SAVE :: DENS    ( :,:,: ) ! air density (kg/m3)
!     REAL, ALLOCATABLE, SAVE :: CONV_DEP( :,:,: ) ! convective wdep only
!     REAL, ALLOCATABLE, SAVE :: TOT_DEP ( :,:,: ) ! total wdep

      REAL FAC                       ! temp conversion factor
      REAL CCMIN

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C...........EXTERNAL FUNCTIONS and their descriptions:

!     LOGICAL, EXTERNAL :: ENVYN
      INTEGER, EXTERNAL :: SETUP_LOGDEV

      INTERFACE
!        SUBROUTINE RESCLD ( CGRID, JDATE, JTIME, TSTEP,
!    &                       N_SPC_WDEP, WDEP_MAP, DEP )
         SUBROUTINE RESCLD ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
!           REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP( 2 )
!           INTEGER, INTENT( IN )  :: N_SPC_WDEP
!           INTEGER, INTENT( IN )  :: WDEP_MAP( : )
!           REAL,    INTENT( OUT ) :: DEP( :,:,: )
         END SUBROUTINE RESCLD
!        SUBROUTINE CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP,
!    &                            N_SPC_WDEP, WDEP_MAP, DEP )
         SUBROUTINE CONVCLD_ACM ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
!           REAL, POINTER          :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP( 2 )
!           INTEGER, INTENT( IN )  :: N_SPC_WDEP
!           INTEGER, INTENT( IN )  :: WDEP_MAP( : )
!           REAL,    INTENT( OUT ) :: DEP( :,:,: )
         END SUBROUTINE CONVCLD_ACM
      END INTERFACE
C-----------------------------------------------------------------------
C   begin body of subroutine  CLDPROC

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.
!       LOGDEV = INIT3()
        LOGDEV = SETUP_LOGDEV()

        WRITE( LOGDEV,'(/5X, A/)' ) 'Using Radm/ACM cloud scheme'

C...Forecast model: won't work if TSTEP(3) .ne. 1 hour (010000), since we're
C...writing the WETDEP file outside of this routine...

        IF ( TSTEP( 1 ) .NE. 010000 ) THEN
           XMSG = 'Output timestep incorrect for this model'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF

        CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

C...check to see if user wants to output extra diagnostic files

!       CLD_DIAG = .FALSE.
!       XMSG = 'Output cloud diagnostic files? (Y/N)'
!       CLD_DIAG = ENVYN( 'CLD_DIAG', XMSG, CLD_DIAG, STATUS )

!       IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) XMSG

!       IF ( STATUS .EQ. 1 ) THEN
!         XMSG = 'Environment variable improperly formatted'
!         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!       ELSE IF ( STATUS .EQ. -1 ) THEN
!         XMSG = 'Environment variable set, but empty ... Using default:'
!         WRITE( LOGDEV, '(5X, A, L9)' ) XMSG, CLD_DIAG
!       ELSE IF ( STATUS .EQ. -2 ) THEN
!         XMSG = 'Environment variable not set ... Using default:'
!         WRITE( LOGDEV, '(5X, A, L9)' ) XMSG, CLD_DIAG
!       END IF

C...Check to make sure that some species in CGRID were specified
C...for output in the wet deposition array, otherwise notify the user
C...and return

        IF ( N_SPC_WDEP .LE. 0 ) THEN

          XMSG = 'No species were specified for wet deposition ' //
     &           'tracking'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )

          XMSG = 'Only cloud diagnostics will be written to the ' //
     &           'WETDEP output file!'
          CALL M3MESG ( XMSG )

        END IF

!       SPC = 0
!       STRT = 1
!       FINI = N_GC_WDEP
!       DO VAR = STRT, FINI
!         SPC = SPC + 1
!         WDEP_MAP( VAR ) = GC_STRT - 1 + GC_WDEP_MAP( SPC )
!       END DO

!       SPC = 0
!       STRT = N_GC_WDEP + 1
!       FINI = N_GC_WDEP + N_AE_WDEP
!       DO VAR = STRT, FINI
!         SPC = SPC + 1
!         WDEP_MAP( VAR ) = AE_STRT - 1 + AE_WDEP_MAP( SPC )
!       END DO

!       SPC = 0
!       STRT = N_GC_WDEP + N_AE_WDEP + 1
!       FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
!       DO VAR = STRT, FINI
!         SPC = SPC + 1
!         WDEP_MAP( VAR ) = NR_STRT - 1 + NR_WDEP_MAP( SPC )
!       END DO

!       SPC = 0
!       STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
!       FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
!       DO VAR = STRT, FINI
!         SPC = SPC + 1
!         WDEP_MAP( VAR ) = TR_STRT - 1 + TR_WDEP_MAP( SPC )
!       END DO

C Try to open existing wet deposition file for update

!       CALL SE_BARRIER

!       IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSRDWR3, PNAME ) ) THEN

!         XMSG = 'Could not open ' // CTM_WET_DEP_1 // ' file for update - '
!    &         // 'try to open new'
!         CALL M3MESG( XMSG )

C...Create wet deposition file:
C...   copy most of header description from
C...   MET_CRO_2D and initialize depositions to zero.

C...open MET_CRO_2D

!         IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
!           XMSG = 'Could not open '// MET_CRO_2D // ' file'
!           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!         END IF

C...get description from the met file

!         IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
!           XMSG = 'Could not get ' // MET_CRO_2D //' file description'
!           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!         END IF

!         NCOLS3D = GL_NCOLS
!         NROWS3D = GL_NROWS

!         XORIG3D = XORIG_GD
!         YORIG3D = YORIG_GD

C...advance to time variable for the output file to the next hour

!         SDATE3D = JDATE
!         STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
!         TSTEP3D = 010000   ! 1-hour timesteps (hhmmss)
!         CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

!         NVARS3D = N_SPC_WDEP + 1

C...define wet deposition output descriptors for gaseous species

!         SPC = 0
!         STRT = 1
!         FINI = N_GC_WDEP
!         DO VAR = STRT, FINI
!           SPC = SPC + 1
!           VTYPE3D ( VAR ) = M3REAL
!           VNAME3D ( VAR ) = GC_WDEP( SPC )
!           UNITS3D ( VAR ) = 'kg/hectare'
!           VDESC3D ( VAR ) = 'hourly wet deposition values'
!         END DO

C...define wet deposition output descriptors for aerosol species

!         SPC = 0
!         STRT = N_GC_WDEP + 1
!         FINI = N_GC_WDEP + N_AE_WDEP
!         DO VAR = STRT, FINI
!           SPC = SPC + 1
!           VTYPE3D ( VAR ) = M3REAL
!           VNAME3D ( VAR ) = AE_WDEP( SPC )
!           IF ( INDEX( AE_WDEP( SPC ), 'NUM' ) .GT. 0 ) THEN
!             UNITS3D ( VAR ) = 'number/hectare'
!           ELSE IF ( INDEX( AE_WDEP( SPC ), 'SRF' ) .GT. 0 ) THEN
!             UNITS3D ( VAR ) = 'm2/hectare'
!           ELSE
!             UNITS3D ( VAR ) = 'kg/hectare'
!           END IF
!           VDESC3D ( VAR ) = 'hourly wet deposition values'
!         END DO

C...define wet deposition output descriptors for non-reactive species

!         SPC = 0
!         STRT = N_GC_WDEP + N_AE_WDEP + 1
!         FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
!         DO VAR = STRT, FINI
!           SPC = SPC + 1
!           VTYPE3D ( VAR ) = M3REAL
!           VNAME3D ( VAR ) = NR_WDEP( SPC )
!           UNITS3D ( VAR ) = 'kg/hectare'
!           VDESC3D ( VAR ) = 'hourly wet deposition values'
!         END DO

C...define wet deposition output descriptors for tracer species

!         SPC = 0
!         STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
!         FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
!         DO VAR = STRT, FINI
!           SPC = SPC + 1
!           VTYPE3D ( VAR ) = M3REAL
!           VNAME3D ( VAR ) = TR_WDEP( SPC )
!           UNITS3D ( VAR ) = 'kg/hectare'
!           VDESC3D ( VAR ) = 'hourly wet deposition values'
!         END DO

C...define output descriptor for hydrogen concentration

!         VAR = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP + 1
!         VTYPE3D( VAR ) = M3REAL
!         VNAME3D( VAR ) = 'HPLUS'
!         UNITS3D( VAR ) = 'kg/hectare'
!         VDESC3D( VAR ) = 'hourly wet deposition values'

C...initialize the file descriptors

!         FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition '
!    &                     // 'from all cloud types'
!         DO L = 2 , MXDESC3
!           FDESC3D( L ) = ' '
!         END DO

!         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

C...try to open the output file new

!         IF ( MYPE .EQ. 0 ) THEN   ! open new
!           IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSNEW3, PNAME ) ) THEN
!             XMSG = 'Could not create '// CTM_WET_DEP_1 // ' file'
!             CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
!           END IF
!         END IF

!       END IF   ! open CTM_WET_DEP_1 read/write

C...open the diagnostic file if user asked for it

!       IF ( CLD_DIAG ) THEN

C...Try to open existing file for update

!         CALL SE_BARRIER

!         IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSRDWR3, PNAME ) ) THEN

!            XMSG = 'Could not open ' // CTM_WET_DEP_2 // ' file for update - '
!    &            // 'try to open new'
!            CALL M3MESG( XMSG )

C...copy most of header description from
C...CTM_WET_DEP_1 and initialize depositions to zero.

C...get description from the other wet dep file

!           IF ( MYPE .EQ. 0 ) THEN
!             IF ( .NOT. DESC3( CTM_WET_DEP_1 ) ) THEN
!               XMSG = 'Could not get ' // CTM_WET_DEP_1 //' file description'
!               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!             END IF

C...advance to time variable for the output file to the next hour

!             SDATE3D = JDATE
!             STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
!             TSTEP3D = 010000   ! 1-hour timesteps (hhmmss)
!             CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

!             NVARS3D = N_SPC_WDEP + 8

C...define output descriptor for pressure at the lifting condensation
C...level (LCL)

!             VAR = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP + 2
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'PLCL'
!             UNITS3D( VAR ) = 'Pa'
!             VDESC3D( VAR ) = 'pressure at lifting condensation level, '
!    &                       // 'or MISSING = -9999.9'

C...define output descriptor for cloud bottom layer number

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'CLOUD_BOTTOM'
!             UNITS3D( VAR ) = 'layer-number'
!             VDESC3D( VAR ) =
!    &              'layer containing bottom of cloud, or MISSING = -9999.9'

C...define output descriptor for precipitating cloud top layer number

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'PR_CLOUDTOP'
!             UNITS3D( VAR ) = 'layer-number'
!             VDESC3D( VAR ) =
!    &              'layer containing top of PR cloud, or MISSING = -9999.9'

C...define output descriptor for non-precipitating cloud top layer number

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'NP_CLOUDTOP'
!             UNITS3D( VAR ) = 'layer-number'
!             VDESC3D( VAR ) =
!    &              'layer containing top of NP cloud, or MISSING = -9999.9'

C...define output descriptor for rain event flag

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'RAIN_FLAG'
!             UNITS3D( VAR ) = '1 or 0'
!             VDESC3D( VAR ) = 'Rain-event flag'
 
C...define output descriptor for precitating cloud fraction

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'PR_CLDFRAC'
!             UNITS3D( VAR ) = ' '
!             VDESC3D( VAR ) = 'PR cloud fraction'

C...define output descriptor for non-precipitating cloud fraction

!             VAR = VAR + 1
!             VTYPE3D( VAR ) = M3REAL
!             VNAME3D( VAR ) = 'NP_CLDFRAC'
!             UNITS3D( VAR ) = ' '
!             VDESC3D( VAR ) = 'NP cloud fraction'

C...initialize the file descriptors

!             FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition '
!    &                         // 'from sub-grid clouds and cloud data'
!             DO L = 2 , MXDESC3
!               FDESC3D( L ) = ' '
!             END DO

C...try to open the diagnostic output file

!             IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSNEW3, PNAME ) ) THEN
!               XMSG = 'Could not create '// CTM_WET_DEP_2 // ' file'
!               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
!             END IF

!           END IF   ! MYPE = 0

!         END IF   ! open CTM_WET_DEP_2 read/write

!       END IF   ! CLD_DIAG

C...create aerosol species pointers to distinguish micro-grams / m**3
C...  # / m**3 (number density), and m**2 / m**3 (surface area) units

        NQAE = 0       ! number of ug/m3 species
        NNAE = 0       ! number of #/m3 species
        NSAE = 0       ! number of m2/m3 species

        DO VAR = 1, N_AE_SPC
          IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'NUM' ) THEN
            NNAE = NNAE + 1
            NAE( NNAE ) = AE_STRT - 1 + VAR
          ELSE IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'SRF' ) THEN
            NSAE = NSAE + 1
            SAE( NSAE ) = AE_STRT - 1 + VAR
          ELSE
            NQAE = NQAE + 1
            QAE( NQAE ) = AE_STRT - 1 + VAR
          END IF
        END DO

C...initialize the deposition array before processing clouds

!       ALLOCATE ( TOT_DEP ( MY_NCOLS,MY_NROWS,N_SPC_WDEP+1 ),
!    &             STAT = ALLOCSTAT )
!       IF ( ALLOCSTAT .NE. 0 ) THEN
!         XMSG = 'Failure allocating TOT_DEP'
!         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!       END IF

!       TOT_DEP = 0.0

!       ALLOCATE ( CONV_DEP( MY_NCOLS,MY_NROWS,N_SPC_WDEP+8 ),
!    &             STAT = ALLOCSTAT )
!       IF ( ALLOCSTAT .NE. 0 ) THEN
!         XMSG = 'Failure allocating CONV_DEP'
!         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!       END IF

!       CONV_DEP = 0.0

        CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                  STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

        ALLOCATE ( DENS( MY_NCOLS,MY_NROWS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating DENS'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      END IF   ! FIRSTIME

      MDATE = JDATE
      MTIME = JTIME

      VNAME = 'DENS'

!     IF ( .NOT. INTERP3( MET_CRO_3D, VNAME, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, DENS ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, DENS ) ) THEN
        XMSG = 'Could not interpolate DENS'
        CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C...convert units on cgrid to molar mixing ratio (by volume)
C...  determine no. of moles of gas per moles of air by volume

      STRT = GC_STRT
      FINI = GC_STRT + N_GC_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6, 1.0E-30 )
            END DO
          END DO
        END DO
      END DO

C...for aerosol mass concentration
C...  convert to moles of aerosol per mole of air by volume

      STRT = 1
      FINI = NQAE
      DO VAR = STRT, FINI
        SPC = QAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              FAC = CNV1 / DENS( COL, ROW, LAY ) / AE_MOLWT( SPC - AE_STRT + 1 )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30 / FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...for aerosol number concentration
C...    convert to # per mole of air by volume

      STRT = 1
      FINI = NNAE
      DO VAR = STRT, FINI
        SPC = NAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
!             CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
!    &                                    * MWAIR / ( 1000.0
!    &                                    * DENS( COL, ROW, LAY ) )
              FAC = CNV3 / DENS( COL, ROW, LAY )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30 / FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...for aerosol surface area
C...    convert to m2 per mole of air by volume

      STRT = 1
      FINI = NSAE
      DO VAR = STRT, FINI
        SPC = SAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              FAC = CNV2 / DENS( COL, ROW, LAY )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30 / FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...determine no. of moles of non-reactive gas per moles of air by volume

      STRT = NR_STRT
      FINI = NR_STRT + N_NR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6, 1.0E-30 )
            END DO
          END DO
        END DO
      END DO

C...determine no. of moles of tracer gas per moles of air by volume

      STRT = TR_STRT
      FINI = TR_STRT + N_TR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6, 1.0E-30 )
            END DO
          END DO
        END DO
      END DO

C...compute cloud effects for the resolved clouds and resolved rainwater

      CALL RESCLD ( JDATE, JTIME, TSTEP )

C...compute cloud effects for convective (subgrid) clouds

      CALL CONVCLD_ACM ( JDATE, JTIME, TSTEP )

C...now check to see if it's time to write the deposition file

!     CALL NEXTIME ( MDATE, MTIME, TSTEP( 2 ) ) ! set mdate:mtime to next tstep
!     NDATE = JDATE
!     NTIME = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
!     CALL NEXTIME ( NDATE, NTIME, 10000 )   ! set Ndate:Ntime to next hour

!     IF ( ( MDATE .EQ. NDATE ) .AND. ( MTIME .GE. NTIME ) ) THEN

!       DO VAR = 1, N_SPC_WDEP + 1
!         DO ROW = 1, MY_NROWS
!           DO COL = 1, MY_NCOLS
!             TOT_DEP( COL, ROW, VAR ) = TOT_DEP ( COL, ROW, VAR )
!    &                                 + CONV_DEP( COL, ROW, VAR )
!           END DO
!         END DO
!       END DO

C...write data to the normal wet deposition file

!       IF ( .NOT. WRITE3( CTM_WET_DEP_1, ALLVAR3, NDATE,
!    &                     NTIME, TOT_DEP ) ) THEN
!         XMSG = 'Could not write ' // CTM_WET_DEP_1 // ' file'
!         CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
!       END IF

!       WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
!    &         'Timestep written to', CTM_WET_DEP_1,
!    &         'for date and time', NDATE, NTIME

C...write data to the diagnostic file if requested by the user

!       IF ( CLD_DIAG ) THEN

!         IF ( .NOT. WRITE3( CTM_WET_DEP_2, ALLVAR3, NDATE,
!    &                       NTIME, CONV_DEP ) ) THEN
!         XMSG = 'Could not write ' // CTM_WET_DEP_2 // ' file'
!           CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
!         END IF

!         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
!    &           'Timestep written to', CTM_WET_DEP_2,
!    &           'for date and time', NDATE, NTIME

!       END IF   ! CLD_DIAG

C...reinitialize deposition array

!       TOT_DEP  = 0.0
!       CONV_DEP = 0.0

!     END IF   ! time to write

C...convert units on cgrid back to original units
C...  convert to ppmV gas

      STRT = GC_STRT
      FINI = GC_STRT + N_GC_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

C...convert to ug/m3 of aerosol mass

      STRT = 1
      FINI = NQAE
      DO VAR = STRT, FINI
        SPC = QAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              FAC = CNV1I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
     &                                    * AE_MOLWT( SPC - AE_STRT + 1 )
            END DO
          END DO
        END DO
      END DO

C...convert to #/m3 of aerosol number

      STRT = 1
      FINI = NNAE
      DO VAR = STRT, FINI
        SPC = NAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              FAC = CNV3I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
            END DO
          END DO
        END DO
      END DO

C...convert to m2/m3 of aerosol surface area

      STRT = 1
      FINI = NSAE
      DO VAR = STRT, FINI
        SPC = SAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              FAC = CNV2I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
            END DO
          END DO
        END DO
      END DO

C...convert to ppmV non-reactive gas

      STRT = NR_STRT
      FINI = NR_STRT + N_NR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

C...convert to ppmV tracer gas

      STRT = TR_STRT
      FINI = TR_STRT + N_TR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

      RETURN

      END
