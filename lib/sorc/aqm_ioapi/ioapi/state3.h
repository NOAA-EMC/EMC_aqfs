
/********************************************************************
C  INCLUDE FILE  state3.h
C
C  EDSS/Models-3 I/O API -- Version 3
C
C       Copyright (C) 2003 Baron Advanced Meteorological Systems, LLC (BAMS)
C       Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C       See file "LGPL.txt" for conditions of use.
C
C  PRIVATE !!  I/O API INTERNAL USE ONLY !!
C
C  DO NOT EDIT !!
C
C       The EDSS/Models-3 I/O API depends in an essential manner
C       upon the contents of this INCLUDE file.  ANY CHANGES are
C       likely to result in very obscure, difficult-to-diagnose
C       bugs caused by an inconsistency between standard "libioapi.a"
C       object-libraries and whatever code is compiled with the
C       resulting modified INCLUDE-file.
C
C       By making any changes to this INCLUDE file, the user
C       explicitly agrees that in the case any assistance is 
C       required of MCNC or of the I/O API author, CARLIE J. COATS, JR.
C       HE AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE BAMS
C       AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
C       RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
C       REQUIRED.
C
C  CONTAINS:  
C
C       Data Structures and constants for the Fortran COMMONs from
C       include file STATE3.EXT, for I/O API Version 3
C
C  DEPENDENT UPON:
C
C       Inlcude files "parms3.h", "iodecl3.h"
C       consistency with FORTRAN include-files PARMS3.EXT, STATE3.EXT
C
C  REVISION HISTORY:
C
C       prototype 10/2003 by CJC
C
************************************************************************/

        /* "parms3.h" defines FLDMN and Fortran-type typedefs */

#include   "parms3.h"

#ifndef    IO3_STATE_DEFINED
#define    IO3_STATE_DEFINED

#if FLDMN

#define BSTATE3    bstate3_
#define CSTATE3    cstate3_

#elif defined(__hpux) || defined(_AIX)

#define BSTATE3    bstate3
#define CSTATE3    cstate3

#else

#error  "Error compiling state3.h:  unsupported architecture"

#endif


typedef struct{
              double p_alp[ MXFILE3 ] ;
              double p_bet[ MXFILE3 ] ;
              double p_gam[ MXFILE3 ] ;
              double xcent[ MXFILE3 ] ;
              double ycent[ MXFILE3 ] ;
              double xorig[ MXFILE3 ] ;
              double yorig[ MXFILE3 ] ;
              double xcell[ MXFILE3 ] ;
              double ycell[ MXFILE3 ] ;
              FINT   finit ;
              FINT   count ;
              FINT   curdate ;
              FINT   curtime ;
              FINT   logdev ;
              FINT   cdfid[ MXFILE3 ] ;
              FINT   ftype[ MXFILE3 ] ;
              FINT   sdate[ MXFILE3 ] ;
              FINT   stime[ MXFILE3 ] ;
              FINT   tstep[ MXFILE3 ] ;
              FINT   mxrec[ MXFILE3 ] ;
              FINT   nvars[ MXFILE3 ] ;
              FINT   nlays[ MXFILE3 ] ;
              FINT   nrows[ MXFILE3 ] ;
              FINT   ncols[ MXFILE3 ] ;
              FINT   nthik[ MXFILE3 ] ;
              FINT   tindx[ MXFILE3 ] ;
              FINT   nindx[ MXFILE3 ] ;
              FINT   sindx[ MXFILE3 ] ;
              FINT   lindx[ MXFILE3 ] ;
              FINT   wcndx[ MXFILE3 ] ;
              FINT   wrndx[ MXFILE3 ] ;
              FINT   xindx[ MXFILE3 ] ;
              FINT   yindx[ MXFILE3 ] ;
              FINT   zindx[ MXFILE3 ] ;
              FINT   dxndx[ MXFILE3 ] ;
              FINT   dyndx[ MXFILE3 ] ;
              FINT   vindx[ MXFILE3 ][ MXVARS3 ] ;
              FINT   gdtyp[ MXFILE3 ] ;
              FINT   volat[ MXFILE3 ] ;
              FINT   ronly[ MXFILE3 ] ;
              FINT   bsize[ MXFILE3 ] ;
              FINT   ldate[ MXFILE3 ][ MXVARS3 ] ;
              FINT   ltime[ MXFILE3 ][ MXVARS3 ] ;
              FINT   ndate[ MXFILE3 ][ MXVARS3 ] ;
              FINT   ntime[ MXFILE3 ][ MXVARS3 ] ;
              FINT   ilast[ MXFILE3 ][ MXVARS3 ] ;
              FINT   vtype[ MXFILE3 ][ MXVARS3 ] ;
              FINT   ilcnt ;
              FINT   nlist[ MXFILE3 ] ;
              FINT   ifrst[ MXFILE3 ] ;
              FINT   ilist[ MXFILE3 ] ;
              FINT   begrc[ MXFILE3 ] ;
              FINT   endrc[ MXFILE3 ] ;
              FINT   stout ;
              }   IOAPI_BSTATE3 ;      /** for Fortran COMMON BSTATE3 **/

typedef struct{
              M3Line  execn ;
              M3Line  versn ;
              M3Line  sdesc[ MXDESC3 ] ;
              M3Name  flist[ MXFILE3 ] ;
              M3Name  gdnam[ MXFILE3 ] ;
              M3Name  vlist[ MXFILE3 ][ MXVARS3 ] ;
              }  IOAPI_CSTATE3 ;                      /** for Fortran COMMON CSTATE3 **/



#endif    /** if not BINIO3_DEFINED **/

/*****************   END   parms3.h   **********************************/


