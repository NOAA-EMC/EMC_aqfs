
/********************************************************************
C  INCLUDE FILE  parms3.h
C
C    VERSION "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/parms3.h,v 1.5 2000/12/13 20:57:18 smith_w Exp $"
C    EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C    See file "LGPL.txt" for conditions of use.
C
C    DO NOT EDIT !!
C
C    The EDSS/Models-3 I/O API depends in an essential manner
C    upon the contents of this INCLUDE file.  ANY CHANGES are
C    likely to result in very obscure, difficult-to-diagnose
C    bugs caused by an inconsistency between standard "libioapi.a"
C    object-libraries and whatever code is compiled with the
C    resulting modified INCLUDE-file.
C
C    By making any changes to this INCLUDE file, the user
C    explicitly agrees that in the case any assistance is 
C    required of MCNC or of the I/O API author, CARLIE J. COATS, JR.
C    HE AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE MCNC
C    AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
C    RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
C    REQUIRED.
C
C  CONTAINS:  dimensioning parameters, standard file-type, grid-type,
C             (etc.) token values for Models-3 I/O System API
C
C  DEPENDENT UPON:  consistency with FORTRAN include-file PARMS3.EXT
C
C  REVISION HISTORY:
C       prototype 3/92 by CJC
C       Modified 12/92 by CJC:  new map projection type STEGRD3.
C       Modified  6/94 by CJC:  I/O API Revisions.
C       Modified 12/96 by CJC:  support for new file types
C	Modified  8/99 by CJC:  FLDMN, Win32 portability enhancements
C
************************************************************************/
#ifndef    PARMS3_DEFINED
#define    PARMS3_DEFINED

/**  REAL8  is for user-customization on those platforms where a 
     Fortran REAL is the same as a C double -- e.g., if you decide 
     to compile with "-r8" on those platforms which support it  **/
     
#if defined(__sgi)    || defined(__sun) || defined(__osf__) || \
    defined(__mips__) || defined(__OPENNT)
#define FLDMN 1
#endif

#if defined(_CRAYMPP) || defined(REAL8)          
   typedef double   FREAL;
#else
   typedef float    FREAL;
#endif


#ifdef __cplusplus
extern "C" {
#endif

                          /*  max number of open files                 */
#define   MXFILE3   (64)  

                          /*  max number of layers per file            */
#define   MXLAYS3  (100)  

                          /*  max number of variables per file         */
#define   MXVARS3  (120)  

                          /*  max number of additional TSRIES3 atts    */
#define   MXATTS3   (20)  

                          /*  max number of description lines          */
#define   MXDESC3   (60)  

                          /*  description line (as FSTR) length        */
#define   MXDLEN3   (80)  

                          /*  name length (as FSTR)                    */
#define   NAMLEN3   (16)  


                          /*  file type value:  cloud-event files      */
#define   KFEVNT3   (-3)  

                          /*  file type value:  study-graph files      */
#define   DGRAPH3   (-2)  

                          /*  file type value:  custom files           */
#define   CUSTOM3   (-1)  

                          /*  file type value:  dictionary files       */
#define   DCTNRY3    (0)  

                          /*  file type value:  gridded files          */
#define   GRDDED3    (1)  

                          /*  file type value:  boundary files         */
#define   BNDARY3    (2)  

                          /*  file type value:  ID/scattered           */
#define   IDDATA3    (3)  

                          /*  file type value:  profile files          */
#define   PROFIL3    (4)  

                          /*  file type value:  grid-nest files        */
#define   GRNEST3    (5)  

                          /*  file type value:  sparse matrix files    */
#define   SMATRX3    (6)  

                          /*  file type value:  time-series files    */
#define   TSRIES3    (7)  

                          /*  file type value:  pointer-flyer files    */
#define   PTRFLY3    (8)  



                          /*  basic data type value:  int */
#define   M3INT      (4)  

                          /*  basic data type value:  float */
#define   M3REAL     (5)  

                          /*  basic data type value:  double */
#define   M3DBLE     (6)  



                          /*  interprocess-comm virtual files (not yet imp.) */
#define   VIRFIL3   (-2)  

                          /*  intraprocess-comm virtual files           */
#define   BUFFIL3   (-1)  


                          /*  open-status:  OLD-READ-ONLY              */
#define   FSREAD3    (1)  

                          /*  open-status:  OLD-READ/WRITE             */
#define   FSRDWR3    (2)  

                          /*  open-status:  NEW-READ/WRITE:  create    */
#define   FSNEW3     (3)  

                          /*  open-status:  unknown -- create if necessary*/
#define   FSUNKN3    (4)  

                          /*  open-status:  create if necessary; truncate */
#define   FSCREA3    (5)  


                          /*  grid type value:  lat-lon coords         */
#define   LATGRD3    (1)  
                          /*  grid type value:  Lambert coords         */
#define   LAMGRD3    (2)  
                          /*  grid type value:  Mercator coords        */
#define   MERGRD3    (3)  
                          /*  grid type value:  Stereographic coords   */
#define   STEGRD3    (4)  
                          /*  grid type value:  UTM coords             */
#define   UTMGRD3    (5)  

                          /*  vert coord type 1:  hydrostatic sigma-P  */
#define   VGSGPH3    (1)  
                          /*  vert coord type 2:  non-h sigma-P        */
#define   VGSGPN3    (2)  
                          /*  vert coord type 3:  sigma-Z              */
#define   VGSIGZ3    (3)  
                          /*  vert coord type 4:  pressure (mb)        */
#define   VGPRES3    (4)  
                          /*  vert coord type 5:  Z (m) (above sea lvl)*/
#define   VGZVAL3    (5)  
                          /*  vert coord type 6:  H (m) (above ground) */
#define   VGHVAL3    (6)  


                          /*  Flag value:  "good" values            */
#define   OKFLAG3   (5461)     

                          /*  Flag value:  read all layers          */
#define   ALLAYS3   (-1)     

                          /*  Flag value:  read all variables       */
#define   ALLVAR3  "ALL"     

                          /* float flag value: "bad" or "missing"   */ 
#define   BADVAL3   (-9.999E36)

                          /* BADVAL3 < AMISS3 on all machines       */
#define   AMISS3    (-9.000E36)  

                          /* int flag value: "bad" or "missing"     */
#define   IMISS3    (-9999)      

                          /* char string "bad" or "missing" */
#define   CMISS3  "????????????????" 

#ifdef __cplusplus
}    /** END 'extern "c"' **/
#endif


#endif    /** PARMS3_DEFINED **/

/*****************   END   parms3.h   **********************************/


