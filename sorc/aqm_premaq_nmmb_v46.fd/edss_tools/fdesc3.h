
/************************************************************************
C  INCLUDE FILE  fdesc3.h
C
C    Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/fdesc3.h,v 1.5 2000/12/13 20:57:17 smith_w Exp $"
C    EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C    See file "LGPL.txt" for conditions of use.
C
C
C    DO NOT EDIT !!
C
C        The EDSS/Models-3 I/O API depends in an essential manner
C        upon the contents of this INCLUDE file.  ANY CHANGES are
C        likely to result in very obscure, difficult-to-diagnose
C        bugs caused by an inconsistency between standard "libioapi.a"
C        object-libraries and whatever code is compiled with the
C        resulting modified INCLUDE-file.
C
C        By making any changes to this INCLUDE file, the user
C        explicitly agrees that in the case any assistance is 
C        required of MCNC or of the I/O API author, CARLIE J. COATS, JR.
C        HE AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE MCNC
C        AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
C        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
C        REQUIRED.
C
C  CONTAINS:  
C       File description typedefs for C bindings to the I/O API
C
C  DEPENDENT UPON:  
C       consistency with FORTRAN include-files PARMS33.EXT, FDESC3.EXT
C
C  REVISION HISTORY:  
C       prototype 5/9/95 by CJC
C
**************************************************************************/

#ifndef    FDESC3_DEFINED
#define    FDESC3_DEFINED

#include   "parms3.h"

/*************************************************************************      
        Typedef for I/O API file description:  same memory layout
        as the Fortran COMMONs BDESC3 and CDESC3.
**************************************************************************/

typedef char   M3Name[ NAMLEN3 ] ; /* Models3 Fortran "name" objects  */
typedef char   M3Line[ MXDLEN3 ] ; /* ... "description-line" objects  */

typedef struct { 
  double p_alp ;   /*  first, second, third map */  
  double p_bet ;   /*  projection descriptive   */  
  double p_gam ;   /*  parameters.              */  
  double xcent ;   /*  lon for coord-system X=0 */  
  double ycent ;   /*  lat for coord-system Y=0 */  
  double xorig ;   /*  X-coordinate origin of grid (map units) */
  double yorig ;   /*  Y-coordinate origin of grid  */  
  double xcell ;   /*  X-coordinate cell dimension  */  
  double ycell ;   /*  Y-coordinate cell dimension  */
  int    ftype ;   /*  file type                    */  
  int    cdate ;   /*  creation date   YYYYDDD      */  
  int    ctime ;   /*  creation time    HHMMSS      */  
  int    wdate ;   /*  update date     YYYYDDD      */  
  int    wtime ;   /*  update time      HHMMSS      */  
  int    sdate ;   /*  file start date YYYYDDD      */  
  int    stime ;   /*  file start time  HHMMSS      */  
  int    tstep ;   /*  file time step   HHMMSS      */  
  int    mxrec ;   /*  max time step record number  */  
  int    nvars ;   /*  number of species            */  
  int    ncols ;   /*  number of grid columns       */  
  int    nrows ;   /*  number of grid rows          */  
  int    nlays ;   /*  number of layers             */  
  int    nthik ;   /*  BOUNDARY:  perimeter thickness (cells)  */  
  int    gdtyp ;   /*  grid type:  1=LAT-LON, 2=LAM, ...       */  
  int    vgtyp ;   /*  vertical coordinate type (VGSIGP3, ...) */
  FREAL  vgtop ;   /*  model-top, for sigma coord types.       */  
  FREAL  vglvs[ MXLAYS3 + 1 ] ;  /*  vertical coord values.    */ 
  int    vtype[ MXVARS3 ] ;      /* basic data type M3INT, ... */
}
IOAPI_Bdesc3 ;

typedef struct { 
  M3Name  gdnam ;  /* grid name                       */ 
  M3Name  upnam ;  /* last program writing to file    */ 
  M3Line  execn ;  /* value of env vble EXECUTION_ID  */ 
  M3Line  fdesc[ MXDESC3 ] ; /* file description      */ 
  M3Line  updsc[ MXDESC3 ] ; /* update   "            */
  M3Name  vname[ MXVARS3 ] ; /* variable names        */
  M3Name  units[ MXVARS3 ] ; /*   "      units        */
  M3Line  vdesc[ MXVARS3 ] ; /*   "      descriptions */
}
IOAPI_Cdesc3 ;


#endif    /*   FDESC3_DEFINED  */

/****************   END   fdesc3.h   ***********************************/


