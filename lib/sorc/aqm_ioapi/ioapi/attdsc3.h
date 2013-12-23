
/************************************************************************
C  INCLUDE FILE  attdsc.h
C
C    VERSION "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/attdsc3.h,v 1.5 2000/12/13 20:57:16 smith_w Exp $"
C    EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
C    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C    See file "LGPL.txt" for conditions of use.
C
C  DO NOT EDIT !!
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
C       Hydrology time series file attribute typedefs and extern structs for 
C       C bindings to the I/O API
C
C  DEPENDENT UPON:  
C       consistency with FORTRAN include-files PARMS33.EXT, ATTDSC3.EXT
C
C  REVISION HISTORY:  
C       prototype 12/30/96 by CJC
C
**************************************************************************/

#ifndef    ATTDSC3_DEFINED
#define    ATTDSC3_DEFINED

#include   "parms3.h"

/*************************************************************************      
        Typedef for a MODELS 3 attributes list:  same memory layout
        as the FORTRAN COMMONs BATTS3 and CATTS3.
**************************************************************************/

typedef struct { 
               FINT   natts[ MXVARS3 ] ;            /* # of atts per vble */
               FREAL  fatts[ MXVARS3 ][ MXATTS3 ] ; /* attribute values */
               }
               IOAPI_Batts3 ;

typedef struct { 
               M3Name  vname[ MXVARS3 ][ MXATTS3 ] ; /* attribute names */
               }
               IOAPI_Catts3 ;


#endif    /*   ATTDSC3_DEFINED  */

#if FLDMN || __sgi || __sun || __osf__ || __mips__
    
    extern IOAPI_Batts3 _batts3 ;
    extern IOAPI_Catts3 _catts3 ;

#elif __hpux || _AIX

    extern IOAPI_Batts3  batts3 ;
    extern IOAPI_Catts3  catts3 ;

#elif  _CRAY

    extern IOAPI_Batts3  BATTS3 ;
    extern IOAPI_Catts3  CATTS3 ;

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

/****************   END   attdsc3.h   ***********************************/


