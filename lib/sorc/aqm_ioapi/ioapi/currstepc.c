/**************************************************************************
VERSION "@(#)$Header$"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    If possible, compute the date&time cdate:ctime for the time 
    step in the time step sequence starting at sdate:stime and 
    having the time step tstep and return TRUE; 
    otherwise, return FALSE.  

    In particular, cdate:ctime is the date&time of largest time step
    in the sequence having the property:

        cdate:ctime <= jdate:jtime

    Return TRUE with cdate=sdate, ctime=stime if tstep==0.

PRECONDITIONS
    normalized dates and times (0 <= SS <= 59, etc.)
    stored in format YYYYDDD:HHMMSS.

CALLS
    none

REVISION HISTORY
    prototype 3/1995 by CJC

    revised   9/1999 by CJC for I/O API v2:  add WIN32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

**************************************************************************/

#include  "iodecl3.h"
#include  <stdlib.h>

#if FLDMN

#define   CURRSTEP currstep_ 

   extern FINT  CURRSTEP( FINT, FINT, FINT, FINT );

#elif defined(__hpux) || defined(_AIX)

#define CURRSTEP              currstep

   extern FINT  CURRSTEP( FINT, FINT, FINT, FINT );

#elif defined( _WIN32)

   extern FINT __stdcall CURRSTEP( FINT, FINT, FINT, FINT );

#elif  defined(_CRAY)

   extern FINT  CURRSTEP( FINT, FINT, FINT, FINT );

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/


int currstepc( int  jdate , int  jtime , 
               int  sdate , int  stime , int  tstep ,
               int *cdate , int *ctime )
    {
    int secs, step ;
    
    if ( tstep ) 
        {
        secs = (int) CURRSTEP( (FINT)sdate, (FINT)stime,
                               (FINT)jdate, (FINT)jtime ) ;
        if ( secs < 0 ) 
            {
            return  0 ;
            }
        else{
            *cdate = sdate ;
            *ctime = stime ;
            step  = time2secc( abs( tstep ) ) ;
            secs  = ( secs / step ) * step ;    /** use truncated step-# **/
            nextimec( cdate, ctime, sec2timec( secs ) ) ;
            return 1 ;
            }
        }
    else{		/** tstep == 0 case **/
        *cdate = sdate ;
        *ctime = stime ;
        return 1 ;
        }                                  

    }       /*  end body of currstepc()  */

