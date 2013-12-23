
/**************************************************************************
VERSION "@(#)$Header$"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Wrapper around "system()" for Fortran use.

RETURNS:
    return status of "system(<command>)"

PRECONDITIONS:
    Valid Fortran-string command to pass along to "system()"

CALLS:
    system().

REVISION HISTORY:
    Version   8/99 by CJC:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications
**************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "parms3.h"

/** Hack for Feldman-descended f77's follows: **/

#if FLDMN
#define SYSTEMF   systemf_
#elif defined(__hpux) || defined(_AIX)
#define SYSTEMF   systemf
#elif defined(_WIN32)
/* DO NOTHING */
#elif defined(_CRAY)
/* DO NOTHING */
#else
#error   "Error compiling SYSTEMF():  unsupported architecture"
#endif

/** -------------------------------------------------------------- **/
/** FIRST CASE:  FELDMANISMS and WIN32:                                    **/
/** -------------------------------------------------------------- **/

#if defined(SYSTEMF) || defined(_WIN32)

/** -------------------- fstr2cstr() ----------------------------- **/

static void  fstr2cstr( const char * source, 
                        char       * target, 
                        long         slen, 
                        long         tlen )
    {
    char *bound ;
    long length ;

    for ( length = ( slen < tlen ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        *target = *source ;
        }
    
    *target = '\0' ;
    
    }       /** END Feldmanish fstr2cstr() **/


FINT SYSTEMF( char * cmdstr,
              FSTR_L length )
    {
    char cmdbuf[ 4096 ] ;
    fstr2cstr( cmdstr, cmdbuf, (long)length, 4096 ) ;
    return( (FINT) system( cmdbuf ) ) ;
    }

/** -------------------------------------------------------------- **/
/** NEXT CASE:  CRAY CF77-TARGETED ENV*():                         **/
/** -------------------------------------------------------------- **/

#elif  defined(_CRAY)

#include <fortran.h>

static void  fstr2cstr( const _fcd   source, 
                        char       * target, 
                        int          tlen )
    {
    char *ptr, *bound, ch ;
    int   slen, length ;

    slen = _fcdlen( source ) ;
    tlen-- ;

    length = ( slen < tlen ? slen : tlen ) ;
    ptr    = _fcdtocp( source ) ;
    for ( bound  = ptr + length ; ptr < bound ; target++ , ptr++ )
        {
        *target = *ptr ;
        }
    
    *target = '\0' ;
    
    }           /** END Cray fstr2cstr() **/

FINT SYSTEMF( const _fcd  cmdstr )
    {
    char cmdbuf[ 4096 ] ;
    fstr2cstr( cmdstr, cmdbuf, 4096 ) ;
    return( (FINT) system( cmdbuf ) ) ;
    }

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

