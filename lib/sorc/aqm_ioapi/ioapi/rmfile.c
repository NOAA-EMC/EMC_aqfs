
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API.
    "rmfile.c" version "@(#)$Header$"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Fortran-callable "remove file" 
	
PRECONDITIONS:

CALLS:
    I/O API's Fortran binding routine INIT3()

REVISION HISTORY:
    ??

    Version   8/1999 by CJC for I/O APIv2:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

**************************************************************************/

/*   (preprocessor DEFINES and INCLUDES here)  */

#if defined(__hpux)		/** -->!!!HACK!!!<---  **/
#define _SYS_UNISTD_INCLUDED
#endif

#include <string.h>
#include <errno.h>
#include "parms3.h"

          
#if                     FLDMN

#define RMFILE rmfile_
#include <unistd.h>

#elif                   defined(__hpux) || defined(_AIX)

#define RMFILE rmfile
#include <unistd.h>

#elif                   defined(_WIN32)

#include <stdio.h>
#include <ctype.h>
#define unlink _unlink

#endif


#define  BUFLEN  256

extern void m3warnc( const char   * caller ,
	             int            jdate ,
	             int            jtime ,
	             const char   * errtxt ) ;


#if  defined(RMFILE) || defined(_WIN32)

/** Hack for Feldman-descended f77's follows: **/

static void  name2cstr( const char * source, 
                        char       * target,
                        int          slen,
                        int          tlen )
    {
    char  *bound ;
    char   ch ;
    int    length ;
    
    tlen-- ;

    for ( length = ( slen < tlen ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        ch = *source ;
        if ( isspace( ch ) ) break ;
        *target = ch ;

        } /**  END FOR-LOOP COPYING  source TO  buffer[], ETC.  **/
    
    *target = '\0' ;
    
    }    /** END Feldmanish name2cstr() **/
    

FINT RMFILE( const char *path, FSTR_L length )
    {       /*  begin body of DUMMY() */
    char  buffer[ BUFLEN + 1 ] ;
    int   status ;
    char *mesg ;

    name2cstr( path, buffer, length, BUFLEN + 1 ) ;

    if ( status = unlink( buffer ) )
        {
        if ( mesg = strerror( errno ) )
            {
            m3warnc( "RMFILE", 0, 0, mesg ) ;
            }
        else{
            m3warnc( "RMFILE", 0, 0, "Error unlinking file" ) ;
            } ;
        } ;

    return( status ) ;

    }       /*  end body of RMFILE()  */



#elif  defined(_CRAY)

#include <fortran.h>

static void  name2cstr( const _fcd   source, 
                        char       * target,
                        int          tlen  )
    {
    char  *bound, *ptr ;
    char   ch ;
    int    slen, length ;
    
    slen = _fcdlen( source ) ;
    tlen-- ;

    length = ( slen < tlen ? slen : tlen ) ;
    ptr    = _fcdtocp( source ) ;
    bound  = ptr + length ;
    
    for ( ; ptr < bound ; target++ , ptr++ )
        {
        if ( isspace( ch = *ptr ) ) break ;
        *target = ch ;

        } /**  END FOR-LOOP COPYING  source TO  buffer[], ETC.  **/

    *target = '\0' ;

    }           /** END Cray name2cstr() **/
    

FINT RMFILE( const _fcd  path )
    {       /*  begin body of DUMMY() */
    char   buffer[ BUFLEN + 1 ] ;
    int    status ;
    char * mesg ;

    name2cstr( path, buffer, BUFLEN + 1 ) ;

    if ( status = unlink( buffer ) )
        {
        if ( mesg = strerror( errno ) )
            {
            m3warnc( "RMFILE", 0, 0, mesg ) ;
            }
        else{
            m3warnc( "RMFILE", 0, 0, "Error unlinking file" ) ;
            } ;
        } ;

    return( status ) ;

    }       /*  end body of RMFILE()  */


#else
#error   "Error compiling RMFILE():  unsupported architecture"
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/
    
