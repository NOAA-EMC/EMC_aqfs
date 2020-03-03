
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API.
    "locatsc.c" version "$Id: mmddyyc.c 45 2014-09-12 20:05:29Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    format and return the date as a character string "M+ D+, YYYY"

PRECONDITIONS:
    valid Julian date YYYYDDD, time HHMMSS

CALLS:
    none

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Revised   8/1999 by CJC -- bug fix for leap-years

    Unification 2/2002 by CJC with Global Climate Model IO_360 version
    that uses a 360-day year; use m3mesgc() for error messages
                    
**************************************************************************/

#include  <string.h>
#include  <stdio.h>
#include  "iodecl3.h"

void   mmddyyc( int   jdate ,
                char  buffer[ 15 ] )
{
static const char   months[ 12 ][ 6 ] =
    { 
    "Jan." , "Feb." , "March", "April", "May"  , "June",
    "July" , "Aug." , "Sept.", "Oct." , "Nov." , "Dec." } ;

static const int    cumday[ 13 ] = 
    {
#ifdef IO_360
    0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360
    };
#else
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
#endif
    } ;		/** Lookup table of cumulative days accumulated  **/
    		/** (in non-leap year) before the given month. **/
		/** CUMDAY(13) is total days per year. **/
    
static const int    leapday[ 13 ] = 
    {
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
    } ;		/** Lookup table of cumulative days accumulated  **/
    		/** (in leap year) before the given month. **/
		/** LEAPDAY(13) is total days per year. **/
    
int   year, iday, mnth, leap ;
char  mesg[256] ;

if ( jdate > 9999999 || jdate < 0 ) 
    {                                
    sprintf( mesg, 
             "%s %d",
             "Year-number error in mmddyycc():  jdate = ", jdate ) ;
    m3mesgc( mesg ) ;
    strcpy( buffer, "<DATE ERROR>" ) ;
    return ;
    }

year = jdate / 1000 ;
iday = jdate % 1000 ;

#ifdef IO_360

    for( mnth = 0 ; mnth < 13 ; mnth++ )
        {
        if ( iday <= cumday[ mnth+1 ] )
            {
            sprintf( buffer, 
                     "%s %d, %4d\0",
                     months[ mnth ],
                     iday - cumday[ mnth],
                     year ) ;
            return ;
            } ;
        }

#else

    leap = ( year % 4 == 0 ) && ( year % 100 ? 1 : ( year % 400 == 0 ) ) ;

    if ( leap )
        {
        for( mnth = 0 ; mnth < 13 ; mnth++ )
            {
            if ( iday <= leapday[ mnth+1 ] )
                {
                sprintf( buffer, 
                         "%s %d, %4d\0",
                         months[ mnth ],
                         iday - leapday[ mnth],
                         year ) ;
                return ;
                } ;
            }
        }
    else
        {
        for( mnth = 0 ; mnth < 13 ; mnth++ )
            {
            if ( iday <= cumday[ mnth+1 ] )
                {
                sprintf( buffer, 
                         "%s %d, %4d\0",
                         months[ mnth ],
                         iday - cumday[ mnth],
                         year ) ;
                return ;
                } ;
            }
        } ;

#endif

/** If you get to here:  bad arguments **/

fprintf( stderr, 
         "\n\n*** %s ***\n    %s %d\n",
         "Bad argument to mmddyyc()",
         "jdate = ", jdate ) ;
         
strcpy( buffer, "<DATE ERROR>" ) ;
return ;
    
}		/** END BODY OF void mmddyyc() **/

