/**  
    Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/time2secc.c,v 1.2 2000/11/27 22:54:34 smith_w Exp $"
    EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.
**/

/**************************************************************************
PURPOSE
	convert back and forth between Models-3 time conventions and 
	seconds

PRECONDITIONS
	time coded HHMMSS = 100 * ( 100 * hours + minutes ) + seconds

CALLS
	none

REVISION HISTORY
	prototype  3/95 byu CJC

**************************************************************************/

#include  "iodecl3.h"


int time2secc ( int time )
    {
    int  secs ;
    if ( time >= 0 )
        {
        secs = time%100 + 60 * ( 60 * ( time / 10000 )  
                               +  ( time / 100 ) % 100 ) ;
        }
    else{
        secs = -time ;
        secs = -( secs%100 + 60 * ( 60 * ( secs / 10000 )  
                                  +  ( secs / 100 ) % 100 ) ) ;
        }
    
    return  secs ;

    }       /*  end body of time2secc()  */

