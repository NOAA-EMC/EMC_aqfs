/* Sample I/O API read */
/*                      */
#include <stdio.h>
#include <stdlib.h>
#include "iodecl3.h"

/** -------------------------------------------------------------- **/
/** ------------------------  auxilliary functions --------------- **/
/** ------------------------  name2cstr()  ----------------------- **/

static void  name2cstr( const char * source, 
                        char       * target,
                        FSTR_L       slen,
                        FSTR_L       tlen )
    {
    char  *bound ;
    char   ch ;
    int    length ;
    
    tlen-- ;
    target[ tlen ] = '\0' ;
    length = ( slen < tlen-1 ? slen : tlen-1 ) ;
    bound  = target + length ;

    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( isspace( ch ) )
            {
            *target = '\0' ;
            return ;
            }
        *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/
    
    *target = '\0' ;
    
    }    /** END Feldmanish name2cstr() **/
    

/** -------------------- fstr2cstr() ----------------------------- **/

static void  fstr2cstr( const char * source, 
                        char       * target, 
                        FSTR_L       slen, 
                        FSTR_L       tlen )
    {
    char  *bound ;
    FSTR_L length ;

    tlen-- ;

    for ( length = ( slen < tlen-1 ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        *target = *source ;
        }
    
    *target = '\0' ;
    
    }       /** END Feldmanish fstr2cstr() **/


/** -------------------------------------------------------------- **/
/** ---------------------  main program  ------------------------- **/
/** -------------------------------------------------------------- **/

int main(void)
    {

    int     jdate, jtime, jstep;
    int     i, m, ngrid ;
    float * rbuf ;
    float   rsum, rdiv ;
    char    varname[ NAMLEN3+1 ] ;
    char    varunit[ NAMLEN3+1 ] ;
    char    vardesc[ MXDLEN3+1 ] ;
    IOAPI_Bdesc3 bdesc;
    IOAPI_Cdesc3 cdesc;

    printf( "    %s\n    %s\n\n    %s\n    %s\n",
            "Program TESTREAD to read data from an M3IO file and",
            "compute the layer-1 mean of the first variable",
            "BEFORE YOU RUN THE PRIGRAM:",
            "\tsetenv INFILE <path-name>" );

    init3c();                                   /*   Initialize I/O API        */

    if ( !open3c( "INFILE",                     /*   Open file for input        */
                  & bdesc, & cdesc, FSREAD3,
                 "TESTREAD" ) )
        {
        m3exitc( "TESTREAD",0,0,"Error opening file",1);
        }

    if ( ! desc3c( "INFILE", &bdesc, &cdesc) )  /*   Get information from file header   */
        {
        m3exitc( "TESTREAD", 0,0, "Error getting file description", 1 );
        }

    ngrid = bdesc.ncols*bdesc.nrows ;
    rdiv  = 1.0 / (float)ngrid ;
    jdate = bdesc.sdate ;
    jtime = bdesc.stime ;
    jstep = bdesc.tstep ;
    
    rbuf = (float *)malloc( sizeof( float ) * (size_t) ngrid ) ;
    if ( ! rbuf )
        {
        m3exitc( "TESTREAD", jdate, jtime, "malloc() failure", 2 );
        }

    printf( "Number of variables %d\n",    bdesc.nvars );
    printf( "Number of layers    %d\n",    bdesc.nlays );
    printf( "Number of rows      %d\n",    bdesc.nrows );
    printf( "Number of columns   %d\n",    bdesc.ncols );
    printf( "Starting date&time  %d:%d\n", jdate, jtime );
    printf( "Time step           %d\n",    jstep );
    printf( "Number of records   %d\n",    bdesc.mxrec );

    /*  Convert first-variable Fortran strings to C strings  */

    name2cstr( cdesc.vname[0], varname, (FSTR_L)NAMLEN3, (FSTR_L)(NAMLEN3+1) ) ;
    name2cstr( cdesc.units[0], varunit, (FSTR_L)NAMLEN3, (FSTR_L)(NAMLEN3+1) ) ;
    fstr2cstr( cdesc.vdesc[0], vardesc, (FSTR_L)MXDLEN3, (FSTR_L)(MXDLEN3+1) ) ;
    printf( "First variable \"%s\" (%s): %s", varname, varunit, vardesc ) ;

    /*  Loop on time steps:        */
    /*  Read data for variable 1 for time step and compute mean  */

    for ( m = 0; m < bdesc.mxrec ; ++m )
        {

        if( ! read3c( "INFILE", varname, 1, jdate, jtime, rbuf ) )
            {
            m3exitc( "TESTREAD", jdate, jtime, "Error reading from file", 2 );
            }

        for ( i = 0, rsum = 0.0 ; i < ngrid ; ++i )
            {
            rsum = rsum + rbuf[i] ;
            }

        printf( "Layer-1 mean for %s at %d:%d is %f\n",
                varname, jdate, jtime, rsum * rdiv );

        nextimec( &jdate, &jtime, jstep );      /*  compute next date/time    */
        }

    m3exitc( "TESTREAD", 0, 0, "Program finished", 0 ) ;

    }           /*  end of program "TESTREAD"  */

