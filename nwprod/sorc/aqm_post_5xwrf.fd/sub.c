#include <string.h>
#include <stdio.h>

#include "iodecl3.h"
#include "state3.h"

#if FLDMN
#define OPEN_BINARY_FILE    open_binary_file_
#define EXTRACT_SPCNAME     extract_spcname_
#define CSTATE3    cstate3_
#elif defined(__hpux) || defined(_AIX)
#define OPEN_BINARY_FILE    open_binary_file
#define EXTRACT_SPCNAME     extract_spcname
#define CSTATE3    cstate3
#else
#error  "Error compiling iobin.c:  unsupported architecture"
#endif

extern IOAPI_CSTATE3     CSTATE3 ;

typedef unsigned char INT4 [ 4] ;     /** header representation for "int32" **/
typedef char          HREAL[16] ;     /**  " for Fortran REAL ( 1PE15.8\0)  **/
typedef char          HDBLE[28] ;     /**  " for Fortran DBLE (1PE27.19\0)  **/

typedef struct{
              INT4      nrecs ;         /**  at seek offset 0:  note that   **/
              M3Name    ioapi_vrsn ;    /**  nrecs is rewritten repeatedly  **/
              INT4      byte_order ;
              INT4      intsize ;
              INT4      rsize ;
              INT4      dsize ;
              INT4      cdate ;
              INT4      ctime ;
              INT4      wdate ;
              INT4      wtime ;
              INT4      ftype ;
              INT4      gdtyp ;
              INT4      vgtyp ;
              INT4      ncols ;
              INT4      nrows ;
              INT4      nlays ;
              INT4      nthik ;
              INT4      nvars ;
              INT4      sdate ;
              INT4      stime ;
              INT4      tstep ;
              HDBLE     p_alpha ;
              HDBLE     p_beta  ;
              HDBLE     p_gamma ;
              HDBLE     x_center  ;
              HDBLE     y_center  ;
              HDBLE     x_origin  ;
              HDBLE     y_origin  ;
              HDBLE     x_cellsize  ;
              HDBLE     y_cellsize  ;
              HREAL     vgtop  ;
              HREAL     vglvl[ MXLAYS3+1]  ;
              INT4      vtype[ MXVARS3 ] ;
              M3Name    gridname ;
              M3Name    updtname ;
              M3Line    execution ;
              M3Line    file_desc[ MXDESC3 ] ;
              M3Line    updt_desc[ MXDESC3 ] ; }  Bin_Hdr3 ;

/** --------------------- name2cstr() ---------------------------- **/

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
          bound  = target + length - 1 ;
              target < bound ;
                  target++ , source++ )
        {
        ch = *source ;
        if ( isspace( ch ) ) break ;
        *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/

    *target = '\0' ;

    }    /** END Feldmanish name2cstr() **/

/** --------------------- cstr2fstr() ---------------------------- **/

static void  cstr2fstr( const char * source,
                        char *       target,
                        FSTR_L       tlen )
{
    char *bound, ch ;

    for ( bound  = target + tlen ; target < bound ; target++ , source++ )
        {
        if ( ! ( ch = *source ) ) break ;
        *target = ch ;
        }

    for ( ; target < bound ; target++ )
        {
        *target = ' ' ;
        }

}         /** END Feldmanish cstr2fstr() **/

/** --------------------- get_int4() ---------------------------- **/

static int get_int4( INT4 int4 )
    {
    return (          (int)int4[0]
           +      256*(int)int4[1]
           +    65536*(int)int4[2]
           + 16777216*(int)int4[3] ) ;
    }                   /** END   static int get_int4() **/

/** ------------------- OPEN_BINARY_FILE() ---------------------- **/

void OPEN_BINARY_FILE (int  * ncols,
                       int  * nrows,
                       int  * nlays,
                       int  * nspcs,
                       int  * ftype,
                       int  * sdate,
                       int  * stime, 
                       int  * tstep, 
                       int  * nrec,
                       char * fname,
                       int    llen,
                       int    plen   )

{
    FILE      * filptr ;
    char        fpath[ 512 ] ;
    Bin_Hdr3    bin_hdr ;
    size_t      asize, nsize, nelts ;
    int         v, nvar;
    char aname[ NAMLEN3+2 ] ;
    FSTR_L       eqlen;

    name2cstr( fname, fpath, llen, (int)256 ) ;
/*
    name2cstr( fname, fpath, (FSTR_L)NAMLEN3, (FSTR_L)NAMLEN3+1 );
*/
    filptr = fopen( fpath, "r");

    nsize = sizeof( Bin_Hdr3 ) ;
    nelts = (size_t) 1 ;
    asize = fread( & bin_hdr, nsize, nelts, filptr ) ;

    *ftype = get_int4( bin_hdr.ftype ) ;
    *sdate = get_int4( bin_hdr.sdate ) ;
    *stime = get_int4( bin_hdr.stime ) ;

    *ncols = get_int4( bin_hdr.ncols ) ;
    *nrows = get_int4( bin_hdr.nrows ) ;
    *nlays = get_int4( bin_hdr.nlays ) ;
    *nspcs = get_int4( bin_hdr.nvars ) ;
    *tstep = get_int4( bin_hdr.tstep ) ;
    *nrec  = get_int4( bin_hdr.nrecs ) ;

    if (*ftype == 2)
       *nrows = (*nrows + *ncols) * 2 + 4;

    if (*ftype != 6)
       { nsize = (size_t) NAMLEN3 ;
         nelts = (size_t)(*nspcs);
         asize = fread( (void *)CSTATE3.vlist[ 0 ], nsize, nelts, filptr ) ;
       }

}

/** ------------------- EXTRACT_SPCNAME() ---------------------- **/

void EXTRACT_SPCNAME (int  * v,
                      char * vname)

{
   cstr2fstr( CSTATE3.vlist[ 0 ][ *v ], vname, 16);

}
