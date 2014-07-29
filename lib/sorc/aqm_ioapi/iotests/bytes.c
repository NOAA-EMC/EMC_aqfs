
typedef char    INT4 [ 4 ] ;     /** header representation for "int32" **/

extern int      get_endian( ) ;

int main()
    {
    int         jj, kk;
    int         b0, b1, b2, b3 ;
    union       UU { int ii ; INT4 i4 ; } uu;

    /**  Detect ENDIANNESS **/
    /**  BIGENDIAN  (e.g., SGI)    give 0:0:0:1 0:0:2:0 0:3:0:0 4:0:0:0 **/
    /**  LITTLEENDIAN  (e.g., x86) give 1:0:0:0 0:2:0:0 0:0:3:0 0:0:0:4 **/
    /**  **/

    for( jj = 1, kk = 1; jj < 5 ; jj++, kk*=256 )
        {
        uu.ii = jj*kk ;
        b0 = (int)uu.i4[0] ;
        b1 = (int)uu.i4[1] ;
        b2 = (int)uu.i4[2] ;
        b3 = (int)uu.i4[3] ;
        printf( "\n %d has bytes %d:%d:%d:%d\n",
                uu.ii, b0, b1, b2, b3 ) ;
        }

    jj = get_endian() ;

    printf( "\n\n %s %d\n", "get_endian()=", jj ) ;

    }
