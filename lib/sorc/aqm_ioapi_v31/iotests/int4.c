
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#define BYTE0( i ) ((char)   (i)              & 255 )
#define BYTE1( i ) ((char) ( (i) /      256 ) & 255 )
#define BYTE2( i ) ((char) ( (i) /    65536 ) & 255 )
#define BYTE3( i ) ((char) ( (i) / 16777216 ) & 255 )

typedef char        INT4 [ 4] ;     /** header representation for "int32" **/

static void set_int4( INT4 int4 , int i )
    {
    int4[0] = BYTE0( i ) ;
    int4[1] = BYTE1( i ) ;
    int4[2] = BYTE2( i ) ;
    int4[3] = BYTE3( i ) ;
    }                   /** END   static void set_int4() **/

static int get_int4( INT4 int4 )
    {
    return (          (int)int4[0]
           +      256*(int)int4[1]
           +    65536*(int)int4[2]
           + 16777216*(int)int4[3] ) ;
    }                   /** END   static int get_int4() **/

int main()
    {
    char        i4[ 5 ] ;
    int         ii, jj ;
    int         b0, b1, b2, b3 ;

    i4[ 4 ] = (char) 0 ;

    for ( ; ; )
        {
        printf( "\n Enter integer: " ) ;
        scanf( "%d", & ii ) ;
        set_int4( i4 , ii ) ;
        b0 = BYTE0( ii ) ;
        b1 = BYTE1( ii ) ;
        b2 = BYTE2( ii ) ;
        b3 = BYTE3( ii ) ;
        jj = get_int4( i4 ) ;
        printf( "\n %d ~~> %s ~~> %d has bytes %d:%d:%d:%d",
                ii, i4, jj, b0, b1, b2, b3 ) ;
        }
    }
