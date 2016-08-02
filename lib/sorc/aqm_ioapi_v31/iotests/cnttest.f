  
        PROGRAM CNTTEST 
  
        IMPLICIT NONE
  
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'PARMS3.EXT' 
        INCLUDE 'STATE3.EXT'
  
        CHARACTER*16    FNAME
        CHARACTER*256   MESG
  
        INTEGER         FUNIT

        CHARACTER*16    PROMPTMFILE
        EXTERNAL        PROMPTMFILE

C...................................................................

        FUNIT  = INIT3()
        WRITE( *, '( 5X, A )' )
     &  'Program CNTTEST to test value of COUNT3 from BSTATE3 COMMON',
     &  'subsequent to calls to OPEN3()',
     &  ' '

11      CONTINUE
            FNAME = PROMPTMFILE( 'Enter file name, or "NONE"',
     &                            FSREAD3, 'INFILE',  'CNTTEST' )
  
            WRITE( MESG, '( 2 ( A, I10, :, 2X ) )' ) 
     &           'After OPEN3, COUNT3 = ', COUNT3,
     &           'MXFILE3 =  ', MXFILE3
            CALL M3MSG2( MESG )
        IF ( FNAME .NE. 'NONE' ) GO TO  11

        CALL M3EXIT( 'CNTTEST', 0, 0, 'Completion', 0 )
        END
  
