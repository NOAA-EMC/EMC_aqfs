
C Version "%W% %P% %G% %U%

        PROGRAM dummy

C***********************************************************************
C  program body starts at line
C
C  DESCRIPTION:
C
C
C  PRECONDITIONS REQUIRED:
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C
C  REVISION  HISTORY:
C
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   PARAMETERS and their descriptions:


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         GETEFILE
        EXTERNAL        GETEFILE

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER       IUNIT     !  unit number for GRIDDESC
        CHARACTER*16  ANAME	!  name read from file
        CHARACTER*300 MESG      !  for m3warn()
        CHARACTER*256 NAMBUF    !  for nameval()
        REAL*8        P_ALP	!  first, second, third map
        REAL*8        P_BET	!  projection descriptive
        REAL*8        P_GAM	!  parameters
        REAL*8        XCENT	!  lon for coord-system X=0
        REAL*8        YCENT	!  lat for coord-system Y=0
        REAL*8        XORIG	!  X-coordinate origin of grid (map units)
        REAL*8        YORIG	!  Y-coordinate origin of grid
        REAL*8        XCELL	!  X-coordinate cell dimension
        REAL*8        YCELL	!  Y-coordinate cell dimension
        INTEGER       NCOLS	!  number of grid columns
        INTEGER       NROWS	!  number of grid rows
        INTEGER       NTHIK	!  BOUNDARY:  perimeter thickness (cells)
        INTEGER        IOS      !  I/O status return

C***********************************************************************
C   begin body of program dummy

        WRITE( *,92000 )
     &  ' ',
     &  'Program dummy to test GRIDDESC options',
     &  ' '
        IUNIT = GETEFILE( 'GRIDDESC', .TRUE., .TRUE., 'DESCGRID' )
        IF ( IUNIT .LT. 0 ) THEN
            CALL NAMEVAL( 'GRIDDESC', NAMBUF )
            CALL M3WARN( 'DSCGRID', 0, 0, 
     &                       'Could not open GRIDDESC file' )
            MESG = 'Path name "' //NAMBUF
            CALL M3MESG( MESG )
            CALL EXIT( 1 )

        END IF	!  if getefile() failed

11      CONTINUE
            READ( IUNIT,*, IOSTAT = IOS ) ANAME
            IF ( IOS .NE. 0 ) THEN
               PRINT *, 'IOSTAT=', IOS
               CALL EXIT( 2 )
            ELSE IF ( ANAME .EQ. ' ' ) THEN
                GO TO  22
            ELSE
                PRINT *, 'CNAME=', ANAME
                READ( IUNIT,*, IOSTAT = IOS ) 
     &              P_ALP, P_BET, P_GAM, XCENT, YCENT
                PRINT *, '<ALP,BET,GAM>=', P_ALP, P_BET, P_GAM
                PRINT *, '<XCENT,YCENT>=', XCENT, YCENT
            END IF
            GO TO  11

22      CONTINUE
        PRINT *, '------------------------------End coords'

33      CONTINUE
            READ( IUNIT,*, IOSTAT = IOS ) ANAME
            IF ( IOS .NE. 0 ) THEN
               PRINT *, 'IOSTAT=', IOS
               CALL EXIT( 2 )
            ELSE IF ( ANAME .EQ. ' ' ) THEN
                GO TO  44
            ELSE
                PRINT *, 'GNAME=', ANAME
                READ( IUNIT,*, IOSTAT = IOS ) 
     &              XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK
                PRINT *, '<XORIG,YORIG>=', XORIG, YORIG
                PRINT *, '<XCELL,YCELL>=', XCELL, YCELL
                PRINT *, '<NCOLS,NROWS,NTHIK>=', NCOLS, NROWS, NTHIK
            END IF
            GO TO  33
44      CONTINUE
        PRINT *, '------------------------------End grids'

        CALL M3EXIT( 'dummy', 0, 0, 
     &               'Successful completion of program dummy', 0 )
C      STOP

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in program dummy ***',
     &            /5X , A ,
     &           // )        !  generic error message format


C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X, A )


C...........   Formatted file I/O formats............ 93xxx


C...........   Internal buffering formats............ 94xxx


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

