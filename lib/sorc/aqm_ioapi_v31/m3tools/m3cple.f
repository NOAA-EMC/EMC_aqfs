
        PROGRAM M3CPLE

C***********************************************************************
C Version "$Id: m3cple.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  202
C
C  DESCRIPTION:
C       For each time step in the specified time step sequence,
C       reads all variables from the specified input file, optionally
C       under the control of the specified synchronization file,
C       interpolate it to the specified output grid, and write them
C       to the specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input,
C       output, and GRIDDESC files.
C       Input file and output grid use the same coordinate system.
C       Specified time step sequence is valid for both the input and
C       synch files.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 8/99 by Carlie J. Coats, Jr., NCSC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  10/2002 by CJC for I/O API Version 2.2:  support for
C       many additional coordinate transformations, direct calls to GCTP
C       Version   6/2005 by CJC:  improved default for NRECS
C       Version  11/2005 by CJC:  eliminate unused vbles and functions
C       Version   9/2006 by CJC:  sphere/spheroid support
C       Version   3/2008 by CJC:  INSPHERE default-value bug-fix.
C       No resolution-warning for to/from-Lat-Lon conversions.
C       Version    6/2008 by CJC:  Albers map-projection support,
C       additional spheres 20, 21 from Steve Howard, NOAA/ASMD
C       Version   12/2008 by CJC:  heuristic to compensate for WMO screw-up
C       that declares all longitudes should be positive
C       Version   4/2009 by CJC:  corrections for TRM, EQM
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

       CHARACTER*16, PARAMETER :: PNAME = 'M3CPLE'
       CHARACTER*70, PARAMETER :: BAR   =
     &  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    SNAME   !  input synch file logical name
        CHARACTER*16    SVBLE   !  input   synch variable   name
        CHARACTER*16    ONAME   !  output data file logical name
        CHARACTER*16    CNAME   !  output coordinate system name
        CHARACTER*16    GNAME   !  output grid name

        LOGICAL         IFLAG   !  true iff interp (instead of copy)
        LOGICAL         SFLAG   !  true iff controlled by synch file

        LOGICAL         EFLAG
        CHARACTER*256   MESG

        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status

        LOGICAL         AFLAG, BFLAG, CFLAG, XFLAG, YFLAG

        INTEGER         C, R, V, N  !  loop counters

C.......   Arguments for GTPZ0:

        REAL*8          DSCR        !  scratch variables
        INTEGER         DEG, MNT    !  scratch variables
        REAL*8          CRDIN( 2 )  !  input coordinates x,y
        INTEGER*4       INSYS       !  input projection code
        INTEGER*4       INZONE      !  input utm zone, etc.
        REAL*8          TPAIN( 15 ) !  input projection parameters
        INTEGER*4       INUNIT      !  input units code
        INTEGER*4       INSPH       !  spheroid code
        INTEGER*4       IPR         !  error print flag
        INTEGER*4       JPR         !  projection parameter print flag
        INTEGER*4       LEMSG       !  error message unit number
        INTEGER*4       LPARM       !  projection parameter unit number

        REAL*8          CRDIO( 2 )  !  output coordinates x,y
        INTEGER*4       IOSYS       !  output projection code
        INTEGER*4       IOZONE      !  output utm zone, etc.
        REAL*8          TPOUT( 15 ) !  output projection parameters
        INTEGER*4       IOUNIT      !  output units code

        INTEGER*4       XXSYS       !  output projection code
        INTEGER*4       XXZONE      !  output utm zone, etc.
        REAL*8          TPAXX( 15 ) !  output projection parameters
        INTEGER*4       XXUNIT      !  output units code
        INTEGER*4       XXSPH       !  spheroid code

        INTEGER*4       LN27        !  NAD1927 file unit number
        INTEGER*4       LN83        !  NAD1983 file unit number
        CHARACTER*128   FN27        !  NAD1927 file name
        CHARACTER*128   FN83        !  NAD1983 file name
        INTEGER*4       LENGTH      !  NAD* record-length
        INTEGER*4       IFLG        !  error flag

        INTEGER         NCOLS1      ! number of grid columns
        INTEGER         NROWS1      ! number of grid rows
        INTEGER         NLAYS1      ! number of layers
        INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension

        INTEGER        NCOLS2       ! number of grid columns
        INTEGER        NROWS2       ! number of grid rows
        INTEGER        GDTYP2       ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8         P_ALP2       ! first, second, third map
        REAL*8         P_BET2       ! projection descriptive
        REAL*8         P_GAM2       ! parameters.
        REAL*8         XCENT2       ! lon for coord-system X=0
        REAL*8         YCENT2       ! lat for coord-system Y=0
        REAL*8         XORIG2       ! X-coordinate origin of grid (map units)
        REAL*8         YORIG2       ! Y-coordinate origin of grid
        REAL*8         XCELL2       ! X-coordinate cell dimension
        REAL*8         YCELL2       ! Y-coordinate cell dimension

        REAL*8          X0, Y0, XADJ

        INTEGER         ISPHERE
        REAL*8          RSPHERE

        CHARACTER*40    SPHERENAMES( 0:21 )
        DATA            SPHERENAMES
     &                  / 'Clarke 1866',
     &                    'Clarke 1880',
     &                    'Bessel',
     &                    'New International 1967',
     &                    'International 1909',
     &                    'WGS 72',
     &                    'Everest',
     &                    'WGS 66',
     &                    'GRS 1980',
     &                    'Airy',
     &                    'Modified Everest',
     &                    'Modified Airy',
     &                    'WGS 84',
     &                    'Southeast Asia',
     &                    'Australian National',
     &                    'Krassovsky',
     &                    'Hough',
     &                    'Mercury 1960',
     &                    'Modified Mercury 1968',
     &                    'Normal Sphere, R_Earth=6370997 M',
     &                    'Normal Sphere, R=6370000 (MM5 & WRF-ARW)',
     &                    'Normal Sphere, R=6371200 (WRF-NMM)'       /

        INTEGER         SIZE        ! grid volume, for copy

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         EDATE, ETIME, TSECS, NRECS

        REAL,    ALLOCATABLE::   INBUF( :, :, : )
        REAL,    ALLOCATABLE::   OUTBUF( :, :, : )
        REAL,    ALLOCATABLE::   XBUF( :, : )
        REAL,    ALLOCATABLE::   YBUF( :, : )
        REAL,    ALLOCATABLE::   XSCR( :, : )
        REAL,    ALLOCATABLE::   YSCR( :, : )
        REAL,    ALLOCATABLE::   CBUF( :, : )
        REAL,    ALLOCATABLE::   COPYBUF( : )
        INTEGER, ALLOCATABLE::   IBUF( :, : )


C...........   STATEMENT FUNCTION:  REAL*8 "definitely unequal"

        LOGICAL         DBLERR
        REAL*8          P, Q

        DBLERR( P, Q ) =
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


C***********************************************************************
C   begin body of program M3CPLE

        LDEV  = INIT3()
        EFLAG = .FALSE.

        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'Program M3CPLE to read all variables in each time step in ',
     & 'the specified time step sequence from the specified input',
     & 'file, optionally under the control of the specified',
     & 'synchronization file, copy or interpolate them to the ',
     & 'output grid, and write them to the specified output file.',
     & ' ',
     & 'THE PROGRAM WILL PROMPT YOU for the logical names of the',
     & 'input data file, the input synch file, and the output file,',
     & 'the time step sequence, and the GRIDDESC name of the output',
     & 'grid.  Default responses are indicated in square brackets',
     & '[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
     & 'If you wish to copy time steps, instead of interpolate them,',
     & 'respond "SAME" to the prompt for output grid name.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <input data  file>    <path-name>',
     & '    setenv <input synch file>    <path-name, or "NONE">',
     & '    setenv GRIDDESC              <path-name> (if interp)',
     & '    setenv IOAPI_ISPH            <USGS spheroid, or REARTH>',
     & ' ',
     & '    time step sequence is valid for both input files',
     & '    For interpolation, file type must be GRIDDED, and either',
     & '    the input and output coordinate systems must either be',
     & '    the same, or must be selected from one of the following',
     & '    supported coordinate conversions ("LL" mean "Lat-Lon, :',
     & '    "LAM" means "Lambert", "UTM" means "Universal Transverse ',
     & '    Mercator", "POL" means "Polar Stereographic", "EQM" means',
     & '    means "Equatorial Mercator", and "TRM")',
     & ' ',
     & '        LL   to/from  LL  (w/ different parameters),',
     & '        LAM  to/from  LAM (w/ different parameters),',
     & '        UTM  to/from  UTM (w/ different zones),',
     & '        POL  to/from  POL (w/ different parameters),',
     & '        EQM  to/from  EQM (w/ different parameters),',
     & '        LL   to/from  LAM, UTM, POL, EQM, TRM, or ALB,',
     & '        LAM  to/from  LL,  UTM, POL, EQM, TRM, or ALB,',
     & '        UTM  to/from  LL,  LAM, POL, EQM, TRM, or ALB,',
     & '        POL  to/from  LL,  LAM, UTM, EQM, TRM, or ALB,',
     & '        EQM  to/from  LL,  LAM, UTM, EQM, TRM, or ALB,',
     & '        TRM  to/from  LL,  LAM, UTM, POL, EQM, or ALB,',
     & '        ALB  to/from  LL,  LAM, UTM, POL, TRM, or EQM,',
     & ' ',
     & '    For interpolation, the output grid should have a finer',
     & '    resolution than the input grid (else you should use an',
     & '    aggregation program instead of an input program).',
     & '    For copy, file type must be GRIDDED, BOUNDARY, or CUSTOM.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: m3cple.f 44 2014-09-12 18:03:16Z coats                  $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF


C...............  Open and get description for optional synch file

        MESG  = 'Enter name for input synch file, or "NONE"'
        SNAME = PROMPTMFILE( MESG, FSREAD3, 'NONE ', PNAME )
        SFLAG = ( SNAME .NE. 'NONE ' )

        IF ( SFLAG ) THEN

            IF ( DESC3( SNAME ) ) THEN
                SVBLE  = VNAME3D( 1 )
            ELSE
                MESG = 'Could not get file description for ' // SNAME
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        END IF          !  if synch-flag option taken


C...............  Open and get description for input data file

        MESG  = 'Enter name for input data file'
        FNAME = PROMPTMFILE( MESG, FSREAD3, 'INFILE', PNAME )

        IF ( DESC3( FNAME ) ) THEN
            NCOLS1 = NCOLS3D
            NROWS1 = NROWS3D
            NLAYS1 = NLAYS3D
            GDTYP1 = GDTYP3D
            P_ALP1 = P_ALP3D
            P_BET1 = P_BET3D
            P_GAM1 = P_GAM3D
            XCENT1 = XCENT3D
            YCENT1 = YCENT3D
            XORIG1 = XORIG3D
            YORIG1 = YORIG3D
            XCELL1 = XCELL3D
            YCELL1 = YCELL3D
        ELSE
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Get output grid description, time step sequence

        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'If you wish to copy time steps (keeping the output grid the',
     & 'same) instead of interpolating them to a new output grid,',
     & 'respond "SAME" to the prompt for output grid name.  ',
     & 'Otherwise, give the GRIDDESC name for the output grid.',
     & ' '
        CALL GETSTR( 'Enter output grid name, or "SAME"',
     &               'SAME', GNAME )

        IF ( TSTEP3D .EQ. 0 ) THEN
            JDATE = 0
            JTIME = 0
            TSTEP = 0
            NRECS = 1
            GO TO  11
        END IF

        JDATE = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                  'Enter STARTING DATE for time step sequence' )

        JTIME = GETNUM(       0, 9999999, STIME3D,
     &                  'Enter STARTING TIME for time step sequence' )

        TSTEP = GETNUM( TSTEP3D, 9999999, TSTEP3D,
     &                  'Enter   TIME STEP   for time step sequence' )

        CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE,ETIME )
        N     = CURREC( EDATE, ETIME, JDATE, JTIME, TSTEP, C, R )
        NRECS = GETNUM( 1, 9999999, N,
     &                  'Enter     NRECS     for time step sequence' )

        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP

11      CONTINUE        !  target of "if tstep3d is zero"


C...............  Setup for mode of operation:  copy or interpolate:

        CNAME = GNAME
        CALL UPCASE( CNAME )
        IF ( ( CNAME .EQ. 'SAME' ) ) THEN   !  set up for copy

            IFLAG = .FALSE.

            IF ( FTYPE3D .EQ. GRDDED3 ) THEN
                SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
                SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
                SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE
                MESG = 'Cannot copy--' //
     &                 'file type not GRIDDED, BOUNDARY, or CUSTOM'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                EFLAG = .TRUE.
            END IF

            ALLOCATE( COPYBUF( SIZE ), STAT = STATUS )

            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' )
     &               'Buffer allocation failed:  STAT=', STATUS
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN

            MESG = 'File type not GRIDDED--cannot interpolate'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        ELSE    !  set up for interpolate:

            IFLAG = .TRUE.

            GDNAM3D = GNAME
            IF ( DSCGRID( GNAME, CNAME, GDTYP3D,
     &                    P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,
     &                    XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &                    NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

                NCOLS2 = NCOLS3D
                NROWS2 = NROWS3D
                GDTYP2 = GDTYP3D
                P_ALP2 = P_ALP3D
                P_BET2 = P_BET3D
                P_GAM2 = P_GAM3D
                XCENT2 = XCENT3D
                YCENT2 = YCENT3D
                XORIG2 = XORIG3D
                YORIG2 = YORIG3D
                XCELL2 = XCELL3D
                YCELL2 = YCELL3D

                CALL M3MESG( BAR )
                CALL M3MESG( 'Input grid parameters' )
                WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLS1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWS1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYP1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_ALP', P_ALP1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_BET', P_BET1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_GAM', P_GAM1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XCENT', XCENT1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YCENT', YCENT1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XORIG', XORIG1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YORIG', YORIG1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XCELL', XCELL1
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YCELL', YCELL1
                CALL M3MESG( MESG )
                CALL M3MESG( BAR )
                CALL M3MESG( 'Output grid parameters' )
                WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLS2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWS2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYP2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_ALP', P_ALP2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_BET', P_BET2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'P_GAM', P_GAM2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XCENT', XCENT2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YCENT', YCENT2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XORIG', XORIG2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YORIG', YORIG2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'XCELL', XCELL2
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, 1PE24.16 )' ) 'YCELL', YCELL2
                CALL M3MESG( MESG )
                CALL M3MESG( BAR )
                RSPHERE = ENVDBLE( 'IOAPI_ISPH',
     &                             'USGS sphere, or Earth-radius (M)',
     &                             8.0D0, STATUS )

                IF ( STATUS .GT. 0 ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad environment variable "IOAPI_ISPH"'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                ELSE IF ( RSPHERE .GT. -0.5D0 .AND.
     &                    RSPHERE .LT. 21.5D0 ) THEN
                    ISPHERE = NINT( RSPHERE )
                    MESG    = 'Using spheroid '//SPHERENAMES( ISPHERE )
                    CALL M3MESG( MESG )
                ELSE IF ( RSPHERE .GT. 6300.0D3 .AND.
     &                    RSPHERE .LT. 6500.0D3 ) THEN
                    WRITE( MESG, '( A, 2X, 1PD24.16, 1X, A )' )
     &                'Using perfect sphere with radius', RSPHERE, 'M'
                    CALL M3MESG( MESG )
                    ISPHERE = 19
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, 2X, 1PD24.16, 1X, A )' )
     &                'Probably-invalid Earth-radius', RSPHERE, 'M'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF

            ELSE

                MESG   = '"' // TRIM( GNAME ) //
     &                   '" not found in GRIDDESC file'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            END IF

            X0 = XORIG3D - 0.5D0 * XCELL3D
            Y0 = YORIG3D - 0.5D0 * YCELL3D

            EFLAG   = .FALSE.


C.......   Compute longitude adjustment to compensate for WMO screw-up
C.......   that declares all longitudes should be positive:

            XADJ = 0.0D0
            IF ( GDTYP1 .EQ. LATGRD3 .AND. XORIG1 .GE. 0.0D0 ) THEN     !  WMO LL input
                IF ( ( GDTYP2.EQ.LATGRD3 .AND. XORIG2.LT.0.0D0 ) .OR.
     &               ( GDTYP2.GT.1       .AND. XCENT2.LT.0.0D0 ) ) THEN
                    XADJ = 360.0D0
                    CALL M3MSG2( 'Longitude adjustment:  360.0D0' )
                END IF
            ELSE IF  ( GDTYP2.EQ.LATGRD3 .AND. XORIG2.GE.0.0D0 ) THEN   !  WMO LL output
                IF ( ( GDTYP1.EQ.LATGRD3 .AND. XORIG1.LT.0.0D0 ) ) THEN
                    XADJ = -360.0D0
                    CALL M3MSG2( 'Longitude adjustment: -360.0D0' )
                END IF
            END IF


C...............  Allocate buffers; compute re-gridding matrix

            ALLOCATE( INBUF ( NCOLS1, NROWS1, NLAYS1 ),
     &                OUTBUF( NCOLS2, NROWS2, NLAYS1 ),
     &                XBUF  ( NCOLS2, NROWS2 ),
     &                YBUF  ( NCOLS2, NROWS2 ),
     &                XSCR  ( NCOLS2, NROWS2 ),
     &                YSCR  ( NCOLS2, NROWS2 ),
     &                IBUF  ( 4, NCOLS2*NROWS2 ),
     &                CBUF  ( 4, NCOLS2*NROWS2 ),
     &                STAT = STATUS )

            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '( A, I10)' )
     &               'Buffer allocation failed:  STAT=', STATUS
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            AFLAG = DBLERR( P_ALP1, P_ALP3D )
            BFLAG = DBLERR( P_BET1, P_BET3D )
            CFLAG = DBLERR( P_GAM1, P_GAM3D )
            XFLAG = DBLERR( XCENT1, XCENT3D )
            YFLAG = DBLERR( YCENT1, YCENT3D )
            EFLAG = (AFLAG .OR. BFLAG .OR. CFLAG .OR. XFLAG .OR. YFLAG)

            IF ( GDTYP1 .EQ. GDTYP3D  .AND. .NOT. EFLAG ) THEN

                !!  Same map projection:

                DO  R = 1, NROWS3D
                DO  C = 1, NCOLS3D
                    XBUF( C,R ) = X0 + DBLE( C ) * XCELL3D
                    YBUF( C,R ) = Y0 + DBLE( R ) * YCELL3D
                END DO
                END DO

            ELSE    !!  different map projections:  set up input
                    !!  arguments for GCTP

                EFLAG = .FALSE.

                TPAXX = 0.0D0
                TPAXX( 1 ) = RSPHERE
                XXSYS  = 0       !  geographic (=Lat-Lon)
                XXZONE = 0
                XXUNIT = 4       !  input units:  degrees
                XXSPH  = ISPHERE !  spheroid
                IPR    = 0       !  print error messages, if any
                JPR    = 1       !  do NOT print projection parameters
                LEMSG  = INIT3() !  unit number for log file
                LPARM  = LEMSG   !  projection parameters file

C...............  GCTP Arguments for input-file coordinate system:

                IF ( GDTYP1 .EQ. LATGRD3 ) THEN

                    TPOUT = 0.0D0
                    TPOUT( 1 ) = RSPHERE
                    IOSYS  = 0       !  geographic (=Lat-Lon)
                    IOZONE = 0
                    IOUNIT = 4       !  output units:  degrees

                ELSE IF ( GDTYP1 .EQ. LAMGRD3 ) THEN

                    TPOUT( 1 ) = RSPHERE
                    TPOUT( 2 ) = 0.0D0

                    DSCR = P_ALP1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_GAM1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = YCENT1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
                    TPOUT( 7 ) = 0.0D0
                    TPOUT( 8 ) = 0.0D0
                    IOSYS  = 4       !  Lambert conformal conic
                    IOZONE = 67
                    IOUNIT = 2       !  output units:  meters

                ELSE IF ( GDTYP1 .EQ. UTMGRD3 ) THEN

                    TPOUT = 0.0D0
                    TPOUT( 1 ) = RSPHERE
                    IOSYS  = 1       !  Universal Transverse Mercator
                    IOZONE = NINT( P_ALP1 )
                    IOUNIT = 2       !  output units:  meters

                ELSE IF ( GDTYP1 .EQ. POLGRD3 ) THEN

                    TPOUT( 1 ) = RSPHERE
                    TPOUT( 2 ) = 0.0D0
                    TPOUT( 3 ) = 0.0D0
                    TPOUT( 4 ) = 0.0D0

                    DSCR = P_GAM1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPOUT( 7 ) = 0.0D0
                    TPOUT( 8 ) = 0.0D0
                    IOSYS  = 6       !  Polar Stereographic
                    IOZONE = 67
                    IOUNIT = 2       !  output units:  meters

                ELSE IF ( GDTYP1 .EQ. TRMGRD3 ) THEN

                    TPOUT( 1 ) = RSPHERE
                    TPOUT( 2 ) = 0.0D0
                    TPOUT( 3 ) = 0.0D0
                    TPOUT( 4 ) = 0.0D0

                    DSCR = P_GAM1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_ALP1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPOUT( 7 ) = 0.0D0
                    TPOUT( 8 ) = 0.0D0
                    IOSYS  = 9       ! Transverse Mercator

                    IOZONE = 67
                    IOUNIT = 2       !  output units:  meters

                ELSE IF ( GDTYP1 .EQ. EQMGRD3 ) THEN

                    TPOUT( 1 ) = RSPHERE
                    TPOUT( 2 ) = 0.0D0
                    TPOUT( 3 ) = 0.0D0
                    TPOUT( 4 ) = 0.0D0

                    DSCR = P_GAM1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_ALP1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPOUT( 7 ) = 0.0D0
                    TPOUT( 8 ) = 0.0D0
                    IOSYS  = 5       ! Equatorial Mercator

                    IOZONE = 67
                    IOUNIT = 2       !  output units:  meters

                ELSE IF ( GDTYP1 .EQ. ALBGRD3 ) THEN

                    TPOUT( 1 ) = RSPHERE
                    TPOUT( 2 ) = 0.0D0

                    DSCR = P_ALP1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_GAM1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = YCENT1
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
                    TPOUT( 7 ) = 0.0D0
                    TPOUT( 8 ) = 0.0D0
                    IOSYS  = 3       !  Albers equal-area conic
                    IOZONE = 67
                    IOUNIT = 2       !  output units:  meters

                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I5 )' )
     &                   'Input  coordinate type ', GDTYP1
                    CALL M3MSG2( MESG )
                END IF


C...............  GCTP Arguments for output-file coordinate system:

                INSPH  = ISPHERE !  spheroid

                IF ( GDTYP2 .EQ. LATGRD3 ) THEN

                    TPAIN = 0.0D0
                    TPAIN( 1 ) = RSPHERE
                    INSYS  = 0       !  geographic (=Lat-Lon)
                    INZONE = 0
                    INUNIT = 4       !  input units:  degrees

                ELSE IF ( GDTYP2 .EQ. LAMGRD3 ) THEN

                    TPAIN( 1 ) = RSPHERE
                    TPAIN( 2 ) = 0.0D0

                    DSCR = P_ALP2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_GAM2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = YCENT2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
                    TPAIN( 7 ) = 0.0D0
                    TPAIN( 8 ) = 0.0D0

                    INSYS  = 4       !  Lambert conformal conic
                    INZONE = 77
                    INUNIT = 2       !  input units:  meters

                ELSE IF ( GDTYP2 .EQ. UTMGRD3 ) THEN

                    TPAIN = 0.0D0
                    TPAIN( 1 ) = RSPHERE
                    INSYS  = 1       !  Universal Transverse Mercator
                    INZONE = NINT( P_ALP2 )
                    INUNIT = 2       !  input units:  meters

                ELSE IF ( GDTYP2 .EQ. POLGRD3 ) THEN

                    TPAIN( 1 ) = RSPHERE
                    TPAIN( 2 ) = 0.0D0
                    TPAIN( 3 ) = 0.0D0
                    TPAIN( 4 ) = 0.0D0

                    DSCR = P_GAM2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPAIN( 7 ) = 0.0D0
                    TPAIN( 8 ) = 0.0D0

                    INSYS  = 6       !  Polar stereographic
                    INZONE = 77
                    INUNIT = 2       !  input units:  meters

                ELSE IF ( GDTYP2 .EQ. TRMGRD3 ) THEN

                    TPAIN( 1 ) = RSPHERE
                    TPAIN( 2 ) = 0.0D0
                    TPAIN( 3 ) = 0.0D0
                    TPAIN( 4 ) = 0.0D0

                    DSCR = P_GAM2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPAIN( 7 ) = 0.0D0
                    TPAIN( 8 ) = 0.0D0

                    INSYS  = 9       !  Transverse Mercator
                    INZONE = 77
                    INUNIT = 2       !  input units:  meters

                ELSE IF ( GDTYP2 .EQ. EQMGRD3 ) THEN

                    TPAIN( 1 ) = RSPHERE
                    TPAIN( 2 ) = 0.0D0
                    TPAIN( 3 ) = 0.0D0
                    TPAIN( 4 ) = 0.0D0

                    DSCR = P_GAM2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_ALP2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    TPAIN( 7 ) = 0.0D0
                    TPAIN( 8 ) = 0.0D0

                    INSYS  = 5       !  Equatorial Mercator
                    INZONE = 77
                    INUNIT = 2       !  input units:  meters

                ELSE IF ( GDTYP2 .EQ. ALBGRD3 ) THEN

                    TPAIN( 1 ) = RSPHERE
                    TPAIN( 2 ) = 0.0D0

                    DSCR = P_ALP2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_BET2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = P_GAM2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

                    DSCR = YCENT2
                    DEG  = INT( DSCR )                              !  int degrees
                    DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
                    MNT  = INT( DSCR )                              !  int minutes
                    DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
                    TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
                    TPAIN( 7 ) = 0.0D0
                    TPAIN( 8 ) = 0.0D0

                    INSYS  = 3       !  Albers equal-area conic
                    INZONE = 77
                    INUNIT = 2       !  input units:  meters

               ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I5 )' )
     &                   'Output  coordinate type ', GDTYP3D
                    CALL M3MSG2( MESG )
                END IF

                IF ( EFLAG ) THEN
                    MESG = 'Unsupported coordinate conversion'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF


C...............  Establish "False-Easting/Northing" values:

                CRDIN( 1 ) = XCENT1
                CRDIN( 2 ) = YCENT1
                CALL GTPZ0( CRDIN, XXSYS, XXZONE, TPAXX, XXUNIT,
     &                      XXSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( GDTYP1 .EQ. UTMGRD3 ) THEN
                    TPOUT( 7 ) = XCENT1
                    TPOUT( 8 ) = YCENT1
                ELSE IF ( IFLG .EQ. 0 .AND. GDTYP1 .NE. UTMGRD3 ) THEN
                    TPOUT( 7 ) = CRDIO( 1 )
                    TPOUT( 8 ) = CRDIO( 2 )
                    IOZONE = IOZONE + 1
                ELSE
                    IFLG = MAX( MIN( 9, IFLG ), 1 )
                    WRITE( MESG, '( A, I3 )' ) 'GCTP XY1 error', IFLG
                    CALL M3MSG2( MESG )
                END IF

                CRDIN( 1 ) = XCENT2
                CRDIN( 2 ) = YCENT2
                CALL GTPZ0( CRDIN, XXSYS, XXZONE, TPAXX, XXUNIT,
     &                      XXSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, INSYS, INZONE, TPAIN, INUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( GDTYP2 .EQ. UTMGRD3 ) THEN
                    TPOUT( 7 ) = XCENT2
                    TPOUT( 8 ) = YCENT2
                ELSE IF ( IFLG .EQ. 0 ) THEN
                    TPAIN( 7 ) = CRDIO( 1 )
                    TPAIN( 8 ) = CRDIO( 2 )
                    INZONE = INZONE + 1
                ELSE
                    IFLG = MAX( MIN( 9, IFLG ), 1 )
                    WRITE( MESG, '( A, I3 )' ) 'GCTP XY2 error', IFLG
                    CALL M3MSG2( MESG )
                END IF

                IF ( EFLAG ) THEN
                    MESG = 'Coordinate setup error(s)'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF


C...............   Compute output-grid center coords wrt input grid coords:

                IF ( XADJ .GT. 0.0D0 ) THEN             !  WMO-screwup input grid

                    DO  R = 1, NROWS3D
                    DO  C = 1, NCOLS3D

                        CRDIN( 1 ) = X0 + DBLE( C ) * XCELL3D
                        CRDIN( 2 ) = Y0 + DBLE( R ) * YCELL3D
                        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                              INSPH,IPR, JPR, LEMSG, LPARM,
     &                              CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                              LN27,LN83,FN27,FN83, LENGTH, IFLG )

                        IF ( IFLG .EQ. 0 ) THEN
                            XBUF( C,R ) = SNGL( CRDIO( 1 ) + XADJ )
                            YBUF( C,R ) = SNGL( CRDIO( 2 ) )
                        ELSE
                            IFLG = MAX( MIN( 9, IFLG ), 1 )
                            WRITE( MESG, '( A,I3 )' ) 'GCTP error', IFLG
                            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                        END IF

                    END DO
                    END DO

                ELSE IF ( XADJ .LT. 0.0D0 ) THEN        !  WMO-screwup output grid

                    DO  R = 1, NROWS3D
                    DO  C = 1, NCOLS3D

                        CRDIN( 1 ) = X0 + DBLE( C ) * XCELL3D + XADJ
                        CRDIN( 2 ) = Y0 + DBLE( R ) * YCELL3D
                        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                              INSPH,IPR, JPR, LEMSG, LPARM,
     &                              CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                              LN27,LN83,FN27,FN83, LENGTH, IFLG )

                        IF ( IFLG .EQ. 0 ) THEN
                            XBUF( C,R ) = SNGL( CRDIO( 1 ) )
                            YBUF( C,R ) = SNGL( CRDIO( 2 ) )
                        ELSE
                            IFLG = MAX( MIN( 9, IFLG ), 1 )
                            WRITE( MESG, '( A,I3 )' ) 'GCTP error', IFLG
                            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                        END IF

                    END DO
                    END DO

                ELSE

                    DO  R = 1, NROWS3D
                    DO  C = 1, NCOLS3D

                        CRDIN( 1 ) = X0 + DBLE( C ) * XCELL3D
                        CRDIN( 2 ) = Y0 + DBLE( R ) * YCELL3D
                        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                              INSPH,IPR, JPR, LEMSG, LPARM,
     &                              CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                              LN27,LN83,FN27,FN83, LENGTH, IFLG )

                        IF ( IFLG .EQ. 0 ) THEN
                            XBUF( C,R ) = SNGL( CRDIO( 1 ) )
                            YBUF( C,R ) = SNGL( CRDIO( 2 ) )
                        ELSE
                            IFLG = MAX( MIN( 9, IFLG ), 1 )
                            WRITE( MESG, '( A,I3 )' ) 'GCTP error', IFLG
                            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                        END IF

                    END DO
                    END DO

                END IF          !  if WMO-screwup in- or out- LL grid

            END IF              !  if same coordinate system, or not

            IF ( ( GDTYP3D .EQ. LATGRD3 .OR. GDTYP1 .EQ. LATGRD3 ) .AND.
     &           ( GDTYP3D .NE. GDTYP1 ) ) THEN
                CONTINUE
            ELSE IF ( ( YCELL1 .LT. YCELL3D ) .OR.
     &                ( XCELL1 .LT. XCELL3D ) ) THEN
                MESG = 'Resolution warning -- should ' //
     &                 'use aggregation instead of interpolation'
                CALL M3MSG2( MESG )
            END IF


C...............  Compute bilinear re-gridding matrix:

            CALL UNGRIDB( NCOLS1, NROWS1, XORIG1, YORIG1,
     &                    XCELL1, YCELL1,
     &                    NCOLS2*NROWS2, XBUF, YBUF, IBUF, CBUF )

        END IF      !  if gname = "SAME", or not


C...............  Open output file

        CALL GETSTR( 'Enter name for output data file',
     &               'OUTFILE', ONAME )

        IF ( .NOT. OPEN3( ONAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open ' // ONAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Process output time step sequence

        DO  N = 1, NRECS

            IF ( SFLAG ) THEN           !!  Synch-file processing
                IF ( .NOT. CHECK3( SNAME, SVBLE, JDATE, JTIME ) ) THEN
                    MESG = 'Failure checking variable "' //
     &                     TRIM( SVBLE ) // '" from synch file "' //
     &                     TRIM( SNAME ) // '"'
                    CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                END IF
            END IF

            WRITE( MESG, '( A, I7.7, A, I6.6 )' )
     &          'Processing  ', JDATE, ':', JTIME

            CALL M3MSG2( ' ' )
            CALL M3MSG2( MESG )

            IF ( IFLAG ) THEN   !  bilin-interpolate vs. copy

                DO  V = 1, NVARS3D

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3,
     &                                JDATE, JTIME, INBUF ) ) THEN
                        MESG = 'Failure reading variable "' //
     &                         TRIM( VNAME3D( V ) )
     &                         // '" from file "' //
     &                         TRIM( FNAME ) // '"'
                        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                    END IF

                    CALL BILIN( NCOLS1 * NROWS1,
     &                          NCOLS2 * NROWS2, NLAYS1,
     &                          IBUF, CBUF, INBUF, OUTBUF )

                    IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                                JDATE, JTIME, OUTBUF ) ) THEN
                        MESG = 'Failure writing variable "' //
     &                         TRIM( VNAME3D( V ) ) // '" to file "' //
     &                         TRIM( ONAME ) // '"'
                        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                    END IF

                END DO      !  end loop on variables

            ELSE        !  else no interpolation:  copy only.

                DO  V = 1, NVARS3D

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3,
     &                                JDATE, JTIME, COPYBUF ) ) THEN
                        MESG = 'Failure reading variable "' //
     &                         TRIM( VNAME3D( V ) ) //
     &                         '" from file "' //
     &                         TRIM( FNAME ) // '"'
                        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                    END IF

                    IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                                JDATE, JTIME, COPYBUF ) ) THEN
                        MESG = 'Failure writing variable "' //
     &                         TRIM( VNAME3D( V ) ) // '" to file "' //
     &                         TRIM( ONAME ) // '"'
                        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                    END IF

                END DO      !  end loop on variables

            END IF      !  if iflag, or not

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program M3CPLE', 0 )

        END PROGRAM M3CPLE

