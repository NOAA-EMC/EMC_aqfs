
        PROGRAM UTMTOOL

C***********************************************************************
C Version "$Id: utmtool.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2007 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  113
C
C  DESCRIPTION:
C       Perform coordinate conversions and grid-related computations
C       for Lat-Lon, Lambert, and UTM coordinate systems.
C
C  PRECONDITIONS REQUIRED:
C       "setenv GRIDDESC <pathname>" for using Lambert projections by name.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETMENU, GETNUM, GETREAL, GETSTR, GETYN, LAMBERT
C
C  REVISION  HISTORY:
C       Augmented 6/11/96 by CJC to support Lambert operations.
C       Augmented 11/2000 by CJC to support POL, TRM, and EQM
C       Version   02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV
        INTEGER         MODE
        INTEGER         ZONE
        INTEGER         NCOL, NROW
        REAL            LAT, LON
        REAL            XX, YY
        REAL            CELL
        REAL            XLL, XUR
        REAL            YLL, YUR
        REAL            A, B, C, X, Y
        DATA            A /  30.0 /
        DATA            B /  60.0 /
        DATA            C / -90.0 /
        DATA            X / -90.0 /
        DATA            Y /  40.0 /

        CHARACTER*16    LAMNAME, POLNAME, TRMNAME, EQMNAME
        DATA            LAMNAME, POLNAME, TRMNAME, EQMNAME / 4*CMISS3 /

        CHARACTER*60, PARAMETER :: MENU( 44 ) =
     &    (/  'Initialize new LAMBERT projection  by name       ',    !!   mode = 1
     &        'Initialize new POLar Stereographic by name (POL) ',    !!   mode = 2
     &        'Initialize new TRansverse Mercator by name (TRM) ',    !!   mode = 3
     &        'Initialize new EQuatorial Mercator by name (EQM) ',    !!   mode = 4
     &        'Define a new LAMBERT projection  by angles       ',    !!   mode = 5
     &        'Define a new Polar Stereographic by angles       ',    !!   mode = 6
     &        'Define a new Transverse Mercator by angles       ',    !!   mode = 7
     &        'Define a new Equatorial Mercator by angles       ',    !!   mode = 8
     &        'Convert from LAT-LON to UTM                      ',    !!   mode = 9
     &        'Convert from UTM     to LAT-LON                  ',    !!   mode =10
     &        'Convert from UAM/EPS to LAT-LON                  ',    !!   mode =11
     &        'Convert from LAMBERT to UTM                      ',    !!   mode =12
     &        'Convert from UTM     to LAMBERT                  ',    !!   mode =13
     &        'Convert from LAMBERT to LAT-LON                  ',    !!   mode =14
     &        'Convert from LAT-LON to LAMBERT                  ',    !!   mode =15
     &        'Convert from POL     to LAT-LON                  ',    !!   mode =16
     &        'Convert from LAT-LON to POL                      ',    !!   mode =17
     &        'Convert from POL     to UTM                      ',    !!   mode =18
     &        'Convert from UTM     to POL                      ',    !!   mode =19
     &        'Convert from POL     to LAMBERT                  ',    !!   mode =20
     &        'Convert from LAMBERT to POL                      ',    !!   mode =21
     &        'Convert from TRM     to LAT-LON                  ',    !!   mode =22
     &        'Convert from LAT-LON to TRM                      ',    !!   mode =23
     &        'Convert from TRM     to UTM                      ',    !!   mode =24
     &        'Convert from UTM     to TRM                      ',    !!   mode =25
     &        'Convert from TRM     to LAMBERT                  ',    !!   mode =26
     &        'Convert from LAMBERT to TRM                      ',    !!   mode =27
     &        'Convert from TRM     to POL                      ',    !!   mode =28
     &        'Convert from POL     to TRM                      ',    !!   mode =29
     &        'Convert from EQM     to LAT-LON                  ',    !!   mode =30
     &        'Convert from LAT-LON to EQM                      ',    !!   mode =31
     &        'Convert from EQM     to UTM                      ',    !!   mode =32
     &        'Convert from UTM     to EQM                      ',    !!   mode =33
     &        'Convert from EQM     to LAMBERT                  ',    !!   mode =34
     &        'Convert from LAMBERT to EQM                      ',    !!   mode =35
     &        'Convert from EQM     to POL                      ',    !!   mode =36
     &        'Convert from POL     to EQM                      ',    !!   mode =37
     &        'Convert from EQM     to TRM                      ',    !!   mode =38
     &        'Convert from TRM     to EQM                      ',    !!   mode =39
     &        'Get LAT-LON grid corners from UTM specs          ',    !!   mode =30
     &        'Get UTM grid corners from LAT-LON specs          ',    !!   mode =41
     &        'Get LAT-LON grid corners from LAMBERT specs      ',    !!   mode =42
     &        'Get LAMBERT grid corners from LAT-LON specs      ',    !!   mode =43
     &        'Quit the program                                 '  /) !!   mode =44

        CHARACTER*60, PARAMETER :: PROMPT =
     &                  'What do you want to do next?'

        LOGICAL         LAMSET, POLSET, TRMSET, EQMSET
        DATA            LAMSET, POLSET, TRMSET, EQMSET / 4*.FALSE. /


C***********************************************************************
C   begin body of program UTMTOOL

        LOGDEV = INIT3()
        WRITE( *,92000 ) ' ',
     &'NOTICE:  Superseded BY "m3tools" program "projtool"',
     &' ',
     &'Program UTMTOOL to provide coordinate conversion back and',
     &'forth among LAT-LON, UTM, LAMBERT, POLAR STEREOGRAPHIC,',
     &'TRANSVERSE MERCATOR, and EQUATORIAL MERCATOR coordinate',
     &'systems.',
     &' ',
     &'Note that according to the standard, UTM coordinates should',
     &'be specified in _meters_ instead of the UAM/EPS bastardized ',
     &'system which claims to be UTM but in fact uses *kilo*meters;',
     &'the latter is a distinct (partially-supported) system. ',
     &' ',
     &'Longitudes are specified in _signed_degrees_ (so that for',
     &'the US longitudes are negative); coordinate system "UAM/EPS"',
     &'means the thing with the *kilometer* units instead of the',
     &'UTM standard units, which are meters.',
     &' ',
     &'Grid-corner calculations assume either a UTM-based or a',
     &'Lambert-based grid, as appropriate (to get UTM or Lambert ',
     &'corners of a LAT-LON based grid, work one corner at a time',
     &'using the coordinate conversion facilities). ',
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
     &'$Id:: utmtool.f 44 2014-09-12 18:03:16Z coats                 $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) )
     &      CALL M3EXIT( 'UTMTOOL', 0, 0, 'Exit at user request', 0 )

        MODE =    1
        ZONE =   17
        NCOL =   90
        NROW =   60
        LAT  =   30.0
        LON  =  -90.0
        XX   =   0.0
        YY   =   0.0
        CELL =   5.0E3

111     CONTINUE

            MODE = GETMENU( 44, MODE, PROMPT, MENU )


            IF ( MODE .EQ. 1 ) THEN        ! new Lambert by name


                CALL GETSTR( 'Enter Lambert projection name',
     &                       '<dummy>',
     &                       LAMNAME )

                LAMSET = LAMBERT( LAMNAME, A, B, C, X, Y )

                IF ( .NOT. LAMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 2 ) THEN        ! new POL by name


                CALL GETSTR( 'Enter Polar Stereographic proj name',
     &                       '<dummy>',
     &                       POLNAME )

                POLSET = POLSTE( POLNAME, A, B, C, X, Y )

                IF ( .NOT. POLSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 3 ) THEN        ! new POL by name


                CALL GETSTR( 'Enter Transverse Mercator proj name',
     &                       '<dummy>',
     &                       TRMNAME )

                TRMSET = TRMERC( TRMNAME, A, B, C, X, Y )

                IF ( .NOT. TRMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 4 ) THEN        ! new EQM by name


                CALL GETSTR( 'Enter Equatorial Mercator proj name',
     &                       '<dummy>',
     &                       EQMNAME )

                EQMSET = EQMERC( EQMNAME, A, B, C, X, Y )

                IF ( .NOT. EQMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 5 ) THEN        ! new Lambert by angles


                WRITE( *,92000 )
     &          ' ',
     &          'Lambert projection coordinate systems are defined',
     &          'by two secant (or tangent) latitudes ALPHA <= BETA',
     &          'a central meridian GAMMA, and the longitude XCENT',
     &          'and latitude YCENT of the coordinate origin (where',
     &          'Cartesian coordinates X=Y=0).',
     &          ' '
                A = GETREAL( -90.0, 90.0, A,
     &                        'Enter first  secant latitude ALPHA' )

                B = GETREAL(  -90.0, 90.0, MAX( A,B ),
     &                        'Enter second secant latitude  BETA' )

                C = GETREAL( -180.0, 180.0, C,
     &                        'Enter central longitude      GAMMA' )

                X = GETREAL( -180.0, 180.0, C,
     &                        'Enter origin longitude       XCENT' )


                IF ( Y .LT. A  .OR.  Y .GT. B ) THEN
                    Y = 0.5 * ( A + B )
                END IF

                Y = GETREAL( -90.0, 90.0, Y,
     &                        'Enter origin  latitude       YCENT' )

                LAMSET = SETLAM( A, B, C, X, Y )

                IF ( .NOT. LAMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 6 ) THEN        ! new POL by angles


                WRITE( *,92000 )
     &          ' ',
     &          'POLar Stereographic projection coordinate systems',
     &          'are defined by a pole (1 for N, 2 for S), a secant',
     &          'latitude BETA (the "latitude of true scale", a',
     &          'central meridian GAMMA, and the longitude XCENT and',
     &          'latitude YCENT of the coordinate origin (where,',
     &          'Cartesian coordinates X=Y=0).',
     &          ' '
                A = GETREAL( -1.0, 1.0, A,
     &                        'Enter 1 for N POLAR, -1 for S POLAR' )

                B = GETREAL(  -90.0, 90.0, MAX( A,B ),
     &                        'Enter secant latitude  BETA' )

                C = GETREAL( -180.0, 180.0, C,
     &                        'Enter central longitude      GAMMA' )

                X = GETREAL( -180.0, 180.0, C,
     &                        'Enter origin longitude       XCENT' )


                Y = GETREAL( -90.0, 90.0, Y,
     &                        'Enter origin  latitude       YCENT' )

                POLSET = SETPOL( A, B, C, X, Y )

                IF ( .NOT. POLSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 7 ) THEN        ! new TRM by angles


                WRITE( *,92000 )
     &          ' ',
     &          'TRansverse Mercator projection coordinate systems',
     &          'are defined by the latitude ALPHA of the origin,',
     &          'scale factor BETA at the central meridian, the',
     &          'longitude GAMMA of the central meridian, and the',
     &          'longitude XCENT and latitude YCENT of the coordinate',
     &          'origin (where Cartesian coordinates X=Y=0).',
     &          ' '
                A = GETREAL( -90.0, 90.0, A,
     &                        'Enter latitude of origin     ALPHA' )

                B = GETREAL(  0.0, 1.0, 1.0,
     &                        'Enter scale factor            BETA' )

                C = GETREAL( -180.0, 180.0, C,
     &                        'Enter central longitude      GAMMA' )

                X = GETREAL( -180.0, 180.0, C,
     &                        'Enter origin longitude       XCENT' )

                Y = GETREAL( -90.0, 90.0, A,
     &                        'Enter origin  latitude       YCENT' )

                TRMSET = SETTRM( A, B, C, X, Y )

                IF ( .NOT. TRMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 8 ) THEN        ! new EQM by angles


                WRITE( *,92000 )
     &          ' ',
     &          'EQuatorial Mercator projection coordinate systems',
     &          'are defined by the latitude ALPHA of true scale,',
     &          'the longitude GAMMA of the central meridian, and',
     &          'the longitude XCENT and latitude YCENT of the',
     &          'coordinate origin (where Cartesian coordinates',
     &          'X=Y=0).',
     &          ' '
                A = GETREAL( -90.0, 90.0, A,
     &                        'Enter latitude of true scale ALPHA' )

                B = 0.0

                C = GETREAL( -180.0, 180.0, C,
     &                        'Enter central longitude      GAMMA' )

                X = GETREAL( -180.0, 180.0, C,
     &                        'Enter origin longitude       XCENT' )

                Y = GETREAL( -90.0, 90.0, A,
     &                        'Enter origin  latitude       YCENT' )

                EQMSET = SETEQM( A, B, C, X, Y )

                IF ( .NOT. EQMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error getting projection; please try again' )
                END IF


            ELSE IF ( MODE .EQ. 9 ) THEN             !  convert from LL to UTM


                LAT = GETREAL( -90.0, 90.0, LAT,
     &                         'Enter input LATITUDE  (degrees)' )

                LON = GETREAL( -180.0, 180.0, LON,
     &                         'Enter input LONGITUDE (degrees)' )

                ZONE = INT( ( 180.0 + LON ) / 6.0 ) + 1
                ZONE = GETNUM( 1, 60, ZONE,
     &                         'Enter         UTM ZONE (1...60)' )

                CALL LL2UTM( LON, LAT, ZONE, XX, YY )

                WRITE( *,92010 )
     &              'Location LAT=', LAT, ' : LON=', LON, ' (deg)',
     &              '           X=', XX,  ' :   Y=', YY, ' (meters)'


            ELSE IF ( MODE .EQ. 10 ) THEN        ! convert from UTM to LL


                XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                        'Enter input X  (meters)' )

                YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                        'Enter input Y  (meters)' )

                ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter UTM ZONE (1...60)' )

                CALL UTM2LL( XX, YY, ZONE, LON, LAT )

                WRITE( *,92010 )
     &              'Location X=', XX,  ' :   Y=', YY,  ' (meters)',
     &              '       LON=', LON, ' : LAT=', LAT, ' (degrees)'


            ELSE IF ( MODE .EQ. 11 ) THEN        ! convert from EPS to LL


                XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                        'Enter input     X  (km)' )

                YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                        'Enter input     Y  (km)' )

                ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter UTM ZONE (1...60)' )

                CALL UTM2LL( 1000.0 * XX, 1000 * YY, ZONE, LON, LAT )

                WRITE( *,92010 )
     &              'Location X=', XX,  ' :   Y=', YY,  ' (km)',
     &              '       LON=', LON, ' : LAT=', LAT, ' (degrees)'


            ELSE IF ( MODE .EQ. 12 ) THEN        ! LAMBERT to UTM


                IF ( LAMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Lambert X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Lambert Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter output UTM ZONE (1...60)' )

                    IF ( LAM2UTM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'Lambert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  '    UTM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to UTM; please try again' )

                    END IF      !  if lam2utm succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &       'Lambert projection not initialized; please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 13 ) THEN        ! UTM to LAMBERT


                IF ( LAMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input UTM    X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input UTM    Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                            'Enter input UTM ZONE  (1...60)' )

                    IF ( UTM2LAM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  '    UTM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'Lambert X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAMBERT; please try again' )

                    END IF      !  if utm2lam succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Lambert projection not initialized; ' //
     &              'please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 14 ) THEN        ! LAMBERT to LAT-LON


                IF ( LAMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Lambert  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Lambert  Y  (meters)' )

                    IF ( LAM2LL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'Lambert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'LAT-LON X=',  X, ' :  Y=',  Y,  ' (degrees)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAT-LON; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'Lambert projection not initialized; please try again')

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 15 ) THEN        ! LAT-LON to LAMBERT


                IF ( LAMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Longitude X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Latitude  Y (degrees)' )


                    IF ( LL2LAM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'LAT-LON X=', XX, ' :  Y=', YY,  ' (degrees)',
     &                  'Lambert X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAMBERT; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'Lambert projection not initialized; please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 16 ) THEN        ! POL to LL


                IF ( POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter POL coords X (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter POL coords  Y (meters)' )


                    IF ( POL2LL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'POL coord X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'LL  coord X=',  X, ' :  Y=',  Y,  ' (degrees)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to Lat-Lon; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POL projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset and POLset,  or not


            ELSE IF ( MODE .EQ. 17 ) THEN        ! LL to POL


                IF ( POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Longitude X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Latitude  Y (degrees)' )

                    IF ( LL2POL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'LL  X=', XX, ' :  Y=', YY,  ' (degrees)',
     &                  'POL X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to POL; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POL projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset or not



            ELSE IF ( MODE .EQ. 18 ) THEN        ! POL to UTM


                IF ( POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input POL coord X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input POL coord Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter output UTM ZONE (1...60)' )

                    IF ( POL2UTM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'POL X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'UTM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to UTM; please try again' )

                    END IF      !  if POL2utm succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'POL projection not initialized; please try again' )

                END IF          !  if POLset or not


            ELSE IF ( MODE .EQ. 19 ) THEN        ! UTM to POL


                IF ( POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input UTM    X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input UTM    Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                            'Enter input UTM ZONE  (1...60)' )

                    IF ( UTM2POL( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'UTM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'POL X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to POLar; please try again' )

                    END IF      !  if utm2lam succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'POL projection not initialized; ' //
     &              'please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 20 ) THEN        ! POL to LAMBERT


                IF ( LAMSET .AND. POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter POL coords X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter POL coords  Y (degrees)' )


                    IF ( POL2LAM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'POL     X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'Lambert X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAMBERT; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not
                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POL projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset and POLset,  or not


            ELSE IF ( MODE .EQ. 21 ) THEN        ! LAMBERT to POL


                IF ( LAMSET .AND. POLSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Lambert  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Lambert  Y  (meters)' )

                    IF ( LAM2POL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'Lambert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'POL     X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to POL; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POL projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 21 ) THEN        ! TRM to LAT-LON


                IF ( TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input TRM  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input TRM  Y  (meters)' )

                    IF ( TRM2LL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRMbert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'LAT-LON X=',  X, ' :  Y=',  Y,  ' (degrees)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAT-LON; please try again' )

                    END IF      !  if TRM2ll succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'TRM projection not initialized; please try again')

                END IF          !  if TRMset or not


            ELSE IF ( MODE .EQ. 22 ) THEN        ! TRM to LL


                IF ( TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter TRM coords X (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter TRM coords  Y (meters)' )


                    IF ( TRM2LL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRM coord X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'LL  coord X=',  X, ' :  Y=',  Y,  ' (degrees)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to Lat-Lon; please try again' )

                    END IF      !  if trm2ll succeeded, or not

                ELSE

                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset and POLset,  or not


            ELSE IF ( MODE .EQ. 23 ) THEN        ! LAT-LON to TRM


                IF ( TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Longitude X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Latitude  Y (degrees)' )


                    IF ( LL2TRM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'LAT-LON X=', XX, ' :  Y=', YY,  ' (degrees)',
     &                  'TRM     X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to TRM; please try again' )

                    END IF      !  if ll2TRM succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'TRM projection not initialized; please try again' )

                END IF          !  if TRMset or not


            ELSE IF ( MODE .EQ. 24 ) THEN        ! TRM to UTM


                IF ( TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input TRM coord X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input TRM coord Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter output UTM ZONE (1...60)' )

                    IF ( TRM2UTM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'UTM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to UTM; please try again' )

                    END IF      !  if TRM2utm succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'TRM projection not initialized; please try again' )

                END IF          !  if TRMset or not


            ELSE IF ( MODE .EQ. 25 ) THEN        ! UTM to TRM


                IF ( TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input UTM    X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input UTM    Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                            'Enter input UTM ZONE  (1...60)' )

                    IF ( UTM2TRM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'UTM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'TRM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to TRMar; please try again' )

                    END IF      !  if utm2lam succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'TRM projection not initialized; ' //
     &              'please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 26 ) THEN        ! TRM to LAMBERT


                IF ( LAMSET .AND. TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter TRM coords X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter TRM coords  Y (degrees)' )


                    IF ( TRM2LAM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRM     X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'Lambert X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAMBERT; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not
                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset and trmset,  or not


            ELSE IF ( MODE .EQ. 27 ) THEN        ! LAMBERT to TRM


                IF ( LAMSET .AND. TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Lambert  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Lambert  Y  (meters)' )

                    IF ( LAM2TRM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'Lambert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'TRM     X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to TRM; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 28 ) THEN        ! TRM to POL


                IF ( POLSET .AND. TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter POLar coords X (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter POLar coords Y (meters)' )


                    IF ( TRM2POL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRM  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'POL  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to POL; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not
                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                END IF          !  if POLSET and trmset,  or not


            ELSE IF ( MODE .EQ. 29 ) THEN        ! POL to TRM


                IF ( POLSET .AND. TRMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input POLar  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input POLar  Y  (meters)' )

                    IF ( POL2TRM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'POL  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'TRM  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to TRM; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POLar projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                END IF          !  if POLSET or not


            ELSE IF ( MODE .EQ. 30 ) THEN        ! EQM to LAT-LON


                IF ( EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input EQM  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input EQM  Y  (meters)' )

                    IF ( EQM2LL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'EQMbert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'LAT-LON X=',  X, ' :  Y=',  Y,  ' (degrees)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAT-LON; please try again' )

                    END IF      !  if EQM2ll succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'EQM projection not initialized; please try again')

                END IF          !  if EQMset or not


            ELSE IF ( MODE .EQ. 31 ) THEN        ! LAT-LON to EQM


                IF ( EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Longitude X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Latitude  Y (degrees)' )


                    IF ( LL2EQM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'LAT-LON X=', XX, ' :  Y=', YY,  ' (degrees)',
     &                  'EQM     X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to EQM; please try again' )

                    END IF      !  if ll2EQM succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'EQM projection not initialized; please try again' )

                END IF          !  if EQMset or not


            ELSE IF ( MODE .EQ. 32 ) THEN        ! EQM to UTM


                IF ( EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input EQM coord X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input EQM coord Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter output UTM ZONE (1...60)' )

                    IF ( EQM2UTM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'EQM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'UTM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to UTM; please try again' )

                    END IF      !  if EQM2utm succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'EQM projection not initialized; please try again' )

                END IF          !  if EQMset or not


            ELSE IF ( MODE .EQ. 33 ) THEN        ! UTM to EQM


                IF ( EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input UTM    X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input UTM    Y  (meters)' )

                    ZONE = GETNUM( 1, 60, ZONE,
     &                            'Enter input UTM ZONE  (1...60)' )

                    IF ( UTM2EQM( XX, YY, ZONE, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'UTM X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'EQM X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to EQMar; please try again' )

                    END IF      !  if utm2lam succeeded, or not

                ELSE

                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'EQM projection not initialized; ' //
     &              'please try again' )

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 34 ) THEN        ! EQM to LAMBERT


                IF ( LAMSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter EQM coords X (degrees)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter EQM coords  Y (degrees)' )


                    IF ( EQM2LAM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'EQM     X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'Lambert X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to LAMBERT; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not
                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset and trmset,  or not


            ELSE IF ( MODE .EQ. 35 ) THEN        ! LAMBERT to EQM


                IF ( LAMSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input Lambert  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input Lambert  Y  (meters)' )

                    IF ( LAM2EQM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'Lambert X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'EQM     X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to EQM; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. LAMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if lamset or not

                END IF          !  if lamset or not


            ELSE IF ( MODE .EQ. 36 ) THEN        ! EQM to POL


                IF ( POLSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter POLar coords X (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter POLar coords Y (meters)' )


                    IF ( EQM2POL( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'EQM  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'POL  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to POL; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not
                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                END IF          !  if POLSET and trmset,  or not


            ELSE IF ( MODE .EQ. 37 ) THEN        ! POL to EQM


                IF ( POLSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input POLar  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input POLar  Y  (meters)' )

                    IF ( POL2EQM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'POL  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'EQM  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to EQM; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. POLSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'POLar projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if POLSET or not

                END IF          !  if POLSET or not


            ELSE IF ( MODE .EQ. 38 ) THEN        ! EQM to TRM


                IF ( TRMSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter TRM coords X (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter TRM coords Y (meters)' )


                    IF ( EQM2TRM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'EQM  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'TRM  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &              'Error converting to TRM; please try again' )

                    END IF      !  if ll2lam succeeded, or not

                ELSE

                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Lambert projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if TRMSET or not
                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if TRMSET or not

                END IF          !  if eqmSET and trmset,  or not


            ELSE IF ( MODE .EQ. 39 ) THEN        ! TRM to EQM


                IF ( TRMSET .AND. EQMSET ) THEN

                    XX = GETREAL( BADVAL3, -BADVAL3, XX,
     &                            'Enter input TRM  X  (meters)' )

                    YY = GETREAL( BADVAL3, -BADVAL3, YY,
     &                            'Enter input TRM  Y  (meters)' )

                    IF ( TRM2EQM( XX, YY, X, Y ) ) THEN

                        WRITE( *,92010 )
     &                  'TRM  X=', XX, ' :  Y=', YY,  ' (meters)',
     &                  'EQM  X=',  X, ' :  Y=',  Y,  ' (meters)'

                    ELSE

                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'Error converting to EQM; please try again' )

                    END IF      !  if lam2ll succeeded, or not

                ELSE

                    IF ( .NOT. TRMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'TRM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if TRMSET or not

                    IF ( .NOT. EQMSET ) THEN
                        CALL M3WARN( 'UTMTOOL', 0, 0,
     &                  'EQM projection not initialized; ' //
     &                  'please try again' )
                    END IF          !  if EQMSET or not

                END IF          !  if TRMSET and EQMSET, or not


            ELSE IF ( MODE .EQ. 40 ) THEN     ! get LL grid corners from UTM


                XLL = GETREAL( BADVAL3, -BADVAL3, XX,
     &                         'Enter lower-left corner X (meters)' )

                YLL = GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter lower-left corner Y  (meters)' )

                CELL= GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter    cell-size DX(=DY) (meters)' )

                ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter             UTM ZONE (1...60)' )

                NCOL = GETNUM( 1, 999, NCOL,
     &                        'Enter number of        grid COLUMNS' )

                NROW = GETNUM( 1, 999, NROW,
     &                        'Enter number of        grid    ROWS' )

                XUR  = XLL + FLOAT( NCOL ) * CELL
                YUR  = YLL + FLOAT( NROW ) * CELL

                CALL UTM2LL( XLL, YLL, ZONE, LON, LAT )

                WRITE( *,92000 ) ' '
                WRITE( *,92020 )
     &              'LL corner X=', XLL, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                CALL UTM2LL( XLL, YUR, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'UL corner X=', XLL, ' :   Y=', YUR,' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                CALL UTM2LL( XUR, YLL, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'LR corner X=', XUR, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'
                WRITE( *,92000 ) ' '

                CALL UTM2LL( XUR, YUR, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'UR corner X=', XUR, ' :   Y=', YUR, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'


            ELSE IF ( MODE .EQ. 41 ) THEN        ! get UTM grid corners from LL


                LON = GETREAL( BADVAL3, -BADVAL3, XX,
     &                         'Enter lower-left corner LON (degrees)' )

                LAT = GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter lower-left corner LAT (degrees)' )

                CALL LL2UTM( LON, LAT, ZONE, XLL, YLL )

                CELL= GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter    cell-size DX(=DY) (degrees)' )

                ZONE = GETNUM( 1, 60, ZONE,
     &                        'Enter             UTM ZONE (1...60)' )

                NCOL = GETNUM( 1, 999, NCOL,
     &                        'Enter number of        grid COLUMNS' )

                NROW = GETNUM( 1, 999, NROW,
     &                        'Enter number of        grid    ROWS' )

                XUR  = XLL + FLOAT( NCOL ) * CELL
                YUR  = YLL + FLOAT( NROW ) * CELL

                CALL LL2UTM( XLL, YLL, ZONE, LON, LAT )

                WRITE( *,92000 ) ' '
                WRITE( *,92020 )
     &              'LL corner X=', XLL, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                CALL LL2UTM( XLL, YUR, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'UL corner X=', XLL, ' :   Y=', YUR,' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                CALL LL2UTM( XUR, YLL, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'LR corner X=', XUR, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'
                WRITE( *,92000 ) ' '

                CALL LL2UTM( XUR, YUR, ZONE, LON, LAT )

                WRITE( *,92020 )
     &              'UR corner X=', XUR, ' :   Y=', YUR, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'


            ELSE IF ( MODE .EQ. 42 ) THEN        ! get LL corners from LAMBERT


                IF ( .NOT. LAMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'Lambert projection not initialized; please try again' )
                    GO TO 111
                END IF          !  if lamset or not

                XLL = GETREAL( BADVAL3, -BADVAL3, XX,
     &                         'Enter lower-left corner X (meters)' )

                YLL = GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter lower-left corner Y  (meters)' )

                CELL= GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter    cell-size DX(=DY) (meters)' )

                NCOL = GETNUM( 1, 999, NCOL,
     &                        'Enter number of        grid COLUMNS' )

                NROW = GETNUM( 1, 999, NROW,
     &                        'Enter number of        grid    ROWS' )

                XUR  = XLL + FLOAT( NCOL ) * CELL
                YUR  = YLL + FLOAT( NROW ) * CELL

                IF ( .NOT. LAM2LL( XLL, YLL, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92000 ) ' '
                WRITE( *,92020 )
     &              'LL corner X=', XLL, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XLL, YUR, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'UL corner X=', XLL, ' :   Y=', YUR,' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XUR, YLL, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'LR corner X=', XUR, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XUR, YUR, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'UR corner X=', XUR, ' :   Y=', YUR, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'
                WRITE( *,92000 ) ' '


            ELSE IF ( MODE .EQ. 43 ) THEN        ! get Lambert corners from LL


                IF ( .NOT. LAMSET ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'Lambert projection not initialized; please try again' )
                    GO TO 111
                END IF          !  if lamset or not

                LON = GETREAL( BADVAL3, -BADVAL3, XX,
     &                         'Enter lower-left corner LON (degrees)' )

                LAT = GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter lower-left corner LAT (degrees)' )

                IF ( .NOT. LL2LAM( LON, LAT, XLL, YLL ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF


                CELL= GETREAL( BADVAL3, -BADVAL3, YY,
     &                         'Enter    cell-size DX(=DY) (meters)' )

                NCOL = GETNUM( 1, 999, NCOL,
     &                        'Enter number of        grid COLUMNS' )

                NROW = GETNUM( 1, 999, NROW,
     &                        'Enter number of        grid    ROWS' )

                XUR  = XLL + FLOAT( NCOL ) * CELL
                YUR  = YLL + FLOAT( NROW ) * CELL

                IF ( .NOT. LAM2LL( XLL, YLL, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92000 ) ' '
                WRITE( *,92020 )
     &              'LL corner X=', XLL, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XLL, YUR, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'UL corner X=', XLL, ' :   Y=', YUR,' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XUR, YLL, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'LR corner X=', XUR, ' :   Y=', YLL, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                IF ( .NOT. LAM2LL( XUR, YUR, LON, LAT ) ) THEN
                    CALL M3WARN( 'UTMTOOL', 0, 0,
     &                           'Coordinate transformation error' )
                    GO TO 111
                END IF

                WRITE( *,92020 )
     &              'UR corner X=', XUR, ' :   Y=', YUR, ' (meters)',
     &              '        LON=', LON, ' : LAT=', LAT, ' (degrees)'

                WRITE( *,92000 ) ' '


            ELSE IF ( MODE .EQ. 44 ) THEN    ! ...then quit:

                GO TO 999       !  to program shut-down

            ELSE !  by construction of menu, should not be able to get to here

                CALL M3WARN( 'UTMTOOL', 0, 0,
     &          'Requested conversion not yet implemented' )

            END IF

            GO TO 111   !  to head of loop


999     CONTINUE
        CALL M3EXIT( 'UTMTOOL', 0, 0,
     &             'Successful completion of program UTMTOOL', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( /5X , A, F15.4, A, F15.4, A,
     &           /5X , A, F15.4, A, F15.4, A, / )

92020   FORMAT (  5X , A, F15.4, A, F15.4, A,
     &           /5X , A, F15.4, A, F15.4, A )


        END PROGRAM UTMTOOL

