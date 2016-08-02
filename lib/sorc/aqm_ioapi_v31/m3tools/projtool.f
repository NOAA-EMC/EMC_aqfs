
        PROGRAM PROJTOOL

C***********************************************************************
C Version "$Id: projtool.f 74 2014-12-08 16:14:06Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1997-2013 Carlie J. Coats, Jr.,
C (C) 2002-2012 Baron Advanced Meteorological Systems. LLC., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  150
C
C  DESCRIPTION:
C       Perform coordinate conversions and grid-related computations
C       for I/O API supported coordinate systems.
C
C  PRECONDITIONS REQUIRED:
C       "setenv GRIDDESC <pathname>" for using map projections by name.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETMENU, GETNUM, GETDBLE, GETSTR, GETYN
C       USGS-derived GCTP package
C
C  REVISION  HISTORY:
C       Adapted 11/2002 by CJC from UTMTOOL (which it supersedes).
C       Version 06/2008 by CJC:  add Albers support
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 04/2011 by CJC:  bug-fixes; add <X,Y>-to-<C,R> operation
C       Version 05/2011 by CJC:  bug-fixes
C       Version 10/2012 by CJC:  bug-fix in M3MESG calls for cases 10,11
C       Version  8/2014 by CJC:  need to call SETPROJ() for cases 6,7
C       Version 12/2014 by CJC:  bug-fix in SHOWGRID(), menu-display;
C       new "define output grid" menu item.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        REAL*8,       PARAMETER :: BAD   = -9.999D36
        CHARACTER*16, PARAMETER :: PNAME = 'PROJTOOL'
        CHARACTER*72, PARAMETER :: BAR   =
     &  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

        CHARACTER*60, PARAMETER :: CHOICES( 13 ) =
     & (/
     & 'Quit the program                                        ',    !!  mode =  1
     & 'Set up input  map projection by name                    ',    !!  mode =  2
     & 'Set up output map projection by name                    ',    !!  mode =  3
     & 'Set up input  grid &  projection by name                ',    !!  mode =  4
     & 'Set up output grid &  projection by name                ',    !!  mode =  5
     & 'Set up input  map projection by angles (etc.)           ',    !!  mode =  6
     & 'Set up output map projection by angles (etc.)           ',    !!  mode =  7
     & 'Define an input grid w.r.t. the input map projection    ',    !!  mode =  8
     & 'Convert from input X-Y to output X-Y                    ',    !!  mode =  9
     & 'Get output-projection grid corners for input grid       ',    !!  mode = 10
     & 'Get output-projection grid-corner-cell centers          ',    !!  mode = 11
     & 'Define an output grid w.r.t. the output map projection  ',    !!  mode = 12
     & 'Get output-grid <C,R> for input-coord <X,Y>             '     !!  mode = 13
     & /)

        CHARACTER*60, PARAMETER :: CPROMPT =
     & 'What operation do you want to do next?'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV
        INTEGER         MODE, NMODES
        INTEGER         ZONE
        REAL*8          X00, Y00, X01, Y01, X10, Y10, X11, Y11
        REAL*8          U00, V00, U01, V01, U10, V10, U11, V11
        REAL*8          XX, YY
        INTEGER         CC, RR
        CHARACTER*256   MESG
        LOGICAL         EFLAG

C.......   Arguments for GTPZ0:

        REAL*8          DSCR        !  scratch variables
        INTEGER         DEG, MNT    !  scratch variables
        REAL*8          CRDIN( 2 )  !  input coordinates ,y
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
        INTEGER*4       IOSPH       !  spheroid code (unused)

        INTEGER*4       LN27        !  NAD1927 file unit number
        INTEGER*4       LN83        !  NAD1983 file unit number
        CHARACTER*128   FN27        !  NAD1927 file name
        CHARACTER*128   FN83        !  NAD1983 file name
        INTEGER*4       LENGTH      !  NAD* record-length
        INTEGER*4       IFLG        !  error flag

        CHARACTER*16    PRNAM       ! GRIDDESC projection name
        INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0

        INTEGER         GDTYP2      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8          P_ALP2      ! first, second, third map
        REAL*8          P_BET2      ! projection descriptive
        REAL*8          P_GAM2      ! parameters.
        REAL*8          XCENT2      ! lon for coord-system X=0
        REAL*8          YCENT2      ! lat for coord-system Y=0

        CHARACTER*16    GDNAM       ! GRIDDESC grid name
        INTEGER         NCOLS1      ! number of grid columns
        INTEGER         NROWS1      ! number of grid rows
        INTEGER         NTHIK1      ! boundary thickness (cells)
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension

        INTEGER         NCOLS2      ! number of grid columns
        INTEGER         NROWS2      ! number of grid rows
        INTEGER         NTHIK2      ! boundary thickness (cells)
        REAL*8          XORIG2      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG2      ! Y-coordinate origin of grid
        REAL*8          XCELL2      ! X-coordinate cell dimension
        REAL*8          YCELL2      ! Y-coordinate cell dimension

        CHARACTER*16    LAMNAME, POLNAME, TRMNAME, EQMNAME
        DATA            LAMNAME, POLNAME, TRMNAME, EQMNAME / 4*CMISS3 /

        LOGICAL         INSET, OUTSET, INGRID, OUTGRID
        DATA            INSET, OUTSET, INGRID, OUTGRID / 4*.FALSE. /


C***********************************************************************
C   begin body of program PROJTOOL

        LOGDEV = INIT3()
        WRITE( *,'( 5X, A )' )
     &' ',
     &'Program PROJTOOL to provide coordinate conversion back and',
     &'forth among LAT-LON, UTM, LAMBERT, POLAR STEREOGRAPHIC,',
     &'TRANSVERSE MERCATOR, and EQUATORIAL MERCATOR coordinate',
     &'systems.',
     &' ',
     &'Note that according to the standard, UTM coordinates should',
     &'be specified in _meters_ instead of the UAM/EPS bastardized ',
     &'system which claims to be UTM but in fact uses *kilo*meters. ',
     &' ',
     &'Longitudes are specified in _signed_degrees_ (so that for',
     &'the US longitudes are negative).  NOTE that this is in',
     &'conformance with ISO STANDARD 6709, *not* the so-called',
     &'WMO "standard" for representation of longitudes.',
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
     &'$Id:: projtool.f 74 2014-12-08 16:14:06Z coats                $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) )
     &      CALL M3EXIT( PNAME, 0, 0, 'Exit at user request', 0 )

        MODE =    1
        NCOLS1 = 100
        NROWS1 = 100
        XORIG1 = 0.0D0
        YORIG1 = 0.0D0
        XCELL1 = 5.0D0
        YCELL1 = 5.0D0

111     CONTINUE        !  head of event loop

            IF ( INSET ) THEN
                CALL SHOWPROJ( 'Input map projection parameters',
     &                         GDTYP1, P_ALP1, P_BET1, P_GAM1,
     &                         XCENT1, YCENT1 )
            END IF

            IF ( OUTSET ) THEN
                CALL SHOWPROJ( 'Output map projection parameters',
     &                         GDTYP2, P_ALP2, P_BET2, P_GAM2,
     &                         XCENT2, YCENT2 )
            END IF

            IF ( INGRID ) THEN
                CALL SHOWGRID( 'Input grid parameters',
     &                         NCOLS1, NROWS1,
     &                         XORIG1, YORIG1, XCELL1, YCELL1 )
            END IF

            IF ( OUTGRID ) THEN
                CALL SHOWGRID( 'Output grid parameters',
     &                         NCOLS2, NROWS2,
     &                         XORIG2, YORIG2, XCELL2, YCELL2 )
            END IF

            IF ( OUTGRID ) THEN
                NMODES = 13
            ELSE IF ( INGRID ) THEN
                NMODES = 12
            ELSE IF ( INSET .AND. OUTSET ) THEN
                NMODES = 12
            ELSE
                NMODES = 7
            END IF

            MODE = GETMENU( NMODES, 1+MOD( MODE, NMODES ),
     &                      CPROMPT, CHOICES )

            IF ( MODE .EQ. 1 ) THEN             ! exit

                GO TO  999                      ! exit program

            ELSE IF ( MODE .EQ. 2 ) THEN        ! new input proj by name

                MESG = 'Enter GRIDDESC name for input projection'
                CALL GETSTR( MESG, 'LATLON', PRNAM )
                IF ( DSCOORD( PRNAM, GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1,
     &                        XCENT1, YCENT1 ) ) THEN
                    IF ( SETPROJ( GDTYP1,
     &                            P_ALP1, P_BET1, P_GAM1,
     &                            XCENT1, YCENT1,
     &                            INSYS,  INZONE,
     &                            TPAIN, INUNIT, INSPH ) ) THEN
                        INSET = .TRUE.
                    END IF
                ELSE
                    MESG = 'Map projection "' // TRIM( PRNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 3  ) THEN       ! new output proj by name

                MESG = 'Enter GRIDDESC name for output projection'
                CALL GETSTR( MESG, 'LATLON', PRNAM )
                IF ( DSCOORD( PRNAM, GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2,
     &                        XCENT2, YCENT2 ) ) THEN
                    IF ( SETPROJ( GDTYP2,
     &                            P_ALP2, P_BET2, P_GAM2,
     &                            XCENT2, YCENT2,
     &                            IOSYS,  IOZONE,
     &                            TPOUT, IOUNIT, IOSPH ) ) THEN
                        OUTSET = .TRUE.
                    END IF
                ELSE
                    MESG = 'Map projection "' // TRIM( PRNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 4  ) THEN       ! new  input grid by name

                MESG = 'Enter GRIDDESC name for input grid'
                CALL GETSTR( MESG, 'LATLON', GDNAM )
                IF ( DSCGRID( GDNAM, PRNAM, GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                        XORIG1, YORIG1, XCELL1, YCELL1,
     &                        NCOLS1, NROWS1, NTHIK1 ) ) THEN
                    IF ( SETPROJ( GDTYP1,
     &                            P_ALP1, P_BET1, P_GAM1,
     &                            XCENT1, YCENT1,
     &                            INSYS,  INZONE,
     &                            TPAIN, INUNIT, INSPH ) ) THEN
                        INSET   = .TRUE.
                        INGRID = .TRUE.
                    END IF
                ELSE
                    MESG = 'Grid "' // TRIM( GDNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 5  ) THEN       ! new output grid by name

                MESG = 'Enter GRIDDESC name for output grid'
                CALL GETSTR( MESG, 'LATLON', GDNAM )
                IF ( DSCGRID( GDNAM, PRNAM, GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,
     &                        XORIG2, YORIG2, XCELL2, YCELL2,
     &                        NCOLS2, NROWS2, NTHIK2 ) ) THEN
                    IF ( SETPROJ( GDTYP2,
     &                            P_ALP2, P_BET2, P_GAM2,
     &                            XCENT2, YCENT2,
     &                            IOSYS,  IOZONE,
     &                            TPOUT, IOUNIT, IOSPH ) ) THEN
                        OUTSET   = .TRUE.
                        OUTGRID  = .TRUE.
                    END IF
                ELSE
                    MESG = 'Grid "' // TRIM( GDNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 6  ) THEN       ! new  input proj by angles

                CALL GETPROJ( GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1 )
                IF ( SETPROJ( GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1,
     &                        XCENT1, YCENT1,
     &                        INSYS,  INZONE,
     &                        TPAIN, INUNIT, INSPH ) ) THEN
                    INSET  = .TRUE.
                END IF

            ELSE IF ( MODE .EQ. 7  ) THEN       ! new output proj by angles

                CALL GETPROJ( GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2 )
                IF ( SETPROJ( GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2,
     &                        XCENT2, YCENT2,
     &                        IOSYS,  IOZONE,
     &                        TPOUT, IOUNIT, IOSPH ) ) THEN
                    OUTSET   = .TRUE.
                END IF

            ELSE IF ( MODE .EQ. 8  ) THEN       ! new input grid

                CALL GETGRID( XORIG1, YORIG1, XCELL1, YCELL1,
     &                        NCOLS1, NROWS1 )
                INGRID = .TRUE.

            ELSE IF ( MODE .EQ. 9  ) THEN       ! Coordinate conversion

                CRDIN( 1 ) = GETDBLE( BAD, -BAD, CRDIN( 1 ),
     &                                'Enter input coord X' )

                CRDIN( 2 ) = GETDBLE( BAD, -BAD, CRDIN( 2 ),
     &                                'Enter input coord Y' )

                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                    CALL M3MESG( BAR )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &                 'Input   X = ', CRDIN( 1 ),
     &                 'Input   Y = ', CRDIN( 2 )
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &                 'Output  X = ', CRDIO( 1 ),
     &                 'Output  Y = ', CRDIO( 2 )
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    CALL M3MESG( BAR )
                ELSE
                    IFLG = MAX( MIN( 9, IFLG ), 1 )
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 10 ) THEN       ! compute grid corners

                IF ( .NOT.INGRID ) THEN
                    CALL M3MESG( 'You must define input grid first' )
                    GO TO  111      !! to head of event-loop
                END IF

                X00 = XORIG1
                Y00 = YORIG1
                X01 = XORIG1
                Y01 = YORIG1 + DBLE( NROWS1 )*YCELL1
                X10 = XORIG1 + DBLE( NCOLS1 )*XCELL1
                Y10 = YORIG1
                X11 = XORIG1 + DBLE( NCOLS1 )*XCELL1
                Y11 = YORIG1 + DBLE( NROWS1 )*YCELL1

                EFLAG = .FALSE.
                CRDIN( 1 ) = X00
                CRDIN( 2 ) = Y00
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U00 = CRDIO( 1 )
                   V00 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X01
                CRDIN( 2 ) = Y01
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U01 = CRDIO( 1 )
                   V01 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X10
                CRDIN( 2 ) = Y10
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U10 = CRDIO( 1 )
                   V10 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X11
                CRDIN( 2 ) = Y11
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U11 = CRDIO( 1 )
                   V11 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                IF ( EFLAG ) THEN

                    MESG = 'Error in coordinate transformations'
                    CALL M3WARN( PNAME, 0, 0, MESG )

                ELSE

                    CALL M3MESG( BAR )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj SW corner (X,Y) = (',
     &                  X00, ',', Y00, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj SW corner (X,Y) = (',
     &                  U00, ',', V00, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj SE corner (X,Y) = (',
     &                  X01, ',', Y01, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj SE corner (X,Y) = (',
     &                  U01, ',', V01, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj NW corner (X,Y) = (',
     &                  X10, ',', Y10, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj NW corner (X,Y) = (',
     &                  U10, ',', V10, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj NE corner (X,Y) = (',
     &                  X11, ',', Y11, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj NE corner (X,Y) = (',
     &                  U11, ',', V11, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    CALL M3MESG( BAR )

                END IF


            ELSE IF ( MODE .EQ. 11 ) THEN       ! compute corner-cell centers

                IF ( .NOT.INGRID ) THEN
                    CALL M3MESG( 'You must define input grid first' )
                    GO TO  111      !! to head of event-loop
                END IF

                X00 = XORIG1 + 0.5D0*XCELL1
                Y00 = YORIG1 + 0.5D0*YCELL1
                X01 = X00
                Y01 = Y00 + DBLE( NROWS1 - 1 )*YCELL1
                X10 = X00 + DBLE( NCOLS1 - 1 )*XCELL1
                Y10 = Y00
                X11 = X00 + DBLE( NCOLS1 - 1 )*XCELL1
                Y11 = Y00 + DBLE( NROWS1 - 1 )*YCELL1

                EFLAG = .FALSE.
                CRDIN( 1 ) = X00
                CRDIN( 2 ) = Y00
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U00 = CRDIO( 1 )
                   V00 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X01
                CRDIN( 2 ) = Y01
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U01 = CRDIO( 1 )
                   V01 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X10
                CRDIN( 2 ) = Y10
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U10 = CRDIO( 1 )
                   V10 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                CRDIN( 1 ) = X11
                CRDIN( 2 ) = Y11
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                   U11 = CRDIO( 1 )
                   V11 = CRDIO( 2 )
                ELSE
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3MESG( MESG )
                END IF

                IF ( EFLAG ) THEN

                    MESG = 'Error in coordinate transformations'
                    CALL M3WARN( PNAME, 0, 0, MESG )

                ELSE

                    CALL M3MESG( BAR )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj SW cell-center (X,Y) = (',
     &                  X00, ',', Y00, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj SW cell-center (X,Y) = (',
     &                  U00, ',', V00, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj SE cell-center (X,Y) = (',
     &                  X01, ',', Y01, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj SE cell-center (X,Y) = (',
     &                  U01, ',', V01, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj NW cell-center (X,Y) = (',
     &                  X10, ',', Y10, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj NW cell-center (X,Y) = (',
     &                  U10, ',', V10, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Input  proj NE cell-center (X,Y) = (',
     &                  X11, ',', Y11, ')'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &                  'Output proj NE cell-center (X,Y) = (',
     &                  U11, ',', V11, ')'
                    CALL M3MESG( MESG )
                    CALL M3MESG( ' ' )
                    CALL M3MESG( BAR )

                END IF

            ELSE IF ( MODE .EQ. 12 ) THEN       ! new output grid

                CALL GETGRID( XORIG2, YORIG2, XCELL2, YCELL2,
     &                        NCOLS2, NROWS2 )
                OUTGRID = .TRUE.

            ELSE IF ( MODE .EQ. 13 ) THEN       ! Coordinate-to-grid conversion

                CRDIN( 1 ) = GETDBLE( BAD, -BAD, CRDIN( 1 ),
     &                                'Enter input coord X' )

                CRDIN( 2 ) = GETDBLE( BAD, -BAD, CRDIN( 2 ),
     &                                'Enter input coord Y' )

                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,
     &                      INSPH,IPR, JPR, LEMSG, LPARM,
     &                      CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,
     &                      LN27, LN83, FN27, FN83, LENGTH, IFLG )

                IF ( IFLG .EQ. 0 ) THEN
                    XX = ( CRDIO( 1 ) - XORIG2 ) / XCELL2
                    YY = ( CRDIO( 2 ) - YORIG2 ) / YCELL2
                    IF ( XX .GE. 0.0D0 ) THEN
                        CC = 1 + INT( XX )
                    ELSE
                        CC = -INT( -XX )
                    END IF
                    IF ( YY .GE. 0.0D0 ) THEN
                        RR = 1 + INT( YY )
                    ELSE
                        RR = -INT( -YY )
                    END IF


                    CALL M3MESG( BAR )
                    WRITE( MESG, '( 2( A, F25.14 , :, 2X ) )' )
     &                 'Input   X = ', CRDIN( 1 ),
     &                 'Input   Y = ', CRDIN( 2 )
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &                 'Output  X = ', CRDIO( 1 ),
     &                 'Output  Y = ', CRDIO( 2 )
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( A, F25.14, 2X, A, I5 )' )
     &                 'Output  grid-normal X = ', XX, 'col =', CC
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( A, F25.14, 2X, A, I5 )' )
     &                 'Output  grid-normal Y = ', YY, 'row =', RR
                    CALL M3MESG( MESG )
                    CALL M3MESG( BAR )
                ELSE
                    IFLG = MAX( MIN( 9, IFLG ), 1 )
                    WRITE( MESG, '( A , I3 )' ) 'GCTP error', IFLG
                    CALL M3WARN( 'PROJTOOL', 0, 0, MESG )
                END IF

            ELSE                                ! error

                MESG = 'Unrecognized choice'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            END IF                              !!  if mode=1,2,...,10, or not

            GO TO 111   !  to head of event loop


999     CONTINUE

        MESG = 'Successful completion of program PROJTOOL'

        CALL M3EXIT( PNAME, 0, 0, MESG, 0 )


      CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


          LOGICAL FUNCTION SETPROJ( GDTYP,
     &                              ALPHA, BETA, GAMMA, XCENT, YCENT,
     &                              KSYS,  KZONE, TPA, KUNIT, KSPH )

          !*********************************************************
          !  FUNCTION:
          !       set up arguments for GCTP package
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          INTEGER*4, INTENT(IN   ) :: GDTYP  !  I/O API map projection code (input)
          REAL*8   , INTENT(IN   ) :: ALPHA  !  map projection parameter    (input)
          REAL*8   , INTENT(IN   ) :: BETA   !  map projection parameter    (input)
          REAL*8   , INTENT(IN   ) :: GAMMA  !  map projection parameter    (input)
          REAL*8   , INTENT(IN   ) :: XCENT  !  Cartesian origin longitude  (input)
          REAL*8   , INTENT(IN   ) :: YCENT  !  Cartesian origin latitude   (input)

          INTEGER*4, INTENT(  OUT) :: KSYS        !  GCTP projection code   (output)
          INTEGER*4, INTENT(INOUT) :: KZONE       !  zone-ID or UTM zone    (input/output)
          REAL*8   , INTENT(  OUT) :: TPA( 15 )   !  projection parameters  (output)
          INTEGER*4, INTENT(  OUT) :: KUNIT       !  units code             (output)
          INTEGER*4, INTENT(  OUT) :: KSPH        !  spheroid code          (output)


          !...........   EXTERNAL FUNCTIONS:

          LOGICAL         INITSPHERES, SPHEREDAT
          EXTERNAL        INITSPHERES, SPHEREDAT


          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          REAL*8          CRDLL( 2 )  !  L,L for false-Easting/Northing calc
          REAL*8          CRDXY( 2 )  !  x,y for false-Easting/Northing calc
          INTEGER*4       XXSYS       !  scratch vbles for E/N calc
          INTEGER*4       XXZONE      !  scratch vbles for E/N calc
          REAL*8          TPAXX( 15 ) !  scratch vbles for E/N calc
          REAL*8          TPDUM( 15 ) !  scratch vbles for E/N calc
          INTEGER*4       XXUNIT      !  scratch vbles for E/N calc
          INTEGER*4       XXSPH       !  scratch vbles for E/N calc

          INTEGER*4       IPR         !  error print flag
          INTEGER*4       JPR         !  projection parameter print flag
          INTEGER*4       LEMSG       !  error message unit number
          INTEGER*4       LPARM       !  projection parameter unit number
          INTEGER*4       IFLG        !  error flag

          INTEGER*4       LN27        !  NAD1927 file unit number
          INTEGER*4       LN83        !  NAD1983 file unit number
          CHARACTER*128   FN27        !  NAD1927 file name
          CHARACTER*128   FN83        !  NAD1983 file name
          INTEGER*4       LENGTH      !  NAD* record-length

          REAL*8          DSCR        !  scratch variables
          INTEGER         DEG, MNT    !  scratch variables

          INTEGER, SAVE :: LZONE = 61

          CHARACTER*256   MESG

          !**************************************************************
          !   begin body of subroutine  SETPROJ

          IF ( .NOT. INITSPHERES() ) THEN
              CALL M3WARN( PNAME,0,0,'Bad geodetic sphere info' )
          END IF


          !! Lat-Lon GCTP Arguments for coordinate system:

          TPA( 1 ) = 0.0D0
          TPA( 2 ) = 0.0D0
          TPA( 3 ) = 0.0D0
          TPA( 4 ) = 0.0D0
          TPA( 5 ) = 0.0D0
          TPA( 6 ) = 0.0D0
          TPA( 7 ) = 0.0D0
          TPA( 8 ) = 0.0D0

          IF ( GDTYP .EQ. LATGRD3 ) THEN

              KSYS  = 0       !  geographic (=Lat-Lon)
              KZONE = 0
              KUNIT = 4       !  output units:  degrees
              SETPROJ = .TRUE.
              RETURN

          ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN

              DSCR = ALPHA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = BETA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = GAMMA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = YCENT
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
              TPA( 7 ) = 0.0D0
              TPA( 8 ) = 0.0D0
              KSYS  = 4       !  Lambert conformal conic
              KUNIT = 2       !  output units:  meters
              KZONE = LZONE
              LZONE = LZONE + 1

          ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN

              KSYS  = 1       !  Universal Transverse Mercator
              KZONE = NINT( ALPHA )
              KUNIT = 2       !  output units:  meters

          ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN

              DSCR = GAMMA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = BETA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              TPA( 7 ) = 0.0D0
              TPA( 8 ) = 0.0D0
              KSYS  = 6       !  Polar Stereographic
              KUNIT = 2       !  output units:  meters
              KZONE = LZONE
              LZONE = LZONE + 1

          ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN

              DSCR = GAMMA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = BETA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              TPA( 7 ) = 0.0D0
              TPA( 8 ) = 0.0D0
              KSYS  = 9       ! Transverse Mercator
              KUNIT = 2       !  output units:  meters
              KZONE = LZONE
              LZONE = LZONE + 1

          ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN

              DSCR = GAMMA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = BETA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              TPA( 7 ) = 0.0D0
              TPA( 8 ) = 0.0D0
              KSYS  = 5       ! Equatorial Mercator
              KUNIT = 2       !  output units:  meters
              KZONE = LZONE
              LZONE = LZONE + 1

          ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN

              DSCR = ALPHA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = BETA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = GAMMA
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

              DSCR = YCENT
              DEG  = INT( DSCR )                              !  int degrees
              DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
              MNT  = INT( DSCR )                              !  int minutes
              DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
              TPA( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0
              TPA( 7 ) = 0.0D0
              TPA( 8 ) = 0.0D0
              KSYS  = 3       !  Albers Equal Area conic
              KUNIT = 2       !  output units:  meters
              KZONE = LZONE
              LZONE = LZONE + 1

          ELSE

              WRITE( MESG, '( A, I5 )' )
     &           'Unsupported map projection type', GDTYP
              CALL M3MSG2( MESG )
              SETPROJ = .FALSE.
              RETURN

          END IF          !  if gdtyp=latgrd3...eqmgrd3, or not

          !!  Calculation of false Easting/Northing

          IF ( .NOT. SPHEREDAT( KSPH, TPAXX, TPA ) ) THEN
              CALL M3MSG2( 'SETSPHERE/SPHEREDAT error in SETPROJ' )
              SETPROJ = .FALSE.
              RETURN
          END IF

          TPAXX( 3 )  = 0.0D0
          TPAXX( 4 )  = 0.0D0
          TPAXX( 5 )  = 0.0D0
          TPAXX( 6 )  = 0.0D0
          TPAXX( 7 )  = 0.0D0
          TPAXX( 8 )  = 0.0D0
          XXSYS  = 0       !  geographic (=Lat-Lon)
          XXZONE = 0
          XXUNIT = 4       !  input units:  degrees
          XXSPH  = KSPH    !  input spheroid
          IPR    = 0       !  print error messages, if any
          JPR    = 1       !  do NOT print projection parameters
          IFLG   = 0
          LEMSG  = INIT3() !  unit number for log file
          LPARM  = LEMSG   !  projection parameters file

          CRDLL( 1 ) = XCENT
          CRDLL( 2 ) = YCENT
          CALL GTPZ0( CRDLL, XXSYS, XXZONE, TPAXX, XXUNIT,
     &                XXSPH, IPR, JPR, LEMSG, LPARM,
     &                CRDXY, KSYS, KZONE, TPA, KUNIT,
     &                LN27, LN83, FN27, FN83, LENGTH, IFLG )

          IF ( IFLG .EQ. 0 ) THEN
              TPA( 7 ) = CRDXY( 1 )
              TPA( 8 ) = CRDXY( 2 )
              KZONE    = KZONE + 2
              SETPROJ  = .TRUE.
          ELSE
              WRITE( MESG, '( A, I10 )' ) 'GCTP error number', IFLG
              CALL M3MSG2( MESG )
              SETPROJ = .FALSE.
          END IF

          RETURN

          END FUNCTION SETPROJ


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


          SUBROUTINE GETPROJ( GDTYP, ALPHA, BETA, GAMMA, XCENT, YCENT )

          !*********************************************************
          !  FUNCTION:
          !       Get map projection defining parameters from user
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          INTEGER*4, INTENT(  OUT) :: GDTYP  !  I/O API map projection code (input)
          REAL*8   , INTENT(  OUT) :: ALPHA  !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: BETA   !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: GAMMA  !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: XCENT  !  Cartesian origin longitude  (input)
          REAL*8   , INTENT(  OUT) :: YCENT  !  Cartesian origin latitude   (input)


          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*64, PARAMETER :: GPROMPT =
     &                  'Enter map projection type'

          CHARACTER*64, PARAMETER :: GMENU( 7 ) =
     &    (/
     &    'Latitude-Longitude                   ',    !!  choice 1
     &    'Lambert Conformala Conic             ',    !!  choice 2
     &    'Universal Transverse Mercator        ',    !!  choice 3
     &    'Polar Sterographic                   ',    !!  choice 4
     &    '(General) Transverse Mercator        ',    !!  choice 5
     &    'Equatorial Mercator                  ',    !!  choice 6
     &    'Albers Equal-Area Conic              '     !!  choice 7
     &    /)
          INTEGER, PARAMETER :: GTYPES( 7 ) =
     &    (/
     &    LATGRD3,                              !!  choice 1
     &    LAMGRD3,                              !!  choice 2
     &    UTMGRD3,                              !!  choice 3
     &    POLGRD3,                              !!  choice 4
     &    TRMGRD3,                              !!  choice 5
     &    EQMGRD3,                              !!  choice 5
     &    ALBGRD3                               !!  choice 6
     &    /)

          !**************************************************************
          !   begin body of subroutine  GETPROJ

          GDTYP = GTYPES( GETMENU( 7, 1, GPROMPT, GMENU ) )

          IF ( GDTYP .EQ. LATGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = 0.0D0
              GAMMA = 0.0D0
              XCENT = 0.0D0
              YCENT = 0.0D0
          ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN
              ALPHA = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                         'Enter first secant angle   (deg N)' )
              BETA  = GETDBLE( ALPHA, 90.0D0, 60.0D0,
     &                         'Enter second secant angle  (deg W)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian     (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN
              ALPHA = DBLE( GETNUM( 1, 60, 17,
     &                         'Enter UTM zone number' ) )
              BETA  = 0.0D0
              GAMMA = 0.0D0
              XCENT = GETDBLE(  -9.999D36, 9.999D36, 0.0D0,
     &                         'Enter Cartesian-origin X (M)' )
              YCENT = GETDBLE(   -9.999D36, 9.999D36, 4235.0D3,
     &                         'Enter Cartesian-origin Y (M)' )
          ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN
              ALPHA = DBLE( GETNUM(-1, 1, 1,
     &        'Enter 1 for North polar, -1 for South Polar' ) )
              BETA  = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                         'Enter latitude of true scale (deg N)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 90.0D0*ALPHA,
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = GETDBLE( 0.0D0, 1.0D0, 1.0D0,
     &                         'Enter scale factor' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                         'Enter latitude of true scale (deg N)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.0D0,
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN
              ALPHA = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                         'Enter first secant angle   (deg N)' )
              BETA  = GETDBLE( ALPHA, 90.0D0, 60.0D0,
     &                         'Enter second secant angle  (deg W)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian     (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          END IF

          RETURN

          END SUBROUTINE GETPROJ


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE GETGRID( XORIG, YORIG,  XCELL, YCELL,
     &                        NCOLS, NROWS )

          !*********************************************************
          !  FUNCTION:
          !       Get grid defining parameters from user
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          REAL*8 , INTENT(   OUT) :: XORIG, YORIG, XCELL, YCELL
          INTEGER, INTENT(   OUT) :: NCOLS, NROWS


          !**************************************************************
          !   begin body of subroutine  GETGRID

          NCOLS = GETNUM( 1, 999999999, NCOLS,
     &                   'Enter number of grid cols' )
          NROWS = GETNUM( 1, 999999999, NROWS,
     &                   'Enter number of grid rows' )
          XORIG = GETDBLE( -9.999D36, 9.999D36, XORIG,
     &                   'Enter X at SW grid corner' )
          YORIG = GETDBLE( -9.999D36, 9.999D36, YORIG,
     &                   'Enter Y at SW grid corner' )
          XCELL = GETDBLE( -9.999D36, 9.999D36, XCELL,
     &                   'Enter DX cellsize' )
          YCELL = GETDBLE( -9.999D36, 9.999D36, XCELL,
     &                   'Enter DY cellsize' )

          RETURN

          END SUBROUTINE GETGRID


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE SHOWPROJ( TEXT,
     &                         GDTYP, ALPHA, BETA, GAMMA, XCENT, YCENT )

          !*********************************************************
          !  FUNCTION:
          !       Display map projection defining parameters
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          CHARACTER*(*), INTENT(IN   ) :: TEXT   !  projection description      (input)
          INTEGER*4    , INTENT(IN   ) :: GDTYP  !  I/O API map projection code (input)
          REAL*8       , INTENT(IN   ) :: ALPHA  !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: BETA   !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: GAMMA  !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: XCENT  !  Cartesian origin longitude  (input)
          REAL*8       , INTENT(IN   ) :: YCENT  !  Cartesian origin latitude   (input)


          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*256   MESG

          !**************************************************************
          !   begin body of subroutine  SHOWPROJ

          IF ( GDTYP .EQ. LATGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Lat-Lon'
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Lambert conformal conic'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Secant angles', ALPHA, BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  UTM'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, I7 )' ) 'UTM Zone', NINT( ALPHA )
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'UTM origin X-Y', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN

              CALL M3MSG2( TEXT )
              IF ( ALPHA .GT. 0 ) THEN
                  MESG = 'Projection type:  North-Polar Stereographic'
              ELSE
                  MESG = 'Projection type:  South-Polar Stereographic'
              END IF
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Latitude of True Scale', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Transverse Mercator'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Scale Factor', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Equatorial Mercator'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Latitude of True Scale', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Albers Equal-Area conic'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Secant angles', ALPHA, BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE

              WRITE( MESG, '( A, I5 )' )
     &           'Unsupported map projection type', GDTYP
              CALL M3MSG2( MESG )

          END IF

          CALL M3MSG2( ' ' )

          RETURN

          END SUBROUTINE SHOWPROJ


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE SHOWGRID( TEXT,
     &                         NCOLS, NROWS,
     &                         XORIG, YORIG, XCELL, YCELL )

          !*********************************************************
          !  FUNCTION:
          !       Display grid defining parameters
          !**********************************************************

          IMPLICIT NONE

          !...........   ARGUMENTS and their descriptions:

          CHARACTER*(*), INTENT(IN   ) :: TEXT        !  projection description      (input)
          INTEGER      , INTENT(IN   ) :: NCOLS       ! number of grid columns
          INTEGER      , INTENT(IN   ) :: NROWS       ! number of grid rows
          REAL*8       , INTENT(IN   ) :: XORIG       ! X-coordinate origin of grid (map units)
          REAL*8       , INTENT(IN   ) :: YORIG       ! Y-coordinate origin of grid
          REAL*8       , INTENT(IN   ) :: XCELL       ! X-coordinate cell dimension
          REAL*8       , INTENT(IN   ) :: YCELL       ! Y-coordinate cell dimension

          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*256   MESG

          !**************************************************************
          !   begin body of subroutine  SHOWGRID

          CALL M3MSG2( TEXT )
          WRITE( MESG, '( A20, 2( :, I10, 2X, A ) )' )
     &           'Dimensions', NCOLS, 'columns', NROWS, 'rows'
          CALL M3MSG2( MESG )
          WRITE( MESG, '( A20, 2( :, 2X, 1PE24.17 ) )' )
     &           'Lower-Left corner X Y', XORIG, YORIG
          CALL M3MSG2( MESG )
          WRITE( MESG, '( A20, 2( :, 2X, 1PE24.17 ) )' )
     &           'Cellsize DX DY', XCELL, YCELL
          CALL M3MSG2( MESG )

          CALL M3MSG2( ' ' )

          RETURN

          END SUBROUTINE SHOWGRID


        END PROGRAM PROJTOOL
