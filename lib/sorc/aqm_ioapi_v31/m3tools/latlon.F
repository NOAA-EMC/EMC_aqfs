
        PROGRAM LATLON

C***********************************************************************
C Version "$Id: latlon.F 1703 2013-11-15 21:39:36Z coats@bdsl$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body      starts at line   77
C  subroutine MAKEGRD starts at line 372
C  subroutine MAKEBDY starts at line 495
C
C  DESCRIPTION:
C       Builds 1-layer time-independent gridded file with LAT and LON
C
C  PRECONDITIONS REQUIRED:
C       "setenv"s for output files, GRIDDESC file
C       "f77 latlon.F -o latlon -L/home/xcc/SunOS5 -lemstuff -lm3io -lnetcdf"
C	from a directory containing PARMS3.EXT, FDESC3.EXT, IODECL3.EXT
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API and utility routines; Lambert conversion routines from
C	libemstuff
C
C  REVISION  HISTORY:
C       prototype 7/96 by CJC
C       Modified  9/99 by CJC for enhanced portability
C       Modified  9/99 by CJC:  more internal documentation about
C                               I/O API grid concepts.
C       Version 11/2001 by CJC for I/O API Version 2.1
C       Version 11/2005 by CJC:  eliminate unused vbles
C       Version 11/2007 by CJC:  splash-screen/contact-info update
C       Version 06/2008 by CJC:  Changes for Albers conformal conic
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 12/2013 by CJC:  PARAMETER  CMENU(:), CTYPE(:)
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: NONE  = 'NONE'
        CHARACTER*16, PARAMETER :: PNAME = 'LATLON'
        CHARACTER*20, PARAMETER :: CMENU( 6 ) =
     &          (/
     &          'lat-lon            ',  !  coordinate types menu item 1
     &          'Lambert Conformal  ',  !  coordinate types menu item 2
     &          'Mercator           ',  !  coordinate types menu item 3
     &          'Stereographic      ',  !  coordinate types menu item 4
     &          'UTM                ',  !  coordinate types menu item 5
     &          'Albers Equal-Area  '   !  coordinate types menu item 6
     &          /)
        INTEGER, PARAMETER :: CTYPE( 6 ) =
     &          (/
     &          LATGRD3, LAMGRD3, MERGRD3, STEGRD3, UTMGRD3, ALBGRD3
     &          /)

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         L
        INTEGER         LOGDEV
        CHARACTER*16    ANAME
        CHARACTER*16    BNAME, GNAME
        CHARACTER*160   MESG


C***********************************************************************
C.......   First:  Initialize the I/O API:

        LOGDEV = INIT3()	!  initialization returns unit # for log

        WRITE( *, '( 5X, A )' )        !  opening screen:
     &' ',
     &'Program LATLON to construct matching TIME-INDEPENDENT 1-LAYER ',
     &'GRIDDED and BOUNDARY TIME-INDEPENDENT I/O API files containing',
     &'the latitude and longitude at cell centers, for a user ',
     &'specified coordinate system and grid.  NOTE:  Currently, only ',
     &'Lat-lon, Lambert, and UTM coordinate systems, and boundaries ',
     &'with  NTHIK > 0  are supported.  You may turn off either file',
     &' by responding "NONE" to the prompt for its name.',
     &' ',
     &'Specifications for this grid may either come from a GRIDDESC  ',
     &'file (if it is a named grid), or may be entered interactively.',
     &' ',
     &'You will be prompted for the logical name of the output files.',
     &'You will need to have set up the environment for this program ',
     &'by appropriate commands ',
     &' ',
     &'    setenv  <FILENAME> <PHYSICAL PATH NAME>"',
     &' ',
     &'for the output files and (if you use it)  the GRIDDESC file.',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
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
     &  '$Id:: latlon.F 42 2014-09-10 20:10:50Z coats               $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?',
     &                    .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program ended at user request', 0 )
        END IF

        MESG = 'Enter logical name for GRIDDED output file, or NONE'
        CALL GETSTR( MESG, 'GRDFILE', GNAME )

        MESG = 'Enter logical name for BOUNDARY output file, or NONE'
        CALL GETSTR( MESG, 'BDYFILE', BNAME )

        IF ( GNAME .EQ. NONE  .AND.  BNAME .EQ. NONE ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'No output files requested', 2 )
        END IF

        !!  According to EDSS/Models-3 I/O API coordinate and grid
        !!  conventions, the basic idea is:
        !!
        !!  One defines a horizontal Cartesian coordinate system
        !!  ("map projection") as giving (X,Y) coordinates (generally
        !!  in MKS units, i.e. Meters) relative to some known origin
        !!  (XCENT,YCENT), in terms of some known defining angles
        !!  (P_ALP, P_BET, P_GAM).
        !!
        !!  Having defined a map projection, one than then define
        !!  horizontal grids within it by specifying:
        !!
        !!      The (1,1)-corner (XORIG, YORIG) in terms of the
        !!      Cartesian coordinates of the map projection
        !!      (note that for the specification of this corner,
        !!      we take a "grid-cells are volumes" approach rather
        !!      than a Poincare-dual "grid is array of node-points"
        !!      point of view);
        !!
        !!      The cellsize (XCELL, YCELL) in both coordinate
        !!      directions;
        !!
        !!      The dimensionality (NCOLS,NROWS).
        !!
        !!      (Optionally) the thickness NTHIK in cells of a
        !!      boundary data-structure for the grid.
        !!
        !!  Note that frequently, we expect to have multiple grids
        !!  with the same map projection.
        !!
        !!  Having defined a grid in terms of a (MKS-unit) Cartesian
        !!  coordinate system mapped from the surface of the Earth,
        !!  one can then transform grid-related problems into problems
        !!  stated in (non-metric) grid-normal coordinates defined
        !!  relative to the (1,1) corner of the grid, as given by the
        !!  formulas
        !!
        !!      REAL C,R
        !!      ...
        !!      C = (X - XORIG) / XCELL
        !!      R = (Y - YORIG) / YCELL
        !!
        !!  for which the (I,J) cell is {(C,R): I-1 <= C <I, J-1 <= R <J }


11      CONTINUE        !  loop:  get grid specs.

            IF ( GETYN( 'Specify grid by name from GRIDDESC file?',
     &                  .TRUE. ) ) THEN

                CALL GETSTR( 'Enter grid name',
     &                       'SMRAQ54_50X48',
     &                       GDNAM3D )
                IF ( .NOT. DSCGRID( GDNAM3D, ANAME  , GDTYP3D,
     &                              P_ALP3D, P_BET3D, P_GAM3D,
     &                              XCENT3D, YCENT3D,
     &                              XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &                              NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

                    MESG = 'Grid "' // TRIM( GDNAM3D ) //
     &                     '" not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        CALL M3EXIT( PNAME, 0, 0,
     &                               'Program ended at user request',
     &                               2 )
                    END IF

                END IF          !  if DSCGRID failed

            ELSE        !  enter grid specs interactively

                CALL GETSTR( 'Enter grid name',
     &                       'SMRAQ54_48X50',
     &                       GDNAM3D )
                GDTYP3D = CTYPE( GETMENU( 6, 2,
     &          'Enter number for horizontal coordinate system type',
     &           CMENU ) )

                IF ( GDTYP3D .EQ. LATGRD3 ) THEN !  lat-lon:  no P_ALP, ...

                    P_ALP3D = 0.0D0
                    P_BET3D = 0.0D0
                    P_GAM3D = 0.0D0
                    XCENT3D = 0.0D0
                    YCENT3D = 0.0D0

                ELSE IF ( GDTYP3D .EQ. LAMGRD3  .OR.
     &                    GDTYP3D .EQ. ALBGRD3  ) THEN !  Lambert or Albers conic projection

                    P_ALP3D = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                                 'Enter secant angle     P_ALP' )
                    P_BET3D = GETDBLE( P_ALP3D, 90.0D0, 60.0D0,
     &                                 'Enter secant angle     P_BET' )
                    P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                                 'Enter central meridian P_GAM' )
                    XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                                 'Enter X coord origin   XCENT' )
                    YCENT3D = GETDBLE( -90.0D0, 90.0D0, 40.0D0,
     &                                 'Enter Y coord origin   YCENT' )

                ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN !  Lambert projection

                    P_ALP3D = DBLE( GETNUM( 1, 60, 17,
     &                                      'Enter UTM zone' ) )
                    P_BET3D = 0.0D0
                    P_GAM3D = 0.0D0
                    XCENT3D = GETDBLE( -999999999.0D0, 999999999.0D0,
     &                                 0.0D0,
     &                                 'Enter UTM offset XCENT' )
                    YCENT3D = GETDBLE( -999999999.0D0, 999999999.0D0,
     &                                 0.0D0,
     &                                 'Enter UTM offset YCENT' )

                ELSE

                    CALL M3WARN( PNAME, 0, 0,
     &              'Only Lat-Lon, Lambert, UTM, and Albers supported' )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        CALL M3EXIT( PNAME, 0, 0,
     &                               'Program ended at user request',
     &                               2 )
                    END IF

                END IF  !  if descriptive angles relevant for this type

                NCOLS3D = GETNUM( 1, 999999999, 48,
     &                            'Enter number NCOLS of grid columns' )
                NROWS3D = GETNUM( 1, 999999999, 50,
     &                            'Enter number NROWS of grid rows' )
                NTHIK3D = GETNUM( 1, 999999999, 1,
     &                            'Enter bdy thickness NTHIK (cells)' )

                XCELL3D = GETDBLE( 0.0D0, 9.0D36, 54000.0D0,
     &                             'Enter X cell size XCELL (meters)' )
                YCELL3D = GETDBLE( 0.0D0, 9.0D36, XCELL3D,
     &                             'Enter Y cell size YCELL (meters)' )
                XORIG3D = GETDBLE( -9.0D36, 9.0D36,
     &                             XCELL3D*( DBLE( NCOLS3D ) - 0.5D0 ),
     &                       'Enter SW corner X coord for (1,1)-cell' )
                YORIG3D = GETDBLE( -9.0D36, 9.0D36,
     &                             YCELL3D*( DBLE( NROWS3D ) - 0.5D0 ),
     &                       'Enter SW corner Y coord for (1,1)-cell' )

            END IF      !  if specify horizontal grid by name, or interactively


C.......   Now enter vertical coordinate structure:

        NLAYS3D = 1
        VGTYP3D = VGSGPH3       ! hydrostatic sigma-P from PARMS3.EXT
        VGTOP3D = 100.0         ! model top (mb)
        VGLVS3D( 1 ) = 1.0
        VGLVS3D( 2 ) = 0.0


C.......   Time step structure: zeros for time-independent file

        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0

C.......   Variables and their descriptions; file description

        NVARS3D = 2
        VNAME3D( 1 ) = 'LAT'
        UNITS3D( 1 ) = 'degrees lat'
        VDESC3D( 1 ) = 'cell-centers latitudes'
        VTYPE3D( 1 ) = M3REAL

        VNAME3D( 2 ) = 'LON'
        UNITS3D( 2 ) = 'degrees lon'
        VDESC3D( 2 ) = 'cell-centers longitudes'
        VTYPE3D( 2 ) = M3REAL

        FTYPE3D = GRDDED3		!  set file data type
        FDESC3D( 1 ) = 'Sample 1-layer gridded file:  lats and lons'
        FDESC3D( 2 ) = 'Generated by sample program LATLON'
        DO  22  L = 3, MXDESC3  ! = 60, from PARMS3.EXT
            FDESC3D( L ) = ' '          !  rest of lines are blank
22      CONTINUE


C.......   Where file names GNAME, BNAME are not "NONE":
C.......   Open files as "unknown" -- if they do not exist, create them;
C.......   else check header against description supplied in FDESC3.EXT;
C.......   open for output in any case.
C.......   Use subroutines MAKEGRD, MAKEBDY to allocate arrays for variables
C.......   LAT and LON, compute them, and write them to files GNAME and BNAME.


        IF ( GNAME .NE. NONE ) THEN

            IF ( .NOT. OPEN3( GNAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open file "' //
     &                  TRIM( GNAME ) // '" for output'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MAKEGRD( GNAME )	!  see below, in this file.

        END IF				!  if gname not "none"

        IF ( BNAME .NE. NONE ) THEN	!  reuses file description

            FTYPE3D = BNDARY3		!  reset file data type, description
            FDESC3D( 1 ) = 'Sample 1-layer boundary file:  ' //
     &                     'lats and lons'

            IF ( .NOT. OPEN3( BNAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open file "' //
     &                 TRIM( BNAME ) // '" for output'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MAKEBDY( BNAME )	!  see below, in this file.

        END IF				!  if bname not "none"


C.......   Clean up and exit (M3EXIT calls SHUT3() automatically)

        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program LATLON', 0 )


      CONTAINS  !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



        SUBROUTINE  MAKEGRD( GNAME )

C...........   ARGUMENTS and their descriptions:

        CHARACTER*16    GNAME   !  name of output file


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    LAT( NCOLS3D, NROWS3D )
        REAL    LON( NCOLS3D, NROWS3D )

        INTEGER         R, C            !  row, column counters
        INTEGER         ZONE            !  UTM zone
        REAL            X0, Y0, X, Y    !  scratch variables
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of subroutine  MAKEGRD

        X0 = XORIG3D - 0.5D0 * XCELL3D  !  to get to cell-centers
        Y0 = YORIG3D - 0.5D0 * YCELL3D  !             "     "

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN        !  formulas for lat-lon:

            DO  22  R = 1, NROWS3D
            DO  11  C = 1, NCOLS3D
                LON( C, R ) = X0 + FLOAT( C ) * XCELL3D
                LAT( C, R ) = Y0 + FLOAT( R ) * YCELL3D
11          CONTINUE
22          CONTINUE

        ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN        !  formulas for Lambert Conformal Conic

            IF ( .NOT. SETLAM( SNGL( P_ALP3D ),       !  first, initialize
     &                         SNGL( P_BET3D ),       !  for LAM2LL()
     &                         SNGL( P_GAM3D ),
     &                         SNGL( XCENT3D ),
     &                         SNGL( YCENT3D ) ) ) THEN
                CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0,
     &                       'Lambert projection setup error', 2 )
            END IF      !  if SETLAM failed

            DO  44  R = 1, NROWS3D
            DO  33  C = 1, NCOLS3D
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT.LAM2LL( X, Y, LON( C,R ), LAT( C,R ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0,
     &                           'Lambert conversion error', 2 )
                END IF                          !  if the conversion failed
33          CONTINUE
44          CONTINUE

        ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN        !  formulas for utm

            ZONE = NINT( P_ALP3D )

            DO  66  R = 1, NROWS3D
            DO  55  C = 1, NCOLS3D
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                CALL UTM2LL( X, Y, ZONE, LON( C, R ), LAT( C, R ) )
55          CONTINUE
66          CONTINUE

        ELSE IF ( GDTYP3D .EQ. ALBGRD3 ) THEN        !  formulas for Albers Equal Area

            IF ( .NOT. SETALB( SNGL( P_ALP3D ),       !  first, initialize
     &                         SNGL( P_BET3D ),       !  for LAM2LL()
     &                         SNGL( P_GAM3D ),
     &                         SNGL( XCENT3D ),
     &                         SNGL( YCENT3D ) ) ) THEN
                CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0,
     &                       'Albers projection setup error', 2 )
            END IF      !  if SETALB failed

            DO  88  R = 1, NROWS3D
            DO  77  C = 1, NCOLS3D
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT.ALB2LL( X, Y, LON( C,R ), LAT( C,R ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0,
     &                           'Albers conversion error', 2 )
                END IF                          !  if the conversion failed
77          CONTINUE
88          CONTINUE

        ELSE

            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0,
     &                   'Unsupported coordinate system type', 2 )

        END IF          !  if lat-lon, lambert, utm, or else if unsupported


C.......   Write out results to file GNAME, then return:

        IF ( .NOT. WRITE3( GNAME, 'LAT', 0, 0, LAT ) ) THEN
            MESG = 'Error writing "LAT" to file "' //
     &             TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( GNAME, 'LON', 0, 0, LON ) ) THEN
            MESG = 'Error writing "LON" to file "' //
     &             TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        RETURN

        END SUBROUTINE  MAKEGRD



      !!*************  subroutine MAKEBDY starts here  ***********************
      !!
      !!  This also serves as an example to show how to traverse the
      !!  standard storage order for I/O API BNDARY3 data structures.
      !!***********************************************************************

      SUBROUTINE  MAKEBDY( BNAME )

        IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        CHARACTER*16    BNAME   !  name of output file


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    LAT( 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D ) )
        REAL    LON( 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D ) )

        INTEGER         R, C, K         !  row, column, bdy-cell counters
        INTEGER         ZONE            !  UTM zone
        REAL            X0, Y0, X, Y    !  scratch variables
        CHARACTER*80    MESG


C***********************************************************************

        X0 = XORIG3D - 0.5D0 * XCELL3D  !  to get to cell-centers
        Y0 = YORIG3D - 0.5D0 * YCELL3D  !             "     "
        K  = 0

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN        !  formulas for lat-lon:


            DO  22  R = 1 - NTHIK3D, 0		!  south boundary component
            DO  11  C = 1 , NCOLS3D + NTHIK3D
                K = K + 1
                LON( K ) = X0 + FLOAT( C ) * XCELL3D
                LAT( K ) = Y0 + FLOAT( R ) * YCELL3D
11          CONTINUE
22          CONTINUE

            DO  44  R = 1, NROWS3D + NTHIK3D 	!  east boundary component
            DO  33  C = NCOLS3D + 1, NCOLS3D + NTHIK3D
                K = K + 1
                LON( K ) = X0 + FLOAT( C ) * XCELL3D
                LAT( K ) = Y0 + FLOAT( R ) * YCELL3D
33          CONTINUE
44          CONTINUE

            DO  66  R = NROWS3D + 1, NROWS3D + NTHIK3D	!  north bdy component
            DO  55  C = 1 - NTHIK3D, NCOLS3D
                K = K + 1
                LON( K ) = X0 + FLOAT( C ) * XCELL3D
                LAT( K ) = Y0 + FLOAT( R ) * YCELL3D
55          CONTINUE
66          CONTINUE

            DO  88  R = 1 - NTHIK3D, NROWS3D
            DO  77  C = 1 - NTHIK3D, 0                  !  west bdy component
                K = K + 1
                LON( K ) = X0 + FLOAT( C ) * XCELL3D
                LAT( K ) = Y0 + FLOAT( R ) * YCELL3D
77          CONTINUE
88          CONTINUE


        ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN        !  formulas for Lambert Conformal Conic

            IF ( .NOT. SETLAM( SNGL( P_ALP3D ),       !  first, initialize
     &                         SNGL( P_BET3D ),       !  for LAM2LL()
     &                         SNGL( P_GAM3D ),
     &                         SNGL( XCENT3D ),
     &                         SNGL( YCENT3D ) ) ) THEN
                CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                       'Lambert projection setup error', 2 )
            END IF      !  if SETLAM failed

            DO  122  R = 1 - NTHIK3D, 0		!  south boundary component
            DO  111  C = 1 , NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. LAM2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Lambert conversion error', 2 )
                END IF                          !  if the conversion failed
111         CONTINUE
122         CONTINUE

            DO  144  R = 1, NROWS3D + NTHIK3D 	!  east boundary component
            DO  133  C = NCOLS3D + 1, NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. LAM2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Lambert conversion error', 2 )
                END IF                          !  if the conversion failed
133         CONTINUE
144         CONTINUE

            DO  166  R = NROWS3D + 1, NROWS3D + NTHIK3D	!  north bdy component
            DO  155  C = 1 - NTHIK3D, NCOLS3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. LAM2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Lambert conversion error', 2 )
                END IF                          !  if the conversion failed
155         CONTINUE
166         CONTINUE

            DO  188  R = 1 - NTHIK3D, NROWS3D
            DO  177  C = 1 - NTHIK3D, 0                  !  west bdy component
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. LAM2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Lambert conversion error', 2 )
                END IF                          !  if the conversion failed
177         CONTINUE
188         CONTINUE


        ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN        !  formulas for utm


            ZONE = NINT( P_ALP3D )

            DO  222  R = 1 - NTHIK3D, 0		!  south boundary component
            DO  211  C = 1 , NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                CALL UTM2LL( X, Y, ZONE, LON( K ), LAT( K ) )
211         CONTINUE
222         CONTINUE

            DO  244  R = 1, NROWS3D + NTHIK3D 	!  east boundary component
            DO  233  C = NCOLS3D + 1, NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                CALL UTM2LL( X, Y, ZONE, LON( K ), LAT( K ) )
233         CONTINUE
244         CONTINUE

            DO  266  R = NROWS3D + 1, NROWS3D + NTHIK3D	!  north bdy component
            DO  255  C = 1 - NTHIK3D, NCOLS3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                CALL UTM2LL( X, Y, ZONE, LON( K ), LAT( K ) )
255         CONTINUE
266         CONTINUE

            DO  288  R = 1 - NTHIK3D, NROWS3D
            DO  277  C = 1 - NTHIK3D, 0                  !  west bdy component
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                CALL UTM2LL( X, Y, ZONE, LON( K ), LAT( K ) )
277         CONTINUE
288         CONTINUE


        ELSE IF ( GDTYP3D .EQ. ALBGRD3 ) THEN        !  formulas for Albers

            IF ( .NOT. SETALB( SNGL( P_ALP3D ),       !  first, initialize
     &                         SNGL( P_BET3D ),       !  for ALB2LL()
     &                         SNGL( P_GAM3D ),
     &                         SNGL( XCENT3D ),
     &                         SNGL( YCENT3D ) ) ) THEN
                CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                       'Albers projection setup error', 2 )
            END IF      !  if SETLAM failed

            DO  322  R = 1 - NTHIK3D, 0		!  south boundary component
            DO  311  C = 1 , NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. ALB2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Albers conversion error', 2 )
                END IF                          !  if the conversion failed
311         CONTINUE
322         CONTINUE

            DO  344  R = 1, NROWS3D + NTHIK3D 	!  east boundary component
            DO  333  C = NCOLS3D + 1, NCOLS3D + NTHIK3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. ALB2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Albers conversion error', 2 )
                END IF                          !  if the conversion failed
333         CONTINUE
344         CONTINUE

            DO  366  R = NROWS3D + 1, NROWS3D + NTHIK3D	!  north bdy component
            DO  355  C = 1 - NTHIK3D, NCOLS3D
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. ALB2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Albers conversion error', 2 )
                END IF                          !  if the conversion failed
355         CONTINUE
366         CONTINUE

            DO  388  R = 1 - NTHIK3D, NROWS3D
            DO  377  C = 1 - NTHIK3D, 0                  !  west bdy component
                K = K + 1
                X = X0 + FLOAT( C ) * XCELL3D
                Y = Y0 + FLOAT( R ) * YCELL3D
                IF ( .NOT. ALB2LL( X, Y, LON( K ), LAT( K ) ) ) THEN
                    CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                           'Albers conversion error', 2 )
                END IF                          !  if the conversion failed
377         CONTINUE
388         CONTINUE


        ELSE

            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0,
     &                   'Unsupported coordinate system type', 2 )

        END IF          !  if lat-lon, lambert, utm, or else if unsupported


C.......   Write out results to file BNAME, then return:

        IF ( .NOT. WRITE3( BNAME, 'LAT', 0, 0, LAT ) ) THEN
            MESG = 'Error writing "LAT" to file "' //
     &             TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( BNAME, 'LON', 0, 0, LON ) ) THEN
            MESG = 'Error writing "LON" to file "' //
     &             TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        RETURN

        END SUBROUTINE  MAKEBDY


      END PROGRAM LATLON
