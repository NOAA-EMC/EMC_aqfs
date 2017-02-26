
        PROGRAM PRESZ

C***********************************************************************
C Version "$Id:: presz.F 1703 2013-11-15 21:39:36Z coats@bdsl$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2005 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body      starts at line   67
C  subroutine MAKEPZ starts at line  331
C
C  DESCRIPTION:
C       Builds multi-layer time-independent gridded file with
C       reference PRES and Z values.
C
C  PRECONDITIONS REQUIRED:
C       "setenv"s for output file, GRIDDESC file
C       "f90 presz.F -o presz -L/home/xcc/SunOS5 -lemstuff -lm3io -lnetcdf"
C       from a directory containing PARMS3.EXT, FDESC3.EXT, IODECL3.EXT
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API and utility routines; Lambert conversion routines from
C       libemstuff
C
C  REVISION  HISTORY:
C       prototype 7/1996 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  11/2007 by CJC flash-screen/contact-info update
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version  12/2013 by CJC:  PARAMETER CMENU(:)
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C.......   Parameter

        CHARACTER*16, PARAMETER :: NONE  = 'NONE'
        CHARACTER*16, PARAMETER :: PNAME = 'PRESZ'
        CHARACTER*16, PARAMETER :: CMENU( 5 ) =
     &          (/
     &          'Lat-Lon        ',      !  coordinate types menu item 1
     &          'Lambert        ',      !  coordinate types menu item 2
     &          'Mercator       ',      !  coordinate types menu item 3
     &          'Stereographic  ',      !  coordinate types menu item 4
     &          'UTM            '       !  coordinate types menu item 5
     &          /)

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         L
        REAL            V
        INTEGER         LOGDEV
        CHARACTER*16    ANAME
        CHARACTER*16    FNAME, TNAME
        CHARACTER*160   MESG

C***********************************************************************
C.......   First:  Initialize the I/O API:

        LOGDEV = INIT3()        !  initialization returns unit # for log

        WRITE( *,92000 )        !  opening screen:
     &' ',
     &'Program PRESZ to construct matching TIME-INDEPENDENT LAYERED',
     &'GRIDDED TIME-INDEPENDENT I/O API files containing hydrostatic',
     &'reference pressure and altitude at cell centers for a user',
     &'specified coordinate system and grid.  NOTE:  Currently, only',
     &'hydrostatic Sigma-P vertical coordinate systems and Lat-lon,',
     &'Lambert, and UTM horizontal coordinate systems are supported.',
     &' ',
     &'Specifications for this grid may either come from a GRIDDESC',
     &'file (if it is a named grid), or may be entered interactively.',
     &' ',
     &'You will be prompted for the logical name of an optional',
     &'terrain file and of the the output file.  You will need to',
     &'have set up the environment for this program by appropriate',
     &'commands ',
     &' ',
     &'    setenv  <FILENAME> <PHYSICAL PATH NAME>"',
     &' ',
     &'for the input and output files.',
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
     &'$$Id:: presz.F 42 2014-09-10 20:10:50Z coats                  $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?',
     &                    .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program ended at user request', 0 )
        END IF

        MESG = 'Enter logical name for TERRAIN input file or NONE'
        CALL GETSTR( MESG, 'INFILE', TNAME )

        MESG = 'Enter logical name for output file'
        CALL GETSTR( MESG, 'OUTFILE', FNAME )


C.......   If input file exists, open it, get its description, and re-use
C.......   the horizontal-grid part of its description:

        IF ( TNAME .NE. NONE ) THEN

            IF ( .NOT. OPEN3( TNAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open file "' //
     &                 TRIM( TNAME ) // '" for input'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            IF ( .NOT. DESC3( TNAME ) ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                       'Could not get terrain file description',
     &                       2 )
            END IF
            GO TO  99           !  skip entry of horiz. grid description
        END IF


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
                GDTYP3D = GETMENU( 5, 2,
     &              'Enter number for horiz coordinate system type',
     &              CMENU )

                IF ( GDTYP3D .EQ. LATGRD3 ) THEN !  lat-lon:  no P_ALP, ...

                    P_ALP3D = 0.0D0
                    P_BET3D = 0.0D0
                    P_GAM3D = 0.0D0
                    XCENT3D = 0.0D0
                    YCENT3D = 0.0D0

                ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN !  Lambert projection

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
     &                  'Only Lat-Lon and Lambert currently supported' )
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

                XCELL3D = GETDBLE( -9.0D36, 9.0D36, 54000.0D0,
     &                             'Enter X cell size XCELL (meters)' )
                YCELL3D = GETDBLE( -9.0D36, 9.0D36, XCELL3D,
     &                             'Enter Y cell size YCELL (meters)' )
                XORIG3D = GETDBLE( -9.0D36, 9.0D36,
     &                       XCELL3D*( DBLE( NCOLS3D ) - 0.5D0 ),
     &                       'Enter SW corner X coord for (1,1)-cell' )
                YORIG3D = GETDBLE( -9.0D36, 9.0D36,
     &                       YCELL3D*( DBLE( NROWS3D ) - 0.5D0 ),
     &                       'Enter SW corner Y coord for (1,1)-cell' )

            END IF      !  if specify horizontal grid by name, or interactively

99      CONTINUE        !  end:  get horizontal grid specs.


C.......   Now enter vertical coordinate structure:

        NLAYS3D = GETNUM( 1, MXLAYS3, 30, 'Enter number of layers' )
        VGTYP3D = VGSGPH3       ! hydrostatic sigma-P from PARMS3.EXT
        VGTOP3D = 100.0         ! model top (mb)

        VGLVS3D( 1 ) = GETREAL( 0.0, 1.0, 1.0,
     &                          'Enter sigma value for bottom of model')

        DO  111  L = 1, NLAYS3D
            WRITE( MESG, '( A, I3 )' )
     &              'Enter sigma value for top of layer', L
            V = 1.0 - ( FLOAT( L ) / FLOAT( NLAYS3D ) )**2
            VGLVS3D( L ) = GETREAL( 0.0, 1.0, V, MESG )
111     CONTINUE        !  end:  get horizontal grid specs.


C.......   Time step structure: zeros for time-independent file

        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0

C.......   Variables and their descriptions; file description

        NVARS3D = 4
        VNAME3D( 1 ) = 'PRESH'
        UNITS3D( 1 ) = 'millibars'
        VDESC3D( 1 ) = 'layer-center reference pressure '
        VTYPE3D( 1 ) = M3REAL

        VNAME3D( 2 ) = 'PRESF'
        UNITS3D( 2 ) = 'millibars'
        VDESC3D( 2 ) = 'layer-top reference pressure '
        VTYPE3D( 2 ) = M3REAL

        VNAME3D( 3 ) = 'ZH'
        UNITS3D( 3 ) = 'meters'
        VDESC3D( 3 ) = 'layer-center elevation above terrain'
        VTYPE3D( 3 ) = M3REAL

        VNAME3D( 4 ) = 'ZF'
        UNITS3D( 4 ) = 'meters'
        VDESC3D( 4 ) = 'layer-top elevation above terrain'
        VTYPE3D( 4 ) = M3REAL

        FTYPE3D      = GRDDED3
        FDESC3D( 1 ) =
     &  'Sample layered gridded file:  pressures and altitudes'
        FDESC3D( 2 ) = 'Generated by sample program PRESZ'
        DO  22  L = 3, MXDESC3          ! mxdesc3 = 60, from PARMS3.EXT
            FDESC3D( L ) = ' '          !  rest of lines are blank
22      CONTINUE


C.......   Open file as "unknown" -- if it does not exist, create it;
C.......   else check header against description supplied in FDESC3.EXT;
C.......   open for output in any case.
C.......   Use subroutine MAKEPZ to allocate arrays for variables
C.......   PRES and Z, compute them, and write them to file FNAME.

        IF ( .NOT. OPEN3( FNAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open file "' //
     &             TRIM( FNAME ) // '" for output'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL MAKEPZ( FNAME , TNAME )    !  see below, in this file.


C.......   Clean up and exit (M3EXIT calls SHUT3() automatically)

        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program PRESZ', 0 )

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X, A )


      CONTAINS        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE  MAKEPZ( FNAME , TNAME )

        IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        CHARACTER*16    FNAME   !  name of output file
        CHARACTER*16    TNAME   !  name of input terrain file, or "NONE"


C...........   PARAMETERS

        REAL, PARAMETER :: P0   = 1012.5
        REAL, PARAMETER :: ZFAC =   -7.2E3
        REAL, PARAMETER :: PFAC = 1.0 / ZFAC

        CHARACTER*16, PARAMETER :: NONE = 'NONE'

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    HT   ( NCOLS3D, NROWS3D )
        REAL    PSFC ( NCOLS3D, NROWS3D )
        REAL    PRESH( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    PRESF( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    ZH   ( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    ZF   ( NCOLS3D, NROWS3D, NLAYS3D )

        INTEGER         R, C, L         !  row, column, layer counters
        REAL            SH, SF, P       !  scratch variables
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of subroutine  MAKEPZ

        IF ( TNAME .NE. NONE ) THEN      !  read HT; compute PSFC from HT

            IF ( .NOT. READ3( TNAME, 'HT', 1, 0, 0, HT ) ) THEN
                MESG = 'Could not read "HT" from "' // TNAME
                CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
            END IF

            DO  22  R = 1, NROWS3D
            DO  11  C = 1, NCOLS3D
                PSFC( C, R ) = P0 * EXP( PFAC * HT( C,R ) )
11          CONTINUE
22          CONTINUE

        ELSE		!  set surface pressure to P0

            DO  44  R = 1, NROWS3D
            DO  33  C = 1, NCOLS3D
                HT  ( C, R ) = 0.0
                PSFC( C, R ) = P0
33          CONTINUE
44          CONTINUE

        END IF		!  if tname "NONE" or not

        DO  77  L = 1, NLAYS3D

            SH = 0.5 * ( VGLVS3D( L ) + VGLVS3D( L+1 ) )
            SF = VGLVS3D( L+1 )

            DO  66  R = 1, NROWS3D
            DO  55  C = 1, NCOLS3D

                P = PSFC( C, R )
                PRESH( C, R, L ) = VGTOP3D + SH * ( P - VGTOP3D )
                PRESF( C, R, L ) = VGTOP3D + SF * ( P - VGTOP3D )

                P = 1.0 / P
                ZH   ( C, R, L ) = HT( C, R ) +
     &                             ZFAC * LOG( PRESH( C, R, L ) * P )
                ZF   ( C, R, L ) = HT( C, R ) +
     &                             ZFAC * LOG( PRESF( C, R, L ) * P )

55          CONTINUE		!  end loop on cols C
66          CONTINUE		!  end loop on rows R

77      CONTINUE		!  end loop on levels L


C.......   Write out results to file FNAME, then return:

        IF ( .NOT. WRITE3( FNAME, 'PRESH', 0, 0, PRESH ) ) THEN
            MESG = 'Error writing "PRESH" to file "' //FNAME
            CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'PRESF', 0, 0, PRESF ) ) THEN
            MESG = 'Error writing "PRESF" to file "' //FNAME
            CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'ZH', 0, 0, ZH ) ) THEN
            MESG = 'Error writing "ZH" to file "' //FNAME
            CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'ZF', 0, 0, ZF ) ) THEN
            MESG = 'Error writing "ZF" to file "' //FNAME
            CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
        END IF

        RETURN

        END SUBROUTINE  MAKEPZ


      END PROGRAM PRESZ

