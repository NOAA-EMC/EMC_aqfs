
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/util/util/subhfile.F,v 1.1.1.1 2005/09/09 18:56:06 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUBHFILE ( FNAME, GXOFF, GYOFF,
     &                      STRTCOL, ENDCOL, STRTROW, ENDROW )

C returns local starting and ending column and row indices for file FNAME

      USE GRID_CONF             ! horizontal & vertical domain specifications

      IMPLICIT NONE
 
C Includes:
 
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/FDESC3.EXT"     ! file header data structuer
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"      ! I/O definitions and declarations
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/FILES_CTM.EXT"    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coord. and domain definitions (req IOPARMS)

C Arguments:

      CHARACTER( 16 ) :: FNAME
      INTEGER GXOFF, GYOFF      ! global origin offset from file (.ge. 0)
      INTEGER STRTCOL, ENDCOL   ! local processor start and end colums in file
      INTEGER STRTROW, ENDROW   ! local processor start and end rows in file

C External Functions (not already declared by IODECL3.EXT):
 
      INTEGER, EXTERNAL :: TRIMLEN, SETUP_LOGDEV
 
C Parameters

!     REAL( 8 ), PARAMETER :: ONE  = 1.0
!     REAL( 8 ), PARAMETER :: TEN  = 10.0
      REAL( 8 ), PARAMETER :: ONEK = 1000.0
      REAL( 8 ), PARAMETER :: TENK = 10000.0
      REAL( 8 ), PARAMETER :: TOL  = 1.0 / ONEK
!*begin added by snl
      REAL( 8 ), PARAMETER :: MIN_DOUBLE = 1.0D-08
!*end added by snl

C local variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV
      INTEGER       :: INDX                 ! because mype starts at 0
      CHARACTER( 16 ) :: PNAME = 'SubhFile_Cell'
      CHARACTER( 16 ) :: BNAME
      CHARACTER( 96 ) :: XMSG = ' '

      REAL( 8 ), SAVE :: XORIG_B, YORIG_B
      REAL( 8 ), SAVE :: XCENT_B, YCENT_B
      REAL( 8 ), SAVE :: XCELL_B, YCELL_B
      REAL( 8 ), SAVE :: XORIG_C, YORIG_C  ! from GRIDDESC, in cell coord

      INTEGER DOTFILE
      REAL( 8 ) :: RELOFFX, RELOFFY

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV()

C open cross file for subsequent comparison

         BNAME = GRID_CRO_2D( 1:TRIMLEN( GRID_CRO_2D ) )

         IF ( .NOT. OPEN3( BNAME, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// BNAME
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( BNAME ) ) THEN
            XMSG = 'Could not get ' // BNAME // ' file description'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF

C set base header data

         XORIG_B = XORIG3D; YORIG_B = YORIG3D
         XCENT_B = XCENT3D; YCENT_B = YCENT3D
         XCELL_B = XCELL3D; YCELL_B = YCELL3D

C Scale resolution

         XCELL_B = FLOAT( NINT( TENK * XCELL_B ) ) / TENK
         YCELL_B = FLOAT( NINT( TENK * YCELL_B ) ) / TENK

C Convert to grid cell coord and truncate

         XORIG_B = XORIG_B / XCELL_B
         XORIG_B = FLOAT( NINT( ONEK * XORIG_B ) ) / ONEK
         YORIG_B = YORIG_B / YCELL_B
         YORIG_B = FLOAT( NINT( ONEK * YORIG_B ) ) / ONEK

         XORIG_C = XORIG_GD / XCELL_GD
         XORIG_C = FLOAT( NINT( ONEK * XORIG_C ) ) / ONEK
         YORIG_C = YORIG_GD / YCELL_GD
         YORIG_C = FLOAT( NINT( ONEK * YORIG_C ) ) / ONEK

!        write( logdev,* ) '    XORIG_B: ', XORIG_B
!        write( logdev,* ) '    YORIG_B: ', YORIG_B
!        write( logdev,* ) '    XORIG_C: ', XORIG_C
!        write( logdev,* ) '    YORIG_C: ', YORIG_C
 
         END IF   ! FIRSTIME

C open existing file for readonly access

      IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// FNAME
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. DESC3( FNAME ) ) THEN
         XMSG = 'Could not get ' // FNAME( 1:TRIMLEN( FNAME ) )
     &        // ' file description'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

C check some header data against the reference file in this met set -
C mother grid center and grid cell resolution

      IF ( ABS( XCENT3D - XCENT_B ) .GT.
     &     TOL * ABS( XCENT3D + XCENT_B + TOL ) .OR.
     &     ABS( YCENT3D - YCENT_B ) .GT.
     &     TOL * ABS( YCENT3D + YCENT_B + TOL ) .OR.
     &     ABS( XCELL3D - XCELL_B ) .GT. TOL * XCELL3D .OR.
     &     ABS( YCELL3D - YCELL_B ) .GT. TOL * YCELL3D ) THEN
         WRITE( LOGDEV,2003 ) XCENT_B, XCENT3D, YCENT_B, YCENT3D,
     &                        XCELL_B, XCELL3D, YCELL_B, YCELL3D
2003     FORMAT(/ 5X, 'XCENT_B:', F20.12, 2X, 'XCENT3D (file):', F20.12 
     &          / 5X, 'YCENT_B:', F20.12, 2X, 'YCENT3D (file):', F20.12
     &          / 5X, 'XCELL_B:', F20.12, 2X, 'XCELL3D (file):', F20.12 
     &          / 5X, 'YCELL_B:', F20.12, 2X, 'YCELL3D (file):', F20.12 )

         XMSG = 'File header inconsistent with GRID_CRO_2D'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

C check some header data against GRIDDESC

      IF ( ABS( XCENT3D - XCENT_GD ) .GT.
     &     TOL * ABS( XCENT3D + XCENT_GD + TOL ) .OR.
     &     ABS( YCENT3D - YCENT_GD ) .GT.
     &     TOL * ABS( YCENT3D + YCENT_GD + TOL ) .OR.
     &     ABS( XCELL3D - XCELL_GD ) .GT. TOL * XCELL3D .OR.
     &     ABS( YCELL3D - YCELL_GD ) .GT. TOL * YCELL3D ) THEN
         WRITE( LOGDEV,2003 ) XCENT_GD, XCENT3D, YCENT_GD, YCENT3D,
     &                        XCELL_GD, XCELL3D, YCELL_GD, YCELL3D

         XMSG = 'File header inconsistent with GRIDDESC'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

C Convert to grid cell coord and truncate

      XORIG3D = XORIG3D / XCELL3D
      XORIG3D = FLOAT( NINT( ONEK * XORIG3D ) ) / ONEK
      YORIG3D = YORIG3D / YCELL3D
      YORIG3D = FLOAT( NINT( ONEK * YORIG3D ) ) / ONEK
 
!     write( logdev,* ) ' '
!     write( logdev,* ) '    Grid cell XORIG3D: ', XORIG3D
!     write( logdev,* ) '    Grid cell YORIG3D: ', YORIG3D

C Check if file (cross or dot) is "lined up"

!*begin change by snl
!     RELOFFX = MOD( ( XORIG_B - XORIG3D ), ONE )
!     RELOFFX = FLOAT( NINT( TEN * RELOFFX ) )
!     IF ( RELOFFX .NE. 0.D0 ) THEN
!        IF ( RELOFFX .NE. 5.D0 ) THEN
!           WRITE( LOGDEV,* ) '    10*RELOFFX: ', RELOFFX
      RELOFFX = 0.001D0 * NINT( 1000.D0 * ( XORIG_B - XORIG3D ) )
      RELOFFX = RELOFFX - 1.0D0 * NINT( RELOFFX )

      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. 0.5D0 - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. 0.5D0 + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
!*end change by snl
            WRITE( LOGDEV,* ) '    XORIG_B, XORIG3D: ', XORIG_B, XORIG3D
            XMSG = 'Inconsistent Base/File Xorig'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
         END IF
!*begin change by snl
!     RELOFFY = MOD ( ( YORIG_B - YORIG3D ), ONE )
!     RELOFFY = FLOAT( NINT( TEN * RELOFFY ) )
!     IF ( RELOFFY .NE. 0.D0 ) THEN
!        IF ( RELOFFY .NE. 5.D0 ) THEN
!           WRITE( LOGDEV,* ) '    10*RELOFFY: ', RELOFFY
      RELOFFY = 0.001D0 * NINT( 1000.D0 * ( YORIG_B - YORIG3D ) )
      RELOFFY = RELOFFY - 1.0D0 * NINT( RELOFFY )
      IF ( ABS( RELOFFY ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFY ) .LT. 0.5D0 - MIN_DOUBLE .OR.
     &        ABS( RELOFFY ) .GT. 0.5D0 + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFY: ', RELOFFY
!*end change by snl
            WRITE( LOGDEV,* ) '    YORIG_B, YORIG3D: ', YORIG_B, YORIG3D
            XMSG = 'Inconsistent Base/File Yorig'
!           CALL M3WARN( PNAME, 0, 0, XMSG )
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
         END IF
!*begin change by snl
!     IF ( RELOFFX .NE. RELOFFY ) THEN
      IF ( ABS( RELOFFX - RELOFFY ) > MIN_DOUBLE ) THEN
!*end change by snl
         XMSG = 'Inconsistent X- and Y-resolution (file vs. file)'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

!     WRITE( LOGDEV,* ) ' RELOFFX, RELOFFY: ', RELOFFX, RELOFFY

C check the file against the processor setup (COORD.EXT), and get the offsets

      DOTFILE = 0
!*begin change by snl
!     RELOFFX = MOD( ( XORIG_C - XORIG3D ), ONE )
!     RELOFFX = FLOAT( NINT( TEN * RELOFFX ) )
!     IF ( RELOFFX .NE. 0.D0 ) THEN
!        IF ( RELOFFX .NE. 5.D0 ) THEN
!            WRITE( LOGDEV,* ) '    10*RELOFFX: ', RELOFFX
      RELOFFX = 0.001D0 * NINT( 1000.D0 * ( XORIG_C - XORIG3D ) )
      RELOFFX = RELOFFX - 1.0D0 * NINT( RELOFFX )
      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. 0.5D0 - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. 0.5D0 + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
!*end change by snl
            WRITE( LOGDEV,* ) '    XORIG_GD, XORIG3D: ', XORIG_C, XORIG3D
            XMSG = 'File Xorig inconsistent with GRIDDESC'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
         END IF
!*begin change by snl
!     RELOFFY = MOD ( ( YORIG_C - YORIG3D ), ONE )
!     RELOFFY = FLOAT( NINT( TEN * RELOFFY ) )
!     IF ( RELOFFY .NE. 0.D0 ) THEN
!        IF ( RELOFFY .NE. 5.D0 ) THEN
!           WRITE( LOGDEV,* ) '    10*RELOFFY: ', RELOFFY
      RELOFFY = 0.001D0 * NINT( 1000.D0 * ( YORIG_C - YORIG3D ) )
      RELOFFY = RELOFFY - 1.0D0 * NINT( RELOFFY )
      IF ( ABS( RELOFFY ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFY ) .LT. 0.5D0 - MIN_DOUBLE .OR.
     &        ABS( RELOFFY ) .GT. 0.5D0 + MIN_DOUBLE ) THEN
!*end change by snl
            WRITE( LOGDEV,* ) '    RELOFFY: ', RELOFFY
            WRITE( LOGDEV,* ) '    YORIG_GD, YORIG3D: ', YORIG_C, YORIG3D
            XMSG = 'File Yorig inconsistent with GRIDDESC'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            ELSE
            DOTFILE = 1
            END IF
         END IF
!*begin change by snl
!     IF ( RELOFFX .NE. RELOFFY ) THEN
      IF ( ABS( RELOFFX - RELOFFY ) > MIN_DOUBLE ) THEN
!*end change by snl
         XMSG = 'Inconsistent X- and Y-resolution (file vs. GRIDDESC)'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

!     WRITE( LOGDEV,* ) ' RELOFFX, RELOFFY: ', RELOFFX, RELOFFY

      INDX = MYPE + 1
      GXOFF   = XORIG_C - XORIG3D
      STRTCOL = GXOFF + COLSX_PE( 1,INDX )
      ENDCOL  = STRTCOL + MY_NCOLS - 1 + DOTFILE
      GYOFF   = YORIG_C - YORIG3D
      STRTROW = GYOFF + ROWSX_PE( 1,INDX )
      ENDROW  = STRTROW + MY_NROWS - 1 + DOTFILE
      IF ( GXOFF .LT. 0 .OR. GYOFF .LT. 0 ) THEN
         XMSG = 'Model domain is outside file domain'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

!     WRITE( LOGDEV,* ) ' '
!     WRITE( LOGDEV,* ) ' File: ', FNAME
!     WRITE( LOGDEV,* ) '    StartCol: ', STRTCOL
!     WRITE( LOGDEV,* ) '      EndCol: ', ENDCOL
!     WRITE( LOGDEV,* ) '    StartRow: ', STRTROW
!     WRITE( LOGDEV,* ) '      EndRow: ', ENDROW

      RETURN
      END SUBROUTINE SUBHFILE
