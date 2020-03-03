
      PROGRAM CAMXTOM3

C***********************************************************************
C Version "$Id: camxtom3.f 43 2014-09-12 14:06:19Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  103
C
C  DESCRIPTION:
C       Convert CAMx/UAM style files to M3IO
C
C  PRECONDITIONS REQUIRED:
C       setenv GRIDDESC  <path name>
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O API
C
C  REVISION  HISTORY:
C       Prototype  12/2006 by Carlie J. Coats, Jr., BAMS
C       Error-message bug-fix 10/2011 by CJC
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........  PARAMETERS and their descriptions:

        INTEGER     , PARAMETER :: MXSPEC= 200
        CHARACTER*16, PARAMETER :: PNAME = 'CAMXTOM3'
        CHARACTER*16, PARAMETER :: BLANK = ' '
        CHARACTER*64, PARAMETER :: BAR   =
     &'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LDEV, IDEV
        INTEGER         ISTAT
        LOGICAL         EFLAG, AFLAG
        CHARACTER*256   MESG

        INTEGER         JYEAR, JDATE, JTIME
        INTEGER         SDATE, STIME, TSTEP, NRECS
        INTEGER         C, R, K, L, V, M, IREC, JREC

        CHARACTER*16    GNAME, CNAME

        INTEGER         NCOLS1      !  parms for  input grid
        INTEGER         NROWS1
        INTEGER         NLAYS1
        INTEGER         NTHIK1
        INTEGER         GDTYP1
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension

        !!  file-header variables:

        CHARACTER*4     FNAME( 10 )
        CHARACTER*4     VNAME( 10, MXSPEC )
        CHARACTER*4     VARBUF( 10 )
        CHARACTER*4     NOTE( 60 )
        CHARACTER*10    FTYPE
        INTEGER         NSEG, NSPEC, BDATE, EDATE
        REAL            BTIME, ETIME
        INTEGER         IZONE
        REAL            X1, Y1, XORIG, YORIG, DX, DY
        INTEGER         NX, NY, NZ, NZLO, NZUP
        REAL            HTS, HTL, HTU
        INTEGER         I1, J1, NX1, NY1

        !!  Working variables:

        CHARACTER(LEN=NAMLEN3) :: NAMBUF            = BLANK
        CHARACTER(LEN=NAMLEN3) :: VNAMES( MXVARS3 ) = BLANK

        REAL,    ALLOCATABLE :: RBUF( :,:,: )


C...........   STATEMENT FUNCTION:  REAL*8 "definitely unequal"

        LOGICAL         DBLERR
        REAL*8          P
        REAL            Q

        DBLERR( P, Q ) =
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


C***********************************************************************
C   begin body of program RAINFIX

        LDEV  = INIT3()
        EFLAG = .FALSE.
        WRITE( LDEV, '( 5X, A )' )
     &'Program CAMXTOM3 to convert CAMx (and other UAM-format) binary',
     &'files to Models-3 I/O API.',
     &'Note that additional coordinate system is supported, because of',
     &'the fact that UAM format does not adequately characterize map',
     &'projections, nor does it provide adequate precision in the',
     &'grid description parameters it does provide.  Even so, the',
     &'vertical grid description will be set to "MISSING" because it',
     &'is still not adequately characterized.',
     &'',
     &'This version should be considered a beta-test release.',
     &'',
     &'ACKNOWLEDGEMENT',
     &'    This program was partially developed under funding for a',
     &'    project sponsored by the California Air Resources Board.',
     &'',
     &'PRECONDITIONS REQUIRED:',
     &'    setenv GRIDDESC  <path name>',
     &'    setenv INFILE    <path name for CAMx  input file>',
     &'    setenv OUTFILE   <path name for M3IO output file>',
     &'',
     &'    setenv GRID_NAME <GRIDDESC name for model-grid>',
     &'',
     &'THE PROGRAM WILL PROMPT YOU for the starting date (YYYYDDD),',
     &'starting time (HHMMSS), and time step (HHMMSS).',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' '
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
     &'$Id:: camxtom3.f 43 2014-09-12 14:06:19Z coats                $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            MESG = 'Program terminated at user request'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Get model-grid parameters

        CALL ENVSTR( 'GRID_NAME', 'GRIDDESC-name for CAMx grid',
     &               'CCOS_CAMX_CRO', GNAME, ISTAT )

        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "GRID_NAME"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.DSCGRID( GNAME,  CNAME,  GDTYP1,
     &                          P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                          XORIG1, YORIG1, XCELL1, YCELL1,
     &                          NCOLS1, NROWS1, NTHIK1 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Grid not found in GRIDDESC: ' // GNAME
            CALL M3MESG( MESG )
        END IF


C...............  Open input file and read header:

        IDEV = GETEFILE( 'INFILE', .TRUE., .FALSE., PNAME )

        IF ( IDEV .LT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not open "INFILE"'
            CALL M3MESG( MESG )
        ELSE

            READ( IDEV, IOSTAT=ISTAT )
     &           FNAME, NOTE, NSEG, NSPEC, BDATE, BTIME, EDATE, ETIME
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Error reading header record 1 in INFILE'
                CALL M3MESG( MESG )
            ELSE
                DO L = 1, 10
                    FTYPE( L:L ) = FNAME(L)( 1:1 )
                END DO
                CALL M3MESG( 'INFILE header record 1:' )
                WRITE( LDEV,
     &             '(5X,10A1,60A1,/5X,I2,1X,I2,1X,I6,F6.0,I6,F6.0)' )
     &             FNAME, NOTE, NSEG, NSPEC, BDATE, BTIME, EDATE, ETIME
            END IF

            READ( IDEV, IOSTAT=ISTAT )
     &           X1, Y1, IZONE, XORIG, YORIG, DX, DY,
     &           NX, NY, NZ, NZLO, NZUP, HTS, HTL, HTU
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Error reading header record 2 in INFILE'
                CALL M3MESG( MESG )
            ELSE
                NLAYS1 = NZ
                CALL M3MESG( 'INFILE header record 2:' )
                WRITE( LDEV,
     &             '(5X,2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)' )
     &           X1, Y1, IZONE, XORIG, YORIG, DX, DY,
     &           NX, NY, NZ, NZLO, NZUP, HTS, HTL, HTU
            END IF

            READ( IDEV, IOSTAT=ISTAT ) I1, J1, NX1, NY1
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Error reading header record 3 in INFILE'
                CALL M3MESG( MESG )
            ELSE
                CALL M3MESG( 'INFILE header record 3:' )
                WRITE( LDEV, '(5X, 4 I5)' ) I1, J1, NX1, NY1
            END IF

            READ( IDEV, IOSTAT=ISTAT )
     &           ( (VNAME(M,V), M=1, 10), V=1, NSPEC)
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Error reading header record 4 in INFILE'
                CALL M3MESG( MESG )
            ELSE
                DO V = 1, NSPEC
                DO M = 1, 10
                    VNAMES( V )( M:M ) = VNAME( M,V )( 1:1 )
                END DO
                END DO
                CALL M3MESG( 'INFILE header record 4:' )
                WRITE( LDEV, '( 200 ( 5X, 10 A1, :, / ) )' )
     &           ( (VNAME(M,L), M=1, 10), L=1, NSPEC)
            END IF

        END IF

        IF ( FTYPE .EQ. 'BOUNDARY' .OR. FTYPE .EQ. 'PTSOURCE' ) THEN
            EFLAG = .TRUE.
            MESG  = 'Unsupported file type: ' // FTYPE
            CALL M3MESG( MESG )
        END IF

        IF ( NSPEC .GT. MXVARS3 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '(A, I4 )' ) 'Unsupported vble-count', NSPEC
            CALL M3MESG( MESG )
        END IF


        IF ( EFLAG ) THEN
            MESG = 'Program environment/setup error(s).'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Allocate input buffer:

        ALLOCATE( RBUF( NCOLS1,NROWS1,NLAYS1 ), STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
           WRITE( MESG, '( A, I10 )' )
     &               'Buffer allocation failed:  STAT=', ISTAT
            CALL M3MESG( MESG )
        END IF


C...............  Check dimensions and coordinates:

        IF ( NCOLS1 .NE. NX ) THEN
            EFLAG = .TRUE.
            MESG  = 'NCOLS mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( NROWS1 .NE. NY ) THEN
            EFLAG = .TRUE.
            MESG  = 'NROWS mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( DBLERR( XCELL1, DX ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'XCELL mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( DBLERR( YCELL1, DY ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'YCELL mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( DBLERR( XORIG1, XORIG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'XORIG mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( DBLERR( YORIG1, YORIG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'YORIG mismatch'
            CALL M3MESG( MESG )
        END IF

        IF ( EFLAG ) THEN
            MESG = 'Inconsistent grid parameter(s).'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Get episode parameters:

        CALL GETDTTIME( JDATE, JTIME )
        JYEAR = JDATE / 1000
        JDATE = 1000 * JYEAR + BDATE
        JTIME = 10000 * NINT( BTIME )

        SDATE = GETNUM( 0, 9999999, JDATE,
     &                 'Enter starting date (YYYYDDD)' )
        STIME = GETNUM( 0, 9999999, JTIME,
     &                 'Enter starting time  (HHMMSS)' )
        TSTEP = GETNUM( 10000, 9999999, 10000,
     &                 'Enter time step      (H*MMSS)' )

        JYEAR = SDATE / 1000

C...............  Open output file:

        FTYPE3D = GRDDED3
        NCOLS3D = NCOLS1
        NROWS3D = NROWS1
        NLAYS3D = NLAYS1
        P_ALP3D = P_ALP1
        P_BET3D = P_BET1
        P_GAM3D = P_GAM1
        XCENT3D = XCENT1
        YCENT3D = YCENT1
        XORIG3D = XORIG1
        YORIG3D = YORIG1
        XCELL3D = XCELL1
        YCELL3D = YCELL1
        GDTYP3D = GDTYP1
        SDATE3D = SDATE
        STIME3D = STIME
        TSTEP3D = TSTEP

        VGTYP3D = IMISS3        !  unknown, not in file header.
        VGTOP3D = 0.0
        VGLVS3D(:) = 0.0

        NVARS3D = NSPEC
        VNAME3D( 1:NSPEC ) = VNAMES( 1:NSPEC )
        UNITS3D( 1:NSPEC ) = 'unknown'
        VDESC3D( 1:NSPEC ) = 'variable from CAMx/UAM file'
        VTYPE3D( 1:NSPEC ) = M3REAL

        FDESC3D( : ) = BLANK
        FDESC3D( 1 ) = 'Data extracted from CAMx/UAM-format file'
        DO L = 1, 60
            FDESC3D( 2 )(L:L) = NOTE( L )(1:1)
        END DO

        IF ( .NOT. OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open OUTFILE"'
            EFLAG = .TRUE.
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Process the file:

        IREC = 0

11      CONTINUE        !  head of input-loop

            IREC = IREC + 1
            READ( IDEV,END=99, IOSTAT=ISTAT ) BDATE, BTIME, EDATE, ETIME
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I10 )' )
     &           'Error', ISTAT, 'reading header of timestep', IREC
                CALL M3MESG( MESG )
                GO TO  99
            ELSE
                JDATE =  1000 * JYEAR  +  BDATE
                JTIME = 10000 * NINT( BTIME )
                WRITE( LDEV, '(5X, A, 2X, 2(I10, F10.2 ) )' )
     &               'Processing', BDATE, BTIME, EDATE, ETIME
            END IF

            JREC  = JSTEP3( JDATE, JTIME, SDATE, STIME, TSTEP )

            DO V = 1, NSPEC

                DO L = 1, NLAYS1
                    READ( IDEV,IOSTAT=ISTAT )
     &               K, ( VARBUF( M ), M=1, 10 ),
     &               ( ( RBUF( C,R,L ), C=1, NCOLS1 ), R=1, NROWS1 )
                    IF ( ISTAT .NE. 0 ) THEN
                        EFLAG = .TRUE.
                        WRITE( MESG, '( 4 ( A, I10, :, 2X ) )' )
     &                     'Error', ISTAT,
     &                     'reading data for record', IREC,
     &                     'variable', V, 'layer', L
                        CALL M3MESG( MESG )
                        GO TO  99
                    END IF
                END DO

                DO M = 1, 10
                    NAMBUF(M:M) = VARBUF( M )(1:1)
                END DO

                IF ( NAMBUF .NE. VNAMES( V ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Inconsistent variable names "' //
     &                      TRIM( NAMBUF ) // '" and "' //
     &                      TRIM( VNAME3D( V ) ) // '"'
                    CALL M3MESG( MESG )
                    GO TO  11
                END IF

                IF ( JREC .LT. 0 ) THEN
                    WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )
     &                   'Skipping "', TRIM( NAMBUF ),
     &                   '" for date&time', JDATE, ':', JTIME
                    CALL M3MESG( MESG )
                ELSE IF ( .NOT.WRITE3( 'OUTFILE', NAMBUF,
     &                                 JDATE, JTIME, RBUF ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Could not write "' //
     &                      TRIM( NAMBUF ) // '" to "OUTFILE"'
                    CALL M3MESG( MESG )
                END IF

            END DO

            GO TO  11   !  to head of input-loop


99      CONTINUE        !  target of "END=" clause



        IF ( EFLAG ) THEN
            MESG  = 'Failure in program'
            ISTAT = 2
        ELSE
            MESG  = 'Success in program'
            ISTAT = 0
        END IF

        CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


      END PROGRAM CAMXTOM3

