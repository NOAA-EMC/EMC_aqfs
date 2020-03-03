
        PROGRAM M3COMBO

C***********************************************************************
C Version "$Id: m3combo.f 44 2014-09-12 18:03:16Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.,
C (C) 2007, 2012 Bonyoung Koo, ENVIRON International Corporation.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  132
C
C  DESCRIPTION:
C       For a given input file, define a new output file with variables
C       that are linear combinations of the variables in the input file,
C       with specified coefficients.
C       For each time step in the specified time step sequence,
C       reads all variables from the specified input file, computes
C       the specified linear combinations, and writes them to the
C       specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv  <logical name> <physical (path) name> for the input and
C                                                     output files.
C       setenv  COMBO_VBLES <comma-delimited list of variable-names>
C       For each combo-variable,
C           setenv  <name>_VBLES  <comma-delimited list of input vbles>
C           setenv  <name>_COEFS  <comma-delimited list of coefficients>
C       Specified time step sequence is valid for the input file.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 8/2001 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       Version  11/2001 by CJC for I/O API Version 2
C       Version   7/2004 by CJC:  add offsets B_i as option.
C       Version   7/2004 bug-fix from M. Talat Odman, Ga. Tech.
C       Version   6/2005 by CJC:  improved default for NRECS
C       Version   2/2007 by CJC:  bug-fix for offsets that evidently
C       got "lost" in the previous revision.
C       BLANK initialization of FVARS, etc.
C       Version  5/2007 Bug-fixes from Bonyoung Koo, ENVIRON International
C       Version  5/2008 Another bug-fix from Bonyoung Koo, ENVIRON International
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 12/2011 by CJC:  bug-fixes for non-GRIDDED files, from
C       Bonyoung Koo, Environ
C       Version 11/2013 by CJC:  OpenMP parallel
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'M3COMBO'
        CHARACTER*16, PARAMETER :: BLANK   = ' '

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    ONAME   !  output data file logical name

        INTEGER         SIZE    ! grid volume, for copy
        INTEGER         NVARS   ! number of output variables
        INTEGER         NFILE   ! number of input files
        INTEGER         NVIN    ! number of input variables
        CHARACTER*16    ANAME
        CHARACTER*32    VFBUF
        CHARACTER*16    FNAME( MXFILE3 )
        CHARACTER*16    FVARS( MXVARS3, MXFILE3 )
        CHARACTER*16    VNAME( MXVARS3 )
        CHARACTER*16    UNITS( MXVARS3 )
        INTEGER         NCOEF( MXVARS3 )
        CHARACTER*16    IFILE( MXVARS3, MXVARS3 )
        CHARACTER*16    INAME( MXVARS3, MXVARS3 )
        CHARACTER*32    CNAME( MXVARS3*MXFILE3 )
        INTEGER         FINDX( MXVARS3*MXFILE3 )
        INTEGER         INDX( MXVARS3, MXVARS3 )
        REAL            COEF( MXVARS3, MXVARS3 )
        REAL            OFFS( MXVARS3 )

        LOGICAL         EFLAG

        INTEGER         NCOLS1      ! number of grid columns
        INTEGER         NROWS1      ! number of grid rows
        INTEGER         NLAYS1      ! number of layers
        INTEGER         NTHIK1      ! bdy thickness
        INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
        INTEGER         FTYPE1      ! file type:  gridded, custom, boundary...
        INTEGER         VGTYP1      ! vertical coord type
        INTEGER         SDATE1      ! starting date
        INTEGER         STIME1      ! starting time
        INTEGER         TSTEP1      ! time step
        INTEGER         NRECS1      ! number of records
        INTEGER         EDATE1      ! starting date
        INTEGER         ETIME1      ! starting time
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension
        REAL            VGTOP1      ! vertical coord top (sigma types)
        REAL            VGLEV1( MXLAYS3+1 )     !  "full" levels

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         EDATE, ETIME, TSECS, NRECS

        INTEGER         K, M, N, I, J, V, F
        REAL            C

        CHARACTER*24    FILBUF, NAMBUF, CHRBUF, OFFBUF, STRBUF
        CHARACTER*256   MESG, SCRBUF

        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status

        REAL,    ALLOCATABLE::   INBUF( :,: )
        REAL,    ALLOCATABLE::   OUTBUF( : )


C***********************************************************************
C   begin body of program M3COMBO

        LDEV  = INIT3()
        EFLAG = .FALSE.

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program M3COMBO to compute linear combinations',
     &' ',
     &'      Y_i = SUM A_ij*Xj + B_i',
     &' ',
     &'of variables, for specified coefficients {A_ij} and offsets',
     &'{B_i}, from a set of input files, for a specified timestep',
     &'sequence, and then write these to a specified output file.',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical names of the input',
     &'and output files, and for the time step sequence.Default ',
     &'responses are indicated in square brackets [LIKE THIS], and may',
     &'be accepted by hitting the RETURN key.',
     &' ',
     &'PRECONDITIONS REQUIRED:',
     &' ',
     &'    Time step sequence is valid for the input file.',
     &'    File type must be GRIDDED, BOUNDARY, or CUSTOM',
     &'    Input variables are all of type REAL',
     &' ',
     &'    setenv <input  file>    <path-name>',
     &'    setenv <output file>    <path-name>',
     &'    setenv COMBO_FILES      <list of names for the ',
     &'                             input files>',
     &'    setenv COMBO_VBLES      <list of names for the ',
     &'                             output variables>',
     &'    setenv COMBO_UNITS      <units for the ',
     &'                             output variables>',
     &'    For each output variable <name>, matching lists for the',
     &'    inputs:',
     &'         setenv  <name>_FILES  <list of input file names>',
     &'     *IF* COMBO_FILES has more than one entry',
     &'         setenv  <name>_VBLES  <list of input variable names>',
     &'         setenv  <name>_COEFS  <list of REAL coefficients>',
     &'         setenv  <name>_OFFSET <optional REAL offsets>',
     &' ',
     &'    All input files must have the same map projection and',
     &'    horizontal and vertical grid structure.',
     &' ',
     &'    Supported input file types:  GRIDDED, BOUNDARY, CUSTOM.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.,  (C) 2007, 2012 Bonyoung Koo,',
     &'ENVIRON International Corporation. Released under Version 2',
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
     &'$Id:: m3combo.f 44 2014-09-12 18:03:16Z coats                 $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            MESG = 'Program terminated at user request'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Get list of input data files
C...............  Open and get descriptions for them

        FNAME( : )   = BLANK
        FVARS( :,: ) = BLANK
        VNAME( : )   = BLANK
        UNITS( : )   = BLANK

        IF ( .NOT. STRLIST( 'COMBO_FILES',
     &                      'list of input-file names',
     &                       MXFILE3, NFILE, FNAME ) ) THEN

            MESG  = 'Bad list of output variable names'
            EFLAG = .TRUE.
            CALL M3MESG( MESG )

        ELSE IF ( .NOT. OPEN3( FNAME(1), FSREAD3,PNAME ) ) THEN

            MESG = 'Could not open ' // FNAME(1)
            EFLAG = .TRUE.
            CALL M3MESG( MESG )

        ELSE IF ( .NOT. DESC3( FNAME(1) ) ) THEN

            MESG = 'Could not get file description for ' // FNAME(1)
            EFLAG = .TRUE.
            CALL M3MESG( MESG )

        ELSE

            SDATE1 = SDATE3D
            STIME1 = STIME3D
            TSTEP1 = TSTEP3D
            NRECS1 = MXREC3D
            NCOLS1 = NCOLS3D
            NROWS1 = NROWS3D
            NLAYS1 = NLAYS3D
            NTHIK1 = NTHIK3D
            FTYPE1 = FTYPE3D
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
            VGTYP1 = VGTYP3D
            VGTOP1 = VGTOP3D
            VGLEV1( : )  = VGLVS3D( : )
            FVARS( 1:NVARS3D,1 ) = VNAME3D( : )

            IF ( FTYPE3D .EQ. GRDDED3 ) THEN
                SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
                SIZE = 2 * NLAYS3D * ABS( NTHIK3D )
     &                   * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
            ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
                SIZE = NCOLS3D * NLAYS3D
            ELSE
                WRITE( MESG, '( A, I10 )' )
     &            'Unsupported input file type', FTYPE3D
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            END IF
            CALL LASTTIME( SDATE1,STIME1,TSTEP1, NRECS1, EDATE1,ETIME1 )

            DO F = 2, NFILE

                IF ( .NOT. OPEN3( FNAME(F), FSREAD3,PNAME ) ) THEN

                    MESG = 'Could not open ' // FNAME(F)
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                ELSE IF ( .NOT. DESC3( FNAME(F) ) ) THEN

                    MESG = 'Could not get description for ' // FNAME(F)
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                ELSE IF ( .NOT.FILCHK3( FNAME(F),  FTYPE1,
     &                          NCOLS1, NROWS1, NLAYS1, NTHIK1 ) ) THEN

                    MESG = 'Inconsistent dimensions  for ' // FNAME(F)
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                ELSE IF ( .NOT.GRDCHK3( FNAME(F),
     &                          P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                          XORIG1, YORIG1, XCELL1, YCELL1,
     &                          NLAYS1, VGTYP1, VGTOP1, VGLEV1 ) ) THEN

                    MESG = 'Inconsistent coord/grid  for ' // FNAME(F)
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                ELSE IF ( TSTEP1 .EQ. 0 ) THEN

                    SDATE1 = SDATE3D
                    STIME1 = STIME3D
                    TSTEP1 = TSTEP3D
                    NRECS1 = MXREC3D
                    EDATE1 = SDATE1
                    ETIME1 = STIME1
                    TSECS  = ( NRECS1 - 1 )*TIME2SEC( TSTEP1 )
                    CALL NEXTIME( EDATE1, ETIME1, SEC2TIME( TSECS ) )

                ELSE IF ( TSTEP3D .NE. 0 ) THEN

                    JDATE = SDATE3D
                    JTIME = STIME3D
                    TSECS = SECSDIFF( JDATE, JTIME, SDATE1, STIME1 )
                    IF ( TSECS .GT. 0 ) THEN
                        SDATE1 = JDATE
                        STIME1 = JTIME
                    END IF

                    EDATE = SDATE3D
                    ETIME = STIME3D
                    TSECS = ( MXREC3D - 1 )*TIME2SEC( TSTEP1 )
                    CALL NEXTIME( EDATE, ETIME, SEC2TIME( TSECS ) )
                    TSECS = SECSDIFF( EDATE, ETIME, EDATE1, ETIME1 )
                    IF ( TSECS .LT. 0 ) THEN
                        EDATE1 = EDATE
                        ETIME1 = ETIME
                    END IF

                END IF

                FVARS( 1:NVARS3D,F ) = VNAME3D( : )

            END DO      !  end loop on the remaining input files

        END IF          !  if strlist() failed; else...


        IF ( EFLAG ) THEN
            MESG = 'Bad input-file setup for program'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Open and get description for first input data file

        IF ( .NOT. STRLIST( 'COMBO_VBLES',
     &                      'list of output-variable names',
     &                       MXVARS3, NVARS, VNAME ) ) THEN
            MESG = 'Bad list of output variable names'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT. STRLIST( 'COMBO_UNITS',
     &                      'list of output-variable units',
     &                       MXVARS3, N, UNITS ) ) THEN
            MESG = 'Bad list of output variable units'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        ELSE IF ( N .NE. NVARS ) THEN
            MESG = 'Mismatched count:  vbles and units'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        NVIN  = 0

        DO  N = 1, NVARS

            FILBUF = TRIM( VNAME( N ) ) // '_FILES'
            NAMBUF = TRIM( VNAME( N ) ) // '_VBLES'
            CHRBUF = TRIM( VNAME( N ) ) // '_COEFS'
            OFFBUF = TRIM( VNAME( N ) ) // '_OFFSET'

            IF ( .NOT. STRLIST( NAMBUF,
     &           'input-variables to construct ' // VNAME( N ),
     &           MXVARS3, NCOEF( N ), INAME( :,N ) ) ) THEN

                MESG  = 'Bad list of input names for '// VNAME( N )
                EFLAG = .TRUE.
                CALL M3MSG2( MESG )
                CYCLE

            END IF      !  if bad strlist

            IF ( NFILE .EQ. 1 ) THEN

                IFILE( :,N ) = FNAME( 1 )

            ELSE IF ( .NOT. STRLIST( FILBUF,
     &           'input-files to construct ' // VNAME( N ),
     &           MXVARS3, M, IFILE( :,N ) ) ) THEN

                MESG  = 'Bad list of input names for '// VNAME( N )
                EFLAG = .TRUE.
                CALL M3MSG2( MESG )
                CYCLE

            ELSE IF ( M .NE.  NCOEF( N ) ) THEN

                MESG = 'Mismatched count:  vbles and files for '//
     &                 VNAME( N )
                EFLAG = .TRUE.
                CALL M3MSG2( MESG )
                CYCLE

            END IF      !  if bad strlist for file-names

            IF ( .NOT. REALIST( CHRBUF,
     &           'coefficients to construct ' // VNAME( N ),
     &           MXVARS3, M, COEF( 1,N ) ) ) THEN

                MESG = 'Bad list of coefficients for '// VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
                CYCLE

            ELSE IF ( M .NE.  NCOEF( N ) ) THEN

                MESG = 'Mismatched count:  vbles and coeffs for '//
     &                 VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
                CYCLE

            END IF      !  if bad reallist for coeffs

            DO  M = 1, NCOEF( N )

                J = INDEX1( IFILE( M,N ), NFILE,  FNAME )
                IF ( J .LE. 0 ) THEN
                    MESG = 'File not in COMBO_FILES: '// IFILE( M,N )
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                I = INDEX1( INAME( M,N ), MXVARS3,  FVARS( :,J ) )

                IF ( I .LE. 0 ) THEN
                    MESG = 'Variable "' // TRIM( INAME( M,N ) ) //
     &                     '" not found in file "' //
     &                     TRIM( FNAME( J ) ) // '"'
                   CALL M3MSG2( MESG )
                   EFLAG = .TRUE.
                   CYCLE
               ELSE IF ( VTYPE3D( I ) .NE. M3REAL ) THEN
                    MESG = 'Variable "' // TRIM( INAME( M,N ) ) //
     &                     '" not of type REAL'
                   CALL M3MSG2( MESG )
                   EFLAG = .TRUE.
                   CYCLE
               END IF

                VFBUF = INAME( M,N ) // FNAME( J )
                I = INDEX1( VFBUF, NVIN,   CNAME )

                IF ( I .GT. 0 ) THEN
                    INDX( M,N ) = I
                ELSE
                    NVIN          = NVIN + 1
                    FINDX( NVIN ) = J
                    CNAME( NVIN ) = VFBUF
                    INDX( M,N )   = NVIN
                END IF

            END DO          !  end loop on inputs for this output vble

            OFFS( N ) = ENVREAL( OFFBUF, 'Offset for linear combo',
     &                           0.0, I )
            IF ( I .GT. 0 ) THEN
                MESG = 'Bad offset for '// VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF


        END DO          !  end loop on output variables


C...............  Allocate buffers:

        ALLOCATE( INBUF ( SIZE, NVIN ),
     &            OUTBUF( SIZE )      ,  STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10)' )
     &               'Buffer allocation failed:  STAT=', STATUS
            CALL M3MESG( MESG )
        END IF


        IF ( EFLAG ) THEN
            MESG = 'Bad setup for program'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Get time step sequence for the run:

        JDATE = GETNUM( SDATE1, 9999999, SDATE1,
     &                  'Enter STARTING DATE for time step sequence' )

        JTIME = GETNUM( 0, 9999999, STIME1,
     &                  'Enter STARTING TIME for time step sequence' )

        TSTEP = GETNUM( TSTEP1, 9999999, TSTEP1,
     &                  'Enter   TIME STEP   for time step sequence' )

        N     =  CURREC( EDATE1, ETIME1, JDATE, JTIME, TSTEP, I, J )
        NRECS = GETNUM( 1, 9999999, N,
     &                  'Enter     NRECS     for time step sequence' )


C...............  Create output file, borrowing most of file
C...............  description from FNAME:

        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP

        NVARS3D = NVARS

        DO  V = 1, NVARS
            VNAME3D( V ) = VNAME( V )
            UNITS3D( V ) = UNITS( V )
            VTYPE3D( V ) = M3REAL
            SCRBUF = 'Linear combination of '
            DO  M = 1, NCOEF( V )
                MESG = TRIM( SCRBUF )//' "'//TRIM( INAME( M,V ) )//'"'
                SCRBUF = MESG
            END DO
            VDESC3D( V ) = SCRBUF
        END DO      ! end loop on output variables

        ONAME =  PROMPTMFILE( 'Enter name for output combo file',
     &                        FSUNKN3, 'COMBO_3D', PNAME )


C...............  Process the output time step sequence

        DO  N = 1, NRECS

            DO V = 1, NVIN

                J = FINDX( V )
                ANAME = CNAME( V )( 1:16 )
                IF ( .NOT.READ3( FNAME( J ), ANAME, ALLAYS3,
     &                           JDATE, JTIME, INBUF( 1,V ) ) ) THEN
                    MESG = 'Failure reading variable "' //
     &                         TRIM( ANAME ) //
     &                         '" from file "' //
     &                         TRIM( FNAME( J ) ) // '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF

            END DO      !  end loop on input variables

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Input error', 2 )
            END IF

            DO  V = 1, NVARS

                I = INDX( 1,V )
                C = COEF( 1,V )
!$OMP           PARALLEL DO DEFAULT( NONE ),
!$OMP&                       SHARED( SIZE, OUTBUF, INBUF, OFFS, C, I, V ),
!$OMP&                      PRIVATE( K )
                DO  K = 1, SIZE
                    OUTBUF( K ) = C * INBUF( K,I ) + OFFS( V )
                END DO

                DO  M = 2, NCOEF( V )
                    I = INDX( M,V )
                    C = COEF( M,V )
!$OMP               PARALLEL DO DEFAULT( NONE ),
!$OMP&                           SHARED( SIZE, OUTBUF, INBUF, C, I ),
!$OMP&                          PRIVATE( K )
                    DO  K = 1, SIZE
                        OUTBUF( K ) = OUTBUF( K ) + C * INBUF( K,I )
                    END DO
                END DO

                IF ( .NOT.WRITE3( ONAME, VNAME( V ),
     &                           JDATE, JTIME, OUTBUF ) ) THEN
                    MESG = 'Failure writing variable "' //
     &                         TRIM( VNAME( V ) ) //
     &                         '" to file "' //
     &                         TRIM( ONAME ) // '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF

            END DO      !  end loop on output variables

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, JDATE, JTIME, 'Output error', 2 )
            END IF

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( 'M3COMBO', 0, 0,
     &               'Successful completion of program M3COMBO', 0 )

        END PROGRAM M3COMBO
