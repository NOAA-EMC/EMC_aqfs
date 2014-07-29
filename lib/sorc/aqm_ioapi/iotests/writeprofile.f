C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 M 3TESTS.  Copyright (C) 2002 MCNC
C All Rights Reserved.
C.........................................................................

        PROGRAM PROFIL_TEST

C***********************************************************************
C  program body starts at line  99
C
C  DESCRIPTION:
C       Interactive test of PROFIL3 file types.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input file.
C       Follow the prompts.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 4/2002 by Carlie J. Coats, Jr.,
C       MCNC Environmental Modeling Center
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations

      INCLUDE 'PROFIL.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER        GETNUM
        LOGICAL        GETYN
        CHARACTER*16   PROMPTMFILE
        EXTERNAL       GETNUM, GETYN, PROMPTMFILE


C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'INTERP_TEST'
        CHARACTER*80       PROGVER
        DATA               PROGVER / '$Id$' /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    VNAME   !  input data  file logical name
        LOGICAL         EFLAG
        CHARACTER*256   MESG
        
        INTEGER         NRECS
        INTEGER         JDATE, JTIME, TSTEP

        INTEGER         C, R, L, V, N  !  loop counters
        
C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        L = INIT3()

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program INTERP_TEST for testing INTERP3() and INTERPX().',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical name of the input',
     &'data file, and repeatedly for the date&time and type of the',
     &'test to be performed.',
     &' ',
     &'Default responses are indicated in square brackets [LIKE THIS]',
     &'[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <output data file>    <path-name>',
     &' ',
     &'Program copyright (C) 2002 MCNC and released under Version 2',
     &'of the GNU General Public License.  See enclosed GPL.txt, or',
     &'URL  http://www.gnu.org/copyleft/gpl.html',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    envpro@emc.mcnc.org',
     &' ',
     &'    MCNC -- Environmental Modeling Center',
     &'    3021 Cornwallis Rd    P. O. Box 12889',
     &'    Research Triangle Park, NC 27709-2889',
     &' ',
     &'Program version: ' // PROGVER, 
     &' ',
     &'Program release tag: $Name$', 
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 
     &                   'Program terminated at user request', 2 )
        END IF
        
        JDATE = GETNUM( 0, 9999999, 2002102, 'Enter starting date' )
        JTIME = 0
        TSTEP = 10000
        NRECS = GETNUM( 1, 999, 8, 'Enter number of output records' )

        P_ALP3D = 0.0D0
        P_BET3D = 0.0D0
        P_GAM3D = 0.0D0
        XCENT3D = 0.0D0
        YCENT3D = 0.0D0
        XORIG3D = 0.0D0
        YORIG3D = 0.0D0
        XCELL3D = 0.0D0
        YCELL3D = 0.0D0
        FTYPE3D = PROFIL3
        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP
        NCOLS3D = NLEVS
        NROWS3D = NSTNS
        NLAYS3D = 1
        NTHIK3D = 1
        GDTYP3D = LATGRD3
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3

        N = 0

        N = N + 1
        VNAME3D(N) = 'VARS1'
        UNITS3D(N) = 'n/a'
        VDESC3D(N) = 'test variable 1'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'VARS2'
        UNITS3D(N) = 'n/a'
        VDESC3D(N) = 'test variable 2'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'VARS3'
        UNITS3D(N) = 'n/a'
        VDESC3D(N) = 'test variable 3'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'VARS4'
        UNITS3D(N) = 'n/a'
        VDESC3D(N) = 'test variable 4'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'VARS5'
        UNITS3D(N) = 'n/a'
        VDESC3D(N) = 'test variable 5'
        VTYPE3D(N) = M3REAL

        NVARS3D = N
        GDNAM3D = 'PROFILTEST'

        FDESC3D( 1 ) = 'Test file for PROFIL3 data'
        DO  L = 2, MXDESC3
            FDESC3D( L ) = ' '
        END DO
        
        FNAME = PROMPTMFILE( 'Enter output file name', FSUNKN3, 
     &                       'PROFIL', PNAME )


        DO  N = 1, NRECS

            STNCNT = NSTNS

            DO R = 1, STNCNT

                IDLIST( R ) = 1000*N + R
                LVLCNT( R ) = NLEVS
                XLONS( R ) = FLOAT( R - N )
                YLATS( R ) = FLOAT( N * R )
                ELEVS( R ) = FLOAT( N + R )

                DO  L = 1, LVLCNT( R )
                    VARS1( L,R ) = FLOAT( R + 100*N )
                    VARS2( L,R ) = FLOAT( R + 200*N )
                    VARS3( L,R ) = FLOAT( R + 300*N )
                    VARS4( L,R ) = FLOAT( R + 400*N )
                    VARS5( L,R ) = FLOAT( R + 500*N )
                END DO

            END DO

           IF ( .NOT.WRITE3( FNAME, 'ALL', JDATE,JTIME,STNCNT ) ) THEN
                MESG = 'Error writing test profile data'
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
           END IF

           CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO

C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0, 
     &               'Successful completion of program '//PNAME, 0 )

        END

       
