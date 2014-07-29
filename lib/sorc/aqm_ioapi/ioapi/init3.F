
        INTEGER FUNCTION INIT3 ( )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  146
C
C  FUNCTION:
C       Initialize state for Models-3 I/O.
C       May be called multiple times whenever the caller wants the
C       unit number for the I/O API system log file.
C
C  RETURN VALUE:
C       unit number for the log file
C
C  PRECONDITIONS REQUIRED:
C       Only call from within "!$OMP CRITICAL( S_LOGDEV )" blocks
C       Do *NOT* call from within "!$OMP CRITICAL( S_NC )" blocks
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       NAMEVAL(), ENVYN(), JUNIT()
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C
C       Modified  8/1995 by CJC to support CLOSE3()
C
C       Modified  4/1996 by CJC to open SCENFILE first time only.
C
C       Modified  5/1996 by CJC
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Modified  9/1999 by ALT for release version
C
C       Modified 11/2001 by CJC for version 2.1
C
C       Modified  5/2003 by CJC:  additional logging; flush LOGDEV
C       for opening screen; environment variable and STATE3 variable
C       STDOUT for control of messages to screen from M3MSG2(), etc.
C
C       Modified  5/2003 by CJC:  factor message output through M3MSG2()
C
C       Modified 10/2003 by CJC:  log all 13 lines of NOTICE (instead of
C       only lines 1:11); update release date; I/O API version moves to
C       CSTATE3 variable VERSN3
C
C       Modified 12/2003 by CJC:  Hack "CALL INITBLK3" to deal with
C       failure of SGI version 7.4 compilers to follow industry standards;
C       re-structuring with INITLOG3() and removal of calls to M3MSG2()
C       to avoid problems with nested critical sections.
C
C       Modified 9/2005 by CJC:  opening screen writes out MXVARS3
C
C       Modified 2/2006 by CJC:  char*512 EQNAME (instead of 256)
C
C       Modified 10/2006 by CJC: fix "STDOUT" logging bug
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'NETCDF.EXT'
        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'

#ifdef   _AIX
#define  FLUSH flush_
#endif


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INITLOG3, JUNIT
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   PARAMETERs:

        CHARACTER*16, PARAMETER:: SCENFILE = 'SCENFILE'
        CHARACTER*64, PARAMETER :: NOTICE( 17 ) = (/
     &'                                                              ',
     &'This program uses the EPA-AREAL/MCNC-EnvPgms/BAMS Models-3    ',
     &'I/O Applications Programming Interface, [I/O API] which is    ',
     &'built on top of the netCDF I/O library (Copyright 1993, 1996  ',
     &'University Corporation for Atmospheric Research/Unidata       ',
     &'Program) and the PVM parallel-programming library (from       ',
     &'Oak Ridge National Laboratory).  Copyright (C) 1992-2002 MCNC ',
     &'and Carlie J. Coats, Jr., and 2003-2006 Baron Advanced        ',
     &'Meteorological Systems, LLC and released under the GNU LGPL   ',
     &'License, version 2.1.  See URL                                ',
     &'                                                              ',
     &'    http://www.baronams.com/products/ioapi/LGPL.txt           ',
     &'                                                              ',
     &'for conditions of use.                                        ',
     &'                                                              ',
     &'Library release tag: $JDate: 2010068 $                        ',
     &'                                                              '
     &   /)


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*512   EQNAME
        INTEGER         I, J, IOST, IDEV
        LOGICAL         AFLAG

        CHARACTER *80 VARVER
        CHARACTER *80 NCFVER
        CHARACTER *80, PARAMETER :: IOAPILIBVER =
     &             '$Id: @(#) ioapi library version 3.1'
!    &             // VERSION
!$   &             // ' OpenMP enabled'
     &             // ' $'
#ifdef IOAPICPL
        CHARACTER *80 IOCPLVER
        CHARACTER *80 PVMVER
#endif


C***********************************************************************
C   begin body of subroutine  INIT3

#if defined(__sgi)    || defined(__mips__)

        !!==========>  *HACK*  to deal with SGI v7.4  compilers, that
        !!             do not follow industry standards with respect
        !!             to how to make sure that BLOCK DATA modules are
        !!             properly linked into an executable:

        CALL INITBLK3

#endif

        IF ( FINIT3 ) THEN
            INIT3 = LOGDEV
            RETURN           !  M3 I/O already set up
        END IF

        LOGDEV = INITLOG3( 'INIT3' )

!$OMP   CRITICAL( S_NC )
        CALL NCPOPT( NCVERBOS )
        NCFVER = NF_INQ_LIBVERS()
!$OMP   END CRITICAL( S_NC )

#ifdef IOAPICPL
        CALL GET_IOCPL_VERSION( IOCPLVER )
        CALL GET_PVM_VERSION  ( PVMVER )
#endif
        VERSN3 = IOAPILIBVER
        WRITE( VARVER, '( A, I5 )' )
     &    'Version with PARMS3.EXT/PARAMETER::MXVARS3=', MXVARS3

        WRITE( LOGDEV,'( 5X, A )' )
     &          ( NOTICE( I ), I = 1, 17 ),
     &          TRIM( VERSN3 ),
     &          TRIM( VARVER ),
#ifdef          IOAPICPL
     &          'IOCPL version '// TRIM( IOCPLVER ),
     &          'PVM   version '// TRIM( PVMVER   ),
#endif
     &          'netCDF version '// TRIM( NCFVER ),
     &          ' '

        IF ( .NOT.STDOUT ) THEN  !  also log this info to the screen:

            WRITE( 6,'( 5X, A )' )
     &          ( NOTICE( I ), I = 1, 17 ),
     &          TRIM( VERSN3 ),
#ifdef IOAPICPL
     &          'IOCPL version '// TRIM( IOCPLVER ),
     &          'PVM   version '// TRIM( PVMVER   ),
#endif
     &          'netCDF version '// TRIM( NCFVER ),
     &          ' '
	    CALL FLUSH( 6 )

        END IF      !  if stdout

        WRITE (LOGDEV, '( 5X, A )' ) ' '

        CALL NAMEVAL( SCENFILE, EQNAME )
        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

        IF ( AFLAG ) THEN

            IDEV = JUNIT( )
            OPEN ( UNIT    =  IDEV,
     &             IOSTAT  =  IOST,
     &             FILE    =  EQNAME,
     &             STATUS  =  'OLD',
     &             ACCESS  =  'SEQUENTIAL' ,
     &             FORM    =  'FORMATTED' )
            IF ( IOST .NE. 0 ) THEN
                WRITE( LOGDEV,91020 )
     &          'Error opening scenario file.  Error number', IOST,
     &          'File: ', EQNAME
                GO TO  12
            END IF
            DO  11  I = 1 , MXDESC3
                READ( IDEV, '( A )', END=12, IOSTAT=IOST )
     &              SCNDSC( I )
                IF ( IOST .NE. 0 ) THEN
                    WRITE( LOGDEV,91020 )
     &              'Error reading scenario file.  Error number',
     &              IOST,
     &              'File: ', EQNAME
                    GO TO  12   ! escape this loop
                END IF
11              CONTINUE
12              CONTINUE            ! target of loop-escape

            CLOSE( IDEV )

        END IF          !  if aflag:  scenfile exists

        CALL NAMEVAL( 'EXECUTION_ID', EXECN3 )
        IF ( EXECN3 .EQ. 'EXECUTION_ID' ) THEN
            EXECN3 = CMISS3
            WRITE( LOGDEV, '( 5X, A )'  )
     &         'Missing environment variable EXECUTION_ID'
        ELSE IF ( EXECN3( 1:1 ) .EQ. CHAR( 0 ) ) THEN
            EXECN3 = CMISS3
        ELSE
            WRITE( LOGDEV, '( 5X, 2 A )'  )
     &         'EXECUTION_ID: ', TRIM( EXECN3 )
        END IF


        ILCNT3 = 0
        DO  22  J = 1, MXFILE3
            FLIST3( J ) = CMISS3  !  "invalid"
            BSIZE3( J ) = IMISS3  !  "invalid"
            NLIST3( J ) = 0       !  "empty"
            IFRST3( J ) = IMISS3  !  "invalid"
            ILIST3( J ) = IMISS3  !  "invalid"
            DO  21  I = 1, MXVARS3
                LDATE3( I,J ) = IMISS3  !  "invalid"
                LTIME3( I,J ) = IMISS3  !  "invalid"
                NDATE3( I,J ) = IMISS3  !  "invalid"
                NTIME3( I,J ) = IMISS3  !  "invalid"
                VLIST3( I,J ) = CMISS3  !  "invalid"
21          CONTINUE
22      CONTINUE

        CALL GETDTTIME( CURDATE, CURTIME )

        IF ( COUNT3 .LT. 0 ) THEN
                WRITE( LOGDEV, '( /, 5X, A, /)'  )
     &             'INIT3() called subsequent to SHUT3()'
        END IF

	CALL FLUSH( LOGDEV )

        FINIT3 = .TRUE.
        COUNT3 = 0
        INIT3  = LOGDEV

        RETURN


C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '*** ERROR ABORT in subroutine INIT3 ***',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine INIT3 <<<',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )

91030   FORMAT ( //5X , '>>> WARNING in subroutine INIT3 <<<',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )


        END FUNCTION INIT3

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#ifdef   AVOID_FLUSH

        SUBROUTINE FLUSH( IDEV )
        IMPLICIT NONE
        INTEGER IDEV
        RETURN
        END

#endif
