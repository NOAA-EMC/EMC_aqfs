      SUBROUTINE pre_beis ( premaq_date, premaq_time )

C***********************************************************************
C
C  DESCRIPTION:
C     opens files and initializes arrays for biogenic emission processing
C     should only be called once in premaq
C     read GSPRO, normalized emission files
C
C  PRECONDITIONS REQUIRED:
C     Normalized gridded emissions B3GRD from NORMBEIS311 
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C     HRBIO, PREBMET
C
C  REVISION  HISTORY:
C    4/01: Prototype by Jeff Vukovich
C          Tested only on 36km Lambert domain 
C          Summer/winter switch file option not tested
C    4/03: extracted components from Beis3.11 for Air Quality Forecasting
C          George Pouliot
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id:  $
C
C COPYRIGHT (C) 2001, MCNC--North Carolina Supercomputing Center
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C MCNC-Environmental Programs Group
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C
C env_progs@mcnc.org
C
C Pathname: $Source:  $
C Last updated: $Date:  $ 
C
C***********************************************************************

C.... Modules for public variables
C.... This module contains the speciation profile tables

      USE MODSPRO

C.... This module contains BEIS3 specific arrays

      USE MODBEIS3V11

C.... This module contains gridded output common blocks from premaq
C     used to get LAT and LON from PREMAQ
C
      USE GROUTCOM

      IMPLICIT NONE

C.... INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations
      INCLUDE 'EMCNST3.EXT'     !
      INCLUDE 'B3V12DIMS3.EXT'     ! biogenic-related constants

C.... PARAMETERS and their descriptions:
C.... LOCAL PARAMETERS

      CHARACTER*50, PARAMETER :: CVSW = '$Name:  $' ! CVS release tag


      REAL, ALLOCATABLE :: BUFFER (: ,: )

        
C...........   EXTERNAL FUNCTIONS and their descriptions:

      EXTERNAL        ENVINT, ENVYN, GETFLINE, HHMMSS, INDEX1,
     &                PROMPTMFILE, PROMPTFFILE, TRIMLEN, VERCHAR

      LOGICAL         ENVYN

      CHARACTER*50    GETCFDSC
      CHARACTER*10    HHMMSS
      CHARACTER*16    PROMPTMFILE, VERCHAR
      CHARACTER*16    UNITSMENU( 2 )        ! output units
      CHARACTER*256   EQNAME                ! env var of filename
      CHARACTER*300   MESG                  ! message buffer for M3EXIT()

      CHARACTER*16 :: PROGNAME = 'PRE_BEIS' ! program name

      DATA            UNITSMENU / 'mole/hr ', 'mole/s ' /

      INTEGER         ENVINT, GETFLINE, INDEX1, PROMPTFFILE, TRIMLEN
      INTEGER         premaq_date, premaq_time, ISTAT

      INTEGER         B, M    !  counters for biogenic, model species
      INTEGER         I, J, K, L, N, C, R  !  loop counters & subscripts

C***********************************************************************
C   begin body of pre_beis
C***********************************************************************

      call M3MSG2 ('PRE_BEIS begins')

      ASSUME_SUMMER = .TRUE.
      SWITCH_FILE   = .FALSE.
	
      LDEV = INIT3()

      RHOURS = 24
      TZONE = 0

      JDATE = premaq_date
      JTIME = premaq_time

C-----------------------------------------------------------------------
C.... Write time zone to character string
C-----------------------------------------------------------------------

      WRITE ( CTZONE,94000 ) TZONE

C-----------------------------------------------------------------------
C.... Speciation profile to use for biogenics
C-----------------------------------------------------------------------

      SPPRO = 'B3V10'

      SOILOUT_TIME = ENVINT ('SOILOUT_TIME',
     &                   'time step to write soil output file',25,ISTAT)

C-----------------------------------------------------------------------
C.... Determine output units : only moles/hour and moles/sec supported
C-----------------------------------------------------------------------
      UNITTYPE = 2

      MESG =  'BEIS3 output will be ' // UNITSMENU( UNITTYPE )  
      WRITE ( LDEV, 92000 ) MESG

      RDEV = PROMPTFFILE(
     &                'Enter logical name for SPECIATION PROFILES file',
     &                .TRUE., .TRUE., 'GSPRO', PROGNAME )

C-----------------------------------------------------------------------
C
C.... Scan speciation profiles file to get all of the pollutant-species
C     combinations that are valid for the pollutants in the inventory.
C
C.... The species names are sorted in ABC order for each pollutant, and
C     and the pollutants are in the same order as BIOTYPES.
C
C.... Also retrieve the maximum number of species per pollutant and
C     maximum number of profile entries per pollutant.
C
C-----------------------------------------------------------------------

      CALL DSCSPROF( RDEV, NSEF, BIOTYPES )

      NLINES = GETFLINE( RDEV, 'Speciation profile file' )

      ALLOCATE( EMSPC( NLINES ), STAT=IOS )
      CALL CHECKMEM( IOS, 'EMSPC', PROGNAME )

      MSPCS = 0

C-----------------------------------------------------------------------
C.... Find emitting species names
C-----------------------------------------------------------------------

      DO I = 1, NSEF
         DO J = 1, MXSPEC
            IF ( SPCNAMES( J, I ) .EQ. ' ' ) CYCLE

            K = INDEX1 ( SPCNAMES( J, I ), MSPCS, EMSPC ) 

            IF ( K .EQ. 0 ) THEN
               MSPCS = MSPCS + 1
               EMSPC( MSPCS ) = SPCNAMES( J, I )
            ENDIF
         ENDDO
      ENDDO

      ALLOCATE( MLFAC ( MSPCS, NSEF ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MLFAC', PROGNAME )

      ALLOCATE( MSFAC ( MSPCS, NSEF ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MSFAC', PROGNAME )

C-----------------------------------------------------------------------
C.............  Read speciation profiles file
C-----------------------------------------------------------------------

      MESG = BLANK5 // 'Reading biogenic speciation profile...'
      CALL M3MSG2( MESG )

      CALL RDBPRO( RDEV, SPPRO, NSEF, BIOTYPES, MSPCS, EMSPC, 
     &             MLFAC, MSFAC ) 

C.... Get the method to calculate PAR

      PARTYPE = 1

      MESG =  'PAR calculation will ' // PARMENU( PARTYPE )  
      WRITE( LDEV, 92000 ) MESG

C.... Check to see if frost date switch file to be used

      MESG = 'Using a frost date switch file?'
      SWITCH_FILE = ENVYN ( 'BIOSW_YN', MESG, .FALSE., IOS )

C-----------------------------------------------------------------------
C.... Get normalized emissions file, BGRD
C-----------------------------------------------------------------------

      NNAME = PROMPTMFILE( 
     &              'Enter name for NORMALIZED EMISSIONS input file',
     &              FSREAD3, 'B3GRD', PROGNAME )

C.... Read description of normalized emissions file

      IF ( .NOT. DESC3( NNAME ) ) THEN
         MESG = 'Could not get description of file "' //
     &           NNAME( 1:TRIMLEN( NNAME ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

C-----------------------------------------------------------------------
C.... Initialize grid definition 
C-----------------------------------------------------------------------

!AQF  CALL CHKGRID( NNAME, 'GRID' , 0, EFLAG )

      NCOLS = NCOLS3D 
      NROWS = NROWS3D
      NGRID = NCOLS3D * NROWS3D
      LUSE  = GETCFDSC ( FDESC3D, '/LANDUSE/', .FALSE. )

C-----------------------------------------------------------------------
C.... Frost date switch file to be used
C-----------------------------------------------------------------------

      IF ( SWITCH_FILE ) THEN

C.... Get bioseason switch file, BIOSEASON

         BNAME = PROMPTMFILE( 
     &            'Enter name for season switch input file',
     &             FSREAD3, 'BIOSEASON', PROGNAME )
           
C.... Read description of switch file

         IF ( .NOT. DESC3( BNAME ) ) THEN
            MESG = 'Could not get description of file "' //
     &              NNAME( 1:TRIMLEN( BNAME ) ) // '"'
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
         ENDIF

C.... Check grid definition 

!AQF        CALL CHKGRID( BNAME, 'GRID' , 0 , EFLAG )

!AQF        IF ( EFLAG ) THEN
!AQF           MESG = 'Problems opening input files. See ERROR(S) above.'
!AQF           CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
!AQF        ENDIF

         ALLOCATE( SWITCH( NCOLS, NROWS), STAT=IOS )
         CALL CHECKMEM( IOS, 'SWITCH', PROGNAME )
         SWITCH = 0   ! array

      ELSE

         MESG = 'Use summer normalized emissions?'
         ASSUME_SUMMER = ENVYN ( 'SUMMER_YN', MESG, .TRUE., IOS )

      ENDIF

C-----------------------------------------------------------------------
C met fields needed :
C      temperature (10 meter)
C      nonconvective rainfall
C      convective rainfall
C      surface pressure
C      radiation  
C      lat
C      lon
C-----------------------------------------------------------------------

C.... Get met and cloud scheme descriptions from M3NAME if they exist

      METSCEN  = GETCFDSC( FDESC3D, '/MET SCENARIO/', .FALSE. )
      CLOUDSHM = GETCFDSC( FDESC3D, '/CLOUD SCHEME/', .FALSE. )

!AQF    M2NAME = M3NAME

C-----------------------------------------------------------------------
C.... Get default time characteristic for output file:
C.... If we're going to prompt, then set the defaults based on met
C.... otherwise, use environment variables to set defaults
C-----------------------------------------------------------------------

      NSTEPS = MXREC3D

!AQF    CALL GETM3EPI( TZONE, JDATE, JTIME, NSTEPS )
        
C-----------------------------------------------------------------------
C.... Build description for, and create/open output file
C.... (all but variables-table in description is borrowed from M3NAME)
C-----------------------------------------------------------------------

      SDATE3D = premaq_date
      STIME3D = premaq_time
      BSTEPS  = NSTEPS
      MXREC3D = BSTEPS
      NVARS3D = MSPCS
      NLAYS3D = 1
      TSTEP3D = 10000
        
      DO M = 1, MSPCS
         VNAME3D( M ) = EMSPC( M )
         UNITS3D( M ) = UNITSMENU( UNITTYPE )
         VDESC3D( M ) = 'biogenic emissions of the indicated species'
         VTYPE3D( M ) = M3REAL
      ENDDO
       

      FDESC3D = ' '   ! array

      FDESC3D( 1 ) = 'Gridded biogenic emissions from SMOKE-BEIS3'
      FDESC3D( 2 ) = '/FROM/ '    // PROGNAME
      FDESC3D( 3 ) = '/VERSION/ ' // VERCHAR( CVSW )
      FDESC3D( 4 ) = '/TZONE/ '   // CTZONE
      FDESC3D( 5 ) = '/LANDUSE/ ' // LUSE
      FDESC3D( 6 ) = '/MET SCENARIO/ ' // METSCEN
      FDESC3D( 7 ) = '/CLOUD SCHEME/ ' // CLOUDSHM

C-----------------------------------------------------------------------
C.... Open first output file (moles/hour)
C-----------------------------------------------------------------------

      ENAME = PROMPTMFILE(
     &          'Enter name for B3GTS output file - moles',
     &          FSUNKN3, 'B3GTS_L', PROGNAME )

      DO M = 1, MSPCS
         UNITS3D( M ) = 'tons/hr'
      ENDDO

C-----------------------------------------------------------------------
C.... Open second output file (tons/hour)
C-----------------------------------------------------------------------

!AQF       SNAME = PROMPTMFILE(
!AQF      &          'Enter name for B3GTS output file - mass',
!AQF      &           FSUNKN3, 'B3GTS_S', PROGNAME )

C.... Build name table for variables in normalized emissions file

      ALLOCATE( AVGEMIS( NCOLS, NROWS, NSEF-1, NSEASONS ), STAT=IOS )
      CALL CHECKMEM( IOS, 'AVGEMIS', PROGNAME )

      ALLOCATE( NOEMIS( NCOLS, NROWS, NNO ), STAT=IOS )
      CALL CHECKMEM( IOS, 'AVGNO', PROGNAME )

      ALLOCATE( AVGLAI( NCOLS, NROWS, NLAI, NSEASONS ), STAT=IOS )
      CALL CHECKMEM( IOS, 'AVGLAI', PROGNAME )

      AVGEMIS = 0.0 ! array
      NOEMIS  = 0.0 ! array
      AVGLAI  = 0.0 ! array

      ALLOCATE( LAT ( NCOLS, NROWS ), STAT=IOS )
      CALL CHECKMEM( IOS, 'LAT', PROGNAME )

      ALLOCATE( LON ( NCOLS, NROWS ), STAT=IOS )
      CALL CHECKMEM( IOS, 'LON', PROGNAME )

      ALLOCATE( COSZEN ( NCOLS, NROWS ), STAT=IOS )
      CALL CHECKMEM( IOS, 'COSZEN', PROGNAME )
        
      LAT( 1:ncols, 1:nrows) = glat_c( 1:NCOLS, 1:NROWS)
      LON( 1:ncols, 1:nrows) = glon_c( 1:NCOLS, 1:NROWS)

C-----------------------------------------------------------------------
C.... Loops reading the various categories of normalized emissions
C-----------------------------------------------------------------------

      DO M = 1, NSEASONS
         DO B = 1, NSEF

            BTMP = BIOTYPES( B )
            IF ( BTMP( 1: TRIMLEN( BTMP ) ) .NE. 'NO' ) THEN
               VTMP = 'AVG_' // BTMP( 1: TRIMLEN( BTMP )) // SEASON( M )

               IF ( .NOT. READ3( NNAME, VTMP, 1, 0, 0,
     &                                  AVGEMIS( 1, 1, B, M ) ) ) THEN
                  MESG = 'Could not read "' //
     &                           VTMP( 1 : TRIMLEN( VTMP ) ) //
     &                                    '" from file "' //
     &                           NNAME( 1 : TRIMLEN( NNAME ) ) // '"'
                  CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
               ENDIF
            ENDIF

         ENDDO

         DO N = 1, NLAI

            BTMP = LAITYPES( N )
            VTMP = 'LAI_' // BTMP( 1: TRIMLEN( BTMP )) // SEASON( M )

            IF ( .NOT. READ3( NNAME,VTMP,1,0,0, AVGLAI(1,1,N,M) ) ) THEN
               MESG = 'Could not read "' //
     &                           VTMP( 1 : TRIMLEN( VTMP ) ) //
     &                                 '" from file "' //
     &                           NNAME( 1 : TRIMLEN( NNAME ) ) // '"'
               CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
            ENDIF

         ENDDO
      ENDDO              ! seasons

      BTMP = 'NO'

      VTMP = 'AVG_' // BTMP( 1: TRIMLEN( BTMP )) // 'AG_GROW'
      IF ( .NOT. READ3( NNAME, VTMP, 1, 0, 0, NOEMIS( 1, 1, 1 ) ) ) THEN
         MESG = 'Could not read "' // 
     &                     VTMP( 1 : TRIMLEN( VTMP ) ) //
     &                           '" from file "' //
     &                     NNAME( 1 : TRIMLEN( NNAME ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

      VTMP = 'AVG_' // BTMP( 1: TRIMLEN( BTMP )) // 'AG_NONGROW'
      IF ( .NOT. READ3( NNAME, VTMP, 1, 0, 0, NOEMIS( 1, 1, 2 ) ) ) THEN
         MESG = 'Could not read "' // 
     &                     VTMP( 1 : TRIMLEN( VTMP ) ) //
     &                           '" from file "' //
     &                     NNAME( 1:TRIMLEN( NNAME ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

      VTMP = 'AVG_' // BTMP( 1: TRIMLEN( BTMP )) // 'NONAG'
      IF ( .NOT. READ3( NNAME, VTMP, 1, 0, 0, NOEMIS( 1, 1, 3 ) ) ) THEN
         MESG = 'Could not read "' // 
     &                     VTMP( 1 : TRIMLEN( VTMP ) ) //
     &                           '" from file "' //
     &                     NNAME( 1:TRIMLEN( NNAME ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

C....  end loop to read normalized emissions ............... 


C-----------------------------------------------------------------------
C.......   Allocate memory for met and emissions 
C-----------------------------------------------------------------------

      ALLOCATE( TASFC (NCOLS, NROWS), STAT=IOS )  ! remove if 10-m temp
      CALL CHECKMEM( IOS, 'TASFC', PROGNAME )

      ALLOCATE( RN (NCOLS, NROWS), STAT=IOS )             !remove
      CALL CHECKMEM( IOS, 'RN', PROGNAME )

      ALLOCATE( RC (NCOLS, NROWS), STAT=IOS )             !remove
      CALL CHECKMEM( IOS, 'RC', PROGNAME )

      ALLOCATE( rainfall (NCOLS,NROWS,RHOURS),STAT=IOS)   !keep
      CALL CHECKMEM( IOS, 'rainfall', PROGNAME )

      ALLOCATE( BUFFER (NCOLS, NROWS), STAT=IOS)
      CALL CHECKMEM( IOS, 'buffer', PROGNAME)

      ALLOCATE( PTYPE (NCOLS, NROWS), STAT=IOS )           !keep
      CALL CHECKMEM( IOS, 'PTYPE', PROGNAME )

      ALLOCATE( PULSEDATE (NCOLS, NROWS), STAT=IOS )       !keep
      CALL CHECKMEM( IOS, 'PULSEDATE', PROGNAME )

      ALLOCATE( PULSETIME (NCOLS, NROWS), STAT=IOS )       !keep
      CALL CHECKMEM( IOS, 'PULSETIME', PROGNAME )

C.... end added by GAP 12/01

      ALLOCATE( TSOLAR( NCOLS, NROWS ), STAT=IOS ) !remove if same as solar radiation
      CALL CHECKMEM( IOS, 'TSOLAR', PROGNAME )

      ALLOCATE( EMPOL( NCOLS, NROWS, NSEF ), STAT=IOS )   !keep
      CALL CHECKMEM( IOS, 'EMPOL', PROGNAME )

      ALLOCATE( PRES( NCOLS, NROWS ), STAT=IOS )        !remove
      CALL CHECKMEM( IOS, 'PRES', PROGNAME )

      ALLOCATE( EMISL( NCOLS, NROWS, MSPCS ), STAT=IOS )  !keep
      CALL CHECKMEM( IOS, 'EMISL', PROGNAME )

      ALLOCATE( EMISS( NCOLS, NROWS, MSPCS ), STAT=IOS ) !keep
      CALL CHECKMEM( IOS, 'EMISS', PROGNAME )

      ALLOCATE( SEMIS( NCOLS, NROWS, NSEF-1 ), STAT=IOS )  !keep
      CALL CHECKMEM( IOS, 'SEMIS', PROGNAME )

      ALLOCATE( NONAGNO( NCOLS, NROWS), STAT=IOS )   !keep
      CALL CHECKMEM( IOS, 'NONAGNO', PROGNAME )

      ALLOCATE( NGROWAGNO( NCOLS, NROWS), STAT=IOS ) !keep
      CALL CHECKMEM( IOS, 'NGROWAGNO', PROGNAME )

      ALLOCATE( GROWAGNO( NCOLS, NROWS), STAT=IOS )  !keep
      CALL CHECKMEM( IOS, 'GROWAGNO', PROGNAME )

      ALLOCATE( SLAI( NCOLS, NROWS, NLAI ), STAT=IOS ) !keep
      CALL CHECKMEM( IOS, 'SLAI', PROGNAME )

C####################################################
C.... begin added by GAP 12/01
C####################################################

      MESG = 'initial run?'
      INITIAL_RUN = ENVYN('INITIAL_RUN',MESG,.false.,IOS)

C-----------------------------------------------------------------------
C   *** if initial run then initialize some variables
C       otherwise get them from file
C-----------------------------------------------------------------------

      if ( initial_run ) then

         pulsedate(1:NCOLS,1:NROWS) = 0
         pulsetime(1:NCOLS,1:NROWS) = 0
         ptype    (1:NCOLS,1:NROWS) = 0

      else

         SOILINP = PROMPTMFILE('Enter name for NO EMISSIONS SAVE  file',
     &                         FSREAD3, 'SOILINP', PROGNAME )
         IF ( .NOT. DESC3( SOILINP ) ) THEN
            MESG = 'Could not get description of file "' //
     &                        SOILINP( 1 : TRIMLEN( SOILINP ) ) // '"'
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
         ENDIF

         NDATE = SDATE3D
         NTIME = STIME3D

         if ( NDATE .ne. JDATE ) then
            write (*,*) 'ERROR: cannot use SOILINP file ',
     &                  'invalid date found: ',NDATE,' expecting ',JDATE
            write (*,*) JDATE, JTIME
            write (*,*) NDATE, NTIME
            stop
         endif

         if ( NTIME .ne. JTIME ) then
            write (*,*) 'ERROR: cannot use SOILINP file ',
     &                  'invalid time found: ',NTIME,' expecting ',JTIME
            stop
         endif


         IF ( .NOT. READ3 (SOILINP,'PTYPE',1,JDATE,JTIME,BUFFER) ) THEN
            CALL M3EXIT( PROGNAME, JDATE, JTIME, 
     &                         'Error reading BEIS3 SOILINP file' , 2 )
         ENDIF
         PTYPE = INT(BUFFER)
		
         IF (.NOT. READ3(SOILINP,'PULSEDATE',1,JDATE,JTIME,BUFFER)) THEN
            CALL M3EXIT( PROGNAME, JDATE, JTIME, 
     &                         'Error reading BEIS3 SOILINP file' , 2 )
         ENDIF  
         PULSEDATE = INT(BUFFER)
		
         IF (.NOT. READ3(SOILINP,'PULSETIME',1,JDATE,JTIME,BUFFER)) THEN
            CALL M3EXIT( PROGNAME, JDATE, JTIME, 
     &                         'Error reading BEIS3 SOILINP file' , 2 )
         ENDIF  
         PULSETIME = INT(BUFFER)


         do i_loop = 1,RHOURS

            if (i_loop .le. 9) then
               write (ichar,'(I1)') i_loop        
               vname = 'RAINFALL'//'0'//ichar
            else
               write (i2char,'(I2)') i_loop
               vname = 'RAINFALL'//i2char
            endif

            IF ( .NOT. READ3( SOILINP, vname, 1,
     &                       JDATE, JTIME, rainfall(1,1,i_loop) ) ) THEN
               CALL M3EXIT( PROGNAME, JDATE, JTIME, 
     &                         'Error reading BEIS3 SOILINP file' , 2 )
            ENDIF 
         enddo 


         IF ( .NOT. CLOSE3(SOILINP)) THEN
            CALL M3EXIT( PROGNAME, JDATE, JTIME, 
     &                         'Error closing BEIS3 SOILINP file' , 2 )
         ENDIF

      endif        ! initial_run

C####################################################
C ..... end added by GAP 12/01
C####################################################


C-----------------------------------------------------------------------
C.... Set up gridded met file(s) dates and times for specific time zone
C-----------------------------------------------------------------------

!AQF    IF ( M3NAME .NE. ' ' ) THEN
!AQF       CALL PREBMET( M3NAME, M2NAME, SAMEFILE, TZONE, TSTEP3D,
!AQF  &               JDATE, JTIME, NSTEPS, MDATE, MTIME, RDATE, RTIME )
!AQF    ENDIF

!AQF    IF ( M2NAME .EQ. M3NAME ) THEN

      MDATE = JDATE
      MTIME = JTIME

      RDATE = JDATE  
      RTIME = JTIME
  
!AQF    ENDIF 

C-----------------------------------------------------------------------
C.... Initialize normalized emissons to be used 
C-----------------------------------------------------------------------

      IF ( ASSUME_SUMMER ) THEN
         SEMIS = AVGEMIS( 1:NCOLS, 1:NROWS, 1:NSEF-1, NSUMMER )
         SLAI  = AVGLAI ( 1:NCOLS, 1:NROWS, 1:NLAI, NSUMMER )
      ELSE
         SEMIS = AVGEMIS( 1:NCOLS, 1:NROWS, 1:NSEF-1, NWINTER )
         SLAI  = AVGLAI ( 1:NCOLS, 1:NROWS, 1:NLAI, NWINTER )
      ENDIF

      GROWAGNO  = NOEMIS( 1:NCOLS, 1:NROWS, 1 )
      NGROWAGNO = NOEMIS( 1:NCOLS, 1:NROWS, 2 )
      NONAGNO   = NOEMIS( 1:NCOLS, 1:NROWS, 3 )

      LDATE = 0     

      HR = 1     ! initialize for first hour

      DEALLOCATE (BUFFER)
      CALL M3MSG2('pre_beis ends')


C************  FORMAT  STATEMENTS   **************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( I2.2 )
94010   FORMAT( 10( A, :, I8, :, 1X ) )

94030   FORMAT( 8X, 'at time ', A8 )

      END SUBROUTINE pre_beis 

