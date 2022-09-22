      PROGRAM COMBINE
      
C**********************************************************************
C
C  DISCLAIMER:
C    THIS SOFTWARE IS NOT PART OF THE OFFICIAL MODELS-3 RELEASE.  IT
C    HAS NOT UNDERGONE THOROUGH CODE REVIEW AND THEREFORE IS NOT 
C    INTENDED FOR OFFICIAL RELEASE.  THIS SOFTWARE IS PROVIDED "AS IS,"
C    WITHOUT A WARRANTY OF ANY KIND.  BY USE OF THE SOFTWARE, YOU
C    ACKNOWLEDGE THAT YOU HAVE READ AND UNDERSTAND THIS DISCLAIMER.
C
C**********************************************************************
C
C  This program combines fields from a set of IOAPI input files
C  to 1 output file.  The file assigned to Environmental variable
C  SPECIES_DEF  defines the new species variables and how they
C  are constructed.
C
C  Environmental variables used
C      SPECIES_DEF --> Species definition file defining the new
C                      variables of the output file
C      INFILE1     --> input file number 1, (max of 9)
C      OUTFILE     --> IOAPI output file name
C      HIST_FILE   --> ASCII file containing file description
C
C   record type descriptions in SPECIES_DEF file
C     / records are comment lines
C     ! records are comment line which are copied to file_desc attribute
C     # records can be used to define parameters
C                       1         2         3
C              12345678901234567890123456789012
C              #start   YYYYDDD  HHMMSS
C              #end     YYYYDDD  HHMMSS
C              #layer      KLAY     (default is all layers)
C              #log        lcol    lrow    llay
C
C     all other are variable definition records 
C        field 1 (2-17)  new variable name  
C              2 (19-28) variable units
C              3 (32-39) multiply factor
C              4 (41-41) input file number
C              5 (43-58) input variable
C              6 (60-129) output variable description
C
C***********************************************************************

      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'FDESC3.EXT'
      INCLUDE 'IODECL3.EXT' 
      INCLUDE 'STATE3.EXT' 

C...define variables to save description

      INTEGER NROWS
      INTEGER NCOLS
      INTEGER NLAYS
      INTEGER SDATE
      INTEGER STIME
      INTEGER TSTEP
      INTEGER NRECORDS
      INTEGER RUNLEN
      INTEGER EDATE
      INTEGER ETIME
      INTEGER KLAYER
      INTEGER IOST
      integer eday, sday, etim, stim
      integer i,ierr, isize, jerr, k, kfile, nfiles
      integer lcol,lrow,llay,ioffset
      integer nfvars( 120 )
      integer ntv, ntvars
      real xcell, xorig, ycell, yorig
      real vfact( 120, 20 )

C...define external functions

      INTEGER TIME2SEC
      INTEGER SEC2TIME

C...define arrays to store variable definitions

      CHARACTER*130 RECORD
      CHARACTER*7 FNAME
      CHARACTER*7 FLDNAME
      CHARACTER*60 SPECDEF
      CHARACTER*60 HISFILE
      CHARACTER*10 VUNITS( 120 )
      CHARACTER*70 VDESC ( 120 )
      CHARACTER*16 TVARS ( 120 )
      CHARACTER*1  OPRTR( 120, 20 )
      CHARACTER*16 FVARS( 120, 20 )

      DIMENSION KFILE( 120, 20 )

C   dimension Buffer (rows, cols, lays)

      REAL, Allocatable :: FBUFFER(:)
      REAL, Allocatable :: TBUFFER(:)

      LOGICAL KSWIT


C...check environment variables for input file names
      
      CALL NAMEVAL( 'SPECIES_DEF', SPECDEF )
      CALL NAMEVAL( 'HIST_FILE', HISFILE )

C...open variable definition file and fill arrays
      
      OPEN( UNIT = 20,
     &      FILE = SPECDEF,
     &      STATUS = 'OLD',
     &      ERR = 985 )
      
      NTVARS = 0
      NFILES = 0
      SDAY = -1
      STIM = -1
      EDAY = -1
      ETIM = -1
      KLAYER = -1   !default is all layers
      LCOL=-1
      LROW=-1
      LLAY=-1
      IOFFSET=-1

20    CONTINUE

      READ( 20, '(A)', END=50 ) RECORD
      IF ( RECORD .EQ. ' ' ) GO TO 20
      IF ( RECORD( 1:1 ) .EQ. '/' ) GO TO 20  ! comment line
      IF ( RECORD( 1:1 ) .EQ. '!' ) GO TO 20  ! file description line

C...Check for parameter values

      IF ( RECORD( 1:1 ) .EQ. '#' ) THEN
        FLDNAME = RECORD(2:8)
        CALL UCASE(FLDNAME,7)
        IF ( FLDNAME( 1:5 ) .EQ. 'START' )
     &    READ( RECORD, '(8X,2I8)', ERR=981 ) SDATE, STIME
        IF ( FLDNAME( 1:3 ) .EQ. 'END' )
     &    READ( RECORD, '(8X,2I8)', ERR=981 ) EDATE, ETIME
        IF ( FLDNAME( 1:5 ) .EQ. 'LAYER' )
     &    READ( RECORD, '(8X,I8)', ERR=981 ) KLAYER
        IF ( FLDNAME( 1:3 ) .EQ. 'LOG' )
     &    READ( RECORD, '(8X,3I8)', ERR=981 ) lcol,lrow,llay
        GO TO 20
      END IF

C...check for to-var line

      IF ( RECORD( 2:17 ) .NE. ' ' ) THEN
        IF ( NTVARS .EQ. 120 ) GO TO 986
        NTVARS = NTVARS + 1
        NFVARS( NTVARS ) = 1
        TVARS ( NTVARS ) = RECORD(2:17)
        VUNITS( NTVARS ) = RECORD( 19:28 )
        VDESC ( NTVARS ) = RECORD( 60:129 )
        READ( RECORD, '(29X,A1,1X,F8.0,1X,I1)', ERR=987 )
     &    OPRTR( NTVARS, 1 ), VFACT( NTVARS, 1 ), KFILE( NTVARS, 1 )  
        NFILES = MAX( NFILES, KFILE( NTVARS, 1 ) )
        FVARS( NTVARS, 1 ) = RECORD( 43:58 )
        GO TO 20
      END IF

C...if no to var is defined, then go back to 20
      
      IF ( NTVARS .EQ. 0 ) GO TO 20

C...add from var to to-var definition

      NFVARS( NTVARS ) = NFVARS( NTVARS ) + 1
      IF ( NFVARS( NTVARS ) .GT. 20 ) GO TO 988
      READ( RECORD, '(29X,A1,1X,F8.0,1X,I1)', ERR=987 )
     &  OPRTR( NTVARS, NFVARS( NTVARS ) ),
     &  VFACT( NTVARS, NFVARS( NTVARS ) ),
     &  KFILE( NTVARS, NFVARS( NTVARS ) )  
      IF ( KFILE( NTVARS, NFVARS( NTVARS ) ) .GT. NFILES )
     &  NFILES = KFILE( NTVARS, NFVARS( NTVARS ) )
      FVARS( NTVARS, NFVARS( NTVARS ) ) = RECORD( 43:58 )
      GO TO 20

50    CONTINUE       


C...start IOAPI

      LOGDEV = INIT3()
      WRITE( *, '('' API Started'')' )

C...open input file 1

      FNAME='INFILE1'
      IF ( .NOT. OPEN3( FNAME, 1, 'COMBINE' ) ) THEN
        WRITE( *, '('' Error opening Input Data file:'',A)' ) FNAME
        GO TO 999
      END IF

C...fill Description Common Block

      IF ( .NOT.  DESC3( FNAME ) ) THEN
        WRITE( *, '(''**Error** While running DESC3 on '',A)' ) FNAME
        GO TO 999
      END IF

C...save values of file description

      NRECORDS = MXREC3D
      TSTEP = TSTEP3D
      NROWS = NROWS3D
      NCOLS = NCOLS3D
      NLAYS = NLAYS3D
      XCELL = XCELL3D
      YCELL = YCELL3D
      XORIG = XORIG3D
      YORIG = YORIG3D

C...compute time of first and last record if needed

      IF ( SDATE .LT. 0 ) THEN
        SDATE = SDATE3D
        STIME = STIME3D
      END IF

      TSTEP = TSTEP3D

      IF ( EDATE .LT. 0 ) THEN
        RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
        EDATE  = SDATE3D
        ETIME  = STIME3D
        CALL NEXTIME ( EDATE, ETIME, RUNLEN )  
      END IF

C...start loop to open additional files

      DO I = 2, NFILES
        FNAME( 7:7 ) = CHAR( 48 + I )

C...open next Input file 

        IF ( .NOT. OPEN3( FNAME, 1, 'COMBINE' ) ) THEN
          WRITE( *, '('' Error opening Input Data file:'',A)' ) FNAME
          GO TO 999
        END IF

C...fill Description Common Block

        IF ( .NOT. DESC3( FNAME ) ) THEN
          WRITE( *, '(''**Error** While running DESC3 on '',A)' ) FNAME
          GO TO 999
        END IF

C...compare grid values with input file

        IF ( ( NROWS .NE. NROWS3D ) .OR.
     &       ( NCOLS .NE. NCOLS3D ) .OR.
     &       ( TSTEP .NE. TSTEP3D ) .OR.
     &       ( XCELL .NE. XCELL3D ) .OR.
     &       ( YCELL .NE. YCELL3D ) .OR.
     &       ( XORIG .NE. XORIG3D ) .OR.
     &       ( YORIG .NE. YORIG3D ) ) GO TO 998

      END DO

C...setup to copy and build records
C...  build description of new variables

      NVARS3D = NTVARS
      if( KLAYER .gt. 0)  NLAYS3D = 1

      DO I = 1, NTVARS
        VNAME3D( I ) = TVARS( I )
        UNITS3D( I ) = VUNITS( I )
        VDESC3D( I ) = VDESC( I )
        VTYPE3D( I ) = M3REAL
      END DO

C...copy species definition file into FDESC3D character array for file
C...  description only records starting with '!' will be copied to
C...  description

      REWIND( UNIT = 20 ) 

C...blank out FDESC3D character array

      DO I = 1, MXDESC3
        FDESC3D( I ) = ' '
        SCNDSC ( I ) = ' '
      END DO

      DO I = 1, MXDESC3
        READ( 20, '(A)', ERR=86, END=86 ) RECORD
        IF ( RECORD( 1:1 ) .EQ. '!' ) FDESC3D( I ) = RECORD( 2: )
      END DO

86    CLOSE( UNIT = 20 )

C...open history file and copy to history attribute

      OPEN( UNIT = 20,
     &      FILE = HISFILE,
     &      STATUS = 'OLD',
     &      ERR = 90 )

      DO I = 1, MXDESC3
        READ( 20, '(A)', END=89 ) SCNDSC( I )
      END DO

89    CLOSE( UNIT = 20 )

C...try to create new file, if error open file as old 

90    SDATE3D = SDATE
      STIME3D = STIME

      IF ( .NOT. OPEN3( 'OUTFILE', 3, 'COMBINE' ) ) THEN
        GO TO 100
      END IF

      PRINT *, SDATE3D, STIME3D
      GO TO 200

C...try to open existing file, if error abort program

100   IF ( .NOT. OPEN3( 'OUTFILE', 2, 'COMBINE' ) ) THEN
        WRITE( *, '('' Error while openning OUTFILE'')' )
        GO TO 999
      END IF

C...fill Description Common Block

      IF ( .NOT.  DESC3( 'OUTFILE' ) ) THEN
        WRITE( *, '(''**ERROR** WHILE RUNNING DESC3 ON OUTFILE'')' )
        GO TO 999
      END IF

C...compare grid values with input file

      IF ( NROWS .NE. NROWS3D ) GO TO 998
      IF ( NCOLS .NE. NCOLS3D ) GO TO 998
      IF ( TSTEP .NE. TSTEP3D ) GO TO 998
      IF ( XCELL .NE. XCELL3D ) GO TO 998
      IF ( YCELL .NE. YCELL3D ) GO TO 998
      IF ( XORIG .NE. XORIG3D ) GO TO 998
      IF ( YORIG .NE. YORIG3D ) GO TO 998

C...compute time of next record to append

      WRITE( *,
     &'('' Using next Time steps computed from existing Output File'')')
      RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
      SDATE = SDATE3D
      STIME = STIME3D

      CALL NEXTIME( SDATE, STIME, RUNLEN ) 

C...Setup to copy and build records
C...  Allocate space for buffer areas

200   ISIZE = NCOLS3D * NROWS3D * NLAYS3D
      Allocate( fBuffer( ISIZE ), tBuffer( ISIZE ), STAT=ierr)
      if(ierr.ne.0) then
       write(*,'('' **Error** Cannot allocate space for Buffers'')')
       go to 999
       endif

C  compute offset value for log
      IF((lcol.gt.0).and.(lcol.le.ncols3d) .and.
     &   (lrow.gt.0).and.(lrow.le.nrows3d) .and.
     &   (llay.gt.0).and.(llay.le.nlays3d))
     & ioffset=(llay-1)*(nrows3d*ncols3d) + (lrow-1)*ncols3d + lcol

C...Start loops to read and write each timestep 

      DO K = 1, NRECORDS
        WRITE( *, '('' Copying Variables at time:'',i7,'':'',i6)' )
     &    SDATE, STIME

        DO NTV = 1, NTVARS

C...zero out to-buffer

          CALL ZARRAY ( TBUFFER, ISIZE )

C...add from variables to to-buffer

          DO I = 1, NFVARS( NTV )
            FNAME( 7:7 ) = CHAR( 48 + KFILE( NTV, I ) ) 

            IF ( .NOT. READ3( FNAME, FVARS( NTV, I ), KLAYER, SDATE, 
     &                        STIME, FBUFFER ) ) GO TO 500

            CALL CPARRAY ( FBUFFER, TBUFFER, ISIZE,
     &                     OPRTR( NTV, I ), VFACT( NTV, I ) )

          END DO


C...write to-buffer to output file

          IF( .NOT. WRITE3( 'OUTFILE', TVARS( NTV ), SDATE, STIME,
     &                      TBUFFER ) ) THEN
            WRITE( *, '('' **Error** Cannot write to OUTFILE at'',2i8)')
     &             SDATE, STIME
            GO TO 999
          END IF


C... Write to log if log is on
          IF( ioffset .gt. 0 )
     &      write(*,'(5x,a,''['',i2,'','',i2,'','',i2,''] = '',g15.5)')
     &      Trim(TVARS(NTV)),lcol,lrow,llay,TBUFFER( ioffset )


        END DO

C...check for end date and time

        IF ( ( SDATE .GE. EDATE ) .AND. ( STIME .GE. ETIME ) ) GO TO 500
        CALL NEXTIME ( SDATE, STIME, TSTEP3D )
      END DO


500   CONTINUE

      WRITE( *, '('' Finished Copying from INFILE at'',2i8)')
     &      SDATE, STIME

C...close files and go to 999 to exit

550   CONTINUE

      DO I = 1, NFILES
        FNAME( 7:7 ) = CHAR( 48 + I )
        IF ( .NOT. CLOSE3( FNAME ) )
     &    WRITE( *, '(''**ERROR** CLOSING FILE '',A)') FNAME
      END DO

      IF ( .NOT. CLOSE3( 'OUTFILE' ) )
     &  WRITE( *, '(''**ERROR** CLOSING OUTFILE'')' )
       GO TO 999

C...error branches

981   WRITE( *, '('' Data Error on line:'',a)' ) RECORD
      STOP

985   WRITE( *, '('' Cannot open species definition file:'',a)' )
     &  SPECDEF
      STOP

986   WRITE( *, '('' Error, the number of Species in output > 120'')' )
      STOP

987   WRITE( *, '('' Error, Reading Species definition file'')' )
      STOP

988   WRITE( *, '('' Error, Number of species to combine > 20'')' )
      STOP

998   WRITE( *, '('' **Error** Grid values do not Match'')' )
      WRITE( *, '('' nrows ='',2i12)' ) NROWS, NROWS3D
      WRITE( *, '('' ncols ='',2i12)' ) NCOLS, NCOLS3D
      WRITE( *, '('' tstep ='',2i12)' ) TSTEP, TSTEP3D
      WRITE( *, '('' xcell ='',2f12.0)' ) XCELL, XCELL3D
      WRITE( *, '('' ycell ='',2f12.0)' ) YCELL, YCELL3D
      WRITE( *, '('' xorig ='',2f12.0)' ) XORIG, XORIG3D
      WRITE( *, '('' yorig ='',2i12.0)' ) YORIG, YORIG3D

      GO TO 550       

C... shut down netcdf
  
999   CONTINUE
      KSWIT = SHUT3()
      WRITE(*,'('' API Shut down'')')
      STOP
      END




C***********************************************************************
C***Utilities Routines**************************************************
C***********************************************************************

      SUBROUTINE ZARRAY ( BUFFER, KSIZE )

C***********************************************************************
C  Routine to set all elements in array to zero
C***********************************************************************

      IMPLICIT NONE
      
      INTEGER KSIZE
      REAL    BUFFER(*)
      
      INTEGER I
             
      DO I = 1, KSIZE
        BUFFER( I ) = 0.0
      END DO

      RETURN
      END


      SUBROUTINE CPARRAY ( BUFFER1, BUFFER2, KSIZE, OPTR, FACTR )

C***********************************************************************
C  Routine to add all elements of one array to another
C***********************************************************************

      IMPLICIT NONE
      
      CHARACTER*1 OPTR

      INTEGER I, KSIZE

      REAL FACTR
      REAL BUFFER1( * )
      REAL BUFFER2( * )
       
      DO I = 1, KSIZE

        IF ( OPTR .EQ. '=' ) THEN
          BUFFER2( I ) = FACTR * BUFFER1( I )
        ELSE IF ( OPTR .EQ. '+' ) THEN
          BUFFER2( I ) = BUFFER2( I ) + FACTR * BUFFER1( I )
        ELSE IF ( OPTR .EQ. '-' ) THEN
          BUFFER2( I ) = BUFFER2( I ) - FACTR * BUFFER1( I )
        ELSE IF ( OPTR .EQ. '*' ) THEN
          BUFFER2( I ) = BUFFER2( I ) * FACTR * BUFFER1( I )
        ELSE IF ( OPTR .EQ. '/' ) THEN
          BUFFER2( I ) = BUFFER2( I )
     &                 / MAX( FACTR * BUFFER1( I ), 1.0E-30 )
        END IF

      END DO

      RETURN
      END


      SUBROUTINE UCASE ( STR, NCHR )

C***********************************************************************
C  Routine to change character string to upper characters
C***********************************************************************

      IMPLICIT NONE

      CHARACTER STR*( * )
      INTEGER NCHR
      INTEGER I
      INTEGER K

      DO I = 1, NCHR
        K = ICHAR(STR(I:I))
        IF ( ( K .GE. 97 ) .AND. ( K .LE. 122 ) )
     &    STR( I:I ) = CHAR( K - 32 )
      END DO

      RETURN
      END
