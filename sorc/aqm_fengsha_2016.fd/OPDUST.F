C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPDUST ( JDATE, JTIME, TSTEP )

C***********************************************************************
C   OPENDUST is a part of the wind-blown dust module FENGSHA that      *
C     simulates dust emissions from dessert and agricultural land.     *
C   OPENDUST opens an IOAPI file to store dust emissoins and other     *
C     related information (transportable fraction, land use etc).      *
C----------------------------------------------------------------------*   
C   History:							       *
C     
C
C***********************************************************************

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_IOPARMS             ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC             ! file header data structuer
      INCLUDE SUBST_IODECL              ! I/O definitions and declarations
C     INCLUDE SUBST_DUSTCT              ! Constant for dust calculation
      INCLUDE SUBST_FILES_ID            ! file name parameters
      INCLUDE SUBST_CONST               ! constants


C...Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      TSTEP      ! output time step
      INTEGER	   IOS	      ! allocate stat

C...Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPENDUST'
      CHARACTER( 16 ) :: FNAME = 'CTM_DUST'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      LOGDEV
      INTEGER      INDEX          ! loop induction variables

C-----------------------------------------------------------------------
C ... Tong: test opening
      INTEGER	MYPE 
      PARAMETER ( MYPE =0 )   !For test 
	
!********** These species are to be changed **************************
      REAL, ALLOCATABLE,SAVE :: SPEC1 (:,:,:)           ! Inputfile
      REAL, ALLOCATABLE,SAVE :: SPEC2 (:,:,:)           ! Outputfile

      INTEGER, ALLOCATABLE,SAVE ::        VTYPE( : ) ! variable type:  M3(INT|REAL|DBLE)
      CHARACTER*16, ALLOCATABLE,SAVE ::   VNAME( : ) ! variable names (length MXDLEN3=80)
      CHARACTER*16, ALLOCATABLE,SAVE ::   UNITS( : ) !   "   units or 'none' (MXDLEN3=80)
      CHARACTER*20, ALLOCATABLE,SAVE ::   VDESC( : ) !   "      descriptions (MXDLEN3=80)

!*** Variable for read index file containing the mapping relationship
      INTEGER           ALLOCSTAT, ALLOCSTAT1, ALLOCSTAT2, ALLOCSTAT3

C************* Variales for domain configuration *********************
        REAL P_ALP, P_BET, P_GAM, XCENT, YCENT, VGTOP
        INTEGER NLAYS, XORIG, YORIG, XCELL, YCELL, MXREC
        INTEGER FTYPE, NTHIK, NVARS, VGTYP, GDTYP 
        CHARACTER GDNAM*20,DFILE*80, UPNAM*20
C       INTEGER SDATE, STIME, TSTEP
        INTEGER NCOLS, NROWS

C************* Get domain parameters *******************************
        CALL GETENV('domainfile',DFILE)
        OPEN(11,FILE=DFILE,STATUS='OLD')
C        READ(11,*) MXREC
        READ(11,*) FTYPE
C        READ(11,*) SDATE
C        READ(11,*) STIME
C        READ(11,*) TSTEP
        READ(11,*) NTHIK
        READ(11,*) NCOLS
        READ(11,*) NROWS
        READ(11,*) NLAYS
        READ(11,*) NVARS
        READ(11,*) GDTYP
        READ(11,*) P_ALP
        READ(11,*) P_BET
        READ(11,*) P_GAM
        READ(11,*) XCENT
        READ(11,*) YCENT
        READ(11,*) XORIG
        READ(11,*) YORIG
        READ(11,*) XCELL
        READ(11,*) YCELL
        READ(11,*) VGTYP
        READ(11,*) VGTOP
        READ(11,*) GDNAM
        READ(11,*) UPNAM
C        ALLOCATE (VNAME(NVARS), STAT = ALLOCSTAT1 )
C        ALLOCATE (UNITS(NVARS), STAT = ALLOCSTAT1 )
C        ALLOCATE (VDESC(NVARS), STAT = ALLOCSTAT1 ) 
C        DO I = 1,NVARS
C          READ(11,*) VNAME(I),UNITS(I),VDESC(I)
C          WRITE(*,*) VNAME(I),UNITS(I),VDESC(I)
C        END DO
        CLOSE(11)

!***********************************************************************
!.......   First:  Initialize the I/O API:

        LOGDEV = INIT3()        !  initialization returns unit # for log
        PRINT *, "       the INIT3 number is ", LOGDEV

!************** Read domain definition fiel. ***************************
C       MXREC3D = MXREC
        FTYPE3D = FTYPE
        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP
        NTHIK3D = NTHIK
        NCOLS3D = NCOLS
        NROWS3D = NROWS
        NLAYS3D = NLAYS
        NVARS3D = NVARS
        GDTYP3D = GDTYP
        P_ALP3D = P_ALP
        P_BET3D = P_BET
        P_GAM3D = P_GAM
        XCENT3D = XCENT
        YCENT3D = YCENT
        XORIG3D = XORIG
        YORIG3D = YORIG
        XCELL3D = XCELL
        YCELL3D = YCELL
        VGTYP3D = VGTYP
        VGTOP3D = VGTOP
        GDNAM3D = GDNAM
        UPNAM3D = UPNAM
C        ALLOCATE (SPEC1(NCOLS, NROWS, NLAYS), STAT = ALLOCSTAT1 )

         LOGDEV = INIT3 ()

CTong: Get file description from an existing IOAPI file
C      IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
C         XMSG = 'Could not open GRID_CRO_2D file'
C         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
C         END IF
C      IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
C         XMSG = 'Could not get GRID_CRO_2D file description '
C         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
C         END IF

C Set output file characteristics based on COORD.EXT and open it
CTong         FTYPE3D = GRDDED3
C         SDATE3D = JDATE
C	 STIME3D = JTIME
C	 TSTEP3D = TSTEP
!         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

C         NVARS3D = 29
         NLAYS3D = 1
CTong         NCOLS3D = GL_NCOLS
CTong         NROWS3D = GL_NROWS

C	 IF (ALLOCATED(UNITS3D)) DEALLOCATE(UNITS3D)
C         IF (ALLOCATED(VNAME3D)) DEALLOCATE(VNMAE3D)
C         IF (ALLOCATED(VDESC3D)) DEALLOCATE(VDESC3D)
C	 ALLOCATE( UNITS3D( NVARS3D ), STAT=IOS )	
C         ALLOCATE( VNAME3D( NVARS3D ), STAT=IOS )
C         ALLOCATE( VDESC3D( NVARS3D ), STAT=IOS )

 
	 DO INDEX = 1, NVARS3D
            VTYPE3D( INDEX ) = M3REAL
            END DO
	    
         UNITS3D(1) ='ug/m2/s'
         UNITS3D(2) ='ug/m2/s'
         UNITS3D(3) ='ug/m2/s'
         UNITS3D(4) ='ug/m2/s'
         UNITS3D(5) ='g/s'
         UNITS3D(6) ='g/s'
         UNITS3D(7) ='g/s'
         UNITS3D(8) ='g/s'
         UNITS3D(9) ='g/s'
         UNITS3D(10) ='%'
         UNITS3D(11) ='%'
         UNITS3D(12) ='%'
         UNITS3D(13) ='%'
         UNITS3D(14) ='%'
         UNITS3D(15) =' '
         UNITS3D(16) =' '
         UNITS3D(17) ='m/s'
         UNITS3D(18) ='m/s'
         UNITS3D(19) ='m/s'
         UNITS3D(20) ='m/s'
         UNITS3D(21) ='m/s'
         UNITS3D(22) =' '
         UNITS3D(23) =' '
         UNITS3D(24) =' '
         UNITS3D(25) =' '
         UNITS3D(26) =' '
         UNITS3D(27) =' '
         UNITS3D(28) =' '
         UNITS3D(29) =' '
	    
         VNAME3D(1) ='FSHLD'
         VNAME3D(2) ='FSHGR'
         VNAME3D(3) ='FSPBA'
         VNAME3D(4) ='FCROP'
         VNAME3D(5) ='QSHLD'
         VNAME3D(6) ='QSHGR'
         VNAME3D(7) ='QSPBA'
         VNAME3D(8) ='QCROP'
         VNAME3D(9) ='ETOTL'
         VNAME3D(10) ='LSHLD'
         VNAME3D(11) ='LSHGR'
         VNAME3D(12) ='LSPBA'
         VNAME3D(13) ='LCROP'
         VNAME3D(14) ='TLDST'
         VNAME3D(15) ='TFABV'
         VNAME3D(16) ='TFINF'
         VNAME3D(17) ='USTAR'
         VNAME3D(18) ='USHLD'
         VNAME3D(19) ='USHGR'
         VNAME3D(20) ='USPBA'
         VNAME3D(21) ='UCROP'
         VNAME3D(22) ='FMOIT'
         VNAME3D(23) ='STDEP'
         VNAME3D(24) ='KVTHZ'
         VNAME3D(25) ='WTMAX'
         VNAME3D(26) ='ULDBR'
         VNAME3D(27) ='ULDVG'
         VNAME3D(28) ='ULDUB'
         VNAME3D(29) ='ULDFR'

         VDESC3D(1) ='Dust emission from shrubland'
         VDESC3D(2) ='Dust emission from shrubgrass'
         VDESC3D(3) ='Dust emission from barrenland'
         VDESC3D(4) ='Dust emission from cropland'
         VDESC3D(5) ='Dust emission rate from shrubland'
         VDESC3D(6) ='Dust emission rate from shrubgrass'
         VDESC3D(7) ='Dust emission rate from barrenland'
         VDESC3D(8) ='Dust emission rate from cropland'
         VDESC3D(9) ='Total dust emission rate'
         VDESC3D(10)='Shrubland'
         VDESC3D(11)='Shrubgrass land'
         VDESC3D(12)='Sparse barren land'
         VDESC3D(13)='Barren cropland'
         VDESC3D(14)='Total dust source land'
         VDESC3D(15)='Transport factor above canopy'
         VDESC3D(16)='Transport factor inflow'
         VDESC3D(17)='friction velocity'
         VDESC3D(18)='U*t shrubland'
         VDESC3D(19)='U*t shrubgrass'
         VDESC3D(20)='U*t barrenland'
         VDESC3D(21)='U*t cropland'
         VDESC3D(22)='Moisture factor'
         VDESC3D(23)='sand2 dust potential'
         VDESC3D(24)='vertical 2 horizontal factor'
         VDESC3D(25)='max water content'
         VDESC3D(26)='water barren land'
         VDESC3D(27)='vege land'
         VDESC3D(28)='urban land'
         VDESC3D(29)='forest land'
	 
CTong: I have removed some global varibles here
CTong:         GDTYP3D = GDTYP_GD

CTong         DO INDEX = 1, NLAYS3D + 1
CTong
CTong
CTong         GDNAM3D = GRID_NAME
	 
         FDESC3D( 1 ) = 'dust intermedia results output'
         FDESC3D( 2 ) = 'From CMAQ model'
         DO INDEX = 3, MXDESC3
            FDESC3D( INDEX ) = ' '
            END DO
	    
C Open dust file new

      IF ( MYPE .EQ. 0 ) THEN   ! open new
         IF ( .NOT. OPEN3( FNAME, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open '// FNAME // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
         END IF

!C      CALL SUBST_BARRIER

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &  'Timestep written to', 'CTM_DUST',
     &  'for date and time', JDATE, JTIME
      WRITE( LOGDEV, '(  5X,  A, 1X, I8, ":", I6.6 )' )
     &  'from timestep on dust files for date and time',
     &   JDATE, JTIME

      IF ( MYPE .NE. 0 ) THEN   ! open read/write
         IF ( .NOT. OPEN3( FNAME, FSRDWR3, PNAME ) ) THEN
            XMSG = 'Could not open ' // FNAME // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      IF ( .NOT. DESC3 (FNAME) ) THEN
            CALL M3ERR( FNAME, JDATE, JTIME,
     &  'Could not read DESC of ' // FNAME // ' file', .TRUE. )
      ENDIF

      IF ( CLOSE3(FNAME)) THEN
            PRINT *, ' Closed program ', FNAME 
      ELSE
            PRINT *, "  ", FNAME
      END IF


      RETURN
      END
