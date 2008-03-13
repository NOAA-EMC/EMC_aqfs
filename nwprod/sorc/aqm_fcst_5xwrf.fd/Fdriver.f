
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
C $Header: /project/cmaq/rel/models/CCTM/src/vadv/vppm/zadvppm.F,v 1.1.1.1 2002/06/27 11:25:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      PROGRAM DRIVER

C-----------------------------------------------------------------------
C Function:

C Preconditions:

C Subroutines and functions called:

C Revision History:
C   Feb 03 - Jeff, Dave Wong: created
C   May 03 - Jeff, add write start step file
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE ACONC_DEFN
      USE DDEP_DEFN             ! inherits HGRD_DEFN
      USE WDEP_DEFN
      USE VIS_DEFN
      USE WVEL_DEFN             ! derived vertical velocity component
      USE MPIM
      USE SE_MODULES         ! stenex
!     USE SUBST_INIT_MODULE     ! stenex

      IMPLICIT NONE

!#include "f_hpm.h"

!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_SPC.EXT"      ! gas chemistry species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/AE_SPC.EXT"      ! aerosol species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/NR_SPC.EXT"      ! non-reactive species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/TR_SPC.EXT"      ! tracer species table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/GC_CONC.EXT"     ! gas chem conc file species and map table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/AE_CONC.EXT"     ! aerosol conc file species and map table
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/PARMS3.EXT"     ! I/O parameters definitions
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/IODECL3.EXT"      ! I/O definitions and declarations
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/FILES_CTM.EXT"    ! file name parameters
      INCLUDE 'TAG.EXT'

C Load the mechanism COMMON ... (next two INCLUDE files)
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/RXCM.EXT"      ! chemical mechamism reactions COMMON
      INCLUDE "/nwpara/sorc/aqm_fcst_5xwrf.fd/RXDT.EXT"      ! chemical mechamism reactions DATA

      INTEGER STDATE, STTIME, NSTEPS
      INTEGER :: JDATE = 0, JTIME = 0
      INTEGER :: TSTEP( 2 ) = (/ 0, 0 /)
      INTEGER ISTEP, ERROR
      INTEGER LOGDEV
      INTEGER FILE_ACCS
      INTEGER NUMPROCS, RANK
      CHARACTER( 16 ) :: PNAME = 'Driver'
      CHARACTER( 16 ) :: FNAME
      CHARACTER( 96 ) :: MSG
      INTEGER DSZE
      INTEGER TAG, REQ, STATUS( MPI_STATUS_SIZE )
      LOGICAL WORKER, WRITER
!     LOGICAL :: CNEW = .FALSE.
!     LOGICAL :: DNEW = .FALSE.
!     LOGICAL :: ANEW = .FALSE.
!     LOGICAL :: WNEW = .FALSE.
!     LOGICAL :: PNEW = .FALSE.
      LOGICAL, EXTERNAL :: PIO_INIT
      INTEGER, EXTERNAL :: SETUP_LOGDEV
      CHARACTER( 2 ) :: DATA_ORI = 'cr'
      INTEGER :: GEO_ORI = 0   ! 0 (Cartesian), 1 (Matrix)

      integer i

C-----------------------------------------------------------------------

!     call f_hpminit ( 99, 'driver' )

C Initialize message-passing groups and communicators
      CALL INIT_MPI

!     CALL SETUP_LOGDEV ( LOGDEV )
      LOGDEV = SETUP_LOGDEV ()

!     write( logdev,* ) ' '
!     write( logdev,* ) '    Known globally ............................'
!     write( logdev,* ) '       G_MYPE: ', G_MYPE,
!    &                     ',  NPROCS: ', NPROCS
!     write( logdev,* ) '       WORLD_GROUP: ', WORLD_GROUP,
!    &                     ',  IO_GROUP: ', IO_GROUP,
!    &                     ',  WORK_GROUP: ', WORK_GROUP
!     write( logdev,* ) '    Known locally only ........................'
!     write( logdev,* ) '       IO_MYPE: ', IO_MYPE,
!    &                    ',  NPROCS_IO: ', NPROCS_IO,
!    &                    ',  IO_COMM: ', IO_COMM 
!     write( logdev,* ) '       WORK_MYPE: ', WORK_MYPE,
!    &                    ',  NPROCS_W: ', NPROCS_W,
!    &                    ',  WORK_COMM: ', WORK_COMM
!     write( logdev,* ) ' '

C Initialize run scenario
      CALL INITSCEN ( STDATE, STTIME, TSTEP, NSTEPS, LOGDEV )
      JDATE = STDATE; JTIME = STTIME

      WORKER = WORK_COMM .NE. MPI_UNDEFINED
      WRITER = IO_COMM .NE. MPI_UNDEFINED

      IF ( WORKER ) THEN
         NUMPROCS = NPROCS_W
         RANK = WORK_MYPE
         ELSE IF ( WRITER ) THEN
!        NUMPROCS = NPROCS - 1 !!! can't leave this with NPROCS-1 - just testing
         NUMPROCS = NPROCS - NPROCS_IO
         RANK = IO_MYPE
         END IF

C Set up horizontal domain and calculate processor-to-subdomain maps
C and define vertical layer structure (in module GRID_CONF)
      IF ( .NOT. GRID_INIT ( NUMPROCS, RANK ) ) THEN
         MSG = '*** Failure defining domain configuration'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

!     write( logdev,* ) ' '
!     do i = 1, nprocs
!        write( logdev,* ) ' i,COLSX_PE( 1,i ): ', i, COLSX_PE( 1,i )
!        write( logdev,* ) ' i,COLSD_PE( 1,i ): ', i, COLSD_PE( 1,i )
!        write( logdev,* ) ' i,COLSX_PE( 2,i ): ', i, COLSX_PE( 2,i )
!        write( logdev,* ) ' i,COLSD_PE( 2,i ): ', i, COLSD_PE( 2,i )
!        write( logdev,* ) ' i,ROWSX_PE( 1,i ): ', i, ROWSX_PE( 1,i )
!        write( logdev,* ) ' i,ROWSD_PE( 1,i ): ', i, ROWSD_PE( 1,i )
!        write( logdev,* ) ' i,ROWSX_PE( 2,i ): ', i, ROWSX_PE( 2,i )
!        write( logdev,* ) ' i,ROWSD_PE( 2,i ): ', i, ROWSD_PE( 2,i )
!        end do
!     write( logdev,* ) ' '

C Initialize stencil exchange routines
      CALL SE_INIT( NPROCS, NPCOL, NPROW, GL_NCOLS, GL_NROWS, NLAYS,
     &              NSPCSD, G_MYPE, MNDIS, MEDIS, MSDIS, MWDIS,
     &              DATA_ORI, GEO_ORI )

!     call f_hpmstart ( g_mype+1, 'driver' )

C Initialize CGRID
      IF ( .NOT. CGRID_INIT () ) THEN
         MSG = "Error initializing CGRID"
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C Initialize ACONC
      IF ( .NOT. ACONC_INIT () ) THEN
         MSG = "Error initializing ACONC"
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C Initialize DRYDEP
      IF ( .NOT. DDEP_INIT () ) THEN
         MSG = "Error initializing DDEP"
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C Initialize WETDEP
      IF ( .NOT. WDEP_INIT () ) THEN
         MSG = "Error initializing WDEP"
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C Initialize VIS
      IF ( .NOT. VIS_INIT () ) THEN
         MSG = "Error initializing VIS"
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C Initialize optional derived vertical velocity writes to conc file
      IF ( .NOT. WVEL_INIT () ) THEN
         MSG = 'Failure initializing derived vertical velocity writes'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2  )
         END IF

      ISTEP = 0

C Try to open existing files for update

!     IF ( WORKER ) THEN
!        FILE_ACCS = FSREAD3
!        ELSE
!        FILE_ACCS = FSRDWR3
!        END IF

!     IF ( .NOT. OPEN3( CTM_CONC_1, FILE_ACCS, PNAME ) ) THEN
!        MSG = 'Could not open CTM_CONC_1 for update - try to open new'
!        CALL M3MESG( MSG )
!        CNEW = .TRUE.
!        END IF

!     IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FILE_ACCS, PNAME ) ) THEN
!        MSG = 'Could not open CTM_DRY_DEP_1 for update - try to open new'
!        CALL M3MESG( MSG )
!        DNEW = .TRUE.
!        END IF

!     IF ( .NOT. OPEN3( A_CONC_1, FILE_ACCS, PNAME ) ) THEN
!        MSG = 'Could not open A_CONC_1 for update - try to open new'
!        CALL M3MESG( MSG )
!        ANEW = .TRUE.
!        ELSE   ! check header
!        CALL CK_ACONC_HDR ( JDATE, JTIME, TSTEP( 1 ) )
!        END IF

!     IF ( .NOT. OPEN3( CTM_WET_DEP_1, FILE_ACCS, PNAME ) ) THEN
!        MSG = 'Could not open CTM_WET_DEP_1 for update - try to open new'
!        CALL M3MESG( MSG )
!        WNEW = .TRUE.
!        END IF

!     IF ( N_AE_SPC .GT. 0 ) THEN
!        IF ( .NOT. OPEN3( CTM_VIS_1, FILE_ACCS, PNAME ) ) THEN
!           MSG = 'Could not open CTM_VIS_1 for update - try to open new'
!           CALL M3MESG( MSG )
!           PNEW = .TRUE.
!           END IF
!        END IF

      IF ( WORKER ) THEN

C Initialize 1 I/O routines
         IF ( .NOT. PIO_INIT( PNAME, GL_NROWS, GL_NCOLS, NLAYS, NTHIK,
!        IF ( .NOT. PIO_INIT( LOGDEV, GL_NROWS, GL_NCOLS, NLAYS, NTHIK,
     &                        NROWS, NCOLS, NPROW, NPCOL, NPROCS-1,
     &                        WORK_MYPE ) ) THEN
            MSG = 'Failed to initialize parallel I/O library.'
            CALL M3WARN( PNAME, 0, 0, MSG )
            END IF

C Get initial data into CGRID
         IF ( N_GC_SPC .GT. 0 ) THEN
            FNAME = INIT_GASC_1
            CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'GC' )
            END IF
         FNAME = MET_CRO_3D
         CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'RJ' )
         IF ( N_AE_SPC .GT. 0 ) THEN
            FNAME = INIT_AERO_1
            CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'AE' )
            END IF
         IF ( N_NR_SPC .GT. 0 ) THEN
            FNAME = INIT_NONR_1
            CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'NR' )
            END IF
         IF ( N_TR_SPC .GT. 0 ) THEN
            FNAME = INIT_TRAC_1
            CALL LOAD_CGRID ( FNAME, STDATE, STTIME, 'TR' )
            END IF

C Copy zeros for initial derived vert vel to global array for advection

         TAG = G_MYPE * TAGFAC + CONC_TAG + ISTEP
!        write( logdev,* ) '         d - ISTEP, TAG: ', istep, tag

         IF ( W_VEL ) THEN
C Append WVEL to CGRID to write on CONC file (all zeros from WVEL_INIT)
            CGRID_X( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS,NSPCSD+1 ) =
     &      WVEL( 1:MY_NCOLS,1:MY_NROWS,1:NLAYS )
            DSZE = MY_NCOLS * MY_NROWS * NLAYS * ( NSPCSD + 1 )
            CALL MPI_ISSEND ( CGRID_X, DSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                        REQ, ERROR )
            ELSE
            DSZE = MY_NCOLS * MY_NROWS * NLAYS * NSPCSD
!           CALL MPI_ISSEND ( CGRID, DSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
!    &                        REQ, ERROR )
            CALL MPI_SEND ( CGRID, DSZE, MPI_REAL, 0, TAG, MPI_COMM_WORLD,
     &                      ERROR )
            END IF

C Make sure send has completed to keep data buffer coherent
!        IF ( REQ .NE. MPI_REQUEST_NULL )
!    &      CALL MPI_WAIT ( REQ, STATUS, ERROR )

!        write( logdev,* ) '         d - ISTEP, after send: ', istep


         END IF   ! WORKER

      IF ( WRITER ) THEN
!        IF ( CNEW ) CALL OPCONC  ( JDATE, JTIME, TSTEP( 1 ), LOGDEV )
!        IF ( DNEW ) CALL OPDDEP  ( JDATE, JTIME, TSTEP( 1 ) )
!        IF ( ANEW ) CALL OPACONC ( JDATE, JTIME, TSTEP( 1 ) )
!        IF ( WNEW ) CALL OPWDEP  ( JDATE, JTIME, TSTEP( 1 ) )
!        IF ( PNEW ) CALL OPVIS   ( JDATE, JTIME, TSTEP( 1 ) )
         CALL OPCONC  ( JDATE, JTIME, TSTEP( 1 ), LOGDEV )
         CALL OPDDEP  ( JDATE, JTIME, TSTEP( 1 ) )
         CALL OPACONC ( JDATE, JTIME, TSTEP( 1 ) )
         CALL OPWDEP  ( JDATE, JTIME, TSTEP( 1 ) )
         IF ( N_AE_SPC .GT. 0 ) CALL OPVIS   ( JDATE, JTIME, TSTEP( 1 ) )
         END IF

      CALL MPI_BARRIER ( MPI_COMM_WORLD, ERROR )

C Core algorithm

      DO WHILE ( ISTEP .LT. NSTEPS )

         ISTEP = ISTEP + 1

         IF ( WORKER ) THEN
            CALL CTK ( JDATE, JTIME, TSTEP, ISTEP, LOGDEV )
            END IF

         IF ( WRITER ) THEN
            CALL NEXTIME ( JDATE, JTIME, TSTEP( 1 ) )
            CALL COLLECT ( JDATE, JTIME, ISTEP, NSTEPS, LOGDEV )
            END IF

         END DO

      CALL MPI_BARRIER ( MPI_COMM_WORLD, ERROR )

!     call f_hpmstop ( g_mype+1 )

!     call f_hpmterminate ( 99 )

      IF ( SHUT3() ) THEN
         WRITE( LOGDEV,'(/ 15X, A )' ) "Program Completed Successfully"
         ELSE
         WRITE( LOGDEV,'(/ 15X, A )' ) "Program NOT Successfully Completed"
         END IF

      CALL SHUT_DOWN_MPI

      END PROGRAM DRIVER
