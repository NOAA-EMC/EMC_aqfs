
      SUBROUTINE BEIS311 ( premaq_date, premaq_time )

C***********************************************************************
C  program body starts at line  187
C
C  DESCRIPTION:
C       Computes hourly time stepped gridded biogenic emissions using 
C       normalized gridded emissions from NORMBEIS311 and postprocessed MM5
C       meteorology.
C
C  PRECONDITIONS REQUIRED:
C       Postprocessed MM5 meteorology that contains temperature, 
C       solar radiation, and pressure data. 
C       Normalized gridded emissions B3GRD from NORMBEIS3v0.9 
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       HRBIO, PREBMET
C
C  REVISION  HISTORY:
C      4/01: Prototype by Jeff Vukovich
C            Tested only on 36km Lambert domain 
C            Summer/winter switch file option not tested
C      4/03: Modified for use with Air Quality FOrecasting
C            George Pouliot
C      4/04: Changed TEMP10 to TEMP2 for AQF.  (TLO)
C     01/05: Output EMISL to a file according to logical variable STORE_FILE
C            (David Wong)
C     01/05: Interchanged order of nested loop in various places to
C            increase number of cache hits (David Wong)
C     01/05: Parallelized certain loop(s) using OpenMP construct (David Wong)
C     03/05: updated index calculation for rainfall data (bug fix for hour 24) (G. Pouliot)
C                  
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

C-----------------------------------------------------------
C.... Modules for public variables
C.... This module contains the speciation profile tables
C-----------------------------------------------------------

      USE MODSPRO

C-----------------------------------------------------------
C.... This module contains BEIS3 specific arrays
C-----------------------------------------------------------

      USE MODBEIS3V11

C-----------------------------------------------------------
C.... This modules contains the MCIP met variables
C-----------------------------------------------------------

      USE MCOUTCOM

C-----------------------------------------------------------
C.... This modules contains the Merge Grid variables
C-----------------------------------------------------------

      USE MOD_RW


      IMPLICIT NONE

C-----------------------------------------------------------
C.... INCLUDES:
C-----------------------------------------------------------

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations
      INCLUDE 'EMCNST3.EXT'     !
      INCLUDE 'B3V11DIMS3.EXT'  ! biogenic-related constants

C-----------------------------------------------------------
C.... PARAMETERS and their descriptions:
C.... LOCAL PARAMETERS
C-----------------------------------------------------------

      CHARACTER*50, PARAMETER :: CVSW = '$Name:  $' ! CVS release tag

      integer premaq_date, premaq_time

C-----------------------------------------------------------
C.... EXTERNAL FUNCTIONS and their descriptions:
C-----------------------------------------------------------

      EXTERNAL      ENVINT, ENVYN, GETFLINE, HHMMSS, INDEX1,
     &              PROMPTMFILE, PROMPTFFILE, TRIMLEN, VERCHAR

      LOGICAL       ENVYN

      CHARACTER*50  GETCFDSC
      CHARACTER*10  HHMMSS
      CHARACTER*16  PROMPTMFILE, VERCHAR
      CHARACTER*300 MESG      !  message buffer for M3EXIT()

      CHARACTER*16 :: PROGNAME = 'TMPBEIS311'   !  program name

      INTEGER       ENVINT, GETFLINE, INDEX1, PROMPTFFILE, TRIMLEN
      INTEGER       B, M    !  counters for biogenic, model species
      INTEGER       I, J, K, L, N, C, R  !  loop counters and subscripts

C***********************************************************************
C   begin body of subroutine BEIS311
C***********************************************************************

      TZONE = 0
      NSTEPS = 1
      JDATE = premaq_date
      JTIME = premaq_time

C---------------------------------------------------------------------
C.... Build description for, and create/open output file
C.... (all but variables-table in description is borrowed from M3NAME)
C---------------------------------------------------------------------

      BSTEPS  = NSTEPS

C---------------------------------------------------------------------
C.... Loop thru the number of time steps (hourly)
C---------------------------------------------------------------------

!AQF  DO HR = 1, BSTEPS

      EMISL = 0   !  array
      EMISS = 0   !  array
      EMPOL = 0   !  array

      IF ( JDATE .NE. LDATE ) THEN

         CALL WRDAYMSG( JDATE, MESG )               

C.... If new date, read season switch 

         IF ( SWITCH_FILE ) THEN
                 
            MESG = 'Reading gridded season switch data..'  
            CALL M3MESG( MESG ) 

            IF (.NOT. READ3(BNAME, 'SEASON', 1, JDATE, 0, SWITCH)) THEN
               MESG = 'Could not read SEASON from file ' //
     &                 BNAME( 1:TRIMLEN (BNAME) )
               CALL M3EXIT( PROGNAME, JDATE, 0, MESG, 2 )
            ENDIF

            MESG = 'Applying gridded season switch data..' 
            CALL M3MESG( MESG )

          ! $omp parallel do

            DO J = 1, NROWS
               DO I = 1, NCOLS

C.... If switch equal to 0 use winter normalized emissions

                  IF ( SWITCH (I, J) .EQ. 0 ) THEN
                          
                     SEMIS(I, J, 1:NSEF-1) = AVGEMIS(I, J, 1:NSEF-1, 2)
                     SLAI (I, J, 1:NLAI  ) = AVGLAI (I, J, 1:NLAI  , 2)
                  ELSE
                     SEMIS(I, J, 1:NSEF-1) = AVGEMIS(I, J, 1:NSEF-1, 1)
                     SLAI( I, J, 1:NLAI  ) = AVGLAI (I, J, 1:NLAI  , 1)
                  ENDIF
               ENDDO
            ENDDO

          ! $omp end parallel do

         ENDIF  ! switch file endif
      ENDIF     ! new day endif
C-----------------------------------------------------------
C.... Write to screen because WRITE3 only writes to LDEV
C-----------------------------------------------------------

      WRITE( *, * )    'Creating Biogenic Emissions ...'
      WRITE( *, 94030 ) HHMMSS( JTIME )

C-----------------------------------------------------------
C.... Compute zenith angle
C-----------------------------------------------------------

      CALL CZANGLE ( JDATE, JTIME, NCOLS, NROWS, LAT, LON, 
     &              COSZEN, GETATN ) 

      TASFC (1:ncols, 1:nrows) = temp2_c(1:ncols,1:nrows) 
      RN    (1:ncols, 1:nrows) = rainn_c(1:ncols,1:nrows)
      RC    (1:ncols, 1:nrows) = rainc_c(1:ncols,1:nrows)
      PRES  (1:ncols, 1:nrows) = prsfc_c(1:ncols,1:nrows)            
      TSOLAR(1:ncols, 1:nrows) = rgrnd_c(1:ncols,1:nrows)
 
C-----------------------------------------------------------
C....  convert from Pa to millibars
C-----------------------------------------------------------

    ! $omp parallel do
      DO R = 1, NROWS
         DO C = 1, NCOLS
            PRES (C, R) = PRES (C, R) * 0.010     ! Pa to mb
         ENDDO
      ENDDO
    ! $omp end parallel do

C-----------------------------------------------------------
C.... Calculate non-speciated emissions
C.... must pass met date and time here
C-----------------------------------------------------------

      if ( INITIAL_RUN ) then
         if ( hr .le. RHOURS-1 ) then
            INITIAL_HOUR = .true.
         endif
      endif
  
!     CALL HRBEIS3( MDATE, MTIME, NCOLS, NROWS, COSZEN, MSPCS,
!    &              SEMIS, SLAI, TASFC, TSOLAR, PRES, EMPOL )

!-----------------------------------------------------------
!**** compute RHOURS hr rainfall totals
!-----------------------------------------------------------

!     index = mod(hr,RHOURS)

      INDEX = MOD( HR-1, RHOURS ) + 1
	    		   
      rainfall(1:NCOLS,1:NROWS,index) = 
     &      RN(1:NCOLS,1:NROWS) + RC(1:NCOLS,1:NROWS)

      if ( .not. INITIAL_RUN ) then
         RN (1:NCOLS,1:NROWS) = 0.0
         do i_loop = 1,RHOURS              
            RN(1:NCOLS,1:NROWS) = 
     &           RN(1:NCOLS,1:NROWS) + rainfall(1:NCOLS,1:NROWS,i_loop)
            enddo
      endif

      CALL HRBEIS3 ( MDATE, MTIME, NCOLS, NROWS, COSZEN,MSPCS,
     &               INITIAL_HOUR,
     &               SEMIS, GROWAGNO, NGROWAGNO, NONAGNO,
     &               SLAI,TASFC, RN,
     &               PTYPE, PULSEDATE, PULSETIME,
     &               TSOLAR, PRES, EMPOL )

      if ( INITIAL_RUN ) then
         if ( hr .ge. RHOURS ) then
            INITIAL_HOUR = .false.
         endif
      endif

C-----------------------------------------------------------
C.... Speciate emissions
C-----------------------------------------------------------

      DO K = 1, NSEF

       ! $omp parallel do
         DO L = 1, MSPCS
            DO J = 1, NROWS
               DO I = 1, NCOLS
                  EMISL( I, J, L ) = EMISL( I ,J, L ) +
     &                               EMPOL( I, J, K ) * MLFAC( L, K )
                  EMISS( I, J, L ) = EMISS( I, J, L ) +
     &                               EMPOL( I, J, K ) * MSFAC( L, K ) 
               ENDDO
            ENDDO
         ENDDO
       ! $omp end parallel do

      ENDDO

C-----------------------------------------------------------
C.... Convert to moles/second if necessary
C-----------------------------------------------------------

      IF ( UNITTYPE .EQ. 2 ) THEN
         EMISL = EMISL * HR2SEC    ! array multiplication 
      ENDIF

C-----------------------------------------------------------
C.... Write out speciated emissions    
C-----------------------------------------------------------

      IF ( STORE_FILE ) THEN
         IF ( .NOT. WRITE3 (ENAME, 'ALL', JDATE, JTIME, EMISL) ) THEN
            CALL M3EXIT ( PROGNAME, JDATE, JTIME, 
     &                   'Error writing BEIS3 OUTPUT file' , 2 )
         ENDIF                              !  if write3 failed
      ENDIF

!AQF  IF ( .NOT. WRITE3 ( SNAME, 'ALL', JDATE, JTIME, EMISS ) ) THEN
!AQF     CALL M3EXIT ( PROGNAME, JDATE, JTIME,
!AQF  &               'Error writing BEIS3 OUTPUT file' , 2 )
!AQF  ENDIF                                 !  if write3 failed

C###################################################################
C-----------------------------------------------------------
C.... Next time step
C-----------------------------------------------------------
C###################################################################

      LDATE = JDATE

C.... added by GAP 12/01 .....................................

!AQF  IF (HR .NE. BSTEPS) CALL NEXTIME ( JDATE, JTIME, 10000 )

      CALL NEXTIME ( JDATE, JTIME, 10000 )

C.....end added by GAP 12/01 .................................

      CALL NEXTIME ( MDATE, MTIME, 10000 ) 

      RDATE = MDATE
      RTIME = MTIME

      IF ( HR .EQ. SOILOUT_TIME ) CALL EMIS_END
      HR = HR + 1

!AQF  ENDDO                !  end loop on hours HR


C******************  FORMAT  STATEMENTS  *********************
C
C.... Informational (LOG) message formats... 92xxx
C.... Internal buffering formats............ 94xxx
C

92000   FORMAT ( 5X , A )

94000   FORMAT( I2.2 )
94010   FORMAT( 10( A, :, I8, :, 1X ) )
94030   FORMAT( 8X, 'at time ', A8 )

      END SUBROUTINE BEIS311  

