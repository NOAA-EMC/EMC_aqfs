
        SUBROUTINE MRGGRID ( premaq_sdate, premaq_stime)

C***********************************************************************
C  program body starts at line 
C
C  DESCRIPTION:
C    Program MRGGRID reads 2-D and 3-D I/O API files and merges them
C    into a single 2-D or 3-D file (depending on the inputs)
C    The time period merged is adjusted based on the latest
C    starting file and earliest ending file, unless MRG_DIFF_DAY is
C    set in which case the time period is based on the standard 
C    environment variables. All variables are merged, even if different 
C    variables are in each file.
C
C  PRECONDITIONS REQUIRED:  
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C    Original by M. Houyoux 4/98
C    01/05 by David Wong
C      -- Modified the code to eliminate reading data from files: PLAY,
C         B3GTS_L, PGTS3D_L, and MGTS_L
C    08/06 by David Wong
C      -- Split the logic IF( MRGDIFF .AND. USEFIRST( F ) ) THEN into
C         two separated ones to avoid the case when MRGDIFF is FALSE
C         which means USEFIRST is undefined.
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: mrggrid.f,v 1.13 2004/06/27 23:58:58 cseppan Exp $
C
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C 
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C 
C smoke@unc.edu
C
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/smkmerge/mrggrid.f,v $
C Last updated: $Date: 2004/06/27 23:58:58 $ 
C
C***********************************************************************
 
C...........   MODULES for public variables
C.........  This module contains the global variables for the 3-d grid
!        USE MODGRID, ONLY: NGRID, NCOLS, NROWS, NLAYS, 
!     &                     VGLVS, VGTYP, VGTOP


        USE MODGRID, ONLY: NGRID, 
     &                     VGLVS, VGTYP, VGTOP
          
        USE PREMAQPARM, ONLY: NCOLS, NROWS, NLAYS

!        USE MOD_MRGGRID, ONLY: LDEV, JDATE, JTIME, NVOUT, INDXN, OUTNAM,
!     &           EOUT,NFILE,MRGDIFF

        USE MOD_MRGGRID

        use mod_mobile, only: nspecies, specieslist, pmspecies,
     &                        pmspecieslist, pm_emis

        use MODBEIS3v11, only : emisl, emspc

        use MODMERGE, only : PEMNAM, PEMGRD

        use mod_rw
	
        IMPLICIT NONE
 
C...........   INCLUDES:
        INCLUDE 'EMCNST3.EXT'
!       INCLUDE 'PARMS3.EXT'	
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'FDESC3.EXT'
      
C...........   EXTERNAL FUNCTIONS
        CHARACTER(2)  CRLF
        LOGICAL       ENVYN
        INTEGER       GETFLINE
        LOGICAL       GETYN       
        INTEGER       INDEX1
        INTEGER       LBLANK
        INTEGER       PROMPTFFILE
        CHARACTER(16) PROMPTMFILE
        INTEGER       SEC2TIME
        INTEGER       SECSDIFF

        CHARACTER*10    HHMMSS
        external HHMMSS


        EXTERNAL CRLF, ENVYN, GETFLINE, GETYN, INDEX1, LBLANK,
     &           PROMPTFFILE, PROMPTMFILE, SEC2TIME, SECSDIFF

C.........  LOCAL PARAMETERS and their descriptions:

        CHARACTER(50), PARAMETER::CVSW = '$Name: SMOKE_v2_1_09302004 $' ! CVS release tag


        integer premaq_sdate, premaq_stime
        integer V,F
	CHARACTER(16)  VNM
        CHARACTER(16) :: PROGNAME = 'MRGGRID' ! program name

        INTEGER :: NROWSNCOLS, STAT
        INTEGER, ALLOCATABLE, SAVE :: LOC(:, :)
        INTEGER, ALLOCATABLE, SAVE :: SWITCH(:)
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C***********************************************************************
C   begin body of program MRGGRID


      WRITE( *, * )    'Merging Emissions ...'
      WRITE( *, 94030 ) HHMMSS(  premaq_stime )
      
        NROWSNCOLS = NROWS * NCOLS
 
        LDEV = INIT3()

        IF (FIRSTIME) THEN
           ALLOCATE (LOC(NVOUT, NFILE), STAT=STAT)
           CALL CHECKMEM( IOS, 'LOC', PROGNAME )
           ALLOCATE (SWITCH(NVOUT), STAT=STAT)
           CALL CHECKMEM( IOS, 'SWITCH', PROGNAME )
           LOC = -1
           SWITCH = -1
        END IF

C.........  Loop through hours
!AQF        JDATE = SDATE
!AQF        JTIME = STIME
	
	
         JDATE = premaq_sdate
         JTIME = premaq_stime
	 	
!AQF        DO T = 1, NSTEPS

C.............  Loop through species
            DO V = 1, NVOUT

                VNM = OUTNAM( INDXN( V ) ) !AQF  VNAME3D( V ) 

                
C.................  Output array
                EOUT = 0.   ! array

                DO F = 1, NFILE

C.....................  Set read date
c                   IF( MRGDIFF .AND. USEFIRST( F ) ) THEN
                    IF( MRGDIFF ) THEN
                      IF( USEFIRST( F ) ) THEN
                        DUMMY = 0
                        STEPS = SEC2TIME( 
     &                            SECSDIFF( 
     &                              SDATE, DUMMY, JDATE, DUMMY ) )
                        RDATE = SDATEA( F )
                        CALL NEXTIME( RDATE, DUMMY, STEPS )
                      END IF
                        
                    ELSE
                        RDATE = JDATE
                    END IF

C.....................  Set tmp variables
                    NAM = FNAME ( F )       ! input file name
                    NL  = NLAYSA( F )       ! number of layers

C.....................  If file has species, read (do this for all files)...
                    IF( LVOUTA( V,F ) ) THEN

C.........................  If 2-d input file, read, and add
                       IF (F .EQ. 2) THEN        ! for B3GTS_L file
                          IF (FIRSTIME) THEN
                             LOC(V,F) = INDEX1 (VNM, SIZE(EMSPC), EMSPC)
                             IF (LOC(V,F) .LE. 0) THEN
                                PRINT *, 'Invalid species: ', VNM,
     &                                   ' in EMSPC list'
                                STOP
                             END IF
                          END IF
                          ARRAY_PT => EMISL(:,:,LOC(V,F))
                          E2D(:,1) = RESHAPE (ARRAY_PT, (/NROWSNCOLS/))
                       ELSE IF (F .EQ. 3) THEN   ! for PGTS3D_L file
                          IF (FIRSTIME) THEN
                             LOC(V,F) = INDEX1 (VNM, SIZE(PEMNAM), PEMNAM)
                             IF (LOC(V,F) .LE. 0) THEN
                                PRINT *, 'Invalid species: ', VNM,
     &                                   ' in PEMNAM list'
                                STOP
                             END IF
                          END IF
                          E2D(:,:) = PEMGRD (:,:,LOC(V,F))
                       ELSE IF (F .EQ. 4) THEN   ! for MGTS_L file
                          IF (FIRSTIME) THEN
                             LOC(V,F) = INDEX1 (VNM, NSPECIES, SPECIESLIST)
                             IF (LOC(V,F) .GT. 0) THEN
                                SWITCH(V) = 1
                                ARRAY_PT => EMIS_MODEL(:,:,LOC(V,F))
                             ELSE
                                LOC(V,F) = INDEX1 (VNM, PMSPECIES, PMSPECIESLIST)
                                IF (LOC(V,F) .GT. 0) THEN
                                   SWITCH(V) = 2
!                                  ARRAY_PT => PM_EMIS(:,:,RW_M_INDEX,
!    &                                                 RW_HR_INDEX,LOC(V,F))
                                   ARRAY_PT => PM_EMIS(:,:,LOC(V,F))
                                ELSE
                                   PRINT *, 'Invalid species: ', VNM,
     &                                      ' in SPECIESLIST/PMSPECIESLIST list'
                                   STOP
                                END IF
                             END IF
                          END IF

                          IF (SWITCH(V) .EQ. 1) THEN
                             ARRAY_PT => EMIS_MODEL(:,:,LOC(V,F))
                          ELSE
!                            ARRAY_PT => PM_EMIS(:,:,RW_M_INDEX,
!    &                                           RW_HR_INDEX,LOC(V,F))
                             ARRAY_PT => PM_EMIS(:,:,LOC(V,F))
                          END IF
                          E2D(:,1) = RESHAPE (ARRAY_PT, (/NROWSNCOLS/))
                       ELSE       ! f .eq. 1       for AGTS_L file
                            IF( .NOT. READ3( NAM, VNM, ALLAYS3, JDATE,
     &                                       JTIME, E2D          )) THEN

                                MESG = 'Could not read "' // VNM //
     &                                 '" from file "' //
     &                                 NAM( 1:LEN_TRIM( NAM ) )// '".'
                                CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
                            ENDIF
                       END IF

                       IF( NL .EQ. 1 ) THEN

c                           EOUT( 1:NGRID,1 ) = EOUT( 1:NGRID,1 ) + E2D (1:NGRID, 1)
                            EOUT( :,1 ) = EOUT( :,1 ) + E2D ( :,1 )

C.........................  If 3-d input file, allocate memory, read, and add
                       ELSE

                            EOUT( 1:NGRID,1:NL )= EOUT( 1:NGRID,1:NL ) +
     &                                              E2D( 1:NGRID, 1:NL )

                       END IF  ! if 2-d or 3-d

                    END IF      ! if pollutant is in this file

C.....................  Build report line if needed
                    IF( MRGDIFF .AND. V == 1 ) THEN
                        IF( F == 1 ) THEN
                            WRITE( RPTLINE,93020 ) JDATE
                            WRITE( RPTCOL,93020 ) JTIME
                            RPTLINE = TRIM( RPTLINE ) // RPTCOL
                        END IF
                        
                        WRITE( RPTCOL,93020 ) RDATE
                        RPTLINE = TRIM( RPTLINE ) // RPTCOL
                    END IF

                END DO          ! loop over input files

C.................  Write species/hour to output file

                IF( .NOT. WRITE3( ONAME, VNM, JDATE, JTIME, EOUT )) THEN

                    MESG = 'Could not write "'// VNM// '" to file "'// 
     &                      ONAME( 1:LEN_TRIM( ONAME ) ) // '".'
     &                        
                    CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

                END IF

            END DO   ! loop through variables

C.............  Write this time step to report
            IF( MRGDIFF ) THEN
                WRITE( RDEV,93000 ) TRIM( RPTLINE )
            END IF

            FIRSTIME = .FALSE.

            CALL NEXTIME( JDATE, JTIME, TSTEP )
      
!AQF        END DO       ! loop through timesteps

C......... Normal Completion
!AQF        CALL M3EXIT( PROGNAME, 0, 0, ' ', 0)
    
C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT( 5X, A )
 
C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT(  A )

93010   FORMAT( A15 )

93020   FORMAT( I15 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I7, :, 1X ) )

94020   FORMAT( A, :, I3, :, 1X, 10 ( A, :, F8.5, :, 1X ) )

94030   FORMAT( 8X, 'at time ', A8)

        END SUBROUTINE MRGGRID
