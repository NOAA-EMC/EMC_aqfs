
        SUBROUTINE TEMPORAL (premaq_jdate, premaq_jtime)

C***********************************************************************
C  program body starts at line 214
C
C  DESCRIPTION:
C    This program computes the hourly emissions data from inventory emissions 
C    and/or activity and emission factor data. It can read average-inventory,
C    day-specific and hour-specific emissions and activity data.
C
C  PRECONDITIONS REQUIRED:  
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C    copied by: M. Houyoux 01/99
C    origin: tmppoint.F 4.3
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: temporal.f,v 1.24 2004/06/27 02:09:21 cseppan Exp $
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
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/temporal/temporal.f,v $
C Last updated: $Date: 2004/06/27 02:09:21 $ 
C
C*************************************************************************

C.........  MODULES for public variables
C.........  This module contains the inventory arrays
        USE MODSOURC, ONLY: TZONES, TPFLAG, FLTRDAYL

C.........  This module contains the temporal cross-reference tables
        USE MODXREF, ONLY: MDEX, WDEX, DDEX

C.........  This module contains the temporal profile tables
        USE MODTMPRL, ONLY: NMON, NWEK, NHRL

C.........  This module contains emission factor tables and related
        USE MODEMFAC, ONLY: NEFS, INPUTHC, OUTPUTHC, EMTNAM,
     &                      EMTPOL, NEPOL, NETYPE

C.........  This module contains data for day- and hour-specific data
        USE MODDAYHR, ONLY: DYPNAM, DYPDSC, NDYPOA, NDYSRC, 
     &                      HRPNAM, HRPDSC, NHRPOA, NHRSRC,
     &                      LDSPOA, LHSPOA, LHPROF,
     &                      INDXD, EMACD, INDXH, EMACH

C.........  This module contains the lists of unique source characteristics
C        USE MODLISTS, ONLY: NINVIFIP, INVIFIP, MXIDAT, INVDNAM, INVDVTS

C.........  This module contains the information about the source category
        USE MODINFO, ONLY: CATEGORY, BYEAR, NIPPA, EANAM, NSRC, 
     &                     NIACT, INVPIDX, NIPOL, EAREAD, EINAM, ACTVTY

C.........  This module is used for MOBILE6 setup information 
        USE MODMBSET, ONLY: DAILY, WEEKLY, MONTHLY, EPISLEN

        USE MOD_TEMPORAL
	
	

C.........  INCLUDES:
        IMPLICIT NONE

        INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions
	
        integer premaq_jdate, premaq_jtime
	
        
	 

C..........  EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         CHKINT
        CHARACTER(2)    CRLF
        INTEGER         ENVINT
        LOGICAL         ENVYN
        INTEGER         FINDC
        INTEGER         GETDATE
        INTEGER         GETFLINE
        INTEGER         GETNUM
        INTEGER         INDEX1
        LOGICAL         ISDSTIME
        CHARACTER(14)   MMDDYY
        INTEGER         PROMPTFFILE
        INTEGER         RDTPROF
        INTEGER         SECSDIFF
        INTEGER         STR2INT
        LOGICAL         SETENVVAR
        CHARACTER*10    HHMMSS	

        EXTERNAL    CHKINT, CRLF, ENVINT, ENVYN, FINDC, 
     &              GETDATE, GETFLINE, GETNUM, INDEX1, ISDSTIME, MMDDYY,
     &              PROMPTFFILE, RDTPROF, SECSDIFF, STR2INT, SETENVVAR,
     &              HHMMSS



        INTEGER         I                        
	
	
	
        CHARACTER(16) :: PROGNAME = 'TEMPORAL' ! program name

C***********************************************************************
C   begin body of program TEMPORAL


	
            JDATE = premaq_jdate
	    JTIME = premaq_jtime


            WRITE( *, * )    'Creating Point Source Emissions ...'
            WRITE( *, 94031 ) HHMMSS( JTIME )
			    
C.........  Loop through pollutant/emission-type groups
        DO N = 1, NGRP


!****** this is the main time loop
!



C.............  Loop through time steps for current pollutant group
!            DO T = 1, NSTEPS

C.................  Adjust sources' time zones to account for daylight time...
C.................  Subtract 1 if date is daylight time and TZONES is not already
C                   converted.  Add 1 if date is standard and TZONES has been
C                   converted.
C.................  FLTRDAYL is a source-array of 0s and 1s to permit sources
C                   to not get daylight time conversion.
                IF( ISDSTIME( JDATE ) .AND. .NOT. DAYLIT ) THEN
                    
                    DAYLIT = .TRUE.
                    
                    TZONES = TZONES - 1 * FLTRDAYL   ! arrays
                
                ELSE IF( .NOT. ISDSTIME( JDATE ) .AND. DAYLIT ) THEN
                
                    DAYLIT = .FALSE.
                
                    TZONES = TZONES + 1 * FLTRDAYL   ! arrays
                
                END IF
                    
                IF( NIACT .GT. 0 ) THEN    ! NOT USED FOR PT or AR sources

C.....................  Create array of emission factors by source

C.....................  Loop through pollutants/emission-types in this group
                    DO I = 1, NGSZ
                        CBUF = EANAM2D( I,N )
                        L1   = LEN_TRIM( CBUF )

C.........................  Skip blanks that can occur when NGRP > 1
                        IF ( CBUF .EQ. ' ' ) CYCLE

C.........................  Check that this pollutant uses emission factors
C                           Look for double underscore in pollutant name
                        K = INDEX( CBUF, ETJOIN )
                        IF( K == 0 ) THEN
                            EMFAC( :,I ) = -1
                            CYCLE
                        END IF 

C.........................  Loop through time zones
                        DO J = TZMIN, TZMAX
                   
C.............................  Adjust time zone J based on output time zone and account
C                               for 6 AM starting time in files
                            K = J - TZONE + 6
                   
                            FDATE = JDATE
                            FTIME = JTIME
                            CALL NEXTIME( FDATE, FTIME, -K * 10000 )

C.............................  Use date and time to find appropriate ef file
                            STPOS = SECSDIFF( EARLYDATE, 0, FDATE, 0 )
                            STPOS = STPOS / ( 24*3600 )
                            STPOS = STPOS + 1
                            
                            DO L = DAILY, EPISLEN
                                IF( USETIME( L ) .EQV. .FALSE. ) CYCLE
                                
                                IF( STPOS <= 0 .OR. STPOS > NDAYS ) THEN
                                    WRITE( *,* ) NDAYS
                                    MESG = 'ERROR: Invalid position'
                                    CALL M3EXIT( PROGNAME, FDATE, FTIME,
     &                                           MESG, 2 )
                                END IF
                                
                                CURFNM = EFLIST( EFDAYS( STPOS,L ) )
                                CURLNM = EFLOGS( EFDAYS( STPOS,L ) )

C.................................  Set logical file name
                                IF( .NOT. SETENVVAR( CURLNM, 
     &                                               CURFNM ) ) THEN
                                    EFLAG = .TRUE.
                                    MESG = 'ERROR: Could not set ' //
     &                                     'logical file name for ' //
     &                                     'file ' // CRLF() // BLANK10
     &                                     // '"' // TRIM( CURFNM ) // 
     &                                     '".'
                                    CALL M3EXIT( PROGNAME, FDATE, FTIME,
     &                                           MESG, 2 )
                                END IF

C.................................  Open current file
                                IF( .NOT. OPENSET( CURLNM, FSREAD3, 
     &                                             PROGNAME ) ) THEN
                                    EFLAG = .TRUE.
                                    MESG = 'ERROR: Could not open ' //
     &                                     'emission factors file ' //
     &                                     CRLF() // BLANK10 // '"' //
     &                                     TRIM( CURFNM ) // '".'
                                    CALL M3EXIT( PROGNAME, FDATE, FTIME,
     &                                           MESG, 2 )
                                END IF

C.................................  Read file description
                                IF( .NOT. DESCSET( CURLNM, 
     &                                             ALLFILES ) ) THEN
                                    MESG = 'ERROR: Could not get ' //
     &                                     'description for file ' //
     &                                     CRLF() // BLANK10 // '"' // 
     &                                     TRIM( CURFNM ) // '".'
                                    CALL M3EXIT( PROGNAME, FDATE, FTIME, 
     &                                           MESG, 2 )
                                END IF

C.................................  Read emission factors from current file
                                IF( .NOT. READSET( CURLNM, CBUF,ALLAYS3, 
     &                                           ALLFILES, SDATE3D, 
     &                                           FTIME, TEMPEF ) ) THEN
                                    EFLAG = .TRUE.
                                    MESG = 'Error reading "'// 
     &                                     CBUF(1:L1) //
     &                                     '" from file ' // 
     &                                     CRLF() // BLANK10 // '"' // 
     &                                     TRIM( CURFNM ) // '."'
                                    CALL M3EXIT( PROGNAME, FDATE, FTIME,
     &                                           MESG, 2 )
                                END IF
                
C.................................  Store emission factors by source                            
                                DO S = 1, NSRC
                            
C.....................................  Skip sources that are outside the grid or
C                                       don't use emission factors
                                    IF( EFIDX( S ) == -9 .OR. 
     &                                  EFIDX( S ) == -1 ) THEN
                                        EMFAC( S,I ) = 0
                                        CYCLE
                                    END IF

                                    IF( TZONES( S ) == J .AND. 
     &                                  STR2INT(EFTYPE( S )) == L ) THEN
                                        EMFAC( S,I ) = 
     &                                       TEMPEF( EFIDX( S ) )
                                    END IF
                                END DO   ! end source loop

                            END DO   ! end time period loop

                        END DO   ! end time zone loop

C.........................  If there are any missing values in the data, give an
C                           error to avoid problems in genhemis routine
                        RTMP = MINVAL( EMFAC( 1:NSRC,I ) )
                        IF( RTMP == IMISS3 ) THEN
                            EFLAG = .TRUE.
                            MESG = 'ERROR: Missing emission ' //
     &                         'factors(s) for "'// CBUF( 1:L1 ) // '".'
                            CALL M3MSG2( MESG )
                        END IF
                    END DO  ! End loop on pollutants/emission-types I in this group

C.....................  Abort if error found
                    IF( EFLAG ) THEN
                        MESG = 'Problem with emission factors.'
                        CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
                    END IF
 
                END IF

C.................  Generate hourly emissions for current hour
                CALL GENHEMIS( NGSZ, JDATE, JTIME, TZONE, DNAME, HNAME, 
     &                         ALLIN2D( 1,N ), EANAM2D( 1,N ), 
     &                         EMAC, EMFAC, EMACV, TMAT, EMIST )

C.................  Loop through pollutants/emission-types in this group

                DO I = 1, NGSZ

                    CBUF = EANAM2D( I,N )

C.....................  Skip blanks that can occur when NGRP > 1
                    IF ( CBUF .EQ. ' ' ) CYCLE

C.....................  Write hourly emissions to I/O API NetCDF file
!                    IF( .NOT. WRITESET( TNAME, CBUF, ALLFILES, JDATE,
!     &                                  JTIME, EMIST( 1,I ) )     ) THEN



		       
!                        L = LEN_TRIM( CBUF )
!                        MESG = 'Could not write "' // CBUF( 1:L ) // 
!     &                         '" to file "' // TNAME( 1:TNLEN ) // '."'
!                        CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

!EMIST                    END IF

                END DO  ! End loop on pollutants/emission-types I in this group

C.................  Advance the output date/time by one time step
!                CALL NEXTIME( JDATE, JTIME, TSTEP )

C.................  Call QA report routine
c               WFLAG = ( T .EQ. NSTEPS )
c               CALL QATMPR( LDEV, NGSZ, T, JDATE, JTIME, WFLAG, 
c    &                       EANAM2D( 1,N ), EMAC )

!            END DO      ! End loop on time steps T

        END DO          ! End loop on pollutant groups N

C.........  Exit program with normal completion
!        CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )
         
!         CALL M3MSG2(' Normal Completion of '//PROGNAME)

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats.............94xxx

94010   FORMAT( 10( A, :, I8, :, 1X ) )

94030   FORMAT( I3 )

94031   FORMAT( 8X, 'at time ', A8 )

94050   FORMAT( A, 1X, I2.2, A, 1X, A, 1X, I6.6, 1X,
     &          A, 1X, I3.3, 1X, A, 1X, I3.3, 1X, A   )
     
94070   FORMAT( A, I5, A )

        END SUBROUTINE TEMPORAL

