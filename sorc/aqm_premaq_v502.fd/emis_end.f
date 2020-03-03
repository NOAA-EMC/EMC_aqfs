
        SUBROUTINE emis_end

C***********************************************************************
C  program body starts at line  187
C
C  DESCRIPTION:
C       writes output for emissions processing after time loop is completed
C       based on beis311 code.
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
C...........   Modules for public variables
C...........   This module contains the speciation profile tables

        USE MODSPRO

C...........   This module contains BEIS3 specific arrays
        USE MODBEIS3V11

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'      ! I/O API constants
        INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
        INCLUDE 'IODECL3.EXT'     ! I/O API function declarations
        INCLUDE 'EMCNST3.EXT'     !
        INCLUDE 'B3V11DIMS3.EXT'     ! biogenic-related constants

C...........   PARAMETERS and their descriptions:

        CHARACTER*50, PARAMETER :: CVSW = '$Name:  $' ! CVS release tag
        
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         ENVINT 
        LOGICAL         ENVYN
        CHARACTER*50    GETCFDSC
        INTEGER         GETFLINE
        CHARACTER*10    HHMMSS
        INTEGER         INDEX1

        CHARACTER*16    PROMPTMFILE
        INTEGER         PROMPTFFILE
        INTEGER         TRIMLEN
        CHARACTER*16    VERCHAR

        EXTERNAL        ENVINT, ENVYN, GETFLINE, HHMMSS, INDEX1, PROMPTMFILE,  
     &                  PROMPTFFILE, TRIMLEN, VERCHAR


C...........   Other variables and their descriptions:

        INTEGER         B, M    !  counters for biogenic, model species
        INTEGER         I, J, K, L, N, C, R  !  loop counters and subscripts

C  local variables to emis_end
        INTEGER         TDATE, TTIME   ! temp storage of date and time 


        CHARACTER*256   EQNAME    ! env var of filename

        CHARACTER*300   MESG      !  message buffer for M3EXIT()

        CHARACTER*16 :: PROGNAME = 'EMIS_END'   !  program name

C***********************************************************************
C   begin body of program EMIS_END




C.........  write information for NO emissions for future restart
!        variables to write: PTYPE, PULSEDATE, PULSETIME
!
!****** set up IOAPI headers
!      
C.......   Build description for, and create/open output file

        TSTEP3D = 10000
        TDATE = JDATE
        TTIME = JTIME

        CALL NEXTIME(TDATE,TTIME,-TSTEP3D)


        NCOLS3D = NCOLS
        NROWS3D = NROWS
        NLAYS3D = 1
        SDATE3D = TDATE
        STIME3D = TTIME
        MXREC3D = 1
        NVARS3D = 3+RHOURS
        

        VNAME3D( 1 ) = 'PTYPE'
        VNAME3D( 2 ) = 'PULSEDATE'
        VNAME3D( 3 ) = 'PULSETIME'
        do i_loop = 4,4+RHOURS-1
           if (i_loop-3 .le. 9) then
              write (ichar,'(I1)') i_loop-3
              VNAME3D( i_loop ) = 'RAINFALL'//'0'//ichar
           else
              write (i2char,'(I2)') i_loop-3
              VNAME3D( i_loop ) = 'RAINFALL'//i2char
           endif

        enddo



        UNITS3D( 1 ) = 'NUMBER'
        UNITS3D( 2 ) = 'M3DATE'
        UNITS3D( 3 ) = 'M3TIME'
        UNITS3D( 4:4+RHOURS-1 ) = 'CM'

        VDESC3D( 1 ) = 'NO EMISSION PULSE TYPE'
        VDESC3D( 2 ) = 'MODELS-3 STARTING DATE FOR NO EMISSION PULSE'
        VDESC3D( 3 ) = 'MODELS-3 STARTING TIME FOR NO EMISSION PULSE' 
        VDESC3D(4:4+RHOURS-1) = 'TOTAL RAINFALL for 6 HOURS'

        VTYPE3D( 1 ) = M3REAL
        VTYPE3D( 2 ) = M3REAL
        VTYPE3D( 3 ) = M3REAL
        VTYPE3D(4:4+RHOURS-1) = M3REAL

        FDESC3D = ' '   ! array

        FDESC3D( 1 ) = 'Gridded NO emission storage'
        FDESC3D( 2 ) = '/FROM/ '    // PROGNAME
        FDESC3D( 3 ) = '/VERSION/ ' // VERCHAR( CVSW )
        FDESC3D( 4 ) = '/TZONE/ '   // CTZONE
        FDESC3D( 5 ) = '/LANDUSE/ ' // LUSE
        FDESC3D( 6 ) = '/MET SCENARIO/ ' // METSCEN
        FDESC3D( 7 ) = '/CLOUD SCHEME/ ' // CLOUDSHM                   
!
!******* open file
!


            SOILOUT = PROMPTMFILE( 
     &          'Enter name for NO EMISSIONS SOIL  file',
     &          FSUNKN3, 'SOILOUT', PROGNAME )




!
!******* write output
!

 
           IF ( .NOT. WRITE3( SOILOUT, 'PTYPE', 
     &                        TDATE, TTIME, REAL(PTYPE) ) ) THEN
               CALL M3EXIT( PROGNAME, TDATE, TTIME, 
     &                      'Error writing BEIS3 SOILOUT file' , 2 )
           END IF  
           IF ( .NOT. WRITE3( SOILOUT, 'PULSEDATE', 
     &                        TDATE, TTIME, REAL(PULSEDATE) ) ) THEN
               CALL M3EXIT( PROGNAME, TDATE, TTIME, 
     &                      'Error writing BEIS3 SOILOUT file' , 2 )
           END IF  
           IF ( .NOT. WRITE3( SOILOUT, 'PULSETIME', 
     &                        TDATE, TTIME, REAL(PULSETIME) ) ) THEN
               CALL M3EXIT( PROGNAME, TDATE, TTIME, 
     &                      'Error writing BEIS3 SOILOUT file' , 2 )
           END IF  

           do i_loop = 1,RHOURS


              if (i_loop .le. 9) then
                  write (ichar,'(I1)') i_loop        
                  vname = 'RAINFALL'//'0'//ichar
              else
                  write (i2char,'(I2)') i_loop
                  vname = 'RAINFALL'//i2char
              endif


              IF ( .NOT. WRITE3( SOILOUT, vname, 
     &                        TDATE, TTIME, rainfall(1,1,i_loop) ) ) 
     &              THEN
                 CALL M3EXIT( PROGNAME, TDATE, TTIME, 
     &                      'Error writing BEIS3 SOILOUT file' , 2 )
              END IF 
           enddo 


!           CLOSE (RDEV)
!           DEALLOCATE (SPCNAMES)
!           DEALLOCATE (MOLUNITS)
!           DEALLOCATE (AVGEMIS)
!           DEALLOCATE (NOEMIS)
!           DEALLOCATE (AVGLAI)

C.........   End of program:

!AQF        CALL M3EXIT( PROGNAME, 0, 0, ' ', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( I2.2 )
94010   FORMAT( 10( A, :, I8, :, 1X ) )

94030   FORMAT( 8X, 'at time ', A8 )

        END SUBROUTINE EMIS_END 

