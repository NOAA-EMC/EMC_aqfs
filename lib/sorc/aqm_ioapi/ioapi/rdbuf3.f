
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      LOGICAL FUNCTION RDBUF3( FID, VID, LAYER, JDATE, JTIME, BUFFER )

C***********************************************************************
C  function body starts at line  80
C
C  FUNCTION:  reads data from Models-3 BUFFEREd "file" with M3 file
C       index FID for variable with name VNAME and layer LAYER, 
C       for the date and time JDATE (coded YYYYDDD) JTIME (HHMMSS).
C       For time-independent files, JDATE:JTIME are ignored.
C       If VNAME is 'ALL', reads all variables; if LAYER is -1,
C       reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  FNAME is a Models-3 BUFFERED "file" already 
C       opened by CREATE3().  Should only be called via READ3().
C       For ALLLVARS3 reads, all variables must be of type M3REAL
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       JSTEP3, BUFGET3
C
C  REVISION  HISTORY:  
C       Prototype 7/1994 by CJC
C
C	Modified 10/1994 by CJC to work with WRITE3() having granularity
C	at the level of individual variables.
C
C       Modified 5/2002 to support types other than REAL
C
C       Modified 5/2003 by CJC: bugfix by David Wong, US EPA -- wrong
C       arguments to the BUFGET*()
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'

C...........   ARGUMENTS and their descriptions:

        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         VID             !  subscript for STATE3 arrays
        INTEGER         LAYER           !  layer number, or 0
        INTEGER         JDATE           !  date, formatted YYYYDDD
        INTEGER         JTIME           !  time, formatted HHMMSS
        REAL            BUFFER(*)       !  input buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         JSTEP3     !  compute time step record numbers
        INTEGER         BUFGET3, BUFGET3D, BUFGET3I

        EXTERNAL        JSTEP3, BUFGET3, BUFGET3D, BUFGET3I


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         STEP            !  time step record number
        INTEGER         VAR             !  loop counter:  variables
        INTEGER         SIZE
        CHARACTER*256   MESG


C***********************************************************************
C   begin body of function  RDBUF3

        IF ( LAYER .GT. 0 ) THEN
           SIZE = BSIZE3( FID )
        ELSE
            SIZE = BSIZE3( FID ) * NLAYS3( FID )
        END IF

        IF ( VID .GT. 0 ) THEN          !  read just this variable
        
            IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                   
                IF( LDATE3( VID, FID ) .EQ. 0 ) THEN
                    STEP = ILAST3( VID,FID )
                ELSE
                    RDBUF3 = .FALSE.
                    RETURN
                END IF
                
            ELSE IF ( JDATE .EQ. LDATE3( VID,FID ) .AND. 
     &                JTIME .EQ. LTIME3( VID,FID ) ) THEN
                
                STEP = ILAST3( VID,FID )
                
            ELSE IF ( JDATE .EQ. NDATE3( VID,FID ) .AND. 
     &                JTIME .EQ. NTIME3( VID,FID ) ) THEN
                
                STEP = 1 - ILAST3( VID,FID )        !  formula swaps 0 and 1
                
            ELSE
                
                CALL M3WARN( 'READ3/RDBUF3', JDATE, JTIME,                     
     &              'Date and time not available in '// FLIST3( FID ) )
                
                RDBUF3 = .FALSE.
                RETURN
                
            END IF
                
            IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
                RDBUF3 = ( 0 .NE. BUFGET3 ( FID, VID, LAYER,
     &                                      NLAYS3( FID ), SIZE, STEP,
     &                                      BUFFER ) ) 
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
                RDBUF3 = ( 0 .NE. BUFGET3I( FID, VID, LAYER,
     &                                      NLAYS3( FID ), SIZE, STEP,
     &                                      BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
                RDBUF3 = ( 0 .NE. BUFGET3D( FID, VID, LAYER,
     &                                      NLAYS3( FID ), SIZE, STEP,
     &                                      BUFFER ) )
            END IF

        ELSE                            !  read all variables
        
            DO  11  VAR = 1, NVARS3( FID )
                
                IF ( VTYPE3( VAR,FID ) .NE. M3REAL ) THEN
                    
                    MESG = 'ALLVAR3 non-REAL types not supported'
                    CALL M3WARN( 'READ3/RDBUF3', JDATE, JTIME, MESG )
                    RDBUF3 = .FALSE.
                    RETURN

                ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                       
                    IF( LDATE3( VAR, FID ) .EQ. 0 ) THEN
                        STEP = ILAST3( VAR,FID )
                    ELSE
                        RDBUF3 = .FALSE.
                        RETURN
                    END IF
                    
                ELSE IF ( JDATE .EQ. LDATE3( VAR,FID ) .AND. 
     &                    JTIME .EQ. LTIME3( VAR,FID ) ) THEN
                    
                    STEP = ILAST3( VAR,FID )
                    
                ELSE IF ( JDATE .EQ. NDATE3( VAR,FID ) .AND. 
     &                    JTIME .EQ. NTIME3( VAR,FID ) ) THEN
                    
                    STEP = 1 - ILAST3( VAR,FID )        !  formula swaps 0 and 1
                    
                ELSE
                    
                    CALL M3WARN( 'READ3/RDBUF3', JDATE, JTIME,                     
     &              'Date and time not available in '// FLIST3( FID ) )
                                        
                    RDBUF3 = .FALSE.
                    RETURN
                    
                END IF
                    
                IF ( 0 .EQ. BUFGET3( FID, VAR, LAYER, NLAYS3( FID ), 
     &                               BSIZE3( FID ), STEP,
     &                               BUFFER ) ) THEN
                    
                    RDBUF3 = .FALSE.
                    RETURN
                
                END IF
            
11          CONTINUE
            
            RDBUF3 = .TRUE.             !  (if you get to here)
                
        END IF                          !  read one vble, or read all vbles
        
        RETURN
        END

