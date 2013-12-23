
C.........................................................................
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL   FUNCTION  CHECK3 ( FNAME, VNAME, JDATE, JTIME )

C***********************************************************************
C  function body starts at line  104
C
C  FUNCTION:  returns TRUE iff time step for (JDATE,JTIME) and variable
C       VNAME is available in file with logical name FNAME.  If FNAME 
C       is time independent or is a dictionary file, only VNAME is 
C       significant and the routine returns the availability of the
C       variable or file description for VNAME.
C
C  PRECONDITIONS REQUIRED:  
C       FNAME already opened by OPEN3().
C       VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:  
C       prototype 3/1992 by CJC
C
C       Modified  7/1994 by CJC to handle restart (circular-buffer) files
C
C       modified 10/1994 by CJC to work with variable-level-granularity
C       WRITE3() 
C
C       modified 19/1999 by C.Coats for thread-safety, A.Trayanov for
C       coupling-mode operation
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes; uses RDTFLAG()
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME   !  file name for query
        CHARACTER*(*)   VNAME   !  vble name for query
        INTEGER         JDATE   !  date (YYYYDDD) for query
        INTEGER         JTIME   !  time (HHMMSS) for query


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INDEX1     !  look up names in name tables
        INTEGER         INIT3      !  initialize I/O system files.
        INTEGER         NAME2FID   !  fname~~> fid lookup
        LOGICAL         RDTFLAG    !  check time step record availability
        INTEGER         TRIMLEN    !  trimmed string length

        EXTERNAL        INDEX1, INIT3, NAME2FID, RDTFLAG, TRIMLEN
        EXTERNAL        INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         STEP            !  time step record number
        INTEGER         IERR            !  netCDF error status return
        INTEGER         VID             !  subscript for vble in state arrays
        INTEGER         VLEN            !  name lengths for file, vble
        LOGICAL         EFLAG
        CHARACTER*256   MESG

#ifdef IOAPICPL
        INTEGER       COUNT, SKIP, TYPE
        INTEGER       F1, F2
        LOGICAL       VFLAG
        REAL          BUFFER(1)
        LOGICAL       READ3V
        EXTERNAL      READ3V
#endif


C***********************************************************************
C   begin body of function  CHECK3

        FID   = NAME2FID( FNAME )
        EFLAG = ( FID .LE. 0 )

        VLEN  = TRIMLEN( VNAME )

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME // '" Variable "'// VNAME //'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual: ', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'CHECK3', JDATE, JTIME, MESG )
	    CHECK3 = .FALSE.
            RETURN
        END IF
        

C.......   Find netCDF index for the file, and check time step availability:

        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN
            
            VID  = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )
            
        ELSE IF ( VNAME .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE

            VID = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )
            IF ( VID .EQ. 0 ) THEN
                CHECK3 = .FALSE.
                MESG = 'Variable "' // VNAME //
     &                 '" not in file "' // FNAME
                CALL M3WARN( 'CHECK3', JDATE, JTIME, MESG )
                RETURN
            END IF

        END IF              !  if FID OK; else if "all variables", or not
         
        
        IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"
           
#ifdef IOAPICPL
            IF ( TSTEP3( FID ) .NE. 0 ) THEN
                F1    = JDATE
                F2    = JTIME
            ELSE
                F1    = 0
                F2    = 0
            END IF

            COUNT = 0
            SKIP  = 0
            IF ( VID .EQ. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                DO VID = 1, NVARS3(FID)
                    TYPE  = VTYPE3(VID,FID)
                    VFLAG = READ3V( PLIST3(FID), VLIST3(VID,FID),
     $                              SKIP, F1, F2, BUFFER, COUNT, TYPE )
                    EFLAG = ( EFLAG .AND. VFLAG )
                END DO
                CHECK3 = EFLAG
            ELSE
                TYPE   = VTYPE3(VID,FID)
                CHECK3 = READ3V( PLIST3(FID), VNAME, SKIP,
     $                           F1, F2, BUFFER, COUNT, TYPE )
            END IF
#endif
#ifndef IOAPICPL
            CALL M3WARN( 'CHECK3', JDATE, JTIME, 
     &                   'Coupling mode not active' )
            CHECK3 = .FALSE.
#endif

        ELSE

            CHECK3 = RDTFLAG( FID, VID, JDATE, JTIME, STEP )

        END IF  

        RETURN

        END

