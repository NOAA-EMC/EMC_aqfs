
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/dscgrid.f,v 1.2 2000/11/28 21:22:39 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-2001 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        FUNCTION  DSCGRID( GNAME, CNAME,
     &              CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &              XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK )

C***********************************************************************
C  function body starts at line  129
C  entry DSCOORD starts at line  333
C
C  FUNCTION:
C
C    Routine returns grid descriptions.  Encapsulates management
C    of grid descriptions file GRIDDESC.
C
C  PRECONDITIONS 
C    File with logical name GRIDDESC exists and contains correctly-
C    formatted coordinate system descriptions and grid descriptions.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  REVISION HISTORY:
C
C       7/94  Prototype version by CJC
C       8/2001:  Bug fix by CJC to correct handling if GRIDDESC file
C       does not exist
C
C***********************************************************************

        IMPLICIT NONE
        
        INCLUDE 'PARMS3.EXT'

C...........   ENTRY Types 
        
        LOGICAL DSCGRID, DSCOORD
        
        
C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*) GNAME	!  grid  sys name
        CHARACTER*(*) CNAME	!  coord sys name
        INTEGER       CTYPE	!  coord sys type
        REAL*8        P_ALP	!  first, second, third map
        REAL*8        P_BET	!  projection descriptive
        REAL*8        P_GAM	!  parameters
        REAL*8        XCENT	!  lon for coord-system X=0
        REAL*8        YCENT	!  lat for coord-system Y=0
        REAL*8        XORIG	!  X-coordinate origin of grid (map units)
        REAL*8        YORIG	!  Y-coordinate origin of grid
        REAL*8        XCELL	!  X-coordinate cell dimension
        REAL*8        YCELL	!  Y-coordinate cell dimension
        INTEGER       NCOLS	!  number of grid columns
        INTEGER       NROWS	!  number of grid rows
        INTEGER       NTHIK	!  BOUNDARY:  perimeter thickness (cells)
        

C...........   PARAMETERS and their descriptions:

        INTEGER       MXCORD3   !  max number of coord systems in GRIDDESC
        INTEGER       MXGRDS3   !  max number of coord systems in GRIDDESC
        CHARACTER*16  GRIDDESC  !  logical name for grid desc file

        PARAMETER ( MXCORD3 = 32 , 
     &              MXGRDS3 = 256 ,
     &              GRIDDESC = 'GRIDDESC' )

            
C...........   EXTERNAL FUNCTIONS:
        
        INTEGER      INDEX1, GETEFILE, TRIMLEN
        EXTERNAL     INDEX1, GETEFILE, TRIMLEN
        

C...........   SAVED LOCAL VARIABLES and their descriptions:
              
        LOGICAL      FIRSTIME
        DATA         FIRSTIME / .TRUE. /
        
        INTEGER      NCORDS	!  number of coord sys defs in GRIDDEFS
        INTEGER      NGRIDS     !  number of grid      defs in GRIDDEFS
        
        CHARACTER*16  CNAMES( MXCORD3 )
        INTEGER       CTYPES( MXCORD3 )
        REAL*8        P_ALPS( MXCORD3 )
        REAL*8        P_BETS( MXCORD3 )
        REAL*8        P_GAMS( MXCORD3 )
        REAL*8        XCENTS( MXCORD3 )
        REAL*8        YCENTS( MXCORD3 )
        
        CHARACTER*16 GNAMES( MXGRDS3 )
        CHARACTER*16 CORDSS( MXGRDS3 )
        REAL*8       XORIGS( MXGRDS3 )
        REAL*8       YORIGS( MXGRDS3 )
        REAL*8       XCELLS( MXGRDS3 )
        REAL*8       YCELLS( MXGRDS3 )
        INTEGER      NCOLSS( MXGRDS3 )
        INTEGER      NROWSS( MXGRDS3 )
        INTEGER      NTHIKS( MXGRDS3 )

        SAVE         FIRSTIME, NCORDS, NGRIDS,
     &               CNAMES, CTYPES, P_ALPS, P_BETS, P_GAMS, 
     &               XCENTS, YCENTS, 
     &               GNAMES, CORDSS, XORIGS, YORIGS, XCELLS, YCELLS, 
     &               NCOLSS, NROWSS, NTHIKS
        
C...........   Scratch local variables and their descriptions:
            
        INTEGER        IUNIT    !  unit number for GRIDDESC
        CHARACTER*160  MESG   !  for m3warn()
        CHARACTER*160  NAMBUF   !  for nameval()
        CHARACTER*16   ANAME	!  name read from file
        CHARACTER*16   GRID16	!  local copy of grid name
        CHARACTER*16   CORD16	!  local copy of coordinate system name
        INTEGER        IOS      !  I/O status return
        INTEGER        IREC     !  record number
        INTEGER        I, J     !  indexes for names in lists
        LOGICAL        EFLAG    !  true iff from entry DSCOORD

C............................................................................
C.......   begin body of routine DSCGRID:

        IF ( TRIMLEN( GNAME ) .GT. 16 ) THEN
            WRITE( MESG,94020 )
     &          'Grid "', GNAME, '" Max name length 16; actual:', 
     &          TRIMLEN( GNAME )
            CALL M3WARN( 'DSCGRID', 0, 0, MESG )
            DSCGRID = .FALSE.
            RETURN
        END IF          !  if len( gname ) > 16, or if len( vname ) > 16

        GRID16 = GNAME   !  fixed-length-16 scratch copy of name
        EFLAG  = .FALSE.
        
1       CONTINUE	!  target of initialization from entry DSCOORD
        
        IF ( FIRSTIME ) THEN
        
            IUNIT = GETEFILE( GRIDDESC, .TRUE., .TRUE., 'DESCGRID' )
            IF ( IUNIT .LT. 0 ) THEN
                CALL NAMEVAL( GRIDDESC, NAMBUF )
                CALL M3WARN( 'DSCGRID', 0, 0, 
     &                       'Could not open GRIDDESC file' )
                MESG = 'Path name "' // 
     &                    NAMBUF( 1:TRIMLEN( NAMBUF ) ) //
     &                   '"'
                CALL M3MESG( MESG )
                DSCGRID = .FALSE.
                RETURN

            END IF	!  if getefile() failed

C...............   Read and discard coord sys chunk header:
                
            READ( IUNIT,93010, IOSTAT = IOS ) ANAME
            IF( IOS .NE. 0 ) THEN
                WRITE( MESG,94010 ) 
     &          'Error ', IOS, ' reading coord header at line 1'
                CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                DSCGRID = .FALSE.
                RETURN
            END IF
                
            NCORDS = 0
            NGRIDS = 0
            IREC   = 1
            
11          CONTINUE	!  head of loop reading coord sys segment of GRIDDEFS
                    
                READ( IUNIT,*, IOSTAT = IOS ) ANAME
                IREC = IREC + 1
                IF( IOS .NE. 0 ) THEN
                    WRITE( MESG,94010 ) 
     &                  'Error ', IOS, 
     &                  'reading coord names at line', IREC
                    CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                    DSCGRID = .FALSE.                    
                    RETURN
                END IF
                
                IF ( ANAME( 1:1 ) .NE. ' ' ) THEN
                    
                    NCORDS = NCORDS + 1
                    IF ( NCORDS .GT. MXCORD3 ) THEN
                        WRITE( MESG,94010 )
     &                      'Max number', MXCORD3, 
     &                      'of coord systems exceeded at line', IREC
                        CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                        DSCGRID = .FALSE.
                        RETURN
                    END IF
                    
                    CNAMES( NCORDS ) = ANAME
                    READ( IUNIT,*, IOSTAT = IOS )  
     &                  CTYPES( NCORDS ),
     &                  P_ALPS( NCORDS ),
     &                  P_BETS( NCORDS ),
     &                  P_GAMS( NCORDS ),
     &                  XCENTS( NCORDS ),
     &                  YCENTS( NCORDS )
                
                    IREC = IREC + 1
                    IF( IOS .NE. 0 ) THEN
                        WRITE( MESG,94010 ) 
     &                      'Error', IOS, 
     &                      'reading coord descriptions at line', IREC
                        CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                        DSCGRID = .FALSE.
                        RETURN
                    END IF
                
                    GO TO  11	!  to head of loop reading coord sys's

                END IF		!  if aname nonblank
                
            
22          CONTINUE	!  head of loop reading grids segment of GRIDDEFS
                    
                READ( IUNIT,*, END = 23, IOSTAT = IOS ) ANAME
                IREC = IREC + 1
                IF( IOS .NE. 0 ) THEN
                    WRITE( MESG,94010 ) 
     &                  'Error', IOS, 
     &                  'reading grid names at line', IREC
                    CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                    DSCGRID = .FALSE.
                    RETURN
                END IF
                
                IF ( ANAME( 1:1 ) .NE. ' ' ) THEN
                    
                    NGRIDS = NGRIDS + 1
                    IF ( NGRIDS .GT. MXGRDS3 ) THEN
                        WRITE( MESG,94010 )
     &                      'Max number', MXGRDS3,
     &                      'of grids exceeded at line', IREC
                        CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                        DSCGRID = .FALSE.
                        RETURN
                    END IF
                    
                    GNAMES( NGRIDS ) = ANAME
                    READ( IUNIT,*, IOSTAT = IOS )
     &                    CORDSS( NGRIDS ),
     &                    XORIGS( NGRIDS ),
     &                    YORIGS( NGRIDS ),
     &                    XCELLS( NGRIDS ),
     &                    YCELLS( NGRIDS ),
     &                    NCOLSS( NGRIDS ),
     &                    NROWSS( NGRIDS ),
     &                    NTHIKS( NGRIDS )
                    IREC = IREC + 1
                
                    IF( IOS .NE. 0 ) THEN
                        WRITE( MESG,94010 ) 
     &                      'Error', IOS, 
     &                      'reading grid descriptions at line', IREC
                        CALL M3WARN( 'DSCGRID', 0, 0, MESG )
                        DSCGRID = .FALSE.
                        RETURN
                    END IF
                
                    GO TO  22	!  to head of loop reading grids

                END IF		!  if aname nonblank

23              CONTINUE
                            
            FIRSTIME = .FALSE.

        END IF          !  if firstime (end of initialization)
        
        IF ( EFLAG )  GO TO  1000	!  back to entry DSCOORD
        
        
        I = INDEX1( GRID16, NGRIDS, GNAMES )
        
        IF ( I .GT. 0 ) THEN	!  grid name found at index I
            
            XORIG = XORIGS( I )
            YORIG = YORIGS( I )
            XCELL = XCELLS( I )
            YCELL = YCELLS( I )
            NCOLS = NCOLSS( I )
            NROWS = NROWSS( I )
            NTHIK = NTHIKS( I )
            CNAME = CORDSS( I )
        
            J = INDEX1( CORDSS( I ), NCORDS, CNAMES )
            
            IF ( J .GT. 0 ) THEN
                   
                CTYPE = CTYPES( J )
                P_ALP = P_ALPS( J )
                P_BET = P_BETS( J )
                P_GAM = P_GAMS( J )
                XCENT = XCENTS( J )
                YCENT = YCENTS( J )
                   
            ELSE        
            
                CTYPE = IMISS3
                P_ALP = BADVAL3
                P_BET = BADVAL3
                P_GAM = BADVAL3
                XCENT = BADVAL3
                YCENT = BADVAL3
                   
                MESG = 'Missing coord system "' //
     &               CORDSS( I )( 1:TRIMLEN( CORDSS( I ) ) ) //
     &               '" for grid "' //
     &               GNAME( 1:TRIMLEN( GNAME ) ) //
     &               '" in GRIDDEFS'
                CALL M3WARN( 'DSCGRID', 0, 0, MESG )

            END IF	!  coord sys name found
            
        END IF	!  grid name found or not
        
        DSCGRID = ( I .GT. 0 )
        RETURN


C......................................................................
C...................   end entry dscgrid; begin entry dscoord  ........
        
        ENTRY DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT )
        
        IF ( LEN( CNAME ) .GT. 16 ) THEN
            WRITE( MESG,94020 )
     &          'Coord sys "', CNAME, 
     &          '" Max name length 16; actual:', 
     &          LEN( CNAME )

            CALL M3WARN( 'DSCOORD', 0, 0, MESG )
            DSCGRID = .FALSE.
            RETURN
        END IF          !  if len( gname ) > 16, or if len( vname ) > 16

        CORD16 = CNAME   !  fixed-length-16 scratch copy of name


        EFLAG = .TRUE.			!  if initialization necessary,
        IF ( FIRSTIME ) GO TO 1		!  set EFLAG to true and go to
1000    CONTINUE			!  initialization section of DSCGRID
            
        J = INDEX1( CORD16, NCORDS, CNAMES )
        
        IF ( J .GT. 0 ) THEN
               
            CTYPE = CTYPES( J )
            P_ALP = P_ALPS( J )
            P_BET = P_BETS( J )
            P_GAM = P_GAMS( J )
            XCENT = XCENTS( J )
            YCENT = YCENTS( J )
               
        END IF	!  coord sys name found
        
        DSCOORD = ( J .GT. 0 )
        RETURN
        
                
C...............   Format statements  ..................................
                
93010   FORMAT( A )
                
94010   FORMAT( A, I7, :, 2X, A, :, I7 )

94020   FORMAT( 3A, I7 )

        END

