C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C 2003 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL FUNCTION  FLUSH3 ( FNAME )

C***********************************************************************
C  function  FLUSH3   starts at line   75
C
C  FUNCTION:
C       Flushes I/O API file with logical name FNAME.
C
C  RETURN VALUE:
C       TRUE iff it succeeds.
C
C  PRECONDITIONS REQUIRED:
C       I/O API already initialized.
C       File with logical name FNAME exists and has been opened
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       NAME2FID
C       SYNCFID
C
C  REVISION  HISTORY:  
C       prototype 8/1995 by CJC
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1998 by CJC:  removed unused local variable "V"
C
C       Modified 10/2003 by CJC for I/O API version 3:
C       Structure in terms of new LOGICAL SYNCFID, INTEGER NAME2FID;
C       support for native-binary BINFILE3 and LISTFIL3 file types
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME   !  logical name of file to be opened


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INDEX1          !  look up names in tables
        INTEGER         NAME2FID        !  fname~~> fid lookup
        INTEGER         TRIMLEN
        LOGICAL         SYNCFID
        EXTERNAL        INDEX1, NAME2FID, TRIMLEN, SYNCFID


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  file index
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of function  FLUSH3

C.......   Find STATE3 index for the file; then call SYNCFID:

        FILE = NAME2FID( FNAME )

        IF ( FILE .EQ. 0 ) THEN !  file not open.
            MESG = 'FLUSH3:  invalid file "' // FNAME // '"'
            CALL M3MSG2( MESG )
            FLUSH3 = .FALSE.
            RETURN
        END IF

        FLUSH3 = SYNCFID( FILE )
        RETURN

        END

