
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/wrcustom.f,v 1.2 2000/11/28 21:23:09 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        LOGICAL   FUNCTION WRCUSTOM( FID, VID, TSTAMP, STEP2, BUFFER )

C***********************************************************************
C  function body starts at line  70
C
C  FUNCTION:  writes data from Models-3 CUSTOM data file with STATE3
C             index FID, for alll variables and layers, for time step
C             record STEP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is CUSTOM3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
C
C  REVISION  HISTORY:  
C       prototype 3/92 by CJC
C       revised  10/94 by CJC:  allow write-by-variable; record 
C       	time-step number as time step flag; restart files.
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER         FID             !  file index within the STATE3 commons
        INTEGER         VID             !  vble index within the STATE3 commons
        INTEGER         TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER         STEP2           !  file record number (maybe mod 2)
        REAL            BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         WRVARS     !  write "variables" part of timestep record
        EXTERNAL        WRVARS
        EXTERNAL        INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()


C***********************************************************************
C   begin body of function  WRCUSTOM

C.......   Set up args for WRVARS:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NLAYS3( FID )

        DIMS ( 3 ) = STEP2
        DELTS( 3 ) = 1

        DELTA = NCOLS3( FID ) * NLAYS3( FID )

C...........   Perform the writes:

        WRCUSTOM = WRVARS( FID, VID, TSTAMP, STEP2, 
     &                     DIMS, DELTS, DELTA, BUFFER )

        RETURN

        END

