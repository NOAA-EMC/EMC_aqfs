
        LOGICAL FUNCTION RDBNDARY( FID, VID, LAYER, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdbndary.f 45 2014-09-12 20:05:29Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  77
C
C  FUNCTION:  reads data from Models-3 GRDDED data file with state-variable
C             file index FID, for variable VID and layer LAYER, for the
C             time step record STEP.
C             If VID is -1 reads all variables; if LAYER is -1,
C             reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file, time step, and layer availability,
C             and that file type is BNDARY3
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C	prototype 3/1992 by CJC
C
C	modified  9/1994 by CJC:  VID argument, not VNAME
C
C       Modified 10/2003 by CJC for I/O API version 3:  RDVARS support for
C       native-binary BINFIL3 file type.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  variable index , or ALLAYS3
        INTEGER, INTENT(IN   ) :: LAYER           !  layer number, or 0
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
        EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         PERIM           !  2-D boundary-slice volume (cells)
        INTEGER         DELTA           !  d(INDX) / d( NCVGT call )
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()


C***********************************************************************
C   begin body of function  RDBNDARY

C.......   Set up DIMS and DELTS arguments for NCVGT(), according
C.......   to whether request is to read all layers:

        PERIM = 2 * NTHIK3( FID )
        PERIM = ABS( PERIM )*( NCOLS3( FID ) + NROWS3( FID ) + PERIM )

        DIMS ( 1 ) = 1
        DELTS( 1 ) = PERIM

        DIMS ( 3 ) = STEP
        DELTS( 3 ) = 1

        DIMS ( 4 ) = 0
        DELTS( 4 ) = 0

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0
        
        IF ( LAYER .EQ. ALLAYS3 ) THEN

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NLAYS3( FID )
            DELTA = PERIM * NLAYS3( FID )

        ELSE    !  read a specific layer

            DIMS ( 2 ) = LAYER
            DELTS( 2 ) = 1
            DELTA = PERIM

        END IF


C...........   Perform the reads, according to VID

        RDBNDARY = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER )

        RETURN

        END FUNCTION RDBNDARY

