C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/air4/yoj/rel/models/PARIO/src/wrsubmap.f,v 1.1.1.1 2003/05/05 17:18:09 yoj Exp $

      SUBROUTINE WRSUBMAP ( NUMPROCS, NROWS_PE, NCOLS_PE, 
     &                      ROWSX_PE, COLSX_PE )
C.....................................................................
 
C  PURPOSE:  Print a table to the log file showing the processor-to-subdomain
C            map.
 
C  REVISION HISTORY: 
C       Original version  3/96 by Al Bourgeois for parallel implementation.
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C        INTEGER    NUMPROCS               ! Number of processors.
C        INTEGER    NROWS_PE(0:*)          ! Number of rows for each PE.
C        INTEGER    NCOLS_PE(0:*)          ! Number of columns for each PE.
C        INTEGER    ROWSX_PE(2, 0:*)       ! Row index range for each PE.
C        INTEGER    COLSX_PE(2, 0:*)       ! Column index range for each PE.
C  OUT:  none
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: WRITE
 
C........................................................................
 
      IMPLICIT  NONE
 
C ARGUMENTS:

      INTEGER    NUMPROCS               ! Number of processors.
      INTEGER    NROWS_PE( 0:* )        ! Number of rows for each PE.
      INTEGER    NCOLS_PE( 0:* )        ! Number of columns for each PE.
      INTEGER    ROWSX_PE( 2,0:* )      ! Row index range for each PE.
      INTEGER    COLSX_PE( 2,0:* )      ! Column index range for each PE.

C LOCAL VARIABLES: 

      INTEGER    I
C........................................................................
C     begin  WRSUBMAP

      WRITE( 6,* )
      WRITE( 6,* ) '     -=-  MPP Processor-to-Subdomain Map  -=-       '
      WRITE( 6,* ) ' ___________________________________________________'
      WRITE( 6,* ) ' |                                                 |'
      WRITE( 6,* ) ' | PE     #Rows   Row_Range      #Cols   Col_Range |'
      WRITE( 6,* ) ' |_________________________________________________|'
      WRITE( 6,* ) ' |                                                 |'    
      DO I = 0, NUMPROCS-1
         WRITE( 6,1003 ) I, NROWS_PE(I), ROWSX_PE(1,I), ROWSX_PE(2,I),
     &                      NCOLS_PE(I), COLSX_PE(1,I), COLSX_PE(2,I)
      END DO
      WRITE( 6,* ) ' |_________________________________________________|'
      WRITE( 6,* )

      RETURN

1003  FORMAT('  |', I3, 5X, I4, 3X, I4, ':', I4, 
     &                  7X, I4, 4X, I4, ':', I4, ' |')

      END
