C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/work/rep/PARIO/src/wrsubmap.f,v 1.6 2006/08/30 11:57:28 yoj Exp $

      SUBROUTINE WRSUBMAP ( NUMPROCS, NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE )
C.....................................................................
 
C  PURPOSE:  Print a table to the log file showing the processor-to-subdomain
C            map.
 
C  REVISION HISTORY: 
C       Original version  3/96 by Al Bourgeois for parallel implementation.
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C        INTEGER    NUMPROCS               ! Number of processors.
C        INTEGER    NCOLS_PE( * )          ! Number of columns for each PE.
C        INTEGER    NROWS_PE( * )          ! Number of rows for each PE.
C        INTEGER    COLSX_PE( 2,* )        ! Column index range for each PE.
C        INTEGER    ROWSX_PE( 2,* )        ! Row index range for each PE.
C  OUT:  none
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: WRITE
 
C........................................................................
 
      IMPLICIT  NONE
 
C ARGUMENTS:

      INTEGER    NUMPROCS               ! Number of processors.
      INTEGER    NCOLS_PE( * )          ! Number of columns for each PE.
      INTEGER    NROWS_PE( * )          ! Number of rows for each PE.
      INTEGER    COLSX_PE( 2,* )        ! Column index range for each PE.
      INTEGER    ROWSX_PE( 2,* )        ! Row index range for each PE.

C LOCAL VARIABLES: 

      INTEGER    I
C........................................................................

      WRITE( *,* )
      WRITE( *,* ) '     -=-  MPP Processor-to-Subdomain Map  -=-       '
      WRITE( *,* ) ' ___________________________________________________'
      WRITE( *,* ) ' |                                                 |'
      WRITE( *,* ) ' | PE     #Rows   Row_Range      #Cols   Col_Range |'
      WRITE( *,* ) ' |_________________________________________________|'
      WRITE( *,* ) ' |                                                 |'    
      DO I = 1, NUMPROCS
         WRITE( *,1003 ) I, NROWS_PE(I), ROWSX_PE(1,I), ROWSX_PE(2,I),
     &                      NCOLS_PE(I), COLSX_PE(1,I), COLSX_PE(2,I)
      END DO
      WRITE( *,* ) ' |_________________________________________________|'
      WRITE( *,* )

      RETURN

1003  FORMAT('  |', I3, 5X, I4, 3X, I4, ':', I4, 
     &                  7X, I4, 4X, I4, ':', I4, ' |')

      END
