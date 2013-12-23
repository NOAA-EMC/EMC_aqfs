C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
 
C  CONTAINS:  Processor-to-subdomain maps for parallel Models-3
C             I/O routines.
 
C  DEPENDENT UPON:  none 
 
C  REVISION HISTORY:
C       Original version 02/04 by David Wong
 
C  NOTES:  The mapping assumes 2-dimensional subdomain decomposition,
C          over grid rows and columns.
C....................................................................

C  ----------------------------------------------------
C  The arrays below are allocated by function PIO_INIT.
C  ----------------------------------------------------

C     INTEGER  NUMPROCS              Number of processors.

C     INTEGER  NROWS_PE(NUMPROCS)    Number grid rows in each processor.
C     INTEGER  NCOLS_PE(NUMPROCS)    Number grid columns in each processor.
C     INTEGER  ROWSX_PE(2,NUMPROCS)  Row range for each PE.
C                                    ROWSX_PE(1,*) is the start row index.
C                                    ROWSX_PE(2,*) is the end row index.
C     INTEGER  COLSX_PE(2,NUMPROCS)  Column range for each PE.
C                                    COLSX_PE(1,*) is the start column index.
C                                    COLSX_PE(2,*) is the end column index.
C     INTEGER  WR_NROWS_PE(NUMPROCS)   Number rows each PE of subgrid to write.
C     INTEGER  WR_NCOLS_PE(NUMPROCS)   Number cols each PE of subgrid to write.
C     INTEGER  WR_ROWSX_PE(2,NUMPROCS) Row range each PE of subgrid to write.
C                                      ROWSX_PE(1,*) is the start row index.
C                                      ROWSX_PE(2,*) is the end row index.
C     INTEGER  WR_COLSX_PE(2,NUMPROCS) Col range each PE of subgrid to write.
C                                      COLSX_PE(1,*) is the start column index.
C                                      COLSX_PE(2,*) is the end column index.

C....................................................................

      MODULE PIOMAPS_MODULE

      INTEGER  NUMPROCS
  
      INTEGER, ALLOCATABLE :: NROWS_PE(:)
      INTEGER, ALLOCATABLE :: ROWSX_PE(:,:)

      INTEGER, ALLOCATABLE :: NCOLS_PE(:)
      INTEGER, ALLOCATABLE :: COLSX_PE(:,:)

      INTEGER, ALLOCATABLE :: WR_NROWS_PE(:)
      INTEGER, ALLOCATABLE :: WR_ROWSX_PE(:,:)

      INTEGER, ALLOCATABLE :: WR_NCOLS_PE(:)
      INTEGER, ALLOCATABLE :: WR_COLSX_PE(:,:)

      END MODULE PIOMAPS_MODULE
