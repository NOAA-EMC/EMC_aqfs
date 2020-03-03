C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/PARIO/src/piomaps_mod.f,v 1.2 2006/06/05 17:36:43 yoj Exp $ 

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

C    INTEGER  NUMPROCS              Number of processors.

C    INTEGER  NCOLS_PE( NUMPROCS )      Number grid columns in the processor
C    INTEGER  NROWS_PE( NUMPROCS )      Number grid rows in the processor
C    INTEGER  COLSX_PE( 2,NUMPROCS )    Processor column range:
C                                       COLSX_PE(1,*) = start column index
C                                       COLSX_PE(2,*) = end column index
C    INTEGER  ROWSX_PE( 2,NUMPROCS )    Processor row range:
C                                       ROWSX_PE(1,*) = start row index
C                                       ROWSX_PE(2,*) = end row index
C    INTEGER  WR_NCOLS_PE( NUMPROCS )   Number of columns in the processor
C                                       subgrid to write
C    INTEGER  WR_NROWS_PE( NUMPROCS )   Number of rows in the processor
C                                       subgrid to write
C                                       ROWSX_PE(2,*) = end row index
C    INTEGER  WR_COLSX_PE( 2,NUMPROCS ) Column range of the processor subgrid
C                                       to write
C                                       COLSX_PE(1,*) = start column index
C                                       COLSX_PE(2,*) = end column index
C    INTEGER  WR_ROWSX_PE( 2,NUMPROCS ) Row range of the processor subgrid
C                                       to write
C                                       ROWSX_PE(1,*) = start row index

C....................................................................

      MODULE PIOMAPS_MODULE

      INTEGER  NUMPROCS
  
      INTEGER, ALLOCATABLE :: NCOLS_PE( : )
      INTEGER, ALLOCATABLE :: COLSX_PE( :,: )

      INTEGER, ALLOCATABLE :: NROWS_PE( : )
      INTEGER, ALLOCATABLE :: ROWSX_PE( :,: )

      INTEGER, ALLOCATABLE :: WR_NCOLS_PE( : )
      INTEGER, ALLOCATABLE :: WR_COLSX_PE( :,: )

      INTEGER, ALLOCATABLE :: WR_NROWS_PE( : )
      INTEGER, ALLOCATABLE :: WR_ROWSX_PE( :,: )

      END MODULE PIOMAPS_MODULE
