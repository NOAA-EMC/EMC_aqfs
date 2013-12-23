C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Allocated variables used by pwrgrdd
              
C  REVISION HISTORY:
C       Original version 01/10/05 by David Wong
C....................................................................

      MODULE ALLOC_DATA_MODULE

      REAL, ALLOCATABLE, SAVE :: WRITBUF(:, :, :)
      REAL, ALLOCATABLE, SAVE :: RECVBUF(:)

      END MODULE ALLOC_DATA_MODULE
