C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/air4/yoj/rel/models/PARIO/src/growbuf.f,v 1.1.1.1 2003/05/05 17:18:09 yoj Exp $

      LOGICAL FUNCTION GROWBUF ( PTR_ARRAY, END ) 
C ....................................................................
 
C  PURPOSE:    Grows memory for dynamic memory array
 
C  RETURN VALUE:  The function fails if an error is detected in this
C       subroutine
 
C  REVISION HISTORY: 
C       Original version 07/20/1998 by Al Bourgeois. Moved from PINTERP3,
C          modified for better error handling across processors.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 05/15/1999 by Al Bourgeois to change variable names for
C          general use.
C       Modified 06/16/1999 by Al Bourgeois to remove interprocessor
C          synchronization. This removes the guarantee that all processors
C          return the same error code, and a "hang" state can occur if
C          PM3EXIT is not called on the condition that this function fails.
C       Modified 02/06/2003 by David Wong
C          -- use f90 syntax to allocate memory and grow memory so it
C             no longer uses DYNMEM library
C       Modified 11/03/2004 by David Wong
C          -- fixed a bug, which only manifested in Sun system, in 
C             allocating new memory space

    
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C     INTEGER  PTR_ARRAY      ! Address of dynamic array.
C     INTEGER  END            ! Offset for end of array.
 
C  OUT:
C     INTEGER  PTR_ARRAY      ! Address of grown array.
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS:  M3WARN
 
C .......................................................................

      USE PINTERPB_MODULE

      IMPLICIT  NONE

C ARGUMENTS:
 
      TYPE(MEM_TYPE) ::  PTR_ARRAY  ! Address of dynamic array.
      INTEGER, INTENT(IN) ::  END        ! Offset for end of array.

C INCLUDE FILES:

      INCLUDE 'IODECL3.EXT'     ! M3IO definitions and declarations

C LOCAL VARIABLES: 

      INTEGER              :: IERROR   ! Error code from allocation subroutine.
      CHARACTER (len = 80) :: MSG      ! Message issued from PM3WARN routine.

c     REAL, ALLOCATABLE, TARGET :: LARRAY1(:), LARRAY2(:)
      INTEGER :: DATA_SIZE, I

C .......................................................................
C     begin function GROWBUF

C Initialize return value.

      GROWBUF = .TRUE.

      DATA_SIZE = (END - BUFFERHD_SIZE) / 2

c     ALLOCATE (LARRAY1(DATA_SIZE), STAT=IERROR)
c     IF ( IERROR .NE. 0 ) THEN
c        MSG = 'Error growing array 1.'
c        CALL M3WARN( 'GROWBUF', 0, 0, MSG )
c        GROWBUF = .FALSE.
c        RETURN
c     END IF

c     ALLOCATE (LARRAY2(DATA_SIZE), STAT=IERROR)
c     IF ( IERROR .NE. 0 ) THEN
c        MSG = 'Error growing array 2.'
c        CALL M3WARN( 'GROWBUF', 0, 0, MSG )
c        GROWBUF = .FALSE.
c        RETURN
c     END IF

      PTR_ARRAY%SIZE = DATA_SIZE * 2
c     PTR_ARRAY%MEM(0)%DATA_PTR => LARRAY1
c     PTR_ARRAY%MEM(1)%DATA_PTR => LARRAY2
      ALLOCATE(PTR_ARRAY%MEM(0)%DATA_PTR(DATA_SIZE), STAT=IERROR)
      ALLOCATE(PTR_ARRAY%MEM(1)%DATA_PTR(DATA_SIZE), STAT=IERROR)
      BUFFERHD_SIZE = BUFFERHD_SIZE + DATA_SIZE * 2

      RETURN
      END
