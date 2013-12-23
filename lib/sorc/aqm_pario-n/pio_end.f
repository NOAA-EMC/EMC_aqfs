      SUBROUTINE PIO_END 

C ....................................................................
 
C  PURPOSE:    Deallocate all dynamically allocated memory
 
C  REVISION HISTORY: 
C       Original version  01/10/05 by David Wong
 
C .......................................................................

      USE PIOMAPS_MODULE
      USE PINTERPB_MODULE
      USE ALLOC_DATA_MODULE

        IMPLICIT  NONE

C LOCAL VARIABLES: 

C .......................................................................
C     begin function PIO_END

      IF (ALLOCATED(MSGBUFHD)) THEN
         DEALLOCATE(MSGBUFHD)
      END IF

      IF (ALLOCATED(NROWS_PE)) THEN
         DEALLOCATE(NROWS_PE)
         DEALLOCATE(NCOLS_PE)
         DEALLOCATE(ROWSX_PE)
         DEALLOCATE(COLSX_PE)
         DEALLOCATE(WR_NROWS_PE)
         DEALLOCATE(WR_NCOLS_PE)
         DEALLOCATE(WR_ROWSX_PE)
         DEALLOCATE(WR_COLSX_PE)
      END IF

      IF (ALLOCATED(WRITBUF)) THEN
         DEALLOCATE(WRITBUF)
      END IF

      IF (ALLOCATED(RECVBUF)) THEN
         DEALLOCATE(RECVBUF)
      END IF

      RETURN
      END
