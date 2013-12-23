C RCS file, release, date & time of last delta, author, state, [and locker] 
C $Header: /project/cmaq12/tmp/tools/PARIO/src/pwrgrdd.f,v 1.1.1.1 2001/01/25 19:14:29 yoj Exp $

      LOGICAL FUNCTION PWRGRDD( FILNAME, VARNAME, DATE, TIME, BUFFER,
     &                          NROWS3D, NCOLS3D, NLAYS3D,
     &                          NROWS, NCOLS, NP,
     &                          WR_NROWS_PE, WR_NCOLS_PE,
     &                          WR_ROWSX_PE, WR_COLSX_PE,         
     &                          MY_PE, IO_PE )
C.....................................................................
C
C  PURPOSE:   Perform Models-3 file-write operation in a parallel
C             environment. Values of variable VARNAME on each processor
C             subdomain region are collected, via MPI calls, by the
C             primary I/O processor and the full grid of values are
C             written to file.
C
C
C  RETURN VALUE:  The function fails if M3IO routine WRITE3 fails. If
C       an MPI error occurs, the program is aborted with a call to
C       PM3EXIT. 
C
C
C  REVISION  HISTORY:
C       07/1998 Original version by Al Bourgeois for parallel implementation.
C       Renamed  10/07/1998 by AJB from PWRITE3 to PRDGRDD, to be called
C                by PWRITE3.
C       Modified 11/09/1998 by AJB to save RBUFSIZ.
C       Modified 12/04/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 01/26/1999 by Al Bourgeois to allow writing to a subgrid.
C       Modified 06/22/1999 by Al Bourgeois to synchronize I/O processors
C          with their target (non-I/O) processors. If the I/O processor fails
C          on WRITE3, all processors will return a value of FALSE. This
C          routine now calls pm3exit if an MPI error occurs.
C       Modified 02/05/2004 by David Wong
C          -- use f90 allocatable structure, collect all and write once
C
C
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C    CHARACTER*(*)  FILNAME       ! Name of file containing variable VARNAME.
C    CHARACTER*(*)  VARNAME       ! Name of file variable to write.
C    INTEGER        DATE          ! Date, formatted YYYYDDD.
C    INTEGER        TIME          ! Time, formatted HHMMSS.
C
C    REAL           BUFFER(NCOLS, NROWS, NLAYS3D)
C                                 ! Buffer holding (local) array to be written.
C
C    INTEGER  NROWS3D             ! Row dimension of file variables.
C    INTEGER  NCOLS3D             ! Column dimension of file variables.
C    INTEGER  NLAYS3D             ! Layer dimension of file variables.
C    INTEGER  NROWS               ! Row dimension of local-processor arrays.
C    INTEGER  NCOLS               ! Column dimension of local-processor arrays.
C    INTEGER  NP                  ! Number of processors.
C    INTEGER  WR_NROWS_PE(NP)     ! No. rows each PE of subgrid to write.
C    INTEGER  WR_NCOLS_PE(NP)     ! No. cols each PE of subgrid to write.
C    INTEGER  WR_ROWSX_PE(2,NP)   ! Row range each PE of subgrid to write.
C    INTEGER  WR_COLSX_PE(2,NP)   ! Col range each PE of subgrid to write.
C
C    REAL WRITBUF(NCOLS3D, NROWS3D, NLAYS3D)
C                                 ! Buffer for writing an array.
C
C    REAL RECVBUF(NCOLS, NROWS, NLAYS3D)
C                                 ! Buffer for message passing an array.
C
C    INTEGER  MY_PE               ! Local processor id, ranging 0 to NP-1.
C    INTEGER  IO_PE               ! Id of primary processor used for file I/O.
C
C  OUT: none
C
C
C  LOCAL VARIABLE DESCRIPTION:  see below
C
C
C  CALLS: WRITE3, PM3WARN, PM3EXIT, TRIMLEN, MPI_SEND, MPI_RECV,
C         MPI_BCAST
C
C  NOTES: (1) Only the primary I/O processor does the file writing. Input
C             arguments FILNAME, VARNAME, DATE, and TIME are meaningful
C             only to the I/O processor.
C
C         (2) This routine handles only gridded variables. The BUFFER is
C             assumed to be declared as BUFFER( NCOLS, NROWS, NLAYS3D ),
C             where NCOLS and NROWS are the local PE grid subdomain
C             dimensions and NLAYS3D is the file variable layer dimension.
C             BUFFER is assumed to be filled as BUFFER(1:C,1:R,1:NLAYS3D),
C             where C = WR_COLSX_PE(2,MY_PE+1), R = WR_ROWSX_PE(2,MY_PE+1).
C             
C-----------------------------------------------------------------------

      USE ALLOC_DATA_MODULE

      IMPLICIT NONE

C.......   INCLUDE FILES

      INCLUDE 'mpif.h'           ! MPI definitions and parameters.
      INCLUDE 'IODECL3.EXT'      ! I/O definitions and declarations.

C.......   ARGUMENTS and their descriptions:

      CHARACTER*(*)  FILNAME     ! Name of file containing variable VARNAME.
      CHARACTER*(*)  VARNAME     ! Name of file variable to write.
      INTEGER        DATE        ! Date, formatted YYYYDDD.
      INTEGER        TIME        ! Time, formatted HHMMSS. 
      INTEGER  NROWS3D           ! Row dimension of file variables.
      INTEGER  NCOLS3D           ! Column dimension of file variables.
      INTEGER  NLAYS3D           ! Layer dimension of file variables.
      INTEGER  NROWS             ! Row dimension of local-processor arrays.
      INTEGER  NCOLS             ! Column dimension of local-processor arrays.
      INTEGER  NP                ! Number of processors.
      INTEGER  WR_NROWS_PE(NP)   ! No. rows each PE of subgrid to write.
      INTEGER  WR_NCOLS_PE(NP)   ! No. cols each PE of subgrid to write.
      INTEGER  WR_ROWSX_PE(2,NP) ! Row range each PE of subgrid to write.
      INTEGER  WR_COLSX_PE(2,NP) ! Col range each PE of subgrid to write.
 
      REAL BUFFER(NCOLS, NROWS, NLAYS3D)       ! Buffer holding (local) array 
                                               ! to be written.

c     REAL, ALLOCATABLE, SAVE :: WRITBUF(:, :, :)
c     REAL, ALLOCATABLE, SAVE :: RECVBUF(:)

      INTEGER, SAVE :: WRITBUF_SIZE = 0
      INTEGER, SAVE :: RECVBUF_SIZE = 0
      INTEGER :: WSIZE, RSIZE

      INTEGER  MY_PE             ! Local processor id, ranging 0 to NP-1.
      INTEGER  IO_PE             ! Id of primary processor used for file I/O.

C.......   EXTERNAL FUNCTIONS:

      INTEGER       TRIMLEN                  ! Effective char. string length. 
      EXTERNAL      TRIMLEN                  ! M3IO library.
      EXTERNAL      PM3WARN                  ! Parallel M3IO library.

C.......   LOCAL VARIABLES:

      INTEGER        MSGSIZE       ! Message size of subgrid to receive.
      INTEGER        IPE           ! For loop over processors.
      INTEGER        WHO           ! For identifying sending processor.
      INTEGER        STATUS(MPI_STATUS_SIZE)   ! MPI status code.
      INTEGER        IERROR        ! MPI error code.
      LOGICAL        LERROR        ! LOCAL ERROR
      LOGICAL        RERROR        ! LOCAL MPI ALLREDUCE ERROR
      INTEGER        IR            ! Loop counter over grid rows.
      INTEGER        IC            ! Loop counter over grid columns.
      INTEGER        IL            ! Loop counter over grid layers.
      INTEGER        C0            ! First column in global grid.
      INTEGER        R0            ! First row in global grid.
      INTEGER        NC            ! Number of columns in local grid.
      INTEGER        NR            ! Number of rows in local grid.
      CHARACTER*16   FIL16         ! Scratch area for file-name.
      CHARACTER*16   VAR16         ! Scratch area for vble-name.
      CHARACTER*80   MSG           ! Message issued from PM3WARN routine.

      INTEGER  TAG1               ! MPI message tag for processor ID. 
      DATA     TAG1 / 901 /
      SAVE     TAG1

      INTEGER  TAG2               ! MPI message tag for data array.
      DATA     TAG2 / 902 /
      SAVE     TAG2

      INTEGER  LOC

C........................................................................
C     begin function PWRGRDD

C......   Initialize return value and error code.
      PWRGRDD  = .TRUE.
      LERROR   = .FALSE.
      IERROR = 0
 
      WSIZE = NCOLS3D * NROWS3D * NLAYS3D 
      RSIZE = NCOLS*NROWS*NLAYS3D

      IF ( WRITBUF_SIZE .LT. WSIZE ) THEN
         IF ( ALLOCATED ( WRITBUF ) ) DEALLOCATE ( WRITBUF )
         ALLOCATE ( WRITBUF  ( NCOLS3D, NROWS3D, NLAYS3D ), STAT = IERROR )
         IF ( IERROR .NE. 0 ) THEN
            MSG = 'Failure allocating WRITBUF '
            CALL M3EXIT( 'PWRGRDD', DATE, TIME, MSG, 1 )
         END IF
         WRITBUF_SIZE = WSIZE
      END IF

      IF ( RECVBUF_SIZE .LT. RSIZE ) THEN
         IF ( ALLOCATED ( RECVBUF ) ) DEALLOCATE ( RECVBUF )
         ALLOCATE ( RECVBUF  ( RSIZE ), STAT = IERROR )
         IF ( IERROR .NE. 0 ) THEN
            MSG = 'Failure allocating RECVBUF '
            CALL M3EXIT( 'PWRGRDD', DATE, TIME, MSG, 1 )
         END IF
         RECVBUF_SIZE = RSIZE
      END IF

C.......  Gather the array and write it to file. 

      IF( MY_PE .EQ. IO_PE ) THEN    ! I/O processor collects and writes data.

C.......   I/O PE copies its own local array into output buffer.

           C0 = WR_COLSX_PE(1, IO_PE+1)
           R0 = WR_ROWSX_PE(1, IO_PE+1)
           NC = WR_NCOLS_PE(IO_PE+1)
           NR = WR_NROWS_PE(IO_PE+1)

           DO IL = 1, NLAYS3D
              DO IR = 1, NR
                 DO IC = 1, NC 
                    WRITBUF( C0+IC-1, R0+IR-1, IL ) = BUFFER(IC, IR, IL )
                 END DO    
              END DO    
           END DO

C.......   I/O PE receives deposition array for all other processors
C.......   and copies it to the output buffer.  Arrays are received
C.......   in a first-come-first-serve order.  

           DO IPE = 1, NP - 1

             CALL MPI_RECV( WHO, 1, MPI_INTEGER, MPI_ANY_SOURCE, 
     &                      TAG1, MPI_COMM_WORLD, STATUS, IERROR )

             IF ( IERROR .NE. 0 ) THEN
               MSG = 'MPI error receiving processor id WHO.'
               CALL M3WARN( 'PWRGRDD', DATE, TIME, MSG )
               LERROR = .TRUE.
             ENDIF

             C0 = WR_COLSX_PE(1, WHO+1)
             R0 = WR_ROWSX_PE(1, WHO+1)
             NC = WR_NCOLS_PE(WHO+1)
             NR = WR_NROWS_PE(WHO+1)

             MSGSIZE = NC * NR * NLAYS3D

             CALL MPI_RECV( RECVBUF, MSGSIZE, MPI_REAL, WHO,
     &                      TAG2, MPI_COMM_WORLD, STATUS, IERROR )
             IF ( IERROR .NE. 0 ) THEN
               MSG = 'MPI error receiving data array RECVBUF.'
               CALL M3WARN( 'PWRGRDD', DATE, TIME, MSG )
               LERROR = .TRUE.
             ENDIF

             LOC = 0
             DO IL = 1, NLAYS3D
                DO IR = 1, NR
                   DO IC = 1, NC
                      LOC = LOC + 1
                      WRITBUF( C0+IC-1, R0+IR-1, IL ) = RECVBUF(LOC)
                   END DO    
                END DO    
             END DO

           END DO


C.......   Write the accumulated depositions to file.

           FIL16 = FILNAME
           VAR16 = VARNAME
           IF ( .NOT. WRITE3( FIL16, VAR16, DATE, TIME, WRITBUF ) ) THEN
              MSG = 'Could not write '
     &              // VARNAME( 1:TRIMLEN( VARNAME ) ) //
     &             ' to file '// FIL16( 1:TRIMLEN( FIL16) )

              CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
              LERROR = .TRUE.
           ENDIF

      ELSE      ! Non-I/O processors send data.

C.......   Each processor (except for the I/O processor) sends its
C.......   local array to the I/O processor.

          WHO = MY_PE
          MSGSIZE = NCOLS * NROWS * NLAYS3D

          CALL MPI_SEND( WHO, 1, MPI_INTEGER, IO_PE, TAG1,
     &                   MPI_COMM_WORLD, IERROR )

          IF ( IERROR .NE. 0 ) THEN
            MSG = 'MPI error sending processor id WHO.'
            CALL M3WARN( 'PWRGRDD', DATE, TIME, MSG )
            LERROR = .TRUE.
          ENDIF

          CALL MPI_SEND( BUFFER, MSGSIZE, MPI_REAL, IO_PE, TAG2,
     &                   MPI_COMM_WORLD, IERROR )

          IF ( IERROR .NE. 0 ) THEN
            MSG = 'MPI error sending data array BUFFER.'
            CALL M3WARN( 'PWRGRDD', DATE, TIME, MSG )
            LERROR = .TRUE.
          ENDIF

      ENDIF     ! end if( my_pe .eq. io_pe )

C.......   Notify non-I/O processors of failure.

      CALL MPI_ALLREDUCE( LERROR, RERROR, 1, MPI_LOGICAL, MPI_LAND, 
     &                    MPI_COMM_WORLD, IERROR )

      IF ( IERROR .NE. 0 ) THEN
         MSG = 'MPI Allreduce error.'
         CALL M3WARN( 'PWRGRDD', DATE, TIME, MSG )
         LERROR = .TRUE.
      END IF

      IF ( RERROR ) THEN
          MSG = 'Failed to write '
     &         // VARNAME( 1:TRIMLEN( VARNAME ) ) //
     &         ' from file '// FILNAME( 1:TRIMLEN( FILNAME) )
          CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
          PWRGRDD = .FALSE.
          RETURN        !Write failed, so return.
      ENDIF

      RETURN
      END
