
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/cmaq/rel/models/CCTM/src/vadv/vppm/zadvppm.F,v 1.1.1.1 2002/06/27 11:25:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ASSEMBLE ( ISTEP, FTAG, logdev,
     &                      FG_NCOLS, OFFSET_CR,
     &                      D_NVARS, NLVLS, DSZE,
     &                      SPCHIT, WRITTEN, NPE, GDATA,
     &                      bounce )

C IN:
C    ISTEP     = current step
C    FTAG      = datatype base tag
C    FG_NCOLS  = global no. of grid columns
C    OFFSET_CR = global offset to the next layer (=GNCOLS*GNROWS)
C    D_NVARS   = no. of variables in distributed data
C    NLVLS     = no. of layers in distributed data
C    DSZE      = dimension size for distributed data
C    SPCHIT    = boolean array to subset output species from distributed data
C INOUT:
C    WRITTEN   = has global data buffer received processor i's data?
C                (effectively, a local array, but must maintain different
C                copies corresponding to different callers)
C    NPE       = incremented processor counter
C OUT:
C    GDATA     = subsetted global data for output
C    bounce    = count of messages not yet received
C NOTE: currently assuming the distributed data no. of layers is the same
C       as the output global data's

C-----------------------------------------------------------------------
C Function:

C Preconditions:

C Subroutines and functions called:

C Revision History:
C   Feb 03 - Jeff, Dave Wong: created
C   Feb 03 - Jeff: for CGRID(C,R,L,S)
C   May 03 - Jeff: Dave's suggestion for efficiencey - move ck for DSZE
C                  increase after MPI_PROBE
C   Feb 07 - Jeff: change fro NPROCS-1 to globally-known N_WORKERS
C-----------------------------------------------------------------------

      USE HGRD_DEFN
      USE MPIM

      IMPLICIT NONE

      INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
      INCLUDE SUBST_IODECL    ! I/O definitions and declarations
      INCLUDE SUBST_IOFDESC   ! file header data structure
      INCLUDE 'TAG.EXT'

      INTEGER, INTENT( IN )    :: ISTEP, FTAG, logdev,
     &                            FG_NCOLS, OFFSET_CR,
     &                            D_NVARS, NLVLS, DSZE
      LOGICAL, INTENT( IN )    :: SPCHIT( : )
      LOGICAL, INTENT( INOUT ) :: WRITTEN( : )
      INTEGER, INTENT( INOUT ) :: NPE
      REAL, INTENT( OUT )      :: GDATA( : )  ! Global Data for Output File
      integer, intent( inout ) :: bounce

      REAL, ALLOCATABLE, SAVE  :: DDATA( : )  ! Distributed Data
      INTEGER, SAVE :: SSZE
!     INTEGER, SAVE :: N_WORKERS   ! = NPROCS_W
      LOGICAL DONE, READY
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER ASTAT
      INTEGER TAG, STATUS( MPI_STATUS_SIZE ), ERROR
      INTEGER STRTCOL, STRTROW, MCOLS, MROWS, MSZE
      INTEGER OFFSET_SPC
      INTEGER I, R, L, V, S1, S2, S2_OLD, E1, E2

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

!        write( logdev,* ) '   assemble-dsze:', dsze

         ALLOCATE ( DDATA( DSZE ), STAT=ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            WRITE( LOGDEV,* ) ' Error allocating memory for DDATA'
            STOP
            END IF
         SSZE = DSZE

!        write( logdev,* ) '   assemble-dsze:', dsze

         END IF   ! Firstime

!     DO I = 1, NPROCS - 1
      DO I = 1, N_WORKERS
         TAG = I * TAGFAC + FTAG + ISTEP
         IF ( .NOT. WRITTEN( I ) ) THEN
            CALL MPI_IPROBE ( I, TAG, MPI_COMM_WORLD, READY, STATUS, ERROR )
            IF ( READY ) THEN   ! msg has arrived

C if DSZE has increased, re-allocate DDATA

               IF ( DSZE .GT. SSZE ) THEN
!                 write( logdev,* ) '   assemble- increasing dsze: ', ssze, dsze
                  DEALLOCATE ( DDATA )
                  ALLOCATE ( DDATA( DSZE ), STAT=ASTAT )
                  IF ( ASTAT .NE. 0 ) THEN
                     WRITE( LOGDEV,* ) ' Error reallocating memory for DDATA'
                     STOP
                  END IF
                  SSZE = DSZE
               END IF

!        write( logdev,* ) '   assemble- ISTEP, I, TAG: ', istep, i, tag

               STRTCOL = COLSX_PE( 1,I )
               STRTROW = ROWSX_PE( 1,I )
               MCOLS = COLSX_PE( 2,I ) - STRTCOL + 1
               MROWS = ROWSX_PE( 2,I ) - STRTROW + 1
               OFFSET_SPC = MCOLS * MROWS * NLVLS
               MSZE = OFFSET_SPC * D_NVARS

!        write( logdev,* ) '   assemble- MCOLS, MROWS, NLVLS, D_NVARS: ',
!    &                                   MCOLS, MROWS, NLVLS, D_NVARS

               CALL MPI_RECV ( DDATA, MSZE, MPI_REAL, I, TAG,
     &                         MPI_COMM_WORLD, STATUS, ERROR )

C blocking a row at a time: S1:E1 -> S2:E2

               S2 = STRTCOL + FG_NCOLS * ( STRTROW - 1 )
               S2_OLD = S2
               DO V = 1, D_NVARS
                  IF ( SPCHIT( V ) ) THEN
                  S1 = 1 + ( V - 1 ) * OFFSET_SPC
                  E1 = S1 + MCOLS - 1
                     DO L = 1, NLVLS
                        E2 = S2 + MCOLS - 1
                        DO R = 1, MROWS
                           GDATA( S2:E2 ) = DDATA( S1:E1 )
                           S1 = S1 + MCOLS
                           E1 = E1 + MCOLS
                           S2 = S2 + FG_NCOLS
                           E2 = E2 + FG_NCOLS
                        END DO
                        S2 = S2_OLD + OFFSET_CR
                        S2_OLD = S2
                     END DO   ! L
                  END IF
               END DO   ! V

               WRITTEN( I ) = .TRUE.
               NPE = NPE + 1
            ELSE     ! msg has not arrived
               bounce = bounce + 1
            END IF   ! ready

         END IF   ! not written
      END DO   ! PE loop

      RETURN

      END SUBROUTINE ASSEMBLE
