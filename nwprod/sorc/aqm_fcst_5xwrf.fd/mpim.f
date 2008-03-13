
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
      MODULE MPIM

C-----------------------------------------------------------------------
C Function: utility mpi module

C Preconditions:

C Subroutines and functions called:

C Revision History:
C   Feb 03 - Dave Wong, Jeff: created
C-----------------------------------------------------------------------

      INTEGER NPROCS       ! Number of processors (PE'S) assigned (global)
      INTEGER G_MYPE       ! MYPE in global group (global)
      INTEGER IO_MYPE      ! MYPE in IO group (local)
      INTEGER WORK_MYPE    ! MYPE in worker group (local)
      INTEGER WORLD_GROUP  ! global PE group index (global)
      INTEGER IO_GROUP     ! IO PE group index (global)
      INTEGER WORK_GROUP   ! worker PE group index (global)
      INTEGER IO_COMM      ! IO group communicator (handle) (local)
      INTEGER WORK_COMM    ! worker group communicator (handle) (local)
      INTEGER NPROCS_IO    ! number of IO processors (local)
      INTEGER NPROCS_W     ! number of worker processors (local)

      INCLUDE 'mpif.h'

      CONTAINS

C-----------------------------------------------------------------------
      SUBROUTINE INIT_MPI

      IMPLICIT NONE

      INTEGER :: ERROR, RANKS( 1 )

      CALL MPI_INIT ( ERROR )
      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, G_MYPE, ERROR )
      CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, NPROCS, ERROR )

C MPI group and communicator setup

      CALL MPI_COMM_GROUP ( MPI_COMM_WORLD, WORLD_GROUP, ERROR )

      RANKS( 1 ) = 0
      CALL MPI_GROUP_INCL ( WORLD_GROUP, 1, RANKS, IO_GROUP, ERROR )
      CALL MPI_GROUP_EXCL ( WORLD_GROUP, 1, RANKS, WORK_GROUP, ERROR )

      CALL MPI_COMM_CREATE ( MPI_COMM_WORLD, IO_GROUP, IO_COMM, ERROR )
      CALL MPI_COMM_CREATE ( MPI_COMM_WORLD, WORK_GROUP, WORK_COMM, ERROR )

      IO_MYPE = -1
      WORK_MYPE = -1

      IF ( IO_COMM .NE. MPI_UNDEFINED ) THEN
         CALL MPI_COMM_RANK ( IO_COMM, IO_MYPE, ERROR )
         CALL MPI_COMM_SIZE ( IO_COMM, NPROCS_IO, ERROR )
      ELSE IF ( WORK_COMM .NE. MPI_UNDEFINED ) THEN
         CALL MPI_COMM_RANK ( WORK_COMM, WORK_MYPE, ERROR )
         CALL MPI_COMM_SIZE ( WORK_COMM, NPROCS_W, ERROR )
      END IF

      RETURN
      END SUBROUTINE INIT_MPI

C-----------------------------------------------------------------------
      SUBROUTINE SHUT_DOWN_MPI

      IMPLICIT NONE

      INTEGER :: ERROR

      CALL MPI_GROUP_FREE ( IO_GROUP, ERROR )
      CALL MPI_GROUP_FREE ( WORK_GROUP, ERROR )

      IF ( IO_COMM .NE. MPI_UNDEFINED ) THEN
         CALL MPI_COMM_FREE ( IO_COMM, ERROR )
      ELSE IF ( WORK_COMM .NE. MPI_UNDEFINED ) THEN
         CALL MPI_COMM_FREE ( WORK_COMM, ERROR )
      END IF

      CALL MPI_FINALIZE ( ERROR )

      RETURN
      END SUBROUTINE SHUT_DOWN_MPI

      END MODULE MPIM
