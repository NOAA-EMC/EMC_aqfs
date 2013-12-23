
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/swap_sandia.f,v 1.2 2004/03/26 16:25:25 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C shared variables for Swap 1d, 2d, 3d, 4d routines & init routine

      module swap_sandia

      use se_pe_info_ext

      implicit none

      integer, parameter :: NORTH = 1
      integer, parameter :: SOUTH = 2
      integer, parameter :: EAST  = 3
      integer, parameter :: WEST  = 4

      integer recvneigh( 4 ) ! 4 neighbor procs I recv from in each dir
      integer sendneigh( 4 ) ! 4 neighbor procs I send to when recv in each dir
                             ! value of -1 means no neighbor in that dir

!     interface swap_snl
!        module procedure swap1d, swap2d, swap3d, swap4d
!     end interface

      contains

C Set up proc IDs of 4 neighboring processors
C Only needs to be called once
C Decomposition is defined by:
C    npcol = # of column procs
C    nprow = # of row procs
C    proc 0 = SW corner of domain
C    proc 1 = east of proc 0
C    proc npcol = north of proc 0
C    proc npcol*nprow - 1 = NE corner of domain
C NOTE: sendneigh(NORTH) is not who I send to in NORTH dir,
C    but who I send to when I am receiving from the NORTH,
C    i.e. sendneigh(NORTH) = the proc to the south of me

!        subroutine swap_init_snl( npcol, nprow )
         subroutine swap_init_snl( )
         implicit none
         include 'mpif.h'

         integer npcol, nprow

         integer me, nprocs, error

!        call MPI_Comm_rank( mpi_comm_world, me, error )
!        call MPI_Comm_size( mpi_comm_world, nprocs, error )

         npcol  = se_npcol
         nprow  = se_nprow
         me     = se_myworker_pe
         nprocs = se_numworkers

         recvneigh( NORTH ) = me + npcol
         recvneigh( SOUTH ) = me - npcol
         recvneigh( EAST )  = me + 1
         recvneigh( WEST )  = me - 1

         sendneigh( NORTH ) = me - npcol
         sendneigh( SOUTH ) = me + npcol
         sendneigh( EAST )  = me - 1
         sendneigh( WEST )  = me + 1

         if ( me < npcol ) then                     ! south boundary
           recvneigh( SOUTH ) = -1
           sendneigh( NORTH ) = -1
         end if
         if ( me >= nprocs - npcol ) then           ! north boundary
           recvneigh( NORTH ) = -1
           sendneigh( SOUTH ) = -1
         end if
         if ( mod( me, npcol ) == 0 ) then          ! west boundary
           recvneigh( WEST ) = -1
           sendneigh( EAST ) = -1
         end if
         if ( mod( me, npcol ) == npcol - 1 ) then  ! east boundary
           recvneigh( EAST ) = -1
           sendneigh( WEST ) = -1
         end if

         return
         end subroutine swap_init_snl

      end module swap_sandia

