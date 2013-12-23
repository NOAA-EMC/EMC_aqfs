C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_f90/se_init_module.f,v 1.1.1.1 2000/04/12 17:39:23 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 module feature to capture se_init routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C --------------------------------------------------------------------------

        module se_init_module

          implicit none

          private :: se_dim_init
          private :: cal_size 

          contains

C -----------------------------------------------------------------------------
C Purpose:
C
C   initialize various variable for stencil exchange library
C
C Revision history:
C
C   Orginal version: 7/10/98 by David Wong
C                    2/15/99 by David Wong
C                      -- add se_global_map call to determine processor
C                         configuration and global domain map
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    10/13/00 by David Wong
C                      -- rename row with dimi and col with dimj and use 
C                         data_ori to determine the order of column and
C                         row input
C                    12/12/00 by David Wong
C                      -- use f90 function ALLOCATED to determine an array has
C                         been allocated or not rather than a logical variable.
C                    12/04/00 by David Wong
C                      -- modified the routine to accommodate: 1. the 
C                         worker and I/O processors partition scheme and 2.
C                         data structure reordering
C
C Local variable description:
C
C   i, j      -- loop index
C   leng      -- length of the character string, decompstr
C   decompstr -- a character string indicates which dimension(s) is/are
C                decomposed
C   loci      -- Cartessian coordinate of mype x-axis
C   locj      -- Cartessian coordinate of mype y-axis
C   pos       -- position of the conversion table
C   conv      -- a conversion table
C
C Include files:
C
C   se_domain_info.ext
C   se_pe_info.ext
C   se_comm_info.ext
C   se_disp_info_ext
C
C Subroutine/Function call:
C
C   se_dim_init
C
C Equivalence of the conversion table
C
C   (loci-1, locj-1) (loci-1, locj) (loci-1, locj+1)                8  1  2
C   ( loci,  locj-1) ( loci,  locj) ( loci,  locj+1) equivalent to  7     3
C   (loci+1, locj-1) (loci+1, locj) (loci+1, locj+1)                6  5  4
C -----------------------------------------------------------------------------

c #ifdef slcr
c       subroutine se_init (numprocs, npdimi, npdimj, gl_ndimks, gl_ndimls, 
c    &                      gl_ndimis, gl_ndimjs, my_pe,
c    &                      mndis, medis, msdis, mwdis, data_ori, geo_ori)
c #else
	subroutine se_init (numprocs, npdimi, npdimj, gl_ndimis, gl_ndimjs,
     &                      gl_ndimks, gl_ndimls, my_pe,
     &                      mndis, medis, msdis, mwdis, data_ori, geo_ori)
c #endif

	use se_pe_info_ext
	use se_domain_info_ext
	use se_comm_info_ext
	use se_disp_info_ext
        use se_bndy_copy_info_ext
	use se_ori_ext
        use se_internal_util_module

	implicit none

        include 'mpif.h'

	integer, intent(in) :: numprocs, npdimi, npdimj
	integer, intent(in) :: gl_ndimis, gl_ndimjs, gl_ndimks, gl_ndimls
	integer, intent(in) :: my_pe
        integer, intent(in) :: mndis, medis, msdis, mwdis
        character (len = 2), intent(in) :: data_ori
        integer, intent(in) :: geo_ori

	integer :: i, j, leng
	character (len = 10) :: decompstr
        integer :: loci, locj, pos, allocate_status
        integer :: conv(8)

        integer :: se_world_group, se_io_group, se_worker_group, error
c       integer, allocatable :: ranks(:)
 
        data conv / 8, 1, 2, 7, 3, 6, 5, 4 /
 
        if ((data_ori .eq. "cr") .or. (data_ori .eq. "CR")) then
           se_nprow = npdimj
           se_npcol = npdimi
           se_gl_nrows = gl_ndimjs
           se_gl_ncols = gl_ndimis
        else
           se_nprow = npdimi
           se_npcol = npdimj
           se_gl_nrows = gl_ndimis
           se_gl_ncols = gl_ndimjs
        end if

        se_numworkers = se_nprow * se_npcol
        se_numiopes   = numprocs - se_numworkers

        se_my_pe = my_pe

        if (se_numiopes .gt. 0) then
           se_partition_mode = 2
           if (my_pe .lt. se_numiopes) then
              se_myworker_pe = -1
              se_myio_pe = my_pe
           else
              se_myworker_pe = my_pe - se_numiopes
              se_myio_pe = -1
           end if
        else
           se_partition_mode = 1
           se_myworker_pe = my_pe
           se_myio_pe = -1
        end if

        call mpi_comm_dup (mpi_comm_world, se_world_comm, error)

        if (se_partition_mode .eq. 2) then
           call mpi_comm_group (mpi_comm_world, se_world_group, error)

           allocate (ranks(se_numiopes), stat=allocate_status)

           do i = 1, se_numiopes
              ranks(i) = i - 1
           end do

           call mpi_group_incl (se_world_group, se_numiopes, ranks, 
     $                          se_io_group, error)
           call mpi_group_excl (se_world_group, se_numiopes, ranks, 
     $                          se_worker_group, error)

           call mpi_comm_create (mpi_comm_world, se_io_group, 
     $                           se_io_comm, error)
           call mpi_comm_create (mpi_comm_world, se_worker_group, 
     $                           se_worker_comm, error)
        else
           call mpi_comm_dup (mpi_comm_world, se_worker_comm, error)
        end if

        se_gl_nlays = gl_ndimks
        se_gl_nspcs = gl_ndimls

        se_mndis = mndis
        se_medis = medis
        se_msdis = msdis
        se_mwdis = mwdis

C       call pxfgetenv ('decompstr', 0, decompstr, leng, 0)

	decompstr = ' 1 1 0 0'
	leng = 8

        se_numprocs = numprocs
	se_numdim = leng / 2
        se_decompstr = decompstr

        se_data_ori = data_ori
        se_geo_ori = geo_ori

        se_ngb_pe = -1
        se_bngb_pe = -1

        if ((se_myworker_pe .eq. my_pe) .or. (se_myio_pe .eq. -1)) then
           loci = se_myworker_pe / se_npcol
           locj = mod (se_myworker_pe, se_npcol)
           pos = 0
           do i = loci-1, loci+1
              do j = locj-1, locj+1
                 if ((i .ne. loci) .or. (j .ne. locj)) then
                    pos = pos + 1
                    if ((i .ge. 0) .and. (i .lt. se_nprow) .and.
     &                  (j .ge. 0) .and. (j .lt. se_npcol)) then
                       se_ngb_pe(conv(pos)) = i * se_npcol + j
                    else
                       se_ngb_pe(conv(pos)) = -1
                    end if
                 end if
              end do
           end do

C -- initialize boundary scenario's ngb_pe

           if (se_myworker_pe .lt. se_npcol) then
              se_bngb_pe(5) = (se_nprow - 1) * se_npcol + se_myworker_pe
           end if

           if (se_myworker_pe .ge. (se_nprow - 1) * se_npcol) then
              se_bngb_pe(1) = mod(se_myworker_pe, se_npcol)
           end if

           if (mod(se_myworker_pe, se_npcol) .eq. 0) then
              se_bngb_pe(3) = se_myworker_pe + se_npcol - 1
           end if

           if (mod(se_myworker_pe, se_npcol) .eq. (se_npcol - 1)) then
              se_bngb_pe(7) = se_myworker_pe / se_npcol * se_npcol
           end if

           if (se_myworker_pe .eq. 0) then
              se_bngb_pe(4) = se_npcol * se_nprow - 1
           end if

           if (se_myworker_pe .eq. se_npcol - 1) then
              se_bngb_pe(6) = se_npcol * (se_nprow - 1)
           end if

           if (se_myworker_pe .eq. (se_nprow - 1) * se_npcol) then
              se_bngb_pe(2) = se_npcol - 1
           end if

           if (se_myworker_pe .eq. se_npcol * se_nprow - 1) then
              se_bngb_pe(8) = 0
           end if

        end if

	call se_dim_init

C -- figure out the low and high column and row index of the
C    original grid, respectively

        if (.not. allocated(se_gl_ind)) then
C -- allocate data
           allocate (se_gl_ind(2, 2, 0:se_numworkers-1), stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Allocation error in subroutine SE_INIT'
              stop
           end if
        end if

        se_gl_ind_ptr => se_gl_ind

        call se_generate_map (1, se_gl_nrows, 1, se_gl_ncols,
     &                        se_nprow, se_npcol, se_gl_ind_ptr)

        return
        end subroutine se_init 

C --------------------------------------------------------------------------
C Purpose:
C
C   1. determine which dimension(s) is/are distributed 
C   2. determine local PE work load of a distributed array 
C
C Revision history:
C
C   Orginal version: 8/10/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C
C Local variable description:
C
C   i       -- loop index
C   ldecomp -- an array indicates which dimension(s) is/are distributed
C   ldim    -- an array holds the global dimension
C   pos1    -- first distributed dimension
C   pos2    -- second distributed dimension
C   first   -- a boolean variable
C
C Include files:
C
C   se_pe_info.ext
C   se_domain_info.ext
C   se_comm_info.ext
C
C Subroutine/Function call:
C
C   cal_size
C
C --------------------------------------------------------------------------

	subroutine se_dim_init

        use se_domain_info_ext
        use se_pe_info_ext
        use se_comm_info_ext

	implicit none

	integer :: i
        integer :: ldecomp(4), ldim(4)
        integer :: pos1, pos2
        logical :: first

        first = .true.

        ldim(1) = se_gl_nrows
        ldim(2) = se_gl_ncols
        ldim(3) = se_gl_nlays
        ldim(4) = se_gl_nspcs

	pos1 = 0
	pos2 = 0

        read (se_decompstr, 10) (ldecomp(i), i=1,se_numdim)
 10	format (5i2)

C -- extract the distributed dimension(s)
	do i = 1, se_numdim
           if (ldecomp(i) .eq. 1) then
              if (first) then
                 pos1 = i
                 first = .false.
              else 
                 pos2 = i
              end if
           end if
        end do

C -- row x column
        if (((pos1 .eq. 1) .and. (pos2 .eq. 2)) .or.
     &      ((pos1 .eq. 1) .and. (pos2 .eq. 0))) then
            se_nrows = (se_gl_nrows - 1) / se_nprow + 1
            se_ncols = (se_gl_ncols - 1) / se_npcol + 1
            se_nlays = se_gl_nlays
            se_nspcs = se_gl_nspcs
C -- column x layer
        else if (((pos1 .eq. 2) .and. (pos2 .eq. 3)) .or.
     &           ((pos1 .eq. 2) .and. (pos2 .eq. 0))) then
            se_nrows = se_gl_nrows
            se_ncols = (se_gl_ncols - 1) / se_nprow + 1
            se_nlays = (se_gl_nlays - 1) / se_npcol + 1
            se_nspcs = se_gl_nspcs
C -- layer x row
        else if (((pos1 .eq. 3) .and. (pos2 .eq. 1)) .or.
     &           ((pos1 .eq. 3) .and. (pos2 .eq. 0))) then
            se_nrows = (se_gl_nrows - 1) / se_nprow + 1
            se_ncols = se_gl_ncols
            se_nlays = (se_gl_nlays - 1) / se_nprow + 1
            se_nspcs = se_gl_nspcs
C -- species x layer
        else if (((pos1 .eq. 4) .and. (pos2 .eq. 3)) .or.
     &           ((pos1 .eq. 4) .and. (pos2 .eq. 0))) then
            se_nrows = se_gl_nrows
            se_ncols = se_gl_ncols
            se_nlays = (se_gl_nlays - 1) / se_npcol + 1
            se_nspcs = (se_gl_nspcs - 1) / se_nprow + 1
C -- row x species
        else if ((pos1 .eq. 1) .and. (pos2 .eq. 4)) then
            se_nrows = (se_gl_nrows - 1) / se_nprow + 1
            se_ncols = se_gl_ncols
            se_nlays = se_gl_nlays
            se_nspcs = (se_gl_nspcs - 1) / se_npcol + 1
C -- column x species
        else
            se_nrows = se_gl_nrows
            se_ncols = (se_gl_ncols - 1) / se_nprow + 1
            se_nlays = se_gl_nlays
            se_nspcs = (se_gl_nspcs - 1) / se_npcol + 1
        end if

C -- determine what type of domain decomposition, logically, the first and 
C    second one become the row and column dimension, respectively
	if (pos2 .gt. 0) then
	   if (((pos1 .eq. 1) .and. (pos2 .eq. 3)) .or.
     &         ((pos1 .eq. 3) .and. (pos2 .eq. 4))) then
              ldecomp(pos2) = 1
              ldecomp(pos1) = 2
           else
              ldecomp(pos1) = 1
              ldecomp(pos2) = 2
           end if
        else
           ldecomp(pos1) = 1
        end if

C -- determine the actual size of each dimension locally
	call cal_size (ldim, ldecomp, se_my_nrows, se_my_ncols, 
     &                 se_my_nlays, se_my_nspcs)

        do i = 1, 4
           ldim(i) = ldim(i) + 1
        end do

C -- determine the actual size of each dimension locally when the global
C    dimension increases by one
	call cal_size (ldim, ldecomp, se_my_nrowsp1, se_my_ncolsp1, 
     &                 se_my_nlaysp1, se_my_nspcsp1)

        return
        end subroutine se_dim_init

C --------------------------------------------------------------------------
C Purpose:
C
C   to determine local work load dimensions each PE owns, of a distributed
C   array
C
C Revision history:
C
C   Orginal version: 8/10/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Parameter list:
C
C   IN:  ldim    -- global dimension values
C        ldecomp -- indicator of which dimension(s) is/are distributed
C   OUT: rows    -- number of rows in a PE
C        cols    -- number of columns in a PE 
C        lays    -- number of layers in a PE
C        spcs    -- number of species in a PE
C
C Local variable description:
C
C   i     -- loop index
C   temp  -- a temporary variable
C   tldim -- local copy of ldim
C
C Include files:
C
C   se_comm_info.ext
C   se_pe_info.ext
C --------------------------------------------------------------------------

	subroutine cal_size (ldim, ldecomp, rows, cols, lays, spcs)

	use se_comm_info_ext
	use se_pe_info_ext

	implicit none

	integer, intent(in) :: ldim(4), ldecomp(4)
	integer, intent(out) :: rows, cols, lays, spcs

        integer :: i, temp
	integer :: tldim(4)

	do i = 1, se_numdim
           tldim(i) = ldim(i)
           if (ldecomp(i) .eq. 1) then
C -- calculating the size of the logical row dimension
              temp = ldim(i)
              tldim(i) = tldim(i) / se_nprow
              temp = (temp - tldim(i) * se_nprow) * se_npcol
	      if (se_myworker_pe .lt. temp) then
                 tldim(i) = tldim(i) + 1
              end if
           else if (ldecomp(i) .eq. 2) then
C -- calculating the size of the logical column dimension
              temp = ldim(i)
              tldim(i) = tldim(i) / se_npcol
              temp = temp - tldim(i) * se_npcol
              if (mod(se_myworker_pe, se_npcol) .lt. temp) then
                 tldim(i) = tldim(i) + 1
              end if
           end if
        end do

	rows = tldim(1)
	cols = tldim(2)
	lays = tldim(3)
	spcs = tldim(4)

        return
        end subroutine cal_size

        end module se_init_module
