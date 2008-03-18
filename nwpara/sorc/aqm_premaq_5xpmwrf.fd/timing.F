      module timing

        implicit none

        private
        include 'mpif.h'

        public :: f_hpmstart, f_hpmstop
        public :: set_rank, init_timing_variables, print_times

        integer, public, parameter :: ntotal = 300

        character(len=12) :: label(0:ntotal)

        real*8, private :: temptime(0:ntotal), times(0:ntotal)
        integer, private :: num_calls(0:ntotal)
        integer :: myrank

      contains

!***************************************************************************!
      subroutine f_hpmstart(id,label_id)

        implicit none

        integer :: id
        character (len = *) :: label_id

        if (temptime(id) > 0.d0) then
          print *, label(id), ' was already started without a stop!!!'
          stop
        endif

        num_calls(id) = num_calls(id) + 1
        temptime(id) = MPI_WTIME()
        label(id) = label_id

      end subroutine f_hpmstart

!***************************************************************************!
      subroutine f_hpmstop(id)

        implicit none

        integer :: id

        if (temptime(id) == 0.d0) then
          print *, label(id), id, ' was never started!!!'
          stop
        endif

        times(id) = times(id) + MPI_WTIME() - temptime(id)
        temptime(id) = 0.d0

      end subroutine f_hpmstop

!***************************************************************************!
      subroutine set_rank(rank)

        implicit none

        integer :: rank

        myrank = rank

      end subroutine set_rank

!***************************************************************************!
      subroutine init_timing_variables()

        implicit none

        num_calls = 0
        times = 0.d0
        temptime = 0.d0
        label = '            '

      end subroutine init_timing_variables

!***************************************************************************!

      subroutine print_times

        implicit none

        logical :: write_all_values = .false.
        logical :: end_of_file = .false.
        character(len=20) :: filename, tfile
        character(len=80) :: buf
        integer, parameter :: in_unit = 10, out_unit = 11
        integer :: subroutine_id, i, sub_id, tempint, index, stat, m, n, l, ll
        real*8 :: tempreal
        logical :: done
        integer :: seq(ntotal) 
        integer :: lvl(ntotal)
        character (len = 40) :: myfmt1, myfmt2, myfmt3
        logical :: blankline

        ll = 0
        do i = 1, ntotal
           if (label(i)(1:1) .ne. ' ') then
              ll = max(ll, len(trim(label(i))))
           end if
        end do
        ll = ll + max(12 - ll, 0)

        seq = -1

        open(unit=in_unit,file='infile')
        i = 0
        do while (.not. end_of_file)
           read (in_unit, 10, iostat=stat) buf
 10        format (a80)
           if (stat .ne. 0) then
              end_of_file = .true.
           else
              i = i + 1
              if (i .eq. 1) then
                 call compute_fmt (buf, ll, myfmt1, myfmt2, l)
              end if
              if (buf(1:2) .eq. '  ') then
                 seq(i) = 0
              else
                 read (buf, myfmt1) lvl(i), seq(i)
              end if
           end if
        end do
        close(in_unit)

        write (tfile, 18) myrank
 18     format (i3.3)
        filename = trim(tfile) // '.dat'

        open(unit=out_unit,file=filename)

        write(out_unit,myfmt2) myrank, times(myrank), label(myrank), &
                               num_calls(myrank), myrank
        write(out_unit, *)

        blankline = .false.
        done = .false.
        i = 0
        do while (.not. done)
           i = i + 1
           if (seq(i) .eq. -1) then
              done = .true.
           else if (seq(i) .eq. 0) then
              if (.not. blankline) then
                 write(out_unit, *)
                 blankline = .true.
              end if
           else
              index = seq(i)
              if (label(index) .ne. '            ') then
                 blankline = .false.

                 m = (lvl(i) - 1) * 2 + ll
                 n = l - m

                 write (myfmt3, 30) '(i4, f15.2, 2x, a', m, ', ', n, 'x, i9, i5)'
 30              format (a17, i2, a2, i2, a10)

                 write(out_unit,myfmt3) myrank, times(index), label(index), &
                                        num_calls(index), index
              end if
           end if
        end do

        close (out_unit)

      end subroutine print_times

!***************************************************************************!

      subroutine compute_fmt (buf, ll, fmt1, fmt2, l)

      implicit none

      integer, intent(in) :: ll
      character (len = 80), intent(in) :: buf
      character (len = 40), intent(out) :: fmt1, fmt2
      integer, intent(out) :: l

      integer :: stage, i, j

      i = 3
      stage = 0
      do while (stage .ne. 3)
         i = i + 1
         if ((stage .eq. 0) .and. (buf(i:i) .ne. ' ')) then
            stage = 1
         else if ((stage .eq. 1) .and. (buf(i:i) .eq. ' ')) then
            stage = 2
         else if ((stage .eq. 2) .and. (buf(i:i) .ne. ' ')) then
            stage = 3
         end if
      end do

      j = i - 4 - 4
      i = j + 3
      l = j + 0

      write (fmt1, 20) '(i2, ', i, 'x, i4)'
 20   format (a5, i2, a6)

      j = j - ll + 0
      write (fmt2, 22) '(i4, f15.2, 2x, a', ll, ', ', j, 'x, i9, i5)'
 22   format (a17, i2, a2, i2, a10)

      end subroutine compute_fmt

      end module timing
