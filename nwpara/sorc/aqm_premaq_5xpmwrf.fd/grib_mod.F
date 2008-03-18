module module_grib
!
! Machine wordsize must be known for the various unpacking routines to work.
! Machine wordsize is set through CPP Directives.
! Use options -DBIT32 (for 32-bit word-size) or -DBIT64 (for 64-bit wordsize) 
! for the CPP pass of the compiler.
!
#if defined (BIT32)
  integer, parameter :: MWSIZE = 32 ! Machine word size in bits
#elif defined (BIT64)
  integer, parameter :: MWSIZE = 64 ! Machine word size in bits
#endif


! Array GREC holds a single packed GRIB record (header and all).
! Array BITMAP holds the bitmap (if a bitmap was used).
!
! For some reason, the cray does not like grec to be allocatable.
!
#if defined (CRAY) 
  integer, dimension(100000) :: grec
  integer, dimension(100000) :: bitmap
#else
  integer, allocatable, save, dimension(:) :: grec
  integer, allocatable, save, dimension(:) :: bitmap
#endif

! SEC0 holds the Section 0 header information
! SEC1 holds the Section 1 header information
! SEC2 holds the Section 2 header information
! SEC3 holds the Section 3 header information
! SEC4 holds the Section 4 header information
! XEC4 holds floating-point Section 4 header information

  integer, dimension(2) :: sec0
  integer, dimension(100) :: sec1
  integer, dimension(10) :: sec2
  integer, dimension(10) :: sec3
  integer, dimension(10) :: sec4
  real, dimension(1) :: xec4

  integer :: sevencount = 0
  logical :: recognized_grid

! INFOGRID holds integer values defining the grid.
! GRIDINFO holds floating-point values definint the grid

  integer, dimension(40) :: infogrid
  real, dimension(40) :: gridinfo

  integer :: ied
  real, parameter :: pi = 3.1415926534
  real, parameter :: degran = pi/180.
  real, parameter :: raddeg = 1./degran

  real :: glat1, glon1, gtrue1, gtrue2, grrth, gx1, gy1, gkappa

contains
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  integer function gribsize(trec, ilen)
!-----------------------------------------------------------------------------!
! Return the size of a single GRIB record.                                    !
!                                                                             !
! Input:                                                                      !
!    TREC: At least a portion of the complete GRIB record.                    !
!    ILEN: The size of array TREC.                                            !
!                                                                             !
! Output                                                                      !
!    GRIBSIZE: The size of the full GRIB record                               !
!                                                                             !
! Side Effects:                                                               !
!    * Module variable IED is set to the GRIB Edition number.                 !
!    * STOP, if not GRIB Edition 0 or 1                                      !
!                                                                             !
! Externals                                                                   !
!    GBYTE                                                                    !
!                                                                             !
!-----------------------------------------------------------------------------!
    implicit none
    integer :: ilen
    integer, dimension(ilen) :: trec
    integer :: isz0 = 32
    integer :: isz1 = 0
    integer :: isz2 = 0
    integer :: isz3 = 0
    integer :: isz4 = 0
    integer :: isz5 = 32
    integer :: iflag

! Unpack the GRIB Edition number, located in the eighth byte (bits 57-64)     !
! of array TREC.                                                              !

    call gbyte(trec, ied, 56, 8)

! GRIB Edition 1 has the size of the whole GRIB record right up front.        !

    if (ied.eq.1) then
       ! Find the size of the whole GRIB record
       call gbyte(trec, gribsize, 32, 24)

! GRIB Edition 0 does not include the total size, so we have to sum up        !
! the sizes of the individual sections                                        !

    elseif (ied.eq.0) then
       ! Find the size of section 1.
       call gbyte(trec, isz1, isz0, 24)
       isz1 = isz1 * 8
       call gbyte(trec, iflag, isz0+56, 8)
       if ((iflag.eq.128).or.(iflag.eq.192)) then ! section 2 is there
          ! Find the size of section 2.
          call gbyte(trec, isz2, isz0+isz1, 24)
          isz2 = isz2 * 8
       endif
       if ((iflag.eq.64).or.(iflag.eq.192)) then ! Section 3 is there
          ! Find the size of section 3.
          call gbyte(trec, isz3, isz0+isz1+isz2, 24)
          isz3 = isz3 * 8
       endif
       ! Find the size of section 4.
       call gbyte(trec, isz4, isz0+isz1+isz2+isz3, 24)
       isz4 = isz4 * 8

       ! Total the sizes of sections 0 through 5.
       gribsize = (isz0+isz1+isz2+isz3+isz4+isz5) / 8

    else
       write(*, '("GRIB Edition: ", I5)') ied
       stop
    endif
  end function gribsize
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  subroutine findgrib(nunit, isize, ierr)

!-----------------------------------------------------------------------------!
!                                                                             !
! Find the string "GRIB", which starts off a GRIB record.                     !
!                                                                             !
! Input:                                                                      !
!    NUNIT:  The C unit to read from.  This should already be opened.         !
!                                                                             !
! Output:                                                                     !
!    ISIZE:  The size in bytes of one complete GRIB Record                    !
!    IERR:   Error flag,                                                      !
!              0 : No error or end-of-file on reading                         !
!              1 : Hit the end of file                                        !
!              2 : Error on reading                                           !
!                                                                             !
! Side effects:                                                               !
!   * The pointer to C unit NUNIT is set to the beginning of the next         !
!     GRIB record.                                                            !
!   * The first time FINDGRIB is called, the integer GTEST is set to          !
!     a value equivalent to the string 'GRIB'                                 !
!                                                                             !
! Modules:                                                                    !
!     MODULE_GRIB                                                             !
!                                                                             !
! Externals:                                                                  !
!     BNREAD                                                                  !
!     BNSEEK                                                                  !
!     GRIBSIZE                                                                !
!                                                                             !
!-----------------------------------------------------------------------------!
    implicit none
    integer, intent(in) :: nunit
    integer, intent(out) :: isize
    integer, intent(out) :: ierr

    integer, parameter :: LENTMP=100
    integer, dimension(lentmp) :: trec

    integer :: isz

    integer, save :: gtest = 0
    integer :: itest

! Set the integer variable GTEST to hold the integer representation of the
! character string 'GRIB'.   This integer variable is later compared to
! integers we read from the GRIB file, to find the beginning of a GRIB record.

    if (gtest.eq.0) then
       if (mwsize.eq.32) then
          gtest = transfer('GRIB', gtest)
       elseif(mwsize.eq.64) then
          call gbyte(char(0)//char(0)//char(0)//char(0)//'GRIB', gtest, 0, mwsize)
       endif
    endif
    ierr = 0

    LOOP : DO
! Read LENTMP bytes into holding array TREC.
       call bnread(nunit, trec, lentmp, isz, ierr, 0)
       if (ierr.eq.1) then
          return
       elseif (ierr.eq.2) then
          write(*,'("Error reading GRIB: IERR = ", I2)') ierr
          return
       endif
! Reposition the file pointer back to where we started.
       call bnseek(nunit, -isz, 0, 0)

! Compare the first four bytes of TREC with the string 'GRIB' stored in 
! integer variable GTEST.
       if (mwsize.eq.32) then
          if (trec(1) == gtest) exit LOOP
       elseif (mwsize.eq.64) then
          call gbyte(trec, itest, 0, 32)
          if (itest == gtest) exit LOOP
       endif

! Advance the file pointer one byte.
       call bnseek(nunit, 1, 0, 0)

    ENDDO LOOP

#if defined (DEC) || defined (ALPHA)
      call swap4(trec, isz)
#endif
    isize = gribsize(trec, isz)

  end subroutine findgrib
!
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
!
!
! Trigonometric functions which deal with degrees, rather than radians:
!
  real function sind(theta)
    real :: theta
    sind = sin(theta*degran)
  end function sind
  real function cosd(theta)
    real :: theta
    cosd = cos(theta*degran)
  end function cosd
  real function tand(theta)
    real :: theta
    tand = tan(theta*degran)
  end function tand
  real function atand(x)
    real :: x
    atand = atan(x)*raddeg
  end function atand
  real function atan2d(x,y)
    real :: x,y
    atan2d = atan2(x,y)*raddeg
  end function atan2d
  real function asind(x)
    real :: x
    asind = asin(x)*raddeg
  end function asind
  real function acosd(x)
    real :: x
    acosd = acos(x)*raddeg
  end function acosd

!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
end module module_grib
