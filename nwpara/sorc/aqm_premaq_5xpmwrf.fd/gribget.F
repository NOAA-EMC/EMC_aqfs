subroutine gribget(nunit, ierr)
  use module_grib
!-----------------------------------------------------------------------------!
!                                                                             !
! Read a single GRIB record, with no unpacking of any header or data fields.  !
!                                                                             !
! Input:                                                                      !
!     NUNIT:  C unit number to read from.  This should already be open.       !
!                                                                             !
! Output:                                                                     !
!     IERR: Error flag, Non-zero means there was a problem with the read.     !
!                                                                             !
! Side Effects:                                                               !
!        The array GREC is allocated, and filled with one GRIB record.        !
!        The C unit pointer is moved to the end of the GRIB record just read. !
!                                                                             !
! Modules:                                                                    !
!       MODULE_GRIB                                                           !
!                                                                             !
! Externals:                                                                  !
!       FINDGRIB                                                              !
!       BNREAD                                                                !
!                                                                             !
!-----------------------------------------------------------------------------!

  implicit none

  integer :: nunit
  integer :: ierr
  integer :: isz, isize

! Position the file pointer at the beginning of the GRIB record.
  call findgrib(nunit, isize, ierr)
  if (ierr.ne.0) return

! Allocate the GREC array to be able to hold the data

#if defined (CRAY)
#else
  allocate(grec((isize+(mwsize/8-1))/(mwsize/8)))
#endif

! Read the full GRIB record.

  call bnread(nunit, grec, isize, isz, ierr, 1)

#if defined (DEC) || defined (ALPHA) 
      call swap4(grec, isz)
#endif


end subroutine gribget
