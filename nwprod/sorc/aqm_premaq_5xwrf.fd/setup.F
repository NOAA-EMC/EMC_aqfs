SUBROUTINE setup (inmetlays, inmetlaysht)

!-------------------------------------------------------------------------------
! Name:     Set Up the GRIB Domain Attributes
! Purpose:  Establishes bounds for GRIB meteorology input file post-processing.
! Revised:  19 May 2003  Original version.  (T. Otte)
!           04 Dec 2003  Removed processing of unused variable
!                        MET_SEASON.  (T. Otte)
!           30 Mar 2004  Added GRIBGRID to arguments for RDGRIBSPECS.
!                        Added call to CHKGRIB.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE metinfo
  USE premaqparm

  IMPLICIT NONE

  LOGICAL                      :: first       = .TRUE.
  CHARACTER*80                 :: gribflnm
  INTEGER                      :: gribgrid
  CHARACTER*24                 :: hdate
  INTEGER                      :: ierr
  INTEGER,       INTENT(OUT)   :: inmetlays   ( maxmetlays )
  INTEGER,       INTENT(OUT)   :: inmetlaysht ( maxmetlays )
  INTEGER                      :: istat
  INTEGER                      :: lyrct
  INTEGER                      :: lyrctht
  CHARACTER*16,  PARAMETER     :: pname       = 'SETUP'

  INTERFACE

    SUBROUTINE rdgribspecs (iunit, iuarr, gribflnm, hdate, gribgrid,  &
                            inmetlays, inmetlaysht, lyrct, lyrctht, ierr)
      USE metinfo
      IMPLICIT NONE
      CHARACTER*(*), INTENT(IN)    :: gribflnm
      INTEGER,       INTENT(OUT)   :: gribgrid
      CHARACTER*24,  INTENT(OUT)   :: hdate
      INTEGER,       INTENT(OUT)   :: ierr
      INTEGER,       INTENT(INOUT) :: inmetlays   ( maxmetlays )
      INTEGER,       INTENT(INOUT) :: inmetlaysht ( maxmetlays )
      INTEGER,       INTENT(INOUT) :: iuarr       ( 255 )
      INTEGER,       INTENT(INOUT) :: iunit
      INTEGER,       INTENT(INOUT) :: lyrct
      INTEGER,       INTENT(INOUT) :: lyrctht
    END SUBROUTINE rdgribspecs

  END INTERFACE

!-------------------------------------------------------------------------------
! Set up Fortran unit for first GRIB input file.
!-------------------------------------------------------------------------------

  iutmm = iutmmi

  OPEN (UNIT=iutmmi,  FILE=files_in(1), FORM='UNFORMATTED', STATUS='OLD',  &
        IOSTAT=istat, ERR=8000)

!-------------------------------------------------------------------------------
! Fill in list of variables to be collected from GRIB file.
!-------------------------------------------------------------------------------

  CALL filltable

!-------------------------------------------------------------------------------
! Read header of GRIB file and collect grid specifications.  Read through
! first hour of GRIB file to get vertical structure of input meteorology.
!-------------------------------------------------------------------------------

  iuarr(:) = 0
  iunit    = iutmmi
  gribflnm = ADJUSTL(files_in(1))

  inmetlays(:)   = -1
  inmetlaysht(:) = -1

  lyrct   = 0
  lyrctht = 0

  readhour1: DO

    CALL rdgribspecs (iunit, iuarr, gribflnm, hdate, gribgrid,  &
                      inmetlays, inmetlaysht, lyrct, lyrctht, ierr)

    IF ( ierr == 1 ) EXIT readhour1  ! end of file reached

  ENDDO readhour1

!-------------------------------------------------------------------------------
! Reset input file so that the first record will be processed properly.
!-------------------------------------------------------------------------------

  CLOSE (iutmm)
  iuarr(:) = 0

!-------------------------------------------------------------------------------
! Set up grid geometry for I/O API header.
!-------------------------------------------------------------------------------

  CALL setgriddefs (lyrct, lyrctht)

!-------------------------------------------------------------------------------
! Verify that the incoming GRIB grid matches a known AQF grid.
!-------------------------------------------------------------------------------

  CALL chkgrib (gribgrid)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmmi, TRIM(files_in(1)), istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: SETUP',                      &
              /, 1x, '***   ERROR OPENING GRIB FILE ON UNIT ', i3, &
              /, 1x, '***   GRIB FILE NAME = ', a,                 &
              /, 1x, '***   IOSTAT = ', i4,                        &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup
