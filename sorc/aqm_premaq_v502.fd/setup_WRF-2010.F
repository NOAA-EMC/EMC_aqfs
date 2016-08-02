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
  INTEGER,       INTENT(OUT)   :: inmetlays   ( maxmetlays )
  INTEGER,       INTENT(OUT)   :: inmetlaysht ( maxmetlays )
  INTEGER                      :: istat, ierr
  INTEGER                      :: lyrct
  INTEGER                      :: lyrctht
  CHARACTER*16,  PARAMETER     :: pname       = 'SETUP'

  CHARACTER*80                 :: gribflnmi


  INTERFACE

    SUBROUTINE rdgribspecs (iunit, gribflnm, gribgrid,  &
                            inmetlays, inmetlaysht, lyrct, lyrctht,   &
                            iuniti, gribflnmi)
      USE metinfo
      IMPLICIT NONE
      CHARACTER*(*), INTENT(IN)    :: gribflnm
      INTEGER,       INTENT(OUT)   :: gribgrid
      INTEGER,       INTENT(INOUT) :: inmetlays   ( maxmetlays )
      INTEGER,       INTENT(INOUT) :: inmetlaysht ( maxmetlays )
      INTEGER,       INTENT(IN)    :: iunit
      INTEGER,       INTENT(INOUT) :: lyrct
      INTEGER,       INTENT(INOUT) :: lyrctht

      CHARACTER*(*), INTENT(IN)    :: gribflnmi
      INTEGER,       INTENT(IN)    :: iuniti

    END SUBROUTINE rdgribspecs

  END INTERFACE

!-------------------------------------------------------------------------------
! Fill in list of variables to be collected from GRIB file.
!-------------------------------------------------------------------------------

  CALL filltable

!-------------------------------------------------------------------------------
! Set up Fortran unit for first GRIB input file.
!-------------------------------------------------------------------------------

  gribflnm  = ADJUSTL(files_in(1))
  gribflnmi = ADJUSTL(index_in(1))

  inmetlays(:)   = -1
  inmetlaysht(:) = -1

  lyrct   = 0
  lyrctht = 0

!-------------------------------------------------------------------------------
! Open grib file and the related index file
!-------------------------------------------------------------------------------

  ierr = 0
  CALL baopenr(iunit, gribflnm, ierr)
  IF (ierr /= 0 ) THEN
     go to 8001
  ENDIF

  CALL baopenr(iuniti, gribflnmi, ierr)
  IF (ierr /= 0 ) THEN
     go to 8002
  ENDIF

!-------------------------------------------------------------------------------
! Read header of GRIB file and collect grid specifications.  Read through
! first hour of GRIB file to get vertical structure of input meteorology.
!-------------------------------------------------------------------------------

  CALL rdgribspecs (iunit, gribflnm, gribgrid,  &
                    inmetlays, inmetlaysht, lyrct, lyrctht,   &
                    iuniti, gribflnmi)

!-------------------------------------------------------------------------------
! Reset input file so that the first record will be processed properly.
!-------------------------------------------------------------------------------

!  CLOSE ( iunit)
!  CLOSE (iuniti)

  CALL baclose(iunit,ierr)
  IF (ierr /= 0 ) THEN
     go to 8003
  ENDIF
  
  CALL baclose(iuniti, ierr)
  IF (ierr /= 0 ) THEN
     go to 8004
  ENDIF  
!-------------------------------------------------------------------------------
! Set up grid geometry for I/O API header.
!-------------------------------------------------------------------------------

  CALL setgriddefs (lyrct, lyrctht)

!-------------------------------------------------------------------------------
! Verify that the incoming GRIB grid matches a known AQF grid.
!-------------------------------------------------------------------------------

  CALL chkgrib (gribgrid, lyrct)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8001 WRITE (6,9001) iunit, gribflnm, ierr
      GOTO 1001

 8002 WRITE (6,9002) iuniti, gribflnmi, ierr
      GOTO 1001
      
 8003 WRITE (6,9003) iunit, gribflnm, ierr
      GOTO 1001

 8004 WRITE (6,9004) iuniti, gribflnmi, ierr
      GOTO 1001      

 9001 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP',                              &
              /, 1x, '***   ERROR OPENING GRIB FILE ON UNIT ', i3,         &
              /, 1x, '***   GRIB FILE NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9002 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP',                              &
              /, 1x, '***   ERROR OPENING GRIB INDX ON UNIT ', i3,         &
              /, 1x, '***   GRIB INDX NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))
	      
 9003 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP',                              &
              /, 1x, '***   ERROR CLOSING GRIB FILE ON UNIT ', i3,         &
              /, 1x, '***   GRIB FILE NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9004 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP',                              &
              /, 1x, '***   ERROR CLOSING GRIB INDX ON UNIT ', i3,         &
              /, 1x, '***   GRIB INDX NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))	      

 1001 CALL graceful_stop (pname)
      RETURN


END SUBROUTINE setup
