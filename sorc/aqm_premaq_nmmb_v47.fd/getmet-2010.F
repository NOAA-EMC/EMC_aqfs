SUBROUTINE getmet (aqf_now)

!-------------------------------------------------------------------------------
! Name:     Get Meteorology
! Purpose:  Get input meteorology data.
! Revised:  19 May 2003  Original version.  (T. Otte)
!           05 Jan 2004  Added variable IPCP to keep track of when Eta
!                        accumulated precipitation buckets are dumped. (T. Otte)
!           30 Mar 2004  Added error checking on date in input file.
!                        Allow for time-stamp error on fluxes in versions of
!                        Eta prior to 16 Mar 2004 implementation at NCEP.  
!                        Removed INFIL in calls to RDGRIB and FILL_METARYS, and
!                        added TBLNUM to call to RDGRIB.  (T. Otte)
!           11 Feb 2005  Remove dummy TBLNUM from RDGRIB.          (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE file
  USE metinfo
  USE metvars
  USE table, ONLY : maxvar
  USE premaqparm


  IMPLICIT NONE

  CHARACTER*24,  INTENT(IN)    :: aqf_now
  CHARACTER*80                 :: gribflnm
  CHARACTER*80                 :: gribflnmi
  CHARACTER*24                 :: hdate
  INTEGER                      :: i, ierr
  INTEGER,       SAVE          :: index      = 1
  INTEGER,       SAVE          :: unit       = 10
  INTEGER,       SAVE          :: uniti      = 510

  CHARACTER*16,  PARAMETER     :: pname      = 'GETMET'

  INTERFACE

    SUBROUTINE rdgrib (iunit, gribflnm, iuniti, gribflnmi, i, hdate)
      IMPLICIT NONE
      CHARACTER*(*), INTENT(IN)    :: gribflnm, gribflnmi
      CHARACTER*24,  INTENT(IN)    :: hdate
      INTEGER,       INTENT(IN)    :: iunit, iuniti
      INTEGER,       INTENT(IN)    :: i
    END SUBROUTINE rdgrib

  END INTERFACE

!-------------------------------------------------------------------------------
! Reset the data flags to false to indicate that no data have been collected
! for this time period.
!-------------------------------------------------------------------------------

  CALL reset_flags

!-------------------------------------------------------------------------------
! set up meteorology processing information at this time period.
!-------------------------------------------------------------------------------

  gribflnm = ADJUSTL(files_in(index))
  gribflnmi= ADJUSTL(index_in(index))

!-------------------------------------------------------------------------------
! Open grib file and the related index file
!-------------------------------------------------------------------------------

  ierr = 0

  CALL baopenr(unit, gribflnm, ierr)
  IF (ierr /= 0 ) THEN
     go to 8001
  ENDIF

  CALL baopenr(uniti, gribflnmi, ierr)
  IF (ierr /= 0 ) THEN
     go to 8002
  ENDIF

!-------------------------------------------------------------------------------
! Read meteorology information for this time period through each variables
!-------------------------------------------------------------------------------

  varloop: DO i = 1, maxvar

     CALL rdgrib (unit, gribflnm, uniti, gribflnmi, i, aqf_now)

  ENDDO varloop

!-------------------------------------------------------------------------------
! close grib file and the related index file after reaching end of data
!-------------------------------------------------------------------------------

  close ( unit)
  close (uniti)

  index = index + 1

  unit  =  unit + 1
  uniti = uniti + 1

!-------------------------------------------------------------------------------
! Ensure all of the fields we need are filled.
!-------------------------------------------------------------------------------

  CALL check_flags

!-------------------------------------------------------------------------------
! Calculate 3-D pressure from hydrostatic sigma and surface pressure.
! **** The following can be commemtted out when  (hybrid lvl P)=(mid lvl P)
!-------------------------------------------------------------------------------

  IF ( metvert == 1 ) THEN
    CALL getprs
  ELSE
    GOTO 8100
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8001 WRITE (6,9001) unit, gribflnm, ierr
      GOTO 1001

 8002 WRITE (6,9002) uniti, gribflnmi, ierr
      GOTO 1001

 8100 WRITE (6,9100) metvert
      GOTO 1001

 9001 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETMET',                             &
              /, 1x, '***   ERROR OPENING GRIB FILE ON UNIT ', i3,         &
              /, 1x, '***   GRIB FILE NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9002 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETMET',                             &
              /, 1x, '***   ERROR OPENING GRIB INDX ON UNIT ', i3,         &
              /, 1x, '***   GRIB INDX NAME = ', a,                         &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GETMET',                     &
              /, 1x, '***   WORKS FOR HYDROSTATIC SIGMA INPUT',    &
              /, 1x, '***   METVERT = ', i3,                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getmet
