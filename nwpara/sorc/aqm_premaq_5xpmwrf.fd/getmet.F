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
  USE premaqparm

  IMPLICIT NONE

  CHARACTER*24,  INTENT(IN)    :: aqf_now
  CHARACTER*80                 :: field
  LOGICAL                      :: gotone
  CHARACTER*80                 :: gribflnm
  CHARACTER*24                 :: hdate
  INTEGER                      :: ierr
  INTEGER,       SAVE          :: index      = 1
  INTEGER                      :: iparm
  INTEGER                      :: ipcp
  INTEGER                      :: ktype
  INTEGER                      :: lvl1
  INTEGER                      :: lvl2
  CHARACTER*16,  PARAMETER     :: pname      = 'GETMET'

  INTERFACE

    SUBROUTINE rdgrib (iunit, gribflnm, hdate, scr2d, ierr, iuarr, gotone,  &
                       iparm, ktype, lvl1, lvl2, ipcp)
      IMPLICIT NONE
      LOGICAL,       INTENT(INOUT) :: gotone
      CHARACTER*(*), INTENT(IN)    :: gribflnm
      CHARACTER*24,  INTENT(OUT)   :: hdate
      INTEGER,       INTENT(OUT)   :: ierr
      INTEGER,       INTENT(OUT)   :: iparm
      INTEGER,       INTENT(OUT)   :: ipcp
      INTEGER,       INTENT(INOUT) :: iuarr      ( 255 )
      INTEGER,       INTENT(INOUT) :: iunit
      INTEGER,       INTENT(OUT)   :: ktype
      INTEGER,       INTENT(OUT)   :: lvl1
      INTEGER,       INTENT(OUT)   :: lvl2
      REAL,          INTENT(OUT)   :: scr2d      ( : , : )
    END SUBROUTINE rdgrib

  END INTERFACE

!-------------------------------------------------------------------------------
! Reset the data flags to false to indicate that no data have been collected
! for this time period.
!-------------------------------------------------------------------------------

  CALL reset_flags

!-------------------------------------------------------------------------------
! Read meteorology information for this time period.
!-------------------------------------------------------------------------------

  gribflnm = ADJUSTL(files_in(index))

  readfile1: DO

    gotone = .FALSE.
    CALL rdgrib (iunit, gribflnm, hdate, scr2d, ierr, iuarr, gotone,  &
                 iparm, ktype, lvl1, lvl2, ipcp)

    IF ( ierr == 1 ) THEN  ! hit end of file
      index = index + 1
      iunit = iunit + 1
      EXIT readfile1
    ENDIF

    IF ( hdate /= aqf_now ) THEN      ! *** see notes at top of routine ***
  !    IF ( ( iparm /= 121 ) .AND.  &  ! latent heat flux
  !         ( iparm /= 122 ) .AND.  &  ! sensible heat flux
  !         ( iparm /= 205 ) .AND.  &  ! downward longwave radiative flux
  !         ( iparm /= 211 ) .AND.  &  ! upward shortwave radiative flux
  !         ( iparm /= 212 ) ) THEN    ! upward longwave radiative flux
        GOTO 8000
  !    ENDIF
    ENDIF

    IF ( gotone ) THEN
       CALL fill_metarys (iparm, ktype, lvl1, lvl2, ipcp)
    ELSE
       CYCLE readfile1
    ENDIF

  ENDDO readfile1

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

 8000 WRITE (6,9000) hdate, aqf_now, iparm
      GOTO 1001

 8100 WRITE (6,9100) metvert
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GETMET',                     &
              /, 1x, '***   UNEXPECTED VALID DATE IN INPUT FIELD', &
              /, 1x, '***   DATE IN FILE  = ', a,                  &
              /, 1x, '***   EXPECTED DATE = ', a,                  &
              /, 1x, '***   INPUT FIELD (gcode) iparm = ', a,      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GETMET',                     &
              /, 1x, '***   WORKS FOR HYDROSTATIC SIGMA INPUT',    &
              /, 1x, '***   METVERT = ', i3,                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getmet
