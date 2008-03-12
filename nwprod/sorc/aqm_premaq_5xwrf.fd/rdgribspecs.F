SUBROUTINE rdgribspecs (iunit, iuarr, gribflnm, hdate, gribgrid,  &
                        inmetlays, inmetlaysht, lyrct, lyrctht, ierr)

!-------------------------------------------------------------------------------
! Name:     Read GRIB Specifications
! Purpose:  Reads grid geometry and date information from GRIB record.
! Notes:    Largely taken from MM5's REGRID/pregrid/grib.misc/rd_grib.F.
!
! Argument list:
!    Input:
!       IUNIT   : "Unit Number" to open and read from.  Not really a Fortran
!                 unit number, since we don't do Fortran I/O for the GRIB
!                 files.  Nor is it a UNIX File Descriptor returned from a C
!                 OPEN statement.  It's really just an array index to the
!                 array (IUARR) where the UNIX File Descriptor values are
!                 stored.
!       GRIBFLNM: File name to open, if it is not already open.
!       IUARR   : Array to hold UNIX File descriptors retured from a C open
!                 statement.  If the value of IUARR(IUNIT) is zero, then the
!                 file GRIBFLNM must be opened, and the value of IUARR(IUNIT)
!                 becomes the UNIX File descriptor to read from.
!
!    Output:
!       HDATE    : The 19-character date of the field to process.
!       IERR     : Error flag: 0 - no error on read from GRIB file.
!                              1 - Hit the end of the GRIB file.
!                              2 - The file GRIBFLNM we tried to open does
!                                  not exist.
!
! Revised:  ?? ??? 1998  Original version.  (K. Manning, NCAR)
!           16 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           03 Dec 2003  Modified settings of grid geometry variables to
!                        eliminate real number imprecision issues.  (T. Otte)
!           30 Mar 2004  Added variable GRIBGRID.  (T. Otte)
!-------------------------------------------------------------------------------

  USE gridinfo
  USE date_pack
  USE metinfo
  USE premaqparm

  IMPLICIT NONE

  INTEGER                      :: day
  INTEGER                      :: fcst
  CHARACTER*(*), INTENT(IN)    :: gribflnm 
  INTEGER,       INTENT(OUT)   :: gribgrid
  REAL                         :: ginfo       ( 40 )
  CHARACTER*24,  INTENT(OUT)   :: hdate
  INTEGER                      :: hour
  INTEGER                      :: icc
  INTEGER,       INTENT(OUT)   :: ierr
  INTEGER                      :: igherr
  INTEGER                      :: ihold
  INTEGER                      :: infogrid    ( 40 )
  INTEGER,       INTENT(INOUT) :: inmetlays   ( maxmetlays )
  INTEGER,       INTENT(INOUT) :: inmetlaysht ( maxmetlays )
  INTEGER                      :: iparm
  INTEGER,       INTENT(INOUT) :: iuarr       ( 255 )
  INTEGER,       INTENT(INOUT) :: iunit              ! for C pointer
  INTEGER                      :: iyy
  INTEGER                      :: ksec1       ( 100 )
  INTEGER                      :: ksec2       ( 10 )
  INTEGER                      :: ktype
  INTEGER                      :: layer
  INTEGER,       INTENT(INOUT) :: lyrct
  INTEGER,       INTENT(INOUT) :: lyrctht
  INTEGER                      :: minute
  INTEGER                      :: month
  INTEGER                      :: n
  LOGICAL                      :: newlyr
  INTEGER                      :: nunit
  CHARACTER*16,  PARAMETER     :: pname        = 'RDGRIBSPECS'
  INTEGER                      :: year

!-------------------------------------------------------------------------------
! If the file GRIBFLNM has not been opened, then IUARR(IUNIT) should be Zero.
! In this case, open the file GRIBFLNM, and store the UNIX File descriptor
! in to IUARR(IUNIT).  This way, we'll know what UNIX File descriptor to use
! next time we call this subroutine.
!-------------------------------------------------------------------------------

  ierr = 0

  IF ( iuarr(iunit) == 0 ) THEN
    CALL copen (iunit, nunit, gribflnm, 1, ierr, 1)
    IF ( ierr /= 0 ) THEN
      CALL deallogrib
      ierr = 2
      RETURN
    ENDIF
    iuarr(iunit) = nunit
  ENDIF

!-------------------------------------------------------------------------------
! Read a single GRIB record, but do no unpacking now.
!-------------------------------------------------------------------------------

  CALL gribget (iuarr(iunit), ierr)

  IF ( ierr /= 0 ) THEN
    CALL deallogrib
    RETURN
  ENDIF

!-------------------------------------------------------------------------------
! Unpack the header information.
!-------------------------------------------------------------------------------

  CALL gribheader (igherr)
  IF ( igherr /= 0 ) THEN
    CALL deallogrib
    RETURN
  ENDIF

!-------------------------------------------------------------------------------
! Copy header information to arrays KSEC1, KSEC2, INFOGRID, and GRIDINFO.
!-------------------------------------------------------------------------------

  CALL get_sec1 (ksec1)
  CALL get_sec2 (ksec2)
  CALL get_gridinfo (infogrid, ginfo)

!-------------------------------------------------------------------------------
! Build the 24-character date string, based on GRIB header date and time
! information, including forecast time information.
!-------------------------------------------------------------------------------

  icc    = ksec1(22)   ! century of the data
  iyy    = ksec1(11)   ! two-digit year of the data
  month  = ksec1(12)   ! month of the data
  day    = ksec1(13)   ! day of the data
  hour   = ksec1(14)   ! hour of the data
  minute = ksec1(15)   ! minute of the data

  IF ( ksec1(19) == 3 ) THEN
    fcst = ( ksec1(17) + ksec1(18) ) / 2
  ELSE IF ( ( ksec1(19) == 4 ) .OR. ( ksec1(19) == 5 ) ) THEN
    fcst = ksec1(18)
  ELSE
    fcst = ksec1(17)
  ENDIF

  IF ( iyy == 00 ) THEN
    year = icc * 100
  ELSE
    year = (icc-1) * 100 + iyy
  ENDIF

  WRITE (hdate, '(i4, a, i2.2, a, i2.2, a, i2.2, a, i2.2, a)') &
    year, '-', month, '-', day, '-', hour, ':', minute, ':00.0000'

  CALL geth_newdate (hdate, hdate, 3600*fcst)

!-------------------------------------------------------------------------------
! Extract vertical level information.  This program will work with input
! fields on either sigma levels (KTYPE = 107) or hybrid levels (KTYPE = 109).
! Since we know temperature will be required in the input data set, we will
! use temperature (IPARM = 11) as an example of the mid-levels (or half-levels)
! that are in the input.  Since height (IPARM = 7) may come in on full or
! half-levels, we will need to also look for it separately.  We will assume that
! all other input data are on the mid-levels, and that will be checked
! elsewhere in the main program.
!-------------------------------------------------------------------------------

  iparm = ksec1(7)
  ktype = ksec1(8)
  layer = ksec1(9)

  newlyr = .TRUE.

  IF ( metvert == 1 ) THEN         ! hydrostatic sigma

    IF ( ktype == 107 ) THEN       ! GRIB ID for sigma

      IF ( iparm == 11 ) THEN      ! GRIB ID for temperature

        IF ( lyrct == 0 ) THEN
          lyrct = lyrct + 1
          IF ( lyrct > maxmetlays ) GOTO 8000
          inmetlays(lyrct) = layer
        ELSE        
          DO n = 1, lyrct
            IF ( layer == inmetlays(n) ) newlyr = .FALSE.
          ENDDO
          IF ( newlyr ) THEN
            lyrct = lyrct + 1
            IF ( lyrct > maxmetlays ) GOTO 8000
            inmetlays(lyrct) = layer
          ENDIF
        ENDIF

      ELSE IF ( iparm == 7 ) THEN  ! GRIB ID for height

        IF ( lyrctht == 0 ) THEN
          lyrctht = lyrctht + 1
          IF ( lyrctht > maxmetlays ) GOTO 8000
          inmetlaysht(lyrctht) = layer
        ELSE        
          DO n = 1, lyrctht
            IF ( layer == inmetlaysht(n) ) newlyr = .FALSE.
          ENDDO
          IF ( newlyr ) THEN
            lyrctht = lyrctht + 1
            IF ( lyrctht > maxmetlays ) GOTO 8000
            inmetlaysht(lyrctht) = layer
          ENDIF
        ENDIF

      ENDIF

    ENDIF

  ELSE IF ( metvert == 2 ) THEN    ! non-hydrostatic hybrid sigma-pressure

    IF ( ktype == 109 ) THEN       ! GRIB ID for hybrid

      IF ( iparm == 11 ) THEN      ! GRIB ID for temperature

        IF ( lyrct == 0 ) THEN
          lyrct = lyrct + 1
          IF ( lyrct > maxmetlays ) GOTO 8000
          inmetlays(lyrct) = layer
        ELSE        
          DO n = 1, lyrct
            IF ( layer == inmetlays(n) ) newlyr = .FALSE.
          ENDDO
          IF ( newlyr ) THEN
            lyrct = lyrct + 1
            IF ( lyrct > maxmetlays ) GOTO 8000
            inmetlays(lyrct) = layer
          ENDIF
        ENDIF

      ELSE IF ( iparm == 7 ) THEN  ! GRIB ID for height

        IF ( lyrctht == 0 ) THEN
          lyrctht = lyrctht + 1
          IF ( lyrctht > maxmetlays ) GOTO 8000
          inmetlaysht(lyrctht) = layer
        ELSE        
          DO n = 1, lyrctht
            IF ( layer == inmetlaysht(n) ) newlyr = .FALSE.
          ENDDO
          IF ( newlyr ) THEN
            lyrctht = lyrctht + 1
            IF ( lyrctht > maxmetlays ) GOTO 8000
            inmetlaysht(lyrctht) = layer
          ENDIF
        ENDIF

      ENDIF

    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Store information about the grid on which the data is. 
! This stuff gets stored in the MAP variable, as defined in module GRIDINFO.
!-------------------------------------------------------------------------------

  gribgrid = ksec1(5)

  IF ( ksec2(4) == 0 ) THEN  ! Lat/Lon grid
    map%igrid = 0
    map%nx    = infogrid(1)
    map%ny    = infogrid(2)
    map%dx    = ginfo(8)
    map%dy    = ginfo(9)
    map%lat1  = ginfo(3)
    map%lon1  = ginfo(4)

  ELSE IF ( ksec2(4) == 1 ) THEN  ! Mercator grid
    map%igrid    = 1
    map%nx       = infogrid(1)
    map%ny       = infogrid(2)
    map%dx       = ginfo(8)
    map%dy       = ginfo(9)
    map%lat1     = ginfo(3)
    map%lon1     = ginfo(4)
    map%truelat1 = ginfo(11)

  ELSE IF ( ksec2(4) == 3 ) THEN  ! Lambert Conformal Grid
    map%igrid    = 3
    map%nx       = infogrid(1)
    map%ny       = infogrid(2)
    map%lov      = ginfo(6)
    map%truelat1 = ginfo(11)
    map%truelat2 = ginfo(12)
    map%dx       = ginfo(7)
    map%dy       = ginfo(8)
    map%lat1     = ginfo(3)
    map%lon1     = ginfo(4)
         
  ELSE IF ( ksec2(4) == 4 ) THEN  ! Gaussian Grid; we'll call it lat/lon
    map%igrid = 0
    map%nx    = infogrid(1)
    map%ny    = infogrid(2)
    map%dx    = ginfo(8)
    map%dy    = ginfo(19)
    map%lon1  = ginfo(4)
    map%lat1  = ginfo(3)

  ELSE IF ( ksec2(4) == 5 ) THEN  ! Polar-Stereographic Grid.
    map%igrid    = 5
    map%nx       = infogrid(1)
    map%ny       = infogrid(2)
    map%lov      = ginfo(6)
    map%truelat1 = 60.0
    map%truelat2 = 91.0
    map%dx       = ginfo(7)
    map%dy       = ginfo(8)
    map%lat1     = ginfo(3)
    map%lon1     = ginfo(4)

  ELSE
    PRINT*, 'Unknown ksec2(4): ', ksec2(4)
  ENDIF

!-------------------------------------------------------------------------------
! Adjust grid geometry variables to eliminate real number imprecision issues.
!-------------------------------------------------------------------------------

  ihold = NINT(map%lov       * 1000.0)
  map%lov      = REAL(ihold) / 1000.0

  ihold = NINT(map%truelat1  * 1000.0)
  map%truelat1 = REAL(ihold) / 1000.0

  ihold = NINT(map%truelat2  * 1000.0)
  map%truelat2 = REAL(ihold) / 1000.0

  ihold = NINT(map%dx        * 1000.0)
  map%dx       = REAL(ihold) / 1000.0

  ihold = NINT(map%dy        * 1000.0)
  map%dy       = REAL(ihold) / 1000.0

!-------------------------------------------------------------------------------
! Deallocate a couple of arrays that may have been allocated by the
! GRIB decoding routines.
!-------------------------------------------------------------------------------

  CALL deallogrib

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RDGRIBSPECS',                        &
              /, 1x, '***   MAXMETLAYS HAS BEEN EXCEEDED',                 &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdgribspecs
