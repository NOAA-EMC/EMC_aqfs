SUBROUTINE readnml (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Read Namelist
! Purpose:  Reads input namelist to get user control variables.
! Revised:  19 May 2003  Original version.  (T. Otte)
!           25 Nov 2003  Added flag to create static output (grid) files.
!                        (T. Otte)
!           31 Mar 2004  Added flags to create emissions and time-varying
!                        chemical boundary conditions.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE premaqparm

  IMPLICIT NONE

  INTEGER                      :: btrim
  INTEGER                      :: ctm2met
  REAL,          INTENT(OUT)   :: ctmlays    ( maxlays )
  INTEGER                      :: hh
  INTEGER                      :: istat
  INTEGER                      :: mm
  INTEGER                      :: n
  INTEGER                      :: ncolsin
  INTEGER                      :: nrowsin
  CHARACTER*16,  PARAMETER     :: pname      = 'READNML'


  NAMELIST /filenames/   file_gd, files_in, makegrid, makeemis, makebc

  NAMELIST /userdefs/    aqf_start, aqf_end, intvl, iwind,      &
                         coordnam, grdnam, ctmlays, ctm2met, btrim,  &
                         metvert, lprt_col, lprt_row

  NAMELIST /windowdefs/  x0, y0, ncolsin, nrowsin

!-------------------------------------------------------------------------------
! Open namelist file.
!-------------------------------------------------------------------------------

  OPEN (iutnml, FILE=file_nml, STATUS='OLD', ERR=8000, IOSTAT=istat)

!-------------------------------------------------------------------------------
! Initialize input file names.
!-------------------------------------------------------------------------------

  file_gd      = "GRIDDESC"
  files_in(:)  = " "
  makegrid     = .TRUE.
  makeemis     = .TRUE.
  makebc       = .TRUE.

!-------------------------------------------------------------------------------
! Set default date.
!-------------------------------------------------------------------------------

  aqf_start = '0000-00-00-00:00:00.0000'
  aqf_end   = '0000-00-00-00:00:00.0000'
  intvl     =  0

!-------------------------------------------------------------------------------
! Set default values for various user-definable control parameters.
!   IWIND = number of iterations for vertical wind correction
!           (use 0, +/- 1,2,3,...)
!   METVERT = vertical layer structure for input meteorology
!             (1=hydrostatic sigma, 2=non-hydrostatic hybrid sigma-pressure)
!-------------------------------------------------------------------------------

  iwind   = 0
  metvert = 1

!-------------------------------------------------------------------------------
! Set coordinates for cell to print diagnostic output.  If 0 is set,
! domain center cell will be used.
!-------------------------------------------------------------------------------

  lprt_col = 0
  lprt_row = 0

!-------------------------------------------------------------------------------
! Initialize CTM layers to an unrealistic value (-1).
!-------------------------------------------------------------------------------

  ctmlays(:) = -1.0

!-------------------------------------------------------------------------------
! Set default ratio of CTM to meteorology horizontal grid spacing to 1.
! Set default meteorology "boundary" point removal to 5.
!-------------------------------------------------------------------------------

  ctm2met = 1
  btrim   = 5

!-------------------------------------------------------------------------------
! Initialize COORDNAM and GRDNAM to missing values.
!-------------------------------------------------------------------------------

  coordnam = "????????????????"
  grdnam   = "????????????????"

!-------------------------------------------------------------------------------
! Set default values for coordinate of full PREMAQ "X" domain (including PREMAQ
! lateral boundary) lower-left corner with respect to the input meteorology
! domain.  Minimum acceptable value is 1.  X0 is the column offset, and
! Y0 is the row offset.
!-------------------------------------------------------------------------------

  x0 = 1
  y0 = 1

!-------------------------------------------------------------------------------
! Set default values for window size.  (20 is arbitrary...just want to prevent
! "very small" domains.)
!-------------------------------------------------------------------------------

  ncolsin = 20
  nrowsin = 20

!-------------------------------------------------------------------------------
! Read namelist to get user definitions.  Rewind namelist file after each
! read in case namelists are not in the correct order in the namelist.
!-------------------------------------------------------------------------------

  READ (iutnml, filenames, ERR=8025, IOSTAT=istat)
  REWIND (iutnml)

  READ (iutnml, userdefs, ERR=8050, IOSTAT=istat)
  REWIND (iutnml)

  IF ( btrim < 0 ) THEN
    READ (iutnml, windowdefs, ERR=8075, IOSTAT=istat)
    REWIND (iutnml)
  ENDIF

!-------------------------------------------------------------------------------
! Crop blank spaces off ends of file names.
!-------------------------------------------------------------------------------

  DO n = 1, SIZE(files_in)
    files_in(n) = TRIM( ADJUSTL( files_in(n) ) )
    IF ( files_in(n)(1:7) == "no_file" ) files_in(n) = " "
  ENDDO

!-------------------------------------------------------------------------------
! Set start and end dates.  Ensure that "special characters" that separate
! components of date are set correctly.  If these are not set properly, the
! lexical time comparisons in the driver (premaq.F) will not work properly.
!-------------------------------------------------------------------------------

  aqf_start( 5: 5) = "-"         ;  aqf_end  ( 5: 5) = "-"
  aqf_start( 8: 8) = "-"         ;  aqf_end  ( 8: 8) = "-"
  aqf_start(11:11) = "-"         ;  aqf_end  (11:11) = "-"
  aqf_start(14:14) = ":"         ;  aqf_end  (14:14) = ":"
  aqf_start(17:24) = ":00.0000"  ;  aqf_end  (17:24) = ":00.0000"

  IF ( aqf_start > aqf_end ) THEN
    WRITE (6,9600) aqf_start, aqf_end
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Ensure that cell coordinates for diagnostic output are not negative.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col < 0 ) .OR. ( lprt_row < 0 ) ) THEN
    WRITE (6,9650) lprt_col, lprt_row
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Determine whether or not a valid input vertical structure type was selected.
!-------------------------------------------------------------------------------

  IF ( ( metvert /= 1 ) .AND. ( metvert /= 2 ) ) THEN
    WRITE (6,9675) metvert
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Determine actual CTM layers and fill arrays.
!-------------------------------------------------------------------------------

  DO n = 1, maxlays
    IF ( ctmlays(n) < 0.0 ) EXIT
    IF ( n == 1 ) THEN
      IF ( ctmlays(n) /= 1.0 ) THEN
        WRITE (6,9700) ctmlays(1)
        GOTO 1001
      ENDIF
      CYCLE
    ENDIF
    IF ( ctmlays(n) >= ctmlays(n-1) ) THEN
      WRITE (6,9800) ctmlays(:)
      GOTO 1001
    ENDIF
  ENDDO

  IF ( ctmlays(n-1) /= 0.0 ) THEN
    WRITE (6,9900) ctmlays(n-1)
    GOTO 1001
  ENDIF

  nlays = n - 2  ! one less than number in array, and one less than loop counter

!-------------------------------------------------------------------------------
! Store ratio of meteorology to CTM horizontal grid spacing and
! meteorology boundary removal in PREMAQPARM.  If not windowing, set
! coordinate of lower-left corner of output domain with respect to
! input meteorology domain.  If windowing, ensure that X0 and Y0 are
! not too small.
!-------------------------------------------------------------------------------

  ndx       = ctm2met
  nbdrytrim = btrim

  IF ( nbdrytrim >= 0 ) THEN
    x0 = nbdrytrim + 1
    y0 = nbdrytrim + 1
  ENDIF

  IF ( ( x0 < 1 ) .OR. ( y0 < 1 ) ) THEN
    WRITE (6,9950) x0, y0
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Verify that user-defined number of rows and columns is not less than 1.
! If windowing, use the user-defined input to set NCOLS and NROWS.
!-------------------------------------------------------------------------------

  IF ( ( ncolsin >= 1 ) .AND. ( nrowsin >= 1 ) ) THEN
    IF ( nbdrytrim < 0 ) THEN
      ncols = ncolsin
      nrows = nrowsin
    ENDIF
  ELSE
    WRITE (6,9975) ncolsin, nrowsin
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Calculate GRSTEP from INTVL.  GRSTEP is in STIME format (HHMMSS).
! Assume SS is 0.
!-------------------------------------------------------------------------------

  hh     = INT(intvl / 60)
  mm     = MOD(intvl,  60)

  grstep = ( hh * 10000 ) + ( mm * 100 )

!-------------------------------------------------------------------------------
! Close namelist file.
!-------------------------------------------------------------------------------

  CLOSE (iutnml)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutnml, TRIM(file_nml), istat
      GOTO 1001

 8025 WRITE (6,9050) iutnml, TRIM(file_nml), "filenames", istat
      GOTO 1001

 8050 WRITE (6,9050) iutnml, TRIM(file_nml), "userdefs", istat
      GOTO 1001

 8075 WRITE (6,9050) iutnml, TRIM(file_nml), "windowdefs", istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   ERROR OPENING NAMELIST FILE ON UNIT ', i3,      &
              /, 1x, '***   NAMELIST FILE NAME = ', a,                      &
              /, 1x, '***   IOSTAT = ', i4,                                 &
              /, 1x, 70('*'))

 9050 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   ERROR READING NAMELIST FILE ON UNIT ', i3,      &
              /, 1x, '***   NAMELIST FILE NAME = ', a,                      &
              /, 1x, '***   NAMELIST = ', a,                                &
              /, 1x, '***   IOSTAT = ', i4,                                 &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Start date must be before end date',            &
              /, 1x, '***   Input AQF_START = ', a,                         &
              /, 1x, '***   Input AQF_END   = ', a,                         &
              /, 1x, 70('*'))

 9650 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid coordinates for cell for diagnostics',  &
              /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4,  &
              /, 1x, 70('*'))

 9675 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid input meteorology vertical structure',  &
              /, 1x, '***   Input METVERT is ', i3,                         &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   First CTM layer must be 1.0',                   &
              /, 1x, '***   First input CTM layer is ', f7.4,               &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Input CTM layers seem to be out of order',      &
              /, 1x, '***     Layers must be in descending order',          &
              /, 1x, '***   Input CTM layers are ', 50f7.4,                 &
              /, 1x, 70('*'))

 9900 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Last CTM layer must be 0.0',                    &
              /, 1x, '***   Last input CTM layer is ', f7.4,                &
              /, 1x, 70('*'))

 9950 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Minimum value for X0 and Y0 is 1',              &
              /, 1x, '***   User input X0 and Y0: ', 2(1x, i4),             &
              /, 1x, 70('*'))

 9975 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Minimum value for NCOLS and NROWS is 1',        &
              /, 1x, '***   User input NCOLS and NROWS: ', 2(1x, i4),       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE readnml
