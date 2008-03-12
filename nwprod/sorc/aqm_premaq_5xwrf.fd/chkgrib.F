SUBROUTINE chkgrib (gribgrid)

!-------------------------------------------------------------------------------
! Name:     Check GRIB Domain
! Purpose:  Verifies that the input GRIB domain is one we expect for AQF.
! Revised:  31 Mar 2004  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo
  USE premaqparm

  IMPLICIT NONE

  INTEGER                      :: gribgrid
  CHARACTER*16,  PARAMETER     :: pname     = 'CHKGRIB'

!-------------------------------------------------------------------------------
! Check basic grid information against expected AQF grids.
!-------------------------------------------------------------------------------

  IF ( gribgrid == 145 ) THEN
    WRITE (6,9000)
    GOTO 1001
  ENDIF

  IF ( ( gribgrid /= 144 ) .AND. ( gribgrid /= 138 ) ) THEN
    WRITE (6,9100) gribgrid
    GOTO 1001
  ENDIF

  IF ( gribgrid == 144 ) THEN
    IF ( .NOT. ( ( ncols == 166 ) .AND.  &
                 ( nrows == 142 ) .AND.  &
                 ( nlays ==  22 ) ) ) THEN
      WRITE (6,9200) ncols, nrows, nlays
      GOTO 1001
    ENDIF
  ENDIF

  IF ( gribgrid == 138 ) THEN
    IF ( .NOT. ( ( ( ncols == 268 ) .AND.  &
                   ( nrows == 259 ) .AND.  &
                   ( nlays ==  22 ) .AND.  &
                   ( x0    == 199 ) .AND.  &
                   ( y0    ==   1 ) )      &
         .OR.    ( ( ncols == 442 ) .AND.  &
                   ( nrows == 265 ) .AND.  &
                   ( nlays ==  22 ) .AND.  &
                   ( x0    ==  25 ) .AND.  &
                   ( y0    ==   1 ) ) ) ) THEN
      WRITE (6,9300) ncols, nrows, nlays, x0, y0
      GOTO 1001
    ENDIF
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: CHKGRIB',                               &
              /, 1x, '***   PREMAQ WILL NO LONGER WORK FOR GRID 145',         &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: CHKGRIB',                               &
              /, 1x, '***   EXPECTING INPUT GRID 138 OR 144 FOR AQF',         &
              /, 1x, '***   INPUT GRID ID:  ', i3,                            &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: CHKGRIB',                               &
              /, 1x, '***   PROCESSING FOR GRID 144 TO OUTPUT ON GRID 146',   &
              /, 1x, '***   GRID DIMENSION MISMATCH SUSPECTED',               &
              /, 1x, '***   NCOLS, NROWS, NLAYS = ', 3(2x, i3),               &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: CHKGRIB',                               &
              /, 1x, '***   PROCESSING FOR GRID 138 TO OUTPUT ON ',           &
              /, 1x, '***   EITHER GRID 147 ("3x") OR 148 ("5x")',            &
              /, 1x, '***   GRID DIMENSION MISMATCH SUSPECTED',               &
              /, 1x, '***   NCOLS, NROWS, NLAYS = ', 3(2x, i3),               &
              /, 1x, '***   CORNER AT (X0, Y0) = ', 2(2x, i3),                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE chkgrib
