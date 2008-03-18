SUBROUTINE readlanda

!-------------------------------------------------------------------------------
! Name:     Read LANDA File
! Purpose:  Read LANDA file to extract percentage of urban land use for output.
! Revised:  09 Apr 2004  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time
  USE coord
  USE file
  USE fdesc3
  USE iodecl3
  USE parms3
  USE premaqparm
  USE xvars

  IMPLICIT NONE

  INTEGER                      :: jdate
  INTEGER                      :: jtime
  INTEGER                      :: ns
  LOGICAL                      :: ok
  INTEGER                      :: p_purb
  CHARACTER*16,  PARAMETER     :: pname       = 'READLANDA'
  CHARACTER*16,  ALLOCATABLE   :: vnames      ( : )

!-------------------------------------------------------------------------------
! Get header information from BCON file.
!-------------------------------------------------------------------------------

  IF ( .NOT. open3 (landa, fsread3, pname) ) THEN
    WRITE (6,9000) TRIM(landa)
    GOTO 1001
  ENDIF

  IF ( .NOT. desc3 (landa) ) THEN
    CALL m3err ('READLANDA', sdate, stime,  &
                'Could not read DESC of ' // landa // ' file', .TRUE.)
  ENDIF

!-------------------------------------------------------------------------------
! Ensure that the incoming LANDA file contains the same grid as the
! meteorology by comparing header information.
!-------------------------------------------------------------------------------

  ok = .TRUE.

  ok = ok .AND. ( ncols3d == ncols    )
  ok = ok .AND. ( nrows3d == nrows    )
  ok = ok .AND. ( p_alp3d == p_alp_gd )
  ok = ok .AND. ( p_bet3d == p_bet_gd )
  ok = ok .AND. ( p_gam3d == p_gam_gd )
  ok = ok .AND. ( xorig3d == xorig_gd )
  ok = ok .AND. ( yorig3d == yorig_gd )
  ok = ok .AND. ( xcell3d == xcell_gd )
  ok = ok .AND. ( ycell3d == ycell_gd )
  ok = ok .AND. ( xcent3d == xcent_gd )
  ok = ok .AND. ( ycent3d == ycent_gd )

  IF ( .NOT. ok ) THEN
    WRITE (6,9100)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Load list of variable names from LANDA into a local array.
!-------------------------------------------------------------------------------

  ALLOCATE ( vnames (nvars3d) )

  vnames(:) = vname3d(1:nvars3d)

!-------------------------------------------------------------------------------
! Verify that percentage of urban category (USGS_urban) is in the file.
! Find the correct index for USGS_urban.
!-------------------------------------------------------------------------------

  DO ns = 1, nvars3d
    IF ( TRIM(vnames(ns)) == "USGS_urban" ) THEN
      p_purb = ns
      EXIT
    ENDIF
    IF ( ns == nvars3d ) THEN
      WRITE (6,9200)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Read array with percentage of urban category.
!-------------------------------------------------------------------------------

  IF ( .NOT. read3 (landa, "USGS_urban", 1, jdate, jtime, xpurb) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF

  DEALLOCATE (vnames)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READLANDA',                          &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READLANDA',                          &
              /, 1x, '***   INCOMPATIBLE FILE CONFIGURATIONS',             &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READLANDA',                          &
              /, 1x, '***   DID NOT FIND USGS_urban IN LANDA FILE',        &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READLANDA',                          &
              /, 1x, '***   ERROR READING FILE ', a,                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE readlanda
