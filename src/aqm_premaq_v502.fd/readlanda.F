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
  INTEGER                      :: p_purb

  REAL                         :: dalp, dbet, dgam, dxcent, dycent

  LOGICAL                      :: ok

  CHARACTER*16,  PARAMETER     :: pname       = 'READLANDA'
  CHARACTER*16,  ALLOCATABLE   :: vnames      ( : )

  EXTERNAL       ENVYN
  LOGICAL        ENVYN, AKHI_RUN
  INTEGER        ISTAT

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
  ok = ok .AND. ( xorig3d == xorig_gd )
  ok = ok .AND. ( yorig3d == yorig_gd )
  ok = ok .AND. ( xcell3d == xcell_gd )
  ok = ok .AND. ( ycell3d == ycell_gd )

  AKHI_RUN = ENVYN ('AKHI_RUN', 'USE DIFF EARTH R ?', .FALSE. , ISTAT)

  if ( AKHI_RUN ) then
     dalp   = abs ( p_alp3d - p_alp_gd )
     dbet   = abs ( p_bet3d - p_bet_gd )
     dgam   = abs ( p_gam3d - p_gam_gd )
     dxcent = abs ( xcent3d - xcent_gd )
     dycent = abs ( ycent3d - ycent_gd )

     ok = ok .AND. ( dalp    <= dmap     )
     ok = ok .AND. ( dbet    <= dmap     )
     ok = ok .AND. ( dgam    <= dmap     )
     ok = ok .AND. ( dxcent  <= dmap     )
     ok = ok .AND. ( dycent  <= dmap     )
  else
     ok = ok .AND. ( p_alp3d == p_alp_gd )
     ok = ok .AND. ( p_bet3d == p_bet_gd )
     ok = ok .AND. ( p_gam3d == p_gam_gd )
     ok = ok .AND. ( xcent3d == xcent_gd )
     ok = ok .AND. ( ycent3d == ycent_gd )
  endif

  IF ( .NOT. ok ) THEN
    write (*,*)"&&&&&&&&&&&&&&&&&&&&&&&&&"
    write (*,*)
    write (*,*) ncols3d, ncols
    write (*,*) nrows3d, nrows
    write (*,*) p_alp3d, p_alp_gd
    write (*,*) p_bet3d, p_bet_gd
    write (*,*) p_gam3d, p_gam_gd
    write (*,*) xorig3d, xorig_gd
    write (*,*) yorig3d, yorig_gd
    write (*,*) xcell3d, xcell_gd
    write (*,*) ycell3d, ycell_gd
    write (*,*) xcent3d, xcent_gd
    write (*,*) ycent3d, ycent_gd
    write (*,*)
    write (*,*)"&&&&&&&&&&&&&&&&&&&&&&&&&"
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

!add by lipan
  
  IF ( .NOT. read3 (landa, "LUFRAC_01", 1, jdate, jtime, xlufrac_01) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_02", 1, jdate, jtime, xlufrac_02) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
  
 IF ( .NOT. read3 (landa, "LUFRAC_03", 1, jdate, jtime, xlufrac_03) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_04", 1, jdate, jtime, xlufrac_04) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF    
  
  IF ( .NOT. read3 (landa, "LUFRAC_05", 1, jdate, jtime, xlufrac_05) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_06", 1, jdate, jtime, xlufrac_06) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
  
 IF ( .NOT. read3 (landa, "LUFRAC_07", 1, jdate, jtime, xlufrac_07) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_08", 1, jdate, jtime, xlufrac_08) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF    
    
  IF ( .NOT. read3 (landa, "LUFRAC_09", 1, jdate, jtime, xlufrac_09) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_10", 1, jdate, jtime, xlufrac_10) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
  
 IF ( .NOT. read3 (landa, "LUFRAC_11", 1, jdate, jtime, xlufrac_11) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_12", 1, jdate, jtime, xlufrac_12) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF    
  
  IF ( .NOT. read3 (landa, "LUFRAC_13", 1, jdate, jtime, xlufrac_13) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_14", 1, jdate, jtime, xlufrac_14) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
  
 IF ( .NOT. read3 (landa, "LUFRAC_15", 1, jdate, jtime, xlufrac_15) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_16", 1, jdate, jtime, xlufrac_16) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF    
  
 IF ( .NOT. read3 (landa, "LUFRAC_17", 1, jdate, jtime, xlufrac_17) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_18", 1, jdate, jtime, xlufrac_18) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF    
  
  IF ( .NOT. read3 (landa, "LUFRAC_19", 1, jdate, jtime, xlufrac_19) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_20", 1, jdate, jtime, xlufrac_20) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
  
 IF ( .NOT. read3 (landa, "LUFRAC_21", 1, jdate, jtime, xlufrac_21) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF
  
  IF ( .NOT. read3 (landa, "LUFRAC_22", 1, jdate, jtime, xlufrac_22) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF        
  
  IF ( .NOT. read3 (landa, "LUFRAC_23", 1, jdate, jtime, xlufrac_23) ) THEN
    WRITE (6,9300) TRIM(landa)
    GOTO 1001
  ENDIF  
          
  IF ( .NOT. read3 (landa, "LUFRAC_24", 1, jdate, jtime, xlufrac_24) ) THEN
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
