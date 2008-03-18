SUBROUTINE readchembc (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Read Chemistry Boundary Conditions
! Purpose:  Read chemistry boundary conditions file from BCON output to
!           create time-varying lateral boundary conditions for forecast.
! Revised:  03 Dec 2003  Original version.  (T. Otte)
!           29 Apr 2004  Moved allocation of arrays for chemical boundary
!                        conditions to this routine so that the number of
!                        species in the incoming file can be set dynamically.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE choutcom
  USE coord
  USE file
  USE fdesc3
  USE iodecl3
  USE parms3
  USE premaqparm

  IMPLICIT NONE

  INTEGER                      :: jdate
  INTEGER                      :: jtime
  INTEGER                      :: ns
  LOGICAL                      :: ok
  CHARACTER*16,  PARAMETER     :: pname       = 'READCHEMBC'
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime

!-------------------------------------------------------------------------------
! Get header information from BCON file.
!-------------------------------------------------------------------------------

  IF ( .NOT. open3 (bndygasc, fsread3, pname) ) THEN
    WRITE (6,9000) TRIM(bndygasc)
    GOTO 1001
  ENDIF

  IF ( .NOT. desc3 (bndygasc) ) THEN
    CALL m3err ('READCHEMBC', sdate, stime,  &
                'Could not read DESC of ' // bndygasc // ' file', .TRUE.)
  ENDIF

!-------------------------------------------------------------------------------
! Ensure that the incoming BCON file contains the same grid as the
! meteorology by comparing header information.
!-------------------------------------------------------------------------------

  ok = .TRUE.

  ok = ok .AND. ( ncols3d == ncols    )
  ok = ok .AND. ( nrows3d == nrows    )
  ok = ok .AND. ( nlays3d == nlays    )
  ok = ok .AND. ( nthik3d == nthik    )
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
! Allocate necessary arrays now that number of species in incoming
! BC file is known.
!-------------------------------------------------------------------------------

  cb3index = nvars3d

  ALLOCATE ( cc3 ( ncols, nrows, nlays, cc3index ) )
  ALLOCATE ( cb3 ( nbndy, nlays, cb3index ) )

  ALLOCATE ( cb3vname ( cb3index ) )
  ALLOCATE ( cb3units ( cb3index ) )
  ALLOCATE ( cb3vdesc ( cb3index ) )

!-------------------------------------------------------------------------------
! Fill header information about chemical species into appropriate
! arrays.
!-------------------------------------------------------------------------------

  cb3vname(:) = vname3d(1:cb3index)
  cb3units(:) = units3d(1:cb3index)
  cb3vdesc(:) = vdesc3d(1:cb3index) 

!-------------------------------------------------------------------------------
! Verify that ozone (O3) is in the file.  Find the correct index for O3.
!-------------------------------------------------------------------------------

  DO ns = 1, cb3index
    IF ( TRIM(cb3vname(ns)) == "O3" ) THEN
      pc_o3 =  1                 ! ozone is only variable in CHEMCRO3D
      pb_o3 =  ns                ! ozone will go back in its slot in CHEMBDY3D
      o3_c  => cc3(:,:,:,pc_o3)
      o3_b  => cb3(:,:,pb_o3)
      cc3vname(pc_o3) = cb3vname(pb_o3)
      cc3units(pc_o3) = cb3units(pb_o3)
      cc3vdesc(pc_o3) = cb3vdesc(pb_o3)
      EXIT
    ENDIF
    IF ( ns == cb3index ) THEN
      WRITE (6,9200)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Read boundary concentrations.
!-------------------------------------------------------------------------------

  IF ( .NOT. read3 (bndygasc, allvar3, allays3, jdate, jtime,  &
                    cb3(1,1,1)) ) THEN
    WRITE (6,9300) TRIM(bndygasc)
    GOTO 1001
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READCHEMBC',                         &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READCHEMBC',                         &
              /, 1x, '***   INCOMPATIBLE FILE CONFIGURATIONS',             &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READCHEMBC',                         &
              /, 1x, '***   DID NOT FIND O3 IN BCON FILE',                 &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READCHEMBC',                         &
              /, 1x, '***   ERROR READING FILE ', a,                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE readchembc
