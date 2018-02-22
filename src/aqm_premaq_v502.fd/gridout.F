SUBROUTINE gridout (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Grid Out
! Purpose:  Fill arrays for GRID files and outputs.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  23 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           11 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           21 Oct 2003  Removed fields associated with GRIDBDY2D and
!                        GRIDBDY3D.  (T. Otte)
!           23 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           09 Apr 2004  Added PURB to output.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           21 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits 
!                        Used F90 array index assignment syntax (David Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE coord
  USE fdesc3
  USE file
  USE iodecl3
  USE groutcom
  USE vgrd
  USE parms3

  IMPLICIT NONE

  INTEGER                      :: c
  INTEGER                      :: col
  REAL,    SAVE, ALLOCATABLE   :: dumaray0    ( : , : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  CHARACTER*63                 :: ifmt1
  INTEGER                      :: ilu
  INTEGER                      :: index
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: l
  INTEGER                      :: lbnd
  CHARACTER*16                 :: luname
  INTEGER                      :: lvl
  INTEGER                      :: n
  CHARACTER*16,  PARAMETER     :: pname       = 'GRIDOUT'
  INTEGER                      :: r
  INTEGER                      :: row
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  REAL                         :: x3jfmin
  REAL                         :: x3jmmin
  REAL                         :: xmapmin
  REAL(8)                      :: xorigdot
  REAL(8)                      :: yorigdot

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,          INTENT(INOUT) :: aa         ( : , : , : )
      REAL,          INTENT(IN)    :: vertin     ( : )
      REAL,          INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE
 
!-------------------------------------------------------------------------------
! Build common header for I/O API files.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Compute XORIGDOT and YORIGDOT.  Assume (XORIG_GD, YORIG_GD) are
! for cross cells without boundary.
!-------------------------------------------------------------------------------

  lbnd = LBOUND(x3jacobf,3)

  xorigdot = xorig_gd - (0.5 * xcell3d)
  yorigdot = yorig_gd - (0.5 * ycell3d)

!-------------------------------------------------------------------------------
! Build rest of header for GRID_DOT_2D file.
!-------------------------------------------------------------------------------

  DO index = 1, gd2index
    vtype3d(index) = m3real
    vname3d(index) = gd2vname(index)
    units3d(index) = gd2units(index)
    vdesc3d(index) = gd2vdesc(index)
  ENDDO

  gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_DOT'

  xorig3d = xorigdot
  yorig3d = yorigdot
  ncols3d = ncols + 1
  nrows3d = nrows + 1
  nthik3d = MAX (nthik, nthikd)

  ftype3d = grdded3
  nvars3d = gd2index
  nlays3d = 1
  tstep3d = 0

  ! Write GRID_DOT_2D header.

  IF ( .NOT. open3 (griddot2d_g1, fsunkn3, pname) ) THEN
    WRITE (6,9000) TRIM(griddot2d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Build rest of header for GRID_CRO_2D file.
!-------------------------------------------------------------------------------

  DO index = 1, gc2index
    vtype3d(index) = m3real
    vname3d(index) = gc2vname(index)
    units3d(index) = gc2units(index)
    vdesc3d(index) = gc2vdesc(index)
  ENDDO

  gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows

  ftype3d = grdded3
  nvars3d = gc2index
  nlays3d = 1
  tstep3d = 0

  ! Write GRID_CRO_2D header.

  IF ( .NOT. open3 (gridcro2d_g1, fsunkn3, pname) ) THEN
    WRITE (6,9000) TRIM(gridcro2d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Build rest of header for GRID_CRO_3D file.
!-------------------------------------------------------------------------------

  DO index = 1, gc3index
    vtype3d(index) = m3real
    vname3d(index) = gc3vname(index)
    units3d(index) = gc3units(index)
    vdesc3d(index) = gc3vdesc(index)
  ENDDO

  gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows

  ftype3d = grdded3
  nvars3d = gc3index
  nlays3d = nlays
  tstep3d = 0

  ! Write GRID_CRO_3D header.

  IF ( .NOT. open3 (gridcro3d_g1, fsunkn3, pname) ) THEN
    WRITE (6,9000) TRIM(gridcro3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in GRID_DOT_2D (time independent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapd)   ! XMAPMIN gets "recycled" below for XMAPC

  IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
     gmsfsq_d (1:ncols+1,1:nrows+1) = badval3
  ELSE
     gmsfsq_d (1:ncols+1,1:nrows+1) = xmapd (1:ncols+1,1:nrows+1)**2
  ENDIF

  glat_d   (1:ncols+1,1:nrows+1) = xlatd (1:ncols+1,1:nrows+1)
  glon_d   (1:ncols+1,1:nrows+1) = xlond (1:ncols+1,1:nrows+1)

!-------------------------------------------------------------------------------
! Assign arrays in GRID_CRO_2D (time independent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)  ! XMAPMIN previously defined for XMAPD
  x3jfmin = MINVAL(x3jacobf)
  x3jmmin = MINVAL(x3jacobm)

  IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
     gmsfsq_c (1:ncols,1:nrows) = badval3
  ELSE
     gmsfsq_c (1:ncols,1:nrows) = xmapc (1+nthik:ncols+nthik,1+nthik:nrows+nthik)**2
  ENDIF

  glat_c   (1:ncols,1:nrows) = xlatc   (1+nthik:ncols+nthik,1+nthik:nrows+nthik)
  glon_c   (1:ncols,1:nrows) = xlonc   (1+nthik:ncols+nthik,1+nthik:nrows+nthik)

  gtopo_c  (1:ncols,1:nrows) = xtopo   (1+nthik:ncols+nthik,1+nthik:nrows+nthik)
  gdluse_c (1:ncols,1:nrows) = xdluse  (1+nthik:ncols+nthik,1+nthik:nrows+nthik)

  gpurb_c  (1:ncols,1:nrows) = xpurb   (1:ncols,1:nrows)
  
  DO c = 1, ncols
   DO r = 1, nrows
    IF ( gdluse_c (c,r) == 16 ) THEN   !ONLY IF IT IS USGS24
      glwmask_c (c,r) = 0.0
    ELSE
      glwmask_c (c,r) = 1.0
    ENDIF
   ENDDO
  ENDDO
  
  glufrac01_c(1:ncols,1:nrows) = xlufrac_01(1:ncols,1:nrows) 
  glufrac02_c(1:ncols,1:nrows) = xlufrac_02(1:ncols,1:nrows)   
  glufrac03_c(1:ncols,1:nrows) = xlufrac_03(1:ncols,1:nrows) 
  glufrac04_c(1:ncols,1:nrows) = xlufrac_04(1:ncols,1:nrows)  
  glufrac05_c(1:ncols,1:nrows) = xlufrac_05(1:ncols,1:nrows) 
  glufrac06_c(1:ncols,1:nrows) = xlufrac_06(1:ncols,1:nrows)   
  glufrac07_c(1:ncols,1:nrows) = xlufrac_07(1:ncols,1:nrows) 
  glufrac08_c(1:ncols,1:nrows) = xlufrac_08(1:ncols,1:nrows) 
  glufrac09_c(1:ncols,1:nrows) = xlufrac_09(1:ncols,1:nrows) 
  glufrac10_c(1:ncols,1:nrows) = xlufrac_10(1:ncols,1:nrows)   
  glufrac11_c(1:ncols,1:nrows) = xlufrac_11(1:ncols,1:nrows) 
  glufrac12_c(1:ncols,1:nrows) = xlufrac_12(1:ncols,1:nrows)  
  glufrac13_c(1:ncols,1:nrows) = xlufrac_13(1:ncols,1:nrows) 
  glufrac14_c(1:ncols,1:nrows) = xlufrac_14(1:ncols,1:nrows)   
  glufrac15_c(1:ncols,1:nrows) = xlufrac_15(1:ncols,1:nrows) 
  glufrac16_c(1:ncols,1:nrows) = xlufrac_16(1:ncols,1:nrows)       
  glufrac17_c(1:ncols,1:nrows) = xlufrac_17(1:ncols,1:nrows) 
  glufrac18_c(1:ncols,1:nrows) = xlufrac_18(1:ncols,1:nrows)   
  glufrac19_c(1:ncols,1:nrows) = xlufrac_19(1:ncols,1:nrows) 
  glufrac20_c(1:ncols,1:nrows) = xlufrac_20(1:ncols,1:nrows)  
  glufrac21_c(1:ncols,1:nrows) = xlufrac_21(1:ncols,1:nrows) 
  glufrac22_c(1:ncols,1:nrows) = xlufrac_22(1:ncols,1:nrows)   
  glufrac23_c(1:ncols,1:nrows) = xlufrac_23(1:ncols,1:nrows) 
  glufrac24_c(1:ncols,1:nrows) = xlufrac_24(1:ncols,1:nrows) 
  
!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays using weighted layer averaging.  (Original values
! are replaced.)  For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray0 ) )  &
      ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 1 ) )
    IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
      ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 1 ) )

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          dumaray1(c,r,k,1) = x3htm(c,r,k)
        ENDDO 
      ENDDO
    ENDDO

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          dumaray0(c,r,k,1) = x3htf(c,r,k)
        ENDDO 
      ENDDO
    ENDDO    

    CALL collapx (x3htm, xx3midl, x3midl)
    CALL collapx (x3htf, xx3face, x3face)

  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in GRID_CRO_3D (time independent).  
!-------------------------------------------------------------------------------

  gx3htf_c (1:ncols,1:nrows,1:nlays) = x3htf (1+nthik:ncols+nthik,1+nthik:nrows+nthik,1:nlays)
  gx3htm_c (1:ncols,1:nrows,1:nlays) = x3htm (1+nthik:ncols+nthik,1+nthik:nrows+nthik,1:nlays)

!-------------------------------------------------------------------------------
! Write GRID_DOT_2D data (time independent data).
! Read FDESC3.EXT from GRIDDOT2D to get header information.
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (griddot2d_g1) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // griddot2d_g1 // ' file', .TRUE.)
  ENDIF

  IF ( .NOT. write3 (griddot2d_g1, allvar3, sdate, stime, gd2) ) THEN
    WRITE (6,9100) TRIM(griddot2d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Write GRID_CRO_2D data (time independent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (gridcro2d_g1) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // GRIDCRO2D_G1 // ' file', .TRUE.)
  ENDIF

  DO l = 1, gc2index
    IF ( .NOT. write3 (gridcro2d_g1, vname3d(l), sdate, stime,  &
                       gc2(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(gridcro2d_g1)
    GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write GRID_CRO_3D data (time independent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (gridcro3d_g1) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // gridcro3d_g1 // ' file', .TRUE.)
  ENDIF

  IF ( .NOT. write3 (gridcro3d_g1, allvar3, sdate, stime, gc3) ) THEN
    WRITE (6,9100) TRIM(gridcro3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          x3htm(c,r,k) = dumaray1(c,r,k,1)
        ENDDO 
      ENDDO
    ENDDO

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          x3htf(c,r,k) = dumaray0(c,r,k,1) 
        ENDDO 
      ENDDO
    ENDDO 

!   DEALLOCATE ( dumaray0 )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation

  ENDIF   

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nlays / 5
  k2 = MOD(nlays, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x,"   &
         &    // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4),/,10x," // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4))"
    ENDIF
  ENDIF

  WRITE (*,'(/,a,/)') '- GRIDOUT: Printing sample cells in output grid'

  DO n = 1, gc2index
    WRITE (*,6000) TRIM(gc2vname(n)), gc2(lprt_col,lprt_row,n), gc2units(n)
  ENDDO

  DO n = 1, gd2index
    WRITE (*,6000) TRIM(gd2vname(n)), gd2(lprt_col,lprt_row,n), gd2units(n)
  ENDDO

  DO n = 1, gc3index
    WRITE (*,ifmt1) TRIM(gc3vname(n)), gc3(lprt_col,lprt_row,:,n)
  ENDDO

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (1x, a9, 2x, f12.4, 2x, a)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GRIDOUT',                            &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GRIDOUT',                            &
              /, 1x, '***   ERROR WRITING TO FILE ', a,                    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE gridout
