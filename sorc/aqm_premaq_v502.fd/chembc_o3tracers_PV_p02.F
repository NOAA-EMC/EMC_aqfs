SUBROUTINE chembc (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Process Chemistry Boundary Conditions
! Purpose:  Process chemistry boundary conditions file to create time-varying
!           lateral boundary conditions for forecast.
! Revised:  03 Dec 2003  Original version.  (T. Otte)
!           09 Dec 2003  Changed O3 in boundary conditions file to be
!                        maximum of GFS O3 and predefined background conditions
!                        in incoming BCON file.  (T. Otte and J. Pleim)
!           29 Apr 2004  Changed local array allocation to only occur on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           24 Jun 2004  Restrict GFS O3 to above a predefined height AGL.
!                        (T. Otte)
!           20 Jan 2005  Interchanged order of nested loop in various places to
!                        increase number of cache hits (David Wong)
!           28 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!           17 Jun 2005  Added boundary tracers for GFS O3.  Restrict GFS O3 to
!                        top layer of model. (T. Otte and R. Mathur)
!           24 Jun 2005  Reinitialize O3 boundary conditions to reference
!                        profile each time step.  (T. Otte)
!           25 Apr 2006  Changed tracers in TB3 from one per lateral boundary
!                        for ozone to tracers that nominally represent ozone in
!                        the boundary layer, free troposphere, and top layer.
!                        (T. Otte)
!           01 Aug 2006  Corrected the range of the third dimension of xo3 when
!                        it assigns to o3_c. (David Wong)
!           01 Sep 2008  Scale O3 in free troposphere (X3FACE_GD >0.58) by PV
!                        =========================================
!                        O3 in ppb = 30*PV or O3 in ppm = 0.03*PV
!                        =========================================
!                        This constant of proportionality is determined based on
!                        examining the relationship between NAM PV and average
!                        observed O3 at the 2006 IONS sites for the top most
!                        layer of a 22-layer configuration.    (Hsin-mu Lin)
!                    *** (this logrithm and the critial "X3FACE_GD>0.58" for KPV
!                         need to be consistent with the CMAQ code "pv_o3.F")
!-------------------------------------------------------------------------------

  USE choutcom
  USE coord
  USE fdesc3
  USE file
  USE iodecl3
  USE premaqparm
  USE vgrd
  USE xvars
  USE mcoutcom

  IMPLICIT NONE

  REAL,          PARAMETER     :: bltop       = 3125.0  ! m
  INTEGER                      :: c
  INTEGER                      :: col
  REAL,    SAVE, ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dumaray2    ( : , : , : , : )
  INTEGER                      :: ec
  INTEGER                      :: er
  LOGICAL,       SAVE          :: first       = .TRUE.
  INTEGER                      :: ibc
  CHARACTER*63                 :: ifmt1
  INTEGER                      :: index, t_index
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER,       SAVE          :: KPV
  INTEGER                      :: l
  INTEGER                      :: lvl
  INTEGER                      :: n
  CHARACTER*16,  PARAMETER     :: pname       = 'CHEMBC'
  REAL                         :: PV4O3
  INTEGER                      :: r
  INTEGER                      :: row
  INTEGER                      :: sc
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER                      :: sr
  INTEGER,       INTENT(IN)    :: stime
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,          INTENT(INOUT) :: aa         ( : , : , : )
      REAL,          INTENT(IN)    :: vertin     ( : )
      REAL,          INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE
 
!-------------------------------------------------------------------------------
! If first time, build headers for files.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ! -------------------------------------------
    ! determine layers above 8.5km or X3 > 0.58
    ! -------------------------------------------

    DO L = 0, NLAYS
       IF ( X3FACE_GD(L) > 0.58) THEN
          KPV = L
          GO TO 20
       ENDIF
    ENDDO

 20 WRITE(6,9201) KPV, X3FACE_GD(KPV)


    ! Build common header.

    CALL comheader (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for CHEMCRO3D file.
    !---------------------------------------------------------------------------

    DO index = 1, cc3index
      vtype3d(index) = m3real
      vname3d(index) = cc3vname(index)
      units3d(index) = cc3units(index)
      vdesc3d(index) = cc3vdesc(index)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows

    ftype3d = grdded3
    nvars3d = cc3index
    nlays3d = nlays

    IF ( .NOT. open3 (chemcro3d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(chemcro3d_g1)
      GOTO 1001
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for CHEMBDY3D file.
    !---------------------------------------------------------------------------

    DO index = 1, cb3index
      vtype3d(index) = m3real
      vname3d(index) = cb3vname(index)
      units3d(index) = cb3units(index)
      vdesc3d(index) = cb3vdesc(index)
    ENDDO

    DO ibc = 1, tb3index
      index = cb3index + ibc
      vtype3d(index) = m3real
      vname3d(index) = tb3vname(ibc)
      units3d(index) = tb3units(ibc)
      vdesc3d(index) = tb3vdesc(ibc)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows

    ftype3d = bndary3
    nvars3d = cb3index + tb3index
    nlays3d = nlays

    IF ( .NOT. open3 (chembdy3d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(chembdy3d_g1)
      GOTO 1001
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays using weighted layer averaging.  (Original values
! are replaced.)  For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
    ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, cc3index ) )

    IF ( .NOT. ALLOCATED ( dumaray2 ) )  &
    ALLOCATE ( dumaray2 ( ncols_x, nrows_x, 1:metlay, cc3index ) )

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          dumaray1(c,r,k,1) =  xo3(c,r,k)
          dumaray2(c,r,k,1) = XPVC(c,r,k)
        ENDDO 
      ENDDO
    ENDDO

    CALL collapx ( xo3, xx3midl, x3midl)
    CALL collapx (XPVC, xx3midl, x3midl)

  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in CHEM_CRO_3D (time-dependent).
!-------------------------------------------------------------------------------

  sc = 1 + nthik
  ec = ncols + nthik
  sr = 1 + nthik
  er = nrows + nthik

! o3_c (1:ncols, 1:nrows, :) = xo3 (SC:EC, SR:ER, :)
  o3_c (1:ncols, 1:nrows, :) = xo3 (SC:EC, SR:ER, 1:size(o3_c,3))
  
!-------------------------------------------------------------------------------
! Assign arrays in CHEM_BDY_3D (time-dependent).
!-------------------------------------------------------------------------------

! Initialize O3 boundary conditions to reference profile.

  o3_b(:,:) = o3_ref_b(:,:)

!$omp parallel do
  DO lvl = 1, nlays
     index = 0
     DO c = 1+nthik, ncols_x
        DO r = 1, nthikd
           index = index + 1
           IF ( lvl >= KPV ) THEN

              !==========================================
              !  O3 in ppb = 30*PV or O3 in ppm = .03*PV
              !==========================================

              PV4O3 = 0.02 * abs ( XPVC(c,r,lvl) )
              o3_b (index,lvl) = max ( o3_b(index,lvl), PV4O3 )
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

  t_index = index

  ! Side I: add one more index at the end
  ! for NTHIK=0, march 1 more position

!$omp parallel do

  DO lvl = 1, nlays
     IF ( nthik == 0 ) THEN
        index = t_index + 1 
     ELSE
        index = t_index
     END IF

     DO r = 1+nthik, nrows_x
        DO l = 1, nthikd
           c = ncols_x - nthikd + l
           index = index + 1
           IF ( lvl >= KPV ) THEN
              PV4O3 = 0.02 * abs ( XPVC(c,r,lvl) )
              o3_b (index,lvl) = max ( o3_b(index,lvl), PV4O3 )
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

  t_index = index

  ! Side II: add one more index at the end
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1 
            
  ! Side III: add one more index before
  ! for NTHIK=0, march 1 more position

! IF ( nthik == 0 ) index = index + 1      

!$omp parallel do
  DO lvl = 1, nlays
     IF ( nthik == 0 ) THEN
        index = t_index + 2 
     ELSE
        index = t_index
     END IF

     DO l = 1, nthikd
        r = nrows_x - nthikd + l
        DO c = 1, ncols_x - nthik
           index = index + 1
           IF ( lvl >= KPV ) THEN
              PV4O3 = 0.02 * abs ( XPVC(c,r,lvl) )
              o3_b (index,lvl) = max ( o3_b(index,lvl), PV4O3 )
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

  t_index = index

  ! Side IV: add one more index before
  ! for NTHIK=0, march 1 more position

!$omp parallel do
  DO lvl = 1, nlays
     IF ( nthik == 0 ) THEN
        index = t_index + 1 
     ELSE
        index = t_index
     END IF

     DO r = 1, nrows_x - nthik
        DO C = 1, nthikd
           index = index + 1
           IF ( lvl >= KPV ) THEN
              PV4O3 = 0.02 * abs ( XPVC(c,r,lvl) )
              o3_b (index,lvl) = max ( o3_b(index,lvl), PV4O3 )
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Fill tracer arrays.  BLTOP is a nominal dividing point for the O3BL and O3FT
! tracers.  Since the same level is desired for all cells and throughout the
! simulation, BLTOP is judiciously selected so that it will (hopefully) not
! change run-to-run.  Use lower-left corner height for convenience since
! precision is not required here.
!-------------------------------------------------------------------------------

  ! Initialize tracer arrays for this time period to 0.0.

  o3bl_b   (:,:) = 0.0
  o3ft_b   (:,:) = 0.0
  o3st_b   (:,:) = 0.0
  o3stadv_b(:,:) = 0.0
  o3sttrn_b(:,:) = 0.0

  ! Find cutoff layer that divides "boundary layer" and "free troposphere".

  DO n = 1, nlays
!   IF ( x3htf(1,1,n) <= bltop ) THEN  ! use full-level height for layer top
    IF ( x3htf_c(1,1,n) <= bltop ) THEN  ! use full-level height for layer top
      CYCLE
    ELSE
      k = n - 1
      EXIT
    ENDIF
    IF ( n == nlays ) THEN
      WRITE (6,9100)
      GOTO 1001
    ENDIF
  ENDDO

  ! Fill tracer arrays.

  o3bl_b   (:,  1:k    ) = o3_b(:,  1:k    )
  o3ft_b   (:,k+1:nlays) = o3_b(:,k+1:nlays)
  o3st_b   (:,    nlays) = o3_b(:,    nlays)
  o3stadv_b(:,    nlays) = o3_b(:,    nlays)
  o3sttrn_b(:,    nlays) = o3_b(:,    nlays)

!-------------------------------------------------------------------------------
! Write CHEM_CRO_3D data (time-dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (chemcro3d_g1) ) THEN
    CALL m3err ('CHEMBC', sdate, stime,  &
                'Could not read DESC of ' // chemcro3d_g1 // ' file', .TRUE.)
  ENDIF

  IF ( .NOT. write3 (chemcro3d_g1, allvar3, sdate, stime, cc3) ) THEN
    WRITE (6,9200) TRIM(chemcro3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Write CHEM_BDY_3D data (time-dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (chembdy3d_g1) ) THEN
    CALL m3err ('CHEMBC', sdate, stime,  &
                'Could not read DESC of ' // chembdy3d_g1 // ' file', .TRUE.)
  ENDIF

! IF ( .NOT. write3 (chembdy3d_g1, allvar3, sdate, stime, cb3) ) THEN
!   WRITE (6,9200) TRIM(chembdy3d_g1)
!   GOTO 1001
! ENDIF

  DO ibc = 1, cb3index
    IF ( .NOT. write3 (chembdy3d_g1, vname3d(ibc), sdate, stime,  &
                       cb3(1,1,ibc)) ) THEN
      WRITE (6,9200) TRIM(chembdy3d_g1)
      GOTO 1001
    ENDIF
  ENDDO

  DO ibc = 1, tb3index
    index = cb3index + ibc
    IF ( .NOT. write3 (chembdy3d_g1, vname3d(index), sdate, stime,  &
                       tb3(1,1,ibc)) ) THEN
      WRITE (6,9200) TRIM(chembdy3d_g1)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xo3 (c,r,k) = dumaray1(c,r,k,1)
          XPVC(c,r,k) = dumaray2(c,r,k,1)
        ENDDO 
      ENDDO
    ENDDO

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

  WRITE (*,'(/,a,/)') '- CHEMBC: Printing sample cells in output grid'

  DO n = 1, cc3index
    WRITE (*,ifmt1) TRIM(cc3vname(n)), cc3(lprt_col,lprt_row,:,n)
  ENDDO

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHEMBC',                             &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHEMBC',                             &
              /, 1x, '***   DID NOT FIND TOP OF BL FOR TRACERS',           &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHEMBC',                             &
              /, 1x, '***   ERROR WRITING TO FILE ', a,                    &
              /, 1x, 70('*'))

 9201 FORMAT( /, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CHEMBC',                             &
              /, 1X, '***   PV Scaling at Layers Starting from:', I4,      &
              /, 1X, '***   X3 coordinate value at starting layer:', F8.6, &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE chembc
