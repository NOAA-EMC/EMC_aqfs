SUBROUTINE metdot (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Meteorology on Dot Points
! Purpose:  Compute and output MET_DOT_3D parameters.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  24 Jan 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Corrected bugs in calculations in UHAT_S and VHAT_T:
!                        added calls to COLLAPX for XUHAT and XVHAT, and fixed
!                        algorithm to translate from dot points to square and
!                        triangle points.  Also changed COLLAPX calls for
!                        UHAT_S and VHAT_T to be on winds coupled with density
!                        and Jacobian for better mass conservation.  (J. Pleim
!                        and T. Otte)
!           27 Mar 2003  Modified for PREMAQ.  (T. Otte)
!           23 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  (T. Otte)
!           29 Apr 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!           06 Jul 2006  Changed filling of XUHAT_S and XVHAT_T into explicit 
!                        loops over their physical domains to avoid array 
!                        overflow.  (T. Otte and D. Wong)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE coord
  USE fdesc3
  USE file
  USE iodecl3
  USE mdoutcom
  USE vgrd
  USE parms3

  IMPLICIT NONE

  INTEGER                      :: c
  INTEGER                      :: col
  INTEGER                      :: cp1
  REAL,    SAVE, ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  LOGICAL,       SAVE          :: first       = .TRUE.
  CHARACTER*63                 :: ifmt1
  INTEGER                      :: ilg
  INTEGER                      :: index
  INTEGER                      :: jdate
  REAL,    SAVE, ALLOCATABLE   :: jdenm       ( : , : , : )
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: l
  INTEGER                      :: lvl
  INTEGER                      :: n
  CHARACTER*16,  PARAMETER     :: pname       = 'METDOT'
  INTEGER                      :: r
  INTEGER                      :: row
  INTEGER                      :: rp1
  INTEGER,      INTENT(IN)     :: sdate
  INTEGER,      INTENT(IN)     :: stime
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  REAL                         :: xorigdot
  REAL,   SAVE, ALLOCATABLE    :: xuhat_s     ( : , : , : )
  REAL,   SAVE, ALLOCATABLE    :: xvhat_t     ( : , : , : )
  REAL                         :: yorigdot

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,          INTENT(INOUT) :: aa         ( : , : , : )
      REAL,          INTENT(IN)    :: vertin     ( : )
      REAL,          INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( jdenm   ) )  &
    ALLOCATE ( jdenm   (ncols_x,   nrows_x,   metlay) )
  IF ( .NOT. ALLOCATED ( xuhat_s ) )  &
    ALLOCATE ( xuhat_s (ncols_x-1, nrows_x-1, metlay) )
  IF ( .NOT. ALLOCATED ( xvhat_t ) )  &
    ALLOCATE ( xvhat_t (ncols_x-1, nrows_x-1, metlay) )

!-------------------------------------------------------------------------------
! If first time, build rest of header for MET_DOT_3D file.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    CALL comheader (sdate, stime)

    ! Compute XORIGDOT and YORIGDOT.
    ! Assume (XORIG_GD, YORIG_GD) are for cross cells without boundary.

    xorigdot = xorig_gd - 0.5 * xcell_gd
    yorigdot = yorig_gd - 0.5 * ycell_gd

    DO index = 1, md3index
      vtype3d(index) = m3real
      vname3d(index) = md3vname(index)
      units3d(index) = md3units(index)
      vdesc3d(index) = md3vdesc(index)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_DOT'

    xorig3d = xorigdot
    yorig3d = yorigdot
    ncols3d = ncols + 1
    nrows3d = nrows + 1

    ftype3d = grdded3
    nvars3d = md3index
    nlays3d = nlays
    tstep3d = grstep

    ! Write MET_DOT_3D header.

    IF ( .NOT. open3 (metdot3d_g1, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metdot3d_g1)
      GOTO 1001
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header for I/O API files.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Compute density*Jacobian on mid-layers for X-domain.
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        jdenm(c,r,k) = xdensam(c,r,k) * x3jacobm(c,r,k) / xmapc(c,r)**2
     
      ENDDO
    ENDDO
  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Compute contra-u*density*Jacobian at flux (square) points,
! and compute contra-v*density*Jacobian at flux (triangle) points.
!-------------------------------------------------------------------------------

!$omp parallel do

  DO k = 1, metlay

    DO r = 1, nrows_x-2
      rp1 = r + 1
      DO c = 1, ncols_x-1
        cp1 = c + 1

        xuhat_s(c,r,k) = 0.25 * ( jdenm(c,rp1,k) + jdenm(cp1,rp1,k) )  &
                              * ( xuhat(c,r,  k) + xuhat(c,  rp1,k) )

      ENDDO
    ENDDO

    xuhat_s(:,nrows_x-1,k) = xuhat_s(:,nrows_x-2,k)

    DO r = 1, nrows_x-1
      rp1 = r + 1
      DO c = 1, ncols_x-2
        cp1 = c + 1

        xvhat_t(c,r,k) = 0.25 * ( jdenm(cp1,r,k) + jdenm(cp1,rp1,k) )  &
                              * ( xvhat(c,  r,k) + xvhat(cp1,r,  k) )
     
      ENDDO
    ENDDO

    xvhat_t(ncols_x-1,:,k) = xvhat_t(ncols_x-2,:,k)

  ENDDO
!$omp end parallel do

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays.  (Original values are replaced.)
! For safe collpasing, store information first in DUMARRAY.
!   IMODE = 1 : weighted layer averaging
!   IMODE = 2 : selective layer collection 
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
      ALLOCATE ( dumaray1 ( ncols_x-1, nrows_x-1, 1:metlay, 4 ) )

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x-1
        DO c = 1, ncols_x-1
          dumaray1(c,r,k,1) = xuhat_s (c,r,k)
          dumaray1(c,r,k,2) = xvhat_t (c,r,k)
          dumaray1(c,r,k,3) = xuu     (c,r,k)
          dumaray1(c,r,k,4) = xvv     (c,r,k)
        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

    CALL collapx (xuhat_s, xx3midl, x3midl)
    CALL collapx (xvhat_t, xx3midl, x3midl)
    CALL collapx (xuu,     xx3midl, x3midl)
    CALL collapx (xvv,     xx3midl, x3midl)

  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in MET_DOT_3D (time dependent).
!-------------------------------------------------------------------------------

!$omp parallel do
  DO k = 1, nlays
    DO row = 1, nrows+1
      r = row                 ! r = row + nthik for "uninverted" grid
      DO col = 1, ncols+1
        c = col               ! c = col + nthik for "uninverted" grid

        uu_d   (col,row,k) = xuu     (c,r,k)
        vv_d   (col,row,k) = xvv     (c,r,k)
        uhat_s (col,row,k) = xuhat_s (c,r,k)
        vhat_t (col,row,k) = xvhat_t (c,r,k)

      ENDDO
    ENDDO
  ENDDO
  
!add by lipan  
  DO k = 1, nlays
    DO row = 1, nrows
      r = row                 ! r = row + nthik for "uninverted" grid
      DO col = 1, ncols+1
        c = col               ! c = col + nthik for "uninverted" grid
	
        uu_c   (col,row,k) = 0.5 * (xuu(c,r,k)+xuu(c,r+1,k))
	
      ENDDO
    ENDDO
  ENDDO  
  
   uu_c(:,nrows+1,:)=uu_c(:,nrows,:)
   
   
 DO k = 1, nlays
    DO row = 1, nrows+1
      r = row                 ! r = row + nthik for "uninverted" grid
      DO col = 1, ncols
        c = col               ! c = col + nthik for "uninverted" grid
	
        vv_c   (col,row,k) = 0.5 * (xvv(c,r,k)+xvv(c+1,r,k))
	
      ENDDO
    ENDDO
  ENDDO  
  
   vv_c(ncols+1,:,:)=vv_c(ncols,:,:)  
  

!$omp end parallel do

!-------------------------------------------------------------------------------
! Write MET_DOT_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metdot3d_g1) ) THEN
    CALL m3err ('METDOT', sdate, stime,  &
                'Could not read DESC of ' // metdot3d_g1 // ' file', .TRUE.)
  ENDIF

  IF ( .NOT. write3 (metdot3d_g1, allvar3, sdate, stime, md3) ) THEN
    WRITE (6,9100) TRIM(metdot3d_g1)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

!$omp parallel do
    DO k = 1, metlay
      DO r = 1, nrows_x-1
        DO c = 1, ncols_x-1
          xuhat_s (c,r,k) = dumaray1(c,r,k, 1)
          xvhat_t (c,r,k) = dumaray1(c,r,k, 2)
          xuu     (c,r,k) = dumaray1(c,r,k, 3)
          xvv     (c,r,k) = dumaray1(c,r,k, 4)
        ENDDO 
      ENDDO
    ENDDO
!$omp end parallel do

  ENDIF  

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( jdenm    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xuhat_s  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xvhat_t  )  ! commented out to avoid memory fragmentation

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

  WRITE (*,'(/,a,/)') '- METDOT: Printing sample cells in output grid'

  DO n = 1, md3index
    WRITE (*,ifmt1) TRIM(md3vname(n)), md3(lprt_col,lprt_row,:,n)
  ENDDO

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METDOT',                             &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METDOT',                             &
              /, 1x, '***   ERROR WRITING TO FILE ', a,                    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE metdot
