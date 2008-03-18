SUBROUTINE check_flags

!-------------------------------------------------------------------------------
! Name:     Check Flags
! Purpose:  Checks to ensure all input data-capture flags are "true".
! Revised:  11 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Added checks for Kh.  (T. Otte)
!           25 Nov 2003  Added checks for GFS Ozone.  (T. Otte)
!           04 Dec 2003  Added checks for tropopause height.  (T. Otte)
!           31 Mar 2004  Added checks for processing GFS ozone for
!                        time-varying chemical boundary conditions.  (T. Otte)
!           29 Apr 2004  Added checks for 10-m component winds.  (T. Otte)
!           02 Feb 2005  Added checks for radiation fields: albedo, csusf, csdsf
!                                                                (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE file

  IMPLICIT NONE

  CHARACTER*8                  :: field
  INTEGER                      :: n
  CHARACTER*16,  PARAMETER     :: pname      = 'CHECK_FLAGS'

!-------------------------------------------------------------------------------
! Loop over each layer of each 3-d array to verify that all layers are filled.
!-------------------------------------------------------------------------------

  DO n = 1, nz

    IF ( .NOT. ( got_tmp(n) ) ) THEN         ! temperature
      field = 'TMP'
      GOTO 8000
    ELSE IF ( .NOT. ( got_spc_h(n) ) ) THEN  ! specific humidity
      field = 'SPC_H'
      GOTO 8000
    ELSE IF ( .NOT. ( got_u_grd(n) ) ) THEN  ! u-component wind
      field = 'U_GRD'
      GOTO 8000
    ELSE IF ( .NOT. ( got_v_grd(n) ) ) THEN  ! v-component wind
      field = 'V_GRD'
      GOTO 8000
    ELSE IF ( .NOT. ( got_v_vel(n) ) ) THEN  ! vertical velocity
      field = 'V_VEL'
      GOTO 8000
    ELSE IF ( .NOT. ( got_tke(n) ) ) THEN    ! turbulent kinetic energy
      field = 'TKE'
      GOTO 8000
    ELSE IF ( .NOT. ( got_clwmr(n) ) ) THEN  ! cloud water mixing ratio
      field = 'CLWMR'
      GOTO 8000
    ELSE IF ( .NOT. ( got_c_ice(n) ) ) THEN  ! cloud ice mixing ratio
      field = 'C_ICE'
      GOTO 8000
    ELSE IF ( .NOT. ( got_rwmr(n) ) ) THEN   ! rain water mixing ratio
      field = 'RWMR'
      GOTO 8000
    ELSE IF ( .NOT. ( got_snmr(n) ) ) THEN   ! snow mixing ratio
      field = 'SNMR'
      GOTO 8000
    ELSE IF ( .NOT. ( got_t_cdc(n) ) ) THEN  ! total cloud cover
      field = 'T_CDC'
      GOTO 8000
    ELSE IF ( .NOT. ( got_kth(n) ) ) THEN    ! eddy diffusivity
      field = 'KTH'
      GOTO 8000
   ELSE IF ( ( .NOT. ( got_o3mr(n) ) ) .AND. ( makebc ) ) THEN  ! GFS o3 mx rat
      field = 'O3MR'
      GOTO 8000
    ENDIF

  ENDDO

  IF ( .NOT. ( got_kth(nz+1) ) ) THEN
    n = nz+1
    field = 'KTH'
    GOTO 8000
  ENDIF

  DO n = 1, nh
    IF ( .NOT. ( got_geohgt(n) ) ) THEN    ! geopotential height
      field = 'GEOHGT'
      GOTO 8000
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Check 2-d fields.
!-------------------------------------------------------------------------------

  IF ( .NOT. ( got_hgt ) ) THEN      ! surface height
    field = 'HGT'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_temp_2m ) ) THEN  ! 2-m temperature
    field = 'TEMP_2M'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_acpcp ) ) THEN    ! convective precipitation
    field = 'ACPCP'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_fricv ) ) THEN    ! friction velocity
    field = 'FRICV'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_veg ) ) THEN      ! vegetation
    field = 'VEG'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_lhtfl ) ) THEN    ! net latent heat flux
    field = 'LHTFL'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_shtfl ) ) THEN    ! net sensible heat flux
    field = 'SHTFL'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_uswrf ) ) THEN    ! upward short-wave rad flux
    field = 'USWRF'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_ulwrf ) ) THEN    ! long-wave radiation flux
    field = 'ULWRF'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_ncpcp ) ) THEN    ! non-convective precipitation
    field = 'NCPCP'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_psfc ) ) THEN     ! surface pressure
    field = 'PSFC'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_vgtyp ) ) THEN    ! vegetation type
    field = 'VGTYP'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_cnwat ) ) THEN    ! plant canopy water
    field = 'CNWAT'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_skint ) ) THEN    ! skin temperature
    field = 'SKINT'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_hpbl ) ) THEN     ! PBL height
    field = 'HPBL'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_ccond ) ) THEN    ! canopy conductance
    field = 'CCOND'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_sfexc ) ) THEN    ! sfc exchange coeff
    field = 'SFEXC'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_dswrf ) ) THEN    ! dnward short-wave rad flux
    field = 'DSWRF'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_csusf ) ) THEN    ! upward clear sky SW flux
    field = 'CSUSF'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_csdsf ) ) THEN    ! dnward clear sky SW flux
    field = 'CSDSF'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_albdo ) ) THEN    ! Surface mid-day Albedo
    field = 'ALBDO'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_trop ) ) THEN     ! tropopause height
    field = 'TROP'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_u10m ) ) THEN     ! 10-m u-component wind
    field = 'U10M'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_v10m ) ) THEN     ! 10-m v-component wind
    field = 'V10M'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_psccb ) ) THEN    ! shallow convective cloud bottom
    field = 'PSCCB'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_pscct ) ) THEN    ! shallow convective cloud top
    field = 'PSCCT'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_pdccb ) ) THEN    ! deep convective cloud bottom
    field = 'PDCCB'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_pdcct ) ) THEN    ! deep convective cloud top
    field = 'PDCCT'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_ptccb ) ) THEN    ! convective cloud bottom
    field = 'PTCCB'
    GOTO 8100
  ENDIF

  IF ( .NOT. ( got_ptcct ) ) THEN    ! convective cloud top
    field = 'PTCCT'
    GOTO 8100
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) TRIM(field), n
      GOTO 1001

 8100 WRITE (6,9100) TRIM(field)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                              &
              /, 1x, '*** SUBROUTINE: CHECK_FLAGS',        &
              /, 1x, '***   MISSING LAYER IN 3-D FIELD',   &
              /, 1x, '***   FIELD = ', a,                  &
              /, 1x, '***   LAYER = ', i3,                 &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                              &
              /, 1x, '*** SUBROUTINE: CHECK_FLAGS',        &
              /, 1x, '***   MISSING 2-D FIELD',            &
              /, 1x, '***   FIELD = ', a,                  &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE check_flags
