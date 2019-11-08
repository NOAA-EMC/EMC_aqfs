SUBROUTINE fill_metarys (jdisc,itabe,iparm,ktype,ztype,lvl1,lvl2,ipcp)

!SUBROUTINE fill_metarys (iparm, ktype, lvl1, ipcp)


!-----------------------------------------------------------------------
! Name:     Fill Meteorology Arrays
! Purpose:  Puts GRIB record into appropriate meteorology array.  Also
!           translates scalar variables to Arakawa B grid (cross points)
!           if necessary and if input field was not filled from nearest-
!           neighbor in NCEP's PRDGEN.
! Revised:  16 Apr 2003  Original version.  (T. Otte)
!           20 Oct 2003  Added new field Kh.  (T. Otte)
!           25 Nov 2003  Added new field GFS Ozone.  (T. Otte)
!           04 Dec 2003  Added new field tropopause height.  (T. Otte)
!           05 Jan 2004  Added variable IPCP to keep track of when Eta
!                        accumulated precipitation buckets are dumped.
!                        (T. Otte)
!           20 Feb 2004  Added capability for Kh to be in either 
!                        GRIB ID 246 or 182.  (T. Otte)
!           30 Mar 2004  Modified to invert dot-point and cross-point
!                        grids.  Removed INFIL from argument list.  (T. Otte)
!           29 Apr 2004  Added new fields 10-m component winds.  (T. Otte)
!           02 Sep 2004  Added albedo.  (T. Otte)
!           13 Jan 2005  Added csusf, csdsf, and Reversioned.   (Hsin-mu Lin)
!           08 Feb 2005  Added convective cloud fiels (pressure): psccb,
!                        pscct, pdccb, pdccb, ptccb, ptcct      (Hsin-mu Lin)
!
!.......................................................................
!
! ** iparm & ktype follow the "plot_cards" of Eta plotting routine
!    & according to NCEP ON-388
!
! INPUTS:                      w3lib (wgrib)
!     iparm : PDS Octet  9    (KPDS 5), ON388-TABLE 2
!     ktype : PDS Octet 10    (KPDS 6), ON388-TABLE 3 & 3a
!     lvl1  : PDS Octet 11-12 (KPDS 7), ON388-TABLE 3
!     ipcp  : 1-h accumulated precip index, "rdgrib.F"
!
!-----------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE premaqparm, ONLY: neta1, nmlays

  IMPLICIT NONE

  REAL,          PARAMETER     :: epsilon    = 0.005

  INTEGER,       INTENT(IN)    :: iparm            ! PDS Octet 9
  INTEGER,       INTENT(IN)    :: jdisc            ! add by lipan 
  INTEGER,       INTENT(IN)    :: itabe            ! add by lipan     
  INTEGER,       INTENT(IN)    :: ipcp
  INTEGER                      :: ivalue
  INTEGER,       INTENT(IN)    :: ktype            ! PDS Octet 10
  INTEGER,       INTENT(IN)    :: ztype            ! add by lipan  
  INTEGER,       INTENT(IN)    :: lvl1
  INTEGER,       INTENT(IN)    :: lvl2             ! add by lipan  
  INTEGER                      :: n
  INTEGER                      :: i,c,r
  INTEGER                      :: igbpv(21),idx

  INTEGER                      :: kpds71, kpds72

  CHARACTER*16,  PARAMETER     :: pname      = 'FILL_METARYS'
  
  EXTERNAL       ENVYN

  LOGICAL        ENVYN, MIXHT_4_PBL, IGBP_4_USGS
  INTEGER        ISTAT  
  

  INTERFACE

    SUBROUTINE crs2dot (varcrs, vardot)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: vardot     ( : , : )
      REAL,          INTENT(IN)    :: varcrs     ( : , : )
    END SUBROUTINE crs2dot

  END INTERFACE
  
  ! Transfer vegetation in IGBP to USGS
  ! the order is from igbp=0 and the related # is USGS value.
  !  *** begin from igbp=0 that maps to USGS=16          (11/17/2011, Formosian)

  DATA (igbpv(i),i=1,21)   /   16,                                            &
                               14,  13,  12,  11,  15,  8,  8,  10,  10,  7,  &
                               17,   2,   1 ,  5,  24, 19, 16,  21,  22, 23   /
			         
  IGBP_4_USGS = ENVYN ('IGBP_4_USGS', 'IGBP TO USGS ?', .TRUE. , ISTAT)
  
!-----------------------------------------------------------------------
! Based on GRIB parameter ID & level, put GRIB data into appropriatae
! array.
!  If INFIL = 1, field was filled by standard bi-linear interpolation in 
!                NCEP's PRDGEN.
!  If INFIL = 2, field was filled by mapping from nearest-neighbor in
!                NCEP's PRDGEN.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! sigmah & sigmaf are sigma values defined in running script "metlayh"
!
! lvl1 is the value read from meteorological input
!   when ktype=107,  lvl1 ==> sigma value
!   when ktype=109,  lvl1 ==> level #
!-----------------------------------------------------------------------

!! Begin Hybrid level (ktype=109) properties

!  IF ( ktype == 109 ) THEN       ! Hybrid level
  IF ( ktype == 105 .and. ztype == 255) THEN       ! Hybrid level
  
!     IF ( iparm == 1 ) THEN                   ! 3D Pressure
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                   ! 3D Pressure     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              pres3d(:,:,n) = scr2d(:,:)
              got_pres3d(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 7  ) THEN              ! geopotential height
     ELSEIF ( itabe == 3 .and. iparm == 5 .and. jdisc == 0 ) THEN              ! geopotential height     
        DO n = 1, nh
           IF ( nh == nz ) THEN
              ivalue = n
           ELSEIF ( nh == nz+1 ) THEN
              ivalue = n
           ENDIF

           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              geohgt(:,:,n) = scr2d(:,:)
              got_geohgt(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nh ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 11 ) THEN              ! 3D temp
     ELSEIF ( itabe == 0 .and. iparm == 0 .and. jdisc == 0 ) THEN              ! 3D temp     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              tmp(:,:,n) = scr2d(:,:)
              got_tmp(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 33 ) THEN              ! 3D u-comp wind
     ELSEIF ( itabe == 2 .and. iparm == 2 .and. jdisc == 0 ) THEN              ! 3D u-comp wind     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              u_grda(:,:,n) = scr2d(:,:)                 ! origional A-grid wind
              CALL crs2dot (scr2d(:,:), u_grd(:,:,n))    ! from A-grid to B-grid
              got_u_grd(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 34 ) THEN              ! 3D v-comp wind
     ELSEIF ( itabe == 2 .and. iparm == 3 .and. jdisc == 0 ) THEN              ! 3D v-comp wind     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              v_grda(:,:,n) = scr2d(:,:)                 ! origional A-grid wind
              CALL crs2dot (scr2d(:,:), v_grd(:,:,n))    ! from A-grid to B-grid
              got_v_grd(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 39 ) THEN              ! 3D vert veloc
     ELSEIF ( itabe == 2 .and. iparm == 8 .and. jdisc == 0 ) THEN              ! 3D vert veloc     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              v_vel(:,:,n) = scr2d(:,:)
              got_v_vel(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 51 ) THEN              ! 3D specific humidity
     ELSEIF ( itabe == 1 .and. iparm == 0 .and. jdisc == 0 ) THEN              ! 3D specific humidity     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              spc_h(:,:,n) = scr2d(:,:)
              got_spc_h(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 58 ) THEN             ! cloud ice mixing ratio
     ELSEIF ( itabe == 6 .and. iparm == 0 .and. jdisc == 0 ) THEN             ! cloud ice mixing ratio     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              c_ice(:,:,n) = scr2d(:,:)
              got_c_ice(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 71 ) THEN               ! cld cover
     ELSEIF ( itabe == 6 .and. iparm == 1 .and. jdisc == 0 ) THEN               ! cld cover     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              t_cdc(:,:,n) = scr2d(:,:)
              got_t_cdc(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 153 ) THEN             ! cloud water mixing ratio
     ELSEIF ( itabe == 1 .and. iparm == 22 .and. jdisc == 0 ) THEN             ! cloud water mixing ratio     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              clwmr(:,:,n) = scr2d(:,:)
              got_clwmr(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 154 ) THEN             ! ozone
     ELSEIF ( itabe == 14 .and. iparm == 1 .and. jdisc == 0 ) THEN             ! ozone     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              o3mr(:,:,n) = scr2d(:,:)
              got_o3mr(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 158 ) THEN             ! 3D TKE
     ELSEIF ( itabe == 19 .and. iparm == 11 .and. jdisc == 0 ) THEN             ! 3D TKE     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              tke(:,:,n) = scr2d(:,:)
              got_tke(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 170 ) THEN             ! rain water mixing ratio
     ELSEIF ( itabe == 1 .and. iparm == 24 .and. jdisc == 0 ) THEN             ! rain water mixing ratio     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              rwmr(:,:,n) = scr2d(:,:)
              got_rwmr(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( iparm == 171 ) THEN              ! snow mixing ratio
     ELSEIF ( itabe == 1 .and. iparm == 25 .and. jdisc == 0 ) THEN              ! snow mixing ratio     
        DO n = 1, nz
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              snmr(:,:,n) = scr2d(:,:)
              got_snmr(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz ) GOTO 8000
        ENDDO

!     ELSEIF ( ( iparm == 246 ) .OR. ( iparm == 182 ) ) THEN      ! Kh
     ELSEIF ( itabe == 2 .and. iparm == 204 .and. jdisc == 0  ) THEN      ! Kh     
        DO n = 1, nz+1
           ivalue = n
           IF ( ABS(ivalue - lvl1) < epsilon ) THEN
              kth(:,:,n) = scr2d(:,:)
              got_kth(n) = .TRUE.
              EXIT
           ENDIF
           IF ( n == nz+1 ) GOTO 8000
        ENDDO
     ENDIF

!! Begin Surface (ktype=1) properties

  ELSEIF ( ktype == 1 ) THEN      ! Ground or Water surface

!     IF ( iparm == 1 ) THEN                ! pressure (surface)
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure (surface)     
        IF ( lvl1 == 0 ) THEN
           psfc(:,:) = scr2d(:,:)
           got_psfc  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 7 ) THEN            ! geopotential height
     ELSEIF ( itabe == 3 .and. iparm == 5 .and. jdisc == 0 ) THEN            ! geopotential height     
        IF ( lvl1 == 0 ) THEN
           hgt(:,:) = scr2d(:,:)
           got_hgt  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 11 ) THEN           ! temperature (skin)
     ELSEIF ( itabe == 0 .and. iparm == 0 .and. jdisc == 0 ) THEN           ! temperature (skin)     
        IF ( lvl1 == 0 ) THEN
           skint(:,:) = scr2d(:,:)
           got_skint  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 62 ) THEN           ! non-conv pcp
     ELSEIF ( itabe == 1 .and. iparm == 9 .and. jdisc == 0 ) THEN           ! non-conv pcp     
        IF ( lvl1 == 0 ) THEN
           ncpcp(:,:) = scr2d(:,:)
           ncpcp(:,:) = ncpcp(:,:) * 0.1   ! convert from mm to cm
           scr2d(:,:) = ncpcp(:,:)
           IF ( (ipcp.eq.1).or.( sum(scr2d).lt.sum(rnold))) THEN
              ncpcp = MAX(0.0,  ncpcp)     ! accum bucket just emptied
           ELSE
              ncpcp = MAX(0.0, (ncpcp-rnold)) ! incremental total
           ENDIF
           rnold = scr2d
           got_ncpcp = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 63 ) THEN           ! convc precip
     ELSEIF ( itabe == 1 .and. iparm == 10 .and. jdisc == 0 ) THEN           ! convc precip     
        IF ( lvl1 == 0 ) THEN
           acpcp(:,:) = scr2d(:,:)
           acpcp(:,:) = acpcp(:,:) * 0.1   ! convert from mm to cm
           scr2d(:,:) = acpcp(:,:)
           IF (( ipcp.eq.1).or.(sum(scr2d).lt.sum(rcold)) ) THEN
              acpcp = MAX(0.0,  acpcp)     ! accum bucket just emptied
           ELSE
              acpcp = MAX(0.0, (acpcp-rcold)) ! incremental total
           ENDIF
           rcold = scr2d
           got_acpcp = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 67 ) THEN           ! Mixed layer depth [m]
     ELSEIF ( itabe == 19 .and. iparm == 3 .and. jdisc == 0 ) THEN           ! Mixed layer depth [m]     
        IF ( lvl1 == 0 ) THEN
           hmixht(:,:) = scr2d(:,:)
           got_hmixht  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 84 ) THEN           ! albedo
     ELSEIF ( itabe == 19 .and. iparm == 1 .and. jdisc == 0 ) THEN           ! albedo     
        IF ( lvl1 == 0 ) THEN
           albdo(:,:) = scr2d(:,:)
           got_albdo  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 87 ) THEN           ! vegetation (%)
     ELSEIF ( itabe == 0 .and. iparm == 4 .and. jdisc == 2 ) THEN           ! vegetation (%)     
        IF ( lvl1 == 0 ) THEN
           veg(:,:) = scr2d(:,:)
           got_veg  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 121 ) THEN          ! latent heat flux
     ELSEIF ( itabe == 0 .and. iparm == 10 .and. jdisc == 0 ) THEN          ! latent heat flux     
        IF ( lvl1 == 0 ) THEN
           lhtfl(:,:) = scr2d(:,:)
           got_lhtfl  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 122 ) THEN          ! sensitive heat flux
     ELSEIF ( itabe == 0 .and. iparm == 11 .and. jdisc == 0 ) THEN          ! sensitive heat flux     
        IF ( lvl1 == 0 ) THEN
           shtfl(:,:) = scr2d(:,:)
           got_shtfl  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
	
!     ELSEIF ( iparm == 142 ) THEN          ! surface ice pellets add by lipan
     ELSEIF ( itabe == 1 .and. iparm == 35 .and. jdisc == 0 ) THEN          ! surface ice pellets add by lipan     
        IF ( lvl1 == 0 ) THEN
           cicep(:,:) = scr2d(:,:)
           got_cicep  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
	
!     ELSEIF ( iparm == 160 ) THEN          ! upward clear sky SW flux
     ELSEIF ( itabe == 4 .and. iparm == 198 .and. jdisc == 0 ) THEN          ! upward clear sky SW flux     
        IF ( lvl1 == 0 ) THEN
           csusf(:,:) = scr2d(:,:)
           got_csusf  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 161 ) THEN          ! dnward clear sky SW flux
    ELSEIF ( itabe == 4 .and. iparm == 196 .and. jdisc == 0 ) THEN          ! dnward clear sky SW flux     
        IF ( lvl1 == 0 ) THEN
           csdsf(:,:) = scr2d(:,:)
           got_csdsf  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 181 ) THEN          ! canopy conductance
     ELSEIF ( itabe == 0 .and. iparm == 199 .and. jdisc == 2 ) THEN          ! canopy conductance     
        IF ( lvl1 == 0 ) THEN
           ccond(:,:) = scr2d(:,:)
           got_ccond  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 182 ) THEN          ! Leaf Area Idex
     ELSEIF ( itabe == 7 .and. iparm == 198 .and. jdisc == 0 ) THEN          ! Leaf Area Idex     
        IF ( lvl1 == 0 ) THEN
           lai(:,:) = scr2d(:,:)
           got_lai  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 204 ) THEN          ! dnward SW rad flux
     ELSEIF ( itabe == 4 .and. iparm == 192 .and. jdisc == 0 ) THEN          ! dnward SW rad flux          
        IF ( lvl1 == 0 ) THEN
           dswrf(:,:) = scr2d(:,:)
           got_dswrf  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 208 ) THEN          ! sfc exchange coeff
!jp     ELSEIF ( itabe == 0 .and. iparm == 195 .and. jdisc == 2 ) THEN          ! sfc exchange coeff     
     ELSEIF ( itabe == 0 .and. iparm == 12 .and. jdisc == 2 ) THEN          ! sfc exchange coeff     
        IF ( lvl1 == 0 ) THEN
           sfexc(:,:) = scr2d(:,:)
           got_sfexc  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 211 ) THEN          ! upward SW rad flux
     ELSEIF ( itabe == 4 .and. iparm == 193 .and. jdisc == 0 ) THEN          ! upward SW rad flux     
        IF ( lvl1 == 0 ) THEN
           uswrf(:,:) = scr2d(:,:)
           got_uswrf  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 212 ) THEN          ! LW rad flux
     ELSEIF ( itabe == 5 .and. iparm == 193 .and. jdisc == 0 ) THEN          ! LW rad flux     
        IF ( lvl1 == 0 ) THEN
           ulwrf(:,:) = scr2d(:,:)
           got_ulwrf  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 221 ) THEN          ! PBL height
     ELSEIF ( itabe == 3 .and. iparm == 196 .and. jdisc == 0 ) THEN          ! PBL height     
        IF ( lvl1 == 0 ) THEN
           hpbl(:,:) = scr2d(:,:)
           got_hpbl  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 223 ) THEN          ! plant canopy water
     ELSEIF ( itabe ==0 .and. iparm == 196 .and. jdisc == 2 ) THEN          ! plant canopy water     
        IF ( lvl1 == 0 ) THEN
           cnwat(:,:) = scr2d(:,:)
           got_cnwat  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 224 ) THEN          ! soil type
     ELSEIF ( itabe == 3 .and. iparm == 0 .and. jdisc == 2 ) THEN          ! soil type     
        IF ( lvl1 == 0 ) THEN
           sotyp(:,:) = NINT(scr2d(:,:))
	   sltyp(:,:) = NINT(scr2d(:,:))    !add by lipan
           got_sotyp  = .TRUE.
	   got_sltyp  = .TRUE.              !add by lipan
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 225 ) THEN          ! vegetation type
     ELSEIF ( itabe == 0 .and. iparm == 198 .and. jdisc == 2 ) THEN          ! vegetation type     
        IF ( lvl1 == 0 ) THEN
           vgtyp(:,:) = NINT(scr2d(:,:))
           got_vgtyp  = .TRUE.
	   IF ( IGBP_4_USGS ) THEN
	    do c = 1, nx    !transfer vegetaion in IGBP to USGS
	     do r = 1, ny
	      idx = vgtyp(c,r)
	      vgtyp(c,r) = igbpv(idx+1)  ! igbp begin from "0"
	     enddo
	    enddo
	   ENDIF
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 238 ) THEN          ! Snow Cover
     ELSEIF ( itabe == 1 .and. iparm == 201 .and. jdisc == 0 ) THEN          ! Snow Cover     
        IF ( lvl1 == 0 ) THEN
           snowc(:,:) = scr2d(:,:)
	   snocov(:,:)= scr2d(:,:)  !add by lipan
           got_snowc  = .TRUE.
	   got_snocov = .TRUE.      !add by lipan
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 253 ) THEN          ! friction velocity
     ELSEIF ( itabe == 2 .and. iparm == 197 .and. jdisc == 0 ) THEN          ! friction velocity     
        IF ( lvl1 == 0 ) THEN
           fricv(:,:) = scr2d(:,:)
           got_fricv  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

!! Begin Special field (temporatory assigned)

  ELSEIF ( ktype == 220  ) THEN
!     IF ( iparm == 7 ) THEN                ! PBL height, based on Richardson number
     IF ( itabe == 3 .and. iparm == 5 .and. jdisc == 0 ) THEN                ! PBL height, based on Richardson number     
        IF ( lvl1 == 0 ) THEN
           hpblr(:,:) = scr2d(:,:)
!!	   print*, "hpblr=",hpblr(10,10),hpblr(50,50),hpblr(100,100)
!!	   print*, "hpblr=",hpblr(150,150),hpblr(200,200),hpblr(250,50)	   
           got_hpblr  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

!! Begin Convective cloud properties

  ELSEIF ( ktype == 242 ) THEN      ! convective cloud bottom
!     IF ( iparm == 1 ) THEN                ! pressure
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure     
        IF ( lvl1 == 0 ) THEN
           ptccb(:,:) = NINT(scr2d(:,:))
           got_ptccb  = .TRUE.
! below added by jp 5/30
           psccb(:,:) = NINT(scr2d(:,:))
           got_psccb  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

  ELSEIF ( ktype == 243 ) THEN      ! convective cloud top

!     IF ( iparm == 1 ) THEN                ! pressure
      IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure      
        IF ( lvl1 == 0 ) THEN
           ptcct(:,:) = NINT(scr2d(:,:))
           got_ptcct  = .TRUE.
!below added by jp 5/30
           pscct(:,:) = NINT(scr2d(:,:))
           got_pscct  = .TRUE.

        ELSE
           GOTO 8100
        ENDIF
     ENDIF

!jp  ELSEIF ( ktype == 248 ) THEN      ! shallow convective cloud bottom
!  ELSEIF ( ktype == 242 ) THEN      ! use convective cloud bottom !jp

!!     IF ( iparm == 1 ) THEN                ! pressure
!     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure      
!1        IF ( lvl1 == 0 ) THEN
!           psccb(:,:) = NINT(scr2d(:,:))
!           got_psccb  = .TRUE.
!        ELSE
!           GOTO 8100
!        ENDIF
!     ENDIF

!!jp  ELSEIF ( ktype == 249 ) THEN      ! shallow convective cloud top
!  ELSEIF ( ktype == 243 ) THEN      ! use convective cloud top !jp

!!     IF ( iparm == 1 ) THEN                ! pressure
!     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure     
!        IF ( lvl1 == 0 ) THEN
!           pscct(:,:) = NINT(scr2d(:,:))
!           got_pscct  = .TRUE.
!        ELSE
!           GOTO 8100
!        ENDIF
!     ENDIF

  ELSEIF ( ktype == 251 ) THEN      ! deep convective cloud bottom

!     IF ( iparm == 1 ) THEN                ! pressure
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure      
        IF ( lvl1 == 0 ) THEN
           pdccb(:,:) = NINT(scr2d(:,:))
           got_pdccb  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

  ELSEIF ( ktype == 252 ) THEN      ! deep convective cloud top

!     IF ( iparm == 1 ) THEN                ! pressure
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure     
        IF ( lvl1 == 0 ) THEN
           pdcct(:,:) = NINT(scr2d(:,:))
           got_pdcct  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

!! Begin Tropopause (ktype=7) properties

  ELSEIF ( ktype == 7 ) THEN        ! Tropopause
!     IF ( iparm == 1 ) THEN                ! pressure
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN                ! pressure 
        IF ( lvl1 == 0 ) THEN
           trop(:,:) = scr2d(:,:)
           got_trop  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
     ENDIF

!!  Begin properties at specific height (ktype=105)

!  ELSEIF ( ktype == 105 ) THEN    ! Specific height level above ground
  ELSEIF ( ktype == 103 ) THEN    ! Specific height level above ground
!     IF ( iparm == 11 ) THEN               ! 2-m temp
     IF ( itabe == 0 .and. iparm == 0 .and. jdisc == 0 ) THEN               ! 2-m temp     
        IF ( lvl1 == 2 ) THEN
           temp_2m(:,:) = scr2d(:,:)
           got_temp_2m  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 33 ) THEN           ! 10-m u-comp wind
     ELSEIF ( itabe == 2 .and. iparm == 2 .and. jdisc == 0 ) THEN           ! 10-m u-comp wind     
        IF ( lvl1 == 10 ) THEN
           u10m(:,:) = scr2d(:,:)
           got_u10m  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF

!     ELSEIF ( iparm == 34 ) THEN           ! 10-m v-comp wind
     ELSEIF ( itabe == 2 .and. iparm == 3 .and. jdisc == 0 ) THEN           ! 10-m v-comp wind     
        IF ( lvl1 == 10 ) THEN
           v10m(:,:) = scr2d(:,:)
           got_v10m  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF
	
!     ELSEIF ( iparm == 51 ) THEN           ! 2-m mixing ratio add by lipan
     ELSEIF ( itabe == 1 .and. iparm == 0 .and. jdisc == 0 ) THEN           ! 2-m mixing ratio add by lipan     
        IF ( lvl1 == 2 ) THEN
           q2m(:,:) = scr2d(:,:)
           got_q2m  = .TRUE.
        ELSE
           GOTO 8100
        ENDIF	
     ENDIF

 !!  Begin properties for PD & PDTOP (data is between 2 interface)

!  ELSEIF ( ktype == 110 ) THEN    ! Between 2 hybrid level (mock)
  ELSEIF ( ktype == 105 .and. ztype == 105 ) THEN    ! Between 2 hybrid level (mock)
  
!     IF ( iparm == 1 ) THEN
     IF ( itabe == 3 .and. iparm == 0 .and. jdisc == 0 ) THEN     
!        kpds71 = 256 + neta1
!        kpds72 = 256 + nmlays+1
        kpds71 = neta1+1
        kpds72 = nmlays+1	
!        IF ( lvl1 == kpds71 ) THEN          ! PDTOP
        IF ( lvl2 == kpds71 ) THEN          ! PDTOP	
           pdtop(:,:) = scr2d(:,:)
           got_pdtop  = .TRUE.

!        ELSEIF ( lvl1 == kpds72 ) THEN      ! PD
        ELSEIF ( lvl2 == kpds72 ) THEN      ! PD	
           pdbot(:,:) = scr2d(:,:)
           got_pd  = .TRUE.
        ELSE
          write(6,*)'error get PD/PDTOP. lvl2,neta1,nmlays,kpds71,kpds72=',lvl2,neta1,nmlays,kpds71,kpds72
           GOTO 8100
        ENDIF
     ENDIF

 !!  Begin properties for SOIL

!  ELSEIF ( ktype == 112 ) THEN    ! SOIL
  ELSEIF ( ktype == 106 .and. ztype == 106 ) THEN    ! SOIL
  
!     IF ( iparm == 144 ) THEN             ! SOILW 
     IF ( itabe == 0 .and. iparm == 192 .and. jdisc == 2 ) THEN             ! SOILW      
!        IF ( lvl1 == 10  ) THEN           ! 0-10 cm down
        IF ( lvl1 == 0 .and. lvl2 == 10  ) THEN           ! 0-10 cm down	
           soilw(:,:) = scr2d(:,:)
	   soim1(:,:) = scr2d(:,:)
           got_soilw  = .TRUE.
	   got_soim1 = .TRUE.
!        ELSEIF ( lvl1 == 2600  ) THEN           ! 10-40 cm down add by lipan
        ELSEIF ( lvl1 == 10 .and. lvl2 == 40  ) THEN           ! 10-40 cm down add by lipan	
           soim2(:,:) = scr2d(:,:)
           got_soim2  = .TRUE.	   
        ELSE
!           GOTO 8100
        ENDIF
     
!     ELSEIF ( iparm == 85 ) THEN             ! SOIT TEMPERATURE add by lipan 
     ELSEIF ( itabe == 0 .and. iparm == 2 .and. jdisc == 2 ) THEN             ! SOIT TEMPERATURE add by lipan     
!        IF ( lvl1 == 10  ) THEN           ! 0-10 cm down
        IF ( lvl1 == 0 .and. lvl2 == 10  ) THEN           ! 0-10 cm down	
	   soit1(:,:) = scr2d(:,:)
	   got_soit1 = .TRUE.
!        ELSEIF ( lvl1 == 2600  ) THEN           ! 10-40 cm down
        ELSEIF ( lvl1 == 10 .and. lvl2 == 40  ) THEN           ! 10-40 cm down	
           soit2(:,:) = scr2d(:,:)
           got_soit2  = .TRUE.	   
        ELSE
!           GOTO 8100
        ENDIF
     ENDIF
     
  ELSE

!    GOTO 8200

  ENDIF

  RETURN

!-----------------------------------------------------------------------
! Error-handling section.
!-----------------------------------------------------------------------

 8000 WRITE (6,9000) iparm, ktype, lvl1
      GOTO 1001

 8100 WRITE (6,9100) iparm, ktype, lvl1
      write(6,*)'itabe,ztype,jdisc,lvl2=',itabe,ztype,jdisc,lvl2
      GOTO 1001

 8200 WRITE (6,9200) iparm, ktype, lvl1
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: FILL_METARYS',      &
              /, 1x, '***   IMPROPER VALUE FOR LVL1',     &
              /, 1x, '***   LVL1 MUST MATCH A Level #'    &
              /, 1x, '***   IPARM, KTYPE = ', 2(i3, 2x),  &
              /, 1x, '***   LVL1 = ', (i3, 2x),           &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: FILL_METARYS',      &
              /, 1x, '***   IMPROPER VALUE FOR LVL1',     &
              /, 1x, '***   LVL1 SHOULD BE A 2-D FIELD',  &
              /, 1x, '***   IPARM, KTYPE = ', 2(i3, 2x),  &
              /, 1x, '***   LVL1 = ', (i3, 2x),           &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: FILL_METARYS',      &
              /, 1x, '***   UNKNOWN VARIABLE',            &
              /, 1x, '***   IPARM, KTYPE = ', 2(i3, 2x),  &
              /, 1x, '***   LVL1 = ', (i3, 2x),           &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE fill_metarys
