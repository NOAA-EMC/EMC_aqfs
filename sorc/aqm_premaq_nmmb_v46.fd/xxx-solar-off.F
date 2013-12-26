SUBROUTINE solar (sdate, gmt)

!-------------------------------------------------------------------------------
! Name:     Solar Radiation
! Purpose:  Computes
!             1) solar radiation reaching the ground (W/M2),
!             2) shortwave radiation absorbed at the ground (W/M2),
!             3) long wave radiation at the ground (W/M2), 
!             4) net radiation (W/M2),
!             5) and a time-dependent adjustment to albedo, based on
!                zenith angle.  
! Revised:  24 Mar 1998  Original version.  (A. Bourgeois)
!           10 Sep 2001  Converted to free-form f90.  Changed JUDATE to
!                        SDATE.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           02 Sep 2004  Modified for PREMAQ.  (T. Otte)
!           11 Jan 2005  Modified for the cclculation of clear sky radiation
!                        (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
 ! USE date_time

  INCLUDE "xxx-rdparm"

 ! IMPLICIT NONE

  INTEGER                      :: c, r, lvl, igmt, iyr
  INTEGER                      :: yyyy, mo, dd, hh, jjj
  INTEGER                      :: juday

  CHARACTER*16,  PARAMETER     :: pname      = 'SOLAR'

  REAL,          INTENT(IN)    :: gmt
  INTEGER,       INTENT(IN)    :: sdate

 ! REAL                         :: GLAT  (IDIM,JDIM)            &
 !                               , GLON  (IDIM,JDIM)            &
 !                               , ALBEDO(IDIM,JDIM)            &
 !                               , SM    (IDIM,JDIM)            &
 !                               , PRESFC(IDIM,JDIM)            &
 !                               , PMID0 (IDIM,JDIM,  LM)       &
 !                               , PINT0 (IDIM,JDIM,0:LM)       &
 !                               , Q     (IDIM,JDIM,  LM)       &
 !                               , swclr (IDIM,JDIM)            &
 !                               , swtoa (IDIM,JDIM)

!-------------------------------------------------------------------------------
! Get the date information of the initial date
!-------------------------------------------------------------------------------

 ! READ ( aqf_start(1:4),   '(i4.4)' ) yyyy
 ! READ ( aqf_start(6:7),   '(i2.2)' ) mo
 ! READ ( aqf_start(9:10),  '(i2.2)' ) dd
 ! READ ( aqf_start(12:13), '(i2.2)' ) hh

 ! jjj   = julian (yyyy, mo, dd)    !  Julian Day

!-------------------------------------------------------------------------------
! Get the date information of current data
!-------------------------------------------------------------------------------

  juday = MOD(sdate, 1000)
  iyr   = INT(sdate/1000)
  igmt  = INT(gmt+0.5)         ! 0.5=> eliminate trancation error, ex. gmt=1.999

!-------------------------------------------------------------------------------
! Loop over horizontal array and compute solar radiation terms.
!-------------------------------------------------------------------------------

  write(*,*) 'INITIALIZE THE SOLAR CALL'

 ! DO c = 1, ncols_x
 !    DO r = 1, nrows_x
 !       GLAT(c,r)=xlatc(c,r)
 !       GLON(c,r)=xlonc(c,r)
 !       ALBEDO(c,r)=xalbedo(c,r)
 !       SM(c,r)=xdluse(c,r)

 !       IF (SM(c,r) == 0.) THEN
 !          SM(c,r) = 1.                ! Sea in Eta
 !       ELSE
 !          SM(c,r) = 0.                ! Land in Eta
 !       ENDIF

 !       PRESFC(c,r)=xprsfc(c,r)

 !       DO lvl = 1, metlay
 !          Q    (c,r,lvl)=xwvapor(c,r,lvl)
 !          PMID0(c,r,lvl)=xpresm (c,r,lvl)
 !          PINT0(c,r,lvl)=xpresf (c,r,lvl)
 !       ENDDO
 !       PINT0(c,r,0)=xpresf(c,r,0)
 !    ENDDO
 ! ENDDO

  write(*,*) 'READY TO CALL RADTN'
  write(*,*) 'iyr=',iyr,'  igmt=',igmt,'  juday=',juday               &
            ,' ncols_x=',ncols_x,' nrows_x=',nrows_x,' metlay=',metlay

 ! call radtn(GLAT,GLON,ALBEDO,SM,PRESFC,PMID0,PINT0,Q                 &
 !           ,ncols_x,nrows_x,metlay,iyr,igmt,juday                     &
 !           ,yyyy,jjj,hh                                              &
 !           ,swclr,swtoa)

  write(*,*) 'FINISH CALL RADTN'

 ! DO c = 1, ncols_x
 !    DO r = 1, nrows_x
 !       xcstoa(c,r) = swtoa(c,r)
 !       xcsrad(c,r) = swclr(c,r)
 !       IF (xcsrad(c,r) == 0.) then                  ! night time
 !          xattenX(c,r) = 1.
 !       ELSE
 !          xattenX(c,r) = xrgrnd(c,r) / xcsrad(c,r)
!
! Because the clear sky radiation is calculated from the interpolated
! varilables, from Eta to CMAQ, there will be some points have "xatten" 
! value greater than 1.0, most of them are along the sunrise and sunset
! interface.  Here we set the value no more than 1.0
!
 !          xattenX(c,r) = min (xattenX(c,r), 1.0)
 !       ENDIF
 !    ENDDO
 ! ENDDO

!#####################################################################
!
! Calculate the attenuation factor directly from the Eta input data
!
!#####################################################################

  DO r = 1, nrows_x
     DO c = 1, ncols_x
        IF (xcsdsf(c,r) == 0.) then                  ! night time
           xatten(c,r) = 1.
        ELSE
           xatten(c,r) = xrgrnd(c,r) / xcsdsf(c,r)
           xatten(c,r) = min (xatten(c,r), 1.0)
        ENDIF
     ENDDO
  ENDDO

END SUBROUTINE solar

