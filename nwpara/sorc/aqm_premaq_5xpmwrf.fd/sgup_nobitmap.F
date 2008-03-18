SUBROUTINE sgup_nobitmap (datarray)

!-------------------------------------------------------------------------------
! Name:     Simple Grid-Point Unpacking (with no Bitmap)
! Purpose:  Unpacks GRIB records that are packed as simple grid points.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/util/gribcode.F.
! Revised:  17 May 2002  Original version in REGRID.  (NCAR)
!           09 Jan 2003  Modified for PREGRID.  (T. Otte)
!           12 Apr 2006  Bug fix (David Wong)
!              -- only call gbytes when sec4(8) is non-zero
!              -- allocate and deallocate ix only when gbytes is called
!-------------------------------------------------------------------------------

  USE module_grib

  IMPLICIT NONE

  REAL                         :: bfac
  REAL,          INTENT(OUT)   :: datarray   ( : )
  REAL                         :: dfac
  INTEGER                      :: iskip
  INTEGER,       ALLOCATABLE   :: ix         ( : )
  INTEGER                      :: ndat

  ndat = SIZE(datarray)

! ALLOCATE ( ix (ndat) )

  dfac = 10.0**(-sec1(24))
  bfac = 2.0**sec4(7)

  IF ( ied == 0 ) THEN
    iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ELSE IF ( ied == 1 ) THEN
    iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ENDIF

! CALL gbytes (grec, ix, iskip, sec4(8), 0, ndat)

  ! SEC4(8) is the number of bits used per datum value.
  ! If SEC4(8) = 255, assume they mean SEC4(8) = 0.

  IF ( sec4(8) == 255 ) sec4(8) = 0

  ! If SEC4(8) is 0, assume DATARRAY is constant value of XEC4(1).

  IF ( sec4(8) == 0 ) THEN
    datarray = dfac *  xec4(1)
  ELSE
    ALLOCATE ( ix (ndat) )
    CALL gbytes (grec, ix, iskip, sec4(8), 0, ndat)
    datarray = dfac * (xec4(1) + (ix*bfac))
    DEALLOCATE ( ix )
  ENDIF

! DEALLOCATE ( ix )

END SUBROUTINE sgup_nobitmap
