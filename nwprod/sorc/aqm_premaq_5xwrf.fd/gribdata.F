SUBROUTINE gribdata (datarray) 

!-------------------------------------------------------------------------------
! Name:     GRIB Data
! Purpose:  Read and unpack the data from a GRIB record.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/util/gribcode.F.
! Revised:  17 May 2002  Original version in REGRID.  (NCAR)
!           08 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE module_grib

  IMPLICIT NONE

  REAL,          INTENT(OUT)   :: datarray   ( : )

  INTERFACE

    SUBROUTINE cshup (datarray)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: datarray   ( : )
    END SUBROUTINE cshup

    SUBROUTINE sgup_bitmap (datarray)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: datarray   ( : )
    END SUBROUTINE sgup_bitmap

    SUBROUTINE sgup_nobitmap (datarray)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: datarray   ( : )
    END SUBROUTINE sgup_nobitmap

  END INTERFACE

!-------------------------------------------------------------------------------
! From header information, determine what type of unpacking is required.
! Then call the appropriate routine to unpack the data.
!-------------------------------------------------------------------------------

  IF ( sec4(2) == 0 ) THEN  ! grid-point data

    IF ( sec4(3) == 0 ) THEN  ! simple unpacking

      IF ( sec1(26) == 1 ) THEN  ! there's a bitmap
        CALL sgup_bitmap (datarray)
      ELSE
        CALL sgup_nobitmap (datarray)
      ENDIF

    ELSE

      WRITE (*,'(//,"***** No complex unpacking of gridpoint data.")')
      WRITE (*,'("***** Option not yet available.",//)')
      STOP

    ENDIF

  ELSE

    IF ( sec4(3) == 0 ) THEN  ! simple unpacking
      WRITE (*,'(//,"***** No simple unpacking of spherical-harmonic coefficients.")')
      WRITE (*,'("***** Option not yet available.",//)')
      STOP
    ELSE IF ( sec4(3) == 1 ) THEN
      CALL cshup (datarray)
    ENDIF

  ENDIF

END SUBROUTINE gribdata
