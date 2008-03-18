SUBROUTINE sgup_bitmap (datarray)

!-------------------------------------------------------------------------------
! Name:     Simple Grid-Point Unpacking with a Bitmap
! Purpose:  Unpacks GRIB records that are packed as simple grid points with
!           a bitmap.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/util/gribcode.F.
! Revised:  17 May 2002  Original version in REGRID.  (NCAR)
!           08 Jan 2003  Modified for PREGRID.  (T. Otte)
!-------------------------------------------------------------------------------

  USE module_grib

  IMPLICIT NONE

  REAL                         :: bfac
  INTEGER,       ALLOCATABLE   :: bmdat      ( : )
  REAL,          INTENT(OUT)   :: datarray   ( : )
  REAL                         :: dfac
  INTEGER                      :: i
  INTEGER                      :: iskip
  INTEGER                      :: nbm
  INTEGER                      :: ndat
  INTEGER                      :: nn

  ndat = SIZE(datarray)

  ! SEC4(1) : The number of bytes in the whole of GRIB Section 4.
  ! SEC4(6) : The number of unused bits at the end of GRIB Section 4.
  ! SEC4(8) : The number of bits per data value.

  datarray(:) = -1.0E30

  ! There are fewer than NDAT data values, because a bitmap was used.  
  ! Compute the number of data values (NBM).  There are 11 extra bytes
  ! in the header section 4.  NBM equals the total number of data bits (not
  ! counting the header bits), minus the number of unused buts, and then
  ! divided by the number of bits per data value.

  ! If SEC4(8) is 0, assume DATARRAY is constant value of XEC4(1).

  IF ( sec4(8) == 0 ) THEN
    WHERE ( bitmap(1:ndat) == 1 ) datarray = xec4(1)
    RETURN
  ENDIF
  nbm = ( (sec4(1)-11) * 8 - sec4(6) ) / sec4(8)
  ALLOCATE ( bmdat(nbm) )

  ! Compute the parameters involved with packing

  dfac = 10.0**(-sec1(24))
  bfac = 2.0**sec4(7)

  ! Set ISKIP to the beginning of the data.

  IF ( ied == 0 ) THEN
    iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ELSE IF ( ied == 1 ) THEN
    iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ENDIF

  ! Read the data from the GREC array

  CALL gbytes (grec, bmdat, iskip, sec4(8), 0, nbm)

  ! SEC4(8) is the number of bits used per datum value.
  ! If SEC4(8) = 255, assume they mean SEC4(8) = 0.

  IF ( sec4(8) == 255 ) sec4(8) = 0

  ! Unpack the data according to packing parameters DFAC, BFAC, and XEC4(1), 
  ! and masked by the bitmap BITMAP.

   nn = 0
   DO i = 1, ndat
     IF ( bitmap(i) == 1 ) THEN
       nn = nn + 1
      datarray(i) = dfac * (xec4(1) + (bmdat(nn)*bfac))
    ENDIF
  ENDDO

  ! Deallocate the scratch BMDAT array

  DEALLOCATE ( bmdat )

END SUBROUTINE sgup_bitmap
