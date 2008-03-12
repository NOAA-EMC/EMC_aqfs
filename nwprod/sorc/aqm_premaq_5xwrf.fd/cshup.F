SUBROUTINE cshup (pdata)

!-------------------------------------------------------------------------------
! Name:     Complex Spherical Harmonic Unpacking
! Purpose:  Unpacks GRIB records that are packed using ECMWF's complex
!           spherical harmonics.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/util/gribcode.F.
!           NCAR notes that this routine was originally adapted from ECMWF's
!           GRIBEX package.
! Revised:  17 May 2002  Original version in REGRID.  (NCAR)
!           08 Jan 2003  Modified for PREGRID.  (T. Otte)
!           26 Jan 2005  Parallelized certain loop(s) using OpenMP construct
!                        (David Wong)
!-------------------------------------------------------------------------------

  USE module_grib

  IMPLICIT NONE

  REAL                         :: bval
  REAL                         :: factor     ( 0:400 )
  INTEGER                      :: i
  INTEGER                      :: ic
  INTEGER,       ALLOCATABLE   :: iexp       ( : )
  INTEGER                      :: il2
  INTEGER                      :: ilast
  INTEGER,       ALLOCATABLE   :: imant      ( : )
  INTEGER                      :: index
  INTEGER                      :: inext
  INTEGER                      :: inum
  INTEGER                      :: ioff
  INTEGER                      :: ipower
  INTEGER                      :: isign
  INTEGER                      :: iskip
  INTEGER                      :: iuc
  INTEGER,       ALLOCATABLE   :: ix         ( : )
  INTEGER                      :: j
  INTEGER                      :: jcol
  INTEGER                      :: jm
  INTEGER                      :: jn
  INTEGER                      :: jrow
  INTEGER                      :: k
  INTEGER                      :: m
  INTEGER                      :: n
  INTEGER                      :: n1
  INTEGER                      :: ndat
  INTEGER                      :: nval
  REAL,          INTENT(OUT)   :: pdata      ( : )
  REAL                         :: power
  REAL                         :: zscale

  ndat = SIZE(pdata)

  ALLOCATE ( ix (ndat+500) )

  index = -1

  IF ( ied == 0 ) THEN
    iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ELSE IF ( ied == 1 ) THEN
    iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ENDIF

  CALL gbyte (grec, n1, iskip, 16)
  iskip = iskip + 16

  CALL gbyte (grec, ipower, iskip, 16)
  iskip = iskip + 16
  IF ( ipower >= 32768 ) ipower = 32768 - ipower

  ! Unpack the resolution parameters for the initial (small) truncation:

  CALL gbyte (grec, j, iskip, 8)
  iskip = iskip + 8
  CALL gbyte (grec, k, iskip, 8)
  iskip = iskip + 8
  CALL gbyte (grec, m, iskip, 8)
  iskip = iskip + 8

  zscale = 2.0**sec4(7)

  iskip = n1 * 8

  nval = ndat - (j+1)*(j+2)

  CALL gbytes (grec, ix, iskip, sec4(8), 0, nval)

  ! SEC4(8) is the number of bits used per datum value.
  ! If SEC4(8) = 255, assume they mean SEC4(8) = 0

  IF ( sec4(8) == 255 ) sec4(8) = 0

  pdata(1:nval) = (Float(ix(1:nval))*zscale)+xec4(1)

  iuc = ndat + 1
  ic  = nval + 1
  DO jm = infogrid(1), 0, -1
    il2  = MAX(jm, j+1)
    inum = 2*(infogrid(1)-il2+1)
    pdata(iuc-inum:iuc-1) = pdata(ic-inum:ic-1)
    iuc = iuc - inum
    ic  = ic  - inum
    iuc = iuc -MAX((il2-jm)*2,0)
  ENDDO

  IF ( ied == 0 ) THEN
    iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
  ELSE IF ( ied == 1 ) THEN
    iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 18*8
  ENDIF

  ALLOCATE ( iexp  (802) )
  ALLOCATE ( imant (802) )

   ilast = j + 1

!$omp parallel do

   DO jrow = 1, ilast

    inext = 2*(ilast-jrow+1)

    ! Extract all the exponents.
    CALL gbytes (grec, iexp, iskip, 8, 24, inext)

    ! Extract all the mantissas.
    ioff = 8
    CALL gbytes (grec, imant, iskip+8, 24, 8, inext)
    iskip = iskip + inext*32

    ! Build the real values from mantissas and exponents.

    bval = 2.0**(-24)
    i = 0
    DO jcol = jrow, infogrid(1)+1
      index = index + 2
      IF ( ilast >= jcol ) THEN
        i = i + 1
        IF ( ( iexp(i) == 128 .OR. iexp(i) == 0 ) .AND.  &
             ( imant(i) == 0 ) ) THEN
          pdata(i) = 0
        ELSE
          IF ( iexp(i) >= 128 ) THEN
            iexp(i) = iexp(i) - 128
            isign = -1
          ELSE
            isign = 1
          ENDIF
          pdata(index) = isign*bval*imant(i)*16.0**(iexp(i)-64)
          i = i + 1
          IF ( iexp(i) >= 128 ) THEN
            iexp(i) = iexp(i) - 128
            isign = -1
          ELSE
            isign = 1
          ENDIF
          pdata(index+1) = isign*bval*imant(i)*16.0**(iexp(i)-64)
        ENDIF
      ENDIF
    ENDDO

  ENDDO

!$omp end parallel do

  ! Apply power scaling.

  IF ( ipower /= 0 ) THEN
    power = float(ipower) / 1000.0
    factor(0) = 1.0
    DO n = 1, infogrid(1)
      IF ( ipower /= 1000 ) THEN
        factor(n) = 1.0 / (n * (n+1))**power
      ELSE
        factor(n) = 1.0 / (n * (n+1))
      ENDIF
    ENDDO
    index = -1
    DO m = 0, j-1
      DO n = m, infogrid(1)
        index = index + 2
        IF ( n >= j ) THEN
          pdata(index:index+1) = pdata(index:index+1) * factor(n)
        ENDIF
      ENDDO
    ENDDO
    DO m = j, infogrid(1)
      DO n = m, infogrid(1)
        index = index + 2
        pdata(index:index+1) = pdata(index:index+1) * factor(n)
      ENDDO
    ENDDO
  ENDIF

  DEALLOCATE ( iexp  )
  DEALLOCATE ( imant )
  DEALLOCATE ( ix    )

END SUBROUTINE cshup
