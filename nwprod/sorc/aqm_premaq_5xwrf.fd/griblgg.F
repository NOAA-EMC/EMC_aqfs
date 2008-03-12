SUBROUTINE gribLGG( NLAT, startlat, deltalat )


  implicit none
!
!  LGGAUS finds the Gaussian latitudes by finding the roots of the
!  ordinary Legendre polynomial of degree NLAT using Newton's
!  iteration method.
!
!  On entry:
  integer NLAT ! the number of latitudes (degree of the polynomial)
!
!  On exit: for each Gaussian latitude

  double precision, dimension(NLAT) :: LATG ! Latitude

! Approximations to a regular latitude grid:
  real :: deltalat
  real :: startlat

!-----------------------------------------------------------------------

  integer :: iskip = 15
  double precision :: sum1 = 0.
  double precision :: sum2 = 0.
  double precision :: sum3 = 0.
  double precision :: sum4 = 0.
  double precision :: xn

  integer, save :: SAVE_NLAT = -99
  real, save :: save_deltalat = -99.
  real, save :: save_startlat = -99.

  double precision, dimension(nlat) ::  COSC, SINC
  double precision, parameter :: PI = 3.141592653589793
!
!    -convergence criterion for iteration of cos latitude
  double precision, parameter :: XLIM  = 1.0E-14
  integer :: nzero, i, j
  double precision :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d

  if (nlat == save_nlat) then
     deltalat = save_deltalat
     startlat = save_startlat
     return
  endif
!
!    -the number of zeros between pole and equator
  NZERO = NLAT/2
!
!    -set first guess for cos(colat)
  DO I=1,NZERO
     COSC(I) = SIN( (I-0.5)*PI/NLAT + PI*0.5 )
  ENDDO
!
!    -constants for determining the derivative of the polynomial
  FI  = NLAT
  FI1 = FI+1.0
  A   = FI*FI1 / SQRT(4.0*FI1*FI1-1.0)
  B   = FI1*FI / SQRT(4.0*FI*FI-1.0)
!
!    -loop over latitudes, iterating the search for each root
  DO I=1,NZERO
     J=0
!
!       -determine the value of the ordinary Legendre polynomial for
!       -the current guess root
     LOOP30 : DO 
        CALL LGORD( G, COSC(I), NLAT )
!
!       -determine the derivative of the polynomial at this point
        CALL LGORD( GM, COSC(I), NLAT-1 )
        CALL LGORD( GP, COSC(I), NLAT+1 )
        GT = (COSC(I)*COSC(I)-1.0) / (A*GP-B*GM)
!
!       -update the estimate of the root
        DELTA   = G*GT
        COSC(I) = COSC(I) - DELTA
!
!       -if convergence criterion has not been met, keep trying
        J = J+1
        IF( ABS(DELTA).LE.XLIM ) EXIT LOOP30
     ENDDO LOOP30
  ENDDO
!
!  Determine the sin(colat)
  SINC(1:NZERO) = SIN(ACOS(COSC(1:NZERO)))
!
!    -if NLAT is odd, set values at the equator
  IF( MOD(NLAT,2) .NE. 0 ) THEN
     I       = NZERO+1
     SINC(I) = 1.0
     latg(i) = 0.
  END IF

! Set the latitudes.

  latg(1:NZERO) = dacos(sinc(1:NZERO)) * 180. / pi

! Determine the southern hemisphere values by symmetry
  do i = 1, nzero
     latg(nlat-nzero+i) = -latg(nzero+1-i)
  enddo

! Now that we have the true values, find some approximate values.

  xn = float(nlat-iskip*2)
  do i = iskip+1, nlat-iskip
     sum1 = sum1 + latg(i)*float(i)
     sum2 = sum2 + float(i)
     sum3 = sum3 + latg(i)
     sum4 = sum4 + float(i)**2
  enddo

  b = (xn*sum1 - sum2*sum3) / (xn*sum4 - sum2**2)
  a = (sum3 - b * sum2) / xn

  deltalat = sngl(b)
  startlat = sngl(a + b)

  save_nlat = nlat
  save_deltalat = deltalat
  save_startlat = startlat

contains
  SUBROUTINE LGORD( F, COSC, N )
    implicit none
!
!  LGORD calculates the value of an ordinary Legendre polynomial at a
!  latitude.
!
!  On entry:
!     COSC - cos(colatitude)
!     N      - the degree of the polynomial
!
!  On exit:
!     F      - the value of the Legendre polynomial of degree N at
!              latitude asin(COSC)
    double precision :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
    integer :: n, k

!------------------------------------------------------------------------

    colat = acos(cosc)
    c1 = sqrt(2.0)
    do k=1,n
       c1 = c1 * sqrt( 1.0 - 1.0/(4*k*k) )
    enddo
    fn = n
    ang= fn * colat
    s1 = 0.0
    c4 = 1.0
    a  =-1.0
    b  = 0.0
    do k=0,n,2
       if (k.eq.n) c4 = 0.5 * c4
       s1 = s1 + c4 * cos(ang)
       a  = a + 2.0
       b  = b + 1.0
       fk = k
       ang= colat * (fn-fk-2.0)
       c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
    enddo
    f = s1 * c1
  end subroutine lgord

END SUBROUTINE GRIBLGG
