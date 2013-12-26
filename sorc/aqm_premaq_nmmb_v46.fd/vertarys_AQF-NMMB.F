SUBROUTINE vertarys (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Vertical Arrays of CTM
! Purpose:  Define vertical structure  CTM arrays from input.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE xvars
  USE coord
  USE vgrd

  REAL,          INTENT(IN)    :: ctmlays      ( maxlays )
  CHARACTER*60                 :: ifmt1
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: lbnd
  INTEGER                      :: lbnd2
  INTEGER                      :: nfull
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2

!-------------------------------------------------------------------------------
! VGLVS_GD( 1...NLAYS+1 ):
! The list of vertical CTM coordinate interface values in the VGLVUN_GD units
! Layer k extends from VGLVS3D( k ) to VGLVS3D( k+1 ).
!-------------------------------------------------------------------------------

  vglvs_gd(1:nlays+1) = ctmlays(1:nlays+1)

!-------------------------------------------------------------------------------
! X3FACE_GD( 0: NLAYS ):
! The list of CTM vertical coordinate interface values in the VGLVUN_GD units 
! coverted to values monotonically increasing with altitude.  ( 1 - VGLVS_GD )
!
! ** X3FACE_GD ==> reverse the CTM sigma distribution, "0" is surface interface
!-------------------------------------------------------------------------------

  lbnd = LBOUND(x3face_gd,1)

  DO k = 0, nlays
    x3face_gd(lbnd+k) = 1.0 - vglvs_gd(k+1)
  ENDDO

!-------------------------------------------------------------------------------
! Layer definition for CTM coordinate (monotonic increase with height).
! From X3FACE values, compute X3MIDL (mid-level coord. definition)
! X3MIDL is defined in VGRD.
!
! *** x3 here are referred to defined CTM Level, not from meteorological input
!-------------------------------------------------------------------------------

  lbnd2 = LBOUND(x3face,1)

  x3face(lbnd2:lbnd2+nlays) = x3face_gd(lbnd:lbnd+nlays)

  DO k = 1, nlays
     x3midl(k) = 0.5 * ( x3face(k) + x3face(k-1) )
  ENDDO

!-------------------------------------------------------------------------------
! Echo user-specified grid description info to log file.
!-------------------------------------------------------------------------------

  nfull = nlays + 1

  k1 = nfull / 6
  k2 = MOD(nfull, 6)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,6(2x,f7.5),/," // str1 // "(12x,6(2x,f7.5),/),12x,"   &
         &    // str2 // "(2x,f7.5),/)"
    ELSE
      ifmt1 = "(/,4x,a,6(2x,f7.5),/," // str1 // "(12x,6(2x,f7.5),/))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,6(2x,f7.5),/,12x," // str2 // "(2x,f7.5),/)"
    ELSE
      ifmt1 = "(/,4x,a,6(2x,f7.5),/)"
    ENDIF
  ENDIF

  WRITE (*, "(1x, 78('-'), /)")

  WRITE (*,ifmt1) 'VGLVS3D ', vglvs_gd
  WRITE (*,ifmt1) 'X3FACE  ', x3face_gd
  WRITE (*,ifmt1) 'X3MIDL  ', x3midl

END SUBROUTINE vertarys
