MODULE chemmech

!-------------------------------------------------------------------------------
! Name:     Chemical Mechanism
! Purpose:  Defines number of chemical species in chemical mechanism.
! Notes:    Used for creating time-varying chemical boundary conditions.
! Revised:  04 Dec 2003  Original version.  (T. Otte)
!           29 Apr 2004  Removed dependency on chemical mechanism for
!                        setting number of species in incoming chemical
!                        boundary conditions file.  Number of species is
!                        now set dynamically.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const

!-------------------------------------------------------------------------------
! Define molecular weight of ozone.
!-------------------------------------------------------------------------------

  REAL,    PARAMETER :: mwo3       = 48.0  ! (oxygen = 16.0) * 3.0

  REAL,    PARAMETER :: mwaovo3    = mwair / mwo3

END MODULE chemmech
