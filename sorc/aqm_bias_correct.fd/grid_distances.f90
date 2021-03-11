!-----------------------------------------------------------------------------
!
! grid_distances.f90 -- Compute distances from grid points to site locations.
!
! This is a support routine for spread.f90, part of the NOAA
! NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-jul-01	Original routines by Irina Djalalova, NOAA/ESRL/PSD/CIRES.
!		Originally part of stand-alone layout.f version 2014-mar-12.
!		Split off by Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Convert to F90, create module for distance calculations only.
! 2014-jul-02	Speed up.  Incorporate subroutine, share common terms.
!
! This version computes a simple, fully populated 3-D matrix of
! earth surface distances between grid points and site locations.
!
! Unlike the original layout.f program, this version does not
! compute masked distances for different radiuses of influence.
!
! This is an optimization.  Radius cutoff is handled more
! efficently in the calling program.  Surface distances are
! computed only once per run.
!
! Inputs:
! * Site coordinates.
! * Grid coordinates.
!
! Output:
! * Matrix of surface distances between grid points and site locations.
!
!-----------------------------------------------------------------------------

module grid__distances
contains

!-------------------------------------------------
! Main routine to calculate distance matrix.
!-------------------------------------------------

subroutine grid_distances (XLAT, XLON, siteLAT, siteLON, sdistance)

   use config, only : dp
   implicit none

! Input arguments.

   real(dp), intent (in) :: XLAT(:,:), XLON(:,:)	! grid coordinates
   real(dp), intent (in) :: siteLAT(:), siteLON(:)	! site coordinates

! Output argument.

   real(dp), intent (out) :: sdistance (:,:,:)		! result index array
							!   (sites, X, Y)
! Local variables.

   integer I0, J0, xg, yg
   integer isite, nsite

   real(dp) xlat1, xlon1
   real(dp) pi, d2r, phi1, phi2
   real(dp) theta1, theta2, xcos, R
   real(dp) sin_phi1, cos_phi1

   real(dp) sin_phi2(size(siteLAT)), cos_phi2(size(siteLAT))

! Initialize.

   xg    = size (XLAT, 1)		! get dimensions
   yg    = size (XLAT, 2)
   nsite = size (siteLAT)

   R   = 6371
   pi  = 2 * ACOS (0.0_dp)		! double precision arg makes
   					! double precision result
   d2r = pi / 180

! Pre-calculate simple trig at site coordinates.

   do isite = 1, nsite
      phi2            = (90.-siteLAT(isite)) * d2r    ! PHI = 90 - Latitude
      sin_phi2(isite) = SIN (phi2)
      cos_phi2(isite) = COS (phi2)
   end do

! Calculation at each grid point and site location.

   DO J0=1,YG
      DO I0=1,XG
         xlat1    = XLAT(I0,J0)
         xlon1    = XLON(I0,J0)
         phi1     = (90.-xlat1) * d2r		! PHI   = 90 - Latitude
         theta1   = xlon1 * d2r			! THETA = Longitude

         sin_phi1 = SIN (phi1)
         cos_phi1 = COS (phi1)

         DO isite=1,nsite
            theta2 = siteLON(isite) * d2r	! THETA = Longitude
            xcos = (sin_phi1 * sin_phi2(isite) * COS (theta1 - theta2)) &
                 + (cos_phi1 * cos_phi2(isite))
            sdistance(isite,I0,J0) = R * ACOS (xcos)
         ENDDO
      ENDDO
   ENDDO

end subroutine grid_distances
end module grid__distances
