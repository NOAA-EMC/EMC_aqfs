!-----------------------------------------------------------------------------
!
! spread_bias.f90 -- Expand bias corrections at site locations to entire
!		     48-hour forecast grids.
!
! This is a support routine for spreading.f90, part of the NOAA
! NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-mar-12	spread.f:
!		Original fortran 77 stand-alone demo version.
!		By Irina Djalalova, NOAA/ESRL/PSD/CIRES.
!
! 2014-jul-01	spread_bias.f90:
!		Split off calculation routines.  Convert to fortran 90 module.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Speed up, compute full distance matrix on the fly, one time
!		  only, rather than reading iterative indexes from files.
!		Several bug fixes, changing output values.
! 2014-jul-02	Allow zeros and negative values in data at site locations.
!		Improve diagnostics.
! 2014-jul-09	Limit bias corrected values to not less than zero.
!		Note, latent bug.  Corrected output grids could include
!		  uninitialized memory values wherever uncorrected input
!		  grids were missing values.  But NCEP said this would never
!		  happen, original forecasts never contained missing values.
!
! 2015-dec-31	Version in cmaq.v4.7.2, by Jianping Huang.
!		Zero and negative values revert to fraction of uncorrected.
!
! 2016-feb-09	DRA: Add config file selector for output limit method.
!		Add time stamps to spreading loop.
!
! 2017-apr-05	Add more comprehensive summary statistics.
!
! This routine calculates bias over whole grid.
! Objective analysis is used iteratively with 8 runs.
!
! This version returns two result arrays: bias grids, and bias
! corrected forecast grids.  File output is handled by the caller.
!
! Primary inputs:
!
! * Uncorrected gridded hourly model forecasts.
! * Uncorrected forecasts interpolated to site locations.
! * Bias corrected forecasts at site locations.
! * Grid coordinates.
! * Site coordinates.
!
! Outputs:
!
! * Gridded bias array.
! * Bias corrected forecast grids.
!
!-----------------------------------------------------------------------------

module spread__bias
contains

!-------------------------------------------------
! Main spreading routine.
!-------------------------------------------------

subroutine spread_bias (uncorr_grids, uncorr_sites, corr_sites, XLAT, XLON, &
      siteLAT, siteLON, vmiss, diag, output_limit_method, bias_grids, &
      corr_grids)

   use config, only : dp
   use grid__distances
   implicit none

! Input arguments.

   real(dp), intent(in) :: uncorr_grids(:,:,:)	! uncorrected gridded forecasts
						!   for one cycle (X, Y, hours)
   real(dp), intent(in) :: uncorr_sites(:,:)	! uncorrected forecasts at
						!   sites (hours, sites)
   real(dp), intent(in) :: corr_sites(:,:)	! bias corrected forecasts at
						!   sites (hours, sites)
   real(dp), intent(in) :: XLAT(:,:), XLON(:,:)	! grid coordinates
   real(dp), intent(in) :: siteLAT(:)		! site coordinates
   real(dp), intent(in) :: siteLON(:)
   real(dp), intent(in) :: vmiss		! common missing value code
   integer,  intent(in) :: diag			! diag verbosity level, 0-N

   character(*), intent(in) :: output_limit_method  ! output limit method name

! Output arguments.

   real(dp), intent(out), allocatable :: bias_grids (:,:,:)  ! output bias grids
							     !   (X, Y, hours)
   real(dp), intent(out), allocatable :: corr_grids (:,:,:)  ! corrected grids
							     !   (X, Y, hours)
! Local variables.

   character fdate_str*24

   integer xg, yg, nsite, nhours, nrun
   integer i, j, isite, irun
   integer RAD, nvalid, nmiss
   integer npositive, nzero, nnegative

   real(dp) dst, avg, vmin, vmax, percent_miss

! Dynamic arrays.

   real(dp), allocatable :: bias_sites(:,:)	! bias at site locations
   						!   (hours, sites)
   real(dp), allocatable :: sDISTANT(:,:,:)	! index array (sites, X, Y)
   logical,  allocatable :: mask_valid(:,:,:)	! data mask (X, Y, hours)
   real(dp), allocatable :: corr_unlim (:,:,:)  ! corr_grids before limit step

   real(dp), allocatable :: numerator(:)	! bias partial sums (hours)
   integer,  allocatable :: np(:)		! site counts (hours)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   xg     = size (uncorr_grids, 1)		! get dimensions
   yg     = size (uncorr_grids, 2)
   nhours = size (uncorr_grids, 3)
   nsite  = size (siteLAT)

   if (diag >= 3) then
      print *
      print *, 'spread_bias: Input dimensions:'
      print '(a,i0)', '   xg     = ', xg
      print '(a,i0)', '   yg     = ', yg
      print '(a,i0)', '   nhours = ', nhours
      print '(a,i0)', '   nsite  = ', nsite

      print *
      print *, 'spread_bias: Input shapes:'
      print '(a,9(2x,i0))', '   shape (uncorr_grids) =', shape (uncorr_grids)
      print '(a,9(2x,i0))', '   shape (uncorr_sites) =', shape (uncorr_sites)
      print '(a,9(2x,i0))', '   shape (corr_sites)   =', shape (corr_sites)
      print '(a,9(2x,i0))', '   shape (XLAT)         =', shape (XLAT)
      print '(a,9(2x,i0))', '   shape (siteLAT)      =', shape (siteLAT)
      print *
   end if

   if (diag >= 3) print *, 'spread_bias: Allocate arrays.'

   allocate (numerator(nhours), np(nhours))
   allocate (bias_sites(nhours, nsite))
   allocate (bias_grids(xg, yg, nhours), corr_grids(xg, yg, nhours))
   allocate (corr_unlim(xg, yg, nhours))
   allocate (sDISTANT(nsite, xg, yg))

   if (diag >= 3) then
      print '(a,9(2x,i0))', '   shape (numerator)    =', shape (numerator)
      print '(a,9(2x,i0))', '   shape (bias_sites)   =', shape (bias_sites)
      print '(a,9(2x,i0))', '   shape (bias_grids)   =', shape (bias_grids)
      print '(a,9(2x,i0))', '   shape (corr_grids)   =', shape (corr_grids)
      print '(a,9(2x,i0))', '   shape (sDISTANT)     =', shape (sDISTANT)
      print *
   end if

!-------------------------------------------------
! Compute distance matrix.
!-------------------------------------------------

! Calculate full matrix of surface distances between grid points and
! site locations.  To be used at all different Radiuses Of Influence.

   if (diag >= 2) then
      call fdate (fdate_str)
      print '(2a)', fdate_str, ': spread_bias: Compute distance matrix.'
   end if

   call grid_distances (XLAT, XLON, siteLAT, siteLON, sDISTANT)

   PRINT '(2(a,f0.3))', '   MIN (sDISTANT) = ', MINVAL (sDISTANT), &
                        '   MAX (sDISTANT) = ', MAXVAL (sDISTANT)
   print *

!-------------------------------------------------
! Compute Model BIAS: PMKFAN - PMsiteFromGrid
!-------------------------------------------------

   if (diag >= 2) print *, 'spread_bias: Compute bias at site locations.'

   where (uncorr_sites /= vmiss .and. corr_sites /= vmiss)
      bias_sites = corr_sites(:,:) - uncorr_sites(:,:)
   elsewhere
      bias_sites = vmiss
   end where

   PRINT '(2(a,f0.2))', '   MIN (site bias) = ', MINVAL (bias_sites), &
                        '   MAX (site bias) = ', MAXVAL (bias_sites)

   if (diag >= 2) print '(2a,i0)', '   Missing value count, computed bias', &
      ' at sites = ', count (bias_sites == vmiss)
   if (diag >= 2) print *

!-------------------------------------------------
! MAIN SPREADING LOOP
!-------------------------------------------------

! FIRST PASS, Radius of influence=2000km
! Initial guess is PMmod[*,*]=0. - no bias at all
! From each grid point find all observed points inside the circle of influence
! and correct the data in the center

   if (diag >= 2) then
      call fdate (fdate_str)
      print '(2a)', fdate_str, ': spread_bias: Main spreading loop.'
   end if

   bias_grids(:,:,:) = 0		! set all bias points to initial guess

   PRINT '(2(a,f0.2))', '   MIN (bias_grids) = ', MINVAL (bias_grids),&
                        '   MAX (bias_grids) = ', MAXVAL (bias_grids)
   RAD=4000
   nrun=8

run_loop: &
   DO irun=1,nrun
     RAD=RAD/2
     call fdate (fdate_str)
     print '(2a,i0,a,i0)', fdate_str, ': irun=',irun,' RAD=',RAD

!! Calculate the sDISTANT array for the certain Radius 0f Influence
!! 2014-jul-01, moved this to single calculation above.
!!     CALL READ_Iterative_Indexes(RAD,nsite,XG,YG,sDISTANT)
!!     call iterative_indexes (RAD, XLAT, XLON, siteLAT, siteLON, vmiss, &
!!        sDISTANT)

     DO j=1,YG
     DO i=1,XG
       np(:)        = 0
       numerator(:) = 0

site_loop: &
       DO isite=1,nsite

         dst = sDISTANT(isite,i,j)
!            PRINT *,isite,PM25(isite),dst

         if (dst >= RAD) cycle site_loop		! skip sites outside
         						!   radius of influence
         where (bias_sites(:,isite) /= vmiss)
!              PRINT *,isite,PM25(isite),dst
           numerator(:) = numerator(:) + &
             (RAD*RAD-dst*dst) * (bias_sites(:,isite) - bias_grids(i,j,:)) / &
             (RAD*RAD+dst*dst)
           np(:) = np(:) + 1
         end where

       end do site_loop

       where (np(:) > 0)
!            PRINT *,i,j,numerator,np
	 bias_grids(i,j,:) = bias_grids(i,j,:) + numerator(:) / np(:)
       end where

     ENDDO
     ENDDO

   PRINT '(2(a,f0.2))', '   MIN (bias_grids) = ', MINVAL (bias_grids), &
                        '   MAX (bias_grids) = ', MAXVAL (bias_grids)
   end do run_loop

!-------------------------------------------------
! Compute bias corrected forecast grids.
!-------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print '(2a)', ' spread_bias: Apply bias, make corrected', &
      ' forecast grids.'

! Initial bias correction.

   corr_grids(:,:,:) = vmiss			! init to all missing

   where (uncorr_grids(:,:,:) /= vmiss)
      corr_grids = uncorr_grids + bias_grids	! add bias to uncorrected values
   end where

   corr_unlim(:,:,:) = corr_grids(:,:,:)	! save copy for final summary

!-------------------------------------------------
! Apply selected output limit method.
!-------------------------------------------------

   if (diag >= 2) then
      print '(2a)', ' spread_bias: Apply selected limit method.'
      print '(2a)', '   Output limit method = "' &
         // trim (output_limit_method) // '"'
   end if

limit_method: &
   if (output_limit_method == 'none') then

      continue					! no limit, negatives possible

   else if (output_limit_method == 'hard zero') then

      where (corr_grids(:,:,:) /= vmiss)
         corr_grids = max (corr_grids, 0d0)	! limit to not less than zero
      end where

   else

      if (output_limit_method /= 'revert to fraction of uncorrected') then
         print *
         print *, '*** spread_bias: Unknown name for output limit method.'
         print *, '*** Selected method = "' // trim (output_limit_method) // '"'
         print *, '*** Instead will use "revert to fraction of uncorrected".'
      end if

      ! ***** SOFT ERROR.  Fall through to fraction method. *****

!jp0
      DO irun=1,nhours
       do j=1,YG
        do i=1,XG
           if ( corr_grids(i,j,irun) .le. 0.0001 ) then
             corr_grids(i,j,irun) = uncorr_grids(i,j,irun)*0.25 ! no bias corr.
           endif
        enddo
       enddo
      enddo
!jp9

   end if limit_method

!-------------------------------------------------
! Print final data summaries.
!-------------------------------------------------

   print *
   print *, 'spread_bias: Final summary of spreading module:'
   print *

   allocate (mask_valid(xg, yg, nhours))

   mask_valid   = (bias_grids(:,:,:) /= vmiss)
   vmin         = minval (bias_grids, mask_valid)
   vmax         = maxval (bias_grids, mask_valid)

   npositive    = count (mask_valid .and. (bias_grids >  0))
   nzero        = count (mask_valid .and. (bias_grids == 0))
   nnegative    = count (mask_valid .and. (bias_grids <  0))

   nvalid       = count (mask_valid)
   nmiss        = size (mask_valid) - nvalid
   percent_miss = (nmiss * 100.0_dp) / size (mask_valid)

   if (nvalid > 0) then				! compute mean bias
      avg = sum (pack (bias_grids, mask_valid)) / nvalid
   else
      avg = 0
   end if

   if (nvalid == 0) vmin = vmiss		! fix display if all missing
   if (nvalid == 0) vmax = vmiss

   print '(2(a,g0.4))',     '   Min, max bias grids        = ', vmin,', ',vmax
   print '(a,g0.4)',        '   Mean of all bias values    = ', avg
   print '(a,i0)',          '   Number greater than zero   = ', npositive
   print '(a,i0)',          '   Number of zeros            = ', nzero
   print '(a,i0)',          '   Number less than zero      = ', nnegative
   print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
      ' (', percent_miss, '%)'
   print *

!------------

   mask_valid   = (uncorr_grids(:,:,:) /= vmiss)
   vmin         = minval (uncorr_grids, mask_valid)
   vmax         = maxval (uncorr_grids, mask_valid)

   npositive    = count (mask_valid .and. (uncorr_grids >  0))
   nzero        = count (mask_valid .and. (uncorr_grids == 0))
   nnegative    = count (mask_valid .and. (uncorr_grids <  0))

   nvalid       = count (mask_valid)
   nmiss        = size (mask_valid) - nvalid
   percent_miss = (nmiss * 100.0_dp) / size (mask_valid)

   if (nvalid > 0) then				! compute mean uncorrected
      avg = sum (pack (uncorr_grids, mask_valid)) / nvalid
   else
      avg = 0
   end if

   if (nvalid == 0) vmin = vmiss		! fix display if all missing
   if (nvalid == 0) vmax = vmiss

   print '(2(a,g0.4))',     '   Min, max uncorrected grids = ', vmin,', ',vmax
   print '(a,g0.4)',        '   Mean of all uncorr. values = ', avg
   print '(a,i0)',          '   Number greater than zero   = ', npositive
   print '(a,i0)',          '   Number of zeros            = ', nzero
   print '(a,i0)',          '   Number less than zero      = ', nnegative
   print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
      ' (', percent_miss, '%)'
   print *

!------------

   mask_valid   = (corr_unlim(:,:,:) /= vmiss)
   vmin         = minval (corr_unlim, mask_valid)
   vmax         = maxval (corr_unlim, mask_valid)

   npositive    = count (mask_valid .and. (corr_unlim >  0))
   nzero        = count (mask_valid .and. (corr_unlim == 0))
   nnegative    = count (mask_valid .and. (corr_unlim <  0))

   nvalid       = count (mask_valid)
   nmiss        = size (mask_valid) - nvalid
   percent_miss = (nmiss * 100.0_dp) / size (mask_valid)

   if (nvalid > 0) then				! compute mean corrected
      avg = sum (pack (corr_unlim, mask_valid)) / nvalid
   else
      avg = 0
   end if

   if (nvalid == 0) vmin = vmiss		! fix display if all missing
   if (nvalid == 0) vmax = vmiss

   print '(2(a,g0.4))',     '   Min, max corrected grids   = ', vmin,', ',vmax
   print '(a)',             '     before zero limiting'
   print '(a,g0.4)',        '   Mean of all corrected vals = ', avg
   print '(a,i0)',          '   Number greater than zero   = ', npositive
   print '(a,i0)',          '   Number of zeros            = ', nzero
   print '(a,i0)',          '   Number less than zero      = ', nnegative
   print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
      ' (', percent_miss, '%)'
   print *

!------------

   mask_valid   = (corr_grids(:,:,:) /= vmiss)
   vmin         = minval (corr_grids, mask_valid)
   vmax         = maxval (corr_grids, mask_valid)

   npositive    = count (mask_valid .and. (corr_grids >  0))
   nzero        = count (mask_valid .and. (corr_grids == 0))
   nnegative    = count (mask_valid .and. (corr_grids <  0))

   nvalid       = count (mask_valid)
   nmiss        = size (mask_valid) - nvalid
   percent_miss = (nmiss * 100.0_dp) / size (mask_valid)

   if (nvalid > 0) then				! compute mean corrected
      avg = sum (pack (corr_grids, mask_valid)) / nvalid
   else
      avg = 0
   end if

   if (nvalid == 0) vmin = vmiss		! fix display if all missing
   if (nvalid == 0) vmax = vmiss

   print '(2(a,g0.4))',     '   Min, max final corrected   = ', vmin,', ',vmax
   print '(a,g0.4)',        '   Mean all final corrected   = ', avg
   print '(a,i0)',          '   Number greater than zero   = ', npositive
   print '(a,i0)',          '   Number of zeros            = ', nzero
   print '(a,i0)',          '   Number less than zero      = ', nnegative
   print '(a,i0,a,f0.1,a)', '   Number of missing values   = ', nmiss, &
      ' (', percent_miss, '%)'
   print *

   if (diag >= 3) print *, 'spread_bias: Return.'

end subroutine spread_bias
end module spread__bias
