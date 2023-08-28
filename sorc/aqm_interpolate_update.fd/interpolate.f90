!------------------------------------------------------------------------------
!
! interpolate.f90
!
! This routine performs spatial interpolation of gridded data to specified
! site locations.  Parabolic interpolation over local 4 x 4 grid subsets
! is used.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-feb-20	interpol.f:
!		Original Fortran 77 version for bias correction project.
!		Translated from IDL by Irina Djalalova, NOAA/ESRL/PSD3.
!
! 2014-apr-13	interpol.f90:
!		Convert to fortran 90.  Another stand-alone demo version.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Change internal procedures to module procedures.
!		Convert some innermost code to array methods.
!
! 2014-apr-30	interpolate.f90:
!		Convert from demo main program to support module only.
!		Remove main program code and file I/O.
! 2014-may-06	Remove support routines for interpol.f90 test program.
!		Add exception for sites too close to grid edges.
!		Improve diagnostics.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
! 2014-jun-22	Add subscript protection for coordinates off grid.
!
! 2019-may-20	Allow output array to change size between calls, in support
!		  of varying length forecasts.
!		Fix missing value handling for data ranging both positive
!		  and negative, such as newly added U and V wind.
! 2019-may-21	Remove output lower limit for variables ranging both
!		  positive and negative, such as U and V.  Keep zero
!		  lower limit for variables that must remain non-negative.
!
! 2022-apr-11	Improve off-grid site diagnostics.  Add site ID's.
!		Reduce default verbosity for off-grid.
!
! INPUTS:
! 4-D array with gridded raw forecast data.
! 2-D grid coordinate arrays.
! Site coordinate arrays.
!
! OUTPUTS:
! 3-D array of forecast time series, interpolated to the given
!   site locations.
!
! Variables with fewer forecast hours than the length of the
! gridded data input array must be padded with missing values.
!
! The 3-D output array "interp_data" is dynamically allocated or
! reallocated on each call, in support of variable length forecasts.
!
! Precision notes:
!
! Data values are input and output in single precision.  However,
! intermediate values during interpolation are maintained in
! double precision.
!
! Grid and site coordinates are read and applied in double
! precision.
!
!------------------------------------------------------------------------------

!-----------------------------------------------------------
! Module container.
!-----------------------------------------------------------

module interpolate_mod

   use config, only : dp
   implicit none

   private			! visibility controls
   public interpolate		! only the main interpolation routine is public

contains

!-----------------------------------------------------------
! Main interpolation routine for 4-D gridded time series.
!-----------------------------------------------------------

subroutine interpolate (grid_data, vmiss, grid_lats, grid_lons, site_lats, &
      site_lons, site_ids, diag, interp_data)

   use grid_location
   implicit none

! Input arguments.

   real,        intent(in) :: grid_data(:,:,:,:) ! gridded forecast data
  						 ! (X, Y, vars, hours)
   real,         intent(in) :: vmiss		 ! missing value for in/out data
   real(dp),     intent(in) :: grid_lats(:,:)	 ! grid coordinates (X, Y)
   real(dp),     intent(in) :: grid_lons(:,:)
   real(dp),     intent(in) :: site_lats(:)	 ! site coordinates (S)
   real(dp),     intent(in) :: site_lons(:)
   character(*), intent(in) :: site_ids(:)	 ! site ID's for diagnostics (S)
   integer,      intent(in) :: diag		 ! verbosity level, 0-N

! Output arguments.

   real, intent (out), allocatable :: interp_data(:,:,:)
   					! interpolated time series at
  					! site locations (sites, vars, hours)
! Local variables.

   integer nsites, nx, ny, nvars, nhours
   integer i, i0, j, j0, isite, vi, hi

   real(dp) x, y, input_min
   real(dp) cell_4x4(4,4), interp

   logical in_bounds, off_grid, too_close

! Automatic arrays.

   integer i_corners(size(site_lats)), j_corners(size(site_lats))
   real(dp) distance(size(site_lats))

   real(dp) lower_limit(size(grid_data,3))	! lower bound for each var

! Get array dimensions.

   nsites = size (site_lats)		! site_lats(S)

   nx     = size (grid_data, 1)		! grid_data(X,Y,V,H)
   ny     = size (grid_data, 2)
   nvars  = size (grid_data, 3)
   nhours = size (grid_data, 4)

! Allocate or reallocate the output array.  May change size between calls.

   allocate (interp_data(nsites, nvars, nhours))

! TEST ONLY: Print EPA site locations

   if (diag >= 3) then
     PRINT '(a,99f23.15)', 'siteLAT=', site_lats(16), site_lats(76), &
       site_lats(116), site_lats(176), site_lats(226)
     PRINT '(a,99f23.15)', 'siteLON=', site_lons(16), site_lons(76), &
       site_lons(116), site_lons(176), site_lons(226)
   end if

! Compute grid indices of cell corners for the site locations.

   call gridlocation (site_lats, site_lons, grid_lats, grid_lons, site_ids, &
     diag, i_corners, j_corners, distance)

! Diagnostics.

   if (diag >= 3) then

     DO isite=1,nsites,50
       PRINT '(3i10,99f23.15)', isite, i_corners(isite), j_corners(isite), &
         site_lats(isite), site_lons(isite)
     END DO

     DO isite=1,nsites,50
       i = i_corners(isite)
       j = j_corners(isite)

       in_bounds = (i >= 1 .and. i <= nx .and. j >= 1 .and. j <= ny)

       if (in_bounds) then
          print '(3i10,99f23.15)', isite, i, j, grid_lats(i,j), &
             grid_lons(i,j), grid_data(i, j, 1, 1)
       else
          print '(3i10,a)',        isite, i, j, &
             '  *** coordinates out of bounds'
       end if
     END DO

   end if

! Calculate the lower bound for each interpolated variable, to limit
! occasional overshoot (ringing) from parabolic interpolation.

   do vi = 1, nvars
      input_min = minval (grid_data(:,:,vi,:), &
                                 (grid_data(:,:,vi,:) /= vmiss) )
					! minimum input value for each var,
					! excluding missing values

! Fields that are always non-negative:  Limit to zero or positive.

      if (input_min >= 0) then
         lower_limit(vi) = 0

! Fields that can range negative:  Effectively no lower bound.

      else
         lower_limit(vi) = 100 * input_min
      end if
   end do

!-----------------------------------------------------------
! Compute interpolated values for each site location.
!-----------------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print *, 'Compute interpolated values.'

   if (diag >= 4) print *, '  nsites, nx, ny =', nsites, nx, ny

site_loop: &
   do isite = 1, nsites
     I0 = i_corners(isite)	! lower left corner of 1 x 1 target cell
     J0 = j_corners(isite)

     if (diag >= 4) print *, '  isite,  I0, J0 =', isite, i0, j0

! Special handling for sites off grid, subscripts out of bounds.

     off_grid = (i0 < 1 .or. j0 < 1 .or. i0 > nx .or. j0 > ny)

     if (off_grid) then
       if (diag >= 1) print '(a,3(2x,i0))', '*** Coordinates out of bounds:' &
         // ' isite, i0, j0 =', isite, i0, j0
       interp_data(isite, :, :) = vmiss
       cycle site_loop
     end if

! Special handling for sites too close to an edge for the 4x4 cell.  Like
! the missing value case below, use lower left corner values for this case.

     too_close = (i0 < 2 .or. j0 < 2 .or. i0 > nx-2 .or. j0 > ny-2)

     if (too_close) then
       if (diag >= 3) print '(a,3(2x,i0))', '*** Too close to grid edge:' &
         // ' isite, i0, j0 =', isite, i0, j0
       interp_data(isite,:,:) = grid_data(I0,J0,:,:)
       cycle site_loop
     end if

! Compute fractional X and Y offsets within the 1 x 1 center cell.

     x=(site_lons(isite)-grid_lons(I0,J0))/(grid_lons(I0+1,J0)-grid_lons(I0,J0))
     y=(site_lats(isite)-grid_lats(I0,J0))/(grid_lats(I0,J0+1)-grid_lats(I0,J0))

! Loop over the extra var and hour dimensions.

     do vi = 1, nvars
       do hi = 1, nhours

! Isolate the 4x4 data cell surrounding the site location.

         cell_4x4 = grid_data(i0-1:i0+2, j0-1:j0+2, vi, hi)
				      ! explicit subset, avoid compiler warning

! If any missing values in 4x4 cell, then use the original value from
! the lower left corner of the 1x1 center cell.  Do not interpolate.
! Result might be a valid number, or a missing value.

         if (any (cell_4x4 == vmiss)) then
           interp = cell_4x4(2,2)		! same as grid_data(I0,J0)

! All 16 values in cell are normal.  Do parabolic interpolation.
! Round normal outputs from double to single, using default rounding mode.

         else
           interp = real (PARABOLIC_OVERLAP_16 (x, y, cell_4x4))
           interp = max (interp, lower_limit(vi))
           			! limit variables that should not go negative
         end if

! Insert result into the output array.

         interp_data(isite, vi, hi) = interp
       end do
     end do

   end do site_loop

end subroutine interpolate

!-----------------------------------------------------------
! Parabolic interpolation for one 4 x 4 cell.
!-----------------------------------------------------------

FUNCTION PARABOLIC_OVERLAP_16 (x, y, cell) result (p)
   implicit none

   real(dp), intent (in) :: x, y	! site offsets within center cell
   real(dp), intent (in) :: cell(4,4)	! gridded input data, 4 x 4 cell (X,Y)
   real(dp) p				! function result variable

   real(dp) col(4)			! local var

!   PRINT *,'cell =', cell

! First interpolate slices over the X dimension.

   col(1) = PARABOLIC_ONEDIMENSION (x, cell(:,1))
   col(2) = PARABOLIC_ONEDIMENSION (x, cell(:,2))
   col(3) = PARABOLIC_ONEDIMENSION (x, cell(:,3))
   col(4) = PARABOLIC_ONEDIMENSION (x, cell(:,4))

! Then interpolate X results over the Y dimension, to get the final result.

   p = PARABOLIC_ONEDIMENSION (y, col(:))

!   PRINT *,'a=',a,' b=',b,' c=',c,' d=',d,' p=',p

END FUNCTION PARABOLIC_OVERLAP_16

!-----------------------------------------------------------
! Parabolic interpolation over a single dimension.
!-----------------------------------------------------------

FUNCTION PARABOLIC_ONEDIMENSION (x, row) result (p)
   implicit none

   real(dp), intent (in) :: x		! site offset within center cell
   real(dp), intent (in) :: row(4)	! input data row or column within cell
   real(dp) p				! function result variable

   real(dp) a, b, c, d			! local vars

   a = row(1)
   b = row(2)
   c = row(3)
   d = row(4)

   p=0.

   IF (x.eq.0..or.x.eq.1.) THEN
     IF (x.eq.0.) THEN
       p=b
     END IF
     IF (x.eq.1.) THEN
       p=c
     END IF
   ENDIF

   IF (b*c.ne.0.) THEN
     IF (a*d.eq.0.) THEN
       IF (a.eq.0..and.d.eq.0.) THEN
         p=(1.-x)*b+c*x
       ELSE IF (a.ne.0.) THEN
         p=b+x*(0.5*(c-a)+x*(0.5*(c+a)-b))
       ELSE IF (d.ne.0.) THEN
         p=c+(1.-x)*(0.5*(b-d)+(1.-x)*(0.5*(b+d)-c))
       END IF
     ELSE
       p=(1.-x)*(b+x*(0.5*(c-a)+x*(0.5*(c+a)-b))) &
         + x*(c+(1.0-x)*(0.5*(b-d)+(1.-x)*(0.5*(b+d)-c)))
     END IF
   END IF

END FUNCTION PARABOLIC_ONEDIMENSION

end module interpolate_mod
