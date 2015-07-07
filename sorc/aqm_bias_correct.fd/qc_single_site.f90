!234567
!------------------------------------------------------------------------------
!
! qc_single_site.f90 -- QC for AIRNow generic hourly time series, single site.
!
! 2014-jun-08	qc_single_site.f90:
!		Split off from stand-alone program quality_control.f/f90.
!		Original fortran 77 version by Irina Djalalova, NOAA/CIRES/PSD.
!		Converted to fortran 90 module by Dave Allured.
!		This initial version is calibrated for PM2.5 data only.
! 2014-jun-10	Add diag argument for verbosity control.
!		Add diagnostic for histogram index out of range.
!		Fix histogram indexing bug.
!
! Primary input:  Raw AIRNow hourly time series of PM2.5 concentrations
! for single site.  Complete days, start on hour 0, end on hour 23.
!
! Primary output:  QC'ed time series.
!
!------------------------------------------------------------------------------

module qc__single_site

   private			! visibility
   public qc_single_site	! only the main subroutine is public

contains

subroutine qc_single_site (y, vmiss, diag, site_id)
   use config, only : dp
   implicit none

   real(dp),     intent (inout) :: y(:)		! hourly time series for 1 site
   real(dp),     intent (in   ) :: vmiss	! missing value in time series
   integer,      intent (in   ) :: diag		! diag verbosity level, 0-N
   character(*), intent (in   ) :: site_id	! site ID for diagnostic

! Local program parameters.

   integer, parameter :: nhour=24

! Local variables.

   real(dp) y500(size(y))			! automatic arrays
   real(dp) yis1(size(y))			! same size as input time series
   real(dp) yis3(size(y))
   real(dp) yhis(size(y))
   real(dp) ycnt(size(y))

   real(dp) xar(1:24),yar(1:24)
   real(dp) badflag, sthresh, ymean

   integer result(1:500)
   integer i, i1, i2, j, k, inp, nc, ndays
   integer ibadflag, iymax, ithresh

! Initialize.

   badflag = vmiss				! use caller's missing value
   ibadflag=int(badflag)
   sthresh=50.

   nc = size (y)				! get length of time series
   ndays = nc / nhour				! number of whole days

   y500(:)=badflag				! clear work arrays
   yis1(:)=badflag				! to all missing
   yis3(:)=badflag
   yhis(:)=badflag
   ycnt(:)=badflag

! comment back to remove "0" obs by jphuang 2/19/2015
   do i=1,nc
        if (y(i).eq.0.) then
          y(i)=badflag
        end if
   enddo

!---------------------------------------
! Find all points over 500 microg/m-3
!---------------------------------------

   do i=1,nc
     if (y(i).gt.500.) then
       y500(i)=y(i)
     end if
   enddo

!-------------------------------------------
! Thresh raw data with 500points
!-------------------------------------------

   do i=1,nc
     if (y500(i).gt.0.) then
       y(i)=badflag
     end if
   enddo
   if (diag>=5) print *,'ymax=',maxval(y)

!------------------------------------
! Find isolated hourly value
!------------------------------------

   do i=2,nc-1
     if (y(i).ne.badflag) then
     if ((y(i-1).ne.badflag.and.y(i)-y(i-1).gt.sthresh) &
     .and.(y(i+1).ne.badflag.and.y(i)-y(i+1).gt.sthresh)) then
       yis1(i)=y(i)
     end if
     end if
   enddo

!-------------------------------------------
! Thresh raw data with is1 points
!-------------------------------------------

   do i=1,nc
     if (yis1(i).gt.0) then
       y(i)=badflag
     end if
   enddo
   if (diag>=5) print *,'ymax=',maxval(y)

!----------------------------------------
! Find isolated cluster over MEAN(3 hours)
!----------------------------------------

   do i=3,nc-2
     if (y(i).gt.0) then
       ymean=y(i)
       if (y(i-1).eq.badflag.and.y(i+1).eq.badflag) then
         if ((y(i-2).ne.badflag.and.ymean-y(i-2).gt.sthresh*2.) &
       .and.(y(i+2).ne.badflag.and.ymean-y(i+2).gt.sthresh*2.)) then
           yis3(i)=y(i)
         end if
       end if
       if (y(i-1).gt.0.and.y(i+1).gt.0) then
         ymean=(y(i-1)+y(i)+y(i+1))/3.
       end if
       if (y(i-1).gt.0.and.y(i+1).eq.badflag) then
         ymean=(y(i-1)+y(i))/2.
       end if
       if (y(i-1).eq.badflag.and.y(i+1).gt.0) then
         ymean=(y(i+1)+y(i))/2.
       end if
       if ((y(i-2).ne.badflag.and.ymean-y(i-2).gt.sthresh*2.) &
       .and.(y(i+2).ne.badflag.and.ymean-y(i+2).gt.sthresh*2.)) then
         yis3(i)=y(i)
         yis3(i-1)=y(i-1)
         yis3(i+1)=y(i+1)
       end if
     end if
   enddo

!-------------------------------------------
! Thresh raw data with is3 points
!-------------------------------------------

   do i=1,nc
     if (yis3(i).gt.0) then
       y(i)=badflag
     end if
   enddo
   if (diag>=5) print *,'ymax=',maxval(y)

!------------------------------------
! Thresh by histogram
!------------------------------------

   iymax=int(maxval(y))
   if (diag>=5) print *,'iymax=',iymax
! For ymax below 200 use thresh=50
! For ymax above 200 use thresh=100
   if (iymax.le.200) then
     ithresh=50
   else
     ithresh=100
   end if
   if (diag>=5) print *,'ithresh=',ithresh
! Get histogram
   if (iymax.gt.ithresh) then
     call histy (y, 1, iymax, 1, diag, site_id, result)
!      print *,'y=',y
     if (diag>=5) print *,'result=',result(1:iymax)
   end if
! Check histogram for the ithresh gap
   if (iymax.gt.ithresh) then
   i1=ibadflag
   do i=1,iymax-ithresh+1
     if (result(i).gt.0) then
       i1=i
       i2=ibadflag
       do j=i+1,iymax
         if (result(j).gt.0.and.i2.eq.badflag) then
           i2=j
         end if
       enddo
       if (i1.gt.0.and.i2.gt.0.and.i2-i1.gt.ithresh) then
         if (diag>=5) print *,'i2-i1=',i2-i1
         do k=1,nc
           if (y(k).ge.i2) then
             y(k)=badflag
           end if
         enddo
       end if
     end if
   enddo
   if (diag>=5) print *,'max(y)=',maxval(y)
   end if

!----------------------------------------
! Thresh the constant value (daily check)
!----------------------------------------

   do i=1,ndays
     xar(:)=y(24*(i-1)+1:24*i)
     inp=0
     do j=1,24
       if (xar(j).ne.badflag) then
         inp=inp+1
         yar(inp)=xar(j)
       end if
     enddo
     if (inp.gt.6) then
       if (abs(maxval(yar(1:inp))-minval(yar(1:inp))).lt.0.1) then
         if (diag>=5) print *,i,y(24*(i-1)+1:24*i)
         y(24*(i-1)+1:24*i)=badflag
       end if
     end if
   enddo

   return
end subroutine qc_single_site

!---------------------------------------------------------------------
! Subroutine histy.  Histogram of the integer array.
!---------------------------------------------------------------------

subroutine histy (y, iymin, iymax, ibinsize, diag, site_id, yhist)
   use config, only : dp
   implicit none

   real(dp),     intent (in ) :: y(:)		! data time series
   integer,      intent (in ) :: iymin		!
   integer,      intent (in ) :: iymax		!
   integer,      intent (in ) :: ibinsize	!
   integer,      intent (in ) :: diag		! diag verbosity level, 0-N
   character(*), intent (in ) :: site_id	! site ID for diagnostic

   integer,      intent (out) :: yhist(:)	! result histogram array

! Local variables.

   character data_str*30
   integer i, j, nysize, isize

! Initialize.

   nysize = size (y)				! length of input time series

   isize=int((iymax-iymin)/ibinsize)
   if (diag>=5) PRINT *,'isize=',isize

   yhist(:)=0					! clear histogram array

! Fill histogram array.
! Note, out of range values are currently not counted in ANY bin.

   do i=1,nysize
     if (y(i).gt.0) then
       j = 1 + int (y(i) / ibinsize)

       if (j < 1 .or. j > size (yhist)) then
          if (diag >= 2) then
             write (data_str, '(g11.4)') y(i)
             print '(3a,i6,a,i11,2a)', '** ', site_id, &
                '  histo index out of range: i =', i, '  index =', j, &
                '  data =', trim (data_str)
          end if
       else
          yhist(j) = yhist(j) + 1
       end if

     end if
   enddo

   return
end subroutine histy

end module qc__single_site
