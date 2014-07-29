      program fire_exist
      
      parameter(imax=442,jmax=265)
      
      integer header(6),nfire,intx,inty,indomain
      
      real truelat1,truelat2,cenlat,cenlon,tlat,tlon,ddx
      real x,y
      
      character hline*300
      
      data truelat1/45.0/
      data truelat2/33.0/
      data cenlat/40.0/
      data cenlon/-97.0/
      data tlat/21.8212/
      data tlon/-120.628/
      data ddx/12.0/
      
      integer, allocatable,dimension (:) :: year
      integer, allocatable,dimension (:) :: mon
      integer, allocatable,dimension (:) :: day
      integer, allocatable,dimension (:) :: hour          
      integer, allocatable,dimension (:) :: min
      integer, allocatable,dimension (:) :: duration
      
      real, allocatable,dimension (:) :: lat
      real, allocatable,dimension (:) :: lon     
      real, allocatable,dimension (:) :: hgt
      real, allocatable,dimension (:) :: rate 
      real, allocatable,dimension (:) :: area
      real, allocatable,dimension (:) :: heat     
  
              
      open(10,file='EMITIMES',status='old')
      open(20,file='FIRE_CHECK')
      read(10,'(a300)')hline
      read(10,'(a300)')hline
      read(10,*)header(1:6)
      print*,header(1:6)
      
      nfire=header(6)
      
      allocate(year(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2001
      allocate(mon(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2002	    
      allocate(day(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2003
      allocate(hour(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2004 
      allocate(min(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2005
      allocate(duration(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2006	    
      allocate(lat(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2007
      allocate(lon(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2008 
      allocate(hgt(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2009
      allocate(rate(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2010	    
      allocate(area(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2011
      allocate(heat(nfire),STAT=ierr)
      if(ierr.ne.0) stop 2012 
                      
      do n=1,nfire
       read(10,*)year(n),mon(n),day(n),hour(n),min(n),duration(n),
     1  lat(n),lon(n),hgt(n),rate(n),area(n),heat(n)
c       print*, year(n),mon(n),day(n),hour(n),min(n),duration(n),
c     1  lat(n),lon(n),hgt(n),rate(n),area(n),heat(n)

       if(duration(n).ge.2400) then
        call llij_lc(lat(n),lon(n),truelat1,truelat2,cenlat,cenlon,
     1   tlat,tlon,x,y,ddx) 
        intx=int(x)
	inty=int(y)
	print*,lon(n),lat(n),duration(n),intx,inty
	if(intx.ge.1.and.intx.le.imax.and.inty.ge.1.and.
     1    inty.le.jmax)   then
         indomain=1
	 exit
	else
	 indomain=0
	endif
       endif
      enddo
      close(10)
      
      if(indomain.eq.1) then
!jp       print*, 'find the fire in the CONUS' 
        write(20,*)"THERE ARE FILES inside the CONUS DOMAIN"
      else
        write(20,*)"THERE IS NO FILE inside the CONUS DOMAIN"
!jp       print*, 'there is no fire in the CONUS lasting more than 24h'
      endif       
      close(20)
      
      end program fire_exist
      
      SUBROUTINE llij_lc( lat, lon, proj_truelat1, proj_truelat2,cenlat,
     1 cenlon,proj_lat1, proj_lon1, i, j, proj_dx)

!   Subroutine to compute the geographical latitude and longitude values
!   to the cartesian x/y on a Lambert Conformal projection.
    
       REAL, PARAMETER    :: pi = 3.1415927
       REAL, PARAMETER    :: deg_per_rad = 180./pi
       REAL, PARAMETER    :: rad_per_deg = pi / 180.


!     Input Args
       REAL, INTENT(IN)              :: lat      ! Latitude (-90->90 deg N)
       REAL, INTENT(IN)              :: lon      ! Longitude (-180->180 E)
       REAL, INTENT(IN)              :: proj_dx,cenlon,cenlat 
       REAL, INTENT(IN)              :: proj_lat1, proj_lon1

!      Output Args                 
       REAL, INTENT(OUT)             :: i        ! Cartesian X coordinate
       REAL, INTENT(OUT)             :: j        ! Cartesian Y coordinate

!      Locals 
       REAL                          :: arg
       REAL                          :: deltalon
       REAL                          :: tl1r
       REAL                          :: rm
       REAL                          :: ctl1r
    

      if(proj_truelat1.gt.proj_truelat2) then
        temp=proj_truelat1
        proj_truelat1=proj_truelat2
        proj_truelat2=temp
      endif
      
      earth_radius_m=6371.2
    
      proj_rebydx = earth_radius_m / proj_dx

      if(proj_truelat1.lt.0) then
        proj_hemi = -1.
      else  
        proj_hemi = 1
      endif       
    
!      First, see if this is a secant or tangent projection.  For tangent
!      projections, truelat1 = truelat2 and the cone is tangent to the 
!      Earth surface at this latitude.  For secant projections, the cone
!      intersects the Earth surface at each of the distinctly different
!      latitudes

      IF (ABS(proj_truelat1-proj_truelat2) .GT. 0.1) THEN
      ! Compute cone factor following:
       proj_cone=(ALOG(COS(proj_truelat1*rad_per_deg))-
     &  ALOG(COS(proj_truelat2*rad_per_deg)))/
     & (ALOG(TAN((90.-ABS(proj_truelat1))*rad_per_deg*0.5 ))- 
     &  ALOG(TAN((90.-ABS(proj_truelat2))*rad_per_deg*0.5 )) )
      ELSE
       proj_cone = SIN(ABS(proj_truelat1)*rad_per_deg )  
      ENDIF    
      
      deltalon1 = proj_lon1 - cenlon
      IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
      IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.
!     Convert truelat1 to radian and compute COS for later use
      tl1r = proj_truelat1 * rad_per_deg
      ctl1r = COS(tl1r)

!     Compute the radius to our known lower-left (SW) corner
      proj_rsw = proj_rebydx * ctl1r/proj_cone *
     & (TAN((90.*proj_hemi-proj_lat1)*rad_per_deg/2.) / 
     & TAN((90.*proj_hemi-proj_truelat1)*rad_per_deg/2.))**proj_cone

!      Find pole point
            
      arg = proj_cone*(deltalon1*rad_per_deg)
      proj_polei = 1. - proj_hemi * proj_rsw * SIN(arg)
      proj_polej = 1. + proj_rsw * COS(arg) 

!    BEGIN CODE    
!    Compute longitude differences and ensure we stay out of the
!    forbidden "cut zone"    
      deltalon = lon - cenlon
      IF (deltalon .GT. +180.) deltalon = deltalon - 360.
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.      

!      Convert truelat1 to radian and compute COS for later use
      tl1r = proj_truelat1 * rad_per_deg
      ctl1r = COS(tl1r)
      
      
!      Radius to desired point
      rm = proj_rebydx * ctl1r/proj_cone *
     & (TAN((90.*proj_hemi-lat)*rad_per_deg/2.) / 
     & TAN((90.*proj_hemi-proj_truelat1)*rad_per_deg/2.))**proj_cone
      
      arg = proj_cone*(deltalon*rad_per_deg)
      i = proj_polei + proj_hemi * rm * SIN(arg)
      j = proj_polej - rm * COS(arg)

!      Finally, if we are in the southern hemisphere, flip the i/j
!      values to a coordinate system where (1,1) is the SW corner
!      (what we assume) which is different than the original NCEP
!      algorithms which used the NE corner as the origin in the 
!      southern hemisphere (left-hand vs. right-hand coordinate?)
      IF (proj_hemi .EQ. -1.) THEN
        i = 2. - i  
        j = 2. - j
      ENDIF
      RETURN
      END SUBROUTINE llij_lc
