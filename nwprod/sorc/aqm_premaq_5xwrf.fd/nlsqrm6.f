!23456
      subroutine nlsqrm6(premaq_jdate, premaq_jtime)

!***********************************************************************
!
! compute emissions data from temporal factors, nlsqr coefficients and met data
!
! REVISED
!    March 1, 2004 for mobile 6 
!    Apr  29, 2004 changed TEM1P5 to TEMP2 to align with change in PREMAQ
!    May 25, 2004 for calculation of PM/NH3/SO2 from lookup table
!    June 2, 2004 for using 'only' in module variables
!    December 9, 2004 for reduced memory requirements
!    December 21, 2004 for separate pm lookup file for jan-mar
!                      (requires script changes)
!    Jan, 2005 by David Wong
!      -- Expanded EMIS_MODEL from a 2D to a 3D variable and moved its
!         declaration to mod_rw.f and modified the code according to
!         the dimension expansion
!      -- Output EMIS_MODEL and PM_EMIS according to the logical
!         variable store_file
!      -- Replaced integer2string subroutine call by a simple
!         format statement
!    February 11, 2004 : make pm lookup file seasonal since data are seasonal anyway
!                        make switch for 2004 vs 2005 version
!
! user inputs:
!    starting date (0Z)
!    ending date (23Z)
!    pathnames to files (emissions, met)
!    row and column of interest
!    assumptions:
!    data starts at 0Z and ends at 23Z at 1 hour time increments
!    so the number of data points = number of days * 24
!
!***********************************************************************

      use mod_rw
      use mod_mobile, only : outfile, nspecies, specieslist,
     &                       pmspecies, pmspecieslist, pm_emis,
     &                       xm_evap, xm_exh, xm_evapexh, nm,
     &                       kdate, ktime, buffer,
     &                       infile_exh, infile_evap, infile_pm, infile,
     &                       index_no2, index_no, index_co,
     &                       is_2004version, is_2005version

      use mcoutcom, only : temp2_c
      use premaqparm, only : ncols, nrows

      implicit none

      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

!
!******* input parameters
!
      integer :: premaq_jdate          ! date to calculate emissions
      integer :: premaq_jtime          ! time to calculate emissions

!
!******* locals
!
      character(len=16) :: vname_model, this_routine
      character(len=80) :: mesg
      character(len=1)  :: nstring
      character(len=16) :: vname

      integer jdate, jtime, ldate, ltime
      integer s_loop, row, col, hour, n_loop, l_month

!     real, allocatable :: emis_model(:,:)


!
!****** local functions
!
      logical :: is_evap
      integer hr_index, m_index,  s_index

!
!******* functions
!      
      integer trimlen
      external trimlen

      CHARACTER*10    HHMMSS
      external HHMMSS

      real voc_nonlinear_model      
      external voc_nonlinear_model

      WRITE( *, * ) 'Creating Mobile Emissions ...'
      WRITE( *, 94030 ) HHMMSS( premaq_jtime )
      
      ! allocate ( emis_model (ncols, nrows) )
    
      if ( .not. allocated(emis_model) ) then
         allocate (emis_model(ncols, nrows, max(nspecies,pmspecies)))
      endif

      this_routine = 'NLSQRM6-is_2005version'

      jdate = premaq_jdate          
      jtime = premaq_jtime
 
      hour = hr_index(jdate, jtime)

      ldate = kdate
      ltime = ktime

!
!***** calculate kdate and ktime here (get from module)
!
      call nextime(ldate,ltime,(hour-1)*10000)
       
      do s_loop = 1,nspecies

         vname_model = specieslist(s_loop)

       ! *** access xm_evap and xm_exh and pm_lookup

         if ( is_2004version ) then

            if ( is_evap(s_loop, index_no, index_no2, index_co) ) then

               do n_loop = 1,nm      ! *** xm_evap loop
                 ! call integer2string (n_loop, nstring)

                  write (nstring, 10) n_loop

                  vname = specieslist(s_loop)
     &                (1:trimlen(specieslist(s_loop)))//'_COEF'//nstring
                
	          if ( .not. read3( infile_evap, vname, 1,
     &                              ldate, ltime, buffer ) ) then
                     mesg = 'Error reading from file '//
     &                       infile_evap( 1: TRIMLEN( infile_evap ) ) 
                     call m3exit( this_routine, 0, 0, MESG, 2 )
                  endif
		   
                  xm_evap (n_loop, 1:ncols, 1:nrows) = 
     &             buffer (        1:ncols, 1:nrows)
               enddo

            endif          ! is_evap

            do n_loop = 1,nm         ! *** xm_exh loop
              ! call integer2string (n_loop, nstring)

               write (nstring, 10) n_loop

               vname = specieslist(s_loop)
     &                (1:trimlen(specieslist(s_loop)))//'_COEF'//nstring

               if ( .not. read3( infile_exh, vname, 1,
     &                           ldate, ltime, buffer ) ) then
                  mesg = 'Error reading from file '//
     &                    infile_evap( 1: TRIMLEN( infile_exh ) ) 
                  call m3exit( this_routine, 0, 0, MESG, 2 )
               endif
		   
               xm_exh(n_loop,1:ncols,1:nrows) = buffer(1:ncols,1:nrows)
            enddo         ! n_loop

            do row = 1,nrows
               do col = 1,ncols  
                 ! emis_model(col,row) = 
                  emis_model(col,row,s_loop) = 
     &                voc_nonlinear_model
     &                   ( nm, xm_evap(1,col,row), temp2_c(col,row), 0 )
     &               +voc_nonlinear_model
     &                   ( nm,  xm_exh(1,col,row), temp2_c(col,row), 0 )
               enddo
            enddo

         endif            ! is_2004version


         if ( is_2005version ) then

            do n_loop = 1,nm
               write (nstring, 10) n_loop

               vname = specieslist(s_loop)
     &                (1:trimlen(specieslist(s_loop)))//'_COEF'//nstring

	       if ( .not. read3 ( infile, vname, 1,
     &                            ldate, ltime, buffer ) ) then
                  mesg = 'Error reading from file '//
     &                    infile( 1: TRIMLEN( infile ) ) 
                  call m3exit( this_routine, 0, 0, MESG, 2 )
               endif
		   
               xm_evapexh( n_loop, 1:ncols, 1:nrows) = 
     &             buffer(         1:ncols, 1:nrows)
            enddo

            do row = 1,nrows
               do col = 1,ncols  
                  emis_model(col,row,s_loop) = voc_nonlinear_model
     &                  (nm, xm_evapexh(1,col,row), temp2_c(col,row), 0)
                                                         
               enddo
            enddo

         endif           ! is_2005version
	     

         call check_for_neg (ncols, nrows,
!    &                        emis_model(1,1), jdate, jtime,
     &                       emis_model(1,1,s_loop), jdate, jtime,
     &                       specieslist(s_loop), s_loop, hour)

         if (store_file) then

            if ( .not. write3 (outfile, vname_model, jdate, jtime,
!    &                   emis_model)) then
     &                 emis_model(1,1,s_loop) ) ) then
               mesg = 'Error writing '//vname_model//'to file'//
     &                 outfile( 1: TRIMLEN( outfile ) ) 
               call m3exit( this_routine, 0, 0, MESG, 2 )
            endif

         endif

      enddo       ! s_loop   



	        
!         deallocate (emis_model)
!
!***** do pm lookup calculations
!	  


          do s_loop = 1,pmspecies

           vname_model = pmspecieslist(s_loop)
	   
	           if (is_2004version) then
	   
                      l_month = m_index(jdate)
		   endif

	           if (is_2005version) then
	   
                      l_month = s_index(jdate)
		   endif
		   
	           if ( .not. 
     &                 read3(infile_pm,vname_model,l_month,ldate,ltime,
!    &                           pm_emis)) then
     &                           pm_emis(1,1,s_loop))) then
                       mesg = 'Error reading from file '//
     &                   infile_pm( 1: TRIMLEN( infile_pm ) ) 
                       call m3exit( this_routine, 0, 0, MESG, 2 )
                   endif
		   	   
                   if (store_file) then	   

	           if ( .not. write3(outfile,vname_model,jdate,jtime,
!    &                 pm_emis(1,1))) then
     &                 pm_emis(1,1,s_loop))) then
                       mesg = 'Error writing '//vname_model//'to file'//
     &                 outfile( 1: TRIMLEN( outfile ) ) 
                       call m3exit( this_routine, 0, 0, MESG, 2 )
                   end if 
                   end if  !store_file

          enddo ! s_loop         



10      format (i1)
94030   FORMAT( 8X, 'at time ', A8)

      end subroutine nlsqrm6




!################ External Function & subroutines #####################

!**********************************************************************

      integer function hr_index(jdate, jtime)
 
!*******************************************************
!      map jdate, jtime to a 1 to 24*7 indexing scheme
!
!      Monday    0z = 1   = (dayofweek-1)*24 + hr + 1
!      Tuesday   0z = 25  = (dayofweek-1)*24 + hr + 1
!      Wednesday 0z = 49
!*******************************************************

      implicit none
       
      integer jdate, jtime, wkday, day, hr
 
      external wkday

      day = wkday(jdate)

      hr = jtime/10000
      hr_index = (day-1)*24 + hr + 1
       
      return
      end



!**********************************************************************

      subroutine check_for_neg(ncols, nrows, emis_model,
     &                         jdate, jtime, species, s_loop, t_loop)
     
     
      use mod_mobile, only:  neg_ct, total_ct 
     
      implicit none
 
      character*(*) species
    
      integer ncols, nrows, jdate, jtime, s_loop, t_loop

      real emis_model(ncols,nrows)

!
!****** local variables
!
      integer :: i_loop,j_loop


      do j_loop = 1, nrows

         do i_loop = 1, ncols

            if (emis_model(i_loop, j_loop) .lt. 0.0) then

!               write (*,*) 'NOTE: NEGATIVE EMISSIONS ',
!     &          '       = ',emis_model(i_loop, j_loop),
!     &          'at col = ',i_loop, 
!     &          'at row = ',j_loop,
!     &          'for date and time = ',jdate,jtime,
!     &          'for species = ', species
!               write (*,*) 'NOTE: adjusting emissions to zero'

               emis_model(i_loop, j_loop) = 0.0

               neg_ct(i_loop, j_loop, s_loop, t_loop) =
     &         neg_ct(i_loop, j_loop, s_loop, t_loop) + 1

               total_ct = total_ct + 1
            else
               total_ct = total_ct + 1
            endif
         enddo
      enddo

      return

      end
 


!**********************************************************************
  
            integer function m_index(jdate)

!******************************************************* 
! get the month index from the date for pm emissions
!*******************************************************

      implicit none
       
      integer jdate, mnth, mday

      call daymon(jdate,mnth,mday)
       
      if ( (mnth .le. 12) .and. (mnth .ge. 5) ) then
         m_index = mnth-4   ! month of May = 1, June = 2, etc
      endif
       
      if (mnth .le. 4) then
         m_index = mnth     ! for Jan - April time period (different file)
      endif

      return

      end




!**********************************************************************

            integer function s_index(jdate)

!******************************************************* 
! get the seasonal index from the date for pm emissions (2005 version)
!*******************************************************

      implicit none
       
      integer jdate, mnth,mday

      call daymon(jdate, mnth, mday)

!*******************************************************
! s_index   1 = summer (jun,jul,aug)
!           2 = fall   (sep,oct,nov)
!           3 = winter (dec.jan,feb)
!           4 = spring (mar,apr,may)
!*******************************************************

       select case (mnth)
          case (1,2,12)
	     s_index = 3
	  case (3,4,5)
	     s_index = 4
	  case (6,7,8)
	     s_index = 1
	  case (9,10,11)
	     s_index = 2
	  case default
	     write (*,*) 'ERROR in month indexing:',mnth ,
     &                   ' not in range 1 to 12'
	     stop
       end select
       
      return

      end
            



!**********************************************************************

      logical function is_evap(index, index_no, index_no2, index_co)

      integer index, index_no, index_no2, index_co

      is_evap = .true.

      if     (index .eq. index_no ) then
         is_evap = .false.
      elseif (index .eq. index_no2) then
         is_evap = .false.
      elseif (index .eq. index_co ) then
         is_evap = .false.
      endif

      return

      end
