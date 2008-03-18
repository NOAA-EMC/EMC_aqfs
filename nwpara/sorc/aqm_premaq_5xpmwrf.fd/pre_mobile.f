       subroutine pre_mobile6 (premaq_sdate, premaq_stime)

!
!******* get nonlinear least squares coefficients from file
!
!        for air quality forecasting
!      Programmer: George Pouliot
!      Date:       May 13, 2003
!      Revised:    March 1, 2004 for Mobile 6
!                  June 1, 2004 for changes to COEF format for 3x domain
!                  changes were not made for the NE US domain
!                  June 2, 2004 for inclusion of pm lookup table and other gases
!                  (PM2.5, SO2, NH3)
!                  Dec 9, 2004 for reduced memory requirements
!                  Feb 11, 2005 for flags to use either 2004 formats or 2005 formats
!      user inputs:
!      starting date (0Z)
!      ending date (23Z)
!      pathnames to files (emissions, met)
!      row and column of interest
!      assumptions:
!      data starts at 0Z and ends at 23Z at 1 hour time increments
!      so the number of data points = number of days * 24
!

      use mod_mobile, only :outfile, nspecies,specieslist,
     &   xm_evap, xm_exh,xm_evapexh,nm,pmspecies, pmspecieslist,
     &   pm_emis,
     &   nhours, neg_ct,total_ct, nmonths, kdate, ktime, buffer,
     &   index_no, index_no2, index_co,is_2004version, is_2005version,
     &   infile_evap,infile_exh, infile_pm, infile
     
      use coord, only :gdtyp_gd,p_alp_gd,p_bet_gd,p_gam_gd,
     &   xcent_gd,ycent_gd,xcell_gd,ycell_gd,xorig_gd,yorig_gd,gdname_gd
	
      use premaqparm, only:ncols, nrows

      implicit none

      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'


      integer premaq_sdate, premaq_stime

       
      
       integer :: jdate, jtime, tstep
      

     
      
!
!******* loop control variables
!
      integer i_loop,j_loop,s_loop, n_loop, l_loop
      integer :: r_loop, c_loop, t_loop
      integer :: index

      character(len=1)  :: nstring
      character(len=16) :: vname
    
      character(len=16) :: this_routine
      character(len=80) :: mesg

      integer :: ialloc

      integer :: istat, ver_yr

!
!******* external functions
!      

      integer :: envint
      integer :: julian
      integer :: trimlen

      external julian, envint, trimlen






!
!*********** begin main program
!      

      CALL M3MSG2( 
     &  'PRE_MOBILE: begin with revised coef file and pm species')
      CALL M3MSG2(
     &  'PRE_MOBILE: optional mobile file inputs 2004 or 2005 version')
      

      ver_yr = envint('MOBILE_PREMAQ_VERSION',
     &   'version of mobile coefficients',2004,istat)
      select case (ver_yr)
          case (2004)
             is_2004version = .true.
	     is_2005version = .false.	  
	  case (2005)
             is_2004version = .false.
	     is_2005version = .true.	  
	  case default
	      CALL M3MSG2( 'ERROR: invalid version of premaq 
     &             mobile coeffiecents selected')
	      stop
      end select
      
            
      nspecies = 12    
      pmspecies = 8

      

 
       nm = 3         ! for quadratic fit to data
       nhours = 24*7  ! 24 hrs per day/ 7 days per week
!       nmonths = 5    !(May, June, July, August, September)

      if (is_2004version) then
      nmonths = 11       
      endif
      
      if (is_2005version) then
      nmonths = 4  ! represents 4 seasons since each season has same data
      endif

      if (is_2004version) then
      infile_evap = 'COEFS_EVAP'
      infile_exh  = 'COEFS_EXH'
      endif
      
      if (is_2005version) then
      infile = 'COEFS_EVAPEXH'
      endif
      
      outfile = 'MGTS_L'
      this_routine = 'PRE_MOBILE6'
      infile_pm   = 'LOOKUP_PM'


      allocate (specieslist(nspecies),STAT=IALLOC)
      call check_allocation('specieslist',ialloc)

      allocate (pmspecieslist(pmspecies),STAT=IALLOC)
      call check_allocation('pmspecieslist',ialloc)
 


        allocate (buffer(ncols,nrows),STAT=IALLOC)
	      call check_allocation('buffer',ialloc)
        if (is_2004version) then 
        allocate (xm_evap(nm,ncols, nrows),STAT=IALLOC)
	      call check_allocation('xm_evap',ialloc)
        allocate ( xm_exh(nm,ncols, nrows),STAT=IALLOC)
	      call check_allocation('xm_exh',ialloc)      
	endif
	
	if (is_2005version) then
        allocate ( xm_evapexh(nm,ncols, nrows),STAT=IALLOC)
	      call check_allocation('xm_evapexh',ialloc) 	
	endif
	      
        allocate ( pm_emis(ncols, nrows, pmspecies),
     &           STAT=IALLOC)
	      call check_allocation('pm_emis',ialloc)	 
	        

      allocate  (  neg_ct(ncols, nrows, nspecies, nhours),STAT=IALLOC)
            call check_allocation('neg_ct',ialloc)

   
      specieslist(1) = 'ALD2'
      specieslist(2) = 'CO'
      specieslist(3) = 'ETH'
      specieslist(4) = 'FORM'
      specieslist(5) = 'ISOP'
      specieslist(6) = 'NO'
      specieslist(7) = 'NO2'
      specieslist(8) = 'NR'
      specieslist(9) = 'OLE'
      specieslist(10) = 'PAR'
      specieslist(11) = 'TOL'
      specieslist(12) = 'XYL'

      pmspecieslist(1) = 'PEC'     !g/s
      pmspecieslist(2) = 'PMC'     !g/s
      pmspecieslist(3) = 'PMFINE'  !g/s
      pmspecieslist(4) = 'POA'     !g/s
      pmspecieslist(5) = 'PSO4'    !g/s
      pmspecieslist(6) = 'SO2'     !moles/s
      pmspecieslist(7) = 'SULF'    !moles/s
      pmspecieslist(8) = 'NH3'     !moles/s
      


      index_co = 2
      index_no = 6
      index_no2 = 7



!
!****** open coefficients file and read them into the xm array
!

        if (is_2004version) then
        xm_evap(1:nm,1:ncols,1:nrows) = 0.0
         xm_exh(1:nm,1:ncols,1:nrows) = 0.0
        
        CALL M3MSG2('reading evaporative regression coefficients hdr')
        
        
        if ( .not. open3( infile_evap, FSREAD3, this_routine ) ) THEN
           MESG = 'Could not open file "'//
     &      infile_evap( 1: TRIMLEN(infile_evap))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if  

        if ( .not. desc3( infile_evap ) ) THEN
           MESG = 'Could not get dsecription of file "'//
     &           infile_evap( 1: TRIMLEN(infile_evap))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if 


	
         kdate = SDATE3D
	 ktime = STIME3D
	 tstep = TSTEP3D
	 
	 





        CALL M3MSG2('reading exhaust regression coefficients header ..')
        
        if ( .not. open3( infile_exh, FSREAD3, this_routine ) ) THEN
           MESG = 'Could not open file "'//
     &           infile_exh( 1: TRIMLEN(infile_exh))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if  

        if ( .not. desc3( infile_exh ) ) THEN
           MESG = 'Could not get dsecription of file "'//
     &           infile_exh( 1: TRIMLEN(infile_exh))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if 

        end if 
	

        if (is_2005version) then
        xm_evapexh(1:nm,1:ncols,1:nrows) = 0.0

        
        CALL M3MSG2( '2005 version: reading regression coefficients hdr')
        
        
        if ( .not. open3( infile, FSREAD3, this_routine ) ) THEN
           MESG = 'Could not open file "'//
     &      infile_evap( 1: TRIMLEN(infile))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if  

        if ( .not. desc3( infile) ) THEN
           MESG = 'Could not get dsecription of file "'//
     &           infile_evap( 1: TRIMLEN(infile))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if 


	
         kdate = SDATE3D
	 ktime = STIME3D
	 tstep = TSTEP3D
	 

        end if 
	


	 CALL M3MSG2( 'reading in pm/so2/nh3 lookup table header')
   


        if ( .not. open3( infile_pm, FSREAD3, this_routine ) ) THEN
           MESG = 'Could not open file "'//
     &           infile_pm( 1: TRIMLEN(infile_pm))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if  

        if ( .not. desc3( infile_pm ) ) THEN
           MESG = 'Could not get dsecription of file "'//
     &           infile_pm( 1: TRIMLEN(infile_pm))//
     &      '" for input'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if 

       


	

	 

    

        CALL M3MSG2('setting up header for mobile emissions output file')

!
!******** set up IOAPI for emissions output file
!
        FTYPE3D = 1
        NCOLS3D = ncols
        NROWS3D = nrows
        NLAYS3D = 1
        GDTYP3D = gdtyp_gd  !coord_mod
        VGTYP3D = -1
        P_ALP3D = p_alp_gd  !coord_mod
        P_BET3D = p_bet_gd  !coord_mod
        P_GAM3D = p_gam_gd  !coord_mod
        XCENT3D = xcent_gd  !coord_mod
        YCENT3D = ycent_gd  !coord_mod
        XCELL3D = xcell_gd
        YCELL3D = ycell_gd
        XORIG3D = xorig_gd
        YORIG3D = yorig_gd
        GDNAM3D = gdname_gd
        NVARS3D = nspecies+pmspecies
        


        do s_loop = 1, nspecies                 
           VNAME3D(s_loop) = specieslist(s_loop)
           VTYPE3D(s_loop) = M3REAL
           UNITS3D(s_loop) = 'mole/s'
        enddo
        
	
	do s_loop = nspecies+1, nspecies+5
           VNAME3D(s_loop) = pmspecieslist(s_loop-nspecies)
           VTYPE3D(s_loop) = M3REAL
           UNITS3D(s_loop) = 'g/s'
        enddo

        do s_loop = nspecies+6, nspecies+pmspecies
           VNAME3D(s_loop) = pmspecieslist(s_loop-nspecies)
           VTYPE3D(s_loop) = M3REAL
           UNITS3D(s_loop) = 'mole/s'
        enddo
		        
			

        SDATE3D = premaq_sdate
        STIME3D = premaq_stime
        TSTEP3D = 10000

        CALL M3MSG2('opening mobile source emission output file ... ')
        
        if ( .not. open3( outfile, FSUNKN3, this_routine ) ) THEN
           MESG = 'Could not open file "'//outfile( 1:
     &                  TRIMLEN(outfile))// '" for output'
           CALL M3EXIT( this_routine, 0, 0, MESG, 2 )
        end if  

        neg_ct = 0   ! array assignment
        total_ct = 0

        CALL M3MSG2('end of pre_mobile6')

      return
      
      end subroutine pre_mobile6

      subroutine check_allocation(string,ialloc)
      implicit none
      character*(*) :: string
      integer :: ialloc
      if (ialloc .eq. 1) then
           write (*,*) 
     &      'ALLOCATION ERROR 1 (system allocation) for var '//string
	   stop
      endif
      if (ialloc .eq. 2) then
           write (*,*) 
     &     'ALLOCATION ERROR 2 (invalid object) for var '//string
	   stop
      endif
      return
      end
