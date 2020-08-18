
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module dust_emis
     
C-----------------------------------------------------------------------
C Description:

C Revision History:
C 16 Dec 10 J.Young: Adapting Daniel Tong`s work on windblown dust
C 21 Apr 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C 11 May 11 D.Wong: incorporated twoway model implementation
C  8 Jul 11 J.Young: unified string lengths in character lists for compiler compatibility
C 11 Nov 11 J.Young: generalizing land use/cover
C  8 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C 13 Jul 12 J.Young: following Daniel Tong: changed clayc, siltc, sandc units from mass
C                    fraction to %; adjusted F/G (vertical to horizontal flux) ratio
C                    to be continuous for clay content > 20%
C 30 Sep 13 J.Young: corrected diag file units description; added snow cover adjustment;
C                    adjusted F/G (vertical to horizontal flux) ratio to be continuous
C                    for clay content > 0.2; convert volumetric soil moisture to
C                    gravimetric water content; corrected soil moisture factor (fmoit);
C                    use lwmask>0 rather than sltyp>0 (non-existent) for over water test
C 15 Sep 15 H.Foroutan: revised threshold friction velocity parameterization
C 20 Oct 15 H.Foroutan: Updated the calculation of the threshold velocity(U*t), which is
C                       now based on dust particle size, following Shao and Lu [JGR,2000].
C                       Implemented a dynamic vegetation fraction based on the MODIS FPAR.
C                       Introduced a new parametrization for surface roughness (z0)
C                       applicable to dust emission schemes, and accordingly calculated
C                       the friction velocity (U*) at the surface using 10m wind speed
C                       and the new (microspcopic) surface roughness.
C                       Surface roughness adjusted for estimated annual vegetation height.
C                       Included drag partitioning coefficient. Updated the calculation of
C                       the vertical-to-horizontal flux based on Lu and Shao [JGR,1999].
C                       Updated the dust diag output file accordingly.
C  8 Jan 16 J.Young: Changes for computational efficiency
C  2 Feb 16 J.Young: move dust aero speciation table to AERO_DATA
C  01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all MY_N clauses
C  9 Jul 19 Gilliam: Removed a lot of old commented out legacy tables. 
C                    Removed the direct read of FPAR MODIS file as this data comes 
C                    directly from WRF LSM models that have access to MODIS veg data now.
C                    Number of soil types fixed to 16 to match WRF and tables updated.
C  1 May 20 B.Baker: Rolling this back to the FENGSHA dust emission code.  Removing land
C                    cover dependency. Removing new parameterization for surface roughness.
C                    Removing particle size dependency on threshold velociy. Adding
C                    new albedo based drag partition and sediment supply map. Removing
C                    sp_ed.  Use u* from the model to make it consistant instead of 
C                    recalculating.  Adding dynamic soil moisture limit calculation with
C                    clay and sand fractions. Update from NAQFC FENGSHA. Adding threshold
C                    velocity map that can change without recompiling code. 
C                    Threshold tool can be found at 
C                    https://github.com/noaa-oar-arl/fengsha_threshold_map@NAQFC
C                    
C-----------------------------------------------------------------------
      use lus_defn
      use aero_data
      use emis_vars

      implicit none 

      public ndust_spc, dust_spc,
     &       dust_emis_init, get_dust_emis
      private

      real, allocatable, save :: dust_em( :,: )  ! total dust emissions [g/m**3/s]

C updated values of mass fraction for "freshly emitted dust"
C based on Kok [PNAS, 2011] and Nabat et al. [ACP, 2012]
      real, parameter :: fracmj = 0.07  ! mass fraction assigned to accum mode
      real, parameter :: fracmk = 0.93  ! mass fraction assigned to coarse mode

C diam`s from fracmj,fracmk-weighted 2 2-bin averages of geom means
C 2 J-mode bins: 0.1-1.0, 1.0-2.5 um
C 2 K-mode bins: 2.5-5.0, 5.0-10.0 um
      real, parameter :: dgvj = 1.3914  ! geom mean diam of accum mode [um]
      real, parameter :: dgvk = 5.2590  ! geom mean diam of coarse mode [um]
      real, parameter :: sigj = 2.0000  ! geom std deviation of accum mode flux
      real, parameter :: sigk = 2.0000  ! geom std deviation of coarse mode flux

C Local Variables:

C Factors for converting 3rd moment emission rates into number and 2nd moment
C emission rates.  (Diameters in [um] changed to [m] ) See Equations 7b and 7c
C of Binkowski & Roselle (2003)
      real       :: l2sgj    ! [ln( sigj )] ** 2
      real       :: l2sgk    ! [ln( sigk )] ** 2
      real, save :: factnumj ! = exp( 4.5 * l2sgj ) / dgvj ** 3 * 1.0e18
      real, save :: factnumk ! = exp( 4.5 * l2sgk ) / dgvk ** 3 * 1.0e18
      real, save :: factm2j  ! = exp( 0.5 * l2sgj ) / dgvj * 1.0e6
      real, save :: factm2k  ! = exp( 0.5 * l2sgk ) / dgvk * 1.0e6
      real, save :: factsrfj ! = pi * factm2j
      real, save :: factsrfk ! = pi * factm2k

      real       :: sumsplit, sumfrac
      integer    :: n, idx

C Number of soil types: For WRF there are 16 types;
      integer, parameter :: nsltyp = 16

C Variables for the windblown dust diagnostic file:
      integer, parameter :: fndust_diag = 19 ! number of fixed diagnostic output vars
      integer, save      :: ndust_diag       ! number of diagnostic output vars
      real, allocatable, save :: diagv( : )  ! diagnostic output variables
      real, allocatable, save :: dustbf( :,:,: ) ! diagnostic accumulate buffer

#ifdef verbose_wbdust
      real, allocatable, save :: sdiagv( : )  ! global sum of each diag output var
#endif

      type diag_type
         character( 16 ) :: var
         character( 16 ) :: units
         character( 80 ) :: desc
      end type diag_type

      type( diag_type ), allocatable, save :: diagnm( : )
      type( diag_type ), allocatable, save :: vdiagnm_emis( : )
      type( diag_type ), allocatable, save :: vdiagnm_frac( : )
      type( diag_type ), allocatable, save :: vdiagnm_ustar( : )
      type( diag_type ), allocatable, save :: vdiagnm_kvh( : )
      type( diag_type ), allocatable, save :: vdiagnm_rough( : )

      character( 10 ) :: truncnm
      character( 16 ) :: vnm

      type( diag_type ), parameter :: fdiagnm( fndust_diag ) = (/
C                      var              units                 desc
C                 ----------------    --------    -------------------------------------------
     & diag_type( 'Cropland_Emis   ', 'g m-3 s-1', 'emissions for cropland landuse type        '),
     & diag_type( 'Desertland_Emis ', 'g m-3 s-1', 'total emis for desert types and cropland   '),
     & diag_type( 'Cropland_Frac   ', 'percent  ', 'cropland erodible landuse fraction (%)     '),
     & diag_type( 'Desertland_Frac ', 'percent  ', 'total desert fraction (%)                  '),
     & diag_type( 'Cropland_Ustar  ', 'm s-1    ', 'u* for cropland                            '),
     & diag_type( 'Cropland_kvh    ', 'm-1      ', 'cropland vert to horiz flux ratio          '),
     & diag_type( 'Cropland_Rough  ', '         ', 'cropland surface roughness factor          '),
     & diag_type( 'Soil_Moist_Fac  ', '         ', 'soil moisture factor for threshold u*      '),
     & diag_type( 'Soil_Erode_Pot  ', '1        ', 'soil -> dust erodiblity potential          '),
     & diag_type( 'Mx_Adsrb_H2O_Frc', '1        ', 'max adsorbed water fraction                '),
     & diag_type( 'Vegetation_Frac ', '1        ', 'vegetation land coverage                   '),
     & diag_type( 'Urban_Cover     ', 'percent  ', 'urban land coverage                        '),
     & diag_type( 'Forest_Cover    ', 'percent  ', 'forest land coverage                       '),
     & diag_type( 'Trfac_Above_Can ', '         ', 'transport factor above canopy              '),
     & diag_type( 'Trfac_Inside_Can', '         ', 'transport factor in and below canopy       '),
     & diag_type( 'ANUMJ           ', 's-1', 'accumulation mode number                   '),
     & diag_type( 'ANUMK           ', 's-1', 'coarse mode number                         '),
     & diag_type( 'ASRFJ           ', 'm2 s-1   ', 'accumulation mode surface area             '),
     & diag_type( 'ASRFK           ', 'm2 s-1   ', 'coarse mode surface area                   ')/)
  
C Module shared variables:
      real, allocatable, save :: agland( :,: )   ! agriculture land fraction
      real, allocatable, save :: wmax  ( :,: )   ! max adsorb water percent
      real, allocatable, save :: kvh   ( :,: ) ! ratio of vertical flux / horizontal (k factor)
      real, allocatable, save :: sd_ep ( :,: )   ! soil->dust erodiblity potential
      real, allocatable, save :: tfb   ( :,: )   ! transport fraction in and below canopy

      CONTAINS

C=======================================================================
         function dust_emis_init( jdate, jtime, tstep ) result( success )

C Revision History.
C   Aug 12, 15 D. Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O
C   implementation

         use hgrd_defn        ! horizontal domain specifications
         use aero_data        ! aerosol species definitions
         use asx_data_mod     ! meteorology data
         use utilio_defn
         use lus_data_module
         use centralized_io_module

C Arguments:
         integer, intent( in ) :: jdate   ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime   ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep   ! output time step
         logical      success

C Includes:
         include SUBST_FILES_ID  ! file name parameters

C Local variables:
         character( 16 ) :: pname = 'DUST_EMIS_INIT'
         character( 16 ) :: vname
         character( 80 ) :: vardesc
         character( 250 ) :: xmsg = ' '

         integer status
         integer c, r, i, j, k, l, n, im
         integer idiag, idust, spc
         integer n_mass_emissions

         character( 16 ) :: sn

         type( diag_type ), allocatable :: diagnm_swap( : )

         interface
            subroutine cropcal ( jdate, jtime, agland )
            integer, intent( in )  :: jdate, jtime
            real,    intent( out ) :: agland( :,: )
            end subroutine cropcal
            subroutine tfbelow ( jdate, jtime, tfb )
            integer, intent( in )  :: jdate, jtime
            real,    intent( out ) :: tfb( :,: )
            end subroutine tfbelow
         end interface

C-----------------------------------------------------------------------

         success = .true.

         CALL LOG_MESSAGE( LOGDEV, 'Initialize Wind-Blown Dust Emissions' )

C...Populate Master Emissions Map Vector So That Diagnostics 
C   can be printed in EMIS_MAP
         Em_File_Surr( IDUSTSRM )%len = ndust_spc*2
         Allocate( Em_File_Surr( IDUSTSRM )%arry( ndust_spc*2 ) )
         Allocate( Em_File_Surr( IDUSTSRM )%units( ndust_spc*2 ) )
         Allocate( Em_File_Surr( IDUSTSRM )%mw   ( ndust_spc*2 ) )
         Allocate( Em_File_Surr( IDUSTSRM )%used ( ndust_spc*2 ) )
         Allocate( Em_File_Surr( IDUSTSRM )%conv ( ndust_spc*2 ) )
         Allocate( Em_File_Surr( IDUSTSRM )%basis( ndust_spc*2 ) )
         Em_File_Surr( IDUSTSRM )%arry( : ) = 'NOT_AVAILABLE'
         do i = 1,ndust_spc
            IF ( dust_spc( i )%spcfac(1) .NE. 0.0 ) 
     &         Em_File_Surr( IDUSTSRM )%arry( i )  = 
     &                    'PMFINE_' // dust_spc(i)%name
            IF ( dust_spc( i )%spcfac(2) .NE. 0.0 ) 
     &         Em_File_Surr( IDUSTSRM )%arry( i+ndust_spc )  = 
     &                    'PMCOARSE_' // dust_spc(i)%name
            Em_File_Surr( IDUSTSRM )%mw( i )  = dust_spc(i)%mw
            Em_File_Surr( IDUSTSRM )%mw( i+ndust_spc )  = dust_spc(i)%mw
         end do
         Em_File_Surr( IDUSTSRM )%units( : ) = 'G/S'
         Em_File_Surr( IDUSTSRM )%used ( : ) = .FALSE.
         Em_File_Surr( IDUSTSRM )%conv ( : ) = 1.0
         Em_File_Surr( IDUSTSRM )%basis( : ) = 'MASS'
 
C...Count the number of mass emissions species
        n_mass_emissions = 0
        do i = 1, ndust_spc
           do j = 1, 2
              if( dust_spc( i )%spcfac( j )  .gt. 0. ) 
     &               n_mass_emissions = n_mass_emissions + 1  
           end do
        end do                     
        allocate ( dustoutm( ndust_spc*2,ncols,nrows ),
     &              dustoutn( 2,ncols,nrows ),
     &              dustouts( 2,ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating DUSTOUTM, DUSTOUTN, or DUSTOUTS'
            call m3warn ( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

C Allocate emissions array
         allocate( dust_em( ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating DUST_EM'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

C Allocate private arrays
         allocate( wmax  ( ncols,nrows ),
     &             tfb   ( ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating WMAX, or TFB'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         wmax   = 0.0   ! array assignment

C Initialize land use/cover variables
         if ( .not. lus_init( jdate, jtime ) ) then
            xmsg = 'Failure initializing land use module'
            call m3exit( pname, jdate, jtime, xmsg, xstat2 )
         end if

         if ( dustem_diag ) then    ! Open the emissions diagnostic file

C Set up variable diagnostic names (from LUS_DEFN)
            allocate( vdiagnm_emis ( n_dlcat ),
     &                vdiagnm_frac ( n_dlcat ),
     &                vdiagnm_kvh  ( n_dlcat ),
     &                vdiagnm_rough( n_dlcat ),
     &                vdiagnm_ustar( n_dlcat ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating VDIAGNM_*'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            vdiagnm_emis  = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_frac  = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_ustar = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_kvh   = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_rough = diag_type( ' ', ' ', ' ' )  ! array assignment

C...Set Up Diagnostic Species Variables
            ndust_diag = fndust_diag + 5 * n_dlcat + n_mass_emissions 

            do i = 1, n_dlcat
               truncnm = vnmld( i )%desc   ! char( 10 )
C...           replace embedded spaces (within 16 chars) with "_"
C...           replace embedded dashes (within 16 chars) with "_"
               l = len_trim( truncnm )
               do k = 1, l
                  if ( truncnm( k:k ) .eq. " " .or.
     &                 truncnm( k:k ) .eq. "-" ) truncnm( k:k ) = "_"
               end do
               vnm = trim( truncnm ) // '_Emis'    ! char( 16 )
               vdiagnm_emis( i ) = diag_type( vnm, 'g/m**2/s', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Frac'    ! char( 16 )
               vdiagnm_frac( i ) = diag_type( vnm, 'percent', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Ustr'    ! char( 16 )
               vdiagnm_ustar( i ) = diag_type( vnm, 'm/s', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Kvh'    ! char( 16 )
               vdiagnm_kvh( i ) = diag_type( vnm, '1/m', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Rough'    ! char( 16 )
               vdiagnm_rough( i ) = diag_type( vnm, '   ', vnmld( i )%desc )
            end do

C Allocate diagnostic emissions arrays
            allocate( diagnm( ndust_diag ),    ! diag_type
     &                diagv ( ndust_diag ),
     &                dustbf( ndust_diag,ncols,nrows ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating DIAGNM, DIAGV or DUSTBF'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

#ifdef verbose_wbdust
            allocate( sdiagv( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating SDIAGV'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
#endif

C Build the complete diagnostic name array                     n for MODIS NOAH
            do i = 1, n_dlcat                                ! 4
               diagnm( i ) = vdiagnm_emis( i )
            end do
            n = n_dlcat + 1
            diagnm( n ) = fdiagnm( 1 )    ! Cropland_Emis
            n = n + 1
            diagnm( n ) = fdiagnm( 2 )    ! Desertland_Emis

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_frac( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 3 )    ! Cropland_Frac
            n = n + 1
            diagnm( n ) = fdiagnm( 4 )    ! Desertland_Frac

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_ustar( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 5 )    ! Cropland_Ustar

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_kvh( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 6 )    ! Cropland_Kvh

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_rough( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 7 )    ! Cropland_Rough

            n = n - 7                     ! add remaining variables in fdiagnm
            do i = 8, fndust_diag 
               idiag = i+n
               diagnm( idiag ) = fdiagnm( i )
            end do

C...append diagnostic variables with mass emissions species
            do j = 1, 2
               do i = 1, ndust_spc
                  if( dust_spc( i )%spcfac( j ) .eq. 0. ) cycle 
                    n = 0
                    do k = 1, idiag   ! determine if dust emissions is already added to diagnostic output
                       if( trim( EM_FILE_SURR( IDUSTSRM )%ARRY( (j-1)*ndust_spc+i )) 
     &                     .Eq. diagnm( k )%var ) Then
                         n = k
                         exit
                     end if
                  end do
                  if( n .gt. 0  ) cycle ! skip already added 
                  
                  idiag = idiag + 1
                  diagnm( idiag )%var = EM_FILE_SURR( IDUSTSRM )%ARRY( (j-1)*ndust_spc+i ) 
                  diagnm( idiag )%units = 'g/m**3/s'

                  Select Case( j ) ! assumes only two aerosol modes dust emissions
                     Case( 1 )
                        diagnm( idiag )%desc = 'fine mode' 
                     Case( 2 )
                        diagnm( idiag )%desc = 'coarse mode' 
                  end Select
                  diagnm( idiag )%desc = Trim( diagnm( idiag )%desc )
     &                                   // ' emissions for '  
     &                                   // Trim( dust_spc( i )%description )
               end do
            end do
            
! remove unused space in diagnm by deallocated and reallocating to idiag value
            allocate( diagnm_swap( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating DIAGNM_SWAP'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            diagnm_swap = diagnm 

            deallocate( diagnm )

            ndust_diag = idiag
            allocate( diagnm( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure reallocating DIAGNM'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            diagnm( 1:ndust_diag ) = diagnm_swap( 1:ndust_diag )
            deallocate( diagnm_swap )

            if ( io_pe_inclusive )
     &         call opdust_emis ( stdate, sttime, tstep, ndust_diag, diagnm )

         end if   ! dustem_diag

C Get transport factor within canopy and 4 land use type percents
         call tfbelow ( jdate, jtime, tfb )

         l2sgj = log( sigj ) * log( sigj )
         l2sgk = log( sigk ) * log( sigk )

C Factors for converting 3rd moment emission rates into number and 2nd moment
C emission rates.  (Diameters in [um] changed to [m] ) See Equations 7b and 7c
C of Binkowski & Roselle (2003)
         factnumj = 1.0e18 * exp( 4.5 * l2sgj ) / dgvj ** 3
         factnumk = 1.0e18 * exp( 4.5 * l2sgk ) / dgvk ** 3
         factm2j  = 1.0e06 * exp( 0.5 * l2sgj ) / dgvj
         factm2k  = 1.0e06 * exp( 0.5 * l2sgk ) / dgvk
         factsrfj = pi * factm2j
         factsrfk = pi * factm2k

#ifdef verbose_wbdust
         write( logdev,* ) ' '
         write( logdev,* ) '    l2sgj,l2sgk:         ', l2sgj, l2sgk
         write( logdev,* ) '    factnumj,factnumk:   ', factnumj, factnumk
         write( logdev,* ) '    factm2j,factm2k:     ', factm2j, factm2k
         write( logdev,* ) '    factsrfj,factsrfk:   ', factsrfj, factsrfk
         write( logdev,* ) ' '
#endif

         end function dust_emis_init

C=======================================================================
         subroutine opdust_emis ( jdate, jtime, tstep, ndust_var, dust_var )

C   27 Dec 10 J.Young: initial

         use grid_conf           ! horizontal & vertical domain specifications
         use utilio_defn

         implicit none

         include SUBST_FILES_ID  ! file name parameters

C Arguments:
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep      ! output time step
         integer, intent( in ) :: ndust_var
         type( diag_type ), intent( in ) :: dust_var( : )

C Local variables:
         character( 16 ) :: pname = 'OPDUST_EMIS'
         character( 96 ) :: xmsg = ' '

         integer      v, l       ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update
         if ( .not. open3( ctm_dust_emis_1, fsrdwr3, pname ) ) then
            xmsg = 'Could not open CTM_DUST_EMIS_1 for update - '
     &           // 'try to open new'
            call m3mesg( xmsg )

C Set output file characteristics based on COORD.EXT and open diagnostic file
            ftype3d = grdded3
            sdate3d = jdate
            stime3d = jtime
            tstep3d = tstep
            call nextime( sdate3d, stime3d, tstep3d ) !  start the next hour

            nvars3d = ndust_var
            ncols3d = gl_ncols
            nrows3d = gl_nrows
            nlays3d = 1
            nthik3d = 1
            gdtyp3d = gdtyp_gd
            p_alp3d = p_alp_gd
            p_bet3d = p_bet_gd
            p_gam3d = p_gam_gd
            xorig3d = xorig_gd
            yorig3d = yorig_gd
            xcent3d = xcent_gd
            ycent3d = ycent_gd
            xcell3d = xcell_gd
            ycell3d = ycell_gd
            vgtyp3d = vgtyp_gd
            vgtop3d = vgtop_gd
!           vgtpun3d = vgtpun_gd ! currently, not defined
            do l = 1, nlays3d + 1
               vglvs3d( l ) = vglvs_gd( l )
            end do
            gdnam3d = grid_name  ! from HGRD_DEFN

            do v = 1, nvars3d
               vtype3d( v ) = m3real
               vname3d( v ) = dust_var( v )%var
               units3d( v ) = dust_var( v )%units
               vdesc3d( v ) = dust_var( v )%desc
            end do

            fdesc3d( 1 ) = 'windblown dust parameters, variables, and'
            fdesc3d( 2 ) = 'hourly layer-1 windblown dust emission rates'
            do l = 3, mxdesc3
               fdesc3d( l ) = ' '
            end do

C Open windblown dust emissions diagnostic file
            if ( .not. open3( ctm_dust_emis_1, fsnew3, pname ) ) then
               xmsg = 'Could not create the CTM_DUST_EMIS_1 file'
               call m3exit( pname, sdate3d, stime3d, xmsg, xstat1 )
            end if

         end if

         return

         end subroutine opdust_emis

C=======================================================================
         subroutine get_dust_emis( jdate, jtime, tstep, rjacm, cellhgt )

         use grid_conf        ! horizontal & vertical domain specifications
         use asx_data_mod     ! meteorology data
         use aero_data
         use utilio_defn
         use lus_data_module

C Arguments:
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep( 3 ) ! output time step, sync step, 2way step
         real,    intent( in ) :: rjacm( ncols,nrows ) ! reciprocal Jacobian [1/m]
         real,    intent( in ) :: cellhgt    ! grid-cell height [sigma]

C Includes:
         include SUBST_FILES_ID  ! file name parameters

C External Functions:
               
C Parameters:
         integer, parameter :: ndp = 4     ! number of soil texture type particle sizes:
                                           !  1  Coarse sand
                                           !  2  Fine-medium sand
                                           !  3  Silt
                                           !  4  Clay

         real, parameter :: f6dpi = 6.0 / pi

         real, parameter :: mv    = 0.16
         real, parameter :: sigv  = 1.45
         real, parameter :: betav = 202.0
         real, parameter :: sigv_mv  = sigv * mv   ! = 0.232
         real, parameter :: betav_mv = betav * mv  ! = 32.32
         real, parameter :: mb    = 0.5
         real, parameter :: sigb  = 1.0
         real, parameter :: betab = 90.0
         real, parameter :: sigb_mb  = sigb * mb   ! = 0.5
         real, parameter :: betab_mb = betab * mb  ! = 45.0

         character( 16 ) :: pname = 'GET_DUST_EMIS'
         character( 16 ) :: vname
         character( 96 ) :: xmsg
         integer status
         integer c, r, j, m, n, v, isd

         integer, save   :: wstep = 0      ! local write counter
         integer         :: mdate, mtime   ! diagnostic file write date&time

                       ! automatic arrays
         real       :: fmoit  ( ncols,nrows )      ! factor of soil moisture on u*t
         real       :: soimt  ( ncols,nrows )      ! gravimetric soil moisture (Kg/Kg)
         real       :: tfa    ( ncols,nrows )      ! transport fraction above canopy
         real       :: wrbuf  ( ncols,nrows )      ! diagnositc write buffer
         real       :: dragp  ( ncols,nrows )      ! drag partition - albedo based
         real       :: clayf  ( ncols,nrows )      ! fractional clay content (0 -> 1)
         real       :: sandf  ( ncols,nrows )      ! fractional sand content (0 -> 1) 
         real       :: ssm    ( ncols,nrows )      ! Erodibility potential from Baker-Schepanski (2020)
         real       :: soilm1 ( ncols,nrows )

         real, allocatable, save :: qam   ( :,: ) ! emis for landuse type [g/m**2/s]

         real       :: edust( 2 )       ! mass emis rate [g/s] per mode (only accum & coarse)
         real       :: rlay1hgt ! reciprocal of layer-1 height [1/m]
         real       :: m3j              ! 3rd moment accumulation (J) mode emis rates [m3/m3/s]
         real       :: m3k              ! 3rd moment coarse mode (K) emis rates [m3/m3/s]
         
         character( 16 ), save :: rc_name, rn_name    ! new names: RC -> RCA, RN -> RNA
         logical, save :: firstime = .true.

         real       :: hflux, vflux ! horizontal and vertical dust flux
         real       :: jday
         integer    :: emap( n_dlcat+1 )

         interface
            subroutine tfabove ( tfa )
               real,    intent( out ) :: tfa( :,: )
            end subroutine tfabove
         end interface

#ifdef verbose_wbdust
         integer dryhit
         integer dusthit
#endif

C-----------------------------------------------------------------------

         if ( firstime ) then
            firstime = .false.
C            ! allocate ( ustr( ncols,nrows,n_dlcat+1 ),
C     ! &                  qam( ncols,nrows,n_dlcat+1 ),
C     ! &                 fruf( ncols,nrows,n_dlcat+1 ),
C     ! &                  kvh( ncols,nrows,n_dlcat+1 ),
C     ! &                 elus( nco
            allocate ( kvh( ncols,nrows ), qam( ncols,nrows ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating USTR, QAM, FRUF, KVH, or ELUS'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if
         end if

C---Calculate transport factor above the canopy
         call tfabove ( tfa )

C---Get Julian day number in year
         jday = float( mod( jdate,1000 ) )

#ifdef verbose_wbdust
         dryhit = 0
         dusthit = 0
#endif

C Initialize windblown dust diagnostics output buffer
         if ( dustem_diag .and. wstep .eq. 0 ) then
            dustbf = 0.0   ! array assignment
#ifdef verbose_wbdust
            sdiagv = 0.0   ! array assignment
#endif
         end if

C set erodible landuse map
C         ! do m = 1, n_dlcat
C         !    emap( m ) = dmap( m )  ! dmap maps to one of the 3 BELD3 desert types
C         ! end do
C         ! emap( n_dlcat+1 ) = 4

C --------- ###### Start Main Loop ###### ---------
 
         do r = 1, nrows
            do c = 1, ncols
               dust_em( c,r ) = 0.0
               soimt( c,r )   = 0.0
               fmoit( c,r )   = 0.0   ! for diagnostic output visualization
               qam ( c,r ) = 0.0
               kvh ( c,r ) = 0.0
                              
               rlay1hgt = rjacm ( c,r ) / cellhgt
               
               !---Dust possiblity only if 1. not over water 
               !                           2. rain < 1/100 in. (1 in. = 2.540 cm)
               !                           3. not snow-covered
               !                           4. if soimt <= limit
               !                           5. Baker-Schepanski Map > 0
               !                           7. friction velocity > threshold
               
               if ( ( Grid_Data%lwmask( c,r ) .gt. 0.0 ) .and.
     &           ( Met_Data%rn( c,r ) + Met_Data%rc( c,r ) .le. 0.0254 ) .and. ! rn, rc = [cm]
     &           ( Met_Data%snocov( c,r ) .lt. 0.001 ) .and. ! less than 0.1% snow coverage
     &           ( Met_Data%ssm( c,r ) .gt. 0.0 ) .and.
     &           ( Met_Data%clayf(c,r) .gt. 0.0 ) .and.
     &           ( Met_Data%drag(c,r) .gt. 0.0) ) then ! ssm > 0.0
                  !---Dust possiblity 1,2,3,5
                  
                  
#ifdef verbose_wbdust
                  dryhit = dryhit + 1
#endif

C Calculate maximum amount of the water absorbed
C    w` = 0.0014(%clay)**2 + 0.17(%clay) - w` in %
C     Fecan et al. [1999,Annales Geophys.,17,144-157]
!              wmax  ( c,r ) = Met_Data%clayf( c,r ) * Met_Data%clayf( c,r )* .0014d0 + 0.17d0 * Met_Data%clayf( c,r )
              wmax  ( c,r ) = (100.*Met_Data%clayf( c,r )) * 
     &                        (100.*Met_Data%clayf( c,r )) * 
     &                        .0014d0 + 0.17d0 * (100.*Met_Data%clayf( c,r ))
              soimt(  c,r ) = dust_volumetric_to_gravimetric( Met_Data%soim1( c,r ), Met_Data%clayf( c,r ), Met_Data%sandf( c,r )) 
C---Soil moisture effect on U*t
                  if ( soimt( c,r ) .le. 0.01 * wmax( c,r ) ) then   ! wmax in [%]
                     fmoit( c,r ) = 1.0
                  else
                     fmoit( c,r ) = sqrt( 1.0 + 1.21
     &                            * ( 100.0 * soimt( c,r ) - wmax( c,r ) ) ** 0.68 )
                  end if
 
C Calculate Vertical to Horizontal Mass Flux Ratio
C -- This is based on MB95 
C             kvh = 10 ** ( 0.134 * (%clay) - 6) 
                  if ( Met_Data%clayf(c,r) < 0.2) then
                        kvh(c,r) = 4.0e-4
                  else
                        kvh( c,r ) = 10. ** (0.134 * (Met_Data%clayf( c,r )*100.) - 6.0) 
                  endif 
C Horizontal Flux
                  hflux = dust_hflux( Met_Data%ustar( c,r),
     &                                fmoit( c,r ),
     &                                Met_Data%drag( c,r ),
     &                                Met_Data%uthr( c,r ),
     &                                Met_Data%ssm( c,r ),
     &                                Met_Data%dens1( c,r ) )
                        vflux = hflux * kvh( c,r )                    ! [g/m**2/s]
                        qam( c,r ) = qam( c,r ) + vflux * rlay1hgt * 0.01    ! [g/m**3/s]
                  if (qam(c,r) < 0) then
                        write(*,*) 'Clay SSM drag ut fmoit c r'
                        write(*,*) Met_Data%clayf(c,r),Met_Data%ssm(c,r),Met_Data%drag(c,r), Met_Data%uthr(c,r), fmoit(c,r), c,r
                  end if
                     dust_em( c,r ) = dust_em( c,r ) + qam( c,r )

 
C Dust removal by surrounding vegetation <-???
C Adjust dust emissions for transport factors

                  dust_em( c,r ) = dust_em( c,r ) * tfa( c,r ) * tfb( c,r )

            end if  ! if rain & land & snow

         end do   ! c
         end do   ! r

C --------- ###### End Main Loop ##### ---------

#ifdef verbose_wbdust
         write( logdev,'( /5x, a, 1x, 2i8 )' ) 'dry hit count, 
     &                                          out of total cells:',
     &                                          dryhit, (c-1)*(r-1)
#endif

         do r = 1, nrows
         do c = 1, ncols

C J/K mass emis rate [g/s]  (edust( 1 ) not used)
            edust( 1 ) = fracmj * dust_em( c,r )
            edust( 2 ) = fracmk * dust_em( c,r )

            do n = 1,2
               do v = 1, ndust_spc
                  dustoutm( (n-1)*ndust_spc+v,c,r ) = 
     &                     edust( n ) * dust_spc( v )%spcfac( n )
               end do
            end do

C J/K 3rd moment emis rate [m3/s] (needed for number and surface)
            m3j = edust( 1 ) * f6dpi / ( gpkg * dust_dens( 1 ) ) + TINY(0.0)
            m3k = edust( 2 ) * f6dpi / ( gpkg * dust_dens( 2 ) ) + TINY(0.0)

C Mode-specific emission rates of particle number [1/s]
            dustoutn( 1,c,r ) = m3j * factnumj
            dustoutn( 2,c,r ) = m3k * factnumk

C Mode-specific dry surface area emission rates [m**2/s].
C 2nd moment multiplied by PI to obtain the surface area emissions rate.
            dustouts( 1,c,r ) = m3j * factsrfj
            dustouts( 2,c,r ) = m3k * factsrfk

! Propagate Number and Surface Area Scaling Factors back to Emissions
! Module so that the dust emissions can be scaled appropriately            
            ISD = INDEX1( 'FINE', EM_STREAM_SIZE( IDUSTSRM )%LEN, 
     &                    EM_STREAM_SIZE( IDUSTSRM )%NAME )
            EM_STREAM_SIZE( IDUSTSRM )%FACNUM( ISD,2 ) = FACTNUMJ
            EM_STREAM_SIZE( IDUSTSRM )%FACSRF( ISD,2 ) = FACTSRFJ
            
            ISD = INDEX1( 'COARSE', EM_STREAM_SIZE( IDUSTSRM )%LEN, 
     &                    EM_STREAM_SIZE( IDUSTSRM )%NAME )
            EM_STREAM_SIZE( IDUSTSRM )%FACNUM( ISD,3 ) = FACTNUMK
            EM_STREAM_SIZE( IDUSTSRM )%FACSRF( ISD,3 ) = FACTSRFK

#ifdef verbose_wbdust
            if ( m3j .ne. 0.0 ) dusthit = dusthit + 1
#endif

            if ( dustem_diag ) then
               do m = 1, n_dlcat+1
                  diagv( m ) = qam( c,r )             ! g/m**3/s
               end do
               n = n_dlcat + 2
               diagv( n ) = dust_em( c,r )              ! g/m**3/s

C               sumdfr = 0.0
C               do m = 1, n_dlcat+1
C                  diagv( m+n ) = elus( c,r )
C                  sumdfr = sumdfr + elus( c,r )
C               end do
C               n = n + n_dlcat + 2
C               diagv( n ) = sumdfr

C               do m = 1, n_dlcat+1
C                  diagv( m+n ) = Met_Data%USTAR( c,r )
C               end do
C               n = n + n_dlcat + 1

               do m = 1, n_dlcat+1
                  diagv( m+n ) = kvh( c,r )
               end do
               n = n + n_dlcat + 1

C               do m = 1, n_dlcat+1
C                  diagv( m+n ) = fruf( c,r )
C               end do
C               n = n + n_dlcat + 1
               diagv( n+1  ) = fmoit( c,r )              ! 'Soil_Moist_Fac  '
               diagv( n+2  ) = Met_Data%SSM( c,r )       ! 'Soil_Erode_Pot  '
               diagv( n+3  ) = wmax ( c,r )              ! 'Mx_Adsrb_H2O_Frc'
               diagv( n+4  ) = Met_Data%CLAYF( c,r )     ! 'Vegetation_Frac '
               diagv( n+5  ) = uland( c,r,3 )            ! 'Urban_Cover     '
               diagv( n+6  ) = uland( c,r,4 )            ! 'Forest_Cover    '
               diagv( n+7  ) = tfa  ( c,r )              ! 'Trfac_Above_Can '
               diagv( n+8 )  = tfb  ( c,r )              ! 'Trfac_Inside_Can'
               n = n + 8

! accum and coarse mode number density emissions
               diagv( n+1 ) = dustoutn( 1,c,r )
               diagv( n+2 ) = dustoutn( 2,c,r )
! accum and coarse mode surface area density emissions
               diagv( n+3 ) = dustouts( 1,c,r )
               diagv( n+4 ) = dustouts( 2,c,r )
              
               n = n + 4
               m = 0
               do v = 1, ndust_spc
                  if ( dust_spc( v )%spcfac( 1 ) .gt. 0. ) then  ! accum. mode mass emissions
                     m = m + 1
                     diagv( m+n ) = dustoutm( v,c,r )
                  end if
               end do

               do v = 1, ndust_spc
                  if ( dust_spc( v )%spcfac( 2 ) .gt. 0. ) then  ! coarse mode mass emissions
                     m = m + 1
                     diagv( m+n ) = dustoutm( v+ndust_spc,c,r )
                  end if
               end do

               n = n + m

C Multiply by sync step because when write to output we divide by the output step
C to get a timestep average.
               do v = 1, ndust_diag
                  dustbf( v,c,r ) = dustbf( v,c,r ) + diagv( v )
     &                            * float( time2sec( tstep( 2 ) ) )
#ifdef verbose_wbdust
                  sdiagv( v ) = sdiagv( v ) + diagv( v )
     &                                      * float( time2sec( tstep( 2 ) ) )
#endif
               end do
            end if   ! dustem_diag
         end do   ! col
         end do   ! row

#ifdef verbose_wbdust
         write( logdev,'( 5x, a, 2i8 / )' ) 'dust hit count, out of total cells:',
     &                                       dusthit, (c-1)*(r-1)
#endif

         if ( dustem_diag ) then

C If last call this hour, write out the windblown dust emissions dignostics.
C Then reset the emissions array and local write counter.

            wstep = wstep + time2sec( tstep( 2 ) )

            if ( wstep .ge. time2sec( tstep( 1 ) ) ) then
               if ( .not. currstep( jdate, jtime, stdate, sttime, tstep( 1 ),
     &                              mdate, mtime ) ) then
                  xmsg = 'Cannot get step date and time'
                  call m3exit( pname, jdate, jtime, xmsg, xstat3 )
               end if
               call nextime( mdate, mtime, tstep( 1 ) )

#ifdef verbose_wbdust
               sdiagv = sdiagv / float( wstep )   ! array assignment
               write( logdev,2015 ) jdate, jtime
               do v = 1, ndust_diag
                  if ( diagnm( v )%var(1:4) .ne. 'ANUM' ) then
                     write( logdev,2019 ) v, diagnm( v )%var, sdiagv( v )
                  else
                     write( logdev,2023 ) v, diagnm( v )%var, sdiagv( v )
                  end if
               end do
               sdiagv = 0.0   ! array assignment
#endif
               do v = 1, ndust_diag
                  do r = 1, nrows
                     do c = 1, ncols
                        wrbuf( c,r ) = dustbf( v,c,r ) / float( wstep )
                     end do
                  end do

                  if ( .not. WRITE3( ctm_dust_emis_1, diagnm( v )%var,
     &                       mdate, mtime, wrbuf ) ) then
                     xmsg = 'Could not write ' // trim( diagnm( v )%var )
     &                    // ' to CTM_DUST_EMIS_1'
                     call m3exit( pname, mdate, mtime, xmsg, xstat1 )
                  end if
               end do
               write( logdev,'( /5x, 2( a, 1x ), i8, ":", i6.6 )' )
     &               'Timestep written to CTM_DUST_EMIS_1',
     &               'for date and time', mdate, mtime
               wstep = 0
               dustbf = 0.0   ! array assignment
            end if   !  time to write
         end if   ! dustem_diag

2009     Format( '*** Erodible landuse incorrect ', 1pe13.5, 1x, 'at: ', 3i4 )
2015     format( /5x, 'Total grid time-avg sum of dust emis variables at:',
     &            1x, i8, ":", I6.6  )
2019     format( i10, 1x, a, f20.5 )
2023     format( i10, 1x, a, e20.3 )

         end subroutine get_dust_emis

C=======================================================================
         function dust_hflux( ustar, fmoit, drag, uthr, ssm, dens )
     &      result( hflux )

C                         hflux = dust_hflux( Met_Data%ustar( c,r),
C     &                                      fmoit( c,r ),
C     &                                      drag( c,r ),
C     &                                      uthr( c,r ),
C     &                                      ssm( c,r ),
C     &                                      Met_Data%dens1( c,r ) )

            implicit none

            include SUBST_CONST   ! for grav

            real,    intent( in ) :: ustar, fmoit, drag, uthr, ssm, dens
            real hflux

            real, parameter :: amen = 1.0          ! Marticorena and Bergametti [JGR,1997]
            real, parameter :: cfac = 1000.0 * amen / grav
            real, parameter :: A = 260.60061       ! 0.0123 * 2650.0 * 9.81 / 1.227
            real, parameter :: B = 1.6540342e-06   ! 0.0123 * 0.000165 / 1.227
            real utstar                            ! threshold U* [m/s]
            real utem                              ! U term [(m/s)**3]
            real fac
            integer n

! I can't initialize dp this way - it has to be passed in since ndp is variable

            fac = cfac * dens 
            utem    = 0.0
            utstar  = 0.0
            hflux   = 0.0
            utstar = fmoit * uthr / drag 
            if ( ustar .gt. utstar ) then  ! wind erosion occurs only if U* > U*t
                  utem = ustar ** 3 * ( 1.0 - (utstar / ustar) ** 2)
C---Horiz. Flux from Owen (1964)
!                 utem = ustr * ( ustr * ustr - utstar * utstar )
                  hflux = hflux + fac * ssm * utem !  [g/m/s]
            end if
         end function dust_hflux
C==============================================================================
        function dust_volumetric_to_gravimetric(vsoilm,clay,sand)
     &       result ( gwc )
C usage: H = dust_volumetric_to_gravimetric(vsoilm(c,r),
C                           clay(c,r),
C                           sand(c,r))

            implicit none
            ! INPUTS
            real, intent(in) :: vsoilm ! volumetric soil moisture
            real, intent(in) :: clay   ! clay fraction (0 -> 1)
            real, intent(in) :: sand   ! sand fraction (0 -> 1)
            ! OUTPUTS
            real :: H
            ! LOCAL
            real :: gwc               ! gravimetric soil moisture
            real :: bulk_dens_dry     ! bulk density
            real :: limit             ! fecan soil moisture limit
            real :: wsat              ! saturated volumentric water content
            real :: mpot              ! saturated soil matric potential

            ! parameters
            real*8, parameter   :: bulk_dens = 2650.0d0
            real*8, parameter   :: h20_dens = 1000.0d0

            ! saturated soil matric potential [ mm H2O ]
            mpot = 10.d0 * (10.0d0 ** (1.88d0 - 0.0131d0 * sand  ))

            ! saturated volumentric water content [ m3 m-3 ]
            wsat = 0.489d0 - 0.00126d0 * sand

            ! Bulk density of dry surface soil  [kg m-3]
            bulk_dens_dry = bulk_dens * ( 1.0d0 - wsat)

            ! Gravimetric water content [ kg kg-1]
            gwc = VSOILM * h20_dens / bulk_dens_dry
            if (gwc.ge.1.0e10) then
                gwc = 0.d0
            endif

          end function dust_volumetric_to_gravimetric
      end module dust_emis

