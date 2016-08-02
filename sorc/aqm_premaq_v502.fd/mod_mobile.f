      module mod_mobile
!
!****** for mobile emissions processing using nonlinear least squares fit to mobile6 runs
!       Date Last modified: June 3, 2004 (changed neg_ct to 4 byte integer)
!

      character(len=16) :: outfile           ! logical name of output file
      integer :: nspecies, pmspecies
      character(len=16), allocatable :: specieslist (: ) 

      character(len=16), allocatable :: pmspecieslist (: ) 
           
      real, allocatable              :: emis_evap  (:,:,:)
      real, allocatable              :: emis_exh   (:,:,:)
      real, allocatable              :: emis_total (:,:,:)
     
      real, allocatable              :: xm_evap( : , : , : )
      real, allocatable              ::  xm_exh( : , : , : )
      real, allocatable              :: xm_evapexh(: , :, :)
      real, allocatable, target      :: pm_emis( : , :, :  )
      real, allocatable              :: buffer(:,:)
      integer :: nm


      integer :: nhours
      integer :: nmonths
      integer, allocatable :: neg_ct(:,:,:,:)
      integer :: total_ct
      
      integer :: kdate      
      integer :: ktime

       integer :: index_no, index_no2, index_co
      
      character(len=16) :: infile_evap
      character(len=16) :: infile_exh
      character(len=16) :: infile_pm   
      character(len=16) :: infile
      
      logical :: is_2004version, is_2005version
      
      end module mod_mobile
