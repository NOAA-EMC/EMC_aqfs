      program testrdcn
C test se_global_max reduction function
      use mpim
      use se_modules
      implicit none
!     real, external :: se_global_max()
      integer npcol, nprow, gl_ncols, gl_nrows, nlays, nspcsd
      integer mndis, medis, msdis, mwdis, geo_ori
      character( 2 ) :: data_ori = 'cr'
      real val, max

      nprocs = 16; npcol = 5; nprow = 5
      gl_ncols = 148; gl_nrows = 112; nlays = 9; nspcsd = 99
      mndis = 2; medis = 2; msdis = 2; mwdis = 2; geo_ori = 0

      call init_mpi
      call se_init( nprocs, npcol, nprow, gl_ncols, gl_nrows, nlays,
     &              nspcsd, g_mype, mndis, medis, msdis, mwdis,
     &              data_ori, geo_ori )

      val = 10.0 * float( g_mype )

      max = se_global_max( val )

      print*, max

      call shut_down_mpi

      end
