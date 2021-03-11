	module data_module

        implicit none

        include 'PARMS3.EXT'

        type domain_info_rec
          integer :: scol
          integer :: ecol
          integer :: ncols
          integer :: gncols
          integer :: srow
          integer :: erow
          integer :: nrows
          integer :: gnrows
          integer :: slay
          integer :: elay
          integer :: nlays
          integer :: gnlays
          integer :: nspcs
          real*8  :: alp
          real*8  :: bet
          real*8  :: gam
          real*8  :: xcent
          real*8  :: ycent
          real*8  :: xorig
          real*8  :: yorig
          real*8  :: xcell
          real*8  :: ycell
          character (len =16) :: spcname(mxvars3)
        end type domain_info_rec

        real, allocatable :: idata(:,:,:), odata(:,:,:)

	end module data_module
