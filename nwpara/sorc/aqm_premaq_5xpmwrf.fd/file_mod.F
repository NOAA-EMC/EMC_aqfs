MODULE file

!-------------------------------------------------------------------------------
! Name:     File
! Purpose:  Contains FORTRAN units and file names.
! Revised:  19 May 2003  Original version.  (T. Otte)
!           21 Oct 2003  Removed files GRIDBDY2D, GRIDBDY3D, and
!                        METBDY2D.  (T. Otte)
!           25 Nov 2003  Added flag to create static output (grid) files.
!                        (T. Otte)
!           02 Dec 2003  Added files for chemical BCs.  (T. Otte)
!           31 Mar 2004  Added flags to create emissions and time-varying
!                        chemical boundary conditions.  (T. Otte)
!           09 Apr 2004  Added file for getting land use array from
!                        external emissions processing.  (T. Otte)
!           12 Apr 2004  Corrected initialization of file names to be
!                        exactly CHARACTER*16 strings.  (T. Otte)
!-------------------------------------------------------------------------------

  INTEGER                      :: iutmm
  INTEGER,       PARAMETER     :: iutgd      =   4
  INTEGER,       PARAMETER     :: iutmmi     =  10
  INTEGER,       PARAMETER     :: iutnml     =   8

  INTEGER                      :: iuarr      ( 255 )
  INTEGER                      :: iunit 

  CHARACTER*80                 :: file_gd
  CHARACTER*80                 :: files_in   ( 49 )
  CHARACTER*80,  PARAMETER     :: file_nml   = 'namelist.premaq'
  LOGICAL                      :: makegrid
  LOGICAL                      :: makeemis
  LOGICAL                      :: makebc

  CHARACTER*16,  PARAMETER     :: gridcro2d  = 'GRID_CRO_2D     '
  CHARACTER*16,  PARAMETER     :: griddot2d  = 'GRID_DOT_2D     '
  CHARACTER*16,  PARAMETER     :: metcro2d   = 'MET_CRO_2D      '
  CHARACTER*16,  PARAMETER     :: metcro3d   = 'MET_CRO_3D      '
  CHARACTER*16,  PARAMETER     :: metdot3d   = 'MET_DOT_3D      '
  CHARACTER*16,  PARAMETER     :: bndygasc   = 'BNDY_GASC_1     '
  CHARACTER*16,  PARAMETER     :: landa      = 'LANDA_G1        '

  CHARACTER*16,  PARAMETER     :: gridcro2d_g1 = 'GRID_CRO_2D_G1  '
  CHARACTER*16,  PARAMETER     :: gridcro3d_g1 = 'GRID_CRO_3D_G1  '
  CHARACTER*16,  PARAMETER     :: griddot2d_g1 = 'GRID_DOT_2D_G1  '
  CHARACTER*16,  PARAMETER     :: metbdy3d_g1  = 'MET_BDY_3D_G1   '
  CHARACTER*16,  PARAMETER     :: metcro2d_g1  = 'MET_CRO_2D_G1   '
  CHARACTER*16,  PARAMETER     :: metcro3d_g1  = 'MET_CRO_3D_G1   '
  CHARACTER*16,  PARAMETER     :: metdot3d_g1  = 'MET_DOT_3D_G1   '
  CHARACTER*16,  PARAMETER     :: chemcro3d_g1 = 'CHEM_CRO_3D_G1  '
  CHARACTER*16,  PARAMETER     :: chembdy3d_g1 = 'CHEM_BDY_3D_G1  '

END MODULE file
