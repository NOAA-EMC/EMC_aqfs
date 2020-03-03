MODULE table

!-------------------------------------------------------------------------------
! Name:     Table Characteristics
! Purpose:  Contains variables that hold characteristics of input
!           meteorology fields.
! Notes:    Liberally adapted from NCAR's MM5/REGRID/pregrid/util/table.F.
! Revised:  31 Jul 1999  Original version in REGRID.  (NCAR)
!           10 Apr 2003  Modified for PREMAQ.  (T. Otte)
!           30 Mar 2004  Removed FIL since there is currently no way to
!                        verify nearest neighbor vs. bilinear interpolation
!                        of input fields.  Added TBL to represent the GRIB
!                        parameter table (octet 4 of PDS).  (T. Otte)
!           02 Feb 2005  Added new varilable "tr" to represent the GRIB
!                        parameter table (octet 21 of PDS, kpds(16)), 
!                        "0" : instaneous, "3" : average.        (Hsin-mu  Lin)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       PARAMETER     :: maxlines   = 100

  INTEGER                      :: gcode      ( maxlines )
  INTEGER                      :: lcode      ( maxlines )
  INTEGER                      :: l1         ( maxlines )
  INTEGER                      :: l2         ( maxlines )
  INTEGER                      :: maxvar     = 0    ! initialized here
  INTEGER                      :: tbl        ( maxlines )
  INTEGER                      :: tr         ( maxlines )
  INTEGER                      :: disc       ( maxlines )  

  INTEGER,       PARAMETER     :: blankcode  = -99
  INTEGER,       PARAMETER     :: splatcode  = -88

END MODULE table
