SUBROUTINE wrgdesc

!-------------------------------------------------------------------------------
! Name:     Write Grid Description
! Purpose:  Writes grid description (GRIDDESC) file.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  30 Jan 2002  Original version.  (T. Otte)
!           06 Jan 2003  Modified for PREMAQ.  (T. Otte)
!           26 Mar 2004  Added blank line at beginning of output file
!                        and changed scientific notation to standard
!                        notation.  (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE file
  USE premaqparm

  IMPLICIT NONE

  CHARACTER*1,   PARAMETER     :: blank      = ' '
  CHARACTER*1,   PARAMETER     :: quote      = CHAR(39)

!-------------------------------------------------------------------------------
! Write grid description in two sets.  Reader uses two list-directed READ
! statements to capture information.
!-------------------------------------------------------------------------------

  OPEN (iutgd, FILE=file_gd)

  WRITE (iutgd, 100) quote // blank // quote

  WRITE (iutgd, 100) quote // TRIM(coordnam_gd) // quote
  WRITE (iutgd, 200) gdtyp_gd, p_alp_gd, p_bet_gd, p_gam_gd, xcent_gd, ycent_gd
  WRITE (iutgd, 100) quote // blank // quote

  WRITE (iutgd, 100) quote // TRIM(gdname_gd) // quote
  WRITE (iutgd, 300) quote // TRIM(coordnam_gd) // quote, xorig_gd, yorig_gd,  &
                     xcell_gd, ycell_gd, ncols, nrows, nthik
  WRITE (iutgd, 100) quote // blank // quote

  CLOSE (iutgd)

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 100 FORMAT (a)
 200 FORMAT (i3, 5(1x, f13.3))
 300 FORMAT ( a, 4(1x, f13.3), 3(1x, i3))

END SUBROUTINE wrgdesc
