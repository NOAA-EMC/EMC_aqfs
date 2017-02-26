SUBROUTINE emis_report

!-------------------------------------------------------------------------------
! Name:     Emissions Report
! Purpose:  Prints out information about emissions processing to log file.
! Revised:  08 Apr 2004  Original version.  (T. Otte and G. Pouliot)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE mod_mobile, only: neg_ct, total_ct, nspecies, nhours, specieslist

  IMPLICIT NONE

  INTEGER                      :: s_loop
  INTEGER                      :: t_loop

!-------------------------------------------------------------------------------
! Write emissions report to log file.
!-------------------------------------------------------------------------------

  WRITE (*,*) 'TOTAL COUNT FOR MOBILE = ', total_ct

  WRITE (*,*) 'NEGATIVE MOBILE EMISSION COUNT BY TIME & SPECIES'
  DO t_loop = 1, nhours
     DO s_loop = 1, nspecies
   

      IF ( SUM( neg_ct(1:ncols, 1:nrows,s_loop, t_loop) ) /= 0.0 ) THEN
        WRITE (*,*) specieslist(s_loop), t_loop,  &
                    SUM( neg_ct(1:ncols,1:nrows,s_loop,t_loop) )
      ENDIF

    ENDDO
  ENDDO

END SUBROUTINE emis_report
