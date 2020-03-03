SUBROUTINE emissions (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Emissions Processing
! Purpose:  Processes meteorologically dependent emissions.
! Revised:  20 May 2003  Original version.  (T. Otte and G. Pouliot)
!           08 Apr 2004  Replaced call to NLSQR with a call to NLSQRM6 to
!                        process Mobile 6 emissions.  (G. Pouliot)
!           06 Mar 2008  ADD option to skip processing Mobile 6 emissions
!                        and directly used pre-processed value  (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  EXTERNAL       ENVYN

  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime

  LOGICAL        ENVYN, MBL_T
  INTEGER        ISTAT

!-------------------------------------------------------------------------------
! Process biogenic emissions.
!-------------------------------------------------------------------------------

!  CALL beis311 (sdate, stime)


!-------------------------------------------------------------------------------
! Process point source emissions.
!-------------------------------------------------------------------------------

  CALL temporal (sdate, stime)

!-------------------------------------------------------------------------------
! Process plume rise from point-source emissions.
!-------------------------------------------------------------------------------

  CALL laypoint (sdate, stime)

!-------------------------------------------------------------------------------
! Process SMOKE merge.
!-------------------------------------------------------------------------------

  CALL smkmerge (sdate, stime)

!-------------------------------------------------------------------------------
! Process mobile-source emissions.
!-------------------------------------------------------------------------------

  MBL_T = ENVYN ('MBL_T', 'MOBIL: Meteorology T dependent ?', .TRUE. , ISTAT)

  IF ( MBL_T ) THEN
     CALL nlsqrm6 (sdate, stime)
  ENDIF

!-------------------------------------------------------------------------------
! Merge emissions into a single file.
!-------------------------------------------------------------------------------

  CALL mrggrid (sdate, stime)

END SUBROUTINE emissions
