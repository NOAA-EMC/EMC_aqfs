SUBROUTINE prep_emis (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Prepare for Emissions Processing
! Purpose:  Sets up processing for meteorologically dependent emissions.
! Revised:  20 May 2003  Original version.  (T. Otte and G. Pouliot)
!           08 Apr 2004  Replaced call to PRE_MOBILE with a call to
!                        PRE_MOBILE6 to process Mobile 6 emissions. (G. Pouliot)
!           06 Mar 2008  ADD option to skip processing Mobile 6 emissions
!                        and directly used pre-processed value  (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime

  EXTERNAL       ENVYN

  LOGICAL        ENVYN, MBL_T
  INTEGER        ISTAT

!-------------------------------------------------------------------------------
! Set up for biogenic emissions.
!-------------------------------------------------------------------------------

  CALL pre_beis (sdate, stime)

!-------------------------------------------------------------------------------
! Set up for temporal for point-source emissions.
!-------------------------------------------------------------------------------
  CALL pre_temporal (sdate, stime)

!-------------------------------------------------------------------------------
! Set up for plume rise from point-source emissions.
!-------------------------------------------------------------------------------

  CALL pre_laypoint (sdate, stime)

!-------------------------------------------------------------------------------
! Set up for SMOKE merge.
!-------------------------------------------------------------------------------

  CALL pre_smkmerge (sdate, stime)

!-------------------------------------------------------------------------------
! Set up for mobile-source emissions.
!-------------------------------------------------------------------------------

  MBL_T = ENVYN ('MBL_T', 'MOBIL: Meteorology T dependent ?', .TRUE. , ISTAT)

  IF ( MBL_T ) then
     CALL pre_mobile6 (sdate, stime)
  ENDIF

!-------------------------------------------------------------------------------
! Set up to merge emissions into a single file.
!-------------------------------------------------------------------------------

  CALL pre_mrggrid (sdate, stime)

END SUBROUTINE prep_emis
