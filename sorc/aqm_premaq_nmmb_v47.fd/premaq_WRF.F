PROGRAM premaq

!-------------------------------------------------------------------------------
! Name:     Pre-Processor for the Multiscale Air Quality Model
! Purpose:  Generates a complete set of meteorological data and meteorologically
!           dependent emissions data from NCEP's Eta model as input for the
!           operational version of CMAQ.
! Notes:    Some emissions processing taken from SMOKE (Copyright 2002 MCNC).
! Revised:  20 May 2003  Original version.  (T. Otte, with emissions added by
!                        G. Pouliot)
!           09 Dec 2003  Added flag to create static output (grid) files.
!                        Added subroutine calls for processing time-dependent
!                        chemical boundary conditions.  (T. Otte)
!           30 Jan 2004  Added initialization of I/O API.  (T. Otte)
!           31 Mar 2004  Added flags to create emissions and time-varying
!                        chemical boundary conditions.  (T. Otte)
!           08 Apr 2004  Added call to EMIS_REPORT to print emissions report.
!                        (G. Pouliot and T. Otte)
!           29 Apr 2004  Updated dates in call to W3TAGB.  (T. Otte)
!           01 Jan 2005  Retrieved environment variable STORE_FILE which is used
!                        to determine whether to store data files: B3GTS_L and
!                        MGTS_L for analysis or not
!           14 JAN 2005  Modified call to DYNFLDS          (Hsin-mu Lin)
!           05 Jun 2006  Code is modified to consistent with new Premaq for
!                        coupling                          (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE premaqparm
  USE metinfo
  USE date_pack
  USE date_time
  USE parms3
  USE file
  USE iodecl3

  USE mod_rw

  IMPLICIT NONE

  CHARACTER*24                 :: aqf_next    ! YYYY-MO-DD-HH:MI:SS.SSSS
  CHARACTER*24                 :: aqf_now     ! YYYY-MO-DD-HH:MI:SS.SSSS
  REAL                         :: ctmlays     ( maxlays )
  LOGICAL                      :: first       = .TRUE.
  INTEGER                      :: funit
  REAL                         :: gmt         ! HH.MI (.MI = MI/60)
  INTEGER                      :: inmetlays   ( maxmetlays )
  INTEGER                      :: inmetlaysht ( maxmetlays )

  INTEGER :: STAT

  LOGICAL, EXTERNAL :: ENVYN

!-------------------------------------------------------------------------------
! Insert NCEP tag at start of PREMAQ run.
!-------------------------------------------------------------------------------

  CALL w3tagb ('PREMAQ', 2004, 0120, 0061, 'EPA')

!-------------------------------------------------------------------------------
! Initialize I/O API.
!-------------------------------------------------------------------------------

  funit = init3()

  store_file = envyn( 'STORE_FILE', 'Store selected ' //            &
                      'emission process temp files', .FALSE., stat)

!-------------------------------------------------------------------------------
! Initialize current date and time to missing values.
!-------------------------------------------------------------------------------

  sdate = imiss3
  stime = imiss3

!-------------------------------------------------------------------------------
! Read namelist to get user input.
!-------------------------------------------------------------------------------

  CALL readnml (ctmlays)

!-------------------------------------------------------------------------------
! Set up input meteorology.  Assume input data are in GRIB format from NCEP.
!-------------------------------------------------------------------------------

  CALL setup (inmetlays, inmetlaysht)

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  CALL alloc_met
  CALL alloc_x
  CALL alloc_ctm

!-------------------------------------------------------------------------------
! Initialize arrays.
!-------------------------------------------------------------------------------

  CALL init_met
  CALL init_x

!-------------------------------------------------------------------------------
! Fill vertical arrays.
!-------------------------------------------------------------------------------

  CALL fillsigma (inmetlays, inmetlaysht)
  CALL vertarys  (ctmlays)

!-------------------------------------------------------------------------------
! Loop over time to get input, process fields, and write output.
!-------------------------------------------------------------------------------

  aqf_now = aqf_start
  CALL getsdt (aqf_now, sdate, stime, gmt)

  timeloop: DO

    WRITE (6,100) aqf_now

    CALL getmet (aqf_now)               ! Read input meteorology file.

    IF ( first ) THEN
       CALL statflds                     ! Process time-invariant fields.

       IF ( makegrid ) THEN
          ! CALL gridout (sdate, stime)   ! Output to GRID files.
          CALL wrgdesc                    ! Write GRIDDESC file.
       ENDIF

       ! IF ( makeemis ) THEN
       !   CALL prep_emis (sdate, stime)   ! Prepare for emissions processing.
       ! ENDIF

       IF ( makebc ) THEN
          CALL readchembc (sdate, stime)  ! Prepare for chemical BC processing.
       ENDIF
    ENDIF

    CALL dynflds (sdate, gmt)           ! Process time-variant fields.
                                        ! Put time-variant fields on PREMAQ grid

!--------------------------------------------------------------------------
! ** "gridout" must call before "prep_emis" for the lat & log info
!
! Because the "refstate" is not used as previous premaq, the information 
! need for "gridout" will be from "dynfld" and need the following changes
!--------------------------------------------------------------------------

    IF ( first ) THEN
       IF ( makegrid ) THEN
          CALL gridout (sdate, stime)   ! Output to GRID files.
       ENDIF

       IF ( makeemis ) THEN
          CALL prep_emis (sdate, stime) ! Prepare for emissions processing
       ENDIF
    ENDIF

!--------------------------------------------------------------------------

    CALL metcro (sdate, stime)          ! Output to MET_CRO and MET_BDY files.
    CALL metdot (sdate, stime)          ! Output to MET_DOT files.

    IF ( makeemis ) THEN
       CALL emissions (sdate, stime)     ! Process met-dependent emissions.
    ENDIF

    IF ( makebc ) THEN
       CALL chembc (sdate, stime)        ! Process time-dependent chemical BCs.
    ENDIF

    CALL geth_newdate (aqf_next, aqf_now, intvl*60)    ! Update SDATE and STIME for 
                                                       ! next I/O API header.
    IF ( aqf_next > aqf_end ) THEN
       EXIT timeloop
    ELSE
       first = .FALSE.

       aqf_now = aqf_next
       CALL getsdt (aqf_now, sdate, stime, gmt)
    ENDIF

  ENDDO timeloop

!-------------------------------------------------------------------------------
! Print emissions report.
!-------------------------------------------------------------------------------

  IF ( makeemis ) THEN
    CALL emis_report
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  CALL dealloc_met
  CALL dealloc_x
  CALL dealloc_ctm

!-------------------------------------------------------------------------------
! Close output files.
!-------------------------------------------------------------------------------

  CALL close_files

!-------------------------------------------------------------------------------
! Insert NCEP tag at end of PREMAQ run.
!-------------------------------------------------------------------------------

  CALL w3tage ('PREMAQ')

  WRITE (6,'(//, a)') '+++++ NORMAL TERMINATION +++++'

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 100 FORMAT (//, 1x, 78('~'),                                      &
             /,  1x, '~~~ Processing meteorology for time = ', a,  &
             /,  1x, 78('~'), /)

END PROGRAM premaq
