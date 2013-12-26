
      SUBROUTINE LAYPOINT (premaq_jdate, premaq_jtime)

!***********************************************************************
!  program body starts at line 262
!
!  DESCRIPTION:
!     This program computes the layer fractions for point sources.  It uses
!     a modified Briggs algorithm to compute plume rise.  The plume is
!     allocated to multiple layers when necessary.
!
!  PRECONDITIONS REQUIRED:  
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION  HISTORY:
!
!    July 2004 by J. Godowitch & G. Pouliot
!    Feb 2005 by G. Pouliot for Air Quality Forecasting
!
!***********************************************************************
!  
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
!  
!***********************************************************************

!**********************************************
!.... MODULES for public variables
!**********************************************

!.... This module is the inventory arrays

      USE MODSOURC, ONLY : XLOCA, YLOCA, STKDM, STKHT, STKTK, STKVE,
     &                     CSOURC, IFIP

!.... This module contains arrays for plume-in-grid and major sources

      USE MODELEV, ONLY : NHRSRC, HRSTKTK, HRSTKVE, HRSTKFL, LMAJOR,
     &                    LAY1F, PLMBOT, PLMTOP

!.... This module contains the information about the source category

      USE MODINFO, ONLY : CATEGORY, CRL, CATLEN, SC_BEGP, SC_ENDP,
     &                    NSRC, NCHARS

!.... This module contains the global variables for the 3-d grid

      USE MODGRID, ONLY : NGRID, VGTYP, VGTOP,
     &                    XORIG, YORIG, GDTYP, P_ALP, P_BET,
     &                    P_GAM, XCENT, YCENT, XCELL, YCELL, GRDNM

      USE MODMERGE, ONLY : LFRAC

!**********************************************
!******* PREMAQ modules
!**********************************************

! this module contains the local variables from laypoint.
! we can split the code into pre_laypoint and the time step components

      USE MOD_LAYPOINT     

      USE MCOUTCOM


!.... This modules contains the MCIP met variables at dot points

      USE MDOUTCOM

      use premaqparm, only: ncols, nrows, nlays


      IMPLICIT NONE

      integer premaq_jdate, premaq_jtime

!.... INCLUDES:

!**********************************************
!.... EXTERNAL FUNCTIONS and their descriptions:
!**********************************************

      LOGICAL         CHKMETEM, DSCM3GRD, DSCM3LAY, ENVYN

      CHARACTER*2     CRLF
      CHARACTER*10    HHMMSS
      CHARACTER*14    MMDDYY
      CHARACTER*16    PROMPTMFILE
      CHARACTER*50    GETCFDSC

      INTEGER         ENVINT, FIND1, INDEX1, PROMPTFFILE, WKDAY

      EXTERNAL   CHKMETEM, CRLF, DSCM3GRD, DSCM3LAY, ENVINT, ENVYN, 
     &           FIND1, GETCFDSC, HHMMSS, INDEX1, MMDDYY, 
     &           PROMPTFFILE, PROMPTMFILE, VERCHAR, WKDAY


      CHARACTER(16) :: PROGNAME = 'LAYPOINT'   ! program name
      REAL          :: CONVPA = 1.0E-2         ! convert Pa to mb

      integer        i_loop, j_loop, idsrc, nrowsncols, nrowsncols1, lxy

      LOGICAL        MIXHT_4_PBL, ACM2PBL
      INTEGER        ISTAT


!***********************************************************************
!   begin body of program LAYPOINT
!***********************************************************************

      nrowsncols = nrows * ncols
      nrowsncols1 = (nrows + 1) * (ncols + 1)
      LDEV = INIT3()

      JDATE = premaq_jdate   ! premaq
      JTIME = premaq_jtime   ! premaq

      CALL M3MSG2('BEGIN: LAYPOINT')	

!AQF      DO T = 1, NSTEPS

      IF ( LDATE .NE. JDATE ) THEN
 
        !... Write day and date message to stdout and log file

         CALL WRDAYMSG( JDATE, MESG )

        !... Write day and date message to report file

         IF ( RDEV .GT. 0 ) THEN
            WRITE( RDEV,93000 ) MESG( 1:LEN_TRIM( MESG ) )
         ENDIF

         LDATE = JDATE
 
      ENDIF

!*********************************************************
!.... Write to screen because WRITE3 only writes to LDEV
!*********************************************************

      WRITE( *, 93020 ) HHMMSS( JTIME )

!*********************************************************
!.... Write to report file if report feature is on
!*********************************************************

      IF ( RDEV .GT. 0 ) THEN
         WRITE( RDEV,93020 ) HHMMSS( JTIME )
      ENDIF

!*********************************************************
!.... Initialize layer fraction array
!*********************************************************

      LFRAC = 0.     ! 2-d array

!*********************************************************
!.... If needed, read hourly plume rise and/or stack parameters...
!.... Read source index
!*********************************************************

      IF ( HFLAG ) THEN

        !...  Do not give an error if could not read data, because it 
        !     might not be there

         IFLAG = .TRUE.
         IF ( .NOT. READ3( HNAME, 'INDXH', ALLAYS3, 
     &                     JDATE, JTIME, LOCINDXH( 1,1 ) ) ) THEN
            L1 = LEN_TRIM( HNAME )
            WRITE( MESG,94010 ) 'WARNING: Could not read '//
     &                          '"IDXH" from file "' // HNAME(1:L1) //'"
     &                          , at', JDATE, ':', JTIME
            CALL M3MESG( MESG )

            LOCINDXH = 0       ! 2-d array
            IFLAG = .FALSE. 
         ENDIF

       !... get number of valid hour-specific sources for current hour

         DO I = NHRSRC, 1, -1
            IF ( LOCINDXH( I,1 ) .NE. 0 ) EXIT
         ENDDO

         NHR = I
      ENDIF

!*********************************************************
!.... Layer-1 fraction
!*********************************************************

      IF ( LFLAG .AND. IFLAG )
     &   CALL SAFE_READ3( HNAME, SPDATNAM(1), ALLAYS3,
     &                    JDATE, JTIME, LAY1F )

!*********************************************************
!.... Plume bottom and top
!*********************************************************

      IF ( BFLAG .AND. IFLAG ) THEN
         CALL SAFE_READ3( HNAME, SPDATNAM(2), ALLAYS3, 
     &                    JDATE, JTIME, PLMBOT )
         CALL SAFE_READ3( HNAME, SPDATNAM(3), ALLAYS3, 
     &                    JDATE, JTIME, PLMTOP )
      ENDIF

!*********************************************************
!.... Temperatures
!*********************************************************

      IF ( TFLAG .AND. IFLAG ) 
     &   CALL SAFE_READ3( HNAME, SPDATNAM(4), ALLAYS3, 
     &                    JDATE, JTIME, HRSTKTK )

!*********************************************************
!.... Velocity
!*********************************************************

      IF ( CFLAG .AND. IFLAG ) 
     &   CALL SAFE_READ3( HNAME, SPDATNAM(5), ALLAYS3, 
     &                    JDATE, JTIME, HRSTKVE )

!*********************************************************
!.... Flow rate
!*********************************************************

      IF ( FFLAG .AND. IFLAG ) 
     &   CALL SAFE_READ3( HNAME, SPDATNAM(6), ALLAYS3, 
     &                    JDATE, JTIME, HRSTKFL )

!*********************************************************
!.... Read time-dependent ZF and ZH for hydrostatic Met data
!.... Compute per-source heights
!*********************************************************

      IF ( .NOT. XFLAG .AND. .NOT. ZSTATIC ) THEN

         !AQF    CALL SAFE_READ3( XNAME,'ZH',ALLAYS3,JDATE,JTIME,XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       lxy = ncols * (j_loop-1) + i_loop
         !       XBUF(lxy, 1:nlays) = x3htm_c(i_loop, j_loop, 1:nlays)
         !    enddo
         ! enddo

         xbuf = reshape ( x3htm_c, (/nrowsncols, nlays/) )
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, ZH )

         !AQF    CALL SAFE_READ3( XNAME,'ZF',ALLAYS3,JDATE,JTIME,XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       lxy = ncols * (j_loop-1) + i_loop
         !       XBUF(lxy, 1:nlays) = x3htf_c(i_loop, j_loop, 1:nlays)
         !    enddo
         ! enddo

         xbuf = reshape ( x3htf_c, (/nrowsncols, nlays/) )
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, ZF )
		
       !****************************************************
       !... Pre-process ZF and ZH to compute DDZH and DDZF
       !****************************************************

         CALL COMPUTE_DELTA_ZS

      ENDIF


!****************************************************
!.... Read and transform meteorology:
!     STRETCH xy 2D to be 1D line, no change for Z
!****************************************************

      IF ( .NOT. XFLAG ) THEN

         !AQF    CALL SAFE_READ3( SNAME, 'HFX', ALLAYS3, JDATE, JTIME, XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       XBUF((ncols*(j_loop-1)+i_loop),1) = hfx_c(i_loop,j_loop)
         !    enddo
         ! enddo

         xbuf(:,1) = reshape ( hfx_c, (/nrowsncols/) )
         CALL BMATVEC( METNGRID, NSRC, 1, NX, CX, XBUF, HFX )

         !AQF    CALL SAFE_READ3( SNAME, 'PBL', ALLAYS3, JDATE, JTIME, XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       XBUF((ncols*(j_loop-1)+i_loop),1) = pbl2_c(i_loop,j_loop)
         !    enddo
         ! enddo

         MIXHT_4_PBL = ENVYN ('MIXHT_4_PBL', 'MIXHT 4 PBL ?', .FALSE. , ISTAT)
         ACM2PBL = ENVYN ('ACM2PBL', 'PBL from ACM2 ?', .TRUE., ISTAT)

         IF ( MIXHT_4_PBL .or. (.not. ACM2PBL) ) THEN
            xbuf(:,1) = reshape ( pbl_c,  (/nrowsncols/) )
         ELSE
            xbuf(:,1) = reshape ( pbl2_c, (/nrowsncols/) )
         ENDIF

         CALL BMATVEC( METNGRID, NSRC, 1, NX, CX, XBUF, HMIX )

         !AQF    CALL SAFE_READ3( SNAME, VNAME, ALLAYS3, JDATE, JTIME, XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       XBUF((ncols*(j_loop-1)+i_loop),1)=temp2_c(i_loop,j_loop)
         !    enddo
         ! enddo

         xbuf(:,1) = reshape ( temp2_c, (/nrowsncols/) )
         CALL BMATVEC( METNGRID, NSRC, 1, NX, CX, XBUF, TSFC )

         CALL GET_VARIABLE_NAME( 'TGD', VNAME )


         !AQF    CALL SAFE_READ3( SNAME, 'USTAR', ALLAYS3, JDATE,JTIME,XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       XBUF((ncols*(j_loop-1)+i_loop),1)=ustar_c(i_loop,j_loop)
         !    enddo
         ! enddo

         xbuf(:,1) = reshape (ustar_c, (/nrowsncols/))
         CALL BMATVEC( METNGRID, NSRC, 1, NX, CX, XBUF, USTAR )

         xbuf(:,1) = reshape (prsfc_c, (/nrowsncols/))        ! surface P, Pa
         CALL BMATVEC( METNGRID, NSRC, 1, NX, CX, XBUF, PRSFC )
    
         !AQF    CALL SAFE_READ3( XNAME, 'TA', ALLAYS3, JDATE, JTIME, XBUF )
         ! do j_loop = 1, nrows
         !    do i_loop = 1, ncols
         !       XBUF((ncols*(j_loop-1)+i_loop),1:nlays)=tempa_c(i_loop, j_loop,1:nlays)
         !    enddo
         ! enddo

         xbuf = reshape (tempa_c, (/nrowsncols, nlays/))
         CALL BMATVEC( METNGRID, NSRC, EMLAYS+1, NX, CX, XBUF, TA )

         xbuf = reshape (wvapor_c, (/nrowsncols, nlays/))
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, QV )


      ! mid-level Pressure, Pa

         xbuf = reshape (press_c, (/nrowsncols, nlays/))     ! mid-level P, Pa
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, PRES )

      ! Full level Pressure, Pa; do not include the "1st layer" ==>PRSFC

         xbuf = reshape (presf_c, (/nrowsncols, nlays/))
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, PREF)


         xbuf = reshape (densa_c, (/nrowsncols, nlays/))		
         CALL BMATVEC( METNGRID, NSRC, EMLAYS, NX, CX, XBUF, DENS )

         dbuf = reshape (uu_d, (/nrowsncols1, nlays/))			
         CALL BMATVEC( NDOTS, NSRC, EMLAYS, ND, CD, DBUF, UWIND )

         dbuf = reshape (vv_d, (/nrowsncols1, nlays/))
         CALL BMATVEC( NDOTS, NSRC, EMLAYS, ND, CD, DBUF, VWIND )
	    
      ENDIF

!*********************************************************
!.... Loop through sources and compute plume rise
!*********************************************************

      K = 0

      DO S = 1, NSRC
         DM = STKDM( S )
         HT = STKHT( S )
         TK = STKTK( S )
         VE = STKVE( S )
         FL = 0.          ! initialize flow
         XL = XLOCA( S )
         YL = YLOCA( S )

       !****************************************************
       !... Find source in index list if hourly data or used
       !****************************************************

         IF ( HFLAG ) THEN
            K = FIND1( S, NHR, LOCINDXH( 1,1 ) )
         ENDIF

       !... Skip source if explicit processing and source not on list

         IF ( XFLAG .AND. K .LE. 0 ) THEN
            CYCLE

       !... Skip source if it is outside output grid

         ELSEIF ( XL .LT. XBEG .OR. XL .GT. XEND .OR.
     &            YL .LT. YBEG .OR. YL .GT. YEND     ) THEN
            CYCLE

       !... Skip source if it is minor source and assign layer fractions
       !    that put all emissions in layer 1

         ELSEIF ( .NOT. LMAJOR( S ) ) THEN
            IF ( XFLAG ) THEN
               WRITE( MESG,94010 ) 
     &              'INTERNAL ERROR: LMAJOR(S) = FALSE for'//
     &              'explicit plume source number', S
               CALL M3MSG2( MESG )
               CALL M3EXIT( PROGNAME, 0, 0, ' ', 2 )
            ELSE
               LFRAC( S,1 ) = 1.
            ENDIF

            CYCLE
         ENDIF

       !****************************************************
       !... If hourly data available, check if source has hourly data
       !    for the current hour, then read hourly stack parameters
       !****************************************************

         IF ( HFLAG ) THEN          ! if hourly used

          !... If source has hourly data...

            IF ( K .GT. 0 ) THEN    ! if source is hourly

             !... If hourly temperatures are available, reset

               IF ( TFLAG ) THEN
                  IF ( HRSTKTK( K ) .GT. 0 ) TK = HRSTKTK( K )
               ENDIF

             !... If hourly velocities are available, reset

               IF ( YFLAG ) THEN
                  IF ( HRSTKVE( K ) .GT. 0 ) VE = HRSTKVE( K )
               ENDIF

             !... If hourly flow rates are available, reset and
             !     recompute velocity if requested

               IF ( FFLAG ) THEN                  !  if flow in hourly file
                  IF ( HRSTKFL(K) .GT. 0 ) THEN   !  if flow valid for source
                     FL = HRSTKFL(K)
                     IF ( CFLAG ) THEN     ! velocity recalculation requested.
                        VE = FL / ( 0.25 * PI * DM * DM )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF 

       !****************************************************
       !... For explicit plume rise, assign plume top and
       !    plume bottom from hourly data, and setup weights for 
       !    allocation to the layers using static layer heights. Weight
       !    by penetration of plume into layer and the layer thickness.
       !... This is the approach for UAM-style processing
       !****************************************************

         IF ( XFLAG ) THEN        ! if compute plume rise

!################## DEFAULT "XFLAG" is false ##################
!
!#############################################################################
!#########  POTENTIAL ERROR FOR TIGHT COUPLING IN THIS SECTION          ######
!#########  VGLVSXG  definition is different from the loosing coupling  ######

          !... If plume bottom, and plume top are available, set 
          !    these and set to skip plume rise computation

            IF ( PLMBOT( K ) .GE. 0. .AND. PLMTOP( K ) .GT. 0. ) THEN
               ZBOT = PLMBOT( K )
               ZTOP = PLMTOP( K )

             !... Otherwise, set top and bottom of plume to be in layer 1.

            ELSE
               ZBOT = VGLVSXG( 1 ) * 0.5    !!#### POTENTIAL ERROR FOR LOOSE & TIGHT
               ZTOP = ZBOT                  !!#### POTENTIAL ERROR FOR LOOSE & TIGHT
            ENDIF

            LFULLHT( 0 ) = 0.

            DO L = EMLAYS, 1, -1
               LFULLHT(L) = VGLVSXG(L  )
               LHALFHT(L) = VGLVSXG(L-1) + 0.5*(VGLVSXG(L)-VGLVSXG(L-1))
               WEIGHTS(L) = 100. * ( LFULLHT(EMLAYS) - LHALFHT(L) )
     &                           /   LFULLHT(EMLAYS)
            ENDDO

!############# End of potential error when use  ous sectione ##################
!##############################################################################

          !... For non-explicit plume rise, preprocess met data...

         ELSE

            DO L = EMLAYS, 0, -1

             ! Calculating Full level pressure
             ! USE SIGMA VALUES AND SFC SURFACES. P = SIGMA(PSFC - TOP) + TOP

              ! PRESF(L) = VGLVSXG(L) * ( PRSFC(S) - VGTOP ) + VGTOP

             !************************************************************************
             !***  The above is only ture for loose coupling, VGLVSXG => X3 => sigma
             !***  It is not ture for tight coupling for X3 => average of eta1 & eta2
             !************************************************************************

             !####################################################
             ! "PRESF(L)" below is bypassing from meteorological 
             !  data and thus fit for any situation
             !####################################################

               if ( L > 0 ) then
                  PRESF(L) = PREF(L,S)
               else
                  PRESF(L) = PRSFC(S)
               endif

               PRESF(L) = PRESF(L) * CONVPA      ! convert from Pa to mb

            ENDDO


          !... convert surface pressure from Pa to mb

            PSFC = PRSFC(S) * CONVPA

     
          !... Compute derived met vars needed before layer assignments

            CALL PREPLM( EMLAYS, HMIX( S ), STKHT( S ), PSFC, TSFC( S ),
     &                   DDZF( 1,S ), QV( 1,S ), TA( 1,S ),
     &                   UWIND( 1,S ), VWIND( 1,S ), 
     &                   ZH( 1,S ), ZF( 1,S ), ZSTK( 1,S ), PRESF( 1 ),
     &                   LSTK, LPBL, TSTK, WSTK, DTHDZ, WSPD, ZZF )

          !  Inserted next 2 lines from earlier laypoint !

          !... Trap USTAR at a minimum realistic value

            USTMP = MAX( USTAR( S ), USTARMIN )

          !... Convert heat flux (watts/m2 to m K /s )

            HFX( S ) = HFX( S ) / ( CP * DENS(1,S) )


            COMPUTE = .TRUE.

          !... If available, assign hourly plume top and plume bottom

            IF ( BFLAG .AND. K .GT. 0 ) THEN

          !... If plume bottom, and plume top are available, set 
          !    these and set to skip plume rise computation

               IF ( PLMBOT( K ) .GE. 0. .AND. PLMTOP( K ) .GT. 0. ) THEN
                  ZBOT = PLMBOT( K )
                  ZTOP = PLMTOP( K )
                  COMPUTE = .FALSE.
               ENDIF
            ENDIF

          !...  Compute plume rise for this source, if needed

            IF ( COMPUTE ) THEN
     
               CALL PLMRIS( EMLAYS, LPBL, LSTK, HFX(S), HMIX(S),
     &                      DM, HT, TK, VE, TSTK, USTMP, DTHDZ,
     &                      TA(1,S), WSPD, ZZF(0), ZH(1,S), ZSTK(1,S),
     &                      WSTK, ZPLM )

             !****** 7/2004
             !... Determine the bottom and top heights of the plume.

               IF ( IPVERT .EQ. 0 ) THEN

             !... Default Turner approach.  Plume thickness = amount of plume rise
             !... Plume rise DH = ZPLM minus the stack height STKHT

	          ZTOP = STKHT(S) + 1.5*(ZPLM-STKHT(S))
                  ZBOT = STKHT(S) + 0.5*(ZPLM-STKHT(S))

               ELSE

             !... Optional alternative method to compute plume top/bot heights

                  CALL PLSPRD ( DTHDZ,ZZF,EMLAYS,ZPLM,HMIX(S),ZTOP,ZBOT )
               ENDIF
            ENDIF

             !******  End change from 7/2004

          !... Setup for computing plume fractions, assuming uniform
          !    distribution in pressure (~mass concentration -- minor 
          !    hydrostatic assumption) from bottom to top.

            LFULLHT( 0:EMLAYS ) = ZZF ( 0:EMLAYS   )
            LHALFHT( 1:EMLAYS ) = ZH  ( 1:EMLAYS,S )
            WEIGHTS( 1:EMLAYS ) = PRES( 1:EMLAYS,S ) * CONVPA
	    TEMPS  ( 1:EMLAYS +1 ) = TA  ( 1:EMLAYS + 1,S )

         ENDIF  !************ XFLAG *****************

       !****************************************************
       !...  Check plume rise for nonsense values
       !****************************************************

         IF ( ZTOP .LT. STKHT( S ) .AND. K .LE. 0 ) THEN

            CALL FMTCSRC( CSOURC( S ), NCHARS, BUFFER, L2 )

            WRITE( MESG,94010 ) 'WARNING: Top of plume found to be ' //
     &                          'less than top of stack for:'//
     &                           CRLF() // BLANK10 // BUFFER( 1:L2 )
            CALL M3MESG( MESG )
            STOP
         ENDIF

       !****************************************************
       !...  Allocate plume to layers
       !****************************************************

         CALL POSTPLM ( EMLAYS, S, ZBOT, ZTOP, PRESF, 
     &                  LFULLHT, TEMPS, LHALFHT, LTOP, TFRAC )     
     
       !****************************************************
       !...  If hourly layer-1 fraction is present, reset this and re-
       !     normalize
       !...  Must account for the case where LAY1F value is missing
       !****************************************************

         IF ( LFLAG .AND. K .GT. 0 ) THEN
            IF ( LAY1F( K ) .GT. 0. .AND. TFRAC( 1 ) .LT. 1. ) THEN
               TSUM = SUM( TFRAC( 2:EMLAYS ) )
               TDIFF = TSUM + TFRAC( 1 ) - LAY1F( K )
               FAC = TDIFF / TSUM

               TFRAC( 1 ) = LAY1F( K )
               TFRAC( 2:EMLAYS ) = TFRAC( 2:EMLAYS ) * FAC
            ENDIF
         ENDIF

       !****************************************************
       !... Check if layer fractions are negative and reset
       !    to output in the first layer if they are.
       !****************************************************

         X = MINVAL( TFRAC( 1:EMLAYS ) )

         IF ( X .LT. 0 ) THEN
            CALL FMTCSRC( CSOURC( S ), NCHARS, BUFFER, L2 )
            WRITE( MESG,94010 ) 'WARNING: One or more negative plume '//
     &                          'fractions found for:'//
     &                        CRLF() // BLANK10 // BUFFER( 1:L2 )//'.'//
     &                        CRLF() // BLANK10 // 'Plume reset to '//
     &                        'have all emissions in surface layer.'
            CALL M3MESG( MESG )

            TFRAC( 1 ) = 1.0
            TFRAC( 2:EMLAYS ) = 0.0
         ENDIF

       !****************************************************
       !... Store layer fractions
       !****************************************************

         IF ( XFLAG ) THEN
            LFRAC( K,1:EMLAYS ) = TFRAC( 1:EMLAYS )  ! array
         ELSE 
            LFRAC( S,1:EMLAYS ) = TFRAC( 1:EMLAYS )  ! array
         ENDIF

       !****************************************************
       !... Check if LTOP out of range, and report (will only work
       !... if REP_LAYR env var has been set b/c default is -1
       !****************************************************

         IF ( REP_LAYR .GT. 0 .AND. LTOP .GT. REP_LAYR ) THEN
            CALL PARSCSRC( CSOURC(S), NCHARS, SC_BEGP, 
     &                               SC_ENDP, LFG, I, CHARS )

            WRITE( OUTFMT, 93042 ) PLTLEN3, NCHARS-2, CHRLEN3
            WRITE( RDEV,OUTFMT ) S, IFIP( S ),
     &                           ( CHARS( I ), I = 2,NCHARS ),
     &                           STKHT( S ), STKVE ( S ), STKTK( S ),
     &                           TSTK, WSTK, LPBL, LTOP 
         ENDIF

      ENDDO 

!############## End loop on sources S ##############


!****************************************************
!.... Write out layer fractions
!****************************************************

!      IF ( .NOT. WRITE3( LNAME, 'LFRAC', JDATE, JTIME, LFRAC ) ) THEN
!
!         MESG = 'Problem writing "LFRAC" to file "' // 
!     &          LNAME( 1:LEN_TRIM( LNAME ) ) // '."'

!         CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
!      ENDIF

!****************************************************
!.... For explicit plume rise, also write out source numbers
!****************************************************

      IF ( XFLAG ) THEN
         IF ( .NOT. WRITE3( LNAME, 'INDXH', JDATE, 
     &                      JTIME, LOCINDXH( 1,1 ) ) ) THEN

            MESG = 'Problem writing "LFRAC" to file "' // 
     &             LNAME( 1:LEN_TRIM( LNAME ) ) // '."'

            CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

         ENDIF
      ENDIF

      CALL NEXTIME( JDATE, JTIME, TSTEP )

!AQF     ENDDO     !  end loop on time steps T

!****************************************************
!.... Exit program with normal completion
!****************************************************

      CALL M3MSG2( 'Normal Completion' // PROGNAME)


!***********************************************************************
!******************  FORMAT  STATEMENTS   ******************************
!***********************************************************************

!****************************************************
!.... Formatted file I/O formats ...... 93xxx
!****************************************************

93000   FORMAT( A )

93020   FORMAT( 8X, 'at time ', A8 )

93040   FORMAT( 'Sources with top of plume greater than layer', I3, //,
     &          'Src ID, ', A, ', H[m], ', 'V[m/s], ', 'Ts[K], ', 
     &          'Ta[K], ', 'U[m/s], ', 'LPBL, ', 'LTOP' )

93042   FORMAT( '( I6, ",", I6.6, ",", A', I2.2, ', ","', I2.2, '(A', 
     &          I2.2, ',", ") , F6.1, ", ", F6.2, ", ", F6.1, ", ",', 
     &          'F5.1, ", ", F6.2, ", ", I3, ", ", I3 )' )



!****************************************************
!.... Internal buffering formats .......94xxx
!****************************************************

94010   FORMAT( 12( A, :, I8, :, 1X ) )




!***********************************************************************
!******************  INTERNAL SUBPROGRAMS  *****************************
!***********************************************************************
 
      CONTAINS

!----------------------------------------------------------------------
!.... This internal subprogram tries to retrieve the I/O API header
!     and aborts if it was not successful
!----------------------------------------------------------------------

      SUBROUTINE RETRIEVE_IOAPI_HEADER( FILNAM )

!.... arguments

      CHARACTER(*) FILNAM

!----------------------------------------------

      IF ( .NOT. DESC3( FILNAM ) ) THEN

         MESG = 'Could not get description of file "' //
     &          FILNAM( 1:LEN_TRIM( FILNAM ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

      ENDIF

      END SUBROUTINE RETRIEVE_IOAPI_HEADER




!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!.... This internal subprogram tries to retrieve the I/O API header
!     and aborts if it was not successful
!----------------------------------------------------------------------

      SUBROUTINE RETRIEVE_SET_HEADER( FILNAM )

      INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions

!.... arguments

      CHARACTER(*) FILNAM

!----------------------------------------------------------------------

      IF ( .NOT. DESCSET( FILNAM,-1 ) ) THEN

         MESG = 'Could not get description of file "' //
     &          FILNAM( 1:LEN_TRIM( FILNAM ) ) // '"'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

      ENDIF

      END SUBROUTINE RETRIEVE_SET_HEADER




!----------------------------------------------------------------------
!.... This internal subprogram resolves the differences in variable
!     names for different version of the Met files
!----------------------------------------------------------------------

      SUBROUTINE GET_VARIABLE_NAME( INNAME, OUTNAME )

!.... arguments

      CHARACTER(*), INTENT (IN) :: INNAME    ! variable name to check
      CHARACTER(*), INTENT(OUT) :: OUTNAME   ! variable name to read

!.... Local variables

      INTEGER J

!**************************************************
!.... Search for variable name in the list of names
!**************************************************

      J = INDEX1( INNAME, NVARS3D, VNAME3D )

!**************************************************
!.... If the input name is there, then set output name and return
!**************************************************

      IF ( J .GT. 0 ) THEN
         OUTNAME = INNAME
         RETURN
      ENDIF

!**************************************************
!.... Set output name
!.... Currently there is only one alternative for each
!**************************************************

      SELECT CASE( INNAME )
         CASE( 'ZH' )
            OUTNAME = 'X3HT0M'
         CASE( 'ZF' )
            OUTNAME = 'X3HT0F'
         CASE( 'TGD' ) 
           ! OUTNAME = 'TEMP10'
            OUTNAME = 'TEMP1P5'
         CASE DEFAULT
            MESG = 'INTERNAL ERROR: Do not have an alternative ' //
     &             'name for met variable ' // INNAME
            CALL M3MSG2( MESG )
            CALL M3EXIT( PROGNAME, 0, 0, ' ', 2 )
      END SELECT

      RETURN

      END SUBROUTINE GET_VARIABLE_NAME




!----------------------------------------------------------------------
!.... This internal subprogram tries to read a variable from an
!     I/O API file, and aborts if not successful.
!----------------------------------------------------------------------

      SUBROUTINE SAFE_READ3( FILNAM, VARNAM, LAYER, JDATE, JTIME, XBUF )

!.... arguments

      CHARACTER(*) FILNAM    ! logical file name
      CHARACTER(*) VARNAM    ! variable name
      INTEGER      LAYER     ! layer number (or ALLAYS3)
      INTEGER      JDATE     ! Julian date
      INTEGER      JTIME     ! time
      REAL         XBUF( * ) ! read buffer

!---------------------------------------

      IF ( .NOT. READ3(FILNAM, VARNAM, LAYER, JDATE, JTIME, XBUF) ) THEN

         L1 = LEN_TRIM( VARNAM )
         L2 = LEN_TRIM( FILNAM )
         MESG = 'Could not read "' // VARNAM( 1:L1 ) // 
     &          '" from file "' // FILNAM( 1:L2 ) // '."'
         CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )

      ENDIF

      END SUBROUTINE SAFE_READ3





!----------------------------------------------------------------------
!.... This internal subprogram computes DDZH and DDZF
!----------------------------------------------------------------------

      SUBROUTINE COMPUTE_DELTA_ZS

!---------------------------------

      DO S = 1, NSRC

        ! ZZ0 = ZF( 1,S )
        ! ZSTK ( 1,S ) = ZZ0 - STKHT( S )

         ZZ0 = ZH( 1,S )
         ZSTK ( 1,S ) = ZF( 1,S ) - STKHT( S )

	 DDZH( 1,S ) = 1.0 / ZZ0

         ZF0 = ZF( 1,S )
         DDZF( 1,S ) = 1.0 / ZF0

         DO L = 2, EMLAYS

           ! ZZ1 = ZF( L,S )
           ! ZSTK( L  ,S ) = ZZ1 - STKHT( S )
           ! DDZH( L-1,S ) = 1.0 / ( ZZ1 - ZZ0 )

            ZZ1 = ZH( L,S )
            ZSTK( L  ,S ) = ZF( L,S ) - STKHT( S )
            DDZH( L,S ) = 1.0 / ( ZZ1 - ZZ0 )

            ZZ0 = ZZ1
            ZF1 = ZF( L,S )
            DDZF( L,S ) = 1.0 / ( ZF1 - ZF0 )

            ZF0 = ZF1

         ENDDO
      ENDDO

      END SUBROUTINE COMPUTE_DELTA_ZS



      END SUBROUTINE LAYPOINT
