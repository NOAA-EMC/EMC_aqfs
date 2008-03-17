
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrsolver.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%




       SUBROUTINE HRSOLVER( JDATE, JTIME, C, R, L )


C**********************************************************************
C
C  FUNCTION: EBI solver 
C
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  HRRATES, HRG1, HRG2, HRG3,
C                                     HRG4, HRPRODLOSS
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, April, 2003
C                    Corrected convergence failure treatment - JGipson, Nov 2004
C                    
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE 

C..INCLUDES:
      INCLUDE "/meso/save/wx20jy/cctm/BLD_u5a/GC_SPC.EXT"    ! Gas chem species names and MWs

C..ARGUMENTS:
      INTEGER JDATE           ! Current date (YYYYDDD)
      INTEGER JTIME           ! Current time (HHMMSS)
      INTEGER C, R, L         ! Cell col, row, lev

C..PARAMETERS:
      INTEGER, PARAMETER :: MXBKUPS = 5  ! Max no. of back-ups allowed
      INTEGER, PARAMETER :: STAT = 1     ! Status code

      REAL, PARAMETER :: EPSLON = 1.0E-30     ! Small number
      REAL, PARAMETER :: MAXPRED = 1.0E+03    ! Upper limit on predicted conc
      REAL, PARAMETER :: ZERO = 0.0               ! zero 

C..EXTERNAL FUNCTIONS:
      INTEGER JUNIT


C..SAVED LOCAL VARIABLES:
      CHARACTER*16, SAVE ::  PNAME      ! Program name
 
     
C..SCRATCH LOCAL VARIABLES:

      CHARACTER( 132 ) :: MSG           ! Message text

      INTEGER CELLNO          ! Cell no. fo debug output
      INTEGER ITER            ! Loop index for Backward Euler iterations
      INTEGER S               ! Loop index for species
      INTEGER NEBI            ! Loop index for time steps
      INTEGER NINR            ! No. of inner time steps 
      INTEGER N               ! Loop index
      INTEGER EBI             ! Loop index
      INTEGER NBKUPS          ! No. of times time step reduced

      LOGICAL LEBI_CONV             ! Flag for EBI convergence
      LOGICAL LEBISPFL( N_GC_SPC )  ! Flag for EBI species

      REAL DTC              ! Time step to take
      REAL FXDLOSS          ! Total loss due to negative stoichiometry
      REAL VARLOSS          ! Loss excluding negative stoichiometry



C**********************************************************************
      DATA PNAME  / 'HRSOLVER'/




c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++

         N_EBI_IT = 0

      DO 3000 NEBI = 1, N_EBI_STEPS    ! outer EBI time-tep loop

         DTC = EBI_TMSTEP
         NBKUPS = 0
         N_INR_STEPS = 1

 100     CONTINUE                        !  Restart location

         DO 2000 NINR = 1, N_INR_STEPS   ! No. of time steps for back-up
 
            DO S = 1, N_SPEC             ! Set ICs for EBI iterations
               YC0( S ) = YC( S )
            ENDDO

            DO 1000 ITER = 1, NEBITER    ! EBI iteration loop

               N_EBI_IT = N_EBI_IT + 1

               CALL HRRATES

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call routines to compute concentrations of groups 1-4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

               CALL HRG1( DTC )
 
               CALL HRG2( DTC )

               CALL HRG3( DTC )

               CALL HRG4( DTC )
 
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the Euler backward method
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               CALL HRPRODLOSS

               DO EBI = 1, N_EBISP
                  S = EBISP( EBI )
                  YCP( S ) = ( YC0( S ) + PROD( S ) * DTC ) / 
     &                       ( 1.0 + LOSS( S ) * DTC / YC( S ) )
               ENDDO

c..Special treatment of PAR because of negative product stoichiometry
               IF( PNEG( PAR ) .GT. 0.0 ) THEN
                  FXDLOSS = PNEG( PAR ) * DTC
                  IF( FXDLOSS .GE. YC0( PAR ) + PROD( PAR ) * DTC ) THEN
                     YCP( PAR ) = 0.0
                  ELSE
                     VARLOSS = MAX( LOSS( PAR ) - PNEG( PAR ) , ZERO )
                     YCP( PAR ) = ( YC0( PAR ) + PROD( PAR ) * DTC  - 
     &                   FXDLOSS ) / ( 1.0 + VARLOSS * DTC / YC( PAR ) )
                  ENDIF
               ENDIF


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check for convergence
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               LEBI_CONV = .TRUE.
               DO S = 1, N_SPEC
                  LEBISPFL( S ) = .FALSE.
                  YCP( S ) = MAX( EPSLON, YCP( S ) )
                  IF( ABS( YC( S ) - YCP( S ) ) .GT. RTOL( S ) *
     &               ( YC( S ) + YCP( S ) ) ) THEN
                     LEBI_CONV = .FALSE.
                     LEBISPFL( S ) = .TRUE.
                  ENDIF
c..if predictions growing too large, treat as a convergence failure
                  IF( YCP( S ) .GT. MAXPRED ) GO TO 1010
                  YC( S ) = YCP( S ) 
               ENDDO

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++


               IF( LEBI_CONV ) GO TO 2000
      
 1000       CONTINUE
          
c...Convergence failure section; cut the inner time step in half &
c.....start inner loop over unless max backups exceeded 

 1010       CONTINUE

            NBKUPS = NBKUPS + 1

            IF( NBKUPS .LE. MXBKUPS ) THEN
  
               WRITE( LOGDEV, 92000 ) C, R, L, NBKUPS

               DO S = 1, N_SPEC
                  YC( S ) = YC0( s )
               ENDDO

               DTC = 0.5 * DTC

               N_INR_STEPS = 2 ** NBKUPS

               GO TO 100

            ELSE
         
               WRITE( LOGDEV, 92040 ) C, R, L

               WRITE( LOGDEV, 92060 )
               DO S = 1, N_SPEC
                  IF( LEBISPFL( S ) ) WRITE( LOGDEV, 92080 ) GC_SPC( S )
               ENDDO

               MSG = 'ERROR: Stopping because of EBI convergence failures'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, STAT )

            ENDIF

 2000    CONTINUE

 3000 CONTINUE

      RETURN


92000 FORMAT( 'WARNING: EBI Euler convergence failure' / 
     &        '         Reducing EBI time step because of ',
     &         'convergence failure for ' /
     &        '         Cell (', I3, ', ', I3, ', ', I3, ')' ,
     &        '  Back-up number', I2 )

92040 FORMAT( 'ERROR: Max number of EBI time step reductions exceeded' 
     &      / '      Convergence failure for cell (', I3, ', ', I3,
     &                ', ', I3, ')' )

92060 FORMAT( '      Convergence failure for the following species:' )

92080 FORMAT( 10X, A )

      END
