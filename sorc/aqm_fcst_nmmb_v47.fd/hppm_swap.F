
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
C $Header: /project/work/rep/CCTM/src/hadv/hppm/hppm.F,v 1.14 2005/02/14 15:39:10 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HPPM ( NI, CONI, VEL, DT, DSI, ORI )
      
C----------------------------------------------------------------------
C Function      
C   This is the one-dimensional implementation of piecewise parabolic
C   method.  Variable grid spacing is allowed. The scheme is positive
C   definite and monotonic. It is conservative, and causes small
C   numerical diffusion.
      
C   A piecewise continuous parabola is used as the intepolation polynomial.
C   The slope of the parabola at cell edges are computed from a cumulative
C   function of the advected quantity.  These slopes are further modified
C   so that the interpolation function is monotone. For more detailed
C   information see:
      
C   Colella, P., and P. L. Woodward, (1984), "The Piecewise Parabolic
C   Method (PPM) for Gas-Dynamical Simulations," J. Comput. Phys. 54,
C   174-201.
      
C   The concentrations at boundary cells (i.e., at 1 and NI) are not
C   computed here.  They should be updated according to the boundary
C   conditions.
      
C   The following definitions are used:
     
C              |---------------> Positive direction
C     
C  -->|Boundary|<----------------Main Grid----------------->|Boundary|<--
C     
C     |---><---|---><---|       ~|---><---|~       |---><---|---><---|
C       CON(0)   CON(1)            CON(i)            CON(n)  CON(n+1)
C     
C     VEL(1)-->|        VEL(i)-->|        |-->VEL(i+1)      |-->VEL(n+1)
C    
C      FP(0)-->|       FP(i-1)-->|        |-->FP(i)         |-->FP(n)
C     
C      FM(1)<--|         FM(i)<--|        |<--FM(i+1)       |<--FM(n+1)
C    
C                             -->| DS(i)  |<--
      
C----------------------------------------------------------------------
      
C Revision History:
      
C   20 April, 1993 by M. Talat Odman at NCSC: 
C   Created based on Colella and Woodward (1984)
      
C   15 Sept., 1993 by Daewon Byun at EPA:
C   Original code obtained from Phillip Colella at Berkeley
      
C   29 Nov.,  1993 by M. Talat Odman at NCSC:
C   Found no difference from original code
      
C   05 Oct.,  1993 by M. Talat Odman at NCSC:
C   Modified for EDSS archive, made discontinuity capturing an option

C   Sep 97 Jeff
C   Aug 98 - Jeff - optimize for mesh coefficients      

C   David Wong - Sep. 1998
C     -- parallelized the code
C     -- Expanded the one-level nested loop which involves either with row or
C        column, into a three-level nested loop with layers and species.
C        Corresponding arrays' dimensions were adjusted accordingly
C   Jeff - optimize for mesh coefficients
C
C   David Wong - 1/8/99
C     -- BARRIER is removed
C
C   David Wong - 1/12/99
C     -- inside BNDY_HI_PE conditional code segment, NI is changed to MY_NI
C
C   David Wong - 1/12/99
C     -- change se_loop_index argument list
C     -- add new subroutine call to determine lo and hi boundary processor

C   22 Nov 00 J.Young: PE_COMM2E -> Dave Wong's f90 stenex COMM
C                      PE_COMM3E -> Dave Wong's f90 stenex COMM

C   23 Feb 01 J.Young: allocatable arrays ...
C                      Since F90 does not preserve dummy argument array
C                      indices, CONI( 1:NI+2,, ) is copied into local array
C                      CON( 0:NI+1,, ).
C                      The caller of HPPM dimensions the actual argument,
C                      as CON( -NTHIK+1:MY_NCOLS+NTHIK,, ).

C   3 Sep 01 David Wong
C     -- use "dynamic" data structure instead of F90 ALLOCATE statement to
C        avoid memory fragmentation which eventually leads to not enough
C        contigous memory (F90 bug?)
C   24 Mar 04 G.Hammond: moved all mpi communication to caller

C   06/16/04 by Peter Percell & Daewon Byun at UH-IMAQS: 
C     - Fixed bug in using fluxes in non-uniform grids to update concentrations

C   14 Feb 05 J.Young: fix DS dimension bug
C   11 Oct 05 J.Young: re-dimension lattice arrays to one
C    1 Nov 06 J.Young: Following Glenn Hammond, moved all communication
C   out of HPPM; using "swap_sandia" communication in caller; update only
C   local values in the CGRID array within a time step, discarding previous
C   ghost values.

C----------------------------------------------------------------------
      
      USE HGRD_DEFN

      USE SUBST_MODULES              ! stenex
!     USE SUBST_COMM_MODULE          ! stenex
!     USE SUBST_UTIL_MODULE          ! stenex

#ifdef snl_timing
      use timing
#endif

      IMPLICIT NONE

C Includes:
      
!     INTEGER, PARAMETER :: NTHIK = 1
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IODECL      ! I/O definitions and declarations
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
 
      INTEGER     NI              ! number of zones (cells) along the stride
      REAL     :: CONI( :,: )     ! conc's along the stride
      REAL     :: VEL( : )        ! velocities at zone (cell) boundaries
      REAL        DT              ! time step
      REAL     :: DSI ( : )       ! distance between zone (cell) boundaries
      CHARACTER   ORI             ! orientation of advection ('C'-x or 'R'-y)

C Parameters:
      
C Flag for discontinuty capturing (steepening)
#ifdef STEEPEN_FLAG
      LOGICAL, PARAMETER :: STEEPEN = .TRUE.
#else
      LOGICAL, PARAMETER :: STEEPEN = .FALSE.
#endif

      REAL, PARAMETER :: ETA1 = 20.0
      REAL, PARAMETER :: ETA2 = 0.05
      REAL, PARAMETER :: EPS = 0.01
      
      REAL, PARAMETER :: TWO3RDS = 2.0 / 3.0

C Local variables:

C orientation flag (ORI)
      CHARACTER, SAVE :: FIRSTORI = ' '
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      
      INTEGER, SAVE :: NSPCS
      INTEGER, SAVE :: MNR

      REAL :: CON( -2:NI+3,SIZE( CONI,2 ) )
      REAL :: DS ( -3:NI+3 ) ! distance between cell boundaries (local)

      REAL A, B, C, D                        ! temp lattice var's.

      REAL :: GAMMA                          ! temp lattice var.
      REAL, ALLOCATABLE, SAVE :: ALPHA ( : )
      REAL, ALLOCATABLE, SAVE :: BETA  ( : )
      REAL, ALLOCATABLE, SAVE :: MU    ( : ) ! lattice var. for CM
      REAL, ALLOCATABLE, SAVE :: NU    ( : ) ! lattice var. for CM
      REAL, ALLOCATABLE, SAVE :: LAMBDA( : ) ! lattice var. for CM
      REAL, ALLOCATABLE, SAVE :: CHI   ( : ) ! lattice var. for DC
      REAL, ALLOCATABLE, SAVE :: PSI   ( : ) ! lattice var. for DC
      REAL, ALLOCATABLE, SAVE :: ZETA  ( : ) ! lattice var. for ETABAR
      REAL, ALLOCATABLE, SAVE :: SIGMA ( : ) ! lattice var. for D2C
      REAL, ALLOCATABLE, SAVE :: TAU   ( : ) ! lattice var. for D2C

C outflux from left or bottom of cell
      REAL :: FM    (    NI+1,SIZE( CONI,2 ) )
C outflux from right or top of cell
      REAL :: FP    (  0:NI,  SIZE( CONI,2 ) )
C zone R.H. trial intercept
      REAL :: CM    (  0:NI+2,SIZE( CONI,2 ) )
C zone L.H. intercept
      REAL :: CL    (  0:NI+1,SIZE( CONI,2 ) )
C zone R.H. intercept
      REAL :: CR    (  0:NI+1,SIZE( CONI,2 ) )
C CR - CL
      REAL :: DC    ( -1:NI+2,SIZE( CONI,2 ) )
C coefficient of second-order term
      REAL :: C6    (  0:NI+1,SIZE( CONI,2 ) )
C second derivative
      REAL :: D2C   ( -1:NI+2,SIZE( CONI,2 ) )
C discontinuity homotopy function
      REAL :: ETA   (  0:NI+1,SIZE( CONI,2 ) )
C 3rd to 1st order derivative ratio
      REAL :: ETABAR(  0:NI+1,SIZE( CONI,2 ) )
C zone L.H. intercept w/ discontinuity
      REAL :: CLD   (  0:NI+1,SIZE( CONI,2 ) )
C zone R.H. intercept w/ discontinuity
      REAL :: CRD   (  0:NI+1,SIZE( CONI,2 ) )

      CHARACTER( 96 ) :: XMSG = ' '

      REAL X, Y                 ! Courant number
      INTEGER ALLOCSTAT
      
      INTEGER I, L, S           ! loop indices

      INTEGER :: START, END
      LOGICAL, SAVE :: BNDY_LO_PE, BNDY_HI_PE

!     integer, save :: logdev

C----------------------------------------------------------------------

C get local DS
      DO I = -2, NI + 3
         DS( I ) = DSI( I+3 )
         END DO
      DS( -3 ) = DS( -2 )

      IF ( ORI .NE. FIRSTORI ) THEN
         FIRSTORI = ORI

         CALL SUBST_HI_LO_BND_PE ( ORI, BNDY_LO_PE, BNDY_HI_PE )

!        WRITE( *,* ) '   HPPM Steepen: ', STEEPEN

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
!           logdev = init3()
            NSPCS = SIZE ( CONI,2 )
            MNR   = MAX( NCOLS, NROWS )
            ALLOCATE ( ALPHA ( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( BETA  ( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( MU    ( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( NU    ( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( LAMBDA( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( CHI   ( -1:MNR+2 ), STAT = ALLOCSTAT )
            ALLOCATE ( PSI   ( -1:MNR+2 ), STAT = ALLOCSTAT )
            IF ( STEEPEN ) THEN
               ALLOCATE ( ZETA  ( -1:MNR+2 ), STAT = ALLOCSTAT )
               ALLOCATE ( SIGMA ( -1:MNR+2 ), STAT = ALLOCSTAT )
               ALLOCATE ( TAU   ( -1:MNR+2 ), STAT = ALLOCSTAT )
            END IF
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating lattice variable(s)'
               CALL M3EXIT ( 'HPPM', 0, 0, XMSG, XSTAT1 )
            END IF
         END IF

         DO I = -1, NI + 2
            ALPHA( I ) = DS( I )   + DS( I+1 )
            BETA( I )  = DS( I-1 ) + DS( I )
            GAMMA      = DS( I-2 ) + DS( I-1 )
            D = DS( I ) / ( BETA( I ) + DS( I+1 ) )
            CHI( I ) = D * ( DS( I-1 ) + BETA( I ) ) / ALPHA( I )
            PSI( I ) = D * ( ALPHA( I ) + DS( I+1 ) ) / BETA( I )
            A = DS( I-1 ) / BETA( I )
            B = 2.0 * DS( I ) / BETA( I )
            C = 1.0 / ( ALPHA( I ) + GAMMA )
            MU( I ) = C * DS( I-1 ) * GAMMA / ( DS( I-1 ) + BETA( I ) )
            NU( I ) = C * DS( I ) * ALPHA( I ) / ( DS( I ) + BETA( I ) )
            LAMBDA( I ) = A + MU( I ) * B - 2.0 * NU( I ) * A
         END DO

C No need to update these arrays with off-PE data

         IF ( STEEPEN ) THEN
            DO I = -1, NI + 2
               C = 1.0 / ( BETA( I ) + DS( I+1 ) )
               SIGMA( I ) = C / ALPHA( I )
               TAU( I )   = C / BETA( I )
               ZETA( I ) = 0.25 * ( ALPHA( I ) * ALPHA( I )
     &                   -          ALPHA( I ) * BETA( I )
     &                   +          BETA( I ) * BETA( I ) )
            END DO
         END IF

      END IF   ! FIRSTORI

C get local con

      DO I = -2, NI + 3
         CON( I,: ) = CONI( I+3,: )
      END DO

C Set all fluxes to zero. Either positive or negative flux will
C remain zero depending on the sign of the velocity.
      
      DO S = 1, NSPCS
         FP( 0,S ) = 0.0
         DO I = 1, NI
            FM( I,S ) = 0.0
            FP( I,S ) = 0.0
         END DO
         FM( NI+1,S ) = 0.0
      END DO

C Second order polynomial inside the domain
      
      DO S = 1, NSPCS
         DO I = -1, NI + 2
      
C Compute average slope in the i'th zone
      
C Equation (1.7)

            DC( I,S ) = CHI( I ) * ( CON( I+1,S ) - CON( I,S ) )
     &                + PSI( I ) * ( CON( I,S )   - CON( I-1,S ) )
      
C Guarantee that CM lies between CON(I) and CON(I+1) - monotonicity constraint
      
            IF ( ( CON( I+1,S ) - CON( I,S ) )
     &         * ( CON( I,S )   - CON( I-1,S ) ) .GT. 0.0 ) THEN
               DC( I,S ) = SIGN( 1.0, DC( I,S ) )
     &                   * MIN(      ABS( DC( I,S ) ),
     &                         2.0 * ABS( CON( I+1,S ) - CON( I,S ) ),
     &                         2.0 * ABS( CON( I,S ) - CON( I-1,S ) ) )
               ELSE
               DC( I,S ) = 0.0
               END IF                                    ! Equation (1.8)

         END DO   ! I
      END DO   ! S

      DO S = 1, NSPCS
         DO I = 0, NI + 2     ! Equation (1.6)
            CM( I,S ) = CON( I-1,S )
     &                + LAMBDA( I ) * ( CON( I,S ) - CON( I-1,S ) )
     &                - MU( I ) * DC( I,S ) + NU( I ) * DC( I-1,S )
         END DO

C Initialize variables for discontinuty capturing. This is necessary
C even if discontinuity capturing is deactivated.
      
         DO I = 0, NI + 1         ! these reduce Eqn. 1.15 
            ETA( I,S ) = 0.0
            CLD( I,S ) = CON( I,S )
            CRD( I,S ) = CON( I,S )
         END DO

      END DO   ! S

C If PE near bottom or left boundary...
C Zeroth order polynomial at the boundary cells
C First order polynomial at the next cells, no monotonicity constraint needed

      IF ( BNDY_LO_PE ) THEN
         DO S = 1, NSPCS
            CM( 1,S ) = CON( 1,S )
            CM( 2,S ) = ( DS( 1 ) * CON( 2,S )
     &                +   DS( 2 ) * CON( 1,S ) )
     &                / ( DS( 1 ) + DS( 2 ) )
         END DO
      END IF

C If PE near top or right boundary...
C Zeroth order polynomial at the boundary cells
C First order polynomial at the next cells, no monotonicity constraint needed

      IF ( BNDY_HI_PE ) THEN
         DO S = 1, NSPCS
            CM( NI+1,S ) = CON( NI,S )
            CM( NI,S )   = ( DS( NI-1 ) * CON( NI,S )
     &                   +   DS( NI )   * CON( NI-1,S ) )
     &                   / ( DS( NI-1 ) + DS( NI ) )
         END DO
      END IF
      
      IF ( STEEPEN ) THEN
 
C Finite diff. approximation to 2nd derivative as in Equation (1.17)

         DO S = 1, NSPCS

            DO I = -1, NI + 2
               D2C( I,S ) = SIGMA( I ) * ( CON( I+1,S ) - CON( I,S ) )
     &                    -   TAU( I ) * ( CON( I,S )   - CON( I-1,S ) )
            END DO

         END DO

C No discontinuity detection near the boundary: cells 1, 2, NI-1, NI
 
         IF ( BNDY_LO_PE ) THEN
            START = 3
         ELSE
            START = 0
         END IF
         IF ( BNDY_HI_PE ) THEN
            END = NI - 2
         ELSE
            END = NI + 1
         END IF

         DO S = 1, NSPCS
            DO I = START, END

C Compute etabars in Equation (1.16)
 
               IF ( ( - D2C( I+1,S ) * D2C( I-1,S ) .GT. 0.0 ) .AND.
     &              (
     &              ABS( CON( I+1,S ) - CON( I-1,S ) )
     &              - EPS * MIN( ABS( CON( I+1,S ) ), ABS( CON( I-1,S ) )
     &              )
     &              .GT. 0.0 ) ) THEN        ! 2nd derivative changes sign
                  ETABAR( I,S ) = - ZETA( I )
     &                          * ( D2C( I+1,S ) - D2C( I-1,S ) )
     &                          / ( CON( I+1,S ) - CON( I-1,S ) )
               ELSE
                  ETABAR( I,S ) = 0.0
               END IF
 
C Equation (1.16)
 
               ETA( I,S ) = MAX( 0.0,
     &                      MIN( ETA1 * ( ETABAR( I,S ) - ETA2 ), 1.0 ) ) 
 
C Equation (1.14)
 
               CRD( I,S ) = CON( I+1,S ) - 0.5 * DC( I+1,S )
               CLD( I,S ) = CON( I-1,S ) + 0.5 * DC( I-1,S )

            END DO   ! I
         END DO   ! S

      END IF                    ! if STEEPEN

C Generate piecewise parabolic distributions

      DO S = 1, NSPCS

         DO I = 0, NI + 1

C Equation (1.15)

            CR( I,S ) = CM( I+1,S )
     &                + ETA( I,S ) * ( CRD( I,S ) - CM( I+1,S ) )
            CL( I,S ) = CM( I,S )
     &                + ETA( I,S ) * ( CLD( I,S ) - CM( I,S ) )
 
C Monotonicity
 
            IF ( ( CR( I,S ) - CON( I,S ) )
     &        * ( CON( I,S ) - CL( I,S ) ) .GT. 0.0 ) THEN

C Temporary computation of DC and C6

               DC( I,S ) = CR( I,S ) - CL( I,S )
               C6( I,S ) = 6.0 * ( CON( I,S )
     &                   - 0.5 * ( CL( I,S ) + CR( I,S ) ) )

C overshoot cases

               IF ( DC( I,S ) * C6( I,S ) .GT.
     &              DC( I,S ) * DC( I,S ) ) THEN
                  CL( I,S ) = 3.0 * CON( I,S ) - 2.0 * CR( I,S )
                  ELSE IF ( -DC( I,S ) * DC( I,S ) .GT.
     &                       DC( I,S ) * C6( I,S ) ) THEN
                  CR( I,S ) = 3.0 * CON( I,S ) - 2.0 * CL( I,S )
                  END IF

            ELSE                   ! Local extremum: Interpolation  
                                   ! function is set to be a constant
               CL( I,S ) = CON( I,S )
               CR( I,S ) = CON( I,S )

            END IF

            DC( I,S ) = CR( I,S ) - CL( I,S )      ! Equation (1.5)
            C6( I,S ) = 6.0 * ( CON( I,S )
     &                - 0.5 * ( CL( I,S ) + CR( I,S ) ) )

         END DO   ! I

C Compute fluxes from the parabolic distribution as in Equation (1.12)

         I = 0
         IF ( VEL( I+1 ) .GT. 0.0 ) THEN
            Y = VEL( I+1 ) * DT
            X = Y / DS( I )
            FP( I,S ) = Y * ( CR( I,S ) - 0.5 * X * ( DC( I,S )
     &                - C6( I,S ) * ( 1.0 - TWO3RDS * X ) ) )
         END IF
      
         DO I = 1, NI

C mass leaving interval I at left end
C = length of segment leaving * average concentration in that segment

            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               X = Y / DS( I )
               FM( I,S ) = Y * ( CL( I,S ) + 0.5 * X * ( DC( I,S )
     &                   + C6( I,S ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

C mass leaving interval I at right end

            IF ( VEL( I+1 ) .GT. 0.0 ) THEN
               Y = VEL( I+1 ) * DT
               X = Y / DS( I )
               FP( I,S ) = Y * ( CR( I,S ) - 0.5 * X * ( DC( I,S )
     &                   - C6( I,S ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

         END DO   ! I

         I = NI + 1
         IF ( VEL( I ) .LT. 0.0 ) THEN
            Y = -VEL( I ) * DT
            X = Y / DS( I )
            FM( I,S ) = Y * ( CL( I,S ) + 0.5 * X * ( DC( I,S )
     &                + C6( I,S ) * ( 1.0 - TWO3RDS * X ) ) )
         END IF

      END DO   ! S

C Compute fluxes from boundary cells assuming uniform distribution
      
C If PE near top or left boundary...

      IF ( BNDY_LO_PE ) THEN
         IF ( VEL( 1 ) .GT. 0.0 ) THEN
            Y = VEL( 1 ) * DT
            DO S = 1, NSPCS
               FP( 0,S ) = Y * CON( 0,S )
            END DO
         END IF
      END IF

C If PE near bottom or right boundary...

      IF ( BNDY_HI_PE ) THEN
         IF ( VEL( NI+1 ) .LT. 0.0 ) THEN
            Y = -VEL( NI+1 ) * DT
            DO S = 1, NSPCS
               FM( NI+1,S ) = Y * CON( NI+1,S )
            END DO
         END IF
      END IF

C Update concentrations as in Equation (1.13)

      DO S = 1, NSPCS
      
         DO I = 1, NI
            CON( I,S ) = CON( I,S )
     &                 + ( FP( I-1,S ) - FP( I,S )
     &                 +   FM( I+1,S ) - FM( I,S ) ) / DS( I )
         END DO
      
      END DO

      DO I = 1, NI
         CONI( I+3,: ) = CON( I,: )
      END DO

      RETURN
      END
