
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
C $Header: /project/air5/sjr/CMAS4.5/rel/models/CCTM/src/chem/ebi_cb4/hrg4.F,v 1.1.1.1 2005/09/09 18:56:05 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%



       SUBROUTINE HRG4( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of NO3 and N2O5
C
C  PRECONDITIONS: For CB4 family of mechanisms only
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 2002
C                    
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE 


C..INCLUDES: NONE


C..ARGUMENTS:      
      REAL DTC             ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER*16 PNAME     ! Program name
      SAVE PNAME

      
C..SCRATCH LOCAL VARIABLES:
      REAL A1, A2, A3      ! Temp scalars
      REAL ATOP3           ! Temp scalar
      REAL ATOP5           ! Temp scalar
      REAL BOTT            ! Temp scalar
      REAL L_N2O5          ! Loss of N2O5
      REAL L_NO3           ! Loss of NO3
      REAL P_NO3           ! Production of NO3

      REAL R56DT           ! Kn2o5-->no3 x delta t
      REAL R65DT           ! Kno3+no2-->n2o5 x delta t x[NO2]

C**********************************************************************
      DATA PNAME  / 'HRG4'/

c..Loss of N2O5 
      L_N2O5 = RKI( 19 ) + RKI( 20 )

c..Production of NO3 (except from N2O5 )
      P_NO3  = RXRAT( 5 ) + RXRAT( 7 ) + RXRAT( 28 )

c..Loss of NO3 
      L_NO3 = RKI( 15 )              + RKI( 16 ) * YC( NO   ) +
     &        RKI( 17 ) * YC( NO2  ) + RKI( 18 ) * YC( NO2  ) +
     &        RKI( 42 ) * YC( FORM ) + RKI( 45 ) * YC( ALD2 ) +
     &        RKI( 60 ) * YC( OLE  ) + RKI( 68 ) * YC( CRES ) +
     &        RKI( 79 ) * YC( ISOP ) + RKI( 91 ) * YC( ISPD )

c..Solve analytically
c.....R56DT = K for N2O5-->NO3 times delta t
c.....R65DT = K for NO3+NO2-->N2O5 times delta t times [NO2]
      R56DT = RKI( 20 ) * DTC
      R65DT = RKI( 18 ) * YC( NO2  ) * DTC

      A1    = 1.0 + L_N2O5 * DTC
      A2    = 1.0 + L_NO3  * DTC
      A3    = YC0( NO3 ) + P_NO3 * DTC

      ATOP3 = A1 * A3 + R56DT * YC0( N2O5 )
      ATOP5 = A2 * YC0( N2O5 ) + R65DT * A3
      BOTT  = A1 * A2 - R56DT * R65DT

      YCP( NO3  ) = ATOP3 / BOTT
      YCP( N2O5 ) = ATOP5 / BOTT

      RETURN

      END
