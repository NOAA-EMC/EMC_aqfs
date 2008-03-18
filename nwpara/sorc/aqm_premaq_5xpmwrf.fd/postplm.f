
        SUBROUTINE POSTPLM( EMLAYS, S, ZBOT, ZTOP, PRESF, ZZF, TA,ZH, 
     &                      LTOP, LFRAC )

C***********************************************************************
C  subroutine body starts at line 
C
C  DESCRIPTION:
C    Subroutine POSTPLM computes plume fractions given a top and bottom
C    height of the plume.  It assumes a uniform distribution in pressure
C    (mass concentration -- minor hydrostatic assumption) from bottom to top.
C
C  PRECONDITIONS REQUIRED:
C    Top and bottom of plume as input, vertical grid structure defined, 
C    vertical pressure distribution provided.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C    Copied from postplm.f v 1.3 in DAQM-V2 Emissions Preprocessor by
C           M. Houyoux 3/99
C       updated 7/6/06 by G. Pouliot to increase array bound limits on TA to EMLAYS + 1
C             require: EMLAYS < NLAYS
C
C***********************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: postplm.f,v 1.1.8.2 2002/09/20 20:34:38 cas Exp $
C
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C
C smoke@emc.mcnc.org
C
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/point/postplm.f,v $
C Last updated: $Date: 2002/09/20 20:34:38 $ 
C
C***********************************************************************
 
        IMPLICIT NONE
 
C...........   INCLUDES:
        INCLUDE 'EMCNST3.EXT'
        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'FDESC3.EXT'
	INCLUDE 'CONST3.EXT'

C...........   EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER*2   CRLF

        EXTERNAL      CRLF

C...........   SUBROUTINE ARGUMENTS
        INTEGER, INTENT (IN) :: EMLAYS           ! no. emissions layers
        INTEGER, INTENT (IN) :: S                ! source ID
        REAL   , INTENT (IN) :: ZBOT             ! plume bottom elevation (m)
        REAL   , INTENT (IN) :: ZTOP             ! plume top elevation (m)
        REAL   , INTENT (IN) :: PRESF( 0:EMLAYS )  ! pressure (Pa) at full-levels (mb)
        REAL   , INTENT (IN) ::   ZZF( 0:EMLAYS )  ! elevation at full-levels (m)
	REAL   , INTENT (IN) ::    TA( 1:EMLAYS + 1)  ! temperature at half-levels (K)
        REAL   , INTENT (IN) ::    ZH( 1:EMLAYS )  ! layer center  height (m)
        INTEGER, INTENT(OUT) :: LTOP             ! plume top layer (integer)
        REAL   , INTENT(OUT) :: LFRAC( EMLAYS )  ! layer fractions for source (fractions)

C...........   Local variables

        INTEGER       L

        INTEGER       LBOT 

        DOUBLE PRECISION          DDP
        REAL          PBOT, PTOP
	REAL          TEMP
        DOUBLE PRECISION          PDIFF


        CHARACTER*300 MESG

C***********************************************************************
C   begin body of subroutine POSTPLM





C...........   Compute LBOT, LTOP so that
C...........   ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C...........   ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )
 
        DO L = 1, EMLAYS - 1
            IF ( ZBOT .LE. ZZF( L ) ) THEN
                LBOT = L
                GO TO  122   ! end loop and skip reset of LBOT
            ELSE
                LFRAC( L ) = 0.0      ! fractions below plume
            END IF
        END DO
        LBOT = EMLAYS           !  fallback

122     CONTINUE                !  loop exit:  bottom found at LBOT
 
        IF ( ZTOP .LE. ZZF( LBOT ) ) THEN  !  plume in this layer
 
            LFRAC( LBOT ) = 1.0
            LTOP = LBOT
 
            DO L = LBOT + 1, EMLAYS  ! fractions above plume
                LFRAC( L ) = 0.0
            END DO
 
C.........  Note- this check not in original algorithm, but w/o it,
C                         can end up with fractions > 1.0
        ELSEIF( LBOT .EQ. EMLAYS ) THEN    ! plume above top layer
 
            LFRAC( LBOT ) = 1.0
 
            DO L = 1, EMLAYS-1       ! fractions below plume
                LFRAC( L ) = 0.0
            END DO
 
        ELSE                               ! plume crosses layers
 
            DO L = LBOT + 1, EMLAYS
                IF ( ZTOP .LE. ZZF( L ) ) THEN
                    LTOP = L
                    GO TO 126  ! end loop and skip reset of LTOP
                END IF
            END DO
            LTOP = EMLAYS

126         CONTINUE
 
C...........   Compute corresponding PBOT,PTOP so that
C...........   PRESF( LBOT-1 ) <= PBOT < PRESF( LBOT ) and
C...........   PRESF( LTOP-1 ) <= PTOP < PRESF( LTOP )
C...........   (Use 3rd order polynomial via POLY() )

 


	    IF (ZBOT .LT. ZH(LBOT) .AND. (ZBOT .GT. ZZF(LBOT-1))) THEN   ! above full layer and below half layer
	        IF (ZBOT .LT. ZH(1)) THEN                                ! special case near ground
		    TEMP = TA(1)    
		ELSE
		    TEMP = (TA(LBOT) + TA(LBOT-1))/2.0
		ENDIF
	    
	    ELSE                                                         ! above full layer and above half layer
	         TEMP = (TA(LBOT) + TA(LBOT+1))/2.0
	    ENDIF
	    	   
            PBOT = PRESF( LBOT) 
     &      *EXP(GRAV/(RDGAS*TEMP)*(ZZF(LBOT) - ZBOT))  !hydrostatic assumption from LBOT layer

c            write (*,*) ZTOP, ZH(LTOP), ZZF(LTOP-1), (ZTOP.LT. ZH(LTOP) .AND. (ZTOP .GT. ZZF(LTOP-1)))
	    
 	    IF (ZTOP.LT. ZH(LTOP) .AND. (ZTOP .GT. ZZF(LTOP-1))) THEN   ! above full layer and below half layer
	        IF (ZTOP .LT. ZH(1)) THEN                                ! special case near ground
		    TEMP = TA(1)    
		ELSE
		    TEMP = (TA(LTOP) + TA(LTOP-1))/2.0
		ENDIF
	    
	    ELSE                                                         ! above full layer and above half layer
	         TEMP = (TA(LTOP) + TA(LTOP+1))/2.0
	    ENDIF
	    	   
            PTOP = PRESF( LTOP-1) 
     &      *EXP(-GRAV/(RDGAS*TEMP)*(ZTOP - ZZF(LTOP-1)))  !hydrostatic assumption

             
	    
!            write (*,*) S,ZZF(LTOP-1),ZTOP,ZZF(LTOP),PRESF(LTOP-1),PTOP,PRESF(LTOP)
!            write (*,*) PRESF(LBOT-1),PBOT,PRESF(LBOT)	    

 
            PDIFF = DBLE(PBOT) - DBLE(PTOP)
            IF( PDIFF .GT. 0 ) THEN

               DDP  = 1.0D0 / ( PDIFF )  !  = d(plumefrac)/d
              LFRAC( LBOT ) = DDP * ( DBLE(PBOT) - DBLE(PRESF( LBOT ) ))
            LFRAC( LTOP ) = DDP * ( DBLE(PRESF( LTOP-1 )) - DBLE(PTOP) )
 
            ELSE

	    
                WRITE( MESG,94010 )
     &           'Infinitely small plume created for source ', S,
     &           CRLF() // BLANK5 // 
     &           'because of inverted vertical pressure gradient!' //
     &           CRLF() // BLANK5 // 
     &           'All emissions put in first layer.'
                CALL M3WARN( 'POSTPLM', 0,0, MESG )
                LBOT = 1
                LTOP = 1
                LFRAC( LBOT ) = 1.0
 
            ENDIF
 
            DO L = LBOT+1, LTOP-1 !  layers in plume
         LFRAC( L ) = DDP*( DBLE(PRESF( L-1 )) - DBLE(PRESF( L )) )
            END DO
 
            DO L = LTOP+1, EMLAYS !  fractions above plume
                LFRAC( L ) = 0.0
            END DO
 
        END IF          !  if ztop in same layer as zbot, or not



        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I7, :, 1X ) )

	END SUBROUTINE POSTPLM
