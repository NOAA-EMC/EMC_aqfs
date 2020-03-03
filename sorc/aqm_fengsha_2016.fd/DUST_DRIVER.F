        PROGRAM DUST_DRIVER 

C-----------------------------------------------------------------------
C Function: Master program to drive the dust emission module.            
C   10/24/2006 Daniel Tong: Prototype 
C   01/25/2010 DT: updates to accommendate changes in other modules
C-----------------------------------------------------------------------

       
      IMPLICIT NONE
C Includes:
C     INCLUDE 'PARMS3.EXT'
C     INCLUDE 'FDESC3.EXT'
C     INCLUDE 'IODECL3.EXT_Tong'
C     INCLUDE 'DUST_CONST.EXT'         ! Constants for dust emissions only
C     INCLUDE 'FILE3_CTM.EXT'          ! File names
      INCLUDE SUBST_IOPARMS            ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC            ! file header data structuer
      INCLUDE SUBST_IODECL             ! I/O definitions and declarations
      INCLUDE SUBST_DUSTCT             ! Constant for dust calculation
      INCLUDE SUBST_FILES_ID           ! file name parameters
      INCLUDE SUBST_CONST              ! constants

C *** arguments
     
      INTEGER JDATE,JDATE0           ! current model date, coded YYYYDDD
      INTEGER JTIME,JTIME0           ! current model time, coded HHMMSS
	INTEGER TSTEP       	       ! sciproc sync. step (chem)
      INTEGER DX1, DX2, I, NSTEP 	 ! 

C *** emissions rates for windblown dust
      REAL DTEMIS ( NCOLS, NROWS )  

c *** initialize the array
      DO R = 1, MY_NROWS
      DO C = 1, MY_NCOLS
	   DTEMIS ( C, R ) = 0.0
	END DO
	END DO

      OPEN(10, FILE='dust.cfg', STATUS='OLD')

      READ(10, *) JDATE, JTIME, NSTEP
C     WRITE(*,*) "JDATE, JTIME, NSTEP: ", JDATE, JTIME, NSTEP
      CLOSE(10)
      
C     DX1 = 12000
C     DX2 = 12000
C     JDATE = 2002175
C     JTIME = 0
      TSTEP = 10000	

      CALL OPDUST( JDATE, JTIME, TSTEP ) 

      JDATE0 = JDATE
      JTIME0 = JTIME

      DO I = 1, NSTEP
C       CALL DUSTER SUBST_GRID_ID (DTEMIS,JDATE0,JTIME0,TSTEP,DX1,DX2)
C       CALL DUSTER ( DTEMIS,JDATE,JTIME,TSTEP,DX1,DX2 )
        CALL DUSTER ( DTEMIS,JDATE,JTIME,TSTEP)
        CALL nextime (JDATE, JTIME, TSTEP)
      END DO       
    
      END 

